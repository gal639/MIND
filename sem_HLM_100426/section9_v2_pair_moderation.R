# =============================================================================
# SECTION 9 v2 — PAIR-LEVEL MODERATION ANALYSIS
# Question: Do informant tools track ADOS, and does Sex/IQ moderate that tracking?
# Einat Waizbard, Gal Or — May 2026
# =============================================================================
#
# STRUCTURE
#   For each (ADOS_outcome, Predictor) pair:
#     M1: ADOS ~ age + Pred                                    [baseline tracking]
#     M2: ADOS ~ age + Pred + Sex + Pred*Sex                   [sex moderation]
#     M3: ADOS ~ age + Pred + IQ  + Pred*IQ                    [IQ moderation]   ← STUB
#     M4: ADOS ~ age + Pred + Sex + IQ + Pred*Sex + Pred*IQ    [joint]           ← STUB
#
#   M3/M4 require Einat's decision on ratio-IQ scale mixing (MIND_preschool
#   = 99% MSEL ratio; SSC has ~25% ratio). Until then, M3/M4 disabled at the
#   top of the script via a single flag. Flip RUN_IQ_MODELS <- TRUE after
#   the policy is set + IQ-augmented RDS files swapped in.
#
# DOMAINS (each ADOS facet predicted by parent/teacher/observer informants)
#   TOTAL   → ADOS_Severity ~ ADI_Total, SRS_Total_Parent, SRS_Total_Teacher*
#   SOCIAL  → ADOS_SA       ~ ADI_Social_Com, SRS_Social_Parent, SRS_Social_Teacher*
#   RRB     → ADOS_RRB      ~ ADI_RRB, SRS_RRB_Parent, RBSR_Total, SRS_RRB_Teacher*
#   * Teacher SRS only available in SSC.
#
# ESTIMATION
#   lavaan sem(), missing = "fiml", fixed.x = FALSE, em.h1.iter.max = 5000.
#   Auxiliaries: age, ADOS_Severity*, sample_source_num — z-scored before fit.
#   *Auxiliary list excludes vars already in the model — see select_aux().
#
# CENTERING
#   Pred, age, (IQ) mean-centered within each subsample fit. Sex dummy: F=1.
#   Interaction terms = product of centered predictors, computed as new cols
#   before model fit (lavaan does not handle ":" in regression syntax for
#   raw FIML the same way as lm(); explicit product columns are cleanest).
#
# LRT COMPARISONS (nested, identical variable sets via 0*-constraint)
#   M2 sex effect:        chi2 of M2 vs (M2 with Pred*Sex fixed to 0)
#   M3 IQ effect:         chi2 of M3 vs (M3 with Pred*IQ  fixed to 0)
#   M4 sex|IQ effect:     chi2 of M4 vs (M4 with Pred*Sex fixed to 0)
#
# CORRECTION
#   Bonferroni within (Subsample × Analysis) — i.e., across the 9-12 pairs of
#   each subsample, separately for Sex / IQ / Joint analyses.
#
# OUTPUT
#   regression_pair_moderation_results.txt   — full text dump
#   regression_pair_moderation_summary.csv   — long-format results table
#   regression_pair_moderation_results.xlsx  — one sheet per subsample
#   forest_pair_moderation_sex.png           — forest plot, Pred*Sex β
#   forest_pair_moderation_iq.png            — STUB until IQ enabled
#   forest_pair_moderation_joint.png         — STUB until IQ enabled
# =============================================================================


# =============================================================================
# --- 0. CONFIG
# =============================================================================

# setwd() removed — project root set by .Rproj / run_pipeline.R

# =============================================================================
# IQ SWITCHBOARD
# =============================================================================
# RUN_IQ_MODELS:
#   TRUE  → fit M3 (Pred×IQ) and M4 (Pred×Sex|IQ) in addition to M1, M2.
#           Auto-points data_src at SEM_output/data_with_iq/ which contains
#           FSIQ + IQ_instrument + IQ_source columns merged into the four RDS.
#   FALSE → only M1, M2 (sex moderation). M3/M4 columns NA in output.
#
# IQ_RATIO_POLICY (only matters when RUN_IQ_MODELS = TRUE):
#   "all"            → no filter. Mixes ratio (DQ, M≈50) and deviation (SS,
#                      M≈100) IQ kids. Exploratory only — Einat has not
#                      decided. NOT for paper without scale-as-covariate.
#   "deviation_only" → drop ratio kids. Cleanest interpretation. Kills
#                      MIND_preschool (374/375 are MSEL ratio).
#   "ratio_only"     → keep only ratio kids. For sanity-check / sensitivity.
#
# Detection rules (from merge_iq_into_rds.R):
#   MIND ratio = IQ_instrument == "MSEL"
#   SSC  ratio = IQ_instrument == "ratio"
#   anything else with non-NA FSIQ counts as deviation.
# =============================================================================
RUN_IQ_MODELS    <- TRUE
IQ_RATIO_POLICY  <- "all"          # "all" | "deviation_only" | "ratio_only"

data_src <- if (RUN_IQ_MODELS) {
  "SEM_output/data_with_iq"
} else {
  "SEM_output/data"
}

output_dir <- "sem_HLM_100426"

# Run tag: policy + timestamp. Each run lands in its own folder so prior
# results aren't overwritten. Format: section9_v2_output/<policy>_<YYYYMMDD_HHMM>/
run_tag    <- if (RUN_IQ_MODELS) {
  sprintf("iq_%s_%s",     IQ_RATIO_POLICY, format(Sys.time(), "%Y%m%d_%H%M"))
} else {
  sprintf("noIQ_%s",                       format(Sys.time(), "%Y%m%d_%H%M"))
}
out_root   <- file.path(output_dir, "section9_v2_output")
out_dir    <- file.path(out_root, run_tag)
plots_dir  <- file.path(out_dir, "plots")
dir.create(out_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

message(sprintf("IQ MODELS: %s   |   POLICY: %s   |   data_src: %s",
                RUN_IQ_MODELS, IQ_RATIO_POLICY, data_src))
message(sprintf("Output → %s", out_dir))


# =============================================================================
# --- 1. LIBRARIES
# =============================================================================

suppressPackageStartupMessages({
  library(lavaan)
  library(tidyverse)
  library(patchwork)
  library(writexl)
})


# =============================================================================
# --- 2. LOAD DATA
# (mirrors Section 1 of original SEM script — no changes needed)
# =============================================================================

common_cols <- c("id", "sex", "age",
                 "ADOS_Severity", "ADOS_SA", "ADOS_RRB",
                 "ADI_Total", "ADI_Social_Com", "ADI_RRB",
                 "SRS_Total_Parent", "SRS_Social_Parent", "SRS_RRB_Parent",
                 "SRS_Total_Teacher", "SRS_Social_Teacher", "SRS_RRB_Teacher",
                 "RBSR_Total", "sample")

# Add IQ cols if the IQ-augmented RDS files are in use
if (RUN_IQ_MODELS) {
  common_cols <- c(common_cols, "FSIQ", "IQ_instrument", "IQ_source")
}

standardize_df <- function(df, cols) {
  df <- as.data.frame(df)
  available <- intersect(cols, names(df))
  df <- df[, available, drop = FALSE]
  num_cols <- setdiff(available,
                      c("id", "sex", "sample", "IQ_instrument", "IQ_source"))
  for (col in num_cols) df[[col]] <- as.numeric(df[[col]])
  df
}

dat <- bind_rows(
  standardize_df(readRDS(file.path(data_src, "SEM_MIND_schoolage.rds")), common_cols),
  standardize_df(readRDS(file.path(data_src, "SEM_MIND_preschool.rds")), common_cols),
  standardize_df(readRDS(file.path(data_src, "SEM_SSC_schoolage.rds")),  common_cols),
  standardize_df(readRDS(file.path(data_src, "SEM_SSC_preschool.rds")),  common_cols)
) %>%
  mutate(
    sample_source = case_when(
      str_detect(sample, "MIND") ~ "MIND",
      str_detect(sample, "SSC")  ~ "SSC"
    ),
    age_group = case_when(
      str_detect(sample, "preschool") ~ "Preschool",
      str_detect(sample, "schoolage") ~ "School"
    ),
    sample_source_num = as.numeric(sample_source == "SSC")
  )

# IQ ratio-policy filter. Tag scale_type first (always; useful as covariate
# even under "all"). Then drop rows per policy. Print N before/after per
# subsample so silent dropouts are visible.
if (RUN_IQ_MODELS && "IQ_instrument" %in% names(dat)) {
  dat <- dat %>%
    mutate(
      scale_type = case_when(
        sample_source == "MIND" & IQ_instrument == "MSEL"     ~ "ratio",
        sample_source == "SSC"  & IQ_instrument == "ratio"    ~ "ratio",
        !is.na(FSIQ)                                          ~ "deviation",
        TRUE                                                  ~ NA_character_
      )
    )
  before_n <- dat %>% count(sample_source, age_group, name = "n_before")
  if (IQ_RATIO_POLICY == "deviation_only") {
    dat <- dat %>% filter(is.na(scale_type) | scale_type == "deviation")
  } else if (IQ_RATIO_POLICY == "ratio_only") {
    dat <- dat %>% filter(is.na(scale_type) | scale_type == "ratio")
  } # "all" → no filter
  after_n <- dat %>% count(sample_source, age_group, name = "n_after")
  message("\nIQ ratio-policy filter applied (", IQ_RATIO_POLICY, "):")
  print(before_n %>% left_join(after_n, by = c("sample_source", "age_group")))
}


# =============================================================================
# --- 3. PAIR DEFINITIONS
# =============================================================================

# Each domain has one ADOS outcome and several informant predictors.
# Teacher SRS only available in SSC — handled by data-availability check.
PAIRS <- tribble(
  ~domain,   ~outcome,         ~predictor,
  "TOTAL",   "ADOS_Severity",  "ADI_Total",
  "TOTAL",   "ADOS_Severity",  "SRS_Total_Parent",
  "TOTAL",   "ADOS_Severity",  "SRS_Total_Teacher",
  "SOCIAL",  "ADOS_SA",        "ADI_Social_Com",
  "SOCIAL",  "ADOS_SA",        "SRS_Social_Parent",
  "SOCIAL",  "ADOS_SA",        "SRS_Social_Teacher",
  "RRB",     "ADOS_RRB",       "ADI_RRB",
  "RRB",     "ADOS_RRB",       "SRS_RRB_Parent",
  "RRB",     "ADOS_RRB",       "RBSR_Total",
  "RRB",     "ADOS_RRB",       "SRS_RRB_Teacher"
)


# =============================================================================
# --- 4. HELPERS
# (lifted from original SEM script — kept self-contained here)
# =============================================================================

sig_stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01,  "**",
                       ifelse(p < .05,  "*", ""))))
}

# Pick auxiliaries present in data, varying, non-sparse, not already in model.
# `exclude_originals` lists original variable names that have been aliased into
# the model (e.g., "ADOS_Severity" → "out", "age" → "age_c"). Including them as
# auxiliaries causes perfect-correlation warnings — the same data appears twice.
select_aux <- function(data, model_vars,
                       exclude_originals = character(0),
                       candidates = c("age", "ADOS_Severity", "sample_source_num")) {
  candidates <- candidates[candidates %in% names(data)]
  candidates <- setdiff(candidates, c(model_vars, exclude_originals))
  if (length(candidates) == 0) return(character(0))
  candidates <- candidates[vapply(candidates, function(v) {
    vals <- data[[v]][!is.na(data[[v]])]
    length(unique(vals)) > 1 && length(vals) >= 10
  }, logical(1))]
  candidates
}

# z-score auxiliaries to keep lavaan from grumbling about variance scale
scale_aux <- function(data, aux) {
  for (a in aux) {
    sd_a <- sd(data[[a]], na.rm = TRUE)
    if (!is.na(sd_a) && sd_a > 0) data[[a]] <- as.numeric(scale(data[[a]]))
  }
  data
}

# Build lavaan model syntax. predictors = vector of RHS names; if any is
# fixed to zero (for nested LRT), pass via zero_pred. Aux vars get free
# variance, mean, and covariances with everything in the model.
build_reg_model <- function(outcome, predictors, zero_pred = NULL, aux = NULL) {
  free_preds <- setdiff(predictors, zero_pred)
  all_vars   <- c(outcome, predictors)
  lines <- character(0)
  
  if (length(free_preds) > 0) {
    lines <- c(lines, paste(outcome, "~", paste(free_preds, collapse = " + ")))
  }
  for (zp in zero_pred) lines <- c(lines, paste0(outcome, " ~ 0*", zp))
  
  lines <- c(lines, "",
             paste0(all_vars, " ~~ ", all_vars),
             paste0(all_vars, " ~ 1"))
  
  if (!is.null(aux) && length(aux) > 0) {
    lines <- c(lines, "",
               paste0(aux, " ~~ ", aux),
               paste0(aux, " ~ 1"))
    for (a in aux) for (v in all_vars) {
      lines <- c(lines, paste0(a, " ~~ ", v))
    }
    if (length(aux) > 1) {
      for (i in 1:(length(aux) - 1)) for (j in (i + 1):length(aux)) {
        lines <- c(lines, paste0(aux[i], " ~~ ", aux[j]))
      }
    }
  }
  paste(lines, collapse = "\n")
}

fit_fiml <- function(data, model) {
  tryCatch(
    sem(model, data = data, missing = "fiml",
        fixed.x = FALSE, em.h1.iter.max = 5000),
    error = function(e) { message("    [FIT ERROR] ", e$message); NULL }
  )
}

# Pull single regression coefficient from a fit
extract_coef <- function(fit, lhs, rhs) {
  if (is.null(fit)) return(rep(NA_real_, 6))
  pe <- parameterEstimates(fit)
  row <- pe[pe$lhs == lhs & pe$op == "~" & pe$rhs == rhs, ]
  if (nrow(row) == 0) return(rep(NA_real_, 6))
  c(row$est[1], row$se[1], row$z[1], row$pvalue[1], row$ci.lower[1], row$ci.upper[1])
}


# =============================================================================
# --- 5. CORE: FIT ONE PAIR
# Returns a tibble row with M1/M2 results (and M3/M4 if RUN_IQ_MODELS).
# =============================================================================

fit_one_pair <- function(data, outcome, predictor, subsample_label) {
  
  # ---- prep ---------------------------------------------------------------
  if (!(outcome %in% names(data)) || !(predictor %in% names(data))) return(NULL)
  
  d <- data %>%
    filter(sex %in% c("Male", "Female")) %>%
    mutate(sex_F = as.numeric(sex == "Female"))
  
  if (sum(!is.na(d[[outcome]]))   < 20) return(NULL)
  if (sum(!is.na(d[[predictor]])) < 20) return(NULL)
  if (sum(d$sex == "Male") < 5 || sum(d$sex == "Female") < 5) return(NULL)
  
  # Z-score continuous predictors within subsample. Z-scoring (not just centering)
  # avoids the "variances differ by factor 1000" warning from lavaan when
  # informant scores (range 0-200+) sit alongside age in months and centered
  # interaction products.
  zscore <- function(x) {
    s <- sd(x, na.rm = TRUE)
    if (is.na(s) || s == 0) return(x - mean(x, na.rm = TRUE))
    (x - mean(x, na.rm = TRUE)) / s
  }
  d$age_c   <- zscore(d$age)
  d$pred_c  <- zscore(d[[predictor]])
  d$out     <- d[[outcome]]                          # alias for syntax
  d$predXsex <- d$pred_c * d$sex_F                   # interaction column
  
  if (RUN_IQ_MODELS && "FSIQ" %in% names(d)) {
    d$iq_c    <- zscore(d$FSIQ)
    d$predXiq <- d$pred_c * d$iq_c
  }
  
  # Originals to keep out of auxiliary lists. `out` is `outcome` aliased; `age_c`
  # is `age` z-scored; if outcome is `ADOS_Severity`, drop the default ADOS
  # auxiliary too. Same logic for FSIQ if IQ models active.
  excl <- c(outcome, "age", predictor)
  if (RUN_IQ_MODELS) excl <- c(excl, "FSIQ")
  
  # ---- M1: out ~ age + pred ----------------------------------------------
  aux1   <- select_aux(d, c("out", "age_c", "pred_c"), exclude_originals = excl)
  d1     <- scale_aux(d, aux1)
  mod1   <- build_reg_model("out", c("age_c", "pred_c"), aux = aux1)
  fit_M1 <- fit_fiml(d1, mod1)
  
  c_M1_pred <- extract_coef(fit_M1, "out", "pred_c")
  R2_M1     <- tryCatch(lavInspect(fit_M1, "r2")[["out"]], error = function(e) NA_real_)
  
  # ---- M2: out ~ age + pred + sex + pred*sex ------------------------------
  aux2   <- select_aux(d, c("out", "age_c", "pred_c", "sex_F", "predXsex"),
                       exclude_originals = excl)
  d2     <- scale_aux(d, aux2)
  mod2_full <- build_reg_model("out", c("age_c", "pred_c", "sex_F", "predXsex"), aux = aux2)
  mod2_null <- build_reg_model("out", c("age_c", "pred_c", "sex_F", "predXsex"),
                               zero_pred = "predXsex", aux = aux2)
  fit_M2_full <- fit_fiml(d2, mod2_full)
  fit_M2_null <- fit_fiml(d2, mod2_null)
  
  c_M2_int  <- extract_coef(fit_M2_full, "out", "predXsex")
  c_M2_pred <- extract_coef(fit_M2_full, "out", "pred_c")
  c_M2_sex  <- extract_coef(fit_M2_full, "out", "sex_F")
  R2_M2     <- tryCatch(lavInspect(fit_M2_full, "r2")[["out"]], error = function(e) NA_real_)
  
  lrt12 <- if (!is.null(fit_M2_full) && !is.null(fit_M2_null))
    tryCatch(lavTestLRT(fit_M2_null, fit_M2_full), error = function(e) NULL) else NULL
  chi2_12 <- if (!is.null(lrt12)) lrt12[["Chisq diff"]][2] else NA_real_
  df_12   <- if (!is.null(lrt12)) lrt12[["Df diff"]][2]    else NA_real_
  p_12    <- if (!is.null(lrt12)) lrt12[["Pr(>Chisq)"]][2] else NA_real_
  
  # ---- M3 / M4: STUBS (require IQ policy) --------------------------------
  c_M3_int <- c_M4_int_sex <- c_M4_int_iq <- rep(NA_real_, 6)
  R2_M3 <- R2_M4 <- NA_real_
  chi2_13 <- df_13 <- p_13 <- NA_real_
  chi2_34 <- df_34 <- p_34 <- NA_real_
  
  if (RUN_IQ_MODELS && "FSIQ" %in% names(d)) {
    # M3: out ~ age + pred + iq + pred*iq
    aux3 <- select_aux(d, c("out", "age_c", "pred_c", "iq_c", "predXiq"),
                       exclude_originals = excl)
    d3   <- scale_aux(d, aux3)
    mod3_full <- build_reg_model("out", c("age_c", "pred_c", "iq_c", "predXiq"), aux = aux3)
    mod3_null <- build_reg_model("out", c("age_c", "pred_c", "iq_c", "predXiq"),
                                 zero_pred = "predXiq", aux = aux3)
    fit_M3_full <- fit_fiml(d3, mod3_full)
    fit_M3_null <- fit_fiml(d3, mod3_null)
    c_M3_int <- extract_coef(fit_M3_full, "out", "predXiq")
    R2_M3    <- tryCatch(lavInspect(fit_M3_full, "r2")[["out"]], error = function(e) NA_real_)
    
    lrt13 <- if (!is.null(fit_M3_full) && !is.null(fit_M3_null))
      tryCatch(lavTestLRT(fit_M3_null, fit_M3_full), error = function(e) NULL) else NULL
    chi2_13 <- if (!is.null(lrt13)) lrt13[["Chisq diff"]][2] else NA_real_
    df_13   <- if (!is.null(lrt13)) lrt13[["Df diff"]][2]    else NA_real_
    p_13    <- if (!is.null(lrt13)) lrt13[["Pr(>Chisq)"]][2] else NA_real_
    
    # M4: out ~ age + pred + sex + iq + pred*sex + pred*iq
    aux4 <- select_aux(d, c("out", "age_c", "pred_c", "sex_F", "iq_c", "predXsex", "predXiq"),
                       exclude_originals = excl)
    d4   <- scale_aux(d, aux4)
    preds4 <- c("age_c", "pred_c", "sex_F", "iq_c", "predXsex", "predXiq")
    mod4_full <- build_reg_model("out", preds4, aux = aux4)
    # Test of sex moderation surviving IQ adjustment = drop predXsex from M4
    mod4_null_sex <- build_reg_model("out", preds4, zero_pred = "predXsex", aux = aux4)
    fit_M4_full    <- fit_fiml(d4, mod4_full)
    fit_M4_null_sx <- fit_fiml(d4, mod4_null_sex)
    c_M4_int_sex <- extract_coef(fit_M4_full, "out", "predXsex")
    c_M4_int_iq  <- extract_coef(fit_M4_full, "out", "predXiq")
    R2_M4        <- tryCatch(lavInspect(fit_M4_full, "r2")[["out"]], error = function(e) NA_real_)
    
    lrt34 <- if (!is.null(fit_M4_full) && !is.null(fit_M4_null_sx))
      tryCatch(lavTestLRT(fit_M4_null_sx, fit_M4_full), error = function(e) NULL) else NULL
    chi2_34 <- if (!is.null(lrt34)) lrt34[["Chisq diff"]][2] else NA_real_
    df_34   <- if (!is.null(lrt34)) lrt34[["Df diff"]][2]    else NA_real_
    p_34    <- if (!is.null(lrt34)) lrt34[["Pr(>Chisq)"]][2] else NA_real_
  }
  
  # ---- assemble row ------------------------------------------------------
  tibble(
    Subsample = subsample_label,
    Outcome   = outcome,
    Predictor = predictor,
    n_male    = sum(d$sex == "Male"),
    n_female  = sum(d$sex == "Female"),
    
    # M1
    R2_M1   = round(R2_M1, 4),
    b_pred_M1 = round(c_M1_pred[1], 3), se_pred_M1 = round(c_M1_pred[2], 3),
    p_pred_M1 = round(c_M1_pred[4], 4),
    
    # M2
    R2_M2     = round(R2_M2, 4),
    b_pred_M2 = round(c_M2_pred[1], 3),
    b_sex_M2  = round(c_M2_sex[1], 3),
    b_int_M2  = round(c_M2_int[1], 3), se_int_M2 = round(c_M2_int[2], 3),
    z_int_M2  = round(c_M2_int[3], 3), p_int_M2  = round(c_M2_int[4], 4),
    ci_lo_M2  = round(c_M2_int[5], 3), ci_hi_M2  = round(c_M2_int[6], 3),
    chi2_12 = round(chi2_12, 3), df_12 = df_12, p_lrt_12 = round(p_12, 4),
    
    # M3 (IQ moderation) — STUB unless RUN_IQ_MODELS
    R2_M3    = round(R2_M3, 4),
    b_int_M3 = round(c_M3_int[1], 3), se_int_M3 = round(c_M3_int[2], 3),
    z_int_M3 = round(c_M3_int[3], 3), p_int_M3  = round(c_M3_int[4], 4),
    ci_lo_M3 = round(c_M3_int[5], 3), ci_hi_M3  = round(c_M3_int[6], 3),
    chi2_13 = round(chi2_13, 3), df_13 = df_13, p_lrt_13 = round(p_13, 4),
    
    # M4 (joint) — STUB unless RUN_IQ_MODELS
    R2_M4         = round(R2_M4, 4),
    b_intSex_M4   = round(c_M4_int_sex[1], 3), se_intSex_M4 = round(c_M4_int_sex[2], 3),
    p_intSex_M4   = round(c_M4_int_sex[4], 4),
    ci_lo_intSex_M4 = round(c_M4_int_sex[5], 3), ci_hi_intSex_M4 = round(c_M4_int_sex[6], 3),
    b_intIQ_M4    = round(c_M4_int_iq[1], 3),  se_intIQ_M4  = round(c_M4_int_iq[2], 3),
    p_intIQ_M4    = round(c_M4_int_iq[4], 4),
    chi2_34 = round(chi2_34, 3), df_34 = df_34, p_lrt_34 = round(p_34, 4)
  )
}


# =============================================================================
# --- 6. RUN ALL SUBSAMPLES × PAIRS
# =============================================================================

run_subsample <- function(data, label) {
  message(sprintf("\n-- %s --", label))
  data <- as.data.frame(data)
  
  # Filter pairs: keep only those whose outcome AND predictor have ≥ 20 valid
  # cases in this subsample. Teacher SRS pairs auto-drop in MIND.
  pairs_here <- PAIRS %>%
    filter(outcome %in% names(data), predictor %in% names(data)) %>%
    rowwise() %>%
    filter(sum(!is.na(data[[outcome]]))   >= 20,
           sum(!is.na(data[[predictor]])) >= 20) %>%
    ungroup()
  
  if (nrow(pairs_here) == 0) {
    message("    [skip] no usable pairs"); return(NULL)
  }
  
  pmap_dfr(pairs_here, function(domain, outcome, predictor) {
    message(sprintf("   %-6s %s ~ %s", domain, outcome, predictor))
    res <- fit_one_pair(data, outcome, predictor, label)
    if (is.null(res)) return(NULL)
    res$Domain <- domain
    res
  })
}

message("\n=== SECTION 9 v2 — PAIR-LEVEL MODERATION ===")
message(sprintf("RUN_IQ_MODELS: %s", RUN_IQ_MODELS))

results <- bind_rows(
  run_subsample(dat,                                                              "1. Full Sample"),
  run_subsample(dat %>% filter(sample_source == "MIND"),                          "2a. MIND"),
  run_subsample(dat %>% filter(sample_source == "SSC"),                           "2b. SSC"),
  run_subsample(dat %>% filter(sample_source == "MIND", age_group == "Preschool"),"3a. MIND Preschool"),
  run_subsample(dat %>% filter(sample_source == "MIND", age_group == "School"),   "3b. MIND School"),
  run_subsample(dat %>% filter(sample_source == "SSC",  age_group == "Preschool"),"3c. SSC Preschool"),
  run_subsample(dat %>% filter(sample_source == "SSC",  age_group == "School"),   "3d. SSC School")
) %>%
  relocate(Domain, .after = Subsample)


# =============================================================================
# --- 7. BONFERRONI CORRECTION (within Subsample × Analysis)
# =============================================================================

results <- results %>%
  group_by(Subsample) %>%
  mutate(
    # Bonferroni — strict FWER control
    p_bonf_M2 = p.adjust(p_int_M2, method = "bonferroni"),
    p_bonf_M3 = if (RUN_IQ_MODELS) p.adjust(p_int_M3,    method = "bonferroni") else NA_real_,
    p_bonf_M4 = if (RUN_IQ_MODELS) p.adjust(p_intSex_M4, method = "bonferroni") else NA_real_,
    # Benjamini-Hochberg FDR — recommended for exploratory moderation across pairs
    p_fdr_M2  = p.adjust(p_int_M2, method = "BH"),
    p_fdr_M3  = if (RUN_IQ_MODELS) p.adjust(p_int_M3,    method = "BH") else NA_real_,
    p_fdr_M4  = if (RUN_IQ_MODELS) p.adjust(p_intSex_M4, method = "BH") else NA_real_,
    sig_M2    = sig_stars(p_bonf_M2),
    sig_M3    = sig_stars(p_bonf_M3),
    sig_M4    = sig_stars(p_bonf_M4),
    sig_fdr_M2 = sig_stars(p_fdr_M2),
    sig_fdr_M3 = sig_stars(p_fdr_M3),
    sig_fdr_M4 = sig_stars(p_fdr_M4)
  ) %>%
  ungroup()


# =============================================================================
# --- 8. TEXT OUTPUT
# =============================================================================

sink(file.path(out_dir, "regression_pair_moderation_results.txt"), split = TRUE)
cat("PAIR-LEVEL MODERATION ANALYSIS — Section 9 v2\n")
cat("Question: Do informant tools track ADOS, and does Sex/IQ moderate that tracking?\n")
cat("Female=1, Male=0  |  continuous predictors z-scored within subsample\n")
cat("LRT: chi-square diff (lavTestLRT) on nested models with 0*-constraint\n")
cat("Multiplicity: Bonferroni AND Benjamini-Hochberg FDR, within Subsample × Analysis\n")
cat(sprintf("RUN_IQ_MODELS: %s   |   IQ_RATIO_POLICY: %s   |   data_src: %s\n",
            RUN_IQ_MODELS, IQ_RATIO_POLICY, basename(data_src)))
cat(sprintf("Generated: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
cat(strrep("=", 76), "\n\n")

for (sub in unique(results$Subsample)) {
  cat(strrep("#", 76), "\n")
  cat(sprintf("## %s\n", sub))
  cat(strrep("#", 76), "\n\n")
  res <- results %>% filter(Subsample == sub)
  for (i in seq_len(nrow(res))) {
    r <- res[i, ]
    cat(sprintf("  %s: %s ~ %s   (M=%d, F=%d)\n",
                r$Domain, r$Outcome, r$Predictor, r$n_male, r$n_female))
    cat(sprintf("    M1: R²=%.3f   b_pred=%.3f (SE=%.3f, p=%.4f)\n",
                r$R2_M1, r$b_pred_M1, r$se_pred_M1, r$p_pred_M1))
    cat(sprintf("    M2: R²=%.3f   b(Pred×Sex)=%.3f  SE=%.3f  z=%.3f  p=%.4f  p_bonf=%.4f %s  p_fdr=%.4f %s\n",
                r$R2_M2, r$b_int_M2, r$se_int_M2, r$z_int_M2,
                r$p_int_M2, r$p_bonf_M2, r$sig_M2, r$p_fdr_M2, r$sig_fdr_M2))
    cat(sprintf("        95%%CI [%.3f, %.3f]   LRT Δχ²(%.0f)=%.3f p=%.4f\n",
                r$ci_lo_M2, r$ci_hi_M2, r$df_12, r$chi2_12, r$p_lrt_12))
    if (RUN_IQ_MODELS) {
      cat(sprintf("    M3: R²=%.3f   b(Pred×IQ)=%.3f  SE=%.3f  95%%CI [%.3f, %.3f]  p=%.4f  p_bonf=%.4f %s  p_fdr=%.4f %s\n",
                  r$R2_M3, r$b_int_M3, r$se_int_M3, r$ci_lo_M3, r$ci_hi_M3,
                  r$p_int_M3, r$p_bonf_M3, r$sig_M3, r$p_fdr_M3, r$sig_fdr_M3))
      cat(sprintf("    M4: R²=%.3f   b(Pred×Sex|IQ)=%.3f  SE=%.3f  95%%CI [%.3f, %.3f]  p=%.4f  p_bonf=%.4f %s  p_fdr=%.4f %s\n",
                  r$R2_M4, r$b_intSex_M4, r$se_intSex_M4, r$ci_lo_intSex_M4, r$ci_hi_intSex_M4,
                  r$p_intSex_M4, r$p_bonf_M4, r$sig_M4, r$p_fdr_M4, r$sig_fdr_M4))
      cat(sprintf("        b(Pred×IQ in M4)=%.3f  SE=%.3f  p=%.4f\n",
                  r$b_intIQ_M4, r$se_intIQ_M4, r$p_intIQ_M4))
    }
    cat("\n")
  }
}
sink()


# =============================================================================
# --- 9. SAVE TABLES
# =============================================================================

write_csv(results, file.path(out_dir, "regression_pair_moderation_summary.csv"))

sheets <- c(
  list(ALL = results),
  map(unique(results$Subsample), ~results %>% filter(Subsample == .x)) %>%
    set_names(
      unique(results$Subsample) %>%
        str_replace_all("[^A-Za-z0-9 ]", "") %>% str_trim() %>% str_sub(1, 31)
    )
)
write_xlsx(sheets, file.path(out_dir, "regression_pair_moderation_results.xlsx"))


# =============================================================================
# --- 10. FOREST PLOTS (one per analysis: Sex, IQ, Joint)
# =============================================================================

# pair_label: for forest Y axis. Short, predictor-only, since outcome is fixed
# within each domain.
make_forest <- function(res, b_col, se_col, ci_lo_col, ci_hi_col,
                        p_col, p_bonf_col, sig_col, title, subtitle_xlab) {
  d <- res %>%
    rename(b = !!b_col, se = !!se_col, ci_lo = !!ci_lo_col, ci_hi = !!ci_hi_col,
           p = !!p_col, p_bonf = !!p_bonf_col, sig_lab = !!sig_col) %>%
    filter(!is.na(b), !is.na(ci_lo), !is.na(ci_hi),
           !is.na(se), se < 50) %>%             # drop non-converged
    mutate(
      pair_label = paste(Domain, ":", Predictor),
      pair_label = factor(pair_label, levels = rev(unique(pair_label))),
      sig = !is.na(p_bonf) & p_bonf < .05
    )
  if (nrow(d) == 0) return(NULL)
  
  ggplot(d, aes(x = b, y = pair_label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi, color = sig),
                  orientation = "y", width = 0.25, linewidth = 0.6) +
    geom_point(aes(color = sig, shape = sig), size = 2.8) +
    geom_text(aes(label = sig_lab, x = ci_hi), hjust = -0.3,
              size = 3.4, fontface = "bold", color = "#E75480") +
    scale_color_manual(values = c(`TRUE` = "#E75480", `FALSE` = "gray60"),
                       labels = c(`TRUE` = "sig (Bonf.)", `FALSE` = "n.s."),
                       name = NULL) +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                       labels = c(`TRUE` = "sig (Bonf.)", `FALSE` = "n.s."),
                       name = NULL) +
    facet_wrap(~ Subsample, scales = "free_x", ncol = 2) +
    labs(title = title,
         subtitle = "Filled/pink = Bonferroni sig within subsample | Bars = 95% CI (uncorrected)",
         x = subtitle_xlab, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title       = element_text(face = "bold", size = 13),
          plot.subtitle    = element_text(color = "gray40", size = 9),
          legend.position  = "bottom",
          panel.grid.minor = element_blank(),
          axis.text.y      = element_text(size = 8),
          strip.text       = element_text(face = "bold", size = 10))
}

p_sex <- make_forest(results,
                     "b_int_M2", "se_int_M2", "ci_lo_M2", "ci_hi_M2",
                     "p_int_M2", "p_bonf_M2", "sig_M2",
                     "Sex Moderation of ADOS-Informant Coupling (M2: Pred × Sex)",
                     "β (Pred × Sex)  |  >0 = stronger coupling for Female")

if (!is.null(p_sex)) {
  ggsave(file.path(plots_dir, "forest_pair_moderation_sex.png"),
         p_sex, width = 13, height = 11, dpi = 300)
  message("  ✓ saved forest_pair_moderation_sex.png")
}

if (RUN_IQ_MODELS) {
  p_iq <- make_forest(results,
                      "b_int_M3", "se_int_M3", "ci_lo_M3", "ci_hi_M3",
                      "p_int_M3", "p_bonf_M3", "sig_M3",
                      "IQ Moderation of ADOS-Informant Coupling (M3: Pred × IQ)",
                      "β (Pred × IQ)  |  >0 = stronger coupling at higher IQ")
  if (!is.null(p_iq)) {
    ggsave(file.path(plots_dir, "forest_pair_moderation_iq.png"),
           p_iq, width = 13, height = 11, dpi = 300)
    message("  ✓ saved forest_pair_moderation_iq.png")
  }
  
  p_joint <- make_forest(results,
                         "b_intSex_M4", "se_intSex_M4", "ci_lo_intSex_M4", "ci_hi_intSex_M4",
                         "p_intSex_M4", "p_bonf_M4", "sig_M4",
                         "Sex Moderation Surviving IQ Control (M4: Pred × Sex | IQ)",
                         "β (Pred × Sex)  |  joint model with Pred × IQ")
  if (!is.null(p_joint)) {
    ggsave(file.path(plots_dir, "forest_pair_moderation_joint.png"),
           p_joint, width = 13, height = 11, dpi = 300)
    message("  ✓ saved forest_pair_moderation_joint.png")
  }
} else {
  message("  -- M3/M4 forests skipped (RUN_IQ_MODELS = FALSE)")
}

message(sprintf("\n✓ Section 9 v2 done. Output in: %s",
                normalizePath(out_dir, mustWork = FALSE)))