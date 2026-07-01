# =============================================================================
# resample_lenses.R
# Contract-conforming lens adapters for the sex-balanced resampling engine
# (resample_balance.R). Wires the THREE existing lenses of the tool-agreement
# pipeline into the resampling contract:
#
#   fisher      Fisher r-to-z, within-domain pairs        (girls fixed vs boys)
#   sem_lrt     two-group covariance LRT (FIML)           (free vs constrained)
#   regression  hierarchical moderation M1-M4 (FIML)      (sex x tool = beta_4)
#
# CONTRACT (see resample_balance.R): a lens takes ONE balanced data frame
# (all girls + a matched boy subsample, handed in by the engine) and returns a
# tibble with columns  term / estimate / es / p_value  (one row per parameter).
#
# DESIGN NOTE — why this file MIRRORS the canonical model code instead of
# sourcing it:
#   The canonical scripts (SEM_output/SEM_cov_comparison.R and
#   sem_HLM_100426/section9_v2_pair_moderation.R) run their full analysis +
#   write files + load plotting-only packages (semPlot, writexl) at source()
#   time, so they cannot be sourced just to borrow helpers. The model SPEC
#   (build_model / build_reg_model) and the ESTIMATOR (the exact sem(...) FIML
#   calls) below are copied VERBATIM from those scripts. They are the canonical
#   objects, unchanged — do NOT edit the model spec or estimator here. If the
#   canonical scripts change, mirror the change here to keep the lenses honest.
#
# Requires: lavaan, dplyr, purrr, tibble, tidyr, withr, stats.
# =============================================================================

suppressPackageStartupMessages({
  library(lavaan)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(tidyr)
})

# resample_balance.R provides: resample_balanced(), aggregate_resamples(),
# add_fdr(), run_balanced_pipeline(). Source it relative to this file's dir
# (the orchestrator sets WD to BASE_DIR, where this file lives).
if (!exists("run_balanced_pipeline")) source("resample_balance.R")


# =============================================================================
# 0. CONSTANTS — sex coding + domain/pair specs (mirrored from canonical)
# =============================================================================

# Sex column + the value marking the (smaller, fixed) female group.
# Detected from the real data: column "sex", levels "Male"/"Female".
SEX_COL      <- "sex"
FEMALE_LEVEL <- "Female"

# Columns carried from each RDS (mirror of common_cols in both canonical scripts).
COMMON_COLS <- c("id", "sex", "age",
                 "ADOS_Severity", "ADOS_SA", "ADOS_RRB",
                 "ADI_Total", "ADI_Social_Com", "ADI_RRB",
                 "SRS_Total_Parent", "SRS_Social_Parent", "SRS_RRB_Parent",
                 "SRS_Total_Teacher", "SRS_Social_Teacher", "SRS_RRB_Teacher",
                 "RBSR_Total", "sample",
                 "FSIQ", "IQ_instrument", "IQ_source")

# Within-domain variable groups for the Fisher lens (mirror of domain_map in
# correlation_matrices/sex_plots_cohorts_einat_040326.R).
FISHER_DOMAINS <- list(
  SA     = c("ADOS_SA", "ADI_Social_Com", "SRS_Social_Parent", "SRS_Social_Teacher"),
  RRB    = c("ADOS_RRB", "ADI_RRB", "SRS_RRB_Parent", "SRS_RRB_Teacher", "RBSR_Total"),
  Totals = c("ADOS_Severity", "ADI_Total", "SRS_Total_Parent", "SRS_Total_Teacher")
)

# ADOS-outcome ~ informant-predictor pairs for the regression lens (mirror of
# PAIRS in sem_HLM_100426/section9_v2_pair_moderation.R).
REG_PAIRS <- tibble::tribble(
  ~domain,  ~outcome,        ~predictor,
  "TOTAL",  "ADOS_Severity", "ADI_Total",
  "TOTAL",  "ADOS_Severity", "SRS_Total_Parent",
  "TOTAL",  "ADOS_Severity", "SRS_Total_Teacher",
  "SOCIAL", "ADOS_SA",       "ADI_Social_Com",
  "SOCIAL", "ADOS_SA",       "SRS_Social_Parent",
  "SOCIAL", "ADOS_SA",       "SRS_Social_Teacher",
  "RRB",    "ADOS_RRB",      "ADI_RRB",
  "RRB",    "ADOS_RRB",      "SRS_RRB_Parent",
  "RRB",    "ADOS_RRB",      "RBSR_Total",
  "RRB",    "ADOS_RRB",      "SRS_RRB_Teacher"
)


# =============================================================================
# 1. DATA LOADER + SUBSAMPLE BUILDER
# (mirror of Section 2 data-load in both canonical scripts)
# =============================================================================

.standardize_df <- function(df, cols) {
  df <- as.data.frame(df)
  available <- intersect(cols, names(df))
  df <- df[, available, drop = FALSE]
  num_cols <- setdiff(available,
                      c("id", "sex", "sample", "IQ_instrument", "IQ_source"))
  for (col in num_cols) df[[col]] <- as.numeric(df[[col]])
  df
}

#' Load + combine the four IQ-augmented RDS into one frame with sample_source /
#' age_group / sample_source_num, and (optionally) apply the IQ ratio-policy
#' filter. Mirrors the canonical load so the resampled analysis sees identical
#' data.
#'
#' @param data_src   Directory with the four SEM_*.rds (default data_with_iq).
#' @param iq_policy  "all" | "deviation_only" | "ratio_only". SURFACED, NOT
#'   DECIDED: ratio-vs-deviation IQ for MIND Preschool is unresolved (374/375
#'   are MSEL ratio). "all" mirrors the current pipeline default; the author +
#'   Einat must confirm. No IQ transform is hardcoded.
build_resample_data <- function(data_src  = "SEM_output/data_with_iq",
                                iq_policy = "all") {
  rds <- c("SEM_MIND_schoolage.rds", "SEM_MIND_preschool.rds",
           "SEM_SSC_schoolage.rds",  "SEM_SSC_preschool.rds")

  dat <- bind_rows(lapply(rds, function(f)
    .standardize_df(readRDS(file.path(data_src, f)), COMMON_COLS))) %>%
    mutate(
      sample_source = case_when(
        grepl("MIND", sample) ~ "MIND",
        grepl("SSC",  sample) ~ "SSC"
      ),
      age_group = case_when(
        grepl("preschool", sample) ~ "Preschool",
        grepl("schoolage", sample) ~ "School"
      ),
      sample_source_num = as.numeric(sample_source == "SSC")  # FIML auxiliary
    )

  # IQ ratio-policy filter (mirror of canonical). scale_type tagged always.
  if ("IQ_instrument" %in% names(dat)) {
    dat <- dat %>%
      mutate(scale_type = case_when(
        sample_source == "MIND" & IQ_instrument == "MSEL"  ~ "ratio",
        sample_source == "SSC"  & IQ_instrument == "ratio" ~ "ratio",
        !is.na(FSIQ)                                       ~ "deviation",
        TRUE                                               ~ NA_character_
      ))
    if (iq_policy == "deviation_only") {
      dat <- dat %>% filter(is.na(scale_type) | scale_type == "deviation")
    } else if (iq_policy == "ratio_only") {
      dat <- dat %>% filter(is.na(scale_type) | scale_type == "ratio")
    }  # "all" -> no filter
  }
  dat
}

#' The four per-subsample data frames the rest of the pipeline already uses.
build_subsamples <- function(dat) {
  list(
    MIND_preschool = dat %>% filter(sample_source == "MIND", age_group == "Preschool"),
    MIND_school    = dat %>% filter(sample_source == "MIND", age_group == "School"),
    SSC_preschool  = dat %>% filter(sample_source == "SSC",  age_group == "Preschool"),
    SSC_school     = dat %>% filter(sample_source == "SSC",  age_group == "School")
  )
}


# =============================================================================
# 2. FISHER r-to-z LENS
# =============================================================================
# Per replicate: girls' r is fixed (computed on all girls every rep); boys' r is
# computed on the matched boy subsample the engine hands in. Fisher-z their
# difference. estimate = r_girls - r_boys (+ve = girls higher, matching the
# direction-check convention). es = Cohen's q = z(r_girls) - z(r_boys).
# NOTE: the canonical Fisher script reports boys-girls; we flip sign so +ve =
# girls higher, consistent with the regression beta_4 (Female = 1) orientation.

.fz <- function(r) 0.5 * log((1 + r) / (1 - r))   # Fisher z (canonical `fz`)

fisher_lens <- function(d, sex_col = SEX_COL, female_level = FEMALE_LEVEL,
                        domains = FISHER_DOMAINS, min_pair_n = 4L) {
  g <- d[d[[sex_col]] == female_level, , drop = FALSE]
  b <- d[d[[sex_col]] != female_level & !is.na(d[[sex_col]]), , drop = FALSE]

  rows <- list()
  for (dom in names(domains)) {
    vars <- intersect(domains[[dom]], names(d))
    if (length(vars) < 2) next
    cb <- t(utils::combn(vars, 2))
    for (i in seq_len(nrow(cb))) {
      v1 <- cb[i, 1]; v2 <- cb[i, 2]
      okg <- stats::complete.cases(g[[v1]], g[[v2]])
      okb <- stats::complete.cases(b[[v1]], b[[v2]])
      ng <- sum(okg); nb <- sum(okb)
      if (ng <= min_pair_n + 2L || nb <= min_pair_n + 2L) next  # need n-3 > 0 both
      rg <- stats::cor(g[[v1]][okg], g[[v2]][okg])
      rb <- stats::cor(b[[v1]][okb], b[[v2]][okb])
      if (is.na(rg) || is.na(rb)) next
      z  <- (.fz(rg) - .fz(rb)) / sqrt(1 / (ng - 3) + 1 / (nb - 3))
      rows[[length(rows) + 1L]] <- tibble(
        term     = sprintf("%s | %s ~ %s", dom, v1, v2),
        estimate = rg - rb,                 # delta_r, +ve = girls higher
        es       = .fz(rg) - .fz(rb),       # Cohen's q (N-free)
        p_value  = 2 * stats::pnorm(-abs(z))
      )
    }
  }
  if (length(rows) == 0L) return(tibble(term = character(), estimate = numeric(),
                                        es = numeric(), p_value = numeric()))
  list_rbind(rows)
}


# =============================================================================
# 3. SEM COVARIANCE / LRT LENS
# (model SPEC + ESTIMATOR mirrored VERBATIM from SEM_cov_comparison.R)
# =============================================================================

# --- canonical helpers (verbatim mirrors) ----------------------------------
.build_model <- function(vars, aux = NULL) {
  n <- length(vars)
  lines <- c()
  for (v in vars) lines <- c(lines, paste0("  ", v, " ~~ ", v))
  lines <- c(lines, "")
  for (i in 1:(n - 1))
    for (j in (i + 1):n)
      lines <- c(lines, paste0("  ", vars[i], " ~~ ", vars[j]))
  lines <- c(lines, "")
  for (v in vars) lines <- c(lines, paste0("  ", v, " ~ 1"))
  if (!is.null(aux) && length(aux) > 0) {
    lines <- c(lines, "")
    for (a in aux) {
      lines <- c(lines, paste0("  ", a, " ~~ ", a))
      lines <- c(lines, paste0("  ", a, " ~ 1"))
    }
    for (a in aux)
      for (v in vars) lines <- c(lines, paste0("  ", a, " ~~ ", v))
    if (length(aux) > 1)
      for (i in 1:(length(aux) - 1))
        for (j in (i + 1):length(aux))
          lines <- c(lines, paste0("  ", aux[i], " ~~ ", aux[j]))
  }
  paste(lines, collapse = "\n")
}

.build_auxiliaries <- function(data, model_vars) {
  candidates <- c("age", "ADOS_Severity", "sample_source_num")
  candidates <- candidates[candidates %in% names(data)]
  if (length(candidates) == 0) return(character(0))
  candidates <- candidates[vapply(candidates, function(v) {
    vals <- data[[v]][!is.na(data[[v]])]
    length(unique(vals)) > 1
  }, logical(1))]
  candidates <- setdiff(candidates, model_vars)
  if (length(candidates) == 0) return(character(0))
  candidates <- candidates[vapply(candidates, function(v) {
    sum(!is.na(data[[v]])) >= 10
  }, logical(1))]
  candidates
}

.scale_auxiliaries <- function(dat_sub, aux) {
  for (a in aux) {
    if (a %in% names(dat_sub)) {
      sd_a <- sd(dat_sub[[a]], na.rm = TRUE)
      if (!is.na(sd_a) && sd_a > 0)
        dat_sub[[a]] <- as.numeric(scale(dat_sub[[a]]))
    }
  }
  dat_sub
}

.has_teacher <- function(data) {
  teacher_vars <- c("SRS_Total_Teacher", "SRS_Social_Teacher", "SRS_RRB_Teacher")
  all(teacher_vars %in% names(data)) &&
    all(sapply(data[teacher_vars], function(x) sum(!is.na(x)) >= 30))
}

.get_domain_vars <- function(data) {
  teacher <- .has_teacher(data)
  keep <- function(vars) {
    available <- vars[vars %in% names(data)]
    available <- available[sapply(available, function(v) sum(!is.na(data[[v]])) >= 30)]
    if (length(available) < 2) return(NULL)
    available
  }
  v_total  <- c("ADOS_Severity", "ADI_Total",      "SRS_Total_Parent")
  v_social <- c("ADOS_SA",       "ADI_Social_Com", "SRS_Social_Parent")
  v_rrb    <- c("ADOS_RRB",      "ADI_RRB",        "SRS_RRB_Parent", "RBSR_Total")
  if (teacher) {
    v_total  <- c(v_total,  "SRS_Total_Teacher")
    v_social <- c(v_social, "SRS_Social_Teacher")
    v_rrb    <- c(v_rrb,    "SRS_RRB_Teacher")
  }
  domains <- list(TOTAL = keep(v_total), SOCIAL = keep(v_social), RRB = keep(v_rrb))
  domains[!sapply(domains, is.null)]
}

# --- the lens ----------------------------------------------------------------
# Per replicate, per domain: fit the two-group (Male vs Female) saturated
# covariance model free vs covariance-constrained (FIML), compare via LRT.
# estimate = chi-square diff; es = delta-CFI (N-free); p_value = LRT p.
sem_lrt_lens <- function(d, sex_col = SEX_COL) {
  dat_sub <- d[d[[sex_col]] %in% c("Male", "Female"), , drop = FALSE]
  domains <- .get_domain_vars(dat_sub)
  if (length(domains) == 0) {
    return(tibble(term = character(), estimate = numeric(),
                  es = numeric(), p_value = numeric()))
  }
  rows <- list()
  for (dom in names(domains)) {
    vars <- domains[[dom]]
    n_m <- sum(dat_sub[[sex_col]] == "Male"); n_f <- sum(dat_sub[[sex_col]] == "Female")
    if (n_m < 10 || n_f < 10) next

    aux  <- .build_auxiliaries(dat_sub, vars)
    dsub <- .scale_auxiliaries(dat_sub, aux)
    model <- .build_model(vars, aux)

    # --- ESTIMATOR: exact canonical free / constrained FIML fits -------------
    fit_free <- tryCatch(
      sem(model, data = dsub, group = sex_col, group.label = c("Male", "Female"),
          missing = "fiml", fixed.x = FALSE, em.h1.iter.max = 5000),
      error = function(e) NULL)
    fit_con  <- tryCatch(
      sem(model, data = dsub, group = sex_col, group.label = c("Male", "Female"),
          missing = "fiml", fixed.x = FALSE, em.h1.iter.max = 5000,
          group.equal = c("residual.covariances", "intercepts")),
      error = function(e) NULL)
    if (is.null(fit_free) || is.null(fit_con)) next

    lr <- tryCatch(lavTestLRT(fit_free, fit_con), error = function(e) NULL)
    if (is.null(lr)) next

    cfi_free <- tryCatch(unname(fitMeasures(fit_free, "cfi")), error = function(e) NA_real_)
    cfi_con  <- tryCatch(unname(fitMeasures(fit_con,  "cfi")), error = function(e) NA_real_)
    # Free model is saturated-per-group (df = 0) -> CFI = 1 by convention.
    if (is.na(cfi_free) && isTRUE(unname(fitMeasures(fit_free, "df")) == 0)) cfi_free <- 1

    rows[[length(rows) + 1L]] <- tibble(
      term     = sprintf("%s | covariance_LRT", dom),
      estimate = as.numeric(lr[["Chisq diff"]][2]),   # chi-square difference
      es       = as.numeric(cfi_free - cfi_con),      # delta-CFI (N-free)
      p_value  = as.numeric(lr[["Pr(>Chisq)"]][2])
    )
  }
  if (length(rows) == 0L) return(tibble(term = character(), estimate = numeric(),
                                        es = numeric(), p_value = numeric()))
  list_rbind(rows)
}


# =============================================================================
# 4. HIERARCHICAL REGRESSION (M1-M4) MODERATION LENS
# (model SPEC + ESTIMATOR mirrored VERBATIM from section9_v2_pair_moderation.R)
# =============================================================================

# --- canonical helpers (verbatim mirrors) ----------------------------------
.select_aux <- function(data, model_vars, exclude_originals = character(0),
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

.scale_aux <- function(data, aux) {
  for (a in aux) {
    sd_a <- sd(data[[a]], na.rm = TRUE)
    if (!is.na(sd_a) && sd_a > 0) data[[a]] <- as.numeric(scale(data[[a]]))
  }
  data
}

.build_reg_model <- function(outcome, predictors, zero_pred = NULL, aux = NULL) {
  free_preds <- setdiff(predictors, zero_pred)
  all_vars   <- c(outcome, predictors)
  lines <- character(0)
  if (length(free_preds) > 0)
    lines <- c(lines, paste(outcome, "~", paste(free_preds, collapse = " + ")))
  for (zp in zero_pred) lines <- c(lines, paste0(outcome, " ~ 0*", zp))
  lines <- c(lines, "",
             paste0(all_vars, " ~~ ", all_vars),
             paste0(all_vars, " ~ 1"))
  if (!is.null(aux) && length(aux) > 0) {
    lines <- c(lines, "",
               paste0(aux, " ~~ ", aux),
               paste0(aux, " ~ 1"))
    for (a in aux) for (v in all_vars) lines <- c(lines, paste0(a, " ~~ ", v))
    if (length(aux) > 1)
      for (i in 1:(length(aux) - 1)) for (j in (i + 1):length(aux))
        lines <- c(lines, paste0(aux[i], " ~~ ", aux[j]))
  }
  paste(lines, collapse = "\n")
}

.fit_fiml <- function(data, model) {
  tryCatch(sem(model, data = data, missing = "fiml",
               fixed.x = FALSE, em.h1.iter.max = 5000),
           error = function(e) NULL)
}

.extract_coef <- function(fit, lhs, rhs) {
  if (is.null(fit)) return(rep(NA_real_, 6))
  pe <- parameterEstimates(fit)
  row <- pe[pe$lhs == lhs & pe$op == "~" & pe$rhs == rhs, ]
  if (nrow(row) == 0) return(rep(NA_real_, 6))
  c(row$est[1], row$se[1], row$z[1], row$pvalue[1], row$ci.lower[1], row$ci.upper[1])
}

.r2_out <- function(fit) tryCatch(lavInspect(fit, "r2")[["out"]],
                                  error = function(e) NA_real_)
# Cohen's f^2 for one nested term = (R2_full - R2_reduced) / (1 - R2_full).
.cohen_f2 <- function(r2_full, r2_reduced) {
  if (is.na(r2_full) || is.na(r2_reduced) || (1 - r2_full) <= 0) return(NA_real_)
  (r2_full - r2_reduced) / (1 - r2_full)
}

# --- the lens ----------------------------------------------------------------
# Per replicate, per ADOS~predictor pair: fit M2 (sex moderation) and, when an
# IQ variable is available, M4 (sex moderation surviving IQ). Returns the
# sex x tool interaction (beta_4) with Cohen's f^2 as the N-free effect size.
#
# iq_var: which IQ column feeds the regression. SURFACED, NOT DECIDED — ratio
#   vs deviation IQ for MIND Preschool is unresolved. Default "FSIQ"; pass NULL
#   to fit M2 only (no IQ models). No transform is hardcoded here.
regression_lens <- function(d, sex_col = SEX_COL, pairs = REG_PAIRS,
                            iq_var = "FSIQ") {
  d <- as.data.frame(d)
  d <- d[d[[sex_col]] %in% c("Male", "Female"), , drop = FALSE]
  d$sex_F <- as.numeric(d[[sex_col]] == "Female")

  zscore <- function(x) {
    s <- sd(x, na.rm = TRUE)
    if (is.na(s) || s == 0) return(x - mean(x, na.rm = TRUE))
    (x - mean(x, na.rm = TRUE)) / s
  }

  use_iq <- !is.null(iq_var) && iq_var %in% names(d) &&
    sum(!is.na(d[[iq_var]])) >= 20

  rows <- list()
  for (k in seq_len(nrow(pairs))) {
    outcome   <- pairs$outcome[k]
    predictor <- pairs$predictor[k]
    domain    <- pairs$domain[k]
    if (!(outcome %in% names(d)) || !(predictor %in% names(d))) next
    if (sum(!is.na(d[[outcome]]))   < 20) next
    if (sum(!is.na(d[[predictor]])) < 20) next
    if (sum(d$sex_F == 0) < 5 || sum(d$sex_F == 1) < 5) next

    dd <- d
    dd$age_c    <- zscore(dd$age)
    dd$pred_c   <- zscore(dd[[predictor]])
    dd$out      <- dd[[outcome]]
    dd$predXsex <- dd$pred_c * dd$sex_F

    excl <- c(outcome, "age", predictor)
    if (use_iq) excl <- c(excl, iq_var)

    # ---- M2: out ~ age + pred + sex + pred*sex ----------------------------
    aux2 <- .select_aux(dd, c("out", "age_c", "pred_c", "sex_F", "predXsex"),
                        exclude_originals = excl)
    d2 <- .scale_aux(dd, aux2)
    mod2_full <- .build_reg_model("out", c("age_c", "pred_c", "sex_F", "predXsex"), aux = aux2)
    mod2_null <- .build_reg_model("out", c("age_c", "pred_c", "sex_F", "predXsex"),
                                  zero_pred = "predXsex", aux = aux2)
    fit2_full <- .fit_fiml(d2, mod2_full)
    fit2_null <- .fit_fiml(d2, mod2_null)
    c2  <- .extract_coef(fit2_full, "out", "predXsex")
    f2_2 <- .cohen_f2(.r2_out(fit2_full), .r2_out(fit2_null))
    if (!is.na(c2[1]))
      rows[[length(rows) + 1L]] <- tibble(
        term     = sprintf("%s | %s ~ %s | M2_sexXtool", domain, outcome, predictor),
        estimate = c2[1],     # beta_4 (Pred x Sex), +ve = girls higher
        es       = f2_2,      # Cohen's f^2 for the sex x tool term (N-free)
        p_value  = c2[4]
      )

    # ---- M4: out ~ age + pred + sex + iq + pred*sex + pred*iq -------------
    if (use_iq) {
      dd$iq_c    <- zscore(dd[[iq_var]])
      dd$predXiq <- dd$pred_c * dd$iq_c
      preds4 <- c("age_c", "pred_c", "sex_F", "iq_c", "predXsex", "predXiq")
      aux4 <- .select_aux(dd, c("out", preds4), exclude_originals = excl)
      d4   <- .scale_aux(dd, aux4)
      mod4_full     <- .build_reg_model("out", preds4, aux = aux4)
      mod4_null_sex <- .build_reg_model("out", preds4, zero_pred = "predXsex", aux = aux4)
      fit4_full <- .fit_fiml(d4, mod4_full)
      fit4_null <- .fit_fiml(d4, mod4_null_sex)
      c4  <- .extract_coef(fit4_full, "out", "predXsex")
      f2_4 <- .cohen_f2(.r2_out(fit4_full), .r2_out(fit4_null))
      if (!is.na(c4[1]))
        rows[[length(rows) + 1L]] <- tibble(
          term     = sprintf("%s | %s ~ %s | M4_sexXtool|IQ", domain, outcome, predictor),
          estimate = c4[1],   # beta_4 surviving IQ control
          es       = f2_4,
          p_value  = c4[4]
        )
    }
  }
  if (length(rows) == 0L) return(tibble(term = character(), estimate = numeric(),
                                        es = numeric(), p_value = numeric()))
  list_rbind(rows)
}


# =============================================================================
# 5. SCOPE-AWARE FDR  (Benjamini-Hochberg, replaces Bonferroni as headline)
# =============================================================================
# run_balanced_pipeline() applies add_fdr() over the WHOLE table. That pools the
# three lenses' p-values into one family, which is rarely what you want. This
# helper re-derives the FDR within a clearly-named correction scope.
#
# SURFACED, NOT DECIDED — the open choice is the correction FAMILY/SCOPE:
#   "within_subsample" : family = all terms of a lens within one subsample
#                        (mirrors the canonical regression script's current
#                        group_by(Subsample)). [DEFAULT]
#   "within_domain"    : family = all terms of a lens within one (subsample,
#                        domain). Narrower; the open alternative.
#   "whole_table"      : one family across everything (what run_balanced_pipeline
#                        does by default).
# FDR is always computed per-lens (lenses are not pooled together). The author +
# Einat must confirm the scope before reporting. Nothing is hardcoded downstream.
apply_fdr_by_scope <- function(tbl,
                               scope = c("within_subsample", "within_domain",
                                         "whole_table"),
                               p_col = "p_median", out_col = "p_fdr") {
  scope <- match.arg(scope)
  if (!p_col %in% names(tbl)) {
    warning(p_col, " not present; skipping scope-aware FDR.")
    return(tbl)
  }
  # domain is the text before the first " | " in term (SEM term is the domain
  # itself, e.g. "TOTAL | covariance_LRT").
  tbl$domain <- trimws(sub("\\|.*$", "", tbl$term))

  grp <- switch(scope,
    within_subsample = c("lens", "subsample"),
    within_domain    = c("lens", "subsample", "domain"),
    whole_table      = c("lens"))

  tbl <- tbl %>%
    group_by(across(all_of(grp))) %>%
    mutate(!!out_col := stats::p.adjust(.data[[p_col]], method = "BH"),
           sig_fdr   := .data[[out_col]] < 0.05) %>%
    ungroup()
  attr(tbl, "fdr_scope") <- scope
  tbl
}


# =============================================================================
# 6. ONE-CALL DRIVER
# =============================================================================
#' Run all three lenses over sex-balanced resamples of all four subsamples and
#' return one tidy, FDR-corrected table.
#'
#' @param data_src   Dir with the four IQ-augmented RDS.
#' @param n_reps     1000 for the reportable run; ~20 for a smoke test.
#' @param seed       Base seed (replicate b uses seed + b).
#' @param iq_policy  IQ ratio policy for the data filter (see build_resample_data).
#' @param iq_var     IQ column for the regression lens (see regression_lens).
#' @param fdr_scope  Correction scope (see apply_fdr_by_scope). SURFACED.
run_resampled_lenses <- function(data_src   = "SEM_output/data_with_iq",
                                 n_reps     = 1000L,
                                 seed       = 1L,
                                 iq_policy  = "all",
                                 iq_var     = "FSIQ",
                                 fdr_scope  = "within_subsample") {
  dat        <- build_resample_data(data_src, iq_policy = iq_policy)
  subsamples <- build_subsamples(dat)

  lenses <- list(
    fisher     = function(d) fisher_lens(d),
    sem_lrt    = function(d) sem_lrt_lens(d),
    regression = function(d) regression_lens(d, iq_var = iq_var)
  )

  # DRAW-ONCE-THEN-TEST (Emilio): for each subsample, every replicate draws ONE
  # matched boy subsample and runs ALL THREE lenses on that same balanced frame,
  # so Fisher / SEM-LRT / regression see identical resampled subjects each rep.
  res <- purrr::imap(subsamples, function(sub_df, sub_name) {
    eng <- resample_all_lenses(
      data = sub_df, lenses = lenses,
      sex_col = SEX_COL, female_level = FEMALE_LEVEL,
      n_reps = n_reps, seed = seed, verbose = TRUE
    )
    if (nrow(eng$reps) == 0L) return(NULL)
    aggregate_resamples(eng$reps, by = c("lens", "term"),
                        value_cols = c("estimate", "es"), p_col = "p_value") %>%
      mutate(subsample = sub_name,
             n_f = eng$n_f, n_m = eng$n_m,
             n_reps = eng$n_reps, n_failed = eng$n_failed,
             balanced = eng$balanced, .before = 1)
  }) %>% purrr::compact() %>% purrr::list_rbind()

  # Scope-aware BH-FDR (per lens, within the chosen correction family).
  res <- apply_fdr_by_scope(res, scope = fdr_scope)

  # Task 4 — emit in manuscript order: covariance/LRT first (the headline
  # contrast), then Fisher (which specific pairs drive the covariance
  # difference), then regression (is the difference a function of sex or IQ).
  # Numbers are unchanged; only row ordering/labelling.
  lens_order <- c("sem_lrt", "fisher", "regression")
  sub_order  <- c("MIND_preschool", "MIND_school", "SSC_preschool", "SSC_school")
  res %>%
    mutate(lens      = factor(lens, levels = lens_order),
           subsample = factor(subsample, levels = sub_order)) %>%
    arrange(lens, subsample, term) %>%
    mutate(lens = as.character(lens), subsample = as.character(subsample))
}
