# =============================================================================
# SEM Multi-Group Measurement Invariance Analysis: Boys vs. Girls
# Thesis: Maternal Perception of Autism Traits (SRS-2)
# Author: Gal Or
# =============================================================================
#
# TWO CFA MODELS (run separately — totals and subscales can't be mixed):
#
#   Model A — "Total" (single general ASD factor):
#     ASD =~ ADOS_Severity + SRS_Total_Parent + RBSR_Total
#
#   Model B — "Subscale" (two correlated factors):
#     Social =~ ADOS_SA + SRS_Social_Parent
#     RRB    =~ ADOS_RRB + SRS_RRB_Parent + RBSR_Total
#
# NESTED ANALYSIS HIERARCHY:
#   1.  Full sample: Boys vs Girls
#   2a. MIND: Boys vs Girls
#   2b. SSC:  Boys vs Girls
#   3a. MIND Preschool: Boys vs Girls
#   3b. MIND School-age: Boys vs Girls
#   3c. SSC Preschool: Boys vs Girls
#   3d. SSC School-age: Boys vs Girls
#
# INVARIANCE STAGES:
#   Configural -> Metric -> Scalar -> Strict
#   Criteria: Chen (2007) — |dCFI| < .010, |dRMSEA| < .015
#
# Missing data: FIML  |  Estimator: MLR (robust ML)
# =============================================================================

# --- 1. PACKAGES --------------------------------------------------------------

required_packages <- c("lavaan", "semTools", "tidyverse", "semPlot", "effectsize")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# --- 2. LOAD & COMBINE DATA --------------------------------------------------

rds1 <- readRDS("~/Desktop/MIND/SEM_MIND_schoolage.rds")
rds2 <- readRDS("~/Desktop/MIND/SEM_MIND_preschool.rds")
rds3 <- readRDS("~/Desktop/MIND/SEM_SSC_schoolage.rds")
rds4 <- readRDS("~/Desktop/MIND/SEM_SSC_preschool.rds")

common_cols <- c("id", "sex", "age",
                 "ADOS_Severity", "ADOS_SA", "ADOS_RRB",
                 "SRS_Total_Parent", "SRS_Social_Parent", "SRS_RRB_Parent",
                 "RBSR_Total", "sample")

standardize_df <- function(df, cols) {
  df <- as.data.frame(df)
  available <- intersect(cols, names(df))
  df <- df[, available, drop = FALSE]
  num_cols <- setdiff(available, c("id", "sex", "sample"))
  for (col in num_cols) df[[col]] <- as.numeric(df[[col]])
  return(df)
}

dat <- bind_rows(
  standardize_df(rds1, common_cols),
  standardize_df(rds2, common_cols),
  standardize_df(rds3, common_cols),
  standardize_df(rds4, common_cols)
)

# --- 3. GROUPING VARIABLES ---------------------------------------------------

dat <- dat %>%
  mutate(
    sample_source = case_when(
      str_detect(sample, "MIND") ~ "MIND",
      str_detect(sample, "SSC")  ~ "SSC"
    ),
    age_group = case_when(
      str_detect(sample, "preschool") ~ "Preschool",
      str_detect(sample, "schoolage") ~ "School"
    )
  )

cat("=== SAMPLE OVERVIEW ===\n")
cat("Total N:", nrow(dat), "\n\n")
cat("Sex x Sample x Age:\n")
print(ftable(dat$sex, dat$sample_source, dat$age_group,
             dnn = c("Sex", "Sample", "Age")))

sem_vars <- c("ADOS_Severity", "ADOS_SA", "ADOS_RRB",
              "SRS_Total_Parent", "SRS_Social_Parent", "SRS_RRB_Parent",
              "RBSR_Total")

cat("\n=== MISSINGNESS ON SEM VARIABLES ===\n")
miss_tbl <- dat %>%
  group_by(sample_source, age_group) %>%
  summarise(N = n(),
            across(all_of(sem_vars), ~sum(is.na(.)), .names = "NA_{.col}"),
            .groups = "drop")
print(as.data.frame(miss_tbl))

# --- 4. CFA MODEL DEFINITIONS ------------------------------------------------

model_total <- '
  ASD =~ ADOS_Severity + SRS_Total_Parent + RBSR_Total
'

model_subscale <- '
  Social =~ ADOS_SA + SRS_Social_Parent
  RRB    =~ ADOS_RRB + SRS_RRB_Parent + RBSR_Total
  Social ~~ RRB
'

# --- 5. INVARIANCE TESTING FUNCTION ------------------------------------------

run_invariance <- function(data, group_var, model, label) {

  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("  ", label, "\n")
  cat("  Grouping:", group_var, "\n")
  cat(strrep("=", 70), "\n\n")

  cat("Group sizes:\n")
  print(table(data[[group_var]]))
  cat("\n")

  stages <- list(
    Configural = NULL,
    Metric     = c("loadings"),
    Scalar     = c("loadings", "intercepts"),
    Strict     = c("loadings", "intercepts", "residual.covariances")
  )

  fits <- list()

  for (stage_name in names(stages)) {
    cat(sprintf("--- %s ---\n", stage_name))
    fit <- tryCatch(
      cfa(model, data = data, group = group_var,
          group.equal = stages[[stage_name]],
          missing = "fiml", estimator = "MLR"),
      error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
    )
    if (!is.null(fit)) {
      fm <- fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                "cfi.robust", "rmsea.robust", "srmr"))
      cat(sprintf("  chi2=%.2f  df=%.0f  p=%.4f  CFI=%.4f  RMSEA=%.4f  SRMR=%.4f\n",
                  fm[1], fm[2], fm[3], fm[4], fm[5], fm[6]))
      fits[[stage_name]] <- fit
    }
  }

  if (length(fits) >= 2) {
    # Fit summary table
    cat("\n--- FIT SUMMARY ---\n")
    fit_table <- data.frame(
      Stage = names(fits),
      t(sapply(fits, function(m) {
        round(fitMeasures(m, c("chisq.scaled", "df.scaled",
                                "cfi.robust", "rmsea.robust", "srmr")), 4)
      })),
      row.names = NULL
    )
    print(fit_table, row.names = FALSE)

    # Sequential comparisons
    cat("\n--- SEQUENTIAL COMPARISONS (Chen, 2007) ---\n")
    nms <- names(fits)
    for (i in 2:length(nms)) {
      prev <- nms[i - 1]; curr <- nms[i]
      d_cfi   <- fitMeasures(fits[[curr]], "cfi.robust") -
                  fitMeasures(fits[[prev]], "cfi.robust")
      d_rmsea <- fitMeasures(fits[[curr]], "rmsea.robust") -
                  fitMeasures(fits[[prev]], "rmsea.robust")

      anova_res <- tryCatch(
        lavTestLRT(fits[[prev]], fits[[curr]], method = "satorra.bentler.2001"),
        error = function(e) NULL
      )

      tag_cfi   <- ifelse(abs(d_cfi)   < 0.010, "PASS", "FAIL")
      tag_rmsea <- ifelse(abs(d_rmsea) < 0.015, "PASS", "FAIL")

      cat(sprintf("  %s vs %s:\n", curr, prev))
      cat(sprintf("    dCFI = %+.4f [%s]   dRMSEA = %+.4f [%s]\n",
                  d_cfi, tag_cfi, d_rmsea, tag_rmsea))
      if (!is.null(anova_res)) {
        cat(sprintf("    Scaled chi2 diff = %.3f, df = %d, p = %.4f\n",
                    anova_res[["Chisq diff"]][2],
                    anova_res[["Df diff"]][2],
                    anova_res[["Pr(>Chisq)"]][2]))
      }
    }
  }

  cat("\n")
  return(fits)
}

# --- 6. LATENT MEAN EXTRACTION ------------------------------------------------

extract_latent_means <- function(data, group_var, model, label) {

  cat(sprintf("  Latent means — %s\n", label))

  fit <- tryCatch(
    cfa(model, data = data, group = group_var,
        group.equal = c("loadings", "intercepts"),
        missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("    ERROR:", e$message, "\n"); NULL }
  )
  if (is.null(fit)) return(NULL)

  groups <- lavInspect(fit, "group.label")
  cat(sprintf("    Ref (=0): %s  |  Comparison: %s\n", groups[1], groups[2]))

  pe <- parameterEstimates(fit, standardized = TRUE)
  lm_rows <- pe %>%
    filter(op == "~1", group == 2, lhs %in% c("ASD", "Social", "RRB"))

  for (r in seq_len(nrow(lm_rows))) {
    sig <- ifelse(lm_rows$pvalue[r] < .001, "***",
           ifelse(lm_rows$pvalue[r] < .01,  "**",
           ifelse(lm_rows$pvalue[r] < .05,  "*", "")))
    cat(sprintf("    %-7s: est = %+.3f, SE = %.3f, z = %+.3f, p = %.4f %s\n",
                lm_rows$lhs[r], lm_rows$est[r], lm_rows$se[r],
                lm_rows$z[r], lm_rows$pvalue[r], sig))
  }
  cat("\n")
  return(fit)
}

# =============================================================================
# --- 7. FULL ANALYSIS HIERARCHY ----------------------------------------------
# =============================================================================

all_results <- list()

run_both_models <- function(data, label_prefix, key) {

  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat("#  ", label_prefix, "\n")
  cat(strrep("#", 70), "\n")

  lab_A <- paste0(label_prefix, " | Total Model")
  lab_B <- paste0(label_prefix, " | Subscale Model")

  res_A <- run_invariance(data, "sex", model_total,    lab_A)
  res_B <- run_invariance(data, "sex", model_subscale, lab_B)

  cat("--- LATENT MEAN COMPARISONS ---\n")
  lm_A <- extract_latent_means(data, "sex", model_total,    lab_A)
  lm_B <- extract_latent_means(data, "sex", model_subscale, lab_B)

  all_results[[key]] <<- list(total = res_A, subscale = res_B)
}

# ==================== LEVEL 1: FULL SAMPLE ====================================
run_both_models(dat, "1. FULL SAMPLE", "Full")

# ==================== LEVEL 2: BY SAMPLE ======================================
dat_MIND <- dat %>% filter(sample_source == "MIND")
dat_SSC  <- dat %>% filter(sample_source == "SSC")

run_both_models(dat_MIND,
  sprintf("2a. MIND (N=%d: %d M, %d F)",
          nrow(dat_MIND),
          sum(dat_MIND$sex == "Male"),
          sum(dat_MIND$sex == "Female")),
  "MIND")

run_both_models(dat_SSC,
  sprintf("2b. SSC (N=%d: %d M, %d F)",
          nrow(dat_SSC),
          sum(dat_SSC$sex == "Male"),
          sum(dat_SSC$sex == "Female")),
  "SSC")

# ==================== LEVEL 3: SAMPLE x AGE (nested) =========================
dat_MIND_pre    <- dat %>% filter(sample_source == "MIND", age_group == "Preschool")
dat_MIND_school <- dat %>% filter(sample_source == "MIND", age_group == "School")
dat_SSC_pre     <- dat %>% filter(sample_source == "SSC",  age_group == "Preschool")
dat_SSC_school  <- dat %>% filter(sample_source == "SSC",  age_group == "School")

run_both_models(dat_MIND_pre,
  sprintf("3a. MIND x Preschool (N=%d: %d M, %d F)",
          nrow(dat_MIND_pre),
          sum(dat_MIND_pre$sex == "Male"),
          sum(dat_MIND_pre$sex == "Female")),
  "MIND_Pre")

run_both_models(dat_MIND_school,
  sprintf("3b. MIND x School-age (N=%d: %d M, %d F)",
          nrow(dat_MIND_school),
          sum(dat_MIND_school$sex == "Male"),
          sum(dat_MIND_school$sex == "Female")),
  "MIND_School")

run_both_models(dat_SSC_pre,
  sprintf("3c. SSC x Preschool (N=%d: %d M, %d F)",
          nrow(dat_SSC_pre),
          sum(dat_SSC_pre$sex == "Male"),
          sum(dat_SSC_pre$sex == "Female")),
  "SSC_Pre")

run_both_models(dat_SSC_school,
  sprintf("3d. SSC x School-age (N=%d: %d M, %d F)",
          nrow(dat_SSC_school),
          sum(dat_SSC_school$sex == "Male"),
          sum(dat_SSC_school$sex == "Female")),
  "SSC_School")

# --- 8. GRAND SUMMARY TABLE --------------------------------------------------

cat("\n\n")
cat(strrep("#", 70), "\n")
cat("#  GRAND SUMMARY TABLE\n")
cat(strrep("#", 70), "\n\n")

collect_fits <- function(fits_list, analysis_label, model_label) {
  if (is.null(fits_list) || length(fits_list) == 0) return(NULL)
  data.frame(
    Analysis  = analysis_label,
    CFA_Model = model_label,
    Stage     = names(fits_list),
    t(sapply(fits_list, function(m) {
      round(fitMeasures(m, c("chisq.scaled", "df.scaled",
                              "cfi.robust", "rmsea.robust", "srmr")), 4)
    })),
    row.names = NULL
  )
}

analysis_labels <- c(
  Full        = "1. Full Sample",
  MIND        = "2a. MIND",
  SSC         = "2b. SSC",
  MIND_Pre    = "3a. MIND Preschool",
  MIND_School = "3b. MIND School",
  SSC_Pre     = "3c. SSC Preschool",
  SSC_School  = "3d. SSC School"
)

summary_rows <- list()
for (key in names(all_results)) {
  lbl <- analysis_labels[[key]]
  summary_rows <- c(summary_rows, list(
    collect_fits(all_results[[key]]$total,    lbl, "Total"),
    collect_fits(all_results[[key]]$subscale, lbl, "Subscale")
  ))
}

summary_table <- bind_rows(summary_rows)
print(summary_table, row.names = FALSE)

cat("\n\nDone!\n")
cat("Key:\n")
cat("  |dCFI| < .010 AND |dRMSEA| < .015 => invariance holds (Chen, 2007)\n")
cat("  Scalar invariance required for latent mean comparisons.\n")
cat("  Reference = Male (means=0); Female means = difference from males.\n")