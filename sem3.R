# =============================================================================
# SEM Covariance Matrix Comparison: Boys vs. Girls
# Multi-group saturated SEM — test equality of covariance matrices
# =============================================================================
#
# METHOD:
#   Fit a saturated (no latent factors) multi-group SEM in lavaan.
#   Model 1 (Baseline):  Covariance matrices estimated freely per group
#   Model 2 (Equal Cov): Covariance matrices constrained equal across groups
#   Model 3 (Equal All):  Covariances AND means constrained equal
#   Compare via Satorra-Bentler scaled likelihood ratio test.
#
# VARIABLE SETS:
#   Total:    ADOS_Severity, SRS_Total_Parent, RBSR_Total
#   Subscale: ADOS_SA, ADOS_RRB, SRS_Social_Parent, SRS_RRB_Parent, RBSR_Total
#
# HIERARCHY:
#   1.  Full sample
#   2a. MIND  |  2b. SSC
#   3a. MIND × Preschool  |  3b. MIND × School
#   3c. SSC × Preschool   |  3d. SSC × School
#
# Missing data: FIML  |  Estimator: MLR
# =============================================================================

# --- 1. PACKAGES --------------------------------------------------------------

library(lavaan)
library(tidyverse)

# --- 2. LOAD DATA -------------------------------------------------------------

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

# --- 3. BUILD SATURATED SEM MODEL --------------------------------------------
# A saturated model = all variances, covariances, and means freely estimated.
# No latent variables. This captures the FULL covariance matrix.

build_saturated_model <- function(vars) {
  lines <- c()
  n <- length(vars)
  for (i in 1:n) {
    # Variance
    lines <- c(lines, paste0(vars[i], " ~~ ", vars[i]))
    # Covariances with all other variables
    if (i < n) {
      for (j in (i + 1):n) {
        lines <- c(lines, paste0(vars[i], " ~~ ", vars[j]))
      }
    }
    # Mean (intercept)
    lines <- c(lines, paste0(vars[i], " ~ 1"))
  }
  paste(lines, collapse = "\n")
}

# --- 4. COMPARISON FUNCTION ---------------------------------------------------

compare_covariances <- function(data, vars, label) {
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("  ", label, "\n")
  cat(strrep("=", 70), "\n\n")
  
  # Group sizes
  cat("Group sizes:\n")
  print(table(data$sex))
  cat("\n")
  
  # Print observed covariance matrices
  for (g in c("Male", "Female")) {
    sub <- data[data$sex == g, vars, drop = FALSE]
    sub_complete <- na.omit(sub)
    cat(sprintf("Covariance matrix — %s (N = %d, complete = %d):\n",
                g, sum(data$sex == g), nrow(sub_complete)))
    print(round(cov(sub_complete), 2))
    cat("\nCorrelation matrix — ", g, ":\n")
    print(round(cor(sub_complete), 3))
    cat("\n")
  }
  
  sat_model <- build_saturated_model(vars)
  
  # --- Model 1: BASELINE (free) ---
  # All parameters estimated separately for each group
  cat("--- Model 1: BASELINE (free covariance matrices) ---\n")
  fit_free <- tryCatch(
    sem(sat_model, data = data, group = "sex",
        missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(fit_free)) {
    cat(sprintf("  logLik = %.2f, AIC = %.1f, npar = %d\n",
                fitMeasures(fit_free, "logl"),
                fitMeasures(fit_free, "aic"),
                fitMeasures(fit_free, "npar")))
  }
  
  # --- Model 2: EQUAL COVARIANCES (means free) ---
  # Covariance matrix constrained equal; means still free per group
  cat("\n--- Model 2: EQUAL COVARIANCES (means free) ---\n")
  fit_eq_cov <- tryCatch(
    sem(sat_model, data = data, group = "sex",
        group.equal = c("residual.covariances"),
        missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(fit_eq_cov)) {
    cat(sprintf("  logLik = %.2f, AIC = %.1f, npar = %d\n",
                fitMeasures(fit_eq_cov, "logl"),
                fitMeasures(fit_eq_cov, "aic"),
                fitMeasures(fit_eq_cov, "npar")))
  }
  
  # --- Model 3: EQUAL MEANS (covariances free) ---
  # Means constrained equal; covariance matrix free per group
  cat("\n--- Model 3: EQUAL MEANS (covariances free) ---\n")
  fit_eq_mean <- tryCatch(
    sem(sat_model, data = data, group = "sex",
        group.equal = c("intercepts"),
        missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(fit_eq_mean)) {
    cat(sprintf("  logLik = %.2f, AIC = %.1f, npar = %d\n",
                fitMeasures(fit_eq_mean, "logl"),
                fitMeasures(fit_eq_mean, "aic"),
                fitMeasures(fit_eq_mean, "npar")))
  }
  
  # --- Model 4: EQUAL EVERYTHING (covariances + means) ---
  cat("\n--- Model 4: EQUAL COVARIANCES + MEANS ---\n")
  fit_eq_all <- tryCatch(
    sem(sat_model, data = data, group = "sex",
        group.equal = c("residual.covariances", "intercepts"),
        missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(fit_eq_all)) {
    cat(sprintf("  logLik = %.2f, AIC = %.1f, npar = %d\n",
                fitMeasures(fit_eq_all, "logl"),
                fitMeasures(fit_eq_all, "aic"),
                fitMeasures(fit_eq_all, "npar")))
  }
  
  # --- LIKELIHOOD RATIO TESTS ---
  cat("\n--- LIKELIHOOD RATIO TESTS (Satorra-Bentler) ---\n\n")
  
  do_lrt <- function(m_free, m_constrained, test_label) {
    if (is.null(m_free) || is.null(m_constrained)) {
      cat(sprintf("  %s: could not compute (model error)\n", test_label))
      return(NULL)
    }
    lrt <- tryCatch(
      lavTestLRT(m_free, m_constrained, method = "satorra.bentler.2001"),
      error = function(e) {
        tryCatch(lavTestLRT(m_free, m_constrained),
                 error = function(e2) NULL)
      }
    )
    if (!is.null(lrt)) {
      chi_diff <- lrt[["Chisq diff"]][2]
      df_diff  <- lrt[["Df diff"]][2]
      p_val    <- lrt[["Pr(>Chisq)"]][2]
      
      sig <- ifelse(p_val < .001, "***",
                    ifelse(p_val < .01,  "**",
                           ifelse(p_val < .05,  "*", "n.s.")))
      
      cat(sprintf("  %s:\n", test_label))
      cat(sprintf("    chi2 diff = %.3f, df = %d, p = %.6f %s\n",
                  chi_diff, df_diff, p_val, sig))
      
      if (p_val < .05) {
        cat("    --> SIGNIFICANT DIFFERENCE\n\n")
      } else {
        cat("    --> No significant difference\n\n")
      }
    }
    return(lrt)
  }
  
  # Test A: Do COVARIANCE MATRICES differ? (Model 1 vs Model 2)
  do_lrt(fit_free, fit_eq_cov,
         "Test A: Covariance matrices equal? (Baseline vs Equal-Cov)")
  
  # Test B: Do MEANS differ? (Model 1 vs Model 3)
  do_lrt(fit_free, fit_eq_mean,
         "Test B: Means equal? (Baseline vs Equal-Means)")
  
  # Test C: Does EVERYTHING differ? (Model 1 vs Model 4)
  do_lrt(fit_free, fit_eq_all,
         "Test C: Full equality? (Baseline vs Equal-All)")
  
  cat("\n")
}

# --- 5. RUN FULL HIERARCHY ---------------------------------------------------

run_both_sets <- function(data, label) {
  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat("#  ", label, "\n")
  cat(strrep("#", 70), "\n")
  
  compare_covariances(data, vars_total,
                      paste0(label, " | TOTAL scores"))
  compare_covariances(data, vars_subscale,
                      paste0(label, " | SUBSCALE scores"))
}

# ==================== LEVEL 1 ====================
run_both_sets(dat, "1. FULL SAMPLE")

# ==================== LEVEL 2 ====================
dat_MIND <- dat %>% filter(sample_source == "MIND")
dat_SSC  <- dat %>% filter(sample_source == "SSC")

run_both_sets(dat_MIND, sprintf("2a. MIND (N=%d)", nrow(dat_MIND)))
run_both_sets(dat_SSC,  sprintf("2b. SSC (N=%d)",  nrow(dat_SSC)))

# ==================== LEVEL 3 ====================
run_both_sets(dat %>% filter(sample_source == "MIND", age_group == "Preschool"),
              "3a. MIND x Preschool")
run_both_sets(dat %>% filter(sample_source == "MIND", age_group == "School"),
              "3b. MIND x School-age")
run_both_sets(dat %>% filter(sample_source == "SSC",  age_group == "Preschool"),
              "3c. SSC x Preschool")
run_both_sets(dat %>% filter(sample_source == "SSC",  age_group == "School"),
              "3d. SSC x School-age")

# --- 6. SUMMARY ---------------------------------------------------------------

cat("\n\n")
cat(strrep("#", 70), "\n")
cat("#  INTERPRETATION GUIDE\n")
cat(strrep("#", 70), "\n\n")
cat("Test A (Covariance matrices equal?):\n")
cat("  Significant = the RELATIONSHIPS between variables differ by sex.\n")
cat("  e.g., ADOS and SRS correlate differently for boys vs girls.\n\n")
cat("Test B (Means equal?):\n")
cat("  Significant = the AVERAGE LEVELS of variables differ by sex.\n")
cat("  e.g., boys have higher/lower mean SRS scores than girls.\n\n")
cat("Test C (Full equality?):\n")
cat("  Significant = SOMETHING differs (means, covariances, or both).\n")
cat("  If A is significant but B is not: relationships differ, levels don't.\n")
cat("  If B is significant but A is not: levels differ, relationships don't.\n")
cat("  If both significant: both structure and levels differ.\n\n")
cat("Next step: Use the CFA invariance script to localize WHERE the\n")
cat("covariance differences lie (loadings? intercepts? residuals?).\n")