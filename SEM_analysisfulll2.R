# =============================================================================
# Covariance Matrix Comparison: Boys vs. Girls
# Direct test of whether the variance-covariance matrices differ by sex
# =============================================================================
#
# TWO APPROACHES:
#   1. Box's M test — classical omnibus test of covariance matrix equality
#   2. Multi-group SEM — saturated (free) vs. constrained (equal) covariance
#      matrices, tested with likelihood ratio in lavaan
#
# HIERARCHY (same nested structure):
#   1.  Full sample
#   2a. MIND
#   2b. SSC
#   3a. MIND × Preschool
#   3b. MIND × School-age
#   3c. SSC × Preschool
#   3d. SSC × School-age
# =============================================================================

# --- 1. PACKAGES --------------------------------------------------------------

required_packages <- c("lavaan", "tidyverse", "biotools", "heplots")

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

# --- 3. DEFINE VARIABLE SETS -------------------------------------------------

# Total scores set
vars_total <- c("ADOS_Severity", "SRS_Total_Parent", "RBSR_Total")

# Subscale scores set
vars_subscale <- c("ADOS_SA", "ADOS_RRB",
                   "SRS_Social_Parent", "SRS_RRB_Parent",
                   "RBSR_Total")

# --- 4. COMPARISON FUNCTION --------------------------------------------------

compare_cov_matrices <- function(data, group_var, vars, label) {
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("  ", label, "\n")
  cat("  Variables:", paste(vars, collapse = ", "), "\n")
  cat(strrep("=", 70), "\n\n")
  
  # --- Group sizes ---
  cat("Group sizes:\n")
  print(table(data[[group_var]]))
  cat("\n")
  
  # --- Print covariance matrices per group ---
  groups <- unique(data[[group_var]])
  for (g in groups) {
    sub <- data[data[[group_var]] == g, vars, drop = FALSE]
    sub_complete <- na.omit(sub)
    cat(sprintf("Covariance matrix — %s (N complete = %d):\n", g, nrow(sub_complete)))
    print(round(cov(sub_complete), 3))
    cat("\n")
  }
  
  # =========================================================
  # APPROACH 1: Box's M Test
  # =========================================================
  cat("--- APPROACH 1: Box's M Test ---\n")
  
  # Box's M needs complete cases
  complete_data <- data[complete.cases(data[, c(vars, group_var)]), ]
  cat(sprintf("  N after listwise deletion: %d\n", nrow(complete_data)))
  
  box_result <- tryCatch({
    # biotools::boxM
    result <- boxM(complete_data[, vars], complete_data[[group_var]])
    cat(sprintf("  Box's M statistic = %.3f\n", result$statistic))
    cat(sprintf("  Approx F  = %.3f\n", result$statistic))  # it's actually chi-sq
    cat(sprintf("  df        = %.0f\n", result$parameter))
    cat(sprintf("  p-value   = %.6f\n", result$p.value))
    
    if (result$p.value < .05) {
      cat("  --> SIGNIFICANT: Covariance matrices differ between groups.\n")
    } else {
      cat("  --> NOT significant: No evidence covariance matrices differ.\n")
    }
    result
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    NULL
  })
  
  cat("\n")
  
  # =========================================================
  # APPROACH 2: Multi-group SEM (Saturated vs. Constrained)
  # =========================================================
  cat("--- APPROACH 2: Multi-group lavaan (saturated vs. constrained) ---\n\n")
  
  # Build a saturated model: all variances, covariances, and means freely estimated
  # In lavaan, a "saturated" model just estimates all means and covariances
  # No latent factors — just the observed covariance structure.
  
  # Model: all observed variables covary freely (lavaan estimates this by default
  # with no factor structure if we just list the covariances)
  n_vars <- length(vars)
  
  # Build explicit covariance model
  cov_lines <- c()
  for (i in 1:n_vars) {
    # Variance
    cov_lines <- c(cov_lines, paste0(vars[i], " ~~ ", vars[i]))
    # Covariances
    if (i < n_vars) {
      for (j in (i + 1):n_vars) {
        cov_lines <- c(cov_lines, paste0(vars[i], " ~~ ", vars[j]))
      }
    }
    # Intercept (mean)
    cov_lines <- c(cov_lines, paste0(vars[i], " ~ 1"))
  }
  sat_model <- paste(cov_lines, collapse = "\n")
  
  # Fit 1: FREE model — covariance matrices estimated separately per group
  cat("  Model 1 (FREE): Separate covariance matrices per group\n")
  fit_free <- tryCatch(
    lavaan(sat_model, data = data, group = group_var,
           missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("    ERROR:", e$message, "\n"); NULL }
  )
  
  if (!is.null(fit_free)) {
    fm_free <- fitMeasures(fit_free, c("chisq.scaled", "df.scaled",
                                       "logl", "aic", "bic"))
    cat(sprintf("    chi2 = %.2f, df = %.0f, logLik = %.2f, AIC = %.1f\n",
                fm_free[1], fm_free[2], fm_free[3], fm_free[4]))
  }
  
  # Fit 2: CONSTRAINED model — covariance matrices constrained equal across groups
  cat("  Model 2 (EQUAL): Covariance matrices constrained equal\n")
  fit_equal <- tryCatch(
    lavaan(sat_model, data = data, group = group_var,
           group.equal = c("residual.covariances", "intercepts"),
           missing = "fiml", estimator = "MLR"),
    error = function(e) {
      # Try with just covariances, not intercepts
      tryCatch(
        lavaan(sat_model, data = data, group = group_var,
               group.equal = c("residual.covariances"),
               missing = "fiml", estimator = "MLR"),
        error = function(e2) { cat("    ERROR:", e2$message, "\n"); NULL }
      )
    }
  )
  
  if (!is.null(fit_equal)) {
    fm_eq <- fitMeasures(fit_equal, c("chisq.scaled", "df.scaled",
                                      "logl", "aic", "bic"))
    cat(sprintf("    chi2 = %.2f, df = %.0f, logLik = %.2f, AIC = %.1f\n",
                fm_eq[1], fm_eq[2], fm_eq[3], fm_eq[4]))
  }
  
  # Likelihood Ratio Test
  if (!is.null(fit_free) && !is.null(fit_equal)) {
    cat("\n  Likelihood Ratio Test (Satorra-Bentler scaled):\n")
    lrt <- tryCatch(
      lavTestLRT(fit_free, fit_equal, method = "satorra.bentler.2001"),
      error = function(e) {
        # Fallback: try without S-B correction
        tryCatch(
          lavTestLRT(fit_free, fit_equal),
          error = function(e2) { cat("    ERROR:", e2$message, "\n"); NULL }
        )
      }
    )
    if (!is.null(lrt)) {
      print(lrt)
      p_val <- lrt[["Pr(>Chisq)"]][2]
      cat(sprintf("\n  --> p = %.6f", p_val))
      if (p_val < .05) {
        cat(" — SIGNIFICANT: Covariance matrices differ between groups.\n")
      } else {
        cat(" — NOT significant: No evidence matrices differ.\n")
      }
    }
  }
  
  # =========================================================
  # APPROACH 3: Test covariances AND means separately
  # =========================================================
  cat("\n--- APPROACH 3: Separate tests for MEANS vs. COVARIANCES ---\n")
  
  # 3a: Equal covariances only (means free)
  cat("  3a. Constrain covariances only (means free):\n")
  fit_cov_only <- tryCatch(
    lavaan(sat_model, data = data, group = group_var,
           group.equal = c("residual.covariances"),
           missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("    ERROR:", e$message, "\n"); NULL }
  )
  
  if (!is.null(fit_free) && !is.null(fit_cov_only)) {
    lrt_cov <- tryCatch(
      lavTestLRT(fit_free, fit_cov_only, method = "satorra.bentler.2001"),
      error = function(e) tryCatch(lavTestLRT(fit_free, fit_cov_only),
                                   error = function(e2) NULL)
    )
    if (!is.null(lrt_cov)) {
      p_cov <- lrt_cov[["Pr(>Chisq)"]][2]
      cat(sprintf("    chi2 diff p = %.6f %s\n", p_cov,
                  ifelse(p_cov < .05, "— COVARIANCES DIFFER", "— covariances equal")))
    }
  }
  
  # 3b: Equal means only (covariances free)
  cat("  3b. Constrain means only (covariances free):\n")
  fit_mean_only <- tryCatch(
    lavaan(sat_model, data = data, group = group_var,
           group.equal = c("intercepts"),
           missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("    ERROR:", e$message, "\n"); NULL }
  )
  
  if (!is.null(fit_free) && !is.null(fit_mean_only)) {
    lrt_mean <- tryCatch(
      lavTestLRT(fit_free, fit_mean_only, method = "satorra.bentler.2001"),
      error = function(e) tryCatch(lavTestLRT(fit_free, fit_mean_only),
                                   error = function(e2) NULL)
    )
    if (!is.null(lrt_mean)) {
      p_mean <- lrt_mean[["Pr(>Chisq)"]][2]
      cat(sprintf("    chi2 diff p = %.6f %s\n", p_mean,
                  ifelse(p_mean < .05, "— MEANS DIFFER", "— means equal")))
    }
  }
  
  cat("\n")
}

# --- 5. RUN THE FULL HIERARCHY ------------------------------------------------

run_all <- function(data, label_prefix) {
  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat("#  ", label_prefix, "\n")
  cat(strrep("#", 70), "\n")
  
  compare_cov_matrices(data, "sex", vars_total,
                       paste0(label_prefix, " | TOTAL scores"))
  compare_cov_matrices(data, "sex", vars_subscale,
                       paste0(label_prefix, " | SUBSCALE scores"))
}

# Level 1: Full sample
run_all(dat, "1. FULL SAMPLE")

# Level 2: By sample
dat_MIND <- dat %>% filter(sample_source == "MIND")
dat_SSC  <- dat %>% filter(sample_source == "SSC")

run_all(dat_MIND, sprintf("2a. MIND (N=%d)", nrow(dat_MIND)))
run_all(dat_SSC,  sprintf("2b. SSC (N=%d)",  nrow(dat_SSC)))

# Level 3: Sample × Age
run_all(dat %>% filter(sample_source == "MIND", age_group == "Preschool"),
        "3a. MIND x Preschool")
run_all(dat %>% filter(sample_source == "MIND", age_group == "School"),
        "3b. MIND x School-age")
run_all(dat %>% filter(sample_source == "SSC",  age_group == "Preschool"),
        "3c. SSC x Preschool")
run_all(dat %>% filter(sample_source == "SSC",  age_group == "School"),
        "3d. SSC x School-age")

cat("\n\n=== DONE ===\n")
cat("Summary:\n")
cat("  - Box's M: Classical test, requires complete cases (listwise deletion)\n")
cat("  - lavaan LRT: Uses FIML (no listwise deletion), robust to non-normality\n")
cat("  - Approach 3 separates covariance differences from mean differences\n")
cat("  - If covariance matrices differ → the CFA invariance cascade tells you WHERE\n")