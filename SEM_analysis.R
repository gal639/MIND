# =============================================================================
# SEM Multi-Group Invariance Analysis: Boys vs. Girls
# Thesis: Maternal Perception of Autism Traits (SRS-2)
# Author: Gal Or
# =============================================================================

# --- 1. LOAD PACKAGES --------------------------------------------------------

required_packages <- c("lavaan", "semTools", "tidyverse", "semPlot", "effectsize")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# --- 2. LOAD DATA -------------------------------------------------------------

# UPDATE THESE PATHS to wherever your .rds files are stored
rds1 <- readRDS("~/Desktop/MIND/SEM_MIND_schoolage.rds")
rds2 <- readRDS("~/Desktop/MIND/SEM_MIND_preschool.rds")
rds3 <- readRDS("~/Desktop/MIND/SEM_SSC_schoolage.rds")
rds4 <- readRDS("~/Desktop/MIND/SEM_SSC_preschool.rds")

# --- 3. DIAGNOSTIC: RUN THIS FIRST AND SEND ME THE OUTPUT --------------------
# Copy-paste everything printed below back to me so I can build the full script.

cat("\n========== DIAGNOSTIC OUTPUT ==========\n\n")

diagnose_rds <- function(obj, label) {
  cat(paste0("--- ", label, " ---\n"))
  cat("Class:", paste(class(obj), collapse = ", "), "\n")
  cat("Dimensions:", paste(dim(obj), collapse = " x "), "\n")
  cat("N rows:", nrow(obj), "\n\n")
  
  cat("Column names:\n")
  print(names(obj))
  
  cat("\nColumn types:\n")
  print(sapply(obj, class))
  
  cat("\nFirst 3 rows:\n")
  print(head(obj, 3))
  
  # Check for likely grouping variables
  cat("\nPotential grouping variables (character/factor columns):\n")
  cat_cols <- names(obj)[sapply(obj, function(x) is.character(x) | is.factor(x))]
  for (col in cat_cols) {
    cat(paste0("  ", col, ": "), paste(unique(obj[[col]]), collapse = ", "), "\n")
  }
  
  cat("\nNumeric column summary:\n")
  num_cols <- names(obj)[sapply(obj, is.numeric)]
  if (length(num_cols) > 0) {
    print(summary(obj[, num_cols, drop = FALSE]))
  }
  
  cat("\n\n")
}

diagnose_rds(rds1, "FILE 1")
diagnose_rds(rds2, "FILE 2")
diagnose_rds(rds3, "FILE 3")
diagnose_rds(rds4, "FILE 4")

cat("========== END DIAGNOSTIC ==========\n")

# --- 4. SEM ANALYSIS (will be built after diagnostic) -------------------------
# =============================================================================
# SEM Multi-Group Measurement Invariance Analysis: Boys vs. Girls
# Thesis: Maternal Perception of Autism Traits (SRS-2)
# Author: Gal Or
# =============================================================================
# 
# ANALYSIS OVERVIEW:
# Two-factor CFA model (Social Communication vs. RRB) tested for measurement
# invariance across boys vs. girls in three contexts:
#   1. Full combined sample
#   2. By sample (MIND vs. SSC separately)
#   3. By age group (Preschool vs. School-age separately)
#
# Invariance stages tested:
#   Configural  — same factor structure, all parameters free
#   Metric      — factor loadings constrained equal
#   Scalar      — loadings + intercepts constrained equal
#   Strict      — loadings + intercepts + residual variances constrained
#
# Missing data handled with FIML (Full Information Maximum Likelihood)
# =============================================================================

# --- 1. LOAD PACKAGES --------------------------------------------------------

required_packages <- c("lavaan", "semTools", "tidyverse", "semPlot", "effectsize")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# --- 2. LOAD & COMBINE DATA --------------------------------------------------

# UPDATE THESE PATHS
rds1 <- readRDS("~/Desktop/MIND/SEM_MIND_schoolage.rds")
rds2 <- readRDS("~/Desktop/MIND/SEM_MIND_preschool.rds")
rds3 <- readRDS("~/Desktop/MIND/SEM_SSC_schoolage.rds")
rds4 <- readRDS("~/Desktop/MIND/SEM_SSC_preschool.rds")

# Select common columns and standardize
common_cols <- c("id", "sex", "age", 
                 "ADOS_Severity", "ADOS_SA", "ADOS_RRB",
                 "ADI_Social_Com", "ADI_RRB",
                 "SRS_Social_Parent", "SRS_RRB_Parent",
                 "RBSR_Total", "sample")

# Ensure all dataframes have the same columns and types
standardize_df <- function(df, cols) {
  df <- as.data.frame(df)
  available <- intersect(cols, names(df))
  df <- df[, available, drop = FALSE]
  num_cols <- setdiff(available, c("id", "sex", "sample"))
  for (col in num_cols) {
    df[[col]] <- as.numeric(df[[col]])
  }
  return(df)
}

d1 <- standardize_df(rds1, common_cols)
d2 <- standardize_df(rds2, common_cols)
d3 <- standardize_df(rds3, common_cols)
d4 <- standardize_df(rds4, common_cols)

# Combine all data
dat <- bind_rows(d1, d2, d3, d4)

# --- 3. CREATE GROUPING VARIABLES --------------------------------------------

dat <- dat %>%
  mutate(
    sample_source = case_when(
      str_detect(sample, "MIND") ~ "MIND",
      str_detect(sample, "SSC")  ~ "SSC",
      TRUE ~ NA_character_
    ),
    age_group = case_when(
      str_detect(sample, "preschool") ~ "Preschool",
      str_detect(sample, "schoolage") ~ "School",
      TRUE ~ NA_character_
    )
  )

# Sanity checks
cat("=== SAMPLE OVERVIEW ===\n")
cat("Total N:", nrow(dat), "\n\n")
print(table(dat$sex, dat$sample_source, dnn = c("Sex", "Sample")))
cat("\n")
print(table(dat$sex, dat$age_group, dnn = c("Sex", "Age Group")))
cat("\n")
print(table(dat$sex, dat$sample, dnn = c("Sex", "Full Sample Label")))

# Check missingness on SEM indicators
sem_vars <- c("ADOS_SA", "ADOS_RRB", 
              "SRS_Social_Parent", "SRS_RRB_Parent", 
              "RBSR_Total")

cat("\n=== MISSINGNESS ON SEM VARIABLES ===\n")
miss_table <- dat %>%
  group_by(sample_source, age_group) %>%
  summarise(
    N = n(),
    across(all_of(sem_vars), ~sum(is.na(.)), .names = "NA_{.col}"),
    .groups = "drop"
  )
print(as.data.frame(miss_table))

# --- 4. DEFINE CFA MODEL -----------------------------------------------------
# 
# Two-factor model: Social Communication vs. Restricted/Repetitive Behaviors
# Using multi-source indicators (clinician ADOS + parent SRS + parent RBSR)
#
# NOTE: ADI excluded — entirely missing for MIND schoolage (178/178 NA).
#       Including it would cause estimation failures for that subgroup.
#
# Social =~ ADOS_SA + SRS_Social_Parent         (2 indicators: clinician + parent)
# RRB    =~ ADOS_RRB + SRS_RRB_Parent + RBSR_Total  (3 indicators: clinician + 2x parent)
#
# Identification: With 5 observed variables, 2 factors correlated,
# df = 5(5+1)/2 - (5 loadings + 5 residuals + 1 covariance + 2 factor variances) = 15 - 13 = 2
# (first loading per factor fixed to 1 for scaling)

cfa_model <- '
  # Latent factors
  Social =~ ADOS_SA + SRS_Social_Parent
  RRB    =~ ADOS_RRB + SRS_RRB_Parent + RBSR_Total
  
  # Factor correlation (estimated by default)
  Social ~~ RRB
'

# --- 5. INVARIANCE TESTING FUNCTION -------------------------------------------

run_invariance <- function(data, group_var, model, label) {
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("  MEASUREMENT INVARIANCE:", label, "\n")
  cat("  Grouping variable:", group_var, "\n")
  cat(strrep("=", 70), "\n\n")
  
  # Print group sizes
  cat("Group sizes:\n")
  print(table(data[[group_var]]))
  cat("\n")
  
  # --- Configural ---
  cat("--- Stage 1: CONFIGURAL (same structure, free parameters) ---\n")
  fit_config <- tryCatch(
    cfa(model, data = data, group = group_var, 
        missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  
  if (is.null(fit_config)) return(NULL)
  cat("  Fit:\n")
  print(fitMeasures(fit_config, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                  "cfi.robust", "rmsea.robust", "srmr")))
  
  # --- Metric ---
  cat("\n--- Stage 2: METRIC (loadings constrained equal) ---\n")
  fit_metric <- tryCatch(
    cfa(model, data = data, group = group_var,
        group.equal = c("loadings"),
        missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(fit_metric)) {
    cat("  Fit:\n")
    print(fitMeasures(fit_metric, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                    "cfi.robust", "rmsea.robust", "srmr")))
  }
  
  # --- Scalar ---
  cat("\n--- Stage 3: SCALAR (loadings + intercepts constrained) ---\n")
  fit_scalar <- tryCatch(
    cfa(model, data = data, group = group_var,
        group.equal = c("loadings", "intercepts"),
        missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(fit_scalar)) {
    cat("  Fit:\n")
    print(fitMeasures(fit_scalar, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                    "cfi.robust", "rmsea.robust", "srmr")))
  }
  
  # --- Strict ---
  cat("\n--- Stage 4: STRICT (loadings + intercepts + residuals constrained) ---\n")
  fit_strict <- tryCatch(
    cfa(model, data = data, group = group_var,
        group.equal = c("loadings", "intercepts", "residual.covariances"),
        missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(fit_strict)) {
    cat("  Fit:\n")
    print(fitMeasures(fit_strict, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                    "cfi.robust", "rmsea.robust", "srmr")))
  }
  
  # --- Model Comparison ---
  cat("\n--- MODEL COMPARISON ---\n")
  
  models_list <- list(
    Configural = fit_config,
    Metric     = fit_metric,
    Scalar     = fit_scalar,
    Strict     = fit_strict
  )
  models_list <- models_list[!sapply(models_list, is.null)]
  
  if (length(models_list) >= 2) {
    
    # Fit indices table
    fit_table <- data.frame(
      Model = names(models_list),
      t(sapply(models_list, function(m) {
        fitMeasures(m, c("chisq.scaled", "df.scaled", 
                         "cfi.robust", "rmsea.robust", "srmr"))
      }))
    )
    cat("\nFit Indices Summary:\n")
    print(fit_table, digits = 3, row.names = FALSE)
    
    # Sequential comparisons with Chen (2007) criteria
    cat("\nSequential Comparisons (Chen, 2007 criteria):\n")
    cat("  |delta_CFI| < .010 AND |delta_RMSEA| < .015 => invariance holds\n\n")
    
    names_seq <- names(models_list)
    for (i in 2:length(names_seq)) {
      prev <- names_seq[i - 1]
      curr <- names_seq[i]
      
      cfi_prev   <- fitMeasures(models_list[[prev]], "cfi.robust")
      cfi_curr   <- fitMeasures(models_list[[curr]], "cfi.robust")
      rmsea_prev <- fitMeasures(models_list[[prev]], "rmsea.robust")
      rmsea_curr <- fitMeasures(models_list[[curr]], "rmsea.robust")
      
      d_cfi   <- cfi_curr - cfi_prev
      d_rmsea <- rmsea_curr - rmsea_prev
      
      # Satorra-Bentler scaled chi-sq difference
      anova_res <- tryCatch(
        lavTestLRT(models_list[[prev]], models_list[[curr]], 
                   method = "satorra.bentler.2001"),
        error = function(e) NULL
      )
      
      cat(sprintf("  %s vs %s:\n", curr, prev))
      cat(sprintf("    delta_CFI   = %+.4f  %s\n", d_cfi, 
                  ifelse(abs(d_cfi) < 0.010, "[PASS]", "[FAIL]")))
      cat(sprintf("    delta_RMSEA = %+.4f  %s\n", d_rmsea,
                  ifelse(abs(d_rmsea) < 0.015, "[PASS]", "[FAIL]")))
      
      if (!is.null(anova_res)) {
        cat(sprintf("    Scaled chi-sq diff = %.3f, df = %d, p = %.4f\n",
                    anova_res[["Chisq diff"]][2],
                    anova_res[["Df diff"]][2],
                    anova_res[["Pr(>Chisq)"]][2]))
      }
      cat("\n")
    }
    
    # Full LRT table
    cat("\nFull Scaled Chi-Square Difference Tests:\n")
    tryCatch({
      anova_full <- do.call(lavTestLRT, 
                            c(models_list, list(method = "satorra.bentler.2001")))
      print(anova_full)
    }, error = function(e) cat("  Could not compute:", e$message, "\n"))
  }
  
  cat("\n")
  return(models_list)
}

# --- 6. RUN ALL INVARIANCE ANALYSES -------------------------------------------

# ============================================================
# 6A. FULL SAMPLE: Boys vs Girls
# ============================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("#  ANALYSIS 1: ALL BOYS vs ALL GIRLS (full combined sample, N=3236)\n")
cat(strrep("#", 70), "\n")

results_all <- run_invariance(dat, "sex", cfa_model, 
                              "Boys vs Girls — Full Sample")

# ============================================================
# 6B. BY SAMPLE: MIND then SSC
# ============================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("#  ANALYSIS 2: BOYS vs GIRLS BY SAMPLE\n")
cat(strrep("#", 70), "\n")

dat_MIND <- dat %>% filter(sample_source == "MIND")
dat_SSC  <- dat %>% filter(sample_source == "SSC")

results_MIND <- run_invariance(dat_MIND, "sex", cfa_model, 
                               "Boys vs Girls — MIND only")
results_SSC  <- run_invariance(dat_SSC,  "sex", cfa_model, 
                               "Boys vs Girls — SSC only")

# ============================================================
# 6C. BY AGE GROUP: Preschool then School-age
# ============================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("#  ANALYSIS 3: BOYS vs GIRLS BY AGE GROUP\n")
cat(strrep("#", 70), "\n")

dat_pre    <- dat %>% filter(age_group == "Preschool")
dat_school <- dat %>% filter(age_group == "School")

results_pre    <- run_invariance(dat_pre,    "sex", cfa_model, 
                                 "Boys vs Girls — Preschool only")
results_school <- run_invariance(dat_school, "sex", cfa_model, 
                                 "Boys vs Girls — School-age only")

# --- 7. LATENT MEAN COMPARISONS (where scalar invariance holds) ---------------

cat("\n\n")
cat(strrep("#", 70), "\n")
cat("#  LATENT MEAN COMPARISONS\n")
cat("#  (Only interpretable if scalar invariance holds for that analysis)\n")
cat(strrep("#", 70), "\n\n")

extract_latent_means <- function(data, group_var, model, label) {
  
  cat(sprintf("\n--- Latent Means: %s ---\n", label))
  
  fit <- tryCatch(
    cfa(model, data = data, group = group_var,
        group.equal = c("loadings", "intercepts"),
        missing = "fiml", estimator = "MLR"),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  
  if (is.null(fit)) return(NULL)
  
  groups <- lavInspect(fit, "group.label")
  cat("  Reference group (means fixed to 0):", groups[1], "\n")
  cat("  Comparison group:", groups[2], "\n\n")
  
  # Latent means for each group
  means_est <- lavInspect(fit, "mean.lv")
  for (g in seq_along(groups)) {
    cat(sprintf("  %s latent means:\n", groups[g]))
    print(means_est[[g]])
    cat("\n")
  }
  
  # Statistical test of mean differences
  pe <- parameterEstimates(fit, standardized = TRUE)
  latent_means <- pe %>% 
    filter(op == "~1" & lhs %in% c("Social", "RRB") & group == 2)
  
  if (nrow(latent_means) >= 2) {
    cat("  Latent mean differences (comparison - reference):\n")
    cat(sprintf("    Social: est = %.3f, SE = %.3f, z = %.3f, p = %.4f\n",
                latent_means$est[1], latent_means$se[1], 
                latent_means$z[1], latent_means$pvalue[1]))
    cat(sprintf("    RRB:    est = %.3f, SE = %.3f, z = %.3f, p = %.4f\n",
                latent_means$est[2], latent_means$se[2], 
                latent_means$z[2], latent_means$pvalue[2]))
  }
  cat("\n")
  
  return(fit)
}

lm_all    <- extract_latent_means(dat,        "sex", cfa_model, "Full Sample")
lm_MIND   <- extract_latent_means(dat_MIND,   "sex", cfa_model, "MIND only")
lm_SSC    <- extract_latent_means(dat_SSC,    "sex", cfa_model, "SSC only")
lm_pre    <- extract_latent_means(dat_pre,    "sex", cfa_model, "Preschool only")
lm_school <- extract_latent_means(dat_school, "sex", cfa_model, "School-age only")

# --- 8. GRAND SUMMARY TABLE --------------------------------------------------

cat("\n\n")
cat(strrep("#", 70), "\n")
cat("#  GRAND SUMMARY TABLE\n")
cat(strrep("#", 70), "\n\n")

collect_fits <- function(models_list, label) {
  if (is.null(models_list)) return(NULL)
  data.frame(
    Analysis = label,
    Model = names(models_list),
    t(sapply(models_list, function(m) {
      round(fitMeasures(m, c("chisq.scaled", "df.scaled", 
                             "cfi.robust", "rmsea.robust", "srmr")), 4)
    })),
    row.names = NULL
  )
}

summary_table <- bind_rows(
  collect_fits(results_all,    "Full Sample"),
  collect_fits(results_MIND,   "MIND only"),
  collect_fits(results_SSC,    "SSC only"),
  collect_fits(results_pre,    "Preschool"),
  collect_fits(results_school, "School-age")
)

print(summary_table, row.names = FALSE)

cat("\n\nDone!\n")
cat("Interpretation key:\n")
cat("  |delta_CFI| < .010 AND |delta_RMSEA| < .015 => invariance holds (Chen, 2007)\n")
cat("  If scalar invariance holds, latent mean comparisons are interpretable.\n")
cat("  Males are typically the reference group (means = 0); Female means show the difference.\n")