# =============================================================================
# Missing Data Assessment — Prerequisite for FIML
# Tests: Little's MCAR, missingness patterns, MAR probe, MNAR risk flags
# Output: ~/Desktop/MIND/SEM_output/missing/
# =============================================================================
# Packages needed:
#   install.packages(c("naniar", "mice", "misty", "ggplot2", "tidyverse",
#                      "patchwork", "ggupset", "gt", "writexl"))
# =============================================================================

library(tidyverse)
library(naniar)       # vis_miss, mcar_test, gg_miss_var, gg_miss_upset
library(mice)         # md.pattern
library(patchwork)
library(gt)           # pretty tables
library(writexl)      # Excel export of summary tables

# --------------------------------------------------------------------------- #
# 0. SETUP                                                                     #
# --------------------------------------------------------------------------- #

output_dir  <- "~/Desktop/MIND/SEM_output"
missing_dir <- file.path(output_dir, "missing")
dir.create(missing_dir, recursive = TRUE, showWarnings = FALSE)

# Variables of interest (same order as main SEM script)
SEM_VARS <- c(
  "ADOS_Severity", "ADOS_SA", "ADOS_RRB",
  "ADI_Total",     "ADI_Social_Com", "ADI_RRB",
  "SRS_Total_Parent", "SRS_Social_Parent", "SRS_RRB_Parent",
  "SRS_Total_Teacher", "SRS_Social_Teacher", "SRS_RRB_Teacher",
  "RBSR_Total"
)

# Colour palette (consistent with main script)
COL_MALE   <- "#2166AC"
COL_FEMALE <- "#E75480"
COL_MISS   <- "#D73027"
COL_OBS    <- "#4DAC26"

# --------------------------------------------------------------------------- #
# 1. LOAD & HARMONISE DATA  (identical to main script)                         #
# --------------------------------------------------------------------------- #

rds1 <- readRDS("~/Desktop/MIND/SEM_MIND_schoolage.rds")
rds2 <- readRDS("~/Desktop/MIND/SEM_MIND_preschool.rds")
rds3 <- readRDS("~/Desktop/MIND/SEM_SSC_schoolage.rds")
rds4 <- readRDS("~/Desktop/MIND/SEM_SSC_preschool.rds")

common_cols <- c("id", "sex", "age",
                 "ADOS_Severity", "ADOS_SA", "ADOS_RRB",
                 "ADI_Total", "ADI_Social_Com", "ADI_RRB",
                 "SRS_Total_Parent", "SRS_Social_Parent", "SRS_RRB_Parent",
                 "SRS_Total_Teacher", "SRS_Social_Teacher", "SRS_RRB_Teacher",
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
    subsample = paste0(sample_source, " — ", age_group)
  )

# Keep only the SEM variables that actually exist in this dataset
vars_present <- intersect(SEM_VARS, names(dat))
cat("Variables present:", paste(vars_present, collapse = ", "), "\n")
cat("Variables absent (will be skipped):",
    paste(setdiff(SEM_VARS, vars_present), collapse = ", "), "\n\n")

# Working data frame: SEM vars + grouping vars
dat_full <- dat %>% select(id, sex, age, sample_source, age_group, subsample,
                           all_of(vars_present))

# --------------------------------------------------------------------------- #
# 2. VARIABLE-LEVEL MISSINGNESS SUMMARY                                        #
# --------------------------------------------------------------------------- #

miss_summary <- dat_full %>%
  select(all_of(vars_present)) %>%
  summarise(across(everything(), list(
    n_miss  = ~ sum(is.na(.)),
    n_obs   = ~ sum(!is.na(.)),
    pct_miss = ~ round(100 * mean(is.na(.)), 1)
  ), .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    domain = case_when(
      str_starts(variable, "ADOS") ~ "ADOS",
      str_starts(variable, "ADI")  ~ "ADI",
      str_starts(variable, "SRS") & str_ends(variable, "Parent")  ~ "SRS (Parent)",
      str_starts(variable, "SRS") & str_ends(variable, "Teacher") ~ "SRS (Teacher)",
      variable == "RBSR_Total"     ~ "RBSR",
      TRUE ~ "Other"
    ),
    miss_flag = case_when(
      pct_miss == 0    ~ "Complete",
      pct_miss < 5     ~ "< 5%",
      pct_miss < 20    ~ "5–20%",
      pct_miss < 50    ~ "20–50%",
      TRUE             ~ "> 50%"
    )
  )

cat("=== Variable-Level Missingness ===\n")
print(miss_summary, n = 30)

write_xlsx(miss_summary, file.path(missing_dir, "miss_summary_variables.xlsx"))

# --------------------------------------------------------------------------- #
# 3. MISSINGNESS BY SEX AND SUBSAMPLE                                           #
# --------------------------------------------------------------------------- #

miss_by_group <- dat_full %>%
  group_by(sex, subsample) %>%
  summarise(
    n_total = n(),
    across(all_of(vars_present),
           ~ round(100 * mean(is.na(.)), 1),
           .names = "{.col}_miss_pct"),
    .groups = "drop"
  )

cat("\n=== Missingness % by Sex × Subsample ===\n")
print(as.data.frame(miss_by_group))

write_xlsx(miss_by_group, file.path(missing_dir, "miss_by_sex_subsample.xlsx"))

# --------------------------------------------------------------------------- #
# 4. LITTLE'S MCAR TEST                                                        #
# --------------------------------------------------------------------------- #
# naniar::mcar_test() implements Little (1988).
# H0: data are MCAR.  p < .05 → reject MCAR (data are MAR or MNAR).
# Run on full sample and each subsample.
# --------------------------------------------------------------------------- #

run_mcar <- function(df, label) {
  d <- df %>% select(all_of(vars_present))
  # Need at least 2 variables and some missing data
  n_miss_total <- sum(is.na(d))
  if (n_miss_total == 0) {
    return(data.frame(subsample = label, statistic = NA, df = NA, p.value = NA,
                      n = nrow(d), verdict = "No missing data"))
  }
  # Drop variables with 0 or 100% missing (mcar_test will error otherwise)
  d <- d[, sapply(d, function(x) sum(is.na(x)) > 0 & sum(is.na(x)) < nrow(d)), drop = FALSE]
  if (ncol(d) < 2) {
    return(data.frame(subsample = label, statistic = NA, df = NA, p.value = NA,
                      n = nrow(d), verdict = "Too few variables with partial missingness"))
  }
  tryCatch({
    res <- naniar::mcar_test(d)
    data.frame(
      subsample = label,
      statistic = round(res$statistic, 3),
      df        = res$df,
      p.value   = round(res$p.value, 6),
      n         = nrow(d),
      verdict   = ifelse(res$p.value < .05,
                         "Reject MCAR (MAR or MNAR likely)",
                         "Cannot reject MCAR")
    )
  }, error = function(e) {
    data.frame(subsample = label, statistic = NA, df = NA, p.value = NA,
               n = nrow(d), verdict = paste("Error:", e$message))
  })
}

mcar_results <- bind_rows(
  run_mcar(dat_full,                                            "Full sample"),
  run_mcar(dat_full %>% filter(sex == "Male"),                 "Males only"),
  run_mcar(dat_full %>% filter(sex == "Female"),               "Females only"),
  run_mcar(dat_full %>% filter(sample_source == "MIND"),       "MIND"),
  run_mcar(dat_full %>% filter(sample_source == "SSC"),        "SSC"),
  run_mcar(dat_full %>% filter(subsample == "MIND — Preschool"), "MIND Preschool"),
  run_mcar(dat_full %>% filter(subsample == "MIND — School"),    "MIND School"),
  run_mcar(dat_full %>% filter(subsample == "SSC — Preschool"),  "SSC Preschool"),
  run_mcar(dat_full %>% filter(subsample == "SSC — School"),     "SSC School")
)

cat("\n=== Little's MCAR Test Results ===\n")
print(mcar_results, row.names = FALSE)
write_xlsx(mcar_results, file.path(missing_dir, "little_mcar_results.xlsx"))

# --------------------------------------------------------------------------- #
# 5. MAR PROBE — Logistic regression of missingness indicators                 #
# --------------------------------------------------------------------------- #
# For each variable V with missing data: create R_V (1 = missing, 0 = observed)
# Regress R_V ~ all other observed variables.
# Significant predictors → missingness on V is predicted by observed data → MAR.
# --------------------------------------------------------------------------- #

mar_probe <- function(df, label) {
  d <- df %>% select(all_of(vars_present))
  results <- list()
  
  for (v in vars_present) {
    n_miss <- sum(is.na(d[[v]]))
    if (n_miss == 0 || n_miss == nrow(d)) next    # skip complete / entirely missing
    
    # Missingness indicator
    R <- as.integer(is.na(d[[v]]))
    
    # Predictors: other variables (use complete obs for each)
    predictors <- setdiff(vars_present, v)
    pred_df    <- d[, predictors, drop = FALSE]
    combined   <- cbind(R = R, pred_df)
    combined   <- combined[complete.cases(combined[, predictors, drop = FALSE]), ]
    
    if (nrow(combined) < 20 || var(combined$R) == 0) next
    
    formula_str <- paste("R ~", paste(predictors, collapse = " + "))
    fit <- tryCatch(
      glm(as.formula(formula_str), data = combined, family = binomial),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    
    coef_tbl <- summary(fit)$coefficients[-1, , drop = FALSE]  # drop intercept
    sig_preds <- rownames(coef_tbl)[coef_tbl[, "Pr(>|z|)"] < .05]
    
    results[[v]] <- data.frame(
      subsample     = label,
      target_var    = v,
      n_miss        = n_miss,
      pct_miss      = round(100 * n_miss / nrow(d), 1),
      n_sig_preds   = length(sig_preds),
      sig_predictors = ifelse(length(sig_preds) == 0, "none",
                              paste(sig_preds, collapse = "; ")),
      mar_signal    = length(sig_preds) > 0,
      stringsAsFactors = FALSE
    )
  }
  
  if (length(results) == 0) return(NULL)
  do.call(rbind, results)
}

cat("\n=== MAR Probe (Full Sample) ===\n")
mar_full <- mar_probe(dat_full, "Full sample")
if (!is.null(mar_full)) {
  print(mar_full, row.names = FALSE)
  write_xlsx(mar_full, file.path(missing_dir, "mar_probe_full.xlsx"))
}

# Run per subsample (to check if MAR structure is consistent)
mar_by_sub <- bind_rows(lapply(
  split(dat_full, dat_full$subsample),
  function(sub) mar_probe(sub, unique(sub$subsample))
))
if (nrow(mar_by_sub) > 0) {
  cat("\n=== MAR Probe (By Subsample) ===\n")
  print(mar_by_sub, row.names = FALSE)
  write_xlsx(mar_by_sub, file.path(missing_dir, "mar_probe_by_subsample.xlsx"))
}

# --------------------------------------------------------------------------- #
# 6. MNAR RISK FLAGS — Sex × Missingness association                           #
# --------------------------------------------------------------------------- #
# A classic MNAR risk for ASD data: girls are harder to diagnose / refer,
# so girls with high symptom severity may be systematically under-represented.
# Test: chi-square / point-biserial between sex and each missingness indicator.
# --------------------------------------------------------------------------- #

mnar_sex_test <- function(df) {
  d <- df %>% select(sex, all_of(vars_present)) %>%
    filter(sex %in% c("Male", "Female"))
  
  rows <- list()
  for (v in vars_present) {
    R <- as.integer(is.na(d[[v]]))
    if (var(R) == 0) next
    sex_bin <- as.integer(d$sex == "Female")
    ct <- chisq.test(table(sex_bin, R), correct = FALSE)
    
    # Missingness rates by sex
    rate_m <- mean(R[d$sex == "Male"],   na.rm = TRUE)
    rate_f <- mean(R[d$sex == "Female"], na.rm = TRUE)
    
    rows[[v]] <- data.frame(
      variable     = v,
      miss_pct_male   = round(100 * rate_m, 1),
      miss_pct_female = round(100 * rate_f, 1),
      chi2         = round(ct$statistic, 3),
      p_value      = round(ct$p.value, 6),
      sex_diff_sig = ct$p.value < .05,
      mnar_flag    = ct$p.value < .05,  # sex-linked missingness = MNAR risk
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

mnar_sex <- mnar_sex_test(dat_full)
cat("\n=== MNAR Risk: Sex × Missingness (chi-square) ===\n")
print(mnar_sex, row.names = FALSE)
write_xlsx(mnar_sex, file.path(missing_dir, "mnar_sex_flags.xlsx"))

# --------------------------------------------------------------------------- #
# 7. MICE MISSINGNESS PATTERN MATRIX                                            #
# --------------------------------------------------------------------------- #
# md.pattern() shows which combinations of missing variables co-occur.
# Left column = N cases with that pattern; right column = N missing per pattern.
# --------------------------------------------------------------------------- #

cat("\n=== Missingness Pattern Matrix (mice) ===\n")
d_sem <- dat_full %>% select(all_of(vars_present))
mp <- mice::md.pattern(d_sem, plot = FALSE)
print(mp)
write.csv(as.data.frame(mp), file.path(missing_dir, "md_pattern_matrix.csv"))

# --------------------------------------------------------------------------- #
# 8. VISUALISATIONS                                                             #
# --------------------------------------------------------------------------- #

# --- 8a. vis_miss heatmap: full sample ---------------------------------------
p_vismiss <- naniar::vis_miss(d_sem, cluster = TRUE, sort_miss = TRUE) +
  labs(title = "Missingness Heatmap — Full Sample",
       subtitle = "Rows clustered by missing pattern; columns sorted by % missing") +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(missing_dir, "01_vismiss_full.png"),
       p_vismiss, width = 12, height = 7, dpi = 300)

# --- 8b. vis_miss by subsample -----------------------------------------------
subsamples <- unique(dat_full$subsample)
vismiss_plots <- lapply(subsamples, function(ss) {
  d_sub <- dat_full %>% filter(subsample == ss) %>% select(all_of(vars_present))
  naniar::vis_miss(d_sub, cluster = TRUE, sort_miss = TRUE) +
    labs(title = ss,
         subtitle = paste0("N = ", nrow(d_sub))) +
    theme(plot.title = element_text(size = 11, face = "bold"),
          legend.position = "none")
})
names(vismiss_plots) <- subsamples

combined_vismiss <- patchwork::wrap_plots(vismiss_plots, ncol = 2) +
  patchwork::plot_annotation(
    title = "Missingness Heatmap by Subsample",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )
ggsave(file.path(missing_dir, "02_vismiss_by_subsample.png"),
       combined_vismiss, width = 18, height = 14, dpi = 300)

# --- 8c. gg_miss_var: % missing per variable, coloured by domain -------------
miss_rate_df <- dat_full %>%
  select(all_of(vars_present)) %>%
  naniar::miss_var_summary() %>%
  mutate(
    domain = case_when(
      str_starts(variable, "ADOS") ~ "ADOS",
      str_starts(variable, "ADI")  ~ "ADI",
      str_starts(variable, "SRS") & str_ends(variable, "Parent")  ~ "SRS Parent",
      str_starts(variable, "SRS") & str_ends(variable, "Teacher") ~ "SRS Teacher",
      variable == "RBSR_Total" ~ "RBSR",
      TRUE ~ "Other"
    )
  )

domain_colours <- c(
  ADOS         = "#1B7837",
  ADI          = "#762A83",
  "SRS Parent" = "#E08214",
  "SRS Teacher"= "#74ADD1",
  RBSR         = "#D6604D",
  Other        = "gray50"
)

p_miss_var <- ggplot(miss_rate_df, aes(x = reorder(variable, pct_miss),
                                       y = pct_miss, fill = domain)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0(round(pct_miss, 1), "%")),
            hjust = -0.15, size = 3.2) +
  scale_fill_manual(values = domain_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(title = "% Missing per Variable — Full Sample",
       x = NULL, y = "% Missing", fill = "Domain") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  )
ggsave(file.path(missing_dir, "03_miss_by_variable.png"),
       p_miss_var, width = 10, height = 7, dpi = 300)

# --- 8d. Dumbbell: missingness rate Male vs Female per variable --------------
miss_sex_df <- dat_full %>%
  filter(sex %in% c("Male", "Female")) %>%
  group_by(sex) %>%
  summarise(across(all_of(vars_present), ~ round(100 * mean(is.na(.)), 1)),
            .groups = "drop") %>%
  pivot_longer(-sex, names_to = "variable", values_to = "pct_miss") %>%
  pivot_wider(names_from = sex, values_from = pct_miss)

# Only plot variables with any missingness
miss_sex_df <- miss_sex_df %>%
  filter(!(Male == 0 & Female == 0))

if (nrow(miss_sex_df) > 0) {
  miss_sex_df <- miss_sex_df %>%
    left_join(miss_rate_df %>% select(variable, domain), by = "variable") %>%
    mutate(variable = factor(variable, levels = variable[order(Male)]))
  
  p_dumbbell <- ggplot(miss_sex_df, aes(y = variable)) +
    geom_segment(aes(x = Male, xend = Female, yend = variable),
                 color = "gray60", linewidth = 0.8) +
    geom_point(aes(x = Male),   color = COL_MALE,   size = 4) +
    geom_point(aes(x = Female), color = COL_FEMALE, size = 4) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Missingness Rate: Males vs Females",
         subtitle = paste0("Blue = Male (N=", sum(dat_full$sex == "Male", na.rm = TRUE),
                           ")   Pink = Female (N=", sum(dat_full$sex == "Female", na.rm = TRUE), ")"),
         x = "% Missing", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(missing_dir, "04_miss_dumbbell_sex.png"),
         p_dumbbell, width = 9, height = 6, dpi = 300)
}

# --- 8e. UpSet plot: co-occurring missingness patterns -----------------------
# gg_miss_upset shows which variables are commonly missing together.
# Top combinations = most frequent missing patterns.
tryCatch({
  p_upset <- naniar::gg_miss_upset(d_sem, nsets = ncol(d_sem), nintersects = 20) +
    labs(title = "Co-Missing Patterns (UpSet Plot)",
         caption = "Each bar = a combination of variables missing on the same case")
  ggsave(file.path(missing_dir, "05_upset_comissing.png"),
         p_upset, width = 14, height = 8, dpi = 300)
}, error = function(e) {
  cat("  Note: UpSet plot skipped —", e$message, "\n")
})

# --- 8f. MCAR p-value summary plot -------------------------------------------
mcar_plot_df <- mcar_results %>%
  filter(!is.na(p.value)) %>%
  mutate(
    subsample = factor(subsample, levels = rev(subsample)),
    sig = p.value < .05
  )

if (nrow(mcar_plot_df) > 0) {
  p_mcar <- ggplot(mcar_plot_df, aes(x = p.value, y = subsample, colour = sig)) +
    geom_vline(xintercept = .05, linetype = "dashed", color = "gray40") +
    geom_point(size = 5) +
    geom_text(aes(label = sprintf("p = %.4f", p.value)),
              hjust = -0.2, size = 3.5) +
    scale_colour_manual(values = c(`TRUE` = COL_MISS, `FALSE` = COL_OBS),
                        labels = c(`TRUE` = "p < .05 (reject MCAR)",
                                   `FALSE` = "p ≥ .05 (cannot reject MCAR)"),
                        name = NULL) +
    scale_x_continuous(limits = c(0, max(mcar_plot_df$p.value) * 1.5)) +
    labs(title = "Little's MCAR Test — by Subsample",
         subtitle = "Dashed line = α = .05",
         x = "p-value", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    )
  
  ggsave(file.path(missing_dir, "06_mcar_pvalues.png"),
         p_mcar, width = 10, height = 6, dpi = 300)
}

# --- 8g. MNAR risk: sex-linked missingness heatmap ---------------------------
if (!is.null(mnar_sex) && nrow(mnar_sex) > 0) {
  mnar_long <- mnar_sex %>%
    select(variable, miss_pct_male, miss_pct_female) %>%
    pivot_longer(-variable, names_to = "sex", values_to = "pct_miss") %>%
    mutate(
      sex = ifelse(sex == "miss_pct_male", "Male", "Female"),
      variable = factor(variable, levels = rev(mnar_sex$variable))
    )
  
  mnar_flags <- mnar_sex %>%
    filter(mnar_flag) %>%
    select(variable, p_value)
  
  p_mnar <- ggplot(mnar_long, aes(x = sex, y = variable, fill = pct_miss)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = paste0(round(pct_miss, 1), "%")),
              size = 3.2, color = "white", fontface = "bold") +
    scale_fill_gradient(low = "#FDDBC7", high = COL_MISS,
                        name = "% Missing", limits = c(0, 100)) +
    {
      if (nrow(mnar_flags) > 0)
        geom_text(data = mnar_flags %>%
                    mutate(sex = "Female", variable = factor(variable)),
                  aes(label = sprintf("χ² p=%.3f *", p_value),
                      fill = NULL),
                  x = 2.55, size = 3, hjust = 0, color = COL_MISS)
    } +
    labs(title = "MNAR Risk: Missingness Rate by Sex",
         subtitle = "Variables flagged (*) have significantly different missingness by sex (χ² p < .05)",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(size = 12, face = "bold"),
      panel.grid = element_blank()
    )
  
  ggsave(file.path(missing_dir, "07_mnar_sex_heatmap.png"),
         p_mnar, width = 10, height = max(6, nrow(mnar_sex) * 0.5 + 2), dpi = 300)
}

# --------------------------------------------------------------------------- #
# 9. WRITTEN VERDICT + FIML GUIDANCE                                           #
# --------------------------------------------------------------------------- #

sink(file.path(missing_dir, "missing_data_verdict.txt"), split = TRUE)

cat("================================================================\n")
cat("MISSING DATA ASSESSMENT — VERDICT & FIML GUIDANCE\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("================================================================\n\n")

cat("--- 1. LITTLE'S MCAR TEST ---\n\n")
print(mcar_results, row.names = FALSE)

cat("\n\nInterpretation guide:\n")
cat("  p >= .05  → Cannot reject MCAR.  FIML is valid (and optimal).\n")
cat("  p <  .05  → Reject MCAR.  Data are MAR or MNAR.\n")
cat("              If MAR → FIML still valid (MAR is FIML's assumption).\n")
cat("              If MNAR → FIML is biased; consider sensitivity analysis.\n\n")

cat("--- 2. MAR SIGNAL ---\n\n")
if (!is.null(mar_full) && nrow(mar_full) > 0) {
  n_mar_sig <- sum(mar_full$mar_signal, na.rm = TRUE)
  cat(sprintf("  %d / %d variables show MAR signal (missingness predicted by other observed vars)\n",
              n_mar_sig, nrow(mar_full)))
  if (n_mar_sig > 0) {
    cat("  Variables with MAR signal:\n")
    print(mar_full %>% filter(mar_signal) %>% select(target_var, pct_miss, sig_predictors),
          row.names = FALSE)
  }
}

cat("\n\n--- 3. MNAR RISK FLAGS ---\n\n")
if (!is.null(mnar_sex) && any(mnar_sex$mnar_flag, na.rm = TRUE)) {
  cat("  The following variables have SIGNIFICANTLY different missingness rates\n")
  cat("  between males and females (potential MNAR by sex):\n\n")
  print(mnar_sex %>% filter(mnar_flag), row.names = FALSE)
  cat("\n  ACTION: Run sensitivity analysis (e.g., pattern mixture model or\n")
  cat("          Diggle-Kenward selection model) for flagged variables.\n")
} else {
  cat("  No significant sex-linked MNAR flags detected.\n")
}

cat("\n\n--- 4. OVERALL RECOMMENDATION ---\n\n")

mcar_rejected <- any(mcar_results$p.value < .05, na.rm = TRUE)
mar_present   <- !is.null(mar_full) && any(mar_full$mar_signal, na.rm = TRUE)
mnar_present  <- !is.null(mnar_sex) && any(mnar_sex$mnar_flag, na.rm = TRUE)

if (!mcar_rejected) {
  cat("MCAR cannot be rejected. FIML is valid and recommended.\n")
} else if (mar_present && !mnar_present) {
  cat("MCAR rejected, but MAR signal present and no MNAR flags.\n")
  cat("FIML under MAR is valid and recommended.\n")
  cat("Consider reporting the MAR pattern in your methods section.\n")
} else if (mnar_present) {
  cat("MCAR rejected AND MNAR risk detected (sex-linked missingness).\n")
  cat("FIML is still the best available estimator, but:\n")
  cat("  1. Report the MNAR-flagged variables explicitly.\n")
  cat("  2. Run a sensitivity analysis comparing FIML estimates to\n")
  cat("     listwise deletion — divergence signals MNAR impact.\n")
  cat("  3. Consider using auxiliary variables (e.g., ADOS CSS) in the\n")
  cat("     FIML model to reduce MNAR bias.\n")
} else {
  cat("Mixed signals. Proceed with FIML; report limitations.\n")
}

cat("\n\n--- 5. AUXILIARY VARIABLE CANDIDATES FOR FIML ---\n")
cat("  Including auxiliary variables (variables correlated with missingness\n")
cat("  and/or the missing values themselves) improves FIML under MAR/MNAR.\n")
cat("  Candidates from this dataset:\n")
cat("  - sex        (predicts missingness on ADI, Teacher SRS in many samples)\n")
cat("  - age        (predicts availability of specific instruments)\n")
cat("  - sample_source (MIND vs SSC have different instrument coverage)\n")
cat("  - ADOS_Severity (observed for nearly all cases; predicts ADI missingness)\n\n")
cat("  In lavaan FIML, add auxiliary vars to the model via:\n")
cat("    sem(model, data = dat, missing = 'fiml',\n")
cat("        auxiliary = c('sex', 'age', 'ADOS_Severity'))\n")

cat("\n================================================================\n")
cat("Output files saved to:", normalizePath(missing_dir, mustWork = FALSE), "\n")
cat("================================================================\n")

sink()

cat("\n✓ Missing data assessment complete.\n")
cat("✓ Output saved to:", normalizePath(missing_dir, mustWork = FALSE), "\n")
cat("\nFiles produced:\n")
for (f in list.files(missing_dir, full.names = FALSE)) {
  cat(sprintf("  %s\n", f))
}