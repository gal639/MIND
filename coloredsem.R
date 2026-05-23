# =============================================================================
# SEM Covariance Matrix Comparison: Boys vs. Girls
# Three domain models: Total, Social, RRB
# Teacher SRS included automatically when available (SSC only)
# =============================================================================

library(lavaan)
library(tidyverse)
library(semPlot)

# --- 1. LOAD DATA -------------------------------------------------------------

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

# --- 2. HELPER FUNCTIONS -----------------------------------------------------

sig_stars <- function(p) {
  ifelse(p < .001, "***",
         ifelse(p < .01,  "**",
                ifelse(p < .05,  "*", "")))
}

# Build saturated lavaan model from variable names
build_model <- function(vars) {
  n <- length(vars)
  lines <- c()
  # Variances
  for (v in vars) lines <- c(lines, paste0("  ", v, " ~~ ", v))
  lines <- c(lines, "")
  # Covariances
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      lines <- c(lines, paste0("  ", vars[i], " ~~ ", vars[j]))
    }
  }
  lines <- c(lines, "")
  # Means
  for (v in vars) lines <- c(lines, paste0("  ", v, " ~ 1"))
  paste(lines, collapse = "\n")
}

# Build short labels from variable names
build_short_labels <- function(vars) {
  lookup <- c(
    ADOS_Severity      = "ADOS\nSeverity",
    ADOS_SA            = "ADOS\nSA",
    ADOS_RRB           = "ADOS\nRRB",
    ADI_Total          = "ADI\nTotal",
    ADI_Social_Com     = "ADI\nSocial",
    ADI_RRB            = "ADI\nRRB",
    SRS_Total_Parent   = "SRS\nTotal\n(Parent)",
    SRS_Social_Parent  = "SRS\nSocial\n(Parent)",
    SRS_RRB_Parent     = "SRS\nRRB\n(Parent)",
    SRS_Total_Teacher  = "SRS\nTotal\n(Teacher)",
    SRS_Social_Teacher = "SRS\nSocial\n(Teacher)",
    SRS_RRB_Teacher    = "SRS\nRRB\n(Teacher)",
    RBSR_Total         = "RBSR\nTotal"
  )
  lookup[vars]
}

# Correlation significance table
cor_sig_table <- function(dat_complete, group_label) {
  vars <- names(dat_complete)
  n <- ncol(dat_complete)
  rows <- list()
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      ct <- cor.test(dat_complete[[i]], dat_complete[[j]])
      rows[[length(rows) + 1]] <- data.frame(
        Pair = paste(vars[i], "~~", vars[j]),
        r    = sprintf("%.3f", ct$estimate),
        p    = sprintf("%.6f", ct$p.value),
        sig  = sig_stars(ct$p.value),
        stringsAsFactors = FALSE
      )
    }
  }
  tbl <- do.call(rbind, rows)
  cat(sprintf("  Correlation significance — %s (N = %d):\n", group_label, nrow(dat_complete)))
  print(tbl, row.names = FALSE, right = FALSE)
  cat("\n")
}

# Edge labels: correlations only, blank for variances
make_edge_labels <- function(fit) {
  pe <- parameterEstimates(fit, standardized = TRUE)
  pe_cov <- pe[pe$op == "~~", ]
  labels <- sapply(1:nrow(pe_cov), function(i) {
    if (pe_cov$lhs[i] == pe_cov$rhs[i]) {
      ""
    } else {
      star <- sig_stars(pe_cov$pvalue[i])
      sprintf("r=%.2f%s", pe_cov$std.all[i], star)
    }
  })
  return(labels)
}

# Build layout as numeric coordinate matrix (n_vars x 2)
# Each row = (x, y) for the variable in order
# Conceptual grouping:
#   Top    = non-parent informant (ADOS clinician, SRS Teacher)
#   Bottom = parent informant     (ADI interview, SRS Parent, RBSR)
build_layout_matrix <- function(vars) {
  n <- length(vars)
  
  # --- 3 variables: triangle ---
  #       ADOS
  #    ADI    SRS_Parent
  if (n == 3) {
    return(matrix(c(
      0,  1,    # var1: ADOS (top center)
      -1, -1,    # var2: ADI (bottom left)
      1, -1     # var3: SRS Parent (bottom right)
    ), ncol = 2, byrow = TRUE))
  }
  
  # --- 4 variables ---
  if (n == 4) {
    has_rbsr <- "RBSR_Total" %in% vars
    
    if (has_rbsr) {
      # RRB domain (no teacher): diamond
      #       ADOS_RRB
      # ADI_RRB      SRS_RRB_Parent
      #       RBSR_Total
      return(matrix(c(
        0,  1,    # var1: ADOS_RRB (top)
        -1,  0,    # var2: ADI_RRB (left)
        1,  0,    # var3: SRS_RRB_Parent (right)
        0, -1     # var4: RBSR_Total (bottom)
      ), ncol = 2, byrow = TRUE))
    } else {
      # Total/Social with teacher: informant grid
      # ADOS         SRS_Teacher
      # ADI          SRS_Parent
      return(matrix(c(
        -1,  1,    # var1: ADOS (top left)
        -1, -1,    # var2: ADI (bottom left)
        1, -1,    # var3: SRS Parent (bottom right)
        1,  1     # var4: SRS Teacher (top right)
      ), ncol = 2, byrow = TRUE))
    }
  }
  
  # --- 5 variables: RRB with teacher --- pentagon
  # ADOS_RRB                SRS_RRB_Teacher
  #      ADI_RRB    SRS_RRB_Parent
  #           RBSR_Total
  if (n == 5) {
    return(matrix(c(
      -1.0,  1.0,    # var1: ADOS_RRB (top left)
      -0.7, -0.3,    # var2: ADI_RRB (mid left)
      0.7, -0.3,    # var3: SRS_RRB_Parent (mid right)
      0.0, -1.0,    # var4: RBSR_Total (bottom center)
      1.0,  1.0     # var5: SRS_RRB_Teacher (top right)
    ), ncol = 2, byrow = TRUE))
  }
  
  # Fallback: circle
  return("circle")
}

# Fit index comparison table
fit_table <- function(fit_diff, fit_ident, n_total) {
  indices <- c("chisq", "df", "pvalue", "aic", "bic", "bic2", "rmsea",
               "rmsea.ci.lower", "rmsea.ci.upper")
  fm_diff  <- fitMeasures(fit_diff,  indices)
  fm_ident <- fitMeasures(fit_ident, indices)
  k_diff  <- fitMeasures(fit_diff,  "npar")
  k_ident <- fitMeasures(fit_ident, "npar")
  aicc_diff  <- fm_diff["aic"]  + (2 * k_diff  * (k_diff + 1))  / (n_total - k_diff - 1)
  aicc_ident <- fm_ident["aic"] + (2 * k_ident * (k_ident + 1)) / (n_total - k_ident - 1)
  
  data.frame(
    Index = c("Chi-sq", "df", "p-value",
              "AIC", "AICc (adj)", "BIC", "aBIC (adj)",
              "RMSEA", "RMSEA 90% CI"),
    Different = c(
      sprintf("%.3f", fm_diff["chisq"]),
      sprintf("%.0f",  fm_diff["df"]),
      ifelse(is.na(fm_diff["pvalue"]), "—", sprintf("%.4f", fm_diff["pvalue"])),
      sprintf("%.1f", fm_diff["aic"]),
      sprintf("%.1f", aicc_diff),
      sprintf("%.1f", fm_diff["bic"]),
      sprintf("%.1f", fm_diff["bic2"]),
      sprintf("%.4f", fm_diff["rmsea"]),
      sprintf("[%.3f, %.3f]", fm_diff["rmsea.ci.lower"], fm_diff["rmsea.ci.upper"])
    ),
    Identical = c(
      sprintf("%.3f", fm_ident["chisq"]),
      sprintf("%.0f",  fm_ident["df"]),
      sprintf("%.4f", fm_ident["pvalue"]),
      sprintf("%.1f", fm_ident["aic"]),
      sprintf("%.1f", aicc_ident),
      sprintf("%.1f", fm_ident["bic"]),
      sprintf("%.1f", fm_ident["bic2"]),
      sprintf("%.4f", fm_ident["rmsea"]),
      sprintf("[%.3f, %.3f]", fm_ident["rmsea.ci.lower"], fm_ident["rmsea.ci.upper"])
    ),
    Better = c(
      ifelse(fm_diff["chisq"] < fm_ident["chisq"], "Different", "Identical"),
      "—", "—",
      ifelse(fm_diff["aic"] < fm_ident["aic"], "Different", "Identical"),
      ifelse(aicc_diff < aicc_ident, "Different", "Identical"),
      ifelse(fm_diff["bic"] < fm_ident["bic"], "Different", "Identical"),
      ifelse(fm_diff["bic2"] < fm_ident["bic2"], "Different", "Identical"),
      ifelse(fm_diff["rmsea"] < fm_ident["rmsea"], "Different", "Identical"),
      "—"
    ),
    stringsAsFactors = FALSE
  )
}

# --- 3. MAIN COMPARISON FUNCTION ---------------------------------------------

compare_cov <- function(data, vars, label) {
  
  model        <- build_model(vars)
  short_labels <- build_short_labels(vars)
  n_vars       <- length(vars)
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("  ", label, "\n")
  cat("  Variables:", paste(vars, collapse = ", "), "\n")
  cat(strrep("=", 70), "\n\n")
  
  dat_m <- data %>% filter(sex == "Male")   %>% select(all_of(vars)) %>% na.omit()
  dat_f <- data %>% filter(sex == "Female") %>% select(all_of(vars)) %>% na.omit()
  
  n_m <- nrow(dat_m); n_f <- nrow(dat_f)
  n_total <- n_m + n_f
  
  if (n_m < 10 || n_f < 10) {
    cat(sprintf("  SKIPPED — too few complete cases (Male: %d, Female: %d)\n", n_m, n_f))
    cat("  (Likely ADI or Teacher SRS missing in this subgroup)\n\n")
    return(invisible(NULL))
  }
  
  cat(sprintf("N (complete) — Male: %d, Female: %d, Total: %d\n\n", n_m, n_f, n_total))
  
  cov_m <- cov(dat_m); cov_f <- cov(dat_f)
  mean_m <- colMeans(dat_m); mean_f <- colMeans(dat_f)
  
  cat("Covariance matrix — Male:\n"); print(round(cov_m, 2))
  cat("\nCovariance matrix — Female:\n"); print(round(cov_f, 2))
  cat("\n")
  
  cor_sig_table(dat_m, "Male")
  cor_sig_table(dat_f, "Female")
  
  # --- Model 1: DIFFERENT ---
  fit_diff <- tryCatch(
    sem(model,
        sample.cov  = list(Male = cov_m, Female = cov_f),
        sample.mean = list(Male = mean_m, Female = mean_f),
        sample.nobs = c(Male = n_m, Female = n_f),
        group.label = c("Male", "Female")),
    error = function(e) { cat("  ERROR (free):", e$message, "\n"); NULL }
  )
  
  # --- Model 2: IDENTICAL ---
  fit_ident <- tryCatch(
    sem(model,
        sample.cov  = list(Male = cov_m, Female = cov_f),
        sample.mean = list(Male = mean_m, Female = mean_f),
        sample.nobs = c(Male = n_m, Female = n_f),
        group.label = c("Male", "Female"),
        group.equal = c("residual.covariances", "intercepts")),
    error = function(e) { cat("  ERROR (constrained):", e$message, "\n"); NULL }
  )
  
  if (is.null(fit_diff) || is.null(fit_ident)) return(invisible(NULL))
  
  # --- FIT TABLE ---
  cat("=== MODEL COMPARISON ===\n")
  cat("  Different = each group has its own covariance matrix\n")
  cat("  Identical = covariance matrix constrained equal across groups\n\n")
  tbl <- fit_table(fit_diff, fit_ident, n_total)
  print(tbl, row.names = FALSE, right = FALSE)
  
  # --- LRT ---
  cat("\n--- LIKELIHOOD RATIO TEST ---\n")
  lrt <- lavTestLRT(fit_diff, fit_ident)
  print(lrt)
  p_val <- lrt[["Pr(>Chisq)"]][2]
  cat(sprintf("\n  chi2 diff = %.3f, df = %d, p = %.6f %s\n",
              lrt[["Chisq diff"]][2], lrt[["Df diff"]][2], p_val, sig_stars(p_val)))
  
  # --- VERDICT ---
  cat("\n--- VERDICT ---\n")
  cat(sprintf("  AIC favors:  %s\n", tbl$Better[tbl$Index == "AIC"]))
  cat(sprintf("  BIC favors:  %s\n", tbl$Better[tbl$Index == "BIC"]))
  cat(sprintf("  LRT:         %s\n", ifelse(p_val < .05, "Significantly different", "Not different")))
  rmsea_val <- fitMeasures(fit_ident, "rmsea")
  cat(sprintf("  RMSEA (constrained): %.4f %s\n", rmsea_val,
              ifelse(rmsea_val < 0.05, "(good)",
                     ifelse(rmsea_val < 0.08, "(acceptable)", "(poor)"))))
  cat("\n")
  
  # --- SEMPLOT ---
  fit_male   <- sem(model, sample.cov = cov_m, sample.mean = mean_m, sample.nobs = n_m)
  fit_female <- sem(model, sample.cov = cov_f, sample.mean = mean_f, sample.nobs = n_f)
  
  labels_m <- make_edge_labels(fit_male)
  labels_f <- make_edge_labels(fit_female)
  
  # --- Custom layout matrix per variable configuration ---
  lay <- build_layout_matrix(vars)
  
  # Adaptive sizing
  label_cex <- ifelse(n_vars >= 5, 0.70, 0.85)
  node_size <- ifelse(n_vars >= 5, 10, 12)
  # curve needs curveAll=TRUE to actually apply to all edges with numeric layouts
  use_curve    <- ifelse(n_vars >= 4, 1, FALSE)
  use_curveAll <- (n_vars >= 4)
  
  # Color palette
  col_pos <- "#B2182B"   # red for positive correlations
  col_neg <- "#2166AC"   # blue for negative correlations
  col_node_border <- "gray30"
  col_node_fill   <- "white"
  
  par(mfrow = c(1, 2), oma = c(1, 0, 3, 0))
  
  semPaths(fit_male,
           what = "std", whatLabels = "omit",
           style = "lisrel", layout = lay,
           residuals = FALSE, intercepts = FALSE,
           edge.label.cex = label_cex, label.cex = 0.9,
           nodeLabels = short_labels, sizeMan = node_size,
           posCol = col_pos, negCol = col_neg,
           fade = FALSE, edge.width = 1.5,
           color = col_node_fill, border.color = col_node_border,
           mar = c(4, 4, 6, 4),
           title = FALSE, ask = FALSE, nCharNodes = 0,
           edgeLabels = labels_m,
           curve = use_curve, curveAll = use_curveAll)
  title(main = "MALE", col.main = "#2166AC", cex.main = 1.4, line = 2.5, font.main = 2)
  mtext(paste0("N = ", n_m), side = 1, line = 0.5, cex = 0.8, col = "gray40")
  
  semPaths(fit_female,
           what = "std", whatLabels = "omit",
           style = "lisrel", layout = lay,
           residuals = FALSE, intercepts = FALSE,
           edge.label.cex = label_cex, label.cex = 0.9,
           nodeLabels = short_labels, sizeMan = node_size,
           posCol = col_pos, negCol = col_neg,
           fade = FALSE, edge.width = 1.5,
           color = col_node_fill, border.color = col_node_border,
           mar = c(4, 4, 6, 4),
           title = FALSE, ask = FALSE, nCharNodes = 0,
           edgeLabels = labels_f,
           curve = use_curve, curveAll = use_curveAll)
  title(main = "FEMALE", col.main = "pink", cex.main = 1.4, line = 2.5, font.main = 2)
  mtext(paste0("N = ", n_f), side = 1, line = 0.5, cex = 0.8, col = "gray40")
  
  mtext(label, side = 3, outer = TRUE, line = 0.5, cex = 1.1, font = 2)
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
}

# --- 4. CHECK TEACHER AVAILABILITY -------------------------------------------

has_teacher <- function(data) {
  teacher_vars <- c("SRS_Total_Teacher", "SRS_Social_Teacher", "SRS_RRB_Teacher")
  all(teacher_vars %in% names(data)) &&
    all(sapply(data[teacher_vars], function(x) sum(!is.na(x)) >= 30))
}

# --- 5. RUN ALL THREE DOMAINS ------------------------------------------------

run_domains <- function(data, label) {
  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat("#  ", label, "\n")
  cat(strrep("#", 70))
  
  teacher <- has_teacher(data)
  if (teacher) cat("\n# Teacher SRS: AVAILABLE\n")
  else         cat("\n# Teacher SRS: not available\n")
  
  # === TOTAL ===
  v_total <- c("ADOS_Severity", "ADI_Total", "SRS_Total_Parent")
  if (teacher) v_total <- c(v_total, "SRS_Total_Teacher")
  compare_cov(data, v_total, paste0(label, " | TOTAL"))
  
  # === SOCIAL ===
  v_social <- c("ADOS_SA", "ADI_Social_Com", "SRS_Social_Parent")
  if (teacher) v_social <- c(v_social, "SRS_Social_Teacher")
  compare_cov(data, v_social, paste0(label, " | SOCIAL"))
  
  # === RRB ===
  v_rrb <- c("ADOS_RRB", "ADI_RRB", "SRS_RRB_Parent", "RBSR_Total")
  if (teacher) v_rrb <- c(v_rrb, "SRS_RRB_Teacher")
  compare_cov(data, v_rrb, paste0(label, " | RRB"))
}

# --- 6. RUN HIERARCHY --------------------------------------------------------

# Level 1
run_domains(dat, "1. FULL SAMPLE")

# Level 2
run_domains(dat %>% filter(sample_source == "MIND"), "2a. MIND")
run_domains(dat %>% filter(sample_source == "SSC"),  "2b. SSC")

# Level 3
run_domains(dat %>% filter(sample_source == "MIND", age_group == "Preschool"), "3a. MIND Preschool")
run_domains(dat %>% filter(sample_source == "MIND", age_group == "School"),    "3b. MIND School")
run_domains(dat %>% filter(sample_source == "SSC",  age_group == "Preschool"), "3c. SSC Preschool")
run_domains(dat %>% filter(sample_source == "SSC",  age_group == "School"),    "3d. SSC School")