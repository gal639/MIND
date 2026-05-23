# =============================================================================
# SEM Covariance Matrix Comparison: Boys vs. Girls
# Three domain models: Total, Social, RRB
# Teacher SRS included automatically when available (SSC only)
# =============================================================================

library(lavaan)
library(tidyverse)
library(semPlot)
library(patchwork)

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

# Edge labels: correlations only (exclude variances entirely)
make_edge_labels <- function(fit) {
  pe <- parameterEstimates(fit, standardized = TRUE)
  pe_cov <- pe[pe$op == "~~" & pe$lhs != pe$rhs, ]
  labels <- sapply(1:nrow(pe_cov), function(i) {
    star <- sig_stars(pe_cov$pvalue[i])
    sprintf("r=%.2f%s", pe_cov$std.all[i], star)
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
  
  # --- 2 variables: horizontal pair ---
  if (n == 2) {
    return(matrix(c(
      -1, 0,    # var1 (left)
      1, 0     # var2 (right)
    ), ncol = 2, byrow = TRUE))
  }
  
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

# --- 3. MAIN COMPARISON FUNCTIONS ---------------------------------------------

# Text output only (console / sink)
compare_cov_text <- function(data, vars, label) {
  
  model        <- build_model(vars)
  
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
  
  fit_diff <- tryCatch(
    sem(model,
        sample.cov  = list(Male = cov_m, Female = cov_f),
        sample.mean = list(Male = mean_m, Female = mean_f),
        sample.nobs = c(Male = n_m, Female = n_f),
        group.label = c("Male", "Female")),
    error = function(e) { cat("  ERROR (free):", e$message, "\n"); NULL }
  )
  
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
  
  cat("=== MODEL COMPARISON ===\n")
  cat("  Different = each group has its own covariance matrix\n")
  cat("  Identical = covariance matrix constrained equal across groups\n\n")
  tbl <- fit_table(fit_diff, fit_ident, n_total)
  print(tbl, row.names = FALSE, right = FALSE)
  
  cat("\n--- LIKELIHOOD RATIO TEST ---\n")
  lrt <- lavTestLRT(fit_diff, fit_ident)
  print(lrt)
  p_val <- lrt[["Pr(>Chisq)"]][2]
  cat(sprintf("\n  chi2 diff = %.3f, df = %d, p = %.6f %s\n",
              lrt[["Chisq diff"]][2], lrt[["Df diff"]][2], p_val, sig_stars(p_val)))
  
  cat("\n--- VERDICT ---\n")
  cat(sprintf("  AIC favors:  %s\n", tbl$Better[tbl$Index == "AIC"]))
  cat(sprintf("  BIC favors:  %s\n", tbl$Better[tbl$Index == "BIC"]))
  cat(sprintf("  LRT:         %s\n", ifelse(p_val < .05, "Significantly different", "Not different")))
  rmsea_val <- fitMeasures(fit_ident, "rmsea")
  cat(sprintf("  RMSEA (constrained): %.4f %s\n", rmsea_val,
              ifelse(rmsea_val < 0.05, "(good)",
                     ifelse(rmsea_val < 0.08, "(acceptable)", "(poor)"))))
  cat("\n")
}

# Plot only (to graphics device)
compare_cov_plot <- function(data, vars, label) {
  
  model        <- build_model(vars)
  short_labels <- build_short_labels(vars)
  n_vars       <- length(vars)
  
  dat_m <- data %>% filter(sex == "Male")   %>% select(all_of(vars)) %>% na.omit()
  dat_f <- data %>% filter(sex == "Female") %>% select(all_of(vars)) %>% na.omit()
  
  n_m <- nrow(dat_m); n_f <- nrow(dat_f)
  
  if (n_m < 10 || n_f < 10) return(invisible(NULL))
  
  cov_m <- cov(dat_m); cov_f <- cov(dat_f)
  mean_m <- colMeans(dat_m); mean_f <- colMeans(dat_f)
  
  fit_male   <- sem(model, sample.cov = cov_m, sample.mean = mean_m, sample.nobs = n_m)
  fit_female <- sem(model, sample.cov = cov_f, sample.mean = mean_f, sample.nobs = n_f)
  
  labels_m <- make_edge_labels(fit_male)
  labels_f <- make_edge_labels(fit_female)
  
  lay <- build_layout_matrix(vars)
  
  label_cex <- ifelse(n_vars >= 5, 0.70, 0.85)
  node_size <- ifelse(n_vars >= 5, 10, 12)
  use_curve    <- ifelse(n_vars >= 4, 1, FALSE)
  use_curveAll <- (n_vars >= 4)
  
  col_pos <- "#B2182B"
  col_neg <- "#2166AC"   # blue for negative
  col_node_border <- "gray30"
  col_node_fill   <- "white"
  
  par(mfrow = c(1, 2), oma = c(2, 1, 4, 1))
  
  semPaths(fit_male,
           what = "std", whatLabels = "omit",
           style = "lisrel", layout = lay,
           residuals = FALSE, intercepts = FALSE,
           edge.label.cex = label_cex, label.cex = 0.9,
           nodeLabels = short_labels, sizeMan = node_size,
           posCol = col_pos, negCol = col_neg,
           fade = FALSE, edge.width = 0.8,
           color = col_node_fill, border.color = col_node_border,
           mar = c(8, 8, 10, 8),
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
           fade = FALSE, edge.width = 0.8,
           color = col_node_fill, border.color = col_node_border,
           mar = c(8, 8, 10, 8),
           title = FALSE, ask = FALSE, nCharNodes = 0,
           edgeLabels = labels_f,
           curve = use_curve, curveAll = use_curveAll)
  title(main = "FEMALE", col.main = "#E75480", cex.main = 1.4, line = 2.5, font.main = 2)
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

# --- 5. DOMAIN VARIABLE BUILDER -----------------------------------------------

get_domain_vars <- function(data) {
  teacher <- has_teacher(data)
  
  # Only keep variables where at least 30 non-NA values exist
  keep <- function(vars) {
    available <- vars[vars %in% names(data)]
    available <- available[sapply(available, function(v) sum(!is.na(data[[v]])) >= 30)]
    if (length(available) < 2) return(NULL)  # need at least 2 vars for a correlation
    available
  }
  
  v_total <- c("ADOS_Severity", "ADI_Total", "SRS_Total_Parent")
  if (teacher) v_total <- c(v_total, "SRS_Total_Teacher")
  
  v_social <- c("ADOS_SA", "ADI_Social_Com", "SRS_Social_Parent")
  if (teacher) v_social <- c(v_social, "SRS_Social_Teacher")
  
  v_rrb <- c("ADOS_RRB", "ADI_RRB", "SRS_RRB_Parent", "RBSR_Total")
  if (teacher) v_rrb <- c(v_rrb, "SRS_RRB_Teacher")
  
  domains <- list(TOTAL = keep(v_total), SOCIAL = keep(v_social), RRB = keep(v_rrb))
  domains[!sapply(domains, is.null)]  # drop empty domains
}

# Short display names for plot labels
short_name <- function(v) {
  lookup <- c(
    ADOS_Severity      = "ADOS Severity",
    ADOS_SA            = "ADOS SA",
    ADOS_RRB           = "ADOS RRB",
    ADI_Total          = "ADI Total",
    ADI_Social_Com     = "ADI Social",
    ADI_RRB            = "ADI RRB",
    SRS_Total_Parent   = "SRS Total (P)",
    SRS_Social_Parent  = "SRS Social (P)",
    SRS_RRB_Parent     = "SRS RRB (P)",
    SRS_Total_Teacher  = "SRS Total (T)",
    SRS_Social_Teacher = "SRS Social (T)",
    SRS_RRB_Teacher    = "SRS RRB (T)",
    RBSR_Total         = "RBSR Total"
  )
  ifelse(v %in% names(lookup), lookup[v], v)
}

# --- 5a. CORRELATION COMPARISON PLOT (dumbbell) ------------------------------
# For each variable pair: dot for male r, dot for female r, connected by line

plot_correlation_comparison <- function(data, vars, label) {
  
  dat_m <- data %>% filter(sex == "Male")   %>% select(all_of(vars)) %>% na.omit()
  dat_f <- data %>% filter(sex == "Female") %>% select(all_of(vars)) %>% na.omit()
  
  n_m <- nrow(dat_m); n_f <- nrow(dat_f)
  if (n_m < 10 || n_f < 10) return(invisible(NULL))
  
  n <- length(vars)
  rows <- list()
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      ct_m <- cor.test(dat_m[[i]], dat_m[[j]])
      ct_f <- cor.test(dat_f[[i]], dat_f[[j]])
      pair_label <- paste0(short_name(vars[i]), " ~\n", short_name(vars[j]))
      rows[[length(rows) + 1]] <- data.frame(
        pair     = pair_label,
        r_male   = ct_m$estimate,
        p_male   = ct_m$p.value,
        r_female = ct_f$estimate,
        p_female = ct_f$p.value,
        stringsAsFactors = FALSE
      )
    }
  }
  df <- do.call(rbind, rows)
  df$pair <- factor(df$pair, levels = rev(df$pair))
  
  # Dumbbell plot
  p <- ggplot(df) +
    geom_segment(aes(x = r_male, xend = r_female, y = pair, yend = pair),
                 color = "gray60", linewidth = 0.8) +
    geom_point(aes(x = r_male, y = pair, shape = ifelse(p_male < .05, "sig", "ns")),
               color = "#2166AC", size = 3.5) +
    geom_point(aes(x = r_female, y = pair, shape = ifelse(p_female < .05, "sig", "ns")),
               color = "#E75480", size = 3.5) +
    scale_shape_manual(values = c(sig = 16, ns = 1), guide = "none") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    labs(title = label,
         subtitle = sprintf("Male (blue, N=%d) vs Female (red, N=%d)  |  Filled = p < .05", n_m, n_f),
         x = "Pearson r", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 10),
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 9)
    )
  
  return(p)
}

# --- 5b. SEX MEANS COMPARISON PLOT (grouped bars) ----------------------------
# Grouped bar chart: mean scores for males vs females on each variable

plot_sex_means <- function(data, vars, label) {
  
  dat_long <- data %>%
    filter(sex %in% c("Male", "Female")) %>%
    select(sex, all_of(vars)) %>%
    na.omit() %>%
    pivot_longer(cols = all_of(vars), names_to = "measure", values_to = "value") %>%
    mutate(measure = short_name(measure))
  
  if (nrow(dat_long) < 20) return(invisible(NULL))
  
  # Compute means and SEs
  summary_df <- dat_long %>%
    group_by(sex, measure) %>%
    summarise(mean = mean(value), se = sd(value) / sqrt(n()), n = n(), .groups = "drop") %>%
    mutate(measure = factor(measure, levels = short_name(vars)))
  
  n_m <- summary_df %>% filter(sex == "Male")   %>% pull(n) %>% max()
  n_f <- summary_df %>% filter(sex == "Female") %>% pull(n) %>% max()
  
  p <- ggplot(summary_df, aes(x = measure, y = mean, fill = sex)) +
    geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.85) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  position = position_dodge(0.7), width = 0.2, linewidth = 0.4) +
    scale_fill_manual(values = c(Male = "#2166AC", Female = "#E75480")) +
    labs(title = label,
         subtitle = sprintf("Mean ± SE  |  Male N≈%d, Female N≈%d", n_m, n_f),
         x = NULL, y = "Mean Score", fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 10),
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 25, hjust = 1, size = 9)
    )
  
  return(p)
}

# --- 5c. FULL SUBSAMPLE FIGURE (all domains in one page) --------------------

save_correlation_page <- function(data, label, file_prefix) {
  domains <- get_domain_vars(data)
  plots <- list()
  for (domain in names(domains)) {
    p <- plot_correlation_comparison(data, domains[[domain]], paste0(label, " | ", domain))
    if (!is.null(p)) plots[[domain]] <- p
  }
  if (length(plots) == 0) return(invisible(NULL))
  
  combined <- patchwork::wrap_plots(plots, ncol = length(plots)) +
    patchwork::plot_annotation(
      title = paste0("Correlation Comparison: ", label),
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
  
  ggsave(file.path(plots_dir, paste0(file_prefix, "_correlations.png")),
         combined, width = 7 * length(plots), height = 9, dpi = 300)
}

save_means_page <- function(data, label, file_prefix) {
  domains <- get_domain_vars(data)
  plots <- list()
  for (domain in names(domains)) {
    p <- plot_sex_means(data, domains[[domain]], paste0(label, " | ", domain))
    if (!is.null(p)) plots[[domain]] <- p
  }
  if (length(plots) == 0) return(invisible(NULL))
  
  combined <- patchwork::wrap_plots(plots, ncol = length(plots)) +
    patchwork::plot_annotation(
      title = paste0("Sex Comparison of Means: ", label),
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
  
  ggsave(file.path(plots_dir, paste0(file_prefix, "_means.png")),
         combined, width = 7 * length(plots), height = 8, dpi = 300)
}

# --- 5d. SEX RATIO PLOT (per domain, stacked bar + ratio annotation) --------

save_sex_ratio_page <- function(data, label, file_prefix) {
  domains <- get_domain_vars(data)
  
  rows <- list()
  for (domain in names(domains)) {
    vars <- domains[[domain]]
    d <- data %>%
      filter(sex %in% c("Male", "Female")) %>%
      select(sex, all_of(vars)) %>%
      na.omit()
    n_m <- sum(d$sex == "Male")
    n_f <- sum(d$sex == "Female")
    if (n_m + n_f == 0) next
    ratio <- round(n_m / max(n_f, 1), 1)
    rows[[domain]] <- data.frame(
      domain = domain,
      sex    = c("Male", "Female"),
      n      = c(n_m, n_f),
      ratio  = ratio,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return(invisible(NULL))
  
  df <- do.call(rbind, rows)
  df$domain <- factor(df$domain, levels = c("TOTAL", "SOCIAL", "RRB"))
  
  # Annotation: ratio text above bars
  ratio_labels <- df %>%
    group_by(domain) %>%
    summarise(total = sum(n), ratio = first(ratio), .groups = "drop") %>%
    mutate(label = sprintf("%s:1  (N=%d)", ratio, total))
  
  p <- ggplot(df, aes(x = domain, y = n, fill = sex)) +
    geom_col(width = 0.6, alpha = 0.85) +
    geom_text(data = ratio_labels,
              aes(x = domain, y = total, label = label, fill = NULL),
              vjust = -0.5, size = 4.5, fontface = "bold") +
    scale_fill_manual(values = c(Male = "#2166AC", Female = "#E75480")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = paste0("Boy:Girl Ratio by Domain — ", label),
         subtitle = "Complete cases per domain (listwise deletion)",
         x = NULL, y = "N (complete cases)", fill = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "gray40", size = 11),
      legend.position = "top",
      panel.grid.major.x = element_blank()
    )
  
  ggsave(file.path(plots_dir, paste0(file_prefix, "_sex_ratio.png")),
         p, width = 8, height = 7, dpi = 300)
}

# --- 6. OUTPUT PIPELINE -------------------------------------------------------

output_dir <- "~/Desktop/MIND/SEM_output"
plots_dir  <- file.path(output_dir, "plots")
data_dir   <- file.path(output_dir, "data")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir,  showWarnings = FALSE)

# Copy script (works in RStudio; silently skips otherwise)
tryCatch({
  script_path <- rstudioapi::getSourceEditorContext()$path
  if (!is.null(script_path) && file.exists(script_path)) {
    file.copy(script_path, file.path(output_dir, "SEM_cov_comparison.R"), overwrite = TRUE)
  }
}, error = function(e) message("Note: could not auto-copy script (not running in RStudio source)"))

# Copy data files
for (f in c("SEM_MIND_schoolage.rds", "SEM_MIND_preschool.rds",
            "SEM_SSC_schoolage.rds",  "SEM_SSC_preschool.rds")) {
  src <- file.path("~/Desktop/MIND", f)
  if (file.exists(src)) file.copy(src, file.path(data_dir, f), overwrite = TRUE)
}

# Master function: text + PNGs for one subsample
run_and_save <- function(data, label, file_prefix) {
  
  teacher <- has_teacher(data)
  domains <- get_domain_vars(data)
  
  # --- Check if subsample has ANY usable data ---
  if (length(domains) == 0) {
    cat("\n\n")
    cat(strrep("#", 70), "\n")
    cat("#  ", label, "\n")
    cat(strrep("#", 70))
    cat("\n  SKIPPED — no variables with sufficient data in any domain\n")
    return(invisible(NULL))
  }
  
  has_data <- FALSE
  for (domain in names(domains)) {
    vars <- domains[[domain]]
    d <- data %>% filter(sex %in% c("Male", "Female")) %>% select(all_of(vars)) %>% na.omit()
    if (nrow(d) >= 20) { has_data <- TRUE; break }
  }
  
  # --- Console text output (captured by sink) ---
  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat("#  ", label, "\n")
  cat(strrep("#", 70))
  if (teacher) cat("\n# Teacher SRS: AVAILABLE\n") else cat("\n# Teacher SRS: not available\n")
  cat("# Available domains/variables:\n")
  for (domain in names(domains)) {
    cat(sprintf("#   %s: %s\n", domain, paste(domains[[domain]], collapse = ", ")))
  }
  
  if (!has_data) {
    cat("\n  SKIPPED — no usable complete cases in any domain\n")
    return(invisible(NULL))
  }
  
  for (domain in names(domains)) {
    compare_cov_text(data, domains[[domain]], paste0(label, " | ", domain))
  }
  
  # --- SEM path diagram PNGs ---
  for (domain in names(domains)) {
    vars <- domains[[domain]]
    d_m <- data %>% filter(sex == "Male")   %>% select(all_of(vars)) %>% na.omit()
    d_f <- data %>% filter(sex == "Female") %>% select(all_of(vars)) %>% na.omit()
    if (nrow(d_m) < 10 || nrow(d_f) < 10) next
    
    png(file.path(plots_dir, paste0(file_prefix, "_sempath_", domain, ".png")),
        width = 4200, height = 2400, res = 300)
    compare_cov_plot(data, domains[[domain]], paste0(label, " | ", domain))
    dev.off()
  }
  
  # --- Correlation comparison ---
  save_correlation_page(data, label, file_prefix)
  
  # --- Sex means comparison ---
  save_means_page(data, label, file_prefix)
  
  # --- Sex ratio by domain ---
  save_sex_ratio_page(data, label, file_prefix)
  
  cat(sprintf("\n  >> Saved all plots for: %s\n", file_prefix))
}

# --- 7. RUN ALL --------------------------------------------------------------

# Start sink
sink(file.path(output_dir, "SEM_results.txt"), split = TRUE)
cat("SEM Covariance Matrix Comparison — Full Output\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(strrep("=", 70), "\n\n")

# Level 1: Full sample
run_and_save(dat, "1. FULL SAMPLE", "1_full_sample")

# Level 2: By source
run_and_save(dat %>% filter(sample_source == "MIND"), "2a. MIND", "2a_MIND")
run_and_save(dat %>% filter(sample_source == "SSC"),  "2b. SSC",  "2b_SSC")

# Level 3: By source × age
run_and_save(dat %>% filter(sample_source == "MIND", age_group == "Preschool"), "3a. MIND Preschool", "3a_MIND_preschool")
run_and_save(dat %>% filter(sample_source == "MIND", age_group == "School"),    "3b. MIND School",    "3b_MIND_school")
run_and_save(dat %>% filter(sample_source == "SSC",  age_group == "Preschool"), "3c. SSC Preschool",  "3c_SSC_preschool")
run_and_save(dat %>% filter(sample_source == "SSC",  age_group == "School"),    "3d. SSC School",     "3d_SSC_school")

# Close sink
sink()
cat("\n✓ All output saved to:", normalizePath(output_dir, mustWork = FALSE), "\n")