# =============================================================================
# SEM Covariance Matrix Comparison + Hierarchical Regression (FIML)
# Boys vs. Girls | MIND + SSC | Preschool + School-Age
# Einat Waizbard, Gal Or — April 2026
# =============================================================================

# Project-relative paths. .Rproj or run_pipeline.R sets the working directory.
# setwd() removed for portability — orchestrator handles WD.

# RDS files: IQ-augmented data (with FSIQ/VIQ/NVIQ from Stage 3).
# Override-able by the orchestrator; defaults to data_with_iq/.
if (!exists("data_src")) data_src <- "SEM_output/data_with_iq"

# All output from this script goes to SEM_output/
output_dir <- "SEM_output"
plots_dir  <- file.path(output_dir, "plots")
reg_dir    <- file.path(output_dir, "regression_output")

dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reg_dir,   recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LIBRARIES
# =============================================================================

library(lavaan)
library(tidyverse)
library(semPlot)
library(patchwork)
library(writexl)

# =============================================================================
# --- 1. LOAD DATA
# =============================================================================

rds1 <- readRDS(file.path(data_src, "SEM_MIND_schoolage.rds"))
rds2 <- readRDS(file.path(data_src, "SEM_MIND_preschool.rds"))
rds3 <- readRDS(file.path(data_src, "SEM_SSC_schoolage.rds"))
rds4 <- readRDS(file.path(data_src, "SEM_SSC_preschool.rds"))

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
    ),
    sample_source_num = as.numeric(sample_source == "SSC")  # FIML auxiliary
  )

# =============================================================================
# --- 2. SHARED HELPER FUNCTIONS
# =============================================================================

sig_stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01,  "**",
                       ifelse(p < .05,  "*", ""))))
}

# Build saturated lavaan model with optional auxiliary block
build_model <- function(vars, aux = NULL) {
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

# Select auxiliary variables for FIML (drops constants, overlaps, sparse)
# Uses vapply(..., logical(1)) instead of sapply to guarantee a logical vector
# even when candidates is empty — sapply returns list() on empty input which
# crashes the [ subscript operator.
build_auxiliaries <- function(data, model_vars) {
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

# Z-score auxiliaries to prevent lavaan variance-scaling warnings
scale_auxiliaries <- function(dat_sub, aux) {
  for (a in aux) {
    if (a %in% names(dat_sub)) {
      sd_a <- sd(dat_sub[[a]], na.rm = TRUE)
      if (!is.na(sd_a) && sd_a > 0)
        dat_sub[[a]] <- as.numeric(scale(dat_sub[[a]]))
    }
  }
  dat_sub
}

build_short_labels <- function(vars) {
  lookup <- c(
    ADOS_Severity = "ADOS\nSeverity", ADOS_SA = "ADOS\nSA", ADOS_RRB = "ADOS\nRRB",
    ADI_Total = "ADI\nTotal", ADI_Social_Com = "ADI\nSocial", ADI_RRB = "ADI\nRRB",
    SRS_Total_Parent = "SRS\nTotal\n(Parent)", SRS_Social_Parent = "SRS\nSocial\n(Parent)",
    SRS_RRB_Parent = "SRS\nRRB\n(Parent)", SRS_Total_Teacher = "SRS\nTotal\n(Teacher)",
    SRS_Social_Teacher = "SRS\nSocial\n(Teacher)", SRS_RRB_Teacher = "SRS\nRRB\n(Teacher)",
    RBSR_Total = "RBSR\nTotal"
  )
  lookup[vars]
}

short_name <- function(v) {
  lookup <- c(
    ADOS_Severity = "ADOS Severity", ADOS_SA = "ADOS SA", ADOS_RRB = "ADOS RRB",
    ADI_Total = "ADI Total", ADI_Social_Com = "ADI Social", ADI_RRB = "ADI RRB",
    SRS_Total_Parent = "SRS Total (P)", SRS_Social_Parent = "SRS Social (P)",
    SRS_RRB_Parent = "SRS RRB (P)", SRS_Total_Teacher = "SRS Total (T)",
    SRS_Social_Teacher = "SRS Social (T)", SRS_RRB_Teacher = "SRS RRB (T)",
    RBSR_Total = "RBSR Total"
  )
  ifelse(v %in% names(lookup), lookup[v], v)
}

cor_sig_table <- function(dat_group, group_label) {
  vars    <- names(dat_group)
  n_vars  <- ncol(dat_group)
  n_total <- nrow(dat_group)
  rows <- list()
  for (i in 1:(n_vars - 1)) {
    for (j in (i + 1):n_vars) {
      x <- dat_group[[i]]; y <- dat_group[[j]]
      valid  <- complete.cases(data.frame(x, y))
      n_pair <- sum(valid)
      if (n_pair < 5) next
      ct <- cor.test(x[valid], y[valid])
      rows[[length(rows) + 1]] <- data.frame(
        Pair = paste(vars[i], "~~", vars[j]),
        r = sprintf("%.3f", ct$estimate), n = n_pair,
        p = sprintf("%.6f", ct$p.value), sig = sig_stars(ct$p.value),
        stringsAsFactors = FALSE
      )
    }
  }
  tbl <- do.call(rbind, rows)
  cat(sprintf("  Correlation significance — %s (N total = %d, pairwise complete):\n",
              group_label, n_total))
  print(tbl, row.names = FALSE, right = FALSE)
  cat("\n")
}

make_edge_labels <- function(fit, vars) {
  pe     <- parameterEstimates(fit, standardized = TRUE)
  pe_cov <- pe[pe$op == "~~" & pe$lhs != pe$rhs, ]
  pe_cov <- pe_cov[pe_cov$lhs %in% vars & pe_cov$rhs %in% vars, ]
  sapply(1:nrow(pe_cov), function(i) {
    sprintf("r=%.2f%s", pe_cov$std.all[i], sig_stars(pe_cov$pvalue[i]))
  })
}

build_layout_matrix <- function(vars) {
  n <- length(vars)
  if (n == 2) return(matrix(c(-1, 0, 1, 0), ncol = 2, byrow = TRUE))
  if (n == 3) return(matrix(c(0, 1, -1, -1, 1, -1), ncol = 2, byrow = TRUE))
  if (n == 4) {
    if ("RBSR_Total" %in% vars)
      return(matrix(c(0, 1, -1, 0, 1, 0, 0, -1), ncol = 2, byrow = TRUE))
    else
      return(matrix(c(-1, 1, -1, -1, 1, -1, 1, 1), ncol = 2, byrow = TRUE))
  }
  if (n == 5)
    return(matrix(c(-1.0, 1.0, -0.7, -0.3, 0.7, -0.3, 0.0, -1.0, 1.0, 1.0),
                  ncol = 2, byrow = TRUE))
  return("circle")
}

fit_table <- function(fit_diff, fit_ident, n_total) {
  indices <- c("chisq", "df", "pvalue", "aic", "bic", "bic2", "rmsea",
               "rmsea.ci.lower", "rmsea.ci.upper")
  fm_diff  <- fitMeasures(fit_diff,  indices)
  fm_ident <- fitMeasures(fit_ident, indices)
  k_diff   <- fitMeasures(fit_diff,  "npar")
  k_ident  <- fitMeasures(fit_ident, "npar")
  aicc_diff  <- fm_diff["aic"]  + (2 * k_diff  * (k_diff  + 1)) / (n_total - k_diff  - 1)
  aicc_ident <- fm_ident["aic"] + (2 * k_ident * (k_ident + 1)) / (n_total - k_ident - 1)
  data.frame(
    Index = c("Chi-sq", "df", "p-value", "AIC", "AICc (adj)", "BIC", "aBIC (adj)",
              "RMSEA", "RMSEA 90% CI"),
    Different = c(
      sprintf("%.3f", fm_diff["chisq"]), sprintf("%.0f", fm_diff["df"]),
      ifelse(is.na(fm_diff["pvalue"]), "—", sprintf("%.4f", fm_diff["pvalue"])),
      sprintf("%.1f", fm_diff["aic"]), sprintf("%.1f", aicc_diff),
      sprintf("%.1f", fm_diff["bic"]), sprintf("%.1f", fm_diff["bic2"]),
      sprintf("%.4f", fm_diff["rmsea"]),
      sprintf("[%.3f, %.3f]", fm_diff["rmsea.ci.lower"], fm_diff["rmsea.ci.upper"])
    ),
    Identical = c(
      sprintf("%.3f", fm_ident["chisq"]), sprintf("%.0f", fm_ident["df"]),
      sprintf("%.4f", fm_ident["pvalue"]),
      sprintf("%.1f", fm_ident["aic"]), sprintf("%.1f", aicc_ident),
      sprintf("%.1f", fm_ident["bic"]), sprintf("%.1f", fm_ident["bic2"]),
      sprintf("%.4f", fm_ident["rmsea"]),
      sprintf("[%.3f, %.3f]", fm_ident["rmsea.ci.lower"], fm_ident["rmsea.ci.upper"])
    ),
    Better = c(
      ifelse(fm_diff["chisq"] < fm_ident["chisq"], "Different", "Identical"), "—", "—",
      ifelse(fm_diff["aic"]   < fm_ident["aic"],   "Different", "Identical"),
      ifelse(aicc_diff        < aicc_ident,         "Different", "Identical"),
      ifelse(fm_diff["bic"]   < fm_ident["bic"],   "Different", "Identical"),
      ifelse(fm_diff["bic2"]  < fm_ident["bic2"],  "Different", "Identical"),
      ifelse(fm_diff["rmsea"] < fm_ident["rmsea"], "Different", "Identical"), "—"
    ),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# --- 3. SEM COMPARISON FUNCTIONS
# =============================================================================

compare_cov_text <- function(data, vars, label) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("  ", label, "\n")
  cat("  Variables:", paste(vars, collapse = ", "), "\n")
  cat(strrep("=", 70), "\n\n")
  
  dat_sub <- data %>% filter(sex %in% c("Male", "Female"))
  n_m <- sum(dat_sub$sex == "Male"); n_f <- sum(dat_sub$sex == "Female")
  n_total <- n_m + n_f
  
  if (n_m < 10 || n_f < 10) {
    cat(sprintf("  SKIPPED — too few observations (Male: %d, Female: %d)\n", n_m, n_f))
    return(invisible(NULL))
  }
  cat(sprintf("N (passed to FIML) — Male: %d, Female: %d, Total: %d\n", n_m, n_f, n_total))
  
  aux     <- build_auxiliaries(dat_sub, vars)
  dat_sub <- scale_auxiliaries(dat_sub, aux)
  model   <- build_model(vars, aux)
  cat(sprintf("Auxiliary variables: %s\n\n",
              if (length(aux) == 0) "none" else paste(aux, collapse = ", ")))
  
  cor_sig_table(dat_sub %>% filter(sex == "Male")   %>% select(all_of(vars)), "Male")
  cor_sig_table(dat_sub %>% filter(sex == "Female") %>% select(all_of(vars)), "Female")
  
  fit_diff <- tryCatch(
    sem(model, data = dat_sub, group = "sex", group.label = c("Male", "Female"),
        missing = "fiml", fixed.x = FALSE, em.h1.iter.max = 5000),
    error = function(e) { cat("  ERROR (free):", e$message, "\n"); NULL }
  )
  fit_ident <- tryCatch(
    sem(model, data = dat_sub, group = "sex", group.label = c("Male", "Female"),
        missing = "fiml", fixed.x = FALSE, em.h1.iter.max = 5000,
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
  lrt   <- lavTestLRT(fit_diff, fit_ident)
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
              ifelse(rmsea_val < 0.05, "(good)", ifelse(rmsea_val < 0.08, "(acceptable)", "(poor)"))))
  cat("\n")
}

compare_cov_plot <- function(data, vars, label) {
  short_labels <- build_short_labels(vars)
  n_vars       <- length(vars)
  
  dat_sub <- data %>% filter(sex %in% c("Male", "Female"))
  dat_m   <- dat_sub %>% filter(sex == "Male")
  dat_f   <- dat_sub %>% filter(sex == "Female")
  n_m <- nrow(dat_m); n_f <- nrow(dat_f)
  if (n_m < 10 || n_f < 10) return(invisible(NULL))
  
  aux_m <- build_auxiliaries(dat_m, vars); aux_f <- build_auxiliaries(dat_f, vars)
  dat_m <- scale_auxiliaries(dat_m, aux_m); dat_f <- scale_auxiliaries(dat_f, aux_f)
  
  fit_male <- tryCatch(
    sem(build_model(vars, aux_m), data = dat_m, missing = "fiml",
        fixed.x = FALSE, em.h1.iter.max = 5000),
    error = function(e) { message("Plot fit error (Male): ", e$message); NULL }
  )
  fit_female <- tryCatch(
    sem(build_model(vars, aux_f), data = dat_f, missing = "fiml",
        fixed.x = FALSE, em.h1.iter.max = 5000),
    error = function(e) { message("Plot fit error (Female): ", e$message); NULL }
  )
  if (is.null(fit_male) || is.null(fit_female)) return(invisible(NULL))
  
  make_display_fit <- function(fit_full, vars, n) {
    cov_full  <- lavInspect(fit_full, "cov.ov")
    mean_full <- lavInspect(fit_full, "mean.ov")
    tryCatch(
      sem(build_model(vars), sample.cov = cov_full[vars, vars, drop = FALSE],
          sample.mean = mean_full[vars], sample.nobs = n),
      error = function(e) { message("Display fit error: ", e$message); NULL }
    )
  }
  fit_male_display   <- make_display_fit(fit_male,   vars, n_m)
  fit_female_display <- make_display_fit(fit_female, vars, n_f)
  if (is.null(fit_male_display) || is.null(fit_female_display)) return(invisible(NULL))
  
  labels_m <- make_edge_labels(fit_male_display,   vars)
  labels_f <- make_edge_labels(fit_female_display, vars)
  lay          <- build_layout_matrix(vars)
  label_cex    <- ifelse(n_vars >= 5, 0.70, 0.85)
  node_size    <- ifelse(n_vars >= 5, 10, 12)
  use_curve    <- ifelse(n_vars >= 4, 1, FALSE)
  use_curveAll <- (n_vars >= 4)
  
  par(mfrow = c(1, 2), oma = c(2, 1, 4, 1))
  semPaths(fit_male_display, what = "std", whatLabels = "omit", style = "lisrel",
           layout = lay, residuals = FALSE, intercepts = FALSE,
           edge.label.cex = label_cex, label.cex = 0.9, nodeLabels = short_labels,
           sizeMan = node_size, posCol = "#B2182B", negCol = "#2166AC",
           fade = FALSE, edge.width = 0.8, color = "white", border.color = "gray30",
           mar = c(8, 8, 10, 8), title = FALSE, ask = FALSE, nCharNodes = 0,
           edgeLabels = labels_m, curve = use_curve, curveAll = use_curveAll)
  title(main = "MALE", col.main = "#2166AC", cex.main = 1.4, line = 2.5, font.main = 2)
  mtext(paste0("N = ", n_m), side = 1, line = 0.5, cex = 0.8, col = "gray40")
  
  semPaths(fit_female_display, what = "std", whatLabels = "omit", style = "lisrel",
           layout = lay, residuals = FALSE, intercepts = FALSE,
           edge.label.cex = label_cex, label.cex = 0.9, nodeLabels = short_labels,
           sizeMan = node_size, posCol = "#B2182B", negCol = "#2166AC",
           fade = FALSE, edge.width = 0.8, color = "white", border.color = "gray30",
           mar = c(8, 8, 10, 8), title = FALSE, ask = FALSE, nCharNodes = 0,
           edgeLabels = labels_f, curve = use_curve, curveAll = use_curveAll)
  title(main = "FEMALE", col.main = "#E75480", cex.main = 1.4, line = 2.5, font.main = 2)
  mtext(paste0("N = ", n_f), side = 1, line = 0.5, cex = 0.8, col = "gray40")
  mtext(label, side = 3, outer = TRUE, line = 0.5, cex = 1.1, font = 2)
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
}

# =============================================================================
# --- 4. TEACHER CHECK
# =============================================================================

has_teacher <- function(data) {
  teacher_vars <- c("SRS_Total_Teacher", "SRS_Social_Teacher", "SRS_RRB_Teacher")
  all(teacher_vars %in% names(data)) &&
    all(sapply(data[teacher_vars], function(x) sum(!is.na(x)) >= 30))
}

# =============================================================================
# --- 5. DOMAIN VARIABLE BUILDER + PLOTS
# =============================================================================

get_domain_vars <- function(data) {
  teacher <- has_teacher(data)
  keep <- function(vars) {
    available <- vars[vars %in% names(data)]
    available <- available[sapply(available, function(v) sum(!is.na(data[[v]])) >= 30)]
    if (length(available) < 2) return(NULL)
    available
  }
  v_total  <- c("ADOS_Severity", "ADI_Total",     "SRS_Total_Parent")
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

plot_correlation_comparison <- function(data, vars, label) {
  dat_m <- data %>% filter(sex == "Male")   %>% select(all_of(vars))
  dat_f <- data %>% filter(sex == "Female") %>% select(all_of(vars))
  n_m <- nrow(dat_m); n_f <- nrow(dat_f)
  if (n_m < 10 || n_f < 10) return(invisible(NULL))
  n <- length(vars); rows <- list()
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      valid_m <- complete.cases(dat_m[, c(i, j)])
      valid_f <- complete.cases(dat_f[, c(i, j)])
      if (sum(valid_m) < 5 || sum(valid_f) < 5) next
      ct_m <- cor.test(dat_m[[i]][valid_m], dat_m[[j]][valid_m])
      ct_f <- cor.test(dat_f[[i]][valid_f], dat_f[[j]][valid_f])
      rows[[length(rows) + 1]] <- data.frame(
        pair = paste0(short_name(vars[i]), " ~\n", short_name(vars[j])),
        r_male = ct_m$estimate, p_male = ct_m$p.value,
        r_female = ct_f$estimate, p_female = ct_f$p.value,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(invisible(NULL))
  df <- do.call(rbind, rows)
  df$pair <- factor(df$pair, levels = rev(df$pair))
  ggplot(df) +
    geom_segment(aes(x = r_male, xend = r_female, y = pair, yend = pair),
                 color = "gray60", linewidth = 0.8) +
    geom_point(aes(x = r_male,   y = pair, shape = ifelse(p_male   < .05, "sig", "ns")),
               color = "#2166AC", size = 3.5) +
    geom_point(aes(x = r_female, y = pair, shape = ifelse(p_female < .05, "sig", "ns")),
               color = "#E75480", size = 3.5) +
    scale_shape_manual(values = c(sig = 16, ns = 1), guide = "none") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    labs(title = label,
         subtitle = sprintf("Male (blue, N=%d) vs Female (red, N=%d)  |  Filled = p < .05  |  Pairwise complete",
                            n_m, n_f),
         x = "Pearson r", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(color = "gray40", size = 10),
          panel.grid.major.y = element_line(color = "gray90"),
          panel.grid.minor = element_blank(), axis.text.y = element_text(size = 9))
}

plot_sex_means <- function(data, vars, label) {
  dat_long <- data %>%
    filter(sex %in% c("Male", "Female")) %>%
    select(sex, all_of(vars)) %>%
    pivot_longer(cols = all_of(vars), names_to = "measure", values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(measure = short_name(measure))
  if (nrow(dat_long) < 20) return(invisible(NULL))
  summary_df <- dat_long %>%
    group_by(sex, measure) %>%
    summarise(mean = mean(value), se = sd(value) / sqrt(n()), n = n(), .groups = "drop") %>%
    mutate(measure = factor(measure, levels = short_name(vars)))
  n_m <- summary_df %>% filter(sex == "Male")   %>% pull(n) %>% max()
  n_f <- summary_df %>% filter(sex == "Female") %>% pull(n) %>% max()
  ggplot(summary_df, aes(x = measure, y = mean, fill = sex)) +
    geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.85) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  position = position_dodge(0.7), width = 0.2, linewidth = 0.4) +
    scale_fill_manual(values = c(Male = "#2166AC", Female = "#E75480")) +
    labs(title = label,
         subtitle = sprintf("Mean ± SE  |  Male N≈%d, Female N≈%d  |  Pairwise complete", n_m, n_f),
         x = NULL, y = "Mean Score", fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(color = "gray40", size = 10),
          legend.position = "top", panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 25, hjust = 1, size = 9))
}

save_correlation_page <- function(data, label, file_prefix) {
  domains <- get_domain_vars(data); plots <- list()
  for (domain in names(domains)) {
    p <- plot_correlation_comparison(data, domains[[domain]], paste0(label, " | ", domain))
    if (!is.null(p)) plots[[domain]] <- p
  }
  if (length(plots) == 0) return(invisible(NULL))
  combined <- patchwork::wrap_plots(plots, ncol = length(plots)) +
    patchwork::plot_annotation(title = paste0("Correlation Comparison: ", label),
                               theme = theme(plot.title = element_text(face = "bold", size = 16)))
  ggsave(file.path(plots_dir, paste0(file_prefix, "_correlations.png")),
         combined, width = 7 * length(plots), height = 9, dpi = 300)
}

save_means_page <- function(data, label, file_prefix) {
  domains <- get_domain_vars(data); plots <- list()
  for (domain in names(domains)) {
    p <- plot_sex_means(data, domains[[domain]], paste0(label, " | ", domain))
    if (!is.null(p)) plots[[domain]] <- p
  }
  if (length(plots) == 0) return(invisible(NULL))
  combined <- patchwork::wrap_plots(plots, ncol = length(plots)) +
    patchwork::plot_annotation(title = paste0("Sex Comparison of Means: ", label),
                               theme = theme(plot.title = element_text(face = "bold", size = 16)))
  ggsave(file.path(plots_dir, paste0(file_prefix, "_means.png")),
         combined, width = 7 * length(plots), height = 8, dpi = 300)
}

save_sex_ratio_page <- function(data, label, file_prefix) {
  domains <- get_domain_vars(data); rows <- list()
  for (domain in names(domains)) {
    vars <- domains[[domain]]
    d <- data %>% filter(sex %in% c("Male", "Female")) %>% select(sex, all_of(vars))
    d_any <- d %>% filter(rowSums(!is.na(select(., all_of(vars)))) > 0)
    n_m <- sum(d_any$sex == "Male"); n_f <- sum(d_any$sex == "Female")
    if (n_m + n_f == 0) next
    rows[[domain]] <- data.frame(domain = domain, sex = c("Male", "Female"),
                                 n = c(n_m, n_f), ratio = round(n_m / max(n_f, 1), 1),
                                 stringsAsFactors = FALSE)
  }
  if (length(rows) == 0) return(invisible(NULL))
  df        <- do.call(rbind, rows)
  df$domain <- factor(df$domain, levels = c("TOTAL", "SOCIAL", "RRB"))
  ratio_labels <- df %>% group_by(domain) %>%
    summarise(total = sum(n), ratio = first(ratio), .groups = "drop") %>%
    mutate(label = sprintf("%s:1  (N=%d)", ratio, total))
  p <- ggplot(df, aes(x = domain, y = n, fill = sex)) +
    geom_col(width = 0.6, alpha = 0.85) +
    geom_text(data = ratio_labels, aes(x = domain, y = total, label = label, fill = NULL),
              vjust = -0.5, size = 4.5, fontface = "bold") +
    scale_fill_manual(values = c(Male = "#2166AC", Female = "#E75480")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = paste0("Boy:Girl Ratio by Domain — ", label),
         subtitle = "Available cases per domain (FIML used for analysis)",
         x = NULL, y = "N (available cases)", fill = NULL) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(color = "gray40", size = 11),
          legend.position = "top", panel.grid.major.x = element_blank())
  ggsave(file.path(plots_dir, paste0(file_prefix, "_sex_ratio.png")),
         p, width = 8, height = 7, dpi = 300)
}

# =============================================================================
# --- 5e. SENSITIVITY ANALYSIS: FIML vs. Listwise (SSC School only)
# =============================================================================

sensitivity_fiml_vs_listwise <- function(data, vars, domain_label) {
  dat_sub  <- data %>% filter(sex %in% c("Male", "Female"))
  aux      <- build_auxiliaries(dat_sub, vars)
  dat_sub  <- scale_auxiliaries(dat_sub, aux)
  model    <- build_model(vars, aux)
  n_m_fiml <- sum(dat_sub$sex == "Male")
  n_f_fiml <- sum(dat_sub$sex == "Female")
  
  fit_fiml <- tryCatch(
    sem(model, data = dat_sub, group = "sex", group.label = c("Male", "Female"),
        missing = "fiml", fixed.x = FALSE, em.h1.iter.max = 5000),
    error = function(e) { cat("  FIML ERROR:", e$message, "\n"); NULL }
  )
  model_lw <- build_model(vars)
  dat_lw   <- dat_sub %>% select(sex, all_of(vars)) %>% na.omit()
  n_m_lw   <- sum(dat_lw$sex == "Male"); n_f_lw <- sum(dat_lw$sex == "Female")
  
  if (n_m_lw < 5 || n_f_lw < 5) {
    cat(sprintf("  SKIPPED listwise arm — too few complete cases (M=%d, F=%d)\n", n_m_lw, n_f_lw))
    return(invisible(NULL))
  }
  cov_m  <- cov(dat_lw %>% filter(sex == "Male")   %>% select(-sex))
  cov_f  <- cov(dat_lw %>% filter(sex == "Female") %>% select(-sex))
  mean_m <- colMeans(dat_lw %>% filter(sex == "Male")   %>% select(-sex))
  mean_f <- colMeans(dat_lw %>% filter(sex == "Female") %>% select(-sex))
  fit_lw <- tryCatch(
    sem(model_lw, sample.cov = list(Male = cov_m, Female = cov_f),
        sample.mean = list(Male = mean_m, Female = mean_f),
        sample.nobs = c(Male = n_m_lw, Female = n_f_lw), group.label = c("Male", "Female")),
    error = function(e) { cat("  Listwise ERROR:", e$message, "\n"); NULL }
  )
  if (is.null(fit_fiml) || is.null(fit_lw)) return(invisible(NULL))
  
  extract_srs <- function(fit, method_label) {
    parameterEstimates(fit, standardized = TRUE) %>%
      filter(op == "~~", lhs != rhs) %>%
      filter(grepl("SRS.*Parent", lhs) | grepl("SRS.*Parent", rhs)) %>%
      mutate(method = method_label, group_label = ifelse(group == 1, "Male", "Female")) %>%
      select(method, group_label, lhs, rhs, est, se, std.all, pvalue)
  }
  comp <- extract_srs(fit_fiml, "FIML") %>%
    rename(est_fiml = est, se_fiml = se, std_fiml = std.all, p_fiml = pvalue) %>%
    left_join(
      extract_srs(fit_lw, "Listwise") %>%
        rename(est_lw = est, se_lw = se, std_lw = std.all, p_lw = pvalue) %>%
        select(-method),
      by = c("group_label", "lhs", "rhs")
    ) %>%
    mutate(abs_diff_std = round(abs(std_fiml - std_lw), 4), diverged = abs_diff_std > 0.05)
  
  cat(strrep("-", 70), "\n")
  cat(sprintf("  Sensitivity: %s\n  FIML N:      Male = %d, Female = %d\n  Listwise N:  Male = %d, Female = %d\n\n",
              domain_label, n_m_fiml, n_f_fiml, n_m_lw, n_f_lw))
  print(comp, row.names = FALSE)
  n_div <- sum(comp$diverged, na.rm = TRUE)
  cat(sprintf("\n  Parameters with |Δstd| > .05: %d / %d\n", n_div, nrow(comp)))
  cat(if (n_div == 0) "  VERDICT: FIML and listwise converge — MNAR impact negligible.\n"
      else            "  VERDICT: Meaningful divergence detected — report sensitivity results.\n")
  cat("\n")
}

# =============================================================================
# --- 6. SEM PIPELINE FUNCTION
# =============================================================================

run_and_save <- function(data, label, file_prefix) {
  teacher <- has_teacher(data)
  domains <- get_domain_vars(data)
  if (length(domains) == 0) {
    cat("\n\n"); cat(strrep("#", 70), "\n"); cat("#  ", label, "\n"); cat(strrep("#", 70))
    cat("\n  SKIPPED — no variables with sufficient data\n")
    return(invisible(NULL))
  }
  n_m <- sum(data$sex == "Male", na.rm = TRUE); n_f <- sum(data$sex == "Female", na.rm = TRUE)
  cat("\n\n"); cat(strrep("#", 70), "\n"); cat("#  ", label, "\n"); cat(strrep("#", 70))
  cat(if (teacher) "\n# Teacher SRS: AVAILABLE\n" else "\n# Teacher SRS: not available\n")
  cat("# Available domains/variables:\n")
  for (domain in names(domains))
    cat(sprintf("#   %s: %s\n", domain, paste(domains[[domain]], collapse = ", ")))
  if (n_m < 10 || n_f < 10) {
    cat("\n  SKIPPED — insufficient sex-labeled observations\n")
    return(invisible(NULL))
  }
  for (domain in names(domains))
    compare_cov_text(data, domains[[domain]], paste0(label, " | ", domain))
  for (domain in names(domains)) {
    png(file.path(plots_dir, paste0(file_prefix, "_sempath_", domain, ".png")),
        width = 4200, height = 2400, res = 300)
    compare_cov_plot(data, domains[[domain]], paste0(label, " | ", domain))
    dev.off()
  }
  save_correlation_page(data, label, file_prefix)
  save_means_page(data, label, file_prefix)
  save_sex_ratio_page(data, label, file_prefix)
  cat(sprintf("\n  >> Saved all plots for: %s\n", file_prefix))
}

# =============================================================================
# --- 7. RUN SEM
# =============================================================================

sink(file.path(output_dir, "SEM_results.txt"), split = TRUE)
cat("SEM Covariance Matrix Comparison — Full Output\n")
cat("Estimation: FIML with auxiliary variables (age, ADOS_Severity, sample_source_num)\n")
cat("Correlations: pairwise complete observations\n")
cat(sprintf("Generated: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
cat(strrep("=", 70), "\n\n")

run_and_save(dat, "1. FULL SAMPLE", "1_full_sample")
run_and_save(dat %>% filter(sample_source == "MIND"), "2a. MIND", "2a_MIND")
run_and_save(dat %>% filter(sample_source == "SSC"),  "2b. SSC",  "2b_SSC")
run_and_save(dat %>% filter(sample_source == "MIND", age_group == "Preschool"), "3a. MIND Preschool", "3a_MIND_preschool")
run_and_save(dat %>% filter(sample_source == "MIND", age_group == "School"),    "3b. MIND School",    "3b_MIND_school")
run_and_save(dat %>% filter(sample_source == "SSC",  age_group == "Preschool"), "3c. SSC Preschool",  "3c_SSC_preschool")
run_and_save(dat %>% filter(sample_source == "SSC",  age_group == "School"),    "3d. SSC School",     "3d_SSC_school")

cat("\n\n"); cat(strrep("#", 70), "\n")
cat("# SENSITIVITY ANALYSIS: FIML vs. Listwise — SSC School\n")
cat(strrep("#", 70), "\n\n")
dat_ssc_school <- dat %>% filter(sample_source == "SSC", age_group == "School")
domains_ssc    <- get_domain_vars(dat_ssc_school)
for (domain in names(domains_ssc)) {
  vars <- domains_ssc[[domain]]
  if (any(grepl("SRS.*Parent", vars)))
    sensitivity_fiml_vs_listwise(dat_ssc_school, vars, paste0("SSC School | ", domain))
}
sink()
cat("\n✓ SEM output saved to:", normalizePath(output_dir, mustWork = FALSE), "\n")

# =============================================================================
# --- 8. SENSITIVITY ANALYSIS (written above inside Section 7 sink)
# =============================================================================
# (already executed above, no separate section needed)