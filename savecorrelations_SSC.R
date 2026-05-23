# ============================================================================
# UPDATED SSC FUNCTION: Now saves correlations + descriptive statistics
# ============================================================================

library(tidyverse)

save_correlation_matrix_SSC <- function(data, group_name, output_dir = "correlation_matrices") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Define SSC column names (lowercase)
  ssc_cols <- c(
    "ados_css", "ados_sa_css", "ados_rrb_css",
    "total_dsm5_adj", "sc_dsm5_adj", "rrb_dsm5_adj",
    "srs_total_parent", "srs_social_parent", "srs_rrb_parent",
    "srs_total_teacher", "srs_social_teacher", "srs_rrb_teacher",
    "rbsr_total"
  )
  
  # Standardized names for output (to match MIND format)
  std_names <- c(
    "ADOS_Severity", "ADOS_SA", "ADOS_RRB",
    "ADI_Total", "ADI_Social_Com", "ADI_RRB",
    "SRS_Total_Parent", "SRS_Social_Parent", "SRS_RRB_Parent",
    "SRS_Total_Teacher", "SRS_Social_Teacher", "SRS_RRB_Teacher",
    "RBSR_Total"
  )
  
  # Extract and rename measures
  measures_data <- data %>%
    select(all_of(ssc_cols)) %>%
    setNames(std_names)
  
  # ============================================================================
  # COMPUTE CORRELATION MATRIX
  # ============================================================================
  corr_matrix <- cor(measures_data, use = "pairwise.complete.obs")
  
  # ============================================================================
  # COMPUTE SAMPLE SIZE MATRIX
  # ============================================================================
  n_matrix <- matrix(NA, nrow = 13, ncol = 13)
  rownames(n_matrix) <- colnames(n_matrix) <- colnames(measures_data)
  
  for(i in 1:10) {
    for(j in 1:10) {
      n_matrix[i,j] <- sum(complete.cases(measures_data[,c(i,j)]))
    }
  }
  
  # ============================================================================
  # NEW: COMPUTE DESCRIPTIVE STATISTICS
  # ============================================================================
  descriptives <- data.frame(
    measure = colnames(measures_data),
    n_valid = colSums(!is.na(measures_data)),
    n_missing = colSums(is.na(measures_data)),
    pct_missing = round(colSums(is.na(measures_data)) / nrow(measures_data) * 100, 1),
    mean = round(colMeans(measures_data, na.rm = TRUE), 2),
    sd = round(apply(measures_data, 2, sd, na.rm = TRUE), 2),
    median = round(apply(measures_data, 2, median, na.rm = TRUE), 2),
    min = round(apply(measures_data, 2, min, na.rm = TRUE), 2),
    max = round(apply(measures_data, 2, max, na.rm = TRUE), 2),
    range = round(apply(measures_data, 2, function(x) diff(range(x, na.rm = TRUE))), 2),
    Q1 = round(apply(measures_data, 2, quantile, probs = 0.25, na.rm = TRUE), 2),
    Q3 = round(apply(measures_data, 2, quantile, probs = 0.75, na.rm = TRUE), 2),
    IQR = round(apply(measures_data, 2, IQR, na.rm = TRUE), 2)
  )
  
  # ============================================================================
  # CREATE OUTPUT WITH ALL INFORMATION
  # ============================================================================
  output <- list(
    corr = corr_matrix,
    n = n_matrix,
    descriptives = descriptives,  # NEW!
    group = group_name,
    n_total = nrow(data),
    n_complete = nrow(na.omit(measures_data)),
    timestamp = Sys.time(),
    original_colnames = ssc_cols
  )
  
  # Save
  filename <- file.path(output_dir, paste0(group_name, ".rds"))
  saveRDS(output, filename)
  
  cat(sprintf("✓ SAVED: %s (N=%d)\n", group_name, nrow(data)))
  cat("  Includes: correlations + sample sizes + descriptive statistics\n")
  
  # Print descriptives to console
  cat("\nDescriptive Statistics:\n")
  print(descriptives, row.names = FALSE)
  cat("\n")
  
  return(invisible(output))
}

# ============================================================================
# USAGE - Use exactly like before, but now it saves descriptives too!
# ============================================================================

# save_correlation_matrix_SSC(ssc_data, "SSC_Preschool_Girls")
# save_correlation_matrix_SSC(ssc_data, "SSC_Preschool_Boys")
# save_correlation_matrix_SSC(ssc_data, "SSC_Schoolage_Girls")
# save_correlation_matrix_SSC(ssc_data, "SSC_Schoolage_Boys")

# ============================================================================
# TO VIEW SAVED DESCRIPTIVES
# ============================================================================

view_descriptives_SSC <- function(rds_file) {
  data <- readRDS(rds_file)
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("GROUP:", data$group, "\n")
  cat(strrep("=", 80), "\n")
  cat("Total N:", data$n_total, "\n")
  cat("Complete cases (all 10 measures):", data$n_complete, "\n")
  cat(strrep("=", 80), "\n\n")
  
  print(data$descriptives, row.names = FALSE)
  
  cat("\n")
}

# Example:
# view_descriptives_SSC("correlation_matrices/SSC_Preschool_Girls.rds")