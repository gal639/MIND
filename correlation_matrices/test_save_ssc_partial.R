# ============================================================================
# UPDATED SSC FUNCTION: Smart Selection for Partial Correlations
# ============================================================================

library(tidyverse)

save_correlation_matrix_SSC_partial <- function(data, group_name, output_dir = "correlation_matrices") {
  
  # Create output directory
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # 1. DEFINE ALL COLUMNS (For Descriptives)
  ssc_cols <- c(
    "ados_css", "ados_sa_css", "ados_rrb_css",
    "total_dsm5_adj", "sc_dsm5_adj", "rrb_dsm5_adj",
    "srs_total_parent", "srs_social_parent", "srs_rrb_parent",
    "rbsr_total"
  )
  
  std_names <- c(
    "ADOS_Severity", "ADOS_SA", "ADOS_RRB",
    "ADI_Total", "ADI_Social_Com", "ADI_RRB",
    "SRS_Total_Parent", "SRS_Social_Parent", "SRS_RRB_Parent",
    "RBSR_Total"
  )
  
  # Load all data
  all_data <- data %>%
    select(all_of(ssc_cols)) %>%
    setNames(std_names)
  
  # 2. SELECTION FOR MATRIX (The Fix!)
  # We select ONLY the distinct subscales to avoid Singularity.
  # We exclude "Totals" that are sums of other columns.
  matrix_vars <- c(
    "ADOS_SA", "ADOS_RRB",           # Exclude ADOS_Severity
    "ADI_Social_Com", "ADI_RRB",     # Exclude ADI_Total
    "SRS_Social_Parent", "SRS_RRB_Parent", # Exclude SRS_Total
    "RBSR_Total"                     # Keep RBSR (No subscales present)
  )
  
  matrix_data <- all_data %>% select(all_of(matrix_vars))
  
  # ============================================================================
  # COMPUTE CORRELATION MATRIX (On the "Clean" Subscale Data)
  # ============================================================================
  corr_matrix <- cor(matrix_data, use = "pairwise.complete.obs")
  
  # ============================================================================
  # COMPUTE SAMPLE SIZE MATRIX (On the "Clean" Subscale Data)
  # ============================================================================
  n_matrix <- matrix(NA, nrow = length(matrix_vars), ncol = length(matrix_vars))
  rownames(n_matrix) <- colnames(n_matrix) <- colnames(matrix_data)
  
  for(i in 1:length(matrix_vars)) {
    for(j in 1:length(matrix_vars)) {
      n_matrix[i,j] <- sum(complete.cases(matrix_data[,c(i,j)]))
    }
  }
  
  # ============================================================================
  # COMPUTE DESCRIPTIVE STATISTICS (On ALL Data)
  # ============================================================================
  # We still calculate means/SDs for the Totals, because that's useful info.
  descriptives <- data.frame(
    measure = colnames(all_data),
    n_valid = colSums(!is.na(all_data)),
    mean = round(colMeans(all_data, na.rm = TRUE), 2),
    sd = round(apply(all_data, 2, sd, na.rm = TRUE), 2),
    min = round(apply(all_data, 2, min, na.rm = TRUE), 2),
    max = round(apply(all_data, 2, max, na.rm = TRUE), 2)
  )
  
  # ============================================================================
  # OUTPUT
  # ============================================================================
  output <- list(
    corr = corr_matrix,         # Clean Matrix (Subscales only)
    n = n_matrix,
    descriptives = descriptives,# Full Descriptives (Includes Totals)
    group = group_name,
    n_total = nrow(data),
    timestamp = Sys.time()
  )
  
  filename <- file.path(output_dir, paste0(group_name, ".rds"))
  saveRDS(output, filename)
  
  cat(sprintf("✓ SAVED: %s (Matrix dim: %dx%d)\n", group_name, nrow(corr_matrix), ncol(corr_matrix)))
  return(invisible(output))
}
