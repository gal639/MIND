# ============================================================================
# MIND FUNCTION - WITH YOUR EXACT COLUMN NAMES
# ============================================================================

library(tidyverse)

save_correlation_matrix_MIND <- function(data, group_name, output_dir = "correlation_matrices") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # YOUR EXACT COLUMN NAMES (with spaces!)
  mind_cols <- c(
    "ADOS severity",           # Note: has SPACE not underscore!
    "ADOS SA severity",        # Note: has SPACE not underscore!
    "ADOS RRB severity",       # Note: has SPACE not underscore!
    "ADI DSM Adj Total",       # Using the DSM-5 adjusted version
    "ADI DSM Adj Social",      # Using the DSM-5 adjusted version
    "ADI DSM Adj RRB",         # Using the DSM-5 adjusted version
    "SRS total parent",        # Note: has SPACE not underscore!
    "SRS SCI parent",          # Note: SCI not "Social"
    "SRS RRB parent",          # Note: has SPACE not underscore!
    "RBSR total"               # Note: has SPACE not underscore!
  )
  
  # Standardized names for output (to match other analyses)
  std_names <- c(
    "ADOS_Severity", 
    "ADOS_SA", 
    "ADOS_RRB",
    "ADI_Total", 
    "ADI_Social_Com", 
    "ADI_RRB",
    "SRS_Total_Parent", 
    "SRS_Social_Parent", 
    "SRS_RRB_Parent",
    "RBSR_Total"
  )
  
  # Check which columns exist
  available_cols <- mind_cols[mind_cols %in% colnames(data)]
  
  if(length(available_cols) < 10) {
    cat("\nWARNING: Only found", length(available_cols), "out of 10 expected columns\n")
    cat("Available columns:\n")
    print(available_cols)
    cat("\nMissing:\n")
    print(mind_cols[!mind_cols %in% colnames(data)])
    stop("Column mismatch!")
  }
  
  # Extract and rename measures
  measures_data <- data %>%
    select(all_of(mind_cols)) %>%
    setNames(std_names)
  
  # ============================================================================
  # COMPUTE CORRELATION MATRIX
  # ============================================================================
  corr_matrix <- cor(measures_data, use = "pairwise.complete.obs")
  
  # ============================================================================
  # COMPUTE SAMPLE SIZE MATRIX
  # ============================================================================
  n_matrix <- matrix(NA, nrow = 10, ncol = 10)
  rownames(n_matrix) <- colnames(n_matrix) <- colnames(measures_data)
  
  for(i in 1:10) {
    for(j in 1:10) {
      n_matrix[i,j] <- sum(complete.cases(measures_data[,c(i,j)]))
    }
  }
  
  # ============================================================================
  # COMPUTE DESCRIPTIVE STATISTICS
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
    descriptives = descriptives,
    group = group_name,
    n_total = nrow(data),
    n_complete = nrow(na.omit(measures_data)),
    timestamp = Sys.time(),
    original_colnames = mind_cols
  )
  
  # Save
  filename <- file.path(output_dir, paste0(group_name, ".rds"))
  saveRDS(output, filename)
  
  cat(sprintf("\n✓ SAVED: %s (N=%d)\n", group_name, nrow(data)))
  cat("  Includes: correlations + sample sizes + descriptive statistics\n\n")
  
  # Print descriptives to console
  cat(strrep("=", 80), "\n")
  cat("DESCRIPTIVE STATISTICS:\n")
  cat(strrep("=", 80), "\n\n")
  print(descriptives, row.names = FALSE)
  cat("\n")
  
  return(invisible(output))
}

# ============================================================================
# USAGE
# ============================================================================

# Now this should work!
# save_correlation_matrix_MIND(adi_ados_plot, "MIND_Preschool_Girls")