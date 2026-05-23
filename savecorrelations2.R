# ============================================================================
# MIND FUNCTION - WITH ALL YOUR CORRECT COLUMN NAMES
# ============================================================================

library(tidyverse)

save_correlation_matrix_MIND <- function(data, group_name, output_dir = "correlation_matrices") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # YOUR EXACT COLUMN NAMES
  # Note: Some columns have variations between preschool and school-age!
  
  # Check which version of ADOS columns exist
  if("ADOS Severity" %in% colnames(data)) {
    ados_severity <- "ADOS Severity"
  } else if("ADOS severity" %in% colnames(data)) {
    ados_severity <- "ADOS severity"
  } else {
    stop("Cannot find ADOS Severity column")
  }
  
  if("ADOS SA" %in% colnames(data)) {
    ados_sa <- "ADOS SA"
  } else if("ADOS SA severity" %in% colnames(data)) {
    ados_sa <- "ADOS SA severity"
  } else {
    stop("Cannot find ADOS SA column")
  }
  
  if("ADOS RRB" %in% colnames(data)) {
    ados_rrb <- "ADOS RRB"
  } else if("ADOS RRB severity" %in% colnames(data)) {
    ados_rrb <- "ADOS RRB severity"
  } else {
    stop("Cannot find ADOS RRB column")
  }
  
  # Check which version of RBS-R exists
  if("rbsr_total" %in% colnames(data)) {
    rbsr <- "rbsr_total"
  } else if("RBSR total" %in% colnames(data)) {
    rbsr <- "RBSR total"
  } else {
    stop("Cannot find RBS-R Total column")
  }
  
  # Build the column list
  mind_cols <- c(
    ados_severity,              # "ADOS Severity" or "ADOS severity"
    ados_sa,                    # "ADOS SA" or "ADOS SA severity"
    ados_rrb,                   # "ADOS RRB" or "ADOS RRB severity"
    "ADI DSM Adj Total",        # Same in all
    "ADI DSM Adj Social",       # Same in all
    "ADI DSM Adj RRB",          # Same in all
    "SRS Total Parent",         # Same in all
    "SRS SCI Parent",           # Same in all
    "SRS RRB Parent",           # Same in all
    rbsr                        # "rbsr_total" or "RBSR total"
  )
  
  # Standardized names for output
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
  
  # Check if all columns exist
  missing_cols <- mind_cols[!mind_cols %in% colnames(data)]
  
  if(length(missing_cols) > 0) {
    cat("\nERROR: Missing columns:\n")
    print(missing_cols)
    cat("\nAvailable columns:\n")
    print(colnames(data))
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
# COLUMN NAME VARIATIONS BETWEEN DATASETS
# ============================================================================

# Preschool has:
# - "ADOS severity" (lowercase s)
# - "ADOS SA severity"
# - "ADOS RRB severity"
# - "RBSR total" (with space)

# School-age has:
# - "ADOS Severity" (uppercase S)
# - "ADOS SA" (no "severity")
# - "ADOS RRB" (no "severity")
# - "rbsr_total" (with underscore)

# The function now handles both automatically!

# ============================================================================
# USAGE
# ============================================================================

# Works for both preschool and school-age now:
# save_correlation_matrix_MIND(adi_ados_plot, "MIND_Preschool_Girls")
# save_correlation_matrix_MIND(adi_ados_plot, "MIND_Schoolage_Girls")