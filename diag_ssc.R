# ============================================================================
# DIAGNOSTIC: Check Your Exact Column Names
# ============================================================================

# Run this first to see what columns you actually have:

cat("\n")
cat(strrep("=", 80), "\n")
cat("YOUR ACTUAL COLUMN NAMES IN adi_ados_plot:\n")
cat(strrep("=", 80), "\n\n")

print(colnames(ssc_publish))

cat("\n\n")
cat(strrep("=", 80), "\n")
cat("SEARCHING FOR AUTISM MEASURE COLUMNS:\n")
cat(strrep("=", 80), "\n\n")

# Search for ADOS columns
ados_cols <- grep("ados", colnames(ssc_publish), ignore.case = TRUE, value = TRUE)
cat("ADOS columns found:\n")
print(ados_cols)

# Search for ADI columns
adi_cols <- grep("adi", colnames(ssc_publish), ignore.case = TRUE, value = TRUE)
cat("\nADI columns found:\n")
print(adi_cols)

# Search for SRS columns
srs_cols <- grep("srs", colnames(ssc_publish), ignore.case = TRUE, value = TRUE)
cat("\nSRS columns found:\n")
print(srs_cols)

# Search for RBS/RBSR columns
rbs_cols <- grep("rbs", colnames(ssc_publish), ignore.case = TRUE, value = TRUE)
cat("\nRBS-R columns found:\n")
print(rbs_cols)

cat("\n")
cat(strrep("=", 80), "\n")
cat("COPY THESE EXACT NAMES TO UPDATE THE FUNCTION\n")
cat(strrep("=", 80), "\n")