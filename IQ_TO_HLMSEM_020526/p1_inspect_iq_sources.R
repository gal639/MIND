# =============================================================================
# Phase 1: Inspect IQ source data + existing RDS files
# Purpose: Identify FSIQ columns and ID conventions BEFORE merging.
# This script does NOT write any files. Run it, review output, then we proceed.
# =============================================================================

setwd("C:/Users/galor/OneDrive/Documents/Desktop/MIND/")

# --- Packages ----------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tibble)
})

# --- Paths -------------------------------------------------------------------
RDS_DIR  <- "C:/Users/galor/OneDrive/Documents/Desktop/MIND/SEM_output/data"
MIND_XLSX <- "C:/Users/galor/OneDrive - University of Haifa/המרכז לאוטיזם/DATA/MIND/MIND_dataset_2024-05-29.xlsx"
SSC_DIR  <- "C:/Users/galor/OneDrive - University of Haifa/המרכז לאוטיזם/DATA/SSC/cognitive"

RDS_FILES <- c(
  "SEM_MIND_preschool.rds",
  "SEM_MIND_schoolage.rds",
  "SEM_SSC_preschool.rds",
  "SEM_SSC_schoolage.rds"
)

# Helper: print a banner so output is scannable
banner <- function(msg) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat(msg, "\n")
  cat(strrep("=", 78), "\n", sep = "")
}

# Helper: spot IQ-ish columns by name pattern
iq_candidates <- function(cols) {
  pat <- "(?i)iq|fsiq|viq|nviq|piq|mullen|elc|das|wasi|wisc|cognitive|composite|standard.score|ss"
  hits <- grep(pat, cols, value = TRUE, perl = TRUE)
  if (length(hits) == 0) "(no obvious IQ-like columns by name)" else hits
}

# Helper: spot ID-ish columns
id_candidates <- function(cols) {
  pat <- "(?i)^id$|subject|sfari|individual|iid|study.?id|family|^pid$|participant"
  hits <- grep(pat, cols, value = TRUE, perl = TRUE)
  if (length(hits) == 0) "(no obvious ID columns by name)" else hits
}

# =============================================================================
# Part A — Existing RDS files
# =============================================================================

banner("PART A — EXISTING RDS FILES")

for (f in RDS_FILES) {
  fp <- file.path(RDS_DIR, f)
  if (!file.exists(fp)) {
    cat("\n[!] MISSING:", fp, "\n"); next
  }
  d <- readRDS(fp)
  cat("\n---", f, "---\n")
  cat("class:    ", paste(class(d), collapse = ", "), "\n")
  cat("dim:      ", nrow(d), "rows x", ncol(d), "cols\n")
  cat("columns:  ", paste(names(d), collapse = ", "), "\n")
  cat("ID col 'id' present? ", "id" %in% names(d), "\n")
  if ("id" %in% names(d)) {
    cat("id class: ", class(d$id), "\n")
    cat("id head:  ", paste(utils::head(d$id, 5), collapse = " | "), "\n")
    cat("id NAs:   ", sum(is.na(d$id)), "\n")
    cat("id dups:  ", sum(duplicated(d$id)), "\n")
  }
  if ("sample" %in% names(d)) {
    cat("sample:   ", paste(unique(d$sample), collapse = ", "), "\n")
  }
  if ("age" %in% names(d)) {
    cat("age range:", round(min(d$age, na.rm = TRUE), 2), "-",
        round(max(d$age, na.rm = TRUE), 2), "\n")
  }
}

# =============================================================================
# Part B — MIND raw xlsx
# =============================================================================

banner("PART B — MIND RAW XLSX")

# List sheets first — sometimes IQ lives on a separate tab
mind_sheets <- excel_sheets(MIND_XLSX)
cat("\nSheets in MIND xlsx:\n")
print(mind_sheets)

# Read each sheet's column names (cheap — no full data load yet)
for (sh in mind_sheets) {
  cat("\n--- Sheet:", sh, "---\n")
  # Read just the header row to get column names without loading full data
  hdr <- tryCatch(
    suppressMessages(read_excel(MIND_XLSX, sheet = sh, n_max = 0)),
    error = function(e) { cat("  (could not read:", conditionMessage(e), ")\n"); NULL }
  )
  if (is.null(hdr)) next
  cols <- names(hdr)
  cat("n cols:        ", length(cols), "\n")
  cat("ID candidates: ", paste(id_candidates(cols), collapse = " | "), "\n")
  cat("IQ candidates: ", paste(iq_candidates(cols), collapse = " | "), "\n")
}

# For the most likely sheet (usually first or the one with most cols), peek at data
cat("\n--- Peeking at first sheet's first 3 rows ---\n")
mind_peek <- suppressMessages(read_excel(MIND_XLSX, sheet = mind_sheets[1], n_max = 3))
print(mind_peek, width = Inf)

# =============================================================================
# Part C — SSC cognitive csvs
# =============================================================================

banner("PART C — SSC COGNITIVE CSVs")

ssc_files <- list.files(SSC_DIR, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
cat("\nCSVs in SSC cognitive folder:\n")
print(basename(ssc_files))

for (fp in ssc_files) {
  cat("\n--- ", basename(fp), " ---\n")
  d <- tryCatch(
    read.csv(fp, stringsAsFactors = FALSE, nrows = 5, check.names = FALSE),
    error = function(e) { cat("  (could not read:", conditionMessage(e), ")\n"); NULL }
  )
  if (is.null(d)) next
  # Read full file just to get N
  n_total <- tryCatch(
    nrow(read.csv(fp, stringsAsFactors = FALSE, check.names = FALSE)),
    error = function(e) NA
  )
  cat("rows:           ", n_total, "\n")
  cat("n cols:         ", ncol(d), "\n")
  cat("ID candidates:  ", paste(id_candidates(names(d)), collapse = " | "), "\n")
  cat("IQ candidates:  ", paste(iq_candidates(names(d)), collapse = " | "), "\n")
  cat("first 3 rows:\n")
  print(d[1:min(3, nrow(d)), ], row.names = FALSE)
}

banner("INSPECTION COMPLETE — paste output back to me")
