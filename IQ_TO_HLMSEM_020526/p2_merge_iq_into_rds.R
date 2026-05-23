# =============================================================================
# Phase 2: Merge FSIQ + VIQ + NVIQ into the 4 RDS files
# - MIND:  pull iq_fsiq / iq_viq / iq_nviq + iq_version from MIND xlsx
# - SSC:   pull ssc_diagnosis_full_scale_iq / verbal_iq / nonverbal_iq
#          from ssc_core_descriptive.csv (pre-harmonized by SSC team)
# - Output: 4 new RDS files in SEM_output/data_with_iq/ with:
#           FSIQ, VIQ, NVIQ,
#           IQ_instrument, VIQ_instrument, NVIQ_instrument,
#           IQ_source
# - Originals untouched.
# =============================================================================

# setwd() removed — project root set by .Rproj / run_pipeline.R

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tibble)
})

# --- Paths -------------------------------------------------------------------
# Project-internal paths (relative to MIND/ project root)
RDS_DIR <- "SEM_output/data"
OUT_DIR <- "SEM_output/data_with_iq"

# Lab data root — lives OUTSIDE the project, configurable per machine.
# Default falls back to the typical Windows location; can be overridden by
# setting LAB_DATA_ROOT in the calling environment (e.g., run_pipeline.R).
if (!exists("LAB_DATA_ROOT")) {
  LAB_DATA_ROOT <- "C:/Users/galor/OneDrive - University of Haifa/המרכז לאוטיזם/DATA"
}
MIND_XLSX     <- file.path(LAB_DATA_ROOT, "MIND", "MIND_dataset_2024-05-29.xlsx")
SSC_CORE_DESC <- file.path(LAB_DATA_ROOT, "SSC", "Summary - general", "ssc_core_descriptive.csv")


dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Helper: print banner
banner <- function(msg) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat(msg, "\n")
  cat(strrep("=", 78), "\n", sep = "")
}

# Helper: summarize FSIQ in a merged df
summarize_fsiq <- function(d, label) {
  cat("\n>>>", label, "\n")
  cat("  N rows:           ", nrow(d), "\n")
  cat("  N FSIQ non-miss:  ", sum(!is.na(d$FSIQ)), " (",
      round(100 * mean(!is.na(d$FSIQ)), 1), "%)\n", sep = "")
  if ("VIQ" %in% names(d)) {
    cat("  N VIQ non-miss:   ", sum(!is.na(d$VIQ)), " (",
        round(100 * mean(!is.na(d$VIQ)), 1), "%)\n", sep = "")
  }
  if ("NVIQ" %in% names(d)) {
    cat("  N NVIQ non-miss:  ", sum(!is.na(d$NVIQ)), " (",
        round(100 * mean(!is.na(d$NVIQ)), 1), "%)\n", sep = "")
  }
  if (sum(!is.na(d$FSIQ)) > 0) {
    cat("  FSIQ mean (SD):   ",
        round(mean(d$FSIQ, na.rm = TRUE), 2), " (",
        round(sd(d$FSIQ, na.rm = TRUE), 2), ")\n", sep = "")
    cat("  FSIQ range:       ",
        round(min(d$FSIQ, na.rm = TRUE), 1), " - ",
        round(max(d$FSIQ, na.rm = TRUE), 1), "\n", sep = "")
    # by sex
    if ("sex" %in% names(d)) {
      by_sex <- d %>%
        filter(!is.na(FSIQ)) %>%
        group_by(sex) %>%
        summarise(n = n(),
                  mean = round(mean(FSIQ), 2),
                  sd = round(sd(FSIQ), 2),
                  .groups = "drop")
      cat("  by sex:\n")
      print(by_sex, row.names = FALSE)
    }
    # by source
    if ("IQ_source" %in% names(d)) {
      by_src <- d %>%
        filter(!is.na(FSIQ)) %>%
        count(IQ_source, name = "n")
      cat("  by IQ_source:\n")
      print(by_src, row.names = FALSE)
    }
    # by instrument (MIND only — SSC has NA here)
    if ("IQ_instrument" %in% names(d) && any(!is.na(d$IQ_instrument))) {
      by_inst <- d %>%
        filter(!is.na(FSIQ), !is.na(IQ_instrument)) %>%
        group_by(IQ_instrument) %>%
        summarise(n = n(),
                  mean = round(mean(FSIQ), 2),
                  sd = round(sd(FSIQ), 2),
                  .groups = "drop")
      cat("  by IQ_instrument:\n")
      print(by_inst, row.names = FALSE)
    }
  }
}

# =============================================================================
# Part 1 — Build MIND IQ lookup
# =============================================================================

banner("PART 1 — BUILD MIND IQ LOOKUP")

mind_raw <- suppressMessages(read_excel(MIND_XLSX, sheet = "Sheet1"))
cat("MIND raw rows:", nrow(mind_raw), "\n")

mind_iq_all <- mind_raw %>%
  select(id = subj_id, FSIQ = iq_fsiq, VIQ = iq_viq, NVIQ = iq_nviq,
         IQ_instrument = iq_version, iq_age) %>%
  mutate(
    id              = as.character(id),
    FSIQ            = suppressWarnings(as.numeric(FSIQ)),
    VIQ             = suppressWarnings(as.numeric(VIQ)),
    NVIQ            = suppressWarnings(as.numeric(NVIQ)),
    iq_age          = suppressWarnings(as.numeric(iq_age)),
    IQ_instrument   = as.character(IQ_instrument),
    VIQ_instrument  = IQ_instrument,
    NVIQ_instrument = IQ_instrument,
    IQ_source       = ifelse(!is.na(FSIQ), "MIND_iq_fsiq", NA_character_)
  ) %>%
  filter(!is.na(FSIQ), !is.na(iq_age))  # need both for age-matching

cat("MIND IQ lookup (all visits w/ FSIQ): ", nrow(mind_iq_all), " rows, ",
    length(unique(mind_iq_all$id)), " unique ids\n", sep = "")
cat("  with VIQ:  ", sum(!is.na(mind_iq_all$VIQ)),  "\n", sep = "")
cat("  with NVIQ: ", sum(!is.na(mind_iq_all$NVIQ)), "\n", sep = "")

cat("\nVisits per id (MIND):\n")
print(mind_iq_all %>% count(id) %>% count(n, name = "n_kids"))

cat("\nIQ instrument distribution across all MIND visits:\n")
print(mind_iq_all %>% count(IQ_instrument, name = "n"), row.names = FALSE)

# =============================================================================
# Part 2 — Build SSC IQ lookup from ssc_core_descriptive.csv
#   This is SSC's pre-harmonized IQ summary. Use directly instead of pooling
#   raw instrument files — matches SSC literature, handles edge cases properly.
# =============================================================================

banner("PART 2 — BUILD SSC IQ LOOKUP")

SSC_CORE <- SSC_CORE_DESC

ssc_raw <- read.csv(SSC_CORE, stringsAsFactors = FALSE, check.names = FALSE)
cat("SSC core_descriptive rows:", nrow(ssc_raw), "\n")

ssc_iq <- ssc_raw %>%
  select(
    id            = individual,
    FSIQ          = ssc_diagnosis_full_scale_iq,
    VIQ           = ssc_diagnosis_verbal_iq,
    NVIQ          = ssc_diagnosis_nonverbal_iq,
    IQ_instrument = ssc_diagnosis_full_scale_iq_type,
    VIQ_instrument  = ssc_diagnosis_verbal_iq_type,
    NVIQ_instrument = ssc_diagnosis_nonverbal_iq_type
  ) %>%
  mutate(
    id              = as.character(id),
    FSIQ            = suppressWarnings(as.numeric(FSIQ)),
    VIQ             = suppressWarnings(as.numeric(VIQ)),
    NVIQ            = suppressWarnings(as.numeric(NVIQ)),
    # SSC dictionary: 1 = deviation IQ (standard score), 2 = ratio IQ (DQ)
    IQ_instrument   = dplyr::recode(as.character(IQ_instrument),
                                    "1" = "deviation", "2" = "ratio",
                                    .default = NA_character_),
    VIQ_instrument  = dplyr::recode(as.character(VIQ_instrument),
                                    "1" = "deviation", "2" = "ratio",
                                    .default = NA_character_),
    NVIQ_instrument = dplyr::recode(as.character(NVIQ_instrument),
                                    "1" = "deviation", "2" = "ratio",
                                    .default = NA_character_),
    IQ_source       = ifelse(!is.na(FSIQ), "SSC_diagnosis_FSIQ", NA_character_)
  )

cat("SSC IQ lookup: ", nrow(ssc_iq), " unique ids, ",
    sum(!is.na(ssc_iq$FSIQ)), " with FSIQ\n", sep = "")
cat("  with VIQ:  ", sum(!is.na(ssc_iq$VIQ)),  "\n", sep = "")
cat("  with NVIQ: ", sum(!is.na(ssc_iq$NVIQ)), "\n", sep = "")

cat("\nFSIQ instrument breakdown (SSC):\n")
print(ssc_iq %>% filter(!is.na(FSIQ)) %>% count(IQ_instrument, name = "n"),
      row.names = FALSE)

# =============================================================================
# Part 3 — Merge into each RDS file
# =============================================================================

banner("PART 3 — MERGE + SAVE")

# Helper: pick closest MIND visit per id, given RDS age (in months)
pick_closest_mind_visit <- function(rds_df, mind_lookup) {
  # For each RDS row, find row in mind_lookup with same id and min |iq_age - age|
  rds_df %>%
    select(id, age) %>%
    distinct() %>%
    left_join(mind_lookup, by = "id", relationship = "many-to-many") %>%
    mutate(age_diff = abs(iq_age - age)) %>%
    group_by(id, age) %>%
    arrange(age_diff, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(id, age, FSIQ, VIQ, NVIQ,
           IQ_instrument, VIQ_instrument, NVIQ_instrument,
           IQ_source, iq_age_used = iq_age, iq_age_diff = age_diff)
}

merge_one <- function(rds_name, lookup, label, cohort) {
  in_fp  <- file.path(RDS_DIR, rds_name)
  out_fp <- file.path(OUT_DIR, rds_name)
  
  d <- readRDS(in_fp)
  orig_class <- class(d)
  orig_n     <- nrow(d)
  
  if (cohort == "MIND") {
    # Age-match: pick closest visit per (id, age)
    matched <- pick_closest_mind_visit(d, lookup)
    d_new <- d %>%
      left_join(matched, by = c("id", "age"))
  } else {
    # SSC: simple join (lookup has one row per id)
    d_new <- d %>%
      left_join(
        lookup %>% select(id, FSIQ, VIQ, NVIQ,
                          IQ_instrument, VIQ_instrument, NVIQ_instrument,
                          IQ_source),
        by = "id"
      )
  }
  
  # Sanity checks
  cat("\n---", rds_name, "---\n")
  cat("  rows before/after: ", orig_n, " -> ", nrow(d_new), "\n", sep = "")
  if (nrow(d_new) != orig_n) {
    stop("ROW COUNT CHANGED during join — duplicated keys in lookup?")
  }
  
  matched_n <- sum(!is.na(d_new$FSIQ))
  cat("  matched FSIQ:      ", matched_n, " / ", orig_n,
      " (", round(100 * matched_n / orig_n, 1), "%)\n", sep = "")
  
  unmatched_ids <- d_new$id[is.na(d_new$FSIQ)]
  if (length(unmatched_ids) > 0) {
    show_n <- min(10, length(unmatched_ids))
    cat("  unmatched ids (first ", show_n, " of ",
        length(unmatched_ids), "): ",
        paste(utils::head(unmatched_ids, show_n), collapse = ", "),
        "\n", sep = "")
  }
  
  # MIND only: report age-matching quality
  if (cohort == "MIND" && "iq_age_diff" %in% names(d_new)) {
    diffs <- d_new$iq_age_diff[!is.na(d_new$iq_age_diff)]
    cat("  age-match quality (|RDS_age - iq_age|, months):\n")
    cat("    median: ", round(median(diffs), 1),
        " | mean: ", round(mean(diffs), 1),
        " | max: ", round(max(diffs), 1), "\n", sep = "")
    cat("    visits w/ diff > 12mo: ", sum(diffs > 12),
        " / ", length(diffs), "\n", sep = "")
  }
  
  summarize_fsiq(d_new, label)
  
  # Preserve original class
  if ("tbl_df" %in% orig_class) {
    d_new <- as_tibble(d_new)
  } else {
    d_new <- as.data.frame(d_new)
  }
  
  saveRDS(d_new, out_fp)
  cat("  saved -> ", out_fp, "\n", sep = "")
  invisible(d_new)
}

merge_one("SEM_MIND_preschool.rds",  mind_iq_all, "MIND_preschool", "MIND")
merge_one("SEM_MIND_schoolage.rds",  mind_iq_all, "MIND_schoolage", "MIND")
merge_one("SEM_SSC_preschool.rds",   ssc_iq,      "SSC_preschool",  "SSC")
merge_one("SEM_SSC_schoolage.rds",   ssc_iq,      "SSC_schoolage",  "SSC")

# =============================================================================
# Part 4 — Verify by re-reading new files
# =============================================================================

banner("PART 4 — VERIFY (re-read new RDS)")

for (f in c("SEM_MIND_preschool.rds","SEM_MIND_schoolage.rds",
            "SEM_SSC_preschool.rds","SEM_SSC_schoolage.rds")) {
  d <- readRDS(file.path(OUT_DIR, f))
  cat("\n", f, "\n",
      "  class:    ", paste(class(d), collapse = ", "), "\n",
      "  dim:      ", nrow(d), " x ", ncol(d), "\n",
      "  has FSIQ: ", "FSIQ" %in% names(d), "\n",
      "  has VIQ:  ", "VIQ"  %in% names(d), "\n",
      "  has NVIQ: ", "NVIQ" %in% names(d), "\n",
      "  has IQ_source: ", "IQ_source" %in% names(d), "\n",
      "  has IQ_instrument: ", "IQ_instrument" %in% names(d), "\n",
      sep = "")
}

banner("DONE")
cat("New RDS files in: ", OUT_DIR, "\n", sep = "")
cat("Downstream change needed:\n")
cat("  data_src <- file.path(dirname(getwd()), 'SEM_output', 'data_with_iq')\n")
cat("  IQ_VAR   <- 'FSIQ'\n")
cat("  Add to common_cols: 'FSIQ', 'VIQ', 'NVIQ',\n")
cat("                      'IQ_instrument', 'VIQ_instrument', 'NVIQ_instrument',\n")
cat("                      'IQ_source'\n")
cat("\nNOTE: Mixed-scale IQ values present in BOTH cohorts.\n")
cat("  MIND IQ_instrument:\n")
cat("    - DAS Early Years / DAS School Age / SB = standard scores (use directly)\n")
cat("    - MSEL = ratio DQ scale, NOT comparable to standard scores\n")
cat("  SSC IQ_instrument:\n")
cat("    - 'deviation' = standard score (use directly)\n")
cat("    - 'ratio' = ratio IQ / DQ, NOT comparable to standard scores\n")
cat("  Decide with Einat whether to filter ratio-scale kids before IQ analyses.\n")
cat("\nSSC IQ comes from ssc_core_descriptive.csv (pre-harmonized by SSC team).\n")
cat("MIND IQ uses age-matched visit (closest iq_age to RDS age per kid).\n")

