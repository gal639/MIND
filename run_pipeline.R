# =============================================================================
# run_pipeline.R — Smart orchestrator for the sex-differences-in-tool-agreement
# analysis pipeline (Paper 1 with Einat Waizbard).
#
# DESIGN PRINCIPLE: don't re-run what's already on disk.
# Each stage checks for its output files first. If they exist and are newer
# than their inputs, the stage is skipped with a "[CACHED]" message.
# Force re-run by setting FORCE_<STAGE> <- TRUE.
#
# Usage:
#   1. Set BASE_DIR below.
#   2. Toggle FORCE_* flags to override caching for a specific stage.
#   3. source("run_pipeline.R")
# =============================================================================

# ---- CONFIG ----------------------------------------------------------------

BASE_DIR <- "C:/Users/galor/OneDrive/Documents/Desktop/MIND"

# Force re-run flags. Default FALSE = use cached outputs when present.
FORCE_STAGE2_DATAPREP <- FALSE   # SEM_*.rds in SEM_output/data/
FORCE_STAGE3_IQ_MERGE <- FALSE   # SEM_*.rds in SEM_output/data_with_iq/
FORCE_STAGE4_SEM      <- FALSE   # SEM_output/SEM_results.txt
FORCE_STAGE4_FISHER   <- T   # SEM_output/Fisher_rz_results.txt
FORCE_STAGE4_MODER    <- FALSE   # sem_HLM_100426/section9_v2_output/iq_<policy>_*
RUN_STAGE4_FISHER_PARTIAL <- TRUE

# Moderation script — which IQ run to execute.
#   "all"            → mixes deviation + ratio IQ (keeps MIND_preschool)
#   "deviation_only" → Wechsler-family only (kills MIND_preschool)
#   "ratio_only"     → ratio IQ only (sensitivity)
IQ_RATIO_POLICY <- "all"

# ---- Stage 5: sex-balanced resampling (Emilio's recommendation) ------------
# Holds all girls fixed, draws a matched boy subsample, runs all three lenses,
# summarises the replicate distribution by its MEDIAN. Headline correction is
# Benjamini-Hochberg FDR; the effect-size empirical CI is the primary call.
# Saved BESIDE the existing per-lens outputs (does not replace them).
RUN_STAGE5_RESAMPLE   <- TRUE
FORCE_STAGE5_RESAMPLE <- FALSE
RESAMPLE_N_REPS       <- 1000L  # final run. Use a small value (e.g. 20) to smoke-test.
RESAMPLE_SEED         <- 1L

# SURFACED, NOT DECIDED — author + Einat must confirm:
#   IQ metric for the regression. MIND Preschool ratio-vs-deviation IQ is
#   unresolved (374/375 MSEL ratio). No transform is hardcoded; this just names
#   the column fed to M3/M4. The IQ ratio-policy row filter reuses IQ_RATIO_POLICY.
RESAMPLE_IQ_VAR       <- "FSIQ"
#   Correction scope/family for BH-FDR over the resampled terms:
#     "within_subsample" (current canonical scope) | "within_domain" | "whole_table"
RESAMPLE_FDR_SCOPE    <- "within_subsample"

# Stage 1 (source data filtering) is never auto-run. It produces transient
# in-memory data frames and the SEM-export snippets must be appended manually.
# If you need to redo Stage 1, do it interactively in RStudio.

# ---- HELPERS ---------------------------------------------------------------

setwd(BASE_DIR)

banner <- function(msg, char = "=") {
  cat("\n", strrep(char, 78), "\n", sep = "")
  cat(msg, "\n", sep = "")
  cat(strrep(char, 78), "\n\n", sep = "")
}

# Outputs are "fresh" if they all exist AND each is newer than every input.
outputs_fresh <- function(outputs, inputs = character(0)) {
  if (!all(file.exists(outputs))) return(FALSE)
  if (length(inputs) == 0) return(TRUE)
  if (!all(file.exists(inputs))) return(TRUE)  # can't compare, assume fresh
  min_out <- min(file.info(outputs)$mtime, na.rm = TRUE)
  max_in  <- max(file.info(inputs)$mtime, na.rm = TRUE)
  min_out > max_in
}

run_or_skip <- function(label, script_path, outputs, inputs = character(0), force = FALSE) {
  banner(paste0("STAGE: ", label))
  cat("Script:  ", script_path, "\n", sep = "")
  cat("Outputs: ", paste(basename(outputs), collapse = ", "), "\n", sep = "")
  
  if (!force && outputs_fresh(outputs, inputs)) {
    cat("\n[CACHED] All outputs present and newer than inputs. Skipping.\n")
    cat("         (Set FORCE_<stage> <- TRUE to override.)\n")
    return(invisible("cached"))
  }
  
  if (!file.exists(script_path)) {
    cat("\n[ERROR] Script not found: ", script_path, "\n", sep = "")
    return(invisible("missing"))
  }
  
  cat("\n[RUNNING]\n\n")
  t0 <- Sys.time()
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(BASE_DIR)  # ensure relative paths resolve from project root
  # Source into global env (FALSE), not a new env. The latter triggered
  # an infinite recursion when sourced scripts called library() / dplyr verbs.
  source(script_path, echo = FALSE, local = FALSE)
  cat("\n>> done in ", round(as.numeric(Sys.time() - t0, units = "secs"), 1),
      " sec\n", sep = "")
  invisible("ran")
}

# ---- PATHS -----------------------------------------------------------------

SEM_DATA_DIR    <- file.path(BASE_DIR, "SEM_output", "data")
SEM_IQDATA_DIR  <- file.path(BASE_DIR, "SEM_output", "data_with_iq")
SEM_OUT_DIR     <- file.path(BASE_DIR, "SEM_output")
HLM_DIR         <- file.path(BASE_DIR, "sem_HLM_100426")
MOD_OUT_DIR     <- file.path(HLM_DIR, "section9_v2_output")

rds_basenames <- c("SEM_MIND_preschool.rds", "SEM_MIND_schoolage.rds",
                   "SEM_SSC_preschool.rds",  "SEM_SSC_schoolage.rds")

# ---- STAGE 2 — Sanity check on data/ RDS files -----------------------------
# These are produced by appending sem_export_*() to the Stage 1 filtering
# scripts. We don't re-run those automatically; just verify the 4 files exist.

stage2_outputs <- file.path(SEM_DATA_DIR, rds_basenames)
banner("STAGE 2 — SEM data prep (sanity check)", char = "-")
if (FORCE_STAGE2_DATAPREP || !all(file.exists(stage2_outputs))) {
  missing <- stage2_outputs[!file.exists(stage2_outputs)]
  if (length(missing) > 0) {
    stop("Stage 2 RDS files missing — re-run Stage 1 with export snippets:\n  ",
         paste(basename(missing), collapse = "\n  "),
         "\n\nThis must be done interactively (see Stage 1 filtering scripts).")
  }
} else {
  cat("[CACHED] All 4 SEM_*.rds present in SEM_output/data/\n")
}

# ---- STAGE 3 — IQ integration ----------------------------------------------

run_or_skip(
  label       = "STAGE 3 — IQ integration",
  script_path = file.path(BASE_DIR, "IQ_TO_HLMSEM_020526", "p2_merge_iq_into_rds.R"),
  outputs     = file.path(SEM_IQDATA_DIR, rds_basenames),
  inputs      = stage2_outputs,
  force       = FORCE_STAGE3_IQ_MERGE
)

# ---- STAGE 4a — SEM covariance comparison ----------------------------------

run_or_skip(
  label       = "STAGE 4a — SEM covariance comparison",
  script_path = file.path(SEM_OUT_DIR, "SEM_cov_comparison.R"),
  outputs     = file.path(SEM_OUT_DIR, "SEM_results.txt"),
  inputs      = file.path(SEM_IQDATA_DIR, rds_basenames),
  force       = FORCE_STAGE4_SEM
)

# ---- STAGE 4b — Fisher r-to-z ----------------------------------------------
# Script: correlation_matrices/sex_plots_cohorts_einat_040326.R
# Reads 8 sex-split RDS files from BASE_DIR (NOT the IQ-augmented data).
# Writes Fisher_rz_results.txt to SEM_output/.
# at the top with the other flags

# later, alongside RUN_STAGE4_FISHER:
if (RUN_STAGE4_FISHER_PARTIAL) {
  run_script("4b' Fisher r-to-z (partial)",
             file.path("correlation_matrices",
                       "sex_plots_cohorts_einat_040326_partial.R"))
}
fisher_inputs <- file.path(BASE_DIR, c(
  "MIND_Preschool_Boys.rds",  "MIND_Preschool_Girls.rds",
  "MIND_Schoolage_Boys.rds",  "MIND_Schoolage_Girls.rds",
  "SSC_Preschool_Boys.rds",   "SSC_Preschool_Girls.rds",
  "SSC_Schoolage_Boys.rds",   "SSC_Schoolage_Girls.rds"
))

run_or_skip(
  label       = "STAGE 4b — Fisher r-to-z",
  script_path = file.path(BASE_DIR, "correlation_matrices",
                          "sex_plots_cohorts_einat_040326.R"),
  outputs     = file.path(SEM_OUT_DIR, "Fisher_rz_results.txt"),
  inputs      = fisher_inputs,
  force       = FORCE_STAGE4_FISHER
)

# ---- STAGE 4c — Pair-level moderation (timestamped outputs) ----------------
# The moderation script writes to section9_v2_output/iq_<policy>_<timestamp>/.
# Caching: scan for newest folder matching iq_<policy>_*; skip if fresh.

find_latest_moder_run <- function(policy) {
  if (!dir.exists(MOD_OUT_DIR)) return(NULL)
  pattern <- paste0("^iq_", policy, "_\\d{8}_\\d{4}$")
  runs <- list.dirs(MOD_OUT_DIR, recursive = FALSE)
  runs <- runs[grepl(pattern, basename(runs))]
  runs <- runs[file.exists(file.path(runs, "regression_pair_moderation_results.txt"))]
  if (length(runs) == 0) return(NULL)
  mtimes <- file.info(file.path(runs, "regression_pair_moderation_results.txt"))$mtime
  runs[which.max(mtimes)]
}

banner(paste0("STAGE 4c — Pair moderation [IQ_RATIO_POLICY = ", IQ_RATIO_POLICY, "]"))
latest_run <- find_latest_moder_run(IQ_RATIO_POLICY)
moder_inputs <- file.path(SEM_IQDATA_DIR, rds_basenames)

if (!FORCE_STAGE4_MODER && !is.null(latest_run)) {
  result_file <- file.path(latest_run, "regression_pair_moderation_results.txt")
  if (outputs_fresh(result_file, moder_inputs)) {
    cat("[CACHED] Latest run: ", basename(latest_run), "\n", sep = "")
    cat("         Results:    ", result_file, "\n", sep = "")
    cat("         (Set FORCE_STAGE4_MODER <- TRUE to re-run.)\n")
  } else {
    cat("Latest run for policy '", IQ_RATIO_POLICY, "' is older than IQ data.\n", sep = "")
    cat("Re-running...\n\n")
    assign("IQ_RATIO_POLICY", IQ_RATIO_POLICY, envir = .GlobalEnv)
    t0 <- Sys.time()
    old_wd <- getwd(); setwd(BASE_DIR)
    source(file.path(HLM_DIR, "section9_v2_pair_moderation.R"),
           echo = FALSE, local = FALSE)
    setwd(old_wd)
    cat("\n>> done in ", round(as.numeric(Sys.time() - t0, units = "secs"), 1),
        " sec\n", sep = "")
  }
} else {
  if (FORCE_STAGE4_MODER) {
    cat("[FORCED RE-RUN]\n\n")
  } else {
    cat("No prior run found for policy '", IQ_RATIO_POLICY, "'. Running.\n\n", sep = "")
  }
  assign("IQ_RATIO_POLICY", IQ_RATIO_POLICY, envir = .GlobalEnv)
  t0 <- Sys.time()
  old_wd <- getwd(); setwd(BASE_DIR)
  source(file.path(HLM_DIR, "section9_v2_pair_moderation.R"),
         echo = FALSE, local = FALSE)
  setwd(old_wd)
  cat("\n>> done in ", round(as.numeric(Sys.time() - t0, units = "secs"), 1),
      " sec\n", sep = "")
}

# ---- STAGE 5 — Sex-balanced resampling (BH-FDR + effect sizes) -------------
# Runs all three lenses (Fisher r-to-z, SEM covariance LRT, regression M2/M4)
# over sex-balanced resamples and writes a single tidy table with, per term:
#   estimate_median/_lo/_hi, es_median/_lo/_hi (the PRIMARY inferential object),
#   p_median, prop_sig, and the BH-FDR p_fdr/sig_fdr at the chosen scope.
# Output lands in SEM_output/, beside SEM_results.txt and Fisher_rz_results.txt.

resamp_out_csv <- file.path(SEM_OUT_DIR, "resampled_lens_results.csv")
resamp_out_rds <- file.path(SEM_OUT_DIR, "resampled_lens_results.rds")
resamp_inputs  <- file.path(SEM_IQDATA_DIR, rds_basenames)

if (RUN_STAGE5_RESAMPLE) {
  banner(paste0("STAGE 5 — Sex-balanced resampling [n_reps = ", RESAMPLE_N_REPS,
                ", FDR scope = ", RESAMPLE_FDR_SCOPE, "]"))
  if (!FORCE_STAGE5_RESAMPLE && outputs_fresh(resamp_out_csv, resamp_inputs)) {
    cat("[CACHED] ", basename(resamp_out_csv), " present and newer than inputs.\n", sep = "")
    cat("         (Set FORCE_STAGE5_RESAMPLE <- TRUE to re-run.)\n")
  } else {
    cat("[RUNNING] sourcing resampling engine + lens adapters...\n\n")
    t0 <- Sys.time()
    old_wd <- getwd(); setwd(BASE_DIR)
    source(file.path(BASE_DIR, "resample_balance.R"), local = FALSE)
    source(file.path(BASE_DIR, "resample_lenses.R"),  local = FALSE)
    resampled <- run_resampled_lenses(
      data_src  = SEM_IQDATA_DIR,
      n_reps    = RESAMPLE_N_REPS,
      seed      = RESAMPLE_SEED,
      iq_policy = IQ_RATIO_POLICY,   # reuse the same ratio-policy as moderation
      iq_var    = RESAMPLE_IQ_VAR,
      fdr_scope = RESAMPLE_FDR_SCOPE
    )
    write.csv(resampled, resamp_out_csv, row.names = FALSE)
    saveRDS(resampled, resamp_out_rds)
    setwd(old_wd)
    cat("\n  >> ", nrow(resampled), " terms written to ", basename(resamp_out_csv),
        " in ", round(as.numeric(Sys.time() - t0, units = "secs"), 1), " sec\n", sep = "")
  }
}

# ---- SUMMARY ---------------------------------------------------------------

banner("Pipeline complete.")
cat("Outputs:\n")
cat("  Stage 2 (SEM RDS):       ", SEM_DATA_DIR, "\n", sep = "")
cat("  Stage 3 (with IQ):       ", SEM_IQDATA_DIR, "\n", sep = "")
cat("  Stage 4a (SEM):          ", file.path(SEM_OUT_DIR, "SEM_results.txt"), "\n", sep = "")
cat("  Stage 4b (Fisher):       ", file.path(SEM_OUT_DIR, "Fisher_rz_results.txt"), "\n", sep = "")
latest <- find_latest_moder_run(IQ_RATIO_POLICY)
if (!is.null(latest)) {
  cat("  Stage 4c (Moderation):   ", latest, "\n", sep = "")
}
if (RUN_STAGE5_RESAMPLE) {
  cat("  Stage 5 (Resampled):     ", resamp_out_csv, "\n", sep = "")
}
cat("\nForce a re-run by setting FORCE_<STAGE> <- TRUE at top.\n")

