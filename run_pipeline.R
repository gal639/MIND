# =============================================================================
# run_pipeline.R — one-shot, PORTABLE orchestrator for the sex-differences-in-
# tool-agreement analysis (Paper 1 with Einat Waizbard).
#
# Runs start-to-finish on ANY machine:
#   0. bootstraps missing R packages (installs and WAITS; will not run without them)
#   1. self-locates BASE_DIR (the data+scripts root); override with env MIND_BASE_DIR
#
# ORDER honours "resample BEFORE any test":
#   data prep -> RESAMPLING -> sex-means -> canonical full-data tests
#   (SEM cov, Fisher, moderation) -> figures (raw + sex-balanced) + bundle.
# The canonical tests report the full sample; the resampling layer + the resampled
# figures report the sex-balanced version. Both are produced every run.
#
# Caching: each stage skips when its outputs are present and newer than inputs;
# set the matching FORCE_* <- TRUE to override.
# =============================================================================

# ---- 0. PACKAGE BOOTSTRAP (install-and-WAIT; do NOT run without these) -------
local({
  r <- getOption("repos")
  if (is.null(r) || is.na(r["CRAN"]) || !nzchar(r["CRAN"]) || r["CRAN"] == "@CRAN@")
    options(repos = c(CRAN = "https://cloud.r-project.org"))
})
REQUIRED_PKGS <- c(
  "dplyr","purrr","tibble","tidyr","ggplot2","withr","lavaan",   # core + resampling
  "pbivnorm","numDeriv","quadprog",                              # lavaan estimator deps
  "pacman","tidytext",                                            # Fisher script
  "ragg","semPlot","writexl","magick","gridExtra","grid")        # figures / SEM diagram / Excel / bundle
.have <- function(p) requireNamespace(p, quietly = TRUE)
.missing <- REQUIRED_PKGS[!vapply(REQUIRED_PKGS, .have, logical(1))]
if (length(.missing)) {
  message("[setup] Installing required packages (first-run setup, please wait): ",
          paste(.missing, collapse = ", "))
  install.packages(.missing)                     # BLOCKS until the install finishes
  .still <- .missing[!vapply(.missing, .have, logical(1))]
  if (length(.still))
    stop("[setup] Required packages failed to install: ", paste(.still, collapse = ", "),
         "\n         Install them manually, then re-run. The pipeline does NOT run without them.",
         call. = FALSE)
  message("[setup] All required packages present.")
}

# ---- 1. PORTABLE BASE_DIR ----------------------------------------------------
# Override with env MIND_BASE_DIR; else self-locate the script's own folder; else
# fall back to the known OneDrive root. A valid BASE_DIR holds SEM_output/data_with_iq.
.find_self <- function() {
  ca <- commandArgs(FALSE)
  f  <- sub("^--file=", "", grep("^--file=", ca, value = TRUE))
  if (length(f)) return(normalizePath(f[1], winslash = "/", mustWork = FALSE))
  for (i in rev(seq_len(sys.nframe()))) {
    of <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(of)) return(normalizePath(of, winslash = "/", mustWork = FALSE))
  }
  NA_character_
}
BASE_DIR <- Sys.getenv("MIND_BASE_DIR", unset = NA_character_)
if (is.na(BASE_DIR) || !dir.exists(BASE_DIR)) {
  self <- .find_self()
  cand <- c(if (!is.na(self)) dirname(self),
            "C:/Users/galor/OneDrive/Documents/Desktop/MIND",
            getwd())
  cand <- unique(cand[!is.na(cand) & dir.exists(cand)])
  ok   <- cand[vapply(cand, function(d) dir.exists(file.path(d, "SEM_output", "data_with_iq")), logical(1))]
  BASE_DIR <- if (length(ok)) ok[1] else if (length(cand)) cand[1] else getwd()
}
if (!dir.exists(file.path(BASE_DIR, "SEM_output")))
  stop("[setup] BASE_DIR does not look like the MIND data root: ", BASE_DIR,
       "\n         Set MIND_BASE_DIR to the folder that holds SEM_output/.", call. = FALSE)
setwd(BASE_DIR)
message("[setup] BASE_DIR = ", BASE_DIR)

# ---- CONFIG ----------------------------------------------------------------
FORCE_DATAPREP <- FALSE   # SEM_*.rds in SEM_output/data/
FORCE_IQMERGE  <- FALSE   # SEM_*.rds in SEM_output/data_with_iq/
FORCE_SEM      <- FALSE   # SEM_output/SEM_results.txt  (loads semPlot + writexl)
FORCE_FISHER   <- TRUE    # Fisher FDR on the within-domain family; cheap, re-run
RUN_FISHER_PARTIAL <- TRUE
FORCE_MODER    <- FALSE   # moderation run (loads writexl); timestamped cache

# IQ ratio policy for the moderation + resampling data filter:
#   "all" | "deviation_only" | "ratio_only"
IQ_RATIO_POLICY <- "all"

# Resampling (Emilio): hold girls fixed, draw matched boys, run all lenses on the
# SAME balanced frame, summarise by MEDIAN. Runs BEFORE the canonical tests.
RUN_RESAMPLE       <- TRUE
FORCE_RESAMPLE     <- TRUE
RESAMPLE_N_REPS    <- 20L    # SMOKE-TEST value. Set 1000L for the reportable run (multi-hour).
RESAMPLE_SEED      <- 1L
RESAMPLE_IQ_VAR    <- "FSIQ"                 # IQ column fed to the regression lens (surfaced)
RESAMPLE_FDR_SCOPE <- "within_subsample"     # "within_subsample" | "within_domain" | "whole_table"

RUN_MEANS   <- TRUE       # between-sex mean profiles (raw + resampled)
FORCE_MEANS <- FALSE

RUN_FIGURES <- TRUE       # fig_*.R (raw + sex-balanced resample) + make_figures_bundle.R

# ---- HELPERS ---------------------------------------------------------------
banner <- function(msg, char = "=") {
  cat("\n", strrep(char, 78), "\n", sep = ""); cat(msg, "\n", sep = "")
  cat(strrep(char, 78), "\n\n", sep = "")
}
outputs_fresh <- function(outputs, inputs = character(0)) {
  if (!all(file.exists(outputs))) return(FALSE)
  if (length(inputs) == 0) return(TRUE)
  if (!all(file.exists(inputs))) return(TRUE)
  min(file.info(outputs)$mtime, na.rm = TRUE) > max(file.info(inputs)$mtime, na.rm = TRUE)
}
run_or_skip <- function(label, script_path, outputs, inputs = character(0), force = FALSE) {
  banner(paste0("STAGE: ", label))
  cat("Script:  ", script_path, "\n", sep = "")
  cat("Outputs: ", paste(basename(outputs), collapse = ", "), "\n", sep = "")
  if (!force && outputs_fresh(outputs, inputs)) {
    cat("\n[CACHED] All outputs present and newer than inputs. Skipping.\n")
    cat("         (Set the matching FORCE_* <- TRUE to override.)\n"); return(invisible("cached"))
  }
  if (!file.exists(script_path)) { cat("\n[ERROR] Script not found: ", script_path, "\n", sep = ""); return(invisible("missing")) }
  cat("\n[RUNNING]\n\n"); t0 <- Sys.time()
  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE); setwd(BASE_DIR)
  source(script_path, echo = FALSE, local = FALSE)
  cat("\n>> done in ", round(as.numeric(Sys.time() - t0, units = "secs"), 1), " sec\n", sep = ""); invisible("ran")
}
source_in_basedir <- function(path) {           # source a script with WD = BASE_DIR
  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE); setwd(BASE_DIR)
  source(path, echo = FALSE, local = FALSE)
}

# ---- PATHS -----------------------------------------------------------------
SEM_DATA_DIR   <- file.path(BASE_DIR, "SEM_output", "data")
SEM_IQDATA_DIR <- file.path(BASE_DIR, "SEM_output", "data_with_iq")
SEM_OUT_DIR    <- file.path(BASE_DIR, "SEM_output")
HLM_DIR        <- file.path(BASE_DIR, "sem_HLM_100426")
MOD_OUT_DIR    <- file.path(HLM_DIR, "section9_v2_output")
rds_basenames  <- c("SEM_MIND_preschool.rds", "SEM_MIND_schoolage.rds",
                    "SEM_SSC_preschool.rds",  "SEM_SSC_schoolage.rds")
iq_rds <- file.path(SEM_IQDATA_DIR, rds_basenames)

# =============================================================================
# STAGE 2 — SEM data prep sanity check
# =============================================================================
stage2_outputs <- file.path(SEM_DATA_DIR, rds_basenames)
banner("STAGE 2 — SEM data prep (sanity check)", char = "-")
if (FORCE_DATAPREP || !all(file.exists(stage2_outputs))) {
  missing <- stage2_outputs[!file.exists(stage2_outputs)]
  if (length(missing) > 0)
    stop("Stage 2 RDS files missing — re-run Stage 1 filtering with export snippets:\n  ",
         paste(basename(missing), collapse = "\n  "), "\n(interactive, see the filtering scripts).")
} else cat("[CACHED] All 4 SEM_*.rds present in SEM_output/data/\n")

# =============================================================================
# STAGE 3 — IQ integration
# =============================================================================
run_or_skip("STAGE 3 — IQ integration",
  file.path(BASE_DIR, "IQ_TO_HLMSEM_020526", "p2_merge_iq_into_rds.R"),
  outputs = iq_rds, inputs = stage2_outputs, force = FORCE_IQMERGE)

# =============================================================================
# STAGE 4 — SEX-BALANCED RESAMPLING  (runs BEFORE the canonical tests)
# All three lenses (Fisher / SEM-LRT / regression) on ONE balanced frame per
# replicate; median summary + empirical effect-size CI + scope-aware BH-FDR.
# =============================================================================
resamp_out_csv <- file.path(SEM_OUT_DIR, "resampled_lens_results.csv")
resamp_out_rds <- file.path(SEM_OUT_DIR, "resampled_lens_results.rds")
if (RUN_RESAMPLE) {
  banner(paste0("STAGE 4 — Sex-balanced resampling [n_reps = ", RESAMPLE_N_REPS,
                ", FDR scope = ", RESAMPLE_FDR_SCOPE, "]"))
  if (!FORCE_RESAMPLE && outputs_fresh(resamp_out_csv, iq_rds)) {
    cat("[CACHED] ", basename(resamp_out_csv), " present and newer than inputs.\n", sep = "")
  } else {
    cat("[RUNNING] sourcing resampling engine + lens adapters...\n\n"); t0 <- Sys.time()
    old_wd <- getwd(); setwd(BASE_DIR)
    source(file.path(BASE_DIR, "resample_balance.R"), local = FALSE)
    source(file.path(BASE_DIR, "resample_lenses.R"),  local = FALSE)
    resampled <- run_resampled_lenses(
      data_src = SEM_IQDATA_DIR, n_reps = RESAMPLE_N_REPS, seed = RESAMPLE_SEED,
      iq_policy = IQ_RATIO_POLICY, iq_var = RESAMPLE_IQ_VAR, fdr_scope = RESAMPLE_FDR_SCOPE)
    write.csv(resampled, resamp_out_csv, row.names = FALSE)
    saveRDS(resampled, resamp_out_rds)
    setwd(old_wd)
    cat("\n  >> ", nrow(resampled), " terms written to ", basename(resamp_out_csv),
        " in ", round(as.numeric(Sys.time() - t0, units = "secs"), 1), " sec\n", sep = "")
  }
}

# =============================================================================
# STAGE 5 — Between-sex MEAN-level profiles (raw + resampled)
# =============================================================================
means_out_csv <- file.path(SEM_OUT_DIR, "sex_means_resampled.csv")
if (RUN_MEANS) {
  banner(paste0("STAGE 5 — Between-sex mean profiles [n_reps = ", RESAMPLE_N_REPS, "]"))
  if (!FORCE_MEANS && outputs_fresh(means_out_csv, iq_rds)) {
    cat("[CACHED] ", basename(means_out_csv), " present and newer than inputs.\n", sep = "")
  } else {
    cat("[RUNNING] sourcing sex-means analysis...\n\n"); t0 <- Sys.time()
    old_wd <- getwd(); setwd(BASE_DIR)
    source(file.path(BASE_DIR, "resample_balance.R"), local = FALSE)
    source(file.path(BASE_DIR, "resample_lenses.R"),  local = FALSE)
    source(file.path(BASE_DIR, "sex_means_profile.R"), local = FALSE)
    run_sex_means(data_src = SEM_IQDATA_DIR, n_reps = RESAMPLE_N_REPS, seed = RESAMPLE_SEED,
                  iq_policy = IQ_RATIO_POLICY, fdr_scope = RESAMPLE_FDR_SCOPE, out_dir = SEM_OUT_DIR)
    setwd(old_wd)
    cat("\n  >> done in ", round(as.numeric(Sys.time() - t0, units = "secs"), 1), " sec\n", sep = "")
  }
}

# =============================================================================
# STAGE 6 — Canonical full-data tests (covariance -> Fisher -> moderation)
# =============================================================================
# 6a — SEM covariance comparison (loads semPlot + writexl)
run_or_skip("STAGE 6a — SEM covariance comparison",
  file.path(SEM_OUT_DIR, "SEM_cov_comparison.R"),
  outputs = file.path(SEM_OUT_DIR, "SEM_results.txt"), inputs = iq_rds, force = FORCE_SEM)

# 6b' — Fisher r-to-z on PARTIAL correlations (non-fatal)
if (RUN_FISHER_PARTIAL) {
  banner("STAGE 6b' — Fisher r-to-z (partial correlations)")
  partial_script <- file.path(BASE_DIR, "correlation_matrices", "sex_plots_cohorts_einat_partial_040326.R")
  if (file.exists(partial_script))
    tryCatch(source_in_basedir(partial_script),
             error = function(e) cat("[WARN] partial Fisher failed (non-fatal): ", conditionMessage(e), "\n", sep = ""))
  else cat("[SKIP] partial Fisher script not found.\n")
}
# 6b — Fisher r-to-z (BH-FDR headline)
fisher_inputs <- file.path(BASE_DIR, c(
  "MIND_Preschool_Boys.rds","MIND_Preschool_Girls.rds","MIND_Schoolage_Boys.rds","MIND_Schoolage_Girls.rds",
  "SSC_Preschool_Boys.rds","SSC_Preschool_Girls.rds","SSC_Schoolage_Boys.rds","SSC_Schoolage_Girls.rds"))
run_or_skip("STAGE 6b — Fisher r-to-z",
  file.path(BASE_DIR, "correlation_matrices", "sex_plots_cohorts_einat_040326.R"),
  outputs = file.path(SEM_OUT_DIR, "Fisher_rz_results.txt"), inputs = fisher_inputs, force = FORCE_FISHER)

# 6c — Pair-level moderation (timestamped outputs; cache = newest iq_<policy>_* run)
find_latest_moder_run <- function(policy) {
  if (!dir.exists(MOD_OUT_DIR)) return(NULL)
  runs <- list.dirs(MOD_OUT_DIR, recursive = FALSE)
  runs <- runs[grepl(paste0("^iq_", policy, "_\\d{8}_\\d{4}$"), basename(runs))]
  runs <- runs[file.exists(file.path(runs, "regression_pair_moderation_results.txt"))]
  if (!length(runs)) return(NULL)
  runs[which.max(file.info(file.path(runs, "regression_pair_moderation_results.txt"))$mtime)]
}
banner(paste0("STAGE 6c — Pair moderation [IQ_RATIO_POLICY = ", IQ_RATIO_POLICY, "]"))
latest_run <- find_latest_moder_run(IQ_RATIO_POLICY)
run_moder <- FORCE_MODER || is.null(latest_run) ||
  !outputs_fresh(file.path(latest_run, "regression_pair_moderation_results.txt"), iq_rds)
if (!run_moder) {
  cat("[CACHED] Latest run: ", basename(latest_run), "\n", sep = "")
  cat("         (Set FORCE_MODER <- TRUE to re-run.)\n")
} else {
  cat(if (FORCE_MODER) "[FORCED RE-RUN]\n\n" else "[RUNNING] no fresh run for this policy.\n\n")
  assign("IQ_RATIO_POLICY", IQ_RATIO_POLICY, envir = .GlobalEnv); t0 <- Sys.time()
  source_in_basedir(file.path(HLM_DIR, "section9_v2_pair_moderation.R"))
  cat("\n>> done in ", round(as.numeric(Sys.time() - t0, units = "secs"), 1), " sec\n", sep = "")
}

# =============================================================================
# STAGE 7 — Figures (raw + sex-balanced resample) + bundle
# =============================================================================
if (RUN_FIGURES) {
  banner("STAGE 7 — Figures (raw + resampled) + bundle")
  fig_scripts <- c("make_profile_by_informant.R", "fig_gap_density.R",
                   "fig_effectsize_forest.R", "fig_reconciliation.R", "fig_profile_cohorts.R",
                   "fig_boxplots.R", "fig_recon_plane.R", "fig_effectsize_age.R",
                   "fig_bland_altman.R")
  for (fs in fig_scripts) {
    fp <- file.path(BASE_DIR, fs)
    if (!file.exists(fp)) { cat("[SKIP] missing ", fs, "\n", sep = ""); next }
    cat("  -> ", fs, "\n", sep = ""); t0 <- Sys.time()
    tryCatch(source_in_basedir(fp),
             error = function(e) cat("[WARN] ", fs, " failed: ", conditionMessage(e), "\n", sep = ""))
    cat("     (", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s)\n", sep = "")
  }
  cat("  -> make_figures_bundle.R\n")
  tryCatch(source_in_basedir(file.path(BASE_DIR, "make_figures_bundle.R")),
           error = function(e) cat("[WARN] bundle failed: ", conditionMessage(e), "\n", sep = ""))
}

# ---- SUMMARY ---------------------------------------------------------------
banner("Pipeline complete.")
cat("BASE_DIR: ", BASE_DIR, "\n\n", sep = "")
cat("Outputs (execution order):\n")
cat("  2/3  data + IQ:        ", SEM_IQDATA_DIR, "\n", sep = "")
if (RUN_RESAMPLE) cat("  4    resampled lenses: ", resamp_out_csv, "\n", sep = "")
if (RUN_MEANS)    cat("  5    sex means:        ", means_out_csv, "\n", sep = "")
cat("  6a   SEM:              ", file.path(SEM_OUT_DIR, "SEM_results.txt"), "\n", sep = "")
cat("  6b   Fisher:           ", file.path(SEM_OUT_DIR, "Fisher_rz_results.txt"), "\n", sep = "")
latest <- find_latest_moder_run(IQ_RATIO_POLICY)
if (!is.null(latest)) cat("  6c   moderation:       ", latest, "\n", sep = "")
if (RUN_FIGURES)  cat("  7    figures + bundle: ", file.path(BASE_DIR, "Figures_for_Einat.pdf"), "\n", sep = "")
cat("\nn_reps = ", RESAMPLE_N_REPS, " (set 1000L for the reportable run). Force any stage with FORCE_* <- TRUE.\n", sep = "")
