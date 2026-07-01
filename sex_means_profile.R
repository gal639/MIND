# =============================================================================
# sex_means_profile.R  —  Stage 6: between-subjects MEAN-LEVEL sex analysis
#
# Einat's request (2026-06-30 meeting): complement the within-child AGREEMENT
# lenses (Fisher / SEM / regression) with a between-GROUP mean comparison.
# For each measure, compare the boys' vs girls' group mean (t-test + Cohen's d),
# and plot the z-scored "clinical profile" (mean of each measure, one line per
# sex). Her theory: girls' measures correlate more WITHIN child, yet the girls
# GROUP is more heterogeneous, so the mean profile can show a different (larger)
# gap between tools than boys — reconciling our sample with the literature.
#
# Spec (confirmed with Gal):
#   - Test unit  : per-measure sex mean difference (girls - boys), Cohen's d + p.
#                  Uncorrected alpha is primary (Einat); BH-FDR kept beside it.
#   - Data       : BOTH — sex-balanced resample (primary) AND raw full sample.
#   - Levels     : 3 — (1) all combined, (2) MIND / SSC, (3) four age subsamples.
#   - Measures   : the within-domain tools (TOTAL / SOCIAL / RRB); teacher SRS
#                  only where available (SSC).
#   - Direction  : estimate = Cohen's d (girls - boys); +ve = girls score higher.
#
# Outputs (SEM_output/):
#   sex_means_raw.csv         raw-sample t-tests per slice x measure (+ BH-FDR)
#   sex_means_resampled.csv   resampled median d + empirical CI + p (+ scope FDR)
#   sex_means_profiles.pdf    z-scored profile plots (boys vs girls) per slice
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tibble); library(tidyr); library(ggplot2)
})

# Engine + data loader (draw-once resampling, aggregation, scope FDR, loader).
if (!exists("resample_all_lenses")) source("resample_balance.R")
if (!exists("build_resample_data")) source("resample_lenses.R")

# ---- CONFIG (inherit run_pipeline.R globals if present) ---------------------
.SM_N_REPS   <- if (exists("RESAMPLE_N_REPS"))   RESAMPLE_N_REPS   else 20L
.SM_SEED     <- if (exists("RESAMPLE_SEED"))     RESAMPLE_SEED     else 1L
.SM_IQPOL    <- if (exists("IQ_RATIO_POLICY"))   IQ_RATIO_POLICY   else "all"
.SM_FDRSCOPE <- if (exists("RESAMPLE_FDR_SCOPE"))RESAMPLE_FDR_SCOPE else "within_subsample"
.SM_DATASRC  <- "SEM_output/data_with_iq"

# Within-domain measures (mirror of the Fisher/regression tool set).
MEAS <- tibble::tribble(
  ~domain,  ~measure,
  "TOTAL",  "ADOS_Severity",  "TOTAL",  "ADI_Total",     "TOTAL",  "SRS_Total_Parent",  "TOTAL",  "SRS_Total_Teacher",
  "SOCIAL", "ADOS_SA",        "SOCIAL", "ADI_Social_Com","SOCIAL", "SRS_Social_Parent", "SOCIAL", "SRS_Social_Teacher",
  "RRB",    "ADOS_RRB",       "RRB",    "ADI_RRB",       "RRB",    "SRS_RRB_Parent",    "RRB",    "RBSR_Total",  "RRB", "SRS_RRB_Teacher"
)
MEAS_LEVELS <- MEAS$measure  # display order (domain-grouped, ADOS first)

# The three analysis levels = seven slices (mirror of the regression subsamples).
build_slices <- function(dat) list(
  "1. Full"           = dat,
  "2a. MIND"          = dat %>% filter(sample_source == "MIND"),
  "2b. SSC"           = dat %>% filter(sample_source == "SSC"),
  "3a. MIND Preschool"= dat %>% filter(sample_source == "MIND", age_group == "Preschool"),
  "3b. MIND School"   = dat %>% filter(sample_source == "MIND", age_group == "School"),
  "3c. SSC Preschool" = dat %>% filter(sample_source == "SSC",  age_group == "Preschool"),
  "3d. SSC School"    = dat %>% filter(sample_source == "SSC",  age_group == "School")
)

# --- Cohen's d (pooled), girls - boys, + Welch t-test p ----------------------
.cohen_d <- function(g, b) {
  ng <- length(g); nb <- length(b)
  sp <- sqrt(((ng - 1) * var(g) + (nb - 1) * var(b)) / (ng + nb - 2))
  if (is.na(sp) || sp == 0) return(NA_real_)
  (mean(g) - mean(b)) / sp
}

# =============================================================================
# 1. MEANS LENS  (contract-conforming: term / estimate / es / p_value)
#    estimate = es = Cohen's d (girls - boys); one row per usable measure.
# =============================================================================
means_lens <- function(d, sex_col = "sex", female_level = "Female",
                       measures = MEAS, min_n = 10L) {
  d <- as.data.frame(d)
  d <- d[d[[sex_col]] %in% c("Male", "Female"), , drop = FALSE]
  rows <- list()
  for (k in seq_len(nrow(measures))) {
    m <- measures$measure[k]; dom <- measures$domain[k]
    if (!(m %in% names(d))) next
    x  <- suppressWarnings(as.numeric(d[[m]])); sx <- d[[sex_col]]
    g  <- x[sx == female_level & !is.na(x)]
    b  <- x[sx != female_level & !is.na(x)]
    if (length(g) < min_n || length(b) < min_n) next
    dd <- .cohen_d(g, b)
    if (is.na(dd)) next
    tt <- tryCatch(stats::t.test(g, b), error = function(e) NULL)   # Welch
    if (is.null(tt)) next
    rows[[length(rows) + 1L]] <- tibble(
      term     = sprintf("%s | %s | mean_sex", dom, m),
      estimate = dd, es = dd, p_value = unname(tt$p.value))
  }
  if (!length(rows)) return(tibble(term = character(), estimate = numeric(),
                                   es = numeric(), p_value = numeric()))
  list_rbind(rows)
}

# =============================================================================
# 2. RAW-SAMPLE t-tests (sensitivity): one pass, no resampling, all 7 slices.
# =============================================================================
raw_means_table <- function(slices, measures = MEAS, min_n = 10L) {
  out <- list()
  for (sl in names(slices)) {
    d <- as.data.frame(slices[[sl]]); d <- d[d$sex %in% c("Male", "Female"), , drop = FALSE]
    for (k in seq_len(nrow(measures))) {
      m <- measures$measure[k]; dom <- measures$domain[k]
      if (!(m %in% names(d))) next
      x <- suppressWarnings(as.numeric(d[[m]]))
      g <- x[d$sex == "Female" & !is.na(x)]; b <- x[d$sex == "Male" & !is.na(x)]
      if (length(g) < min_n || length(b) < min_n) next
      tt <- tryCatch(stats::t.test(g, b), error = function(e) NULL); if (is.null(tt)) next
      out[[length(out) + 1L]] <- tibble(
        slice = sl, level = substr(sl, 1, 1), domain = dom, measure = m,
        n_boys = length(b), n_girls = length(g),
        mean_boys = mean(b), mean_girls = mean(g), sd_boys = sd(b), sd_girls = sd(g),
        cohen_d = .cohen_d(g, b), t = unname(tt$statistic), p = unname(tt$p.value))
    }
  }
  res <- list_rbind(out)
  if (nrow(res) == 0) return(res)
  # BH-FDR within each slice (uncorrected p stays primary, per Einat).
  res %>% group_by(slice) %>%
    mutate(p_fdr = p.adjust(p, "BH"),
           sig = p < .05, sig_fdr = p_fdr < .05) %>% ungroup()
}

# =============================================================================
# 3. RESAMPLED means (primary): draw-once per slice, means_lens only.
# =============================================================================
resampled_means_table <- function(slices, n_reps, seed, fdr_scope) {
  lenses <- list(means = function(d) means_lens(d))
  res <- imap(slices, function(sl_df, sl) {
    eng <- resample_all_lenses(data = sl_df, lenses = lenses,
                               sex_col = "sex", female_level = "Female",
                               n_reps = n_reps, seed = seed, verbose = TRUE)
    if (nrow(eng$reps) == 0L) return(NULL)
    aggregate_resamples(eng$reps, by = c("lens", "term"),
                        value_cols = c("estimate", "es"), p_col = "p_value") %>%
      mutate(slice = sl, level = substr(sl, 1, 1),
             n_f = eng$n_f, n_m = eng$n_m, n_reps = eng$n_reps,
             balanced = eng$balanced, .before = 1)
  }) %>% compact() %>% list_rbind()
  if (nrow(res) == 0) return(res)
  # scope-aware BH-FDR needs a `subsample` column; alias slice -> subsample.
  res$subsample <- res$slice
  res <- apply_fdr_by_scope(res, scope = fdr_scope)
  res$measure <- trimws(sub("^[^|]*\\|\\s*([^|]+)\\|.*$", "\\1", res$term))
  res
}

# =============================================================================
# 4. z-scored PROFILE PLOTS (boys vs girls), per slice, faceted.
# =============================================================================
profile_plot_data <- function(slices, measures = MEAS, min_n = 10L) {
  out <- list()
  for (sl in names(slices)) {
    d <- as.data.frame(slices[[sl]]); d <- d[d$sex %in% c("Male", "Female"), , drop = FALSE]
    for (m in measures$measure) {
      if (!(m %in% names(d))) next
      x <- suppressWarnings(as.numeric(d[[m]]))
      if (sum(!is.na(x)) < min_n * 2) next
      z <- as.numeric(scale(x))                 # z within slice, pooled across sexes
      for (sxl in c("Male", "Female")) {
        zz <- z[d$sex == sxl & !is.na(z)]
        if (length(zz) < min_n) next
        out[[length(out) + 1L]] <- tibble(slice = sl, measure = m,
          sex = ifelse(sxl == "Female", "Girls", "Boys"), mean_z = mean(zz), n = length(zz))
      }
    }
  }
  df <- list_rbind(out)
  if (nrow(df) == 0) return(df)
  df$measure <- factor(df$measure, levels = intersect(MEAS_LEVELS, unique(df$measure)))
  df$slice   <- factor(df$slice, levels = names(slices))
  df
}

save_profiles <- function(pdat, path) {
  if (nrow(pdat) == 0) { message("no profile data"); return(invisible()) }
  p <- ggplot(pdat, aes(x = measure, y = mean_z, color = sex, group = sex)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
    geom_line(linewidth = 0.9) + geom_point(size = 2) +
    facet_wrap(~ slice, ncol = 2, scales = "free_x") +
    scale_color_manual(values = c(Boys = "#2166AC", Girls = "#E75480"), name = NULL) +
    labs(title = "Score profiles by sex (z-scored within slice)",
         subtitle = "Mean standardized score per measure; the shape/gap between tools is the signal",
         x = NULL, y = "Mean z-score") +
    theme_bw(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          legend.position = "top", strip.text = element_text(face = "bold"))
  # cairo_pdf embeds fonts so Windows PDF viewers don't crash on referenced fonts.
  dev <- if (isTRUE(capabilities("cairo"))) cairo_pdf else NULL
  ggsave(path, p, width = 12, height = 12, device = dev)
  message("  >> saved ", path)
}

# =============================================================================
# 5. RUN
# =============================================================================
run_sex_means <- function(data_src = .SM_DATASRC, n_reps = .SM_N_REPS,
                          seed = .SM_SEED, iq_policy = .SM_IQPOL,
                          fdr_scope = .SM_FDRSCOPE, out_dir = "SEM_output") {
  dat    <- build_resample_data(data_src, iq_policy = iq_policy)
  slices <- build_slices(dat)

  message("Stage 6: raw-sample means ...")
  raw <- raw_means_table(slices)
  message("Stage 6: resampled means (n_reps=", n_reps, ") ...")
  rsm <- resampled_means_table(slices, n_reps = n_reps, seed = seed, fdr_scope = fdr_scope)
  message("Stage 6: profile plots ...")
  save_profiles(profile_plot_data(slices), file.path(out_dir, "sex_means_profiles.pdf"))

  write.csv(raw, file.path(out_dir, "sex_means_raw.csv"), row.names = FALSE)
  write.csv(rsm, file.path(out_dir, "sex_means_resampled.csv"), row.names = FALSE)
  saveRDS(list(raw = raw, resampled = rsm), file.path(out_dir, "sex_means.rds"))
  message(sprintf("  >> raw: %d rows | resampled: %d rows", nrow(raw), nrow(rsm)))
  invisible(list(raw = raw, resampled = rsm))
}

# Auto-run when sourced by the pipeline (guarded so interactive source is quiet
# unless asked). run_pipeline.R sets SM_AUTORUN <- TRUE before sourcing.
if (isTRUE(get0("SM_AUTORUN", ifnotfound = FALSE))) run_sex_means()
