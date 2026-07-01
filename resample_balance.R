# =============================================================================
# resample_balance.R
# Sex-balanced resampling engine for the MIND/SSC tool-agreement pipeline.
#
# Implements Emilio's recommendation: the M-F imbalance over-weights boys in
# every two-group contrast (correlation difference, covariance/LRT, regression
# moderation). Fix = hold ALL girls fixed, draw a random subsample of boys
# matched to the girls' n, run the contrast, repeat, and summarise the
# replicate distribution by its MEDIAN (robust if that distribution is not
# normal -- this is why median, not mean).
#
# Designed as a generic engine + thin per-lens adapters so it slots around the
# existing run_pipeline.R internals. Each lens still does its own boy-vs-girl
# contrast internally; the engine only hands it a balanced data frame.
#
# CONTRACT for an analysis_fn (a "lens"):
#   input : one balanced data frame (all girls + a matched boy subsample)
#   output: a tibble, one row per parameter, with columns
#             term      <chr>  identifier for the parameter / pair / cell
#             estimate  <dbl>  the contrast (e.g. delta_r, delta_chisq, beta_4)
#           and OPTIONALLY
#             es        <dbl>  an effect size free of N (Cohen's q, f^2, ...)
#             p_value   <dbl>  per-replicate p for that parameter
#   The engine never inspects what the lens does -- only the returned columns.
#
# Requires: dplyr (>= 1.1), purrr (>= 1.0), tibble, tidyr, withr, stats.
#           lavaan only inside the SEM adapter (your code).
# =============================================================================

library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(withr)


# -----------------------------------------------------------------------------
# Core engine
# -----------------------------------------------------------------------------
#' Run a lens over sex-balanced resamples
#'
#' @param data        Data frame for ONE subsample (must contain both sexes).
#' @param analysis_fn A lens function obeying the contract above.
#' @param sex_col     Name of the sex column.
#' @param female_level Value in `sex_col` marking the (smaller) fixed group.
#' @param n_reps      Number of resamples. NOTE on the default: Emilio said
#'   "~50". The median stabilises after a few dozen draws, but the empirical
#'   2.5/97.5 CI we return below is the 1st-2nd order statistic at n_reps = 50
#'   and is far too noisy to report. 1000 makes the CI stable; the cost is a
#'   1000x re-fit, so dial down for quick checks and back up for the final run.
#' @param seed        Base seed. Replicate b uses seed + b, so the whole run is
#'   reproducible AND two lenses called with the same seed + same data reuse the
#'   identical boy draws -- letting you align the three lenses on one resampled
#'   dataset if you ever want that.
#' @param verbose     Print a one-line progress/failure summary.
#'
#' @return list(reps = <long tibble of replicate-level rows, +.rep>,
#'              n_f, n_m, n_reps, n_failed, balanced)
resample_balanced <- function(data,
                              analysis_fn,
                              sex_col       = "sex",
                              female_level  = "F",
                              n_reps        = 1000L,
                              seed          = 1L,
                              verbose       = TRUE) {

  stopifnot(is.data.frame(data), is.function(analysis_fn),
            sex_col %in% names(data))

  is_f    <- data[[sex_col]] == female_level
  females <- data[is_f, , drop = FALSE]
  males   <- data[!is_f & !is.na(data[[sex_col]]), , drop = FALSE]
  n_f     <- nrow(females)
  n_m     <- nrow(males)

  # --- No imbalance to correct: run once, return as a single degenerate rep ---
  if (n_m <= n_f) {
    if (verbose) {
      message(sprintf("balanced already (n_F=%d >= n_M=%d): single fit, no resampling.",
                      n_f, n_m))
    }
    res <- tryCatch(analysis_fn(data), error = function(e) NULL)
    reps <- if (is.null(res)) tibble() else mutate(as_tibble(res), .rep = 1L)
    return(list(reps = reps, n_f = n_f, n_m = n_m,
                n_reps = 1L, n_failed = as.integer(is.null(res)),
                balanced = FALSE))
  }

  # --- Imbalance: girls fixed, resample n_f boys each replicate ----------------
  one_rep <- function(b) {
    idx <- with_seed(seed + b, sample.int(n_m, size = n_f, replace = FALSE))
    balanced <- bind_rows(females, males[idx, , drop = FALSE])
    res <- tryCatch(analysis_fn(balanced), error = function(e) NULL)
    if (is.null(res)) return(NULL)             # e.g. SEM non-convergence on small n
    mutate(as_tibble(res), .rep = b)
  }

  reps_list <- map(seq_len(n_reps), one_rep)
  n_failed  <- sum(map_lgl(reps_list, is.null))
  reps      <- list_rbind(compact(reps_list))

  if (verbose) {
    message(sprintf("resampled %d boys to match %d girls | %d/%d reps ok (%d failed).",
                    n_f, n_f, n_reps - n_failed, n_reps, n_failed))
  }

  list(reps = reps, n_f = n_f, n_m = n_m,
       n_reps = n_reps, n_failed = n_failed, balanced = TRUE)
}


# -----------------------------------------------------------------------------
# Multi-lens engine: DRAW ONCE, then run every lens on the SAME resample
# -----------------------------------------------------------------------------
#' Run SEVERAL lenses over ONE set of sex-balanced resamples.
#'
#' Emilio's design, made explicit: resample BEFORE any test runs. Each replicate
#' draws ONE matched boy subsample, builds ONE balanced data frame (all girls +
#' those boys), and runs EVERY lens on that identical frame. So Fisher, the SEM
#' LRT and the regression all see the *same* resampled subjects each replicate —
#' rather than each lens drawing its own boys (which the old per-lens driver did,
#' aligned only implicitly by a shared seed).
#'
#' @param lenses Named list of analysis_fn; each returns term/estimate/es/p_value.
#' @return list(reps = <long tibble, incl. `lens` + `.rep`>, n_f, n_m, n_reps,
#'              n_failed, balanced)
resample_all_lenses <- function(data, lenses,
                                sex_col      = "sex",
                                female_level = "F",
                                n_reps       = 1000L,
                                seed         = 1L,
                                verbose      = TRUE) {

  stopifnot(is.data.frame(data), is.list(lenses), length(lenses) > 0,
            sex_col %in% names(data))

  is_f    <- data[[sex_col]] == female_level
  females <- data[is_f, , drop = FALSE]
  males   <- data[!is_f & !is.na(data[[sex_col]]), , drop = FALSE]
  n_f     <- nrow(females)
  n_m     <- nrow(males)

  # Apply every lens to ONE balanced data frame; tag each row with its lens.
  apply_lenses <- function(bal_df, b) {
    map(names(lenses), function(lname) {
      res <- tryCatch(lenses[[lname]](bal_df), error = function(e) NULL)
      if (is.null(res) || nrow(res) == 0L) return(NULL)
      res <- as_tibble(res)
      # Strip estimator-specific S3 classes (e.g. lavaan.vector) so the three
      # lenses' columns row-bind cleanly across the shared resample.
      for (cc in intersect(c("estimate", "es", "p_value"), names(res)))
        res[[cc]] <- as.numeric(res[[cc]])
      mutate(res, lens = lname, .rep = b)
    }) |> compact() |> list_rbind()
  }

  # No imbalance to correct: one fit per lens on the full data (single rep).
  if (n_m <= n_f) {
    if (verbose)
      message(sprintf("balanced already (n_F=%d >= n_M=%d): single fit per lens.",
                      n_f, n_m))
    reps <- apply_lenses(data, 1L)
    return(list(reps = reps, n_f = n_f, n_m = n_m,
                n_reps = 1L, n_failed = as.integer(nrow(reps) == 0L),
                balanced = FALSE))
  }

  # Imbalance: girls fixed; ONE matched boy draw per replicate feeds ALL lenses.
  reps_list <- map(seq_len(n_reps), function(b) {
    idx <- with_seed(seed + b, sample.int(n_m, size = n_f, replace = FALSE))
    bal <- bind_rows(females, males[idx, , drop = FALSE])
    apply_lenses(bal, b)
  })
  n_failed <- sum(map_lgl(reps_list, function(x) is.null(x) || nrow(x) == 0L))
  reps     <- list_rbind(compact(reps_list))

  if (verbose)
    message(sprintf("drew %d boys to match %d girls x %d reps | %d reps ok (%d empty) | lenses: %s",
                    n_f, n_f, n_reps, n_reps - n_failed, n_failed,
                    paste(names(lenses), collapse = ", ")))

  list(reps = reps, n_f = n_f, n_m = n_m,
       n_reps = n_reps, n_failed = n_failed, balanced = TRUE)
}


# -----------------------------------------------------------------------------
# Aggregation
# -----------------------------------------------------------------------------
#' Collapse the replicate distribution to a per-term summary
#'
#' Primary inferential object is the effect-size CI, NOT a pooled p-value:
#' the median of a set of p-values has no clean reference distribution, so we
#' treat "does the empirical CI of the effect exclude the null?" as the test,
#' and report median p + proportion-significant only as descriptive support.
#' This also lines up with Emilio's push to lead with effect sizes.
#'
#' @param reps      The `$reps` tibble from resample_balanced().
#' @param value_cols Numeric columns to summarise by median + empirical CI
#'   (those absent from `reps` are silently skipped, with a warning).
#' @param p_col     Name of the per-replicate p column, or NULL.
#' @param ci        Central mass for the empirical interval (default .95).
#' @param alpha     Significance threshold for prop_sig (default .05).
aggregate_resamples <- function(reps,
                                by         = "term",
                                value_cols = c("estimate", "es"),
                                p_col      = "p_value",
                                ci         = 0.95,
                                alpha      = 0.05) {

  if (nrow(reps) == 0L) {
    warning("no successful replicates to aggregate.")
    return(tibble(term = character()))
  }

  present <- intersect(value_cols, names(reps))
  missing <- setdiff(value_cols, names(reps))
  if (length(missing)) {
    warning("value_cols not found and skipped: ", paste(missing, collapse = ", "))
  }

  lo <- (1 - ci) / 2
  hi <- 1 - lo
  q  <- function(x, p) stats::quantile(x, p, na.rm = TRUE, names = FALSE)

  # median + empirical CI for each present value column, named <col>_{median,lo,hi}
  out <- reps |>
    summarise(
      n_ok = n(),
      across(
        all_of(present),
        list(median = ~ stats::median(.x, na.rm = TRUE),
             lo     = ~ q(.x, lo),
             hi     = ~ q(.x, hi)),
        .names = "{.col}_{.fn}"
      ),
      .by = all_of(by)
    )

  # descriptive p summaries, if a p column was supplied and exists
  if (!is.null(p_col) && p_col %in% names(reps)) {
    psum <- reps |>
      summarise(
        p_median = stats::median(.data[[p_col]], na.rm = TRUE),
        prop_sig = mean(.data[[p_col]] < alpha, na.rm = TRUE),
        .by = all_of(by)
      )
    out <- left_join(out, psum, by = by)
  }

  out
}


#' Benjamini-Hochberg FDR over the per-term summary
#'
#' Applied to p_median as a convenience so the Fisher/regression panels can keep
#' an FDR column. Treat it as supplementary: the median-p it adjusts is itself a
#' descriptive summary, so the effect-size CI remains the primary call.
add_fdr <- function(agg, p_col = "p_median", out_col = "p_fdr") {
  if (!p_col %in% names(agg)) {
    warning(p_col, " not present; skipping FDR.")
    return(agg)
  }
  agg[[out_col]]  <- stats::p.adjust(agg[[p_col]], method = "BH")
  agg[["sig_fdr"]] <- agg[[out_col]] < 0.05
  agg
}


# -----------------------------------------------------------------------------
# Driver: every lens x every subsample, returned as one tidy table
# -----------------------------------------------------------------------------
#' @param subsamples Named list of data frames (e.g. MIND_pre, MIND_school, ...).
#' @param lenses     Named list of analysis_fn (e.g. fisher, sem_lrt, regression).
run_balanced_pipeline <- function(subsamples,
                                  lenses,
                                  sex_col      = "sex",
                                  female_level = "F",
                                  n_reps       = 1000L,
                                  seed         = 1L,
                                  value_cols   = c("estimate", "es"),
                                  p_col        = "p_value") {

  grid <- expand_grid(subsample = names(subsamples), lens = names(lenses))

  pmap(grid, function(subsample, lens) {
    eng <- resample_balanced(
      data         = subsamples[[subsample]],
      analysis_fn  = lenses[[lens]],
      sex_col      = sex_col,
      female_level = female_level,
      n_reps       = n_reps,
      seed         = seed,
      verbose      = TRUE
    )
    aggregate_resamples(eng$reps, value_cols = value_cols, p_col = p_col) |>
      mutate(subsample = subsample, lens = lens,
             n_f = eng$n_f, n_m = eng$n_m,
             n_reps = eng$n_reps, n_failed = eng$n_failed,
             balanced = eng$balanced,
             .before = 1)
  }) |>
    list_rbind() |>
    add_fdr()  # BH within the whole table; swap to per-lens if you prefer
}


# =============================================================================
# LENS ADAPTERS  --  wire your existing run_pipeline.R internals into the TODOs.
# Each receives ONE balanced data frame and returns term / estimate / es / p_value.
# These are scaffolds: the contrast logic is yours, the return shape is the contract.
# =============================================================================

# --- Fisher r-to-z -----------------------------------------------------------
# Per replicate: girls' r is fixed, boys' r is computed on the matched subsample,
# then Fisher-z their difference. estimate = delta_r (girls - boys); es = Cohen's q.
fisher_lens <- function(d, sex_col = "sex", female_level = "F",
                        pairs = NULL) {
  # pairs: your within-domain pair spec, e.g. list(c("ADI_RRB","RBSR"), ...)
  # TODO: replace the body with your existing pair-correlation routine.
  fz <- function(r) atanh(pmin(pmax(r, -0.999), 0.999))   # Fisher z

  g <- d[d[[sex_col]] == female_level, , drop = FALSE]
  b <- d[d[[sex_col]] != female_level, , drop = FALSE]

  map(pairs, function(p) {
    rg <- stats::cor(g[[p[1]]], g[[p[2]]], use = "pairwise.complete.obs")
    rb <- stats::cor(b[[p[1]]], b[[p[2]]], use = "pairwise.complete.obs")
    ng <- sum(stats::complete.cases(g[[p[1]]], g[[p[2]]]))
    nb <- sum(stats::complete.cases(b[[p[1]]], b[[p[2]]]))
    z  <- (fz(rg) - fz(rb)) / sqrt(1 / (ng - 3) + 1 / (nb - 3))
    tibble(
      term     = paste(p, collapse = "~"),
      estimate = rg - rb,                       # delta_r, +ve = girls higher
      es       = fz(rg) - fz(rb),               # Cohen's q
      p_value  = 2 * stats::pnorm(-abs(z))
    )
  }) |> list_rbind()
}

# --- SEM covariance / likelihood-ratio test ----------------------------------
# Per replicate: fit a two-group (boys vs girls) model, compare free vs
# covariance-constrained via LRT. estimate = delta_chisq; es = delta_CFI (N-free).
# FIML still works on the balanced subset; tryCatch in the engine drops any rep
# that fails to converge on the smaller n.
sem_lrt_lens <- function(d, sex_col = "sex",
                         model = NULL) {
  # TODO: drop in your restored FIML lavaan model + the free/constrained fits.
  # Expected pattern (pseudocode kept inert so the file sources cleanly):
  #
  #   fit_free <- lavaan::lavaan(model, data = d, group = sex_col,
  #                              missing = "fiml")
  #   fit_con  <- lavaan::lavaan(model, data = d, group = sex_col,
  #                              missing = "fiml",
  #                              group.equal = "lv.covariances")  # or residual covariances
  #   lr  <- lavaan::lavTestLRT(fit_con, fit_free)
  #   cf  <- lavaan::fitMeasures(c(fit_free, fit_con), "cfi")
  #   tibble(term = "covariance_structure",
  #          estimate = lr[["Chisq diff"]][2],
  #          es       = unname(cf[1] - cf[2]),     # delta CFI, N-free
  #          p_value  = lr[["Pr(>Chisq)"]][2])
  stop("sem_lrt_lens: plug in your lavaan FIML free/constrained fits.")
}

# --- Hierarchical regression moderation (M1-M4) ------------------------------
# Per replicate: fit the nested models, extract the sex x tool interaction
# (beta_4). estimate = beta_4; es = f^2 (or delta R^2) for the interaction term.
regression_lens <- function(d, focus = "M4") {
  # TODO: drop in your M1-M4 formulas. Pseudocode kept inert:
  #
  #   m_no_int <- lm(ados ~ tool + sex + IQ + tool:IQ,        data = d)   # M3-ish
  #   m_full   <- lm(ados ~ tool + sex + IQ + tool:IQ + sex:tool, data = d) # M4
  #   b4   <- coef(m_full)[["toolX:sexF"]]                 # the sex x tool term
  #   r2f  <- summary(m_full)$r.squared
  #   r2r  <- summary(m_no_int)$r.squared
  #   f2   <- (r2f - r2r) / (1 - r2f)                       # Cohen's f^2 for the term
  #   p4   <- summary(m_full)$coefficients["toolX:sexF", "Pr(>|t|)"]
  #   tibble(term = "sex_x_tool", estimate = b4, es = f2, p_value = p4)
  stop("regression_lens: plug in your M1-M4 nested models.")
}


# -----------------------------------------------------------------------------
# Optional: diagnostic plot of one lens' replicate distribution
# (basic geoms only -- no theme-element customisation, so ggplot2 4.0-safe)
# -----------------------------------------------------------------------------
plot_resamples <- function(reps, value = "estimate", null_at = 0) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for plot_resamples().")
  }
  med <- reps |>
    summarise(m = stats::median(.data[[value]], na.rm = TRUE), .by = term)

  ggplot2::ggplot(reps, ggplot2::aes(x = .data[[value]],
                                     y = stats::reorder(term, .data[[value]],
                                                        FUN = stats::median))) +
    ggplot2::geom_vline(xintercept = null_at, linetype = "dashed") +
    ggplot2::geom_jitter(height = 0.15, alpha = 0.15, size = 0.6) +
    ggplot2::geom_point(data = med, ggplot2::aes(x = m, y = term),
                        inherit.aes = FALSE, size = 2) +
    ggplot2::labs(x = value, y = NULL,
                  title = "Resampled distribution per term (point = median)") +
    ggplot2::theme_minimal()
}


# -----------------------------------------------------------------------------
# Usage sketch
# -----------------------------------------------------------------------------
# subsamples <- list(MIND_pre = d_mind_pre, MIND_school = d_mind_school,
#                    SSC_pre  = d_ssc_pre,  SSC_school  = d_ssc_school)
#
# lenses <- list(
#   fisher     = function(d) fisher_lens(d, pairs = my_pairs),
#   sem_lrt    = function(d) sem_lrt_lens(d, model = my_model),
#   regression = regression_lens
# )
#
# results <- run_balanced_pipeline(subsamples, lenses,
#                                  sex_col = "sex", female_level = "F",
#                                  n_reps = 1000, seed = 1)
# # results: one row per term x lens x subsample, with
# #   estimate_median / _lo / _hi, es_median / _lo / _hi, p_median, prop_sig, p_fdr
