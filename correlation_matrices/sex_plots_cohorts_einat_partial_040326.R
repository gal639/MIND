# =========================================================================
#  Fisher r-to-z on WITHIN-DOMAIN PARTIAL correlations
#  -----------------------------------------------------------------------
#  Sibling of Fisher_rz_comparison.R. For each domain (SA / RRB / Totals)
#  and each sex × subsample, correlations between variables are computed
#  controlling for the OTHER variables in the same domain. Then boys vs
#  girls partials are compared via Fisher r-to-z with df adjustment.
#
#  Method note:
#    Within-domain partials use the precision-matrix identity:
#      partial_r(X,Y | rest) = -K[X,Y] / sqrt(K[X,X] * K[Y,Y])
#    where K = inverse of the sub-correlation matrix restricted to the
#    domain variables. This avoids needing raw data — the existing
#    $corr matrices in the RDS files are sufficient.
#
#  df adjustment for Fisher z comparison:
#    z = (z1 - z2) / sqrt( 1/(n1 - 3 - k1) + 1/(n2 - 3 - k2) )
#    where k = (domain vars - 2)  i.e. the number of control variables.
# =========================================================================

# 0. packages -------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidytext)

# 1. output setup ---------------------------------------------------------
output_dir <- "~/Desktop/MIND/SEM_output"
plots_dir  <- file.path(output_dir, "plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# 2. load data ------------------------------------------------------------
files <- c(
  MIND_Preschool_Boys  = "MIND_Preschool_Boys.rds",
  MIND_Preschool_Girls = "MIND_Preschool_Girls.rds",
  MIND_Schoolage_Boys  = "MIND_Schoolage_Boys.rds",
  MIND_Schoolage_Girls = "MIND_Schoolage_Girls.rds",
  SSC_Preschool_Boys   = "SSC_Preschool_Boys.rds",
  SSC_Preschool_Girls  = "SSC_Preschool_Girls.rds",
  SSC_Schoolage_Boys   = "SSC_Schoolage_Boys.rds",
  SSC_Schoolage_Girls  = "SSC_Schoolage_Girls.rds"
)
dat <- map(files, readRDS)

# 3. domain mapping -------------------------------------------------------
domain_map <- tribble(
  ~variable,             ~domain,
  # SA
  "ADOS_SA",             "SA",
  "ADI_Social_Com",      "SA",
  "SRS_Social_Parent",   "SA",
  "SRS_Social_Teacher",  "SA",
  # RRB
  "ADOS_RRB",            "RRB",
  "ADI_RRB",             "RRB",
  "SRS_RRB_Parent",      "RRB",
  "SRS_RRB_Teacher",     "RRB",
  "RBSR_Total",          "RRB",
  # Totals
  "ADOS_Severity",       "Totals",
  "ADI_Total",           "Totals",
  "SRS_Total_Parent",    "Totals",
  "SRS_Total_Teacher",   "Totals"
)

domain_vars_lookup <- domain_map %>%
  group_by(domain) %>%
  summarise(vars = list(variable), .groups = "drop") %>%
  deframe()

# 4. partial-correlation helpers ------------------------------------------

# Compute within-domain partial correlation matrix from a full $corr.
# Returns NULL if subset is degenerate (<3 vars or singular).
within_domain_partial <- function(corr_mat, domain_vars) {
  vars <- intersect(domain_vars, rownames(corr_mat))
  if (length(vars) < 3) return(NULL)   # need >=3 vars for "partialing out" to mean anything

  R <- corr_mat[vars, vars, drop = FALSE]
  if (any(is.na(R))) R[is.na(R)] <- 0  # rare; defensive

  K <- tryCatch(solve(R), error = function(e) NULL)
  if (is.null(K)) return(NULL)

  p <- length(vars)
  P <- matrix(0, p, p, dimnames = list(vars, vars))
  for (i in seq_len(p)) for (j in seq_len(p)) {
    if (i != j) P[i, j] <- -K[i, j] / sqrt(K[i, i] * K[j, j])
  }
  diag(P) <- 1
  P
}

# Effective n for a domain in a given group.
# Uses the MINIMUM pairwise n across the domain's variable pairs — conservative
# and accounts for missing-data attenuation.
domain_n <- function(n_mat, domain_vars) {
  vars <- intersect(domain_vars, rownames(n_mat))
  if (length(vars) < 2) return(NA_integer_)
  sub <- n_mat[vars, vars, drop = FALSE]
  diag(sub) <- NA   # ignore diagonal (it's the self-n)
  min(sub, na.rm = TRUE)
}

# Fisher r-to-z and helpers
fz <- function(r) 0.5 * log((1 + r) / (1 - r))

fisher_partial <- function(r1, n1, k1, r2, n2, k2) {
  # df-adjusted variance for each transformed partial
  v1 <- 1 / (n1 - 3 - k1)
  v2 <- 1 / (n2 - 3 - k2)
  z  <- (fz(r1) - fz(r2)) / sqrt(v1 + v2)
  p  <- 2 * pnorm(-abs(z))
  tibble(diff = r1 - r2, p = p, z_stat = z)
}

# 5. compute partials per group × domain ----------------------------------

cohorts <- list(
  MIND_Preschool = c("MIND_Preschool_Boys", "MIND_Preschool_Girls"),
  MIND_Schoolage = c("MIND_Schoolage_Boys", "MIND_Schoolage_Girls"),
  SSC_Preschool  = c("SSC_Preschool_Boys",  "SSC_Preschool_Girls"),
  SSC_Schoolage  = c("SSC_Schoolage_Boys",  "SSC_Schoolage_Girls")
)

domains_to_test <- names(domain_vars_lookup)

results <- imap_dfr(cohorts, function(fg, cohort_lab) {
  boys  <- dat[[fg[1]]]
  girls <- dat[[fg[2]]]

  map_dfr(domains_to_test, function(dom) {
    dvars <- domain_vars_lookup[[dom]]

    P_b <- within_domain_partial(boys$corr,  dvars)
    P_g <- within_domain_partial(girls$corr, dvars)
    if (is.null(P_b) || is.null(P_g)) return(NULL)

    # Vars that exist in BOTH sexes' partial matrices for this domain
    vars_common <- intersect(rownames(P_b), rownames(P_g))
    if (length(vars_common) < 2) return(NULL)

    n_b <- domain_n(boys$n,  vars_common)
    n_g <- domain_n(girls$n, vars_common)
    k_b <- length(vars_common) - 2
    k_g <- length(vars_common) - 2

    # Sanity: enough df left over after partialing?
    if (n_b - 3 - k_b < 1 || n_g - 3 - k_g < 1) return(NULL)

    comb <- t(combn(vars_common, 2))
    map_dfr(seq_len(nrow(comb)), function(i) {
      v1 <- comb[i, 1]; v2 <- comb[i, 2]
      r1 <- P_b[v1, v2]; r2 <- P_g[v1, v2]
      if (any(is.na(c(r1, r2)))) return(NULL)

      fisher_partial(r1, n_b, k_b, r2, n_g, k_g) %>%
        mutate(
          cohort  = cohort_lab,
          domain  = dom,
          v1 = v1, v2 = v2,
          pair    = paste(v1, v2, sep = " ↔ "),
          r_boys  = r1, r_girls = r2,
          n_boys  = n_b, n_girls = n_g,
          k_boys  = k_b, k_girls = k_g
        )
    })
  })
}) %>%
  mutate(
    p_bonf = p.adjust(p, "bonferroni"),
    p_fdr  = p.adjust(p, "fdr"),
    sig    = p_bonf < .05
  )

# helper for clean ordering inside facets
order_within <- function(x, by) factor(x, levels = x[order(by)])

# 6. split by domain ------------------------------------------------------
res_sa     <- results %>% filter(domain == "SA")
res_rrb    <- results %>% filter(domain == "RRB")
res_totals <- results %>% filter(domain == "Totals")

# 7. plot functions -------------------------------------------------------

make_dumbbell <- function(df, title_str) {
  if (nrow(df) == 0) return(NULL)
  df_long <- df %>%
    pivot_longer(c(r_boys, r_girls), names_to = "sex", values_to = "r") %>%
    mutate(sex = recode(sex, r_boys = "Boys", r_girls = "Girls"))

  ggplot(df_long,
         aes(x = reorder_within(pair, diff, cohort),
             y = r, color = sex)) +
    geom_line(aes(group = pair), color = "grey70", linewidth = 0.6) +
    geom_point(size = 3) +
    facet_wrap(~ cohort, scales = "free_y") +
    scale_x_reordered() +
    scale_color_manual(values = c("Boys" = "#2166AC", "Girls" = "#E75480")) +
    coord_flip() +
    labs(title    = paste0("Partial r — ", title_str),
         subtitle = "Each pair controlling for other variables in the same domain",
         x = NULL, y = "Partial r", color = NULL) +
    theme_bw()
}

make_bars <- function(df, title_str) {
  if (nrow(df) == 0) return(NULL)
  ggplot(df,
         aes(x = reorder_within(pair, diff, cohort),
             y = diff, fill = diff > 0)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ cohort, scales = "free_y") +
    scale_x_reordered() +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#D55E00")) +
    labs(title    = paste0("Δ partial r (Boys − Girls) — ", title_str),
         subtitle = "Each pair controlling for other variables in the same domain",
         x = NULL, y = "Δ partial r") +
    theme_bw()
}

save_plot <- function(p, name) {
  if (is.null(p)) {
    cat("  -- Skipped (no rows):", name, "\n"); return(invisible(NULL))
  }
  path <- file.path(plots_dir, paste0("fisher_partial_", name, ".png"))
  ggsave(path, p, width = 14, height = 8, dpi = 300)
  cat("  >> Saved:", path, "\n")
}

# 8. generate & save ------------------------------------------------------
cat("Fisher r-to-z on within-domain PARTIAL correlations\n")
cat(strrep("=", 60), "\n\n")

save_plot(make_dumbbell(res_sa,     "SA (Social Affect)"),            "dumbbell_SA")
save_plot(make_dumbbell(res_rrb,    "RRB (Restricted & Repetitive)"), "dumbbell_RRB")
save_plot(make_dumbbell(res_totals, "Totals"),                        "dumbbell_TOTAL")

save_plot(make_bars(res_sa,     "SA (Social Affect)"),                "deltaR_SA")
save_plot(make_bars(res_rrb,    "RRB (Restricted & Repetitive)"),     "deltaR_RRB")
save_plot(make_bars(res_totals, "Totals"),                            "deltaR_TOTAL")

# 9. text results ---------------------------------------------------------
sink(file.path(output_dir, "Fisher_rz_partial_results.txt"))
cat("Fisher r-to-z — WITHIN-DOMAIN PARTIAL CORRELATIONS\n")
cat("Each correlation controls for the other variables in the same domain.\n")
cat("Method: precision-matrix partials from the sub-correlation matrix.\n")
cat("df adjusted as n - 3 - k, where k = (domain vars - 2).\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(strrep("=", 70), "\n\n")

for (dom in c("SA", "RRB", "Totals")) {
  cat(strrep("-", 50), "\n")
  cat("Domain:", dom, "\n")
  cat(strrep("-", 50), "\n")
  sub <- results %>%
    filter(domain == dom) %>%
    select(cohort, pair, r_boys, r_girls, diff,
           z_stat, p, p_bonf, p_fdr, sig,
           n_boys, n_girls, k_boys) %>%
    arrange(cohort, desc(abs(diff)))
  if (nrow(sub) == 0) {
    cat("  (no pairs computed for this domain)\n\n")
  } else {
    print(as.data.frame(sub), row.names = FALSE, right = FALSE)
    cat("\n")
  }
}
sink()

cat("\n✓ Partial-correlation Fisher output saved to:",
    normalizePath(output_dir, mustWork = FALSE), "\n")
cat("  Text:  Fisher_rz_partial_results.txt\n")
cat("  Plots: plots/fisher_partial_*.png\n")
