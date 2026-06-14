# =========================================================================
#  Fisher r-to-z  —  Dumbbell & Δr bar charts by domain (SA / RRB / Totals)
# =========================================================================

# 0. packages -------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidytext)

# 1. output setup ----------------------------------------------------------
output_dir <- "SEM_output"
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

# 4. Fisher r-to-z --------------------------------------------------------
fz <- function(r) 0.5 * log((1 + r) / (1 - r))

fisher_test <- function(r1, n1, r2, n2) {
  z <- (fz(r1) - fz(r2)) / sqrt(1 / (n1 - 3) + 1 / (n2 - 3))
  p <- 2 * pnorm(-abs(z))
  tibble(diff = r1 - r2, p = p)
}

cohorts <- list(
  MIND_Preschool = c("MIND_Preschool_Boys", "MIND_Preschool_Girls"),
  MIND_Schoolage = c("MIND_Schoolage_Boys", "MIND_Schoolage_Girls"),
  SSC_Preschool  = c("SSC_Preschool_Boys",  "SSC_Preschool_Girls"),
  SSC_Schoolage  = c("SSC_Schoolage_Boys",  "SSC_Schoolage_Girls")
)

results <- imap_dfr(cohorts, \(fg, lab) {
  boys  <- dat[[fg[1]]]
  girls <- dat[[fg[2]]]
  vars  <- rownames(boys$corr)
  comb  <- t(combn(vars, 2))
  
  map_dfr(seq_len(nrow(comb)), \(i) {
    v1 <- comb[i, 1]; v2 <- comb[i, 2]
    r1 <- boys$corr[v1, v2];  r2 <- girls$corr[v1, v2]
    n1 <- boys$n[v1, v2];     n2 <- girls$n[v1, v2]
    if (any(is.na(c(r1, r2)))) return(NULL)
    fisher_test(r1, n1, r2, n2) |>
      mutate(cohort = lab,
             v1 = v1, v2 = v2,
             pair   = paste(v1, v2, sep = " \u2194 "),
             r_boys = r1, r_girls = r2)
  })
})

# 5. assign domain (within-domain pairs only) -----------------------------
assign_domain <- function(v1, v2, map = domain_map) {
  d1 <- map$domain[match(v1, map$variable)]
  d2 <- map$domain[match(v2, map$variable)]
  if_else(!is.na(d1) & !is.na(d2) & d1 == d2, d1, NA_character_)
}

results <- results |>
  mutate(domain = assign_domain(v1, v2))

# 5b. multiplicity correction --------------------------------------------
# HEADLINE is Benjamini-Hochberg FDR (replaces Bonferroni). Bonferroni kept
# beside it for reference only — do not report it as the primary call.
#
# SURFACED, NOT DECIDED — correction scope/family (author + Einat to confirm):
#   "within_subsample" : correct across all pairs within a cohort  [DEFAULT]
#   "within_domain"    : correct across pairs within a cohort x domain
#   "whole_table"      : one family across every pair (old Bonferroni scope)
# Inherits run_pipeline.R's RESAMPLE_FDR_SCOPE if set, else within_subsample.
FISHER_CORRECTION_SCOPE <- if (exists("RESAMPLE_FDR_SCOPE")) RESAMPLE_FDR_SCOPE else "within_subsample"

fisher_grp <- switch(FISHER_CORRECTION_SCOPE,
  within_subsample = vars(cohort),
  within_domain    = vars(cohort, domain),
  whole_table      = vars())

results <- results |>
  group_by(!!!fisher_grp) |>
  mutate(p_fdr  = p.adjust(p, "BH"),          # HEADLINE
         p_bonf = p.adjust(p, "bonferroni"),  # reference only
         sig    = p_fdr < .05) |>
  ungroup()

res_sa     <- results |> filter(domain == "SA")
res_rrb    <- results |> filter(domain == "RRB")
res_totals <- results |> filter(domain == "Totals")

# 6. plot helpers ----------------------------------------------------------
theme_clean <- theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank())

make_dumbbell <- function(df, title_suffix) {
  df |>
    ggplot(aes(y = reorder_within(pair, diff, cohort))) +
    geom_segment(aes(x    = r_girls,
                     xend = r_boys,
                     yend = reorder_within(pair, diff, cohort)),
                 colour = "grey70", linewidth = 0.4) +
    geom_point(aes(x = r_girls, colour = "Girls"), size = 2.5) +
    geom_point(aes(x = r_boys,  colour = "Boys"),  size = 2.5) +
    scale_colour_manual(name = NULL,
                        values = c("Girls" = "#E75480", "Boys" = "#2166AC")) +
    facet_wrap(~ cohort, scales = "free_y") +
    scale_y_reordered() +
    labs(title    = paste0("Dumbbell \u2014 ", title_suffix),
         x = "Correlation", y = NULL) +
    theme_clean
}

make_bars <- function(df, title_suffix) {
  df |>
    ggplot(aes(x    = reorder_within(pair, diff, cohort),
               y    = diff,
               fill = diff > 0)) +
    geom_col(show.legend = TRUE) +
    facet_wrap(~ cohort, scales = "free_y") +
    scale_x_reordered() +
    coord_flip() +
    scale_fill_manual(name   = NULL,
                      values = c("TRUE" = "#2166AC", "FALSE" = "#E75480"),
                      labels = c("TRUE" = "Boys > Girls", "FALSE" = "Girls > Boys")) +
    labs(title = paste0("\u0394r (Boys \u2212 Girls) \u2014 ", title_suffix),
         x = NULL, y = "\u0394r  (Boys \u2212 Girls)") +
    theme_clean
}

# 7. save helper -----------------------------------------------------------
save_plot <- function(p, name) {
  path <- file.path(plots_dir, paste0("fisher_", name, ".png"))
  ggsave(path, p, width = 14, height = 8, dpi = 300)
  cat("  >> Saved:", path, "\n")
}

# 8. generate & save -------------------------------------------------------
cat("Fisher r-to-z plots\n")
cat(strrep("=", 50), "\n\n")

save_plot(make_dumbbell(res_sa,     "SA (Social Affect)"),           "dumbbell_SA")
save_plot(make_dumbbell(res_rrb,    "RRB (Restricted & Repetitive)"),"dumbbell_RRB")
save_plot(make_dumbbell(res_totals, "Totals"),                       "dumbbell_TOTAL")

save_plot(make_bars(res_sa,     "SA (Social Affect)"),           "deltaR_SA")
save_plot(make_bars(res_rrb,    "RRB (Restricted & Repetitive)"),"deltaR_RRB")
save_plot(make_bars(res_totals, "Totals"),                       "deltaR_TOTAL")

# 9. save text results -----------------------------------------------------
sink(file.path(output_dir, "Fisher_rz_results.txt"))
cat("Fisher r-to-z — All Within-Domain Pair Comparisons\n")
cat("Headline correction: Benjamini-Hochberg FDR (p_fdr); Bonferroni kept for reference.\n")
cat("Correction scope:", FISHER_CORRECTION_SCOPE, "\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(strrep("=", 70), "\n\n")

for (dom in c("SA", "RRB", "Totals")) {
  cat(strrep("-", 50), "\n")
  cat("Domain:", dom, "\n")
  cat(strrep("-", 50), "\n")
  sub <- results |> filter(domain == dom) |>
    select(cohort, pair, r_boys, r_girls, diff, p, p_fdr, p_bonf, sig) |>
    arrange(cohort, desc(abs(diff)))
  print(as.data.frame(sub), row.names = FALSE, right = FALSE)
  cat("\n")
}
sink()

cat("\n✓ All Fisher r-to-z output saved to:", normalizePath(output_dir, mustWork = FALSE), "\n")