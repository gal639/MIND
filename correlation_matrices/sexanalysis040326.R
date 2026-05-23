# =========================================================================
#  Fisher r-to-z  —  Dumbbell & Top-N Δr visualisations
# =========================================================================

# 0. packages -------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidytext)

# 1. load data ------------------------------------------------------------
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

# 2. Fisher r-to-z comparison ---------------------------------------------
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
             pair   = paste(v1, v2, sep = " \u2194 "),
             r_boys = r1, r_girls = r2)
  })
}) |>
  mutate(p_bonf = p.adjust(p, "bonferroni"),
         sig    = p_bonf < .05)

# 3. plots ----------------------------------------------------------------

## shared theme tweaks
theme_clean <- theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank())

## A — Dumbbell: girls (orange) vs boys (blue) ----------------------------
plot_dumbbell <- results |>
  ggplot(aes(y = reorder_within(pair, diff, cohort))) +
  geom_segment(aes(x    = r_girls,
                   xend = r_boys,
                   yend = reorder_within(pair, diff, cohort)),
               colour = "grey70", linewidth = 0.4) +
  geom_point(aes(x = r_girls), colour = "#E69F00", size = 2) +
  geom_point(aes(x = r_boys),  colour = "#56B4E9", size = 2) +
  facet_wrap(~ cohort, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "Dumbbell: r Girls (orange) vs r Boys (blue)",
       x = "Correlation", y = NULL) +
  theme_clean

## B — Top-N absolute Δr per cohort ---------------------------------------
top_n_pairs <- 30

plot_top_n <- results |>
  group_by(cohort) |>
  slice_max(order_by = abs(diff), n = top_n_pairs) |>
  ungroup() |>
  ggplot(aes(x    = reorder_within(pair, diff, cohort),
             y    = diff,
             fill = diff > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ cohort, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#56B4E9", "FALSE" = "#E69F00")) +
  labs(title = paste0("Top ", top_n_pairs, " absolute \u0394r per cohort"),
       x = NULL, y = "\u0394r  (Boys \u2212 Girls)") +
  theme_clean

# 4. display --------------------------------------------------------------
print(plot_dumbbell)
print(plot_top_n)