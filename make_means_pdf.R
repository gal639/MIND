# =============================================================================
# make_means_pdf.R  —  presentation PDF for the between-sex MEAN-profile analysis
# (Stage 6). Two views: (1) z-scored score profiles, boys vs girls, per slice;
# (2) Cohen's d heatmap (measure x slice) so the ADOS-vs-SRS gap is visible at a
# glance. Reads SEM_output/sex_means.rds (resampled d) + rebuilds the raw slices
# for the profile means. Writes Means_Profile_Report_2026-07-01.pdf at repo root.
# =============================================================================
setwd("C:/Users/galor/OneDrive/Documents/Desktop/MIND")
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(purrr); library(tibble); library(tidyr) })
source("resample_balance.R"); source("resample_lenses.R"); source("sex_means_profile.R")

sm  <- readRDS("SEM_output/sex_means.rds")
dat <- build_resample_data("SEM_output/data_with_iq", iq_policy = "all")
slices <- build_slices(dat)

# ---- (1) profile data (z-scored means by sex) -------------------------------
pdat <- profile_plot_data(slices)
pdat$slice <- factor(pdat$slice, levels = names(slices))

p_profiles <- ggplot(pdat, aes(measure, mean_z, color = sex, group = sex)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey75") +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  facet_wrap(~ slice, ncol = 2, scales = "free_x") +
  scale_color_manual(values = c(Boys = "#2166AC", Girls = "#E75480"), name = NULL) +
  labs(title = "Score profiles by sex  (z-scored within slice)",
       subtitle = "Mean standardized score per measure. Divergence between the lines = a sex gap; the SHAPE is the signal.",
       x = NULL, y = "Mean z-score") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "top", strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# ---- (2) Cohen's d heatmap (resampled median), measure x slice --------------
hd <- sm$resampled %>%
  transmute(slice, measure, domain = trimws(sub("\\|.*$", "", term)),
            d = es_median, sig_fdr = sig_fdr) %>%
  mutate(slice = factor(slice, levels = names(slices)),
         domain = factor(domain, levels = c("TOTAL", "SOCIAL", "RRB")),
         measure = factor(measure, levels = rev(intersect(MEAS_LEVELS, unique(measure)))),
         lab = ifelse(is.na(d), "", sprintf("%.2f%s", d, ifelse(sig_fdr, "*", ""))))

p_heat <- ggplot(hd, aes(slice, measure, fill = d)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = lab), size = 2.9) +
  facet_grid(domain ~ ., scales = "free_y", space = "free_y") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-1.3, 1.3), oob = scales::squish,
                       name = "Cohen's d\n(girls - boys)") +
  labs(title = "Sex mean differences  (Cohen's d, resampled median)",
       subtitle = "Red = girls score higher.  * = survives BH-FDR.  Note ADOS (clinician) ~ 0 vs SRS (informants) strongly girls-higher = the ADOS-SRS gap.",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        panel.grid = element_blank(), strip.text.y = element_text(face = "bold", angle = 0),
        plot.title = element_text(face = "bold"))

# ---- write outputs ----------------------------------------------------------
# PNGs FIRST — they need no font embedding to display, so they always work even
# if a viewer is misbehaving. Written via ragg (crisp, no base-font issues).
ggsave("Means_Profile_1_profiles.png", p_profiles, width = 11, height = 8.5, dpi = 150, device = ragg::agg_png)
ggsave("Means_Profile_2_cohensD.png",  p_heat,     width = 11, height = 8.5, dpi = 150, device = ragg::agg_png)
cat("WROTE Means_Profile_1_profiles.png + Means_Profile_2_cohensD.png\n")

# cairo_pdf embeds fonts (base pdf() only references them, which crashes some
# Windows viewers with "unknown software exception"). Wrapped so a locked/open
# output file can't halt the run — PNGs above are the guaranteed deliverable.
out <- "Means_Profile_Report.pdf"
ok <- tryCatch({
  cairo_pdf(out, width = 11, height = 8.5, onefile = TRUE)
  print(p_profiles); print(p_heat); dev.off(); TRUE
}, error = function(e) { try(dev.off(), silent = TRUE)
  message("PDF write failed (", conditionMessage(e),
          ") — likely the old PDF is open/locked. Close it and rerun; PNGs are ready."); FALSE })
if (ok) cat("WROTE", out, "\n")
