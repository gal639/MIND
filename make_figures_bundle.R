# Iteration 7 (robust): assemble Figures_for_Einat.pdf from the rendered PNGs
# (each visually verified) using magick. Run the fig_*.R / make_profile_*.R
# scripts first to (re)generate the PNGs; this just collates them + a contents page.
setwd("C:/Users/galor/OneDrive/Documents/Desktop/MIND")
suppressPackageStartupMessages({ library(magick); library(grid) })

pages <- tibble::tribble(
  ~png, ~title,
  "Profile_by_Informant_1_profile.png",     "1. Symptom profile by informant & sex (+ variance)",
  "Profile_by_Informant_2_descriptives.png","1b. Descriptives table (n, mean, SD, SD ratio)",
  "Fig6_Masking_gap.png",                   "2. The masking gap (informant − clinician, per child)",
  "Fig4_EffectSize_forest.png",             "3. Sex mean difference (Cohen's d) + 95% CI by informant",
  "Fig3_Reconciliation.png",                "4. Two kinds of agreement: LEVEL vs RANK",
  "Fig1_Profile_MIND_vs_SSC.png",           "5. Profile by cohort (MIND vs SSC)",
  "Fig2_Distributions_by_informant.png",    "6. Score distributions (violin + box)",
  "Fig5_Reconciliation_plane.png",          "7. Reconciliation plane (LEVEL d vs RANK q)")
pages <- pages[file.exists(pages$png), ]

# ---- contents page (rendered to PNG at ~ landscape) -------------------------
ragg::agg_png("._contents.png", width = 1650, height = 1200, res = 150, background = "white")
grid.newpage()
grid.text("Figures — Sex differences in autism tool agreement",
          x = 0.04, y = 0.93, hjust = 0, gp = gpar(fontface = "bold", fontsize = 20))
grid.text("Prepared for Dr. Einat Waizbard  ·  MIND (APP) + SSC  ·  2026-07-01",
          x = 0.04, y = 0.885, hjust = 0, gp = gpar(fontsize = 12, col = "grey35"))
capt <- c(
 "Grouped by instrument (ADOS→ADI→SRS→RBSR). Girls-higher on SRS, esp. teacher; more variable only on teacher-SRS.",
 "Raw n, mean, SD per group; SD ratio flags girls more variable on teacher-SRS (1.30 / 1.18).",
 "Per-child informant−clinician gap; girls' curve shifts right, largest for teacher-SRS (gap d up to +0.60) = masking.",
 "Cohen's d with 95% CI by informant, All/MIND/SSC. ADOS ~0 (MIND girls-lower); SRS girls-higher and replicates.",
 "Girls show a bigger LEVEL gap (A) yet a small higher RANK agreement (B). The level gap is the robust axis.",
 "Cohort replication: SRS-Parent girls-higher in both; SRS-Teacher (SSC only) largest; MIND ADOS girls-lower.",
 "Distributions: girls' teacher-SRS shifted up and more spread; ADOS overlaps.",
 "LEVEL (Cohen's d) vs RANK (Cohen's q): mostly horizontal spread — two different quantities.")
y <- 0.80
for (i in seq_len(nrow(pages))) {
  grid.text(pages$title[i], x = 0.05, y = y, hjust = 0, gp = gpar(fontface = "bold", fontsize = 11.5))
  grid.text(capt[i], x = 0.075, y = y - 0.033, hjust = 0, gp = gpar(fontsize = 9.5, col = "grey30"))
  y <- y - 0.088
}
grid.text("Effect sizes throughout (Cohen's d/q); uncorrected alpha primary, BH-FDR beside. z-standardized within slice. Preview n_reps=20.",
          x = 0.04, y = 0.04, hjust = 0, gp = gpar(fontsize = 9, col = "grey40", fontface = "italic"))
dev.off()

# ---- collate to one PDF ------------------------------------------------------
all_png <- c("._contents.png", pages$png)
imgs <- magick::image_read(all_png)
magick::image_write(imgs, "Figures_for_Einat.pdf", format = "pdf", density = 150)
unlink("._contents.png")
cat("WROTE Figures_for_Einat.pdf —", length(all_png), "pages\n")
