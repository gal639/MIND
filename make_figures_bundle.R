# Iteration 7 (robust): assemble Figures_for_Einat.pdf from the rendered PNGs
# (each visually verified) using magick. Run the fig_*.R / make_profile_*.R
# scripts first to (re)generate the PNGs; this just collates them + a contents page.
if (!exists("BASE_DIR")) BASE_DIR <- Sys.getenv("MIND_BASE_DIR", unset = "C:/Users/galor/OneDrive/Documents/Desktop/MIND")
setwd(BASE_DIR)
suppressPackageStartupMessages({ library(magick); library(grid) })

# Caption lives IN the tribble (3rd column) so it can never desync from the page
# list after the file.exists() filter below. Bland-Altman figures (raw + the
# sex-balanced resample) are included per the "figures on resampled + actual" ask.
pages <- tibble::tribble(
  ~png, ~title, ~caption,
  "Profile_by_Informant_1_profile.png",     "Symptom profile by informant & sex (+ variance)",   "Grouped by instrument (ADOS→ADI→SRS→RBSR). Girls-higher on SRS, esp. teacher.",
  "Profile_by_Informant_2_descriptives.png","Descriptives table (n, mean, SD, SD ratio)",        "SD ratio flags girls more variable on teacher-SRS (1.30 / 1.18).",
  "Fig8_BlandAltman_by_sex.png",            "Bland-Altman by sex — ADOS vs SRS (raw)",           "Per-child level gap; girls' bias off 0, boys near 0; largest for teacher.",
  "Fig8_BlandAltman_by_sex_resampled.png",  "Bland-Altman by sex — ADOS vs SRS (sex-balanced)",  "Same, on the sex-balanced resample: girls fixed, boys balanced; median of the draws.",
  "Fig6_Masking_gap.png",                   "The masking gap (informant − clinician, per child)","Girls' curve shifts right, largest for teacher-SRS (gap d up to +0.60) = masking.",
  "Fig8_BlandAltman_Total.png",             "Bland-Altman — Total domain, all pairs (raw)",      "Every within-domain instrument pair; ADOS panels = clinician gap.",
  "Fig8_BlandAltman_Social.png",            "Bland-Altman — Social domain, all pairs (raw)",     "Every within-domain instrument pair, Social.",
  "Fig8_BlandAltman_RRB.png",               "Bland-Altman — RRB domain, all pairs (raw)",        "Every within-domain instrument pair, RRB.",
  "Fig8_BlandAltman_Total_resampled.png",   "Bland-Altman — Total, all pairs (sex-balanced)",    "Total-domain pairs on the sex-balanced resample.",
  "Fig8_BlandAltman_Social_resampled.png",  "Bland-Altman — Social, all pairs (sex-balanced)",   "Social-domain pairs on the sex-balanced resample.",
  "Fig8_BlandAltman_RRB_resampled.png",     "Bland-Altman — RRB, all pairs (sex-balanced)",      "RRB-domain pairs on the sex-balanced resample.",
  "Fig4_EffectSize_forest.png",             "Sex mean difference (Cohen's d) + 95% CI",          "ADOS ~0 (MIND girls-lower); SRS girls-higher and replicates.",
  "Fig3_Reconciliation.png",                "Two kinds of agreement: LEVEL vs RANK",             "Bigger LEVEL gap yet a small higher RANK agreement.",
  "Fig1_Profile_MIND_vs_SSC.png",           "Profile by cohort (MIND vs SSC)",                   "SRS-Parent girls-higher in both; SRS-Teacher (SSC) largest.",
  "Fig2_Distributions_by_informant.png",    "Score distributions (violin + box)",                "Girls' teacher-SRS shifted up and more spread; ADOS overlaps.",
  "Fig5_Reconciliation_plane.png",          "Reconciliation plane (LEVEL d vs RANK q)",          "Mostly horizontal spread — two different quantities.",
  "Fig7_EffectSize_by_age.png",             "Robustness across age × cohort (Cohen's d + CI)",   "SRS-Parent girls-higher holds across age/cohort; teacher-SRS (SSC) largest.")
pages <- pages[file.exists(pages$png), ]

# ---- contents page (dynamic layout so it fits ANY number of pages) ----------
ragg::agg_png("._contents.png", width = 1650, height = 1200, res = 150, background = "white")
grid.newpage()
grid.text("Figures — Sex differences in autism tool agreement",
          x = 0.04, y = 0.955, hjust = 0, gp = gpar(fontface = "bold", fontsize = 19))
grid.text("Prepared for Dr. Einat Waizbard  ·  MIND (APP) + SSC",
          x = 0.04, y = 0.918, hjust = 0, gp = gpar(fontsize = 11, col = "grey35"))
n <- nrow(pages); top <- 0.86; bot <- 0.06; step <- (top - bot) / max(n, 1)
show_cap <- n <= 12; tfs <- if (n > 14) 9 else 11
y <- top
for (i in seq_len(n)) {
  grid.text(paste0(i, ". ", pages$title[i]), x = 0.05, y = y, hjust = 0,
            gp = gpar(fontface = "bold", fontsize = tfs))
  if (show_cap) grid.text(pages$caption[i], x = 0.075, y = y - step * 0.42, hjust = 0,
                          gp = gpar(fontsize = 9, col = "grey30"))
  y <- y - step
}
grid.text("Effect sizes throughout (Cohen's d/q); uncorrected alpha primary, BH-FDR beside. z-standardized. Resampled = sex-balanced (girls fixed, boys matched).",
          x = 0.04, y = 0.03, hjust = 0, gp = gpar(fontsize = 8.5, col = "grey40", fontface = "italic"))
dev.off()

# ---- collate to one PDF ------------------------------------------------------
all_png <- c("._contents.png", pages$png)
imgs <- magick::image_read(all_png)
magick::image_write(imgs, "Figures_for_Einat.pdf", format = "pdf", density = 150)
file.rename("._contents.png", "Figures_00_contents.png")
cat("WROTE Figures_for_Einat.pdf —", length(all_png), "pages\n")
