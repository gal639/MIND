# =============================================================================
# make_profile_by_informant.R
# Symptom-profile by SEX, grouped BY INSTRUMENT/INFORMANT (Einat's request):
# ADOS -> ADI -> SRS-Parent -> SRS-Teacher -> RBSR, subscales (Total/Social/RRB)
# within each, so one can see whether the sex difference is consistent across ALL
# subscales of an instrument. Two stacked panels (profile lines, no heatmaps):
#   A. LEVEL  — mean z per subscale by sex (the sex difference per informant)
#   B. SPREAD — SD ratio girls/boys per subscale (where are girls more variable?)
# Plus a descriptives table (n, raw mean, raw SD, SD ratio).
# =============================================================================
setwd("C:/Users/galor/OneDrive/Documents/Desktop/MIND")
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(purrr); library(tibble); library(gridExtra); library(grid) })
source("resample_balance.R"); source("resample_lenses.R")

BLUE <- "#2166AC"; PINK <- "#C0397F"; GREY <- "grey55"
dat <- build_resample_data("SEM_output/data_with_iq", iq_policy = "all")
dat <- dat[dat$sex %in% c("Male","Female"), ]

imeta <- tibble::tribble(
  ~measure,            ~instrument,    ~subscale,
  "ADOS_Severity",     "ADOS",         "Total",
  "ADOS_SA",           "ADOS",         "Social",
  "ADOS_RRB",          "ADOS",         "RRB",
  "ADI_Total",         "ADI",          "Total",
  "ADI_Social_Com",    "ADI",          "Social",
  "ADI_RRB",           "ADI",          "RRB",
  "SRS_Total_Parent",  "SRS-Parent",   "Total",
  "SRS_Social_Parent", "SRS-Parent",   "Social",
  "SRS_RRB_Parent",    "SRS-Parent",   "RRB",
  "SRS_Total_Teacher", "SRS-Teacher",  "Total",
  "SRS_Social_Teacher","SRS-Teacher",  "Social",
  "SRS_RRB_Teacher",   "SRS-Teacher",  "RRB",
  "RBSR_Total",        "RBSR",         "RRB")
INSTR_ORD <- c("ADOS","ADI","SRS-Parent","SRS-Teacher","RBSR")
SUB_ORD   <- c("Total","Social","RRB")

# ---- descriptives per measure x sex -----------------------------------------
desc <- list()
for (k in seq_len(nrow(imeta))) {
  m <- imeta$measure[k]; if (!(m %in% names(dat))) next
  x <- suppressWarnings(as.numeric(dat[[m]]))
  s <- sd(x, na.rm=TRUE); z <- if (is.na(s)||s==0) x-mean(x,na.rm=TRUE) else (x-mean(x,na.rm=TRUE))/s
  for (sx in c("Male","Female")) {
    xi <- x[dat$sex==sx & !is.na(x)]; zi <- z[dat$sex==sx & !is.na(z)]
    if (length(xi) < 10) next
    desc[[length(desc)+1]] <- tibble(
      instrument=imeta$instrument[k], subscale=imeta$subscale[k], measure=m,
      sex=ifelse(sx=="Female","Girls","Boys"),
      n=length(xi), mean_raw=mean(xi), sd_raw=sd(xi), mean_z=mean(zi), sd_z=sd(zi))
  }
}
desc <- list_rbind(desc) %>%
  mutate(instrument=factor(instrument,levels=INSTR_ORD), subscale=factor(subscale,levels=SUB_ORD))

# SD ratio (girls / boys) per subscale
sdr <- desc %>% select(instrument, subscale, sex, sd_raw) %>%
  pivot_wider(names_from=sex, values_from=sd_raw) %>%
  mutate(ratio = Girls/Boys, hi = ratio >= 1.10)

# ---- PANEL A: mean z profile by instrument/sex (clean) ----------------------
pd <- position_dodge(width=0.25)
pA <- ggplot(desc, aes(subscale, mean_z, color=sex, group=sex)) +
  geom_hline(yintercept=0, linetype=2, color="grey60") +
  geom_line(position=pd, linewidth=0.9) +
  geom_point(position=pd, size=2.8) +
  facet_grid(~instrument, scales="free_x", space="free_x") +
  scale_color_manual(values=c(Boys=BLUE, Girls=PINK), name=NULL) +
  scale_y_continuous(expand=expansion(mult=c(.12,.12))) +
  labs(title="A.  LEVEL — mean symptom score by informant and sex",
       subtitle="Mean standardized score per subscale. Read down each instrument: is the sex gap consistent across its Total/Social/RRB subscales? (ADOS ~ flat; SRS uniformly girls-higher.)",
       x=NULL, y="Mean z-score") +
  theme_bw(base_size=10) +
  theme(legend.position="top", plot.title=element_text(face="bold"),
        plot.subtitle=element_text(size=8, color="grey35"), strip.text=element_text(face="bold"),
        panel.spacing=unit(0.5,"lines"))

# ---- PANEL B: SD ratio girls/boys (variance) --------------------------------
pB <- ggplot(sdr, aes(subscale, ratio)) +
  geom_hline(yintercept=1, linetype=2, color="grey55") +
  geom_segment(aes(xend=subscale, y=1, yend=ratio, color=hi), linewidth=0.7) +
  geom_point(aes(color=hi), size=3) +
  geom_text(aes(label=sprintf("%.2f", ratio)), vjust=-0.9, size=2.7, color="grey25") +
  facet_grid(~instrument, scales="free_x", space="free_x") +
  scale_color_manual(values=c(`TRUE`=PINK, `FALSE`=GREY), guide="none") +
  scale_y_continuous(expand=expansion(mult=c(.10,.18))) +
  labs(title="B.  SPREAD — variance ratio (girls SD ÷ boys SD)",
       subtitle="Above the dashed line = girls MORE variable. Spread is ~equal everywhere EXCEPT SRS-Teacher (girls markedly more heterogeneous) — Einat's subgroup signal.",
       x=NULL, y="SD ratio (G / B)") +
  theme_bw(base_size=10) +
  theme(plot.title=element_text(face="bold"), plot.subtitle=element_text(size=8, color="grey35"),
        strip.text=element_text(face="bold"), panel.spacing=unit(0.5,"lines"))

fig <- (pA / pB) + plot_layout(heights=c(1,0.9)) +
  plot_annotation(title="Symptom profile by informant and sex  (all children combined)",
    subtitle="Grouped by instrument. Informants (esp. teachers) rate girls more severe than the clinician (ADOS) does, consistently across subscales (A); girls are also markedly more variable on teacher-SRS (B).",
    theme=theme(plot.title=element_text(face="bold", size=14),
                plot.subtitle=element_text(size=9.5, color="grey25"), plot.margin=margin(10,12,8,10)))

# ---- descriptives TABLE ------------------------------------------------------
tab <- desc %>% select(instrument, subscale, sex, n, mean_raw, sd_raw) %>%
  pivot_wider(names_from=sex, values_from=c(n, mean_raw, sd_raw)) %>%
  left_join(sdr %>% select(instrument, subscale, ratio, hi), by=c("instrument","subscale")) %>%
  arrange(instrument, subscale) %>%
  transmute(Instrument=as.character(instrument), Subscale=as.character(subscale),
    `n (B)`=n_Boys, `Mean (B)`=sprintf("%.2f", mean_raw_Boys), `SD (B)`=sprintf("%.2f", sd_raw_Boys),
    `n (G)`=n_Girls, `Mean (G)`=sprintf("%.2f", mean_raw_Girls), `SD (G)`=sprintf("%.2f", sd_raw_Girls),
    `SD ratio G/B`=sprintf("%.2f%s", ratio, ifelse(hi," ↑","")))
tt <- ttheme_minimal(base_size=9, core=list(fg_params=list(hjust=1, x=0.94)),
                     colhead=list(fg_params=list(fontface="bold")))
tg <- tableGrob(tab, rows=NULL, theme=tt)
title_g <- textGrob("Descriptives by sex — raw means & SD (variance).  ↑ = girls more variable (SD ratio ≥ 1.10)",
                    gp=gpar(fontface="bold", fontsize=11), x=0.01, hjust=0)

# ---- write PNGs + cairo PDF --------------------------------------------------
ggsave("Profile_by_Informant_1_profile.png", fig, width=12, height=8.5, dpi=150, device=ragg::agg_png)
ragg::agg_png("Profile_by_Informant_2_descriptives.png", width=1350, height=560, res=150)
grid.arrange(title_g, tg, heights=c(0.10, 0.90)); dev.off()
cat("WROTE 2 PNGs\n")
tryCatch({
  cairo_pdf("Profile_by_Informant.pdf", width=12, height=9, onefile=TRUE)
  print(fig); grid.newpage(); grid.arrange(title_g, tg, heights=c(0.10,0.90)); dev.off()
  cat("WROTE Profile_by_Informant.pdf\n")
}, error=function(e){ try(dev.off(),silent=TRUE); message("pdf skipped: ", conditionMessage(e)) })
