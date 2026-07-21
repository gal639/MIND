# Iteration 2: distribution view (violin + boxplot) by informant/subscale/sex.
# Shows the full spread (variance) and any multimodality (subgroups), z-scored.
if (!exists("BASE_DIR")) BASE_DIR <- Sys.getenv("MIND_BASE_DIR", unset = "C:/Users/galor/OneDrive/Documents/Desktop/MIND")
setwd(BASE_DIR)
source("fig_common.R")

L <- fc_long_z(fc_slices()[["All combined"]])

p <- ggplot(L, aes(subscale, z, fill=sex, color=sex)) +
  geom_violin(position=position_dodge(width=0.8), width=0.85, alpha=0.18, linewidth=0.3, color=NA) +
  geom_boxplot(position=position_dodge(width=0.8), width=0.28, alpha=0.85, outlier.size=0.4,
               outlier.alpha=0.25, linewidth=0.4, color="grey25") +
  facet_grid(~instrument, scales="free_x", space="free_x") +
  scale_fill_manual(values=SEXCOL, name=NULL) +
  scale_color_manual(values=SEXCOL, name=NULL, guide="none") +
  labs(title="Score distributions by informant and sex  (all children combined, z-scored)",
       subtitle="Violin = distribution shape (look for bumps = subgroups); box = median + IQR (spread). Girls' teacher-SRS is shifted UP and MORE spread; ADOS distributions overlap.",
       x=NULL, y="Standardized score (z)") +
  theme_einat() +
  theme(panel.grid.major.x=element_blank())

fc_save(p, "Fig2_Distributions_by_informant", w=12, h=6.8)
