# Iteration 4: Cohen's d (girls-boys) per measure with 95% CI, by instrument,
# across All / MIND / SSC — shows which sex mean-differences are robust & replicate.
setwd("C:/Users/galor/OneDrive/Documents/Desktop/MIND")
source("fig_common.R")

sm <- readRDS("SEM_output/sex_means.rds")$resampled
lab <- c("1. Full"="All", "2a. MIND"="MIND", "2b. SSC"="SSC")
D <- sm %>% filter(slice %in% names(lab)) %>%
  left_join(imeta, by="measure") %>% filter(!is.na(instrument)) %>%
  mutate(cohort=factor(unname(lab[slice]), levels=c("All","MIND","SSC")),
         instrument=factor(instrument, levels=INSTR_ORD),
         subscale=factor(subscale, levels=rev(SUB_ORD)))

pd <- position_dodge(width=0.6)
p <- ggplot(D, aes(es_median, subscale, color=cohort)) +
  annotate("rect", xmin=0.2, xmax=Inf, ymin=-Inf, ymax=Inf, fill="#C0397F", alpha=.04) +
  geom_vline(xintercept=0, color="grey35") +
  geom_vline(xintercept=c(0.2,0.5,0.8), linetype=3, color="grey70") +
  geom_errorbarh(aes(xmin=es_lo, xmax=es_hi), height=0, position=pd, linewidth=0.5) +
  geom_point(position=pd, size=2) +
  facet_grid(instrument~., scales="free_y", space="free_y") +
  scale_color_manual(values=c(All="grey25", MIND="#1B7837", SSC="#762A83"), name="Cohort") +
  scale_x_continuous(breaks=c(0,.2,.5,.8)) +
  labs(title="Sex mean difference (Cohen's d, girls − boys) with 95% CI, by informant",
       subtitle="Dotted lines = small/medium/large. CI clear of 0 = robust. SRS (esp. teacher) girls-higher and replicates; ADOS ~0 everywhere. Pink zone = d ≥ 0.2 (girls higher).",
       x="Cohen's d (girls − boys)", y=NULL) +
  theme_einat() + theme(panel.grid.major.y=element_blank())

fc_save(p, "Fig4_EffectSize_forest", w=10.5, h=8)
