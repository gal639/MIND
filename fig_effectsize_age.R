# Iteration 8: Cohen's d (girls-boys) + 95% CI by informant across the FOUR
# age x cohort subsamples — robustness of the sex pattern to age and cohort.
setwd("C:/Users/galor/OneDrive/Documents/Desktop/MIND")
source("fig_common.R")

sm <- readRDS("SEM_output/sex_means.rds")$resampled
lab <- c("3a. MIND Preschool"="MIND pre", "3b. MIND School"="MIND school",
         "3c. SSC Preschool"="SSC pre",  "3d. SSC School"="SSC school")
D <- sm %>% filter(slice %in% names(lab)) %>%
  left_join(imeta, by="measure") %>% filter(!is.na(instrument)) %>%
  mutate(sub=factor(unname(lab[slice]), levels=unname(lab)),
         instrument=factor(instrument, levels=INSTR_ORD),
         subscale=factor(subscale, levels=rev(SUB_ORD)))

pd <- position_dodge(width=0.7)
p <- ggplot(D, aes(es_median, subscale, color=sub)) +
  annotate("rect", xmin=0.2, xmax=Inf, ymin=-Inf, ymax=Inf, fill="#C0397F", alpha=.04) +
  geom_vline(xintercept=0, color="grey35") +
  geom_vline(xintercept=c(0.2,0.5,0.8), linetype=3, color="grey72") +
  geom_errorbar(aes(xmin=es_lo, xmax=es_hi), orientation="y", width=0, position=pd, linewidth=0.45) +
  geom_point(position=pd, size=1.8) +
  facet_grid(instrument~., scales="free_y", space="free_y") +
  scale_color_manual(values=c(`MIND pre`="#5AAE61", `MIND school`="#1B7837",
                              `SSC pre`="#9970AB", `SSC school`="#762A83"), name="Subsample") +
  scale_x_continuous(breaks=c(0,.2,.5,.8)) +
  labs(title="Sex mean difference (Cohen's d) + 95% CI by informant — four age × cohort subsamples",
       subtitle="Does the pattern hold across age and cohort? SRS girls-higher is robust in SSC (both ages); teacher-SRS (SSC only) largest. Small girl-n subsamples (MIND school 52, SSC pre 46) have wide CIs.",
       x="Cohen's d (girls − boys)", y=NULL) +
  theme_einat() + theme(panel.grid.major.y=element_blank())

fc_save(p, "Fig7_EffectSize_by_age", w=10.5, h=8)
