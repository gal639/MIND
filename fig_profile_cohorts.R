# Iteration 1: informant-grouped mean z-profile faceted by COHORT x instrument.
setwd("C:/Users/galor/OneDrive/Documents/Desktop/MIND")
source("fig_common.R")

slices <- fc_slices()
pick <- c("MIND","SSC")
D <- purrr::imap(slices[pick], function(df, nm) fc_desc(df) %>% mutate(cohort=nm)) %>%
  purrr::list_rbind() %>% mutate(cohort=factor(cohort, levels=pick))

pd <- position_dodge(width=0.25)
p <- ggplot(D, aes(subscale, mean_z, color=sex, group=sex)) +
  geom_hline(yintercept=0, linetype=2, color="grey60") +
  geom_line(position=pd, linewidth=0.9) + geom_point(position=pd, size=2.5) +
  facet_grid(cohort ~ instrument, scales="free_x", space="free_x") +
  scale_color_manual(values=SEXCOL, name=NULL) +
  scale_y_continuous(expand=expansion(mult=c(.12,.12))) +
  labs(title="Symptom profile by informant and sex — MIND vs SSC",
       subtitle="Mean standardized score per subscale. Rows = cohort. Does the girls-higher informant pattern replicate across cohorts? (SRS-Teacher exists in SSC only.)",
       x=NULL, y="Mean z-score") +
  theme_einat()

fc_save(p, "Fig1_Profile_MIND_vs_SSC", w=12, h=6.8)
