# =============================================================================
# make_reconciliation_fig.R
# The paper's key figure: "two kinds of agreement, one set of children."
# Panel A (LEVEL): clinician-anchored z-profile — ADOS ~0 for both sexes, girls'
#   informant scores fan up (tools disagree on level, worse for girls =
#   literature-consistent). Cohen's d annotated.
# Panel B (RANK): boys' r vs girls' r dumbbell for ADOS<->informant pairs, with a
#   weak-moderate "literature" band and Cohen's q (girls higher = the new lens).
# Slice = Full (all children combined) for the headline.
# =============================================================================
if (!exists("BASE_DIR")) BASE_DIR <- Sys.getenv("MIND_BASE_DIR", unset = "C:/Users/galor/OneDrive/Documents/Desktop/MIND")
setwd(BASE_DIR)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(purrr); library(tibble) })
source("resample_balance.R"); source("resample_lenses.R")

BLUE <- "#2166AC"; PINK <- "#C0397F"
dat <- build_resample_data("SEM_output/data_with_iq", iq_policy = "all")
dat <- dat[dat$sex %in% c("Male","Female"), ]
sm  <- readRDS("SEM_output/sex_means.rds")$resampled
dfull <- function(m){ r <- sm[sm$slice=="1. Full" & sm$measure==m,]; if(nrow(r)) r$es_median[1] else NA_real_ }

meta <- tibble::tribble(
  ~measure,            ~domain,  ~tool,   ~kind,
  "ADOS_Severity",     "TOTAL",  "ADOS",  "clinician",
  "ADI_Total",         "TOTAL",  "ADI",   "informant",
  "SRS_Total_Parent",  "TOTAL",  "SRS-P", "informant",
  "SRS_Total_Teacher", "TOTAL",  "SRS-T", "informant",
  "ADOS_SA",           "SOCIAL", "ADOS",  "clinician",
  "ADI_Social_Com",    "SOCIAL", "ADI",   "informant",
  "SRS_Social_Parent", "SOCIAL", "SRS-P", "informant",
  "SRS_Social_Teacher","SOCIAL", "SRS-T", "informant",
  "ADOS_RRB",          "RRB",    "ADOS",  "clinician",
  "ADI_RRB",           "RRB",    "ADI",   "informant",
  "SRS_RRB_Parent",    "RRB",    "SRS-P", "informant",
  "RBSR_Total",        "RRB",    "RBSR",  "informant",
  "SRS_RRB_Teacher",   "RRB",    "SRS-T", "informant")
TOOL_ORD <- c("ADOS","ADI","SRS-P","RBSR","SRS-T")
DOM_ORD  <- c("TOTAL","SOCIAL","RRB")

# ---- Panel A data: mean z by sex per measure (Full, pooled-sex z) ------------
zsc <- function(x){ s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) x-mean(x,na.rm=TRUE) else (x-mean(x,na.rm=TRUE))/s }
A <- list()
for (k in seq_len(nrow(meta))) {
  m <- meta$measure[k]; if (!(m %in% names(dat))) next
  x <- suppressWarnings(as.numeric(dat[[m]])); if (sum(!is.na(x)) < 40) next
  z <- zsc(x)
  for (sx in c("Male","Female")) {
    zz <- z[dat$sex==sx & !is.na(z)]; if (length(zz) < 10) next
    A[[length(A)+1]] <- tibble(measure=m, domain=meta$domain[k], tool=meta$tool[k],
      sex=ifelse(sx=="Female","Girls","Boys"), mean_z=mean(zz))
  }
}
A <- list_rbind(A) %>% mutate(tool=factor(tool,levels=TOOL_ORD), domain=factor(domain,levels=DOM_ORD))
# d labels: informant tools only (ADOS anchors ~0), placed at girls' point
dlab <- meta %>% filter(kind=="informant") %>% mutate(d=map_dbl(measure,dfull)) %>%
  filter(!is.na(d)) %>% left_join(A %>% filter(sex=="Girls") %>% select(measure,mean_z), by="measure") %>%
  mutate(tool=factor(tool,levels=TOOL_ORD), domain=factor(domain,levels=DOM_ORD),
         lab=sprintf("d=%+.2f", d))

# ---- Panel B data: r_boys, r_girls, q for ADOS<->informant pairs -------------
fz <- function(r) 0.5*log((1+r)/(1-r))
B <- list()
for (dom in DOM_ORD) {
  mm <- meta %>% filter(domain==dom)
  ados <- mm$measure[mm$kind=="clinician"][1]
  for (k in which(mm$kind=="informant")) {
    inf <- mm$measure[k]
    if (!(ados %in% names(dat)) || !(inf %in% names(dat))) next
    rr <- function(sx){ d<-dat[dat$sex==sx,]; ok<-complete.cases(d[[ados]],d[[inf]])
      if (sum(ok)<10) return(NA_real_); cor(d[[ados]][ok], d[[inf]][ok]) }
    rb<-rr("Male"); rg<-rr("Female"); if (any(is.na(c(rb,rg)))) next
    B[[length(B)+1]] <- tibble(domain=dom, tool=mm$tool[k], r_boys=rb, r_girls=rg,
      q=fz(rg)-fz(rb))
  }
}
B <- list_rbind(B) %>% mutate(tool=factor(tool,levels=TOOL_ORD), domain=factor(domain,levels=DOM_ORD),
                              qlab=sprintf("q=%+.2f", q))

# ---- Panel A plot: the fan ---------------------------------------------------
pA <- ggplot(A, aes(tool, mean_z, group=sex, color=sex)) +
  geom_hline(yintercept=0, linetype=2, color="grey55") +
  geom_line(aes(linewidth=sex)) + geom_point(size=2.6) +
  geom_label(data=dlab, aes(x=tool, y=mean_z, label=lab), color=PINK, fill="white",
             label.size=0, label.padding=unit(0.1,"lines"), vjust=-0.55, size=2.7, inherit.aes=FALSE) +
  facet_grid(~domain, scales="free_x", space="free_x") +
  scale_color_manual(values=c(Boys=BLUE, Girls=PINK), name=NULL) +
  scale_linewidth_manual(values=c(Boys=0.7, Girls=1.2), guide="none") +
  scale_y_continuous(expand=expansion(mult=c(.08,.18))) +
  labs(title="A.  LEVEL — do the tools agree on how severe the child is?",
       subtitle="Mean standardized score. ADOS (clinician) sits at ~0 for both sexes; girls' informant scores fan up = tools disagree on level, more for girls (Cohen's d, girls−boys).",
       x=NULL, y="Group mean (z)") +
  theme_bw(base_size=10) +
  theme(legend.position="top", plot.title=element_text(face="bold"),
        plot.subtitle=element_text(size=8, color="grey35"), strip.text=element_text(face="bold"))

# ---- Panel B plot: rank dumbbell ---------------------------------------------
pB <- ggplot(B, aes(y=tool)) +
  annotate("rect", xmin=0.1, xmax=0.5, ymin=-Inf, ymax=Inf, fill="grey85", alpha=.5) +
  geom_segment(aes(x=r_boys, xend=r_girls, yend=tool), color="grey65", linewidth=0.8) +
  geom_point(aes(x=r_boys, color="Boys"), size=2.6) +
  geom_point(aes(x=r_girls, color="Girls"), size=2.6) +
  geom_text(aes(x=pmax(r_boys,r_girls), label=qlab), hjust=-0.25, size=2.9, color=PINK) +
  facet_grid(domain~., scales="free_y", space="free_y") +
  scale_color_manual(values=c(Boys=BLUE, Girls=PINK), name=NULL) +
  scale_x_continuous(limits=c(0,0.62), breaks=c(0,.1,.3,.5), expand=expansion(mult=c(.02,.16))) +
  labs(title="B.  RANK — do the tools track the same children? (ADOS ↔ informant)",
       subtitle="Correlation r of ADOS with each informant tool, boys vs girls (shaded = weak–moderate, the literature's typical band). Girls' r is higher (Cohen's q, girls−boys) yet stays weak–moderate = literature-consistent.",
       x="Correlation r (ADOS ↔ informant)", y=NULL) +
  theme_bw(base_size=10) +
  theme(legend.position="top", plot.title=element_text(face="bold"),
        plot.subtitle=element_text(size=8, color="grey35"), strip.text=element_text(face="bold"))

fig <- (pA / pB) + plot_layout(heights=c(1.1,1)) +
  plot_annotation(
    title="Two kinds of tool agreement, one set of children",
    subtitle="Girls show a BIGGER level gap (A) yet HIGHER rank agreement (B) — the opposite-pattern that reconciles our sample with the literature.",
    caption="Same children, two axes. Absolute agreement is weak–moderate for everyone (literature-consistent). By sex: informants rate girls far more severe than the clinician does (A, a level gap = under-detection/masking), while the tools co-vary slightly more for girls (B). Correlation ≠ level agreement.",
    theme=theme(plot.title=element_text(face="bold", size=15),
                plot.subtitle=element_text(size=10, color="grey20"),
                plot.caption=element_text(hjust=0, size=8, color="grey30"),
                plot.margin=margin(10,14,10,10)))

ggsave("Reconciliation_Figure.png", fig, width=11, height=10, dpi=150, device=ragg::agg_png)
tryCatch({ cairo_pdf("Reconciliation_Figure.pdf", width=11, height=10); print(fig); dev.off() },
         error=function(e) message("pdf skipped: ", conditionMessage(e)))
cat("WROTE Reconciliation_Figure.png (+pdf)\n")
cat("Panel A rows:", nrow(A), " Panel B pairs:", nrow(B), "\n")
