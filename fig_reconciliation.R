# Iteration 3: reconciliation figure — LEVEL (fan) vs RANK (dumbbell), polished.
setwd("C:/Users/galor/OneDrive/Documents/Desktop/MIND")
source("fig_common.R")
suppressPackageStartupMessages(library(patchwork))

full <- fc_slices()[["All combined"]]
sm   <- readRDS("SEM_output/sex_means.rds")$resampled
dfull <- function(m){ r <- sm[sm$slice=="1. Full" & sm$measure==m,]; if(nrow(r)) r$es_median[1] else NA_real_ }

dmeta <- tibble::tribble(
  ~measure,~domain,~tool,~kind,
  "ADOS_Severity","TOTAL","ADOS","clin","ADI_Total","TOTAL","ADI","inf","SRS_Total_Parent","TOTAL","SRS-P","inf","SRS_Total_Teacher","TOTAL","SRS-T","inf",
  "ADOS_SA","SOCIAL","ADOS","clin","ADI_Social_Com","SOCIAL","ADI","inf","SRS_Social_Parent","SOCIAL","SRS-P","inf","SRS_Social_Teacher","SOCIAL","SRS-T","inf",
  "ADOS_RRB","RRB","ADOS","clin","ADI_RRB","RRB","ADI","inf","SRS_RRB_Parent","RRB","SRS-P","inf","RBSR_Total","RRB","RBSR","inf","SRS_RRB_Teacher","RRB","SRS-T","inf")
TOOL_ORD <- c("ADOS","ADI","SRS-P","RBSR","SRS-T"); DOM_ORD <- c("TOTAL","SOCIAL","RRB")
fz <- function(r) 0.5*log((1+r)/(1-r))

# ---- Panel A: level fan (mean z by sex, domain facets, clinician-first) ------
zsc <- function(x){ s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) x-mean(x,na.rm=TRUE) else (x-mean(x,na.rm=TRUE))/s }
A <- list()
for (k in seq_len(nrow(dmeta))) { m<-dmeta$measure[k]; if(!(m %in% names(full))) next
  x<-suppressWarnings(as.numeric(full[[m]])); if(sum(!is.na(x))<40) next; z<-zsc(x)
  for(sx in c("Male","Female")){ zz<-z[full$sex==sx & !is.na(z)]; if(length(zz)<10) next
    A[[length(A)+1]]<-tibble(domain=dmeta$domain[k],tool=dmeta$tool[k],kind=dmeta$kind[k],
      sex=ifelse(sx=="Female","Girls","Boys"),mean_z=mean(zz)) } }
A <- list_rbind(A) %>% mutate(tool=factor(tool,TOOL_ORD),domain=factor(domain,DOM_ORD))
dlab <- dmeta %>% filter(kind=="inf") %>% mutate(d=map_dbl(measure,dfull)) %>% filter(!is.na(d)) %>%
  left_join(A %>% filter(sex=="Girls") %>% select(domain,tool,mean_z), by=c("domain","tool")) %>%
  mutate(tool=factor(tool,TOOL_ORD),domain=factor(domain,DOM_ORD),lab=sprintf("d=%+.2f",d))

pA <- ggplot(A, aes(tool, mean_z, group=sex, color=sex)) +
  geom_hline(yintercept=0, linetype=2, color="grey55") +
  geom_line(aes(linewidth=sex)) + geom_point(size=2.6) +
  geom_label(data=dlab, aes(tool,mean_z,label=lab), color=PINK, fill="white", label.size=NA,
             label.padding=unit(0.1,"lines"), vjust=-0.55, size=2.7, inherit.aes=FALSE) +
  facet_grid(~domain, scales="free_x", space="free_x") +
  scale_color_manual(values=SEXCOL, name=NULL) +
  scale_linewidth_manual(values=c(Boys=0.7,Girls=1.2), guide="none") +
  scale_y_continuous(expand=expansion(mult=c(.08,.20))) +
  labs(title="A.  LEVEL — do the tools agree on how severe the child is?",
       subtitle="Mean standardized score. ADOS (clinician) ~0 for both sexes; girls' informant scores fan up = tools disagree on level, more for girls (Cohen's d, girls−boys).",
       x=NULL, y="Group mean (z)") + theme_einat()

# ---- Panel B: rank dumbbell (r boys vs girls, ADOS<->informant) --------------
B <- list()
for (dom in DOM_ORD){ mm<-dmeta %>% filter(domain==dom); ados<-mm$measure[mm$kind=="clin"][1]
  for(k in which(mm$kind=="inf")){ inf<-mm$measure[k]; if(!(ados%in%names(full))||!(inf%in%names(full))) next
    rr<-function(sx){ d<-full[full$sex==sx,]; ok<-complete.cases(d[[ados]],d[[inf]]); if(sum(ok)<10) return(NA_real_); cor(d[[ados]][ok],d[[inf]][ok]) }
    rb<-rr("Male"); rg<-rr("Female"); if(any(is.na(c(rb,rg)))) next
    B[[length(B)+1]]<-tibble(domain=dom,tool=mm$tool[k],r_boys=rb,r_girls=rg,q=fz(rg)-fz(rb)) } }
B <- list_rbind(B) %>% mutate(tool=factor(tool,TOOL_ORD),domain=factor(domain,DOM_ORD),qlab=sprintf("q=%+.2f",q))
bandlab <- tibble(domain=factor("TOTAL",DOM_ORD))

pB <- ggplot(B, aes(y=tool)) +
  annotate("rect", xmin=0.1, xmax=0.5, ymin=-Inf, ymax=Inf, fill="grey86", alpha=.6) +
  geom_text(data=bandlab, aes(x=0.3, y=Inf, label="weak–moderate (literature)"), vjust=1.4,
            size=2.5, color="grey45", inherit.aes=FALSE) +
  geom_segment(aes(x=r_boys, xend=r_girls, yend=tool), color="grey60", linewidth=0.8) +
  geom_point(aes(x=r_boys, color="Boys"), size=2.6) + geom_point(aes(x=r_girls, color="Girls"), size=2.6) +
  geom_text(aes(x=pmax(r_boys,r_girls), label=qlab), hjust=-0.22, size=2.8, color=PINK) +
  facet_grid(domain~., scales="free_y", space="free_y") +
  scale_color_manual(values=SEXCOL, name=NULL) +
  scale_x_continuous(limits=c(0,0.62), breaks=c(0,.1,.3,.5), expand=expansion(mult=c(.02,.16))) +
  labs(title="B.  RANK — do the tools track the same children? (ADOS ↔ informant)",
       subtitle="Correlation of ADOS with each informant tool, boys vs girls. Girls' r is higher (Cohen's q) yet stays weak–moderate = literature-consistent.",
       x="Correlation r (ADOS ↔ informant)", y=NULL) + theme_einat()

fig <- (pA / pB) + plot_layout(heights=c(1.1,1)) +
  plot_annotation(title="Two kinds of tool agreement, one set of children",
    subtitle="Girls show a BIGGER level gap (A) yet HIGHER rank agreement (B) — the opposite-pattern that reconciles our sample with the literature (correlation ≠ level agreement).",
    theme=theme(plot.title=element_text(face="bold",size=15),
                plot.subtitle=element_text(size=10,color="grey20"), plot.margin=margin(10,14,8,10)))
fc_save(fig, "Fig3_Reconciliation", w=11, h=10)
