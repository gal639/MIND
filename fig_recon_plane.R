# Iteration 5: reconciliation plane — LEVEL (Cohen's d) x RANK (Cohen's q) per
# ADOS<->informant pair. Orthogonal axes prove the two "agreements" are different
# quantities that co-occur; upper-right = literature-consistent gap AND new finding.
if (!exists("BASE_DIR")) BASE_DIR <- Sys.getenv("MIND_BASE_DIR", unset = "C:/Users/galor/OneDrive/Documents/Desktop/MIND")
setwd(BASE_DIR)
source("fig_common.R")
suppressPackageStartupMessages(library(ggrepel))

full <- fc_slices()[["All combined"]]
fz <- function(r) 0.5*log((1+r)/(1-r))
cohen_d <- function(g,b){ sp<-sqrt(((length(g)-1)*var(g)+(length(b)-1)*var(b))/(length(g)+length(b)-2)); if(is.na(sp)||sp==0) NA else (mean(g)-mean(b))/sp }

dmeta <- tibble::tribble(
  ~ados,~inf,~domain,~tool,
  "ADOS_Severity","ADI_Total","TOTAL","ADI","ADOS_Severity","SRS_Total_Parent","TOTAL","SRS-P","ADOS_Severity","SRS_Total_Teacher","TOTAL","SRS-T",
  "ADOS_SA","ADI_Social_Com","SOCIAL","ADI","ADOS_SA","SRS_Social_Parent","SOCIAL","SRS-P","ADOS_SA","SRS_Social_Teacher","SOCIAL","SRS-T",
  "ADOS_RRB","ADI_RRB","RRB","ADI","ADOS_RRB","SRS_RRB_Parent","RRB","SRS-P","ADOS_RRB","RBSR_Total","RRB","RBSR","ADOS_RRB","SRS_RRB_Teacher","RRB","SRS-T")

P <- list()
for (k in seq_len(nrow(dmeta))) {
  ados<-dmeta$ados[k]; inf<-dmeta$inf[k]
  if(!(ados%in%names(full))||!(inf%in%names(full))) next
  xi<-suppressWarnings(as.numeric(full[[inf]]))
  g<-xi[full$sex=="Female"&!is.na(xi)]; b<-xi[full$sex=="Male"&!is.na(xi)]
  if(length(g)<10||length(b)<10) next
  d<-cohen_d(g,b)
  rr<-function(sx){ dd<-full[full$sex==sx,]; ok<-complete.cases(dd[[ados]],dd[[inf]]); if(sum(ok)<10) return(NA); cor(dd[[ados]][ok],dd[[inf]][ok]) }
  q<-fz(rr("Female"))-fz(rr("Male"))
  P[[length(P)+1]]<-tibble(domain=dmeta$domain[k],tool=dmeta$tool[k],inf=inf,d=d,q=q,
                           lab=sprintf("%s·%s", dmeta$tool[k], substr(dmeta$domain[k],1,3)))
}
P <- list_rbind(P)

p <- ggplot(P, aes(d, q, color=domain, shape=tool)) +
  annotate("rect", xmin=0.2, xmax=Inf, ymin=0.1, ymax=Inf, fill="grey30", alpha=.06) +
  annotate("text", x=Inf, y=Inf, label="reconciliation zone:\nlevel gap (lit.) + higher rank agreement",
           hjust=1.02, vjust=1.3, size=2.8, color="grey35", lineheight=0.9) +
  geom_hline(yintercept=0, color="grey40") + geom_vline(xintercept=0, color="grey40") +
  geom_vline(xintercept=c(.2,.5,.8), linetype=3, color="grey78") +
  geom_hline(yintercept=c(.1,.3,.5), linetype=3, color="grey78") +
  geom_point(size=3.2) +
  ggrepel::geom_text_repel(aes(label=lab), size=2.9, min.segment.length=0, seed=1, max.overlaps=20, show.legend=FALSE) +
  scale_color_manual(values=c(TOTAL="#1B7837", SOCIAL="#2166AC", RRB="#B2182B"), name="Domain") +
  scale_shape_manual(values=c(ADI=16, `SRS-P`=17, `SRS-T`=15, RBSR=18), name="Informant") +
  labs(title="Reconciliation plane — LEVEL gap vs RANK agreement (each ADOS ↔ informant pair)",
       subtitle="x = Cohen's d (informant girls−boys, the level gap).  y = Cohen's q (girls−boys correlation, rank).\nSpread is mostly HORIZONTAL: level gaps are LARGE (teacher-SRS far right) while rank differences are SMALL and mostly positive — the two 'agreements' are different quantities, and the level gap is the robust one.",
       x="LEVEL: sex gap in informant severity  (Cohen's d, girls − boys)",
       y="RANK: sex gap in ADOS↔informant agreement  (Cohen's q)") +
  theme_einat(11) + theme(plot.subtitle=element_text(size=8.5))

fc_save(p, "Fig5_Reconciliation_plane", w=10.5, h=7.5)
