# Iteration 6: the masking gap. Per child, gap = z(informant) - z(ADOS clinician).
# gap>0 = informant reports MORE than the clinician sees. Girls' distribution
# shifted right of boys' = informant-clinician gap larger for girls (masking).
setwd("C:/Users/galor/OneDrive/Documents/Desktop/MIND")
source("fig_common.R")

full <- fc_slices()[["All combined"]]
zsc <- function(x){ x<-suppressWarnings(as.numeric(x)); s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) x-mean(x,na.rm=TRUE) else (x-mean(x,na.rm=TRUE))/s }
cohen_d <- function(g,b){ sp<-sqrt(((length(g)-1)*var(g)+(length(b)-1)*var(b))/(length(g)+length(b)-2)); if(is.na(sp)||sp==0) NA else (mean(g)-mean(b))/sp }

pairs <- tibble::tribble(
  ~informant,~domain,~ados,~inf,
  "SRS-Parent","TOTAL","ADOS_Severity","SRS_Total_Parent","SRS-Parent","SOCIAL","ADOS_SA","SRS_Social_Parent","SRS-Parent","RRB","ADOS_RRB","SRS_RRB_Parent",
  "SRS-Teacher","TOTAL","ADOS_Severity","SRS_Total_Teacher","SRS-Teacher","SOCIAL","ADOS_SA","SRS_Social_Teacher","SRS-Teacher","RRB","ADOS_RRB","SRS_RRB_Teacher")

G <- list(); ann <- list()
for (k in seq_len(nrow(pairs))) {
  za <- zsc(full[[pairs$ados[k]]]); zi <- zsc(full[[pairs$inf[k]]])
  gap <- zi - za
  df <- tibble(sex=ifelse(full$sex=="Female","Girls","Boys"), gap=gap,
               informant=pairs$informant[k], domain=pairs$domain[k])[!is.na(gap),]
  G[[k]] <- df
  g<-df$gap[df$sex=="Girls"]; b<-df$gap[df$sex=="Boys"]
  ann[[k]] <- tibble(informant=pairs$informant[k], domain=pairs$domain[k],
                     d=cohen_d(g,b), mg=mean(g), mb=mean(b))
}
G <- list_rbind(G) %>% mutate(informant=factor(informant,c("SRS-Parent","SRS-Teacher")), domain=factor(domain,c("TOTAL","SOCIAL","RRB")))
ann <- list_rbind(ann) %>% mutate(informant=factor(informant,c("SRS-Parent","SRS-Teacher")), domain=factor(domain,c("TOTAL","SOCIAL","RRB")),
                                  lab=sprintf("gap d=%+.2f", d))

p <- ggplot(G, aes(gap, fill=sex, color=sex)) +
  geom_vline(xintercept=0, linetype=2, color="grey45") +
  geom_density(alpha=0.35, linewidth=0.6) +
  geom_vline(data=ann, aes(xintercept=mb), color=BLUE, linetype=3) +
  geom_vline(data=ann, aes(xintercept=mg), color=PINK, linetype=3) +
  geom_text(data=ann, aes(x=Inf, y=Inf, label=lab), hjust=1.05, vjust=1.5, size=3, color="grey20", inherit.aes=FALSE) +
  facet_grid(informant~domain) +
  scale_fill_manual(values=SEXCOL, name=NULL) + scale_color_manual(values=SEXCOL, name=NULL) +
  coord_cartesian(xlim=c(-3.5,3.5)) +
  labs(title="The masking gap — informant minus clinician, per child",
       subtitle="gap = z(SRS informant) − z(ADOS clinician); right of 0 = informant reports more than the clinician sees. Girls (pink) sit right of boys (dotted means); gap Cohen's d per panel — largest for teacher-SRS.",
       x="Informant − clinician gap  (z units; >0 = informant reports more severity)", y="Density") +
  theme_einat() + theme(panel.grid.minor=element_blank())

fc_save(p, "Fig6_Masking_gap", w=11, h=6.5)
