# fig_bland_altman.R — Bland-Altman agreement by sex. Emits EIGHT figures:
#   Fig8_BlandAltman_by_sex[_resampled]            headline 6-panel ADOS<->SRS
#   Fig8_BlandAltman_{Total,Social,RRB}[_resampled] all within-domain pairs (Fisher family)
#
# RAW figures use the full observed sample. RESAMPLED figures show the sex-balanced
# version, built to "unite the N draws" rather than pick one:
#   - girls are held fixed (always fully included -> girls' points & lines identical
#     to raw), only boys are subsampled to balance n;
#   - each per-sex LINE (bias, +/-1.96 SD limits, r) is the MEDIAN across all N
#     balanced draws (Stage-5 aggregation) -> not from any single draw;
#   - the point CLOUD needs real paired children, so boys are shown from ONE
#     representative balanced draw = the MEDOID (the draw whose per-pair boys-bias
#     vector is closest to the N-draw median). Its stats match the median lines by
#     construction, and it is shared across all four figures = one "resampled
#     dataset". Draws are stratified by cohort x age (same granularity as Stage 5).
#
# y = z(B) - z(A) for a panel "B - A"; z-scoring is pooled over the full sample per
# measure (same as fig_gap_density.R) in BOTH modes so axes are comparable.
if (!exists("BASE_DIR")) BASE_DIR <- Sys.getenv("MIND_BASE_DIR", unset = "C:/Users/galor/OneDrive/Documents/Desktop/MIND")
setwd(BASE_DIR)
source("fig_common.R")

full <- fc_slices()[["All combined"]]
zsc  <- function(x){ x<-suppressWarnings(as.numeric(x)); s<-sd(x,na.rm=TRUE); if(is.na(s)||s==0) x-mean(x,na.rm=TRUE) else (x-mean(x,na.rm=TRUE))/s }
N_REPS <- if (exists("RESAMPLE_N_REPS")) get("RESAMPLE_N_REPS") else 20L
SEED   <- if (exists("RESAMPLE_SEED"))   get("RESAMPLE_SEED")   else 1L

DOMAINS <- list(
  Total  = c("ADOS_Severity","ADI_Total","SRS_Total_Parent","SRS_Total_Teacher"),
  Social = c("ADOS_SA","ADI_Social_Com","SRS_Social_Parent","SRS_Social_Teacher"),
  RRB    = c("ADOS_RRB","ADI_RRB","SRS_RRB_Parent","SRS_RRB_Teacher","RBSR_Total"))
SHORT <- c(ADOS="ADOS", ADI="ADI", "SRS-Parent"="SRS-P", "SRS-Teacher"="SRS-T", RBSR="RBSR")
abbr  <- function(m) unname(SHORT[imeta$instrument[match(m, imeta$measure)]])
lv_sex <- c("Girls","Boys")

## ---- shared balanced draws + z cache ---------------------------------------
girls_idx <- which(full$sex=="Female"); boys_idx <- which(full$sex=="Male")
draws      <- fc_balanced_draws(full, n_reps=N_REPS, seed=SEED)          # list of row-index vectors
draws_boys <- lapply(draws, function(ix) ix[full$sex[ix]=="Male"])       # drawn boys per rep
zcache <- new.env(parent=emptyenv())
zof <- function(m){ if(is.null(zcache[[m]])) assign(m, zsc(full[[m]]), zcache); get(m, zcache) }

sex_stat <- function(za,zb,idx,lab){
  a<-za[idx]; b<-zb[idx]; ok<-is.finite(a)&is.finite(b)
  if(sum(ok)<10) return(NULL)
  d<-b[ok]-a[ok]; bias<-mean(d); s<-sd(d)
  tibble(sex=lab, n=sum(ok), bias=bias, sd=s, lo=bias-1.96*s, hi=bias+1.96*s,
         r=suppressWarnings(cor(a[ok],b[ok])))
}
boys_perdraw <- function(za,zb) lapply(draws_boys, function(bx) sex_stat(za,zb,bx,"Boys"))
med_num <- function(perd, f) stats::median(sapply(perd, function(t) if(is.null(t)) NA_real_ else f(t)), na.rm=TRUE)

## ---- one GLOBAL representative (medoid) draw across all 22 within-domain pairs
ALL <- do.call(rbind, lapply(names(DOMAINS), function(dm){
  cb<-t(utils::combn(DOMAINS[[dm]],2)); data.frame(a=cb[,1], b=cb[,2], stringsAsFactors=FALSE)}))
Mb <- vapply(seq_len(nrow(ALL)), function(i){
  za<-zof(ALL$a[i]); zb<-zof(ALL$b[i])
  sapply(draws_boys, function(bx){ s<-sex_stat(za,zb,bx,"Boys"); if(is.null(s)) NA_real_ else s$bias })
}, numeric(length(draws_boys)))                     # draws x pairs
medoid   <- fc_medoid(Mb)
boys_rep <- draws_boys[[medoid]]
cat(sprintf("resampled figures: N_REPS=%d, seed=%d, medoid draw=%d, balanced n = %d girls + %d boys\n",
            N_REPS, SEED, medoid, length(girls_idx), length(boys_rep)))

## ---- per-pair lines + points (mode = raw | resampled) ----------------------
pair_lines <- function(za,zb, mode){
  gr <- sex_stat(za,zb,girls_idx,"Girls")                       # girls fixed in both modes
  if(mode=="raw"){
    br <- sex_stat(za,zb,boys_idx,"Boys")
  } else {
    perd <- boys_perdraw(za,zb)
    bi<-med_num(perd,function(t)t$bias); sdb<-med_num(perd,function(t)t$sd)
    br <- tibble(sex="Boys", n=round(med_num(perd,function(t)t$n)),
                 bias=bi, sd=sdb, lo=bi-1.96*sdb, hi=bi+1.96*sdb, r=med_num(perd,function(t)t$r))
  }
  bind_rows(gr,br)
}
pair_points <- function(za,zb, mode){
  idx <- if(mode=="raw") c(girls_idx,boys_idx) else c(girls_idx, boys_rep)
  tibble(sex=ifelse(full$sex[idx]=="Female","Girls","Boys"),
         avg=(za[idx]+zb[idx])/2, diff=zb[idx]-za[idx]) %>% filter(is.finite(avg)&is.finite(diff))
}

## ---- renderer --------------------------------------------------------------
render <- function(P, L, facet, title, subtitle, stem, w, h, txt=2.5){
  LG<-filter(L,sex=="Girls"); LB<-filter(L,sex=="Boys")
  p <- ggplot(P, aes(avg,diff,color=sex)) +
    geom_hline(yintercept=0,color="grey75",linewidth=0.3) +
    geom_point(alpha=0.16,size=0.5,shape=16,position=position_jitter(width=0.14,height=0.14,seed=1)) +
    geom_hline(data=L,aes(yintercept=bias,color=sex),linewidth=0.8) +
    geom_hline(data=L,aes(yintercept=lo,color=sex),linetype=2,linewidth=0.4) +
    geom_hline(data=L,aes(yintercept=hi,color=sex),linetype=2,linewidth=0.4) +
    geom_text(data=LG,aes(x=-Inf,y=Inf,label=sprintf("G bias %+.2f  r=%.2f",bias,r)),hjust=-0.05,vjust=1.5,size=txt,color=PINK,inherit.aes=FALSE) +
    geom_text(data=LB,aes(x=-Inf,y=Inf,label=sprintf("B bias %+.2f  r=%.2f",bias,r)),hjust=-0.05,vjust=3.0,size=txt,color=BLUE,inherit.aes=FALSE) +
    facet + scale_color_manual(values=SEXCOL,name=NULL) +
    guides(color=guide_legend(override.aes=list(alpha=1,size=1.6))) +
    coord_cartesian(xlim=c(-2.6,2.6), ylim=c(-4.3,4.3)) +
    labs(title=title, subtitle=subtitle,
         x="Mean standardized severity   ( z(A) + z(B) ) / 2", y="z(B) - z(A)   (z units)") +
    theme_einat() + theme(panel.grid.minor=element_blank())
  fc_save(p, stem, w=w, h=h)
}
sub_line2 <- function(mode){
  if(mode=="raw")
    "Solid = per-sex mean bias; dashed = +/-1.96 SD limits; r = within-sex correlation. Points jittered for legibility; lines/r on exact values."
  else
    sprintf("Sex-balanced: girls fixed, boys balanced to girls' n. Lines = MEDIAN over %d draws; cloud = representative (medoid) draw. Jittered for legibility.", N_REPS)
}

## ---- headline: 6-panel ADOS(clinician) <-> SRS(informant) ------------------
build_focused <- function(mode){
  pr <- tibble::tribble(~informant,~domain,~a,~b,
    "SRS-Parent","TOTAL","ADOS_Severity","SRS_Total_Parent",
    "SRS-Parent","SOCIAL","ADOS_SA","SRS_Social_Parent",
    "SRS-Parent","RRB","ADOS_RRB","SRS_RRB_Parent",
    "SRS-Teacher","TOTAL","ADOS_Severity","SRS_Total_Teacher",
    "SRS-Teacher","SOCIAL","ADOS_SA","SRS_Social_Teacher",
    "SRS-Teacher","RRB","ADOS_RRB","SRS_RRB_Teacher")
  P<-list(); L<-list()
  for(k in seq_len(nrow(pr))){
    za<-zof(pr$a[k]); zb<-zof(pr$b[k])
    P[[k]] <- pair_points(za,zb,mode) %>% mutate(informant=pr$informant[k], domain=pr$domain[k])
    L[[k]] <- pair_lines(za,zb,mode)  %>% mutate(informant=pr$informant[k], domain=pr$domain[k])
  }
  ff <- function(t) t %>% mutate(informant=factor(informant,c("SRS-Parent","SRS-Teacher")),
                                 domain=factor(domain,c("TOTAL","SOCIAL","RRB")), sex=factor(sex,lv_sex))
  P<-ff(bind_rows(P)) %>% arrange(desc(sex)); L<-ff(bind_rows(L))
  tag <- if(mode=="raw") "" else "  [sex-balanced resample]"
  render(P,L, facet_grid(informant~domain),
         paste0("Bland-Altman agreement by sex - ADOS clinician vs SRS informant", tag),
         paste0("Each point a child; x = mean of the two z-scores, y = z(SRS) - z(ADOS), >0 = informant reports more.\n", sub_line2(mode)),
         paste0("Fig8_BlandAltman_by_sex", if(mode=="raw") "" else "_resampled"), w=11, h=6.8, txt=2.6)
}

## ---- all within-domain pairs, one figure per domain ------------------------
build_domain <- function(dom, mode, ncol, w, h){
  vars<-DOMAINS[[dom]]; cb<-t(utils::combn(vars,2)); labs<-character(nrow(cb))
  P<-list(); L<-list()
  for(i in seq_len(nrow(cb))){
    a<-cb[i,1]; b<-cb[i,2]; labs[i]<-sprintf("%s - %s", abbr(b), abbr(a))
    za<-zof(a); zb<-zof(b)
    P[[i]] <- pair_points(za,zb,mode) %>% mutate(pair=labs[i])
    L[[i]] <- pair_lines(za,zb,mode)  %>% mutate(pair=labs[i])
  }
  P<-bind_rows(P) %>% mutate(pair=factor(pair,levels=labs), sex=factor(sex,lv_sex)) %>% arrange(desc(sex))
  L<-bind_rows(L) %>% mutate(pair=factor(pair,levels=labs), sex=factor(sex,lv_sex))
  tag <- if(mode=="raw") "" else "  [sex-balanced resample]"
  render(P,L, facet_wrap(~pair,ncol=ncol),
         sprintf("Bland-Altman agreement by sex - %s domain (all within-domain pairs)%s", dom, tag),
         paste0("Panel 'B - A': y = z(B) - z(A), >0 = B rates higher. ADOS panels = clinician-vs-informant (masking); others = informant-vs-informant.\n", sub_line2(mode)),
         sprintf("Fig8_BlandAltman_%s%s", dom, if(mode=="raw") "" else "_resampled"), w=w, h=h)
}

for(mode in c("raw","resampled")){
  build_focused(mode)
  build_domain("Total",  mode, ncol=3, w=11, h=7.6)
  build_domain("Social", mode, ncol=3, w=11, h=7.6)
  build_domain("RRB",    mode, ncol=3, w=11, h=11.4)
}

## console check: raw vs resampled boys bias for the ADOS<->SRS pairs
cat("\n== raw vs resampled(median) BOYS bias, ADOS-anchored SRS pairs ==\n")
for(pr in list(c("ADOS_Severity","SRS_Total_Parent"), c("ADOS_Severity","SRS_Total_Teacher"),
               c("ADOS_SA","SRS_Social_Teacher"), c("ADOS_RRB","SRS_RRB_Teacher"))){
  za<-zof(pr[1]); zb<-zof(pr[2])
  raw<-sex_stat(za,zb,boys_idx,"Boys")$bias
  rs <-med_num(boys_perdraw(za,zb),function(t)t$bias)
  cat(sprintf("  %-32s raw=%+.3f  resampled=%+.3f\n", sprintf("%s~%s",abbr(pr[2]),abbr(pr[1])), raw, rs))
}
