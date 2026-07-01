# =============================================================================
# fig_common.R — shared data, metadata, palette and theme for the Einat figures.
# Sourced by the make_*.R figure scripts so each renders fast and consistently.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(purrr); library(tibble) })
if (!exists("build_resample_data")) { source("resample_balance.R"); source("resample_lenses.R") }

## ---- palette ----------------------------------------------------------------
BLUE <- "#2166AC"; PINK <- "#C0397F"; GREY <- "grey55"
SEXCOL <- c(Boys = BLUE, Girls = PINK)

## ---- instrument / subscale metadata ----------------------------------------
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

## ---- data + slices ----------------------------------------------------------
.fc_dat <- build_resample_data("SEM_output/data_with_iq", iq_policy = "all")
.fc_dat <- .fc_dat[.fc_dat$sex %in% c("Male","Female"), ]

fc_slices <- function() list(
  "All combined"    = .fc_dat,
  "MIND"            = dplyr::filter(.fc_dat, sample_source=="MIND"),
  "SSC"             = dplyr::filter(.fc_dat, sample_source=="SSC"),
  "MIND preschool"  = dplyr::filter(.fc_dat, sample_source=="MIND", age_group=="Preschool"),
  "MIND school"     = dplyr::filter(.fc_dat, sample_source=="MIND", age_group=="School"),
  "SSC preschool"   = dplyr::filter(.fc_dat, sample_source=="SSC",  age_group=="Preschool"),
  "SSC school"      = dplyr::filter(.fc_dat, sample_source=="SSC",  age_group=="School"))

## descriptives (n, raw mean/sd, z mean/sd) per measure x sex for one slice df
fc_desc <- function(df, min_n = 10L) {
  out <- list()
  for (k in seq_len(nrow(imeta))) {
    m <- imeta$measure[k]; if (!(m %in% names(df))) next
    x <- suppressWarnings(as.numeric(df[[m]]))
    s <- sd(x, na.rm=TRUE); z <- if (is.na(s)||s==0) x-mean(x,na.rm=TRUE) else (x-mean(x,na.rm=TRUE))/s
    for (sx in c("Male","Female")) {
      xi <- x[df$sex==sx & !is.na(x)]; zi <- z[df$sex==sx & !is.na(z)]
      if (length(xi) < min_n) next
      out[[length(out)+1]] <- tibble(instrument=imeta$instrument[k], subscale=imeta$subscale[k],
        measure=m, sex=ifelse(sx=="Female","Girls","Boys"),
        n=length(xi), mean_raw=mean(xi), sd_raw=sd(xi), mean_z=mean(zi), sd_z=sd(zi))
    }
  }
  if (!length(out)) return(tibble())
  list_rbind(out) %>% mutate(instrument=factor(instrument,levels=INSTR_ORD),
                             subscale=factor(subscale,levels=SUB_ORD))
}

## long z-scored per-child data for one slice (for boxplots/violins)
fc_long_z <- function(df, min_n = 10L) {
  out <- list()
  for (k in seq_len(nrow(imeta))) {
    m <- imeta$measure[k]; if (!(m %in% names(df))) next
    x <- suppressWarnings(as.numeric(df[[m]]))
    if (sum(!is.na(x)) < min_n*2) next
    s <- sd(x, na.rm=TRUE); z <- if (is.na(s)||s==0) x-mean(x,na.rm=TRUE) else (x-mean(x,na.rm=TRUE))/s
    out[[length(out)+1]] <- tibble(instrument=imeta$instrument[k], subscale=imeta$subscale[k],
      sex=ifelse(df$sex=="Female","Girls","Boys"), z=z)[!is.na(z),]
  }
  list_rbind(out) %>% mutate(instrument=factor(instrument,levels=INSTR_ORD),
                             subscale=factor(subscale,levels=SUB_ORD))
}

## ---- publication theme ------------------------------------------------------
theme_einat <- function(base=10) {
  theme_bw(base_size=base) +
    theme(legend.position="top",
          plot.title=element_text(face="bold"),
          plot.subtitle=element_text(size=base*0.82, color="grey35"),
          strip.text=element_text(face="bold"),
          strip.background=element_rect(fill="grey93", color="grey70"),
          panel.spacing=unit(0.5,"lines"),
          panel.grid.minor=element_blank())
}

## safe writer: PNG (ragg) always; cairo PDF best-effort (font-embedded, viewer-safe)
fc_save <- function(plot, stem, w=12, h=8, dpi=150) {
  ggplot2::ggsave(paste0(stem,".png"), plot, width=w, height=h, dpi=dpi, device=ragg::agg_png)
  tryCatch({ grDevices::cairo_pdf(paste0(stem,".pdf"), width=w, height=h); print(plot); grDevices::dev.off() },
           error=function(e){ try(grDevices::dev.off(),silent=TRUE); message("pdf skipped ",stem,": ",conditionMessage(e)) })
  message("saved ", stem, ".png/.pdf")
}
cat("fig_common.R loaded:", length(fc_slices()), "slices,", nrow(imeta), "measures\n")
