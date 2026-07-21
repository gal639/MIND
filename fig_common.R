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
## ---- sex-balanced resampling for figures ------------------------------------
## Mirror of the Stage-5 engine (resample_balance.R) for the figure layer: hold
## ALL girls fixed; within each stratum (cohort x age) draw n_girls boys without
## replacement to match; combine. Stratified so the balanced sample respects the
## cohort/age composition (same granularity as the within-subsample resampling).
## Returns a list of n_reps integer vectors = row indices into `df` of the
## balanced sample (all girls + the drawn boys), reproducible from `seed`.
fc_balanced_draws <- function(df, n_reps = 20L, seed = 1L, sex_col = "sex",
                              female = "Female",
                              strata = c("sample_source", "age_group")) {
  gi   <- which(df[[sex_col]] == female)
  bi0  <- which(df[[sex_col]] != female & !is.na(df[[sex_col]]))
  key  <- if (all(strata %in% names(df))) as.character(interaction(df[strata], drop = TRUE))
          else rep("all", nrow(df))
  bo_by <- split(bi0, key[bi0])                 # boy row-indices per stratum
  ng_by <- vapply(split(gi, key[gi]), length, integer(1))  # n girls per stratum
  withr::with_seed(seed, lapply(seq_len(n_reps), function(b) {
    bi <- unlist(lapply(names(bo_by), function(k) {
      pool <- bo_by[[k]]; ng <- if (k %in% names(ng_by)) ng_by[[k]] else 0L
      if (ng <= 0L)              integer(0)
      else if (length(pool) <= ng) pool
      else                         sample(pool, ng, replace = FALSE)
    }), use.names = FALSE)
    sort(c(gi, bi))
  }))
}

## Medoid row of a draws-by-statistic matrix: the draw whose statistic vector is
## closest (L2) to the column-wise median across draws. Used to pick the single
## most-representative balanced draw for a figure's point cloud (its stats match
## the median lines by construction), so the cloud is not an arbitrary draw.
fc_medoid <- function(M) {
  M   <- as.matrix(M)
  med <- apply(M, 2, stats::median, na.rm = TRUE)
  d   <- apply(M, 1, function(r) sqrt(sum((r - med)^2, na.rm = TRUE)))
  which.min(d)
}

cat("fig_common.R loaded:", length(fc_slices()), "slices,", nrow(imeta), "measures\n")
