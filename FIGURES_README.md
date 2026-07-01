# Figures — Sex differences in autism tool agreement

Figure set prepared for discussion with Dr. Einat Waizbard (MIND/APP + SSC, 2026-07-01).
Bundled in **`Figures_for_Einat.pdf`**; each figure also exists as a standalone PNG/PDF.
All effect sizes are sample-size-free (Cohen's *d*/*q*); uncorrected α is primary with BH-FDR beside;
scores are z-standardized within slice. Resampling preview at n_reps = 20 (rerun at 1000 for final CIs).

## The story in one line
Autism tools are known to agree only weakly (literature). Our data confirm this on **level** —
informants (especially teachers) rate girls far more severe than the clinician (ADOS) does, a
group-level gap that is largest for girls (masking). Looking *by sex* is the new lens; the level
gap is the robust finding, the rank (correlation) sex-difference is a small consistent trend.

## Figures & draft captions

**Fig 1 — Symptom profile by informant and sex (`make_profile_by_informant.R`).**
Mean standardized score per subscale, grouped by instrument (ADOS → ADI → SRS-Parent →
SRS-Teacher → RBSR) so the sex gap can be read for consistency across an instrument's
Total/Social/RRB subscales. Panel A (level): ADOS ≈ flat/overlapping (no sex difference on the
clinician), SRS uniformly girls-higher. Panel B (spread): SD ratio girls/boys ≈ 1 everywhere
**except SRS-Teacher (Total 1.30, Social 1.18)** — girls more heterogeneous, only on teacher report.
Descriptives table (n, raw mean, SD, SD ratio) on the facing page.

**Fig 2 — The masking gap (`fig_gap_density.R`).** Per-child gap = z(informant) − z(ADOS clinician);
right of 0 = informant reports more than the clinician observes. Girls' distribution sits right of
boys', largest for teacher-SRS: **gap Cohen's d = +0.60 (Total), +0.43 (Social), +0.56 (RRB)**;
SRS-Parent +0.24 / +0.23 / +0.16.

**Fig 3 — Sex mean difference (Cohen's d) with 95% CI by informant (`fig_effectsize_forest.R`).**
All / MIND / SSC. ADOS ≈ 0 (in MIND, girls **lower**, CI excludes 0); SRS-Parent girls-higher
(d ≈ 0.2–0.35) replicating in both cohorts; SRS-Teacher (SSC) large (d ≈ 0.5–0.75); RBSR ≈ 0.

**Fig 4 — Two kinds of agreement (`fig_reconciliation.R`).** LEVEL (A): clinician-anchored profile,
girls' informant scores fan up (Cohen's d). RANK (B): ADOS↔informant correlation, boys vs girls,
weak–moderate literature band, Cohen's q. Girls show a bigger level gap yet a small higher rank
agreement — the level gap is the robust axis (correlation ≠ level agreement).

**Fig 5 — Profile by cohort, MIND vs SSC (`fig_profile_cohorts.R`).** SRS-Parent girls-higher
replicates in both cohorts; SRS-Teacher (SSC only) largest; **MIND ADOS rates girls lower** than boys.

**Fig 6 — Score distributions, violin + box (`fig_boxplots.R`).** Girls' teacher-SRS shifted up and
more spread; ADOS distributions overlap. No dramatic multimodality (heterogeneity = wider spread,
not obvious discrete subgroups).

**Fig 7 — Reconciliation plane (`fig_recon_plane.R`).** LEVEL (Cohen's d) × RANK (Cohen's q) per
ADOS↔informant pair. Spread is mostly horizontal: level gaps large (teacher-SRS far right), rank
differences small — the two "agreements" are different quantities.

**Fig 8 — Robustness across age × cohort (`fig_effectsize_age.R`).** Cohen's d + CI over the four
subsamples. SRS-Parent girls-higher holds across age and cohort; MIND ADOS girls-lower at both
ages; teacher-SRS (SSC) large (wide CIs in the small SSC-preschool cell, n_girls = 46).

## Regenerate
```r
# in RStudio, working dir = repo root; needs R 4.5+, ggplot2, patchwork, gridExtra, magick, ragg
for (f in c("make_profile_by_informant.R","fig_gap_density.R","fig_effectsize_forest.R",
            "fig_reconciliation.R","fig_profile_cohorts.R","fig_boxplots.R",
            "fig_recon_plane.R","fig_effectsize_age.R")) source(f)
source("make_figures_bundle.R")   # -> Figures_for_Einat.pdf
```
Shared data/theme/palette live in `fig_common.R`. Output PDFs/PNGs are gitignored; the scripts are the source of truth.
