# Sex Differences in Autism Tool Agreement

R analysis pipeline for **Paper 1 with Dr. Einat Waizbard** — examining whether
diagnostic-tool agreement (ADOS, ADI, SRS-Parent, SRS-Teacher, RBSR) differs by
sex in autistic children, across the MIND and SSC cohorts at preschool and
school-age.

The framing separates two things that "agreement" conflates:

- **LEVEL agreement** — do two instruments give the same *absolute* severity (mean gap)?
- **RANK agreement** — do they agree on *which* children are more severe (correlation)?

These are independent. The headline: for **girls**, informants (especially teachers)
rate severity systematically higher than the clinician (ADOS) — a large **LEVEL gap** —
yet the **RANK** order is preserved (often tighter than boys'). A gap that is systematic
across nearly every girl, not idiosyncratic, is the signature of **camouflage**: girls
present differently in the clinic than at home/school. Boys' means align, but their rank
agreement is lower (idiosyncratic scatter).

---

## Analytic lenses

1. **SEM covariance comparison** — multi-group lavaan (boys vs girls), full covariance
   matrices per subsample × domain (TOTAL / SOCIAL / RRB), FIML + auxiliaries; RMSEA,
   AIC/BIC, LRT χ²Δ.
2. **Fisher r-to-z** — within-domain pairwise correlation comparison between sexes,
   BH-FDR headline; dumbbell + Δr plots.
3. **Pair-level moderation regression** — M1–M4 (baseline → sex → IQ → Sex×IQ), FIML +
   auxiliaries, BH-FDR.
4. **Sex-balanced resampling** (per Emilio Ferrer) — the M:F imbalance (MIND ~2:1, SSC
   ~6:1) over-weights boys, so hold all girls fixed, draw a matched boy subsample, run all
   three lenses on that same frame, repeat, summarise by the **median**; the empirical
   effect-size CI is the primary inferential object. Between-sex mean profiles (Cohen's d)
   run on both raw and resampled data.

**Figures** (`fig_*.R` → `make_figures_bundle.R` → `Figures_for_Einat.pdf`) include the
**Bland-Altman-by-sex** plots that make the level-gap / rank-agreement dissociation visible,
in both raw and sex-balanced-resample versions.

---

## Key findings (PREVIEW — resampling at n_reps = 20; the reportable run is 1000)

**The masking gradient** (informant − clinician gap, per child, z units). Girls' mean bias
grows with how "everyday" the informant is; boys sit near zero throughout:

| girls' gap vs ADOS | Total | Social | RRB |
|---|---|---|---|
| ADI (retrospective interview) | +0.08 | +0.20 | −0.15 |
| SRS-Parent (questionnaire)    | +0.26 | +0.24 | +0.18 |
| SRS-Teacher (questionnaire)   | **+0.57** | **+0.43** | **+0.54** |

As girls-minus-boys Cohen's *d*: teacher +0.60 / +0.43 / +0.56; parent +0.24 / +0.23 / +0.16.
Teacher-vs-ADI gaps for girls are the largest in the whole set (+0.5 to +0.7). **RBSR does
not show the girls-higher pattern** — within RRB, the masking signal is SRS-specific.

**Level gap + preserved rank (the dissociation).** Within-sex ADOS↔informant correlations
(rank agreement) are generally **higher for girls than boys** (e.g. teacher-RRB .40 vs .24;
teacher-Total .32 vs .24), so the girls' gap is systematic and patterned (camouflage), not noise.

**Covariance structure differs by sex** (full-data SEM LRT): significant in every full-sample
domain (TOTAL / SOCIAL / RRB, all p < 2e-16) and across SSC; MIND TOTAL (p = .008) and SOCIAL
(p = .019) but not RRB; MIND-preschool underpowered (n.s.).

**Not a power artifact.** Balancing the M:F ratio (resampling) leaves the girls-higher story
intact — it only shifts the boys' parent-pair bias slightly by controlling cohort composition.

**Caveats for the write-up:** SRS has sex-based norms, ADOS does not (may itself drive part of
the ADOS↔SRS gap); teachers rate girls at the highest severity.

---

## Running the pipeline

`run_pipeline.R` is a **single, portable, one-shot** run:

```r
source("run_pipeline.R")     # RStudio — or:  Rscript run_pipeline.R
```

- **Portable** — self-locates the data/scripts root (`BASE_DIR`): `MIND_BASE_DIR` env var
  override → the script's own folder → known fallback (validated by the data being present).
- **Self-installing** — installs any missing R packages and *waits*; it will not run partially.
- **Order** — data prep → sex-balanced resampling → mean profiles → canonical tests
  (SEM → Fisher → moderation) → figures (raw + resampled) + bundle.
- **Cache-aware** — skips stages whose outputs are current; force any stage with its `FORCE_*`
  flag. Set `RESAMPLE_N_REPS <- 1000L` for the reportable run (multi-hour).

Requires **R 4.6+**; the first run auto-installs lavaan, tidyverse, semPlot, writexl, magick,
patchwork, ggrepel, etc.

---

## Data access

This repository contains **code only**. Raw data files are not included because they are
governed by Data Use Agreements:

- **SFARI Base / SSC** — apply at https://base.sfari.org
- **SPARK** — apply at https://sparkforautism.org/portal/page/sfari-base/
- **MIND** — internal to University of Haifa autism research lab

The pipeline expects data in this structure on the local machine (typically OneDrive-synced),
i.e. `BASE_DIR`:

```
MIND/
├── SEM_output/
│   ├── data/              ← 4 RDS files, sex-unified (Stage 2 output)
│   └── data_with_iq/      ← 4 RDS files with FSIQ merged (Stage 3 output)
├── SPARK March 2025/      ← SFARI/SPARK raw data
├── *_Boys.rds, *_Girls.rds ← sex-split RDS files (for the Fisher script)
└── (scripts, synced from this repo)
```

---

## Repository structure

```
.
├── run_pipeline.R                          ← portable one-shot orchestrator
├── resample_balance.R / resample_lenses.R  ← sex-balanced resampling engine + lens adapters
├── sex_means_profile.R                     ← between-sex mean profiles (raw + resampled)
├── fig_common.R                            ← shared data/theme/palette + resampling helpers
├── fig_bland_altman.R                      ← Bland-Altman by sex (raw + resampled)
├── fig_*.R / make_*.R                      ← figure suite → Figures_for_Einat.pdf
├── IQ_TO_HLMSEM_020526/p2_merge_iq_into_rds.R   ← Stage 3: FSIQ integration
├── SEM_output/SEM_cov_comparison.R              ← SEM covariance LRT
├── correlation_matrices/sex_plots_cohorts_einat_040326.R  ← Fisher r-to-z
├── sem_HLM_100426/section9_v2_pair_moderation.R ← pair-level moderation (M1–M4)
├── mind_filtering_*.R / ssc_filtering_*.R       ← Stage 1: source filtering
└── (analysis report HTML, CLAUDE.md / MEMORY.md handoff docs)
```

---

## Authors

- **Gal Or** — MA Psychology, University of Haifa (analysis & code)
- **Dr. Einat Waizbard** — supervisor

---

## License

Code in this repository is shared for academic transparency. Findings above are
**preliminary** (preview resampling). Please contact the authors before any reuse.

Cloned to work laptop on 2026-05-24.
Cloned to Asus laptop on 2026-06-14.
