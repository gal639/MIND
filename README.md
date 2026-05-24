# Sex Differences in Autism Tool Agreement

R analysis pipeline for **Paper 1 with Dr. Einat Waizbard** — examining whether
diagnostic-tool agreement (ADOS, ADI, SRS, RBSR) differs by sex in autistic
children, across the MIND and SSC cohorts at preschool and school-age.

---

## Overview

Three triangulating analytic lenses:

1. **SEM covariance comparison** — multi-group lavaan, boys' vs girls' full
   covariance matrices per subsample × domain (TOTAL / SOCIAL / RRB). FIML +
   auxiliaries. Reports RMSEA, AIC/BIC, LRT χ² Δ.
2. **Fisher r-to-z** — bivariate pairwise correlation comparison between sexes,
   dumbbell + Δr plots by domain.
3. **Pair-level moderation regression** — M1 baseline + M2 sex + M3 IQ +
   M4 joint Sex×IQ. FIML + auxiliaries, Bonferroni & FDR corrections.

---

## Data access

This repository contains **code only**. Raw data files are not included
because they are governed by Data Use Agreements:

- **SFARI Base / SSC** — apply at https://base.sfari.org
- **SPARK** — apply at https://sparkforautism.org/portal/page/sfari-base/
- **MIND** — internal to University of Haifa autism research lab

The pipeline expects data in the following structure on the local machine
(typically synced via OneDrive):

```
MIND/
├── SEM_output/
│   ├── data/              ← 4 RDS files, sex-unified (Stage 2 output)
│   └── data_with_iq/      ← 4 RDS files with FSIQ merged (Stage 3 output)
├── SPARK March 2025/      ← SFARI/SPARK raw data
├── *_Boys.rds, *_Girls.rds ← sex-split RDS files (for Fisher script)
└── (scripts at root)
```

---

## Running the pipeline

```r
# In RStudio, with working directory = repo root:
source("run_pipeline.R")
```

The orchestrator is **cache-aware**: it skips stages whose outputs already
exist and are newer than their inputs. To force a re-run of any stage:

```r
FORCE_STAGE4_MODER <- TRUE  # before source()
```

See `run_pipeline.R` for full configuration.

---

## Repository structure

```
.
├── run_pipeline.R                          ← master orchestrator
├── mind_filtering_clean_*.R                ← Stage 1: MIND source filtering
├── ssc_filtering_*.R                       ← Stage 1: SSC source filtering
├── IQ_TO_HLMSEM_020526/
│   └── p2_merge_iq_into_rds.R              ← Stage 3: FSIQ integration
├── SEM_output/
│   └── SEM_cov_comparison.R                ← Stage 4a: SEM
├── correlation_matrices/
│   └── sex_plots_cohorts_einat_040326.R    ← Stage 4b: Fisher r-to-z
├── sem_HLM_100426/
│   └── section9_v2_pair_moderation.R       ← Stage 4c: pair moderation
└── (analysis report HTML, handoff docs, etc.)
```

---

## Authors

- **Gal Or** — MA Psychology, University of Haifa (analysis & code)
- **Dr. Einat Waizbard** — supervisor

---

## License

Code in this repository is shared for academic transparency. Please contact
the authors before any reuse.

Cloned to work laptop on 2026-05-24.

