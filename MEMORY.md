# Project memory — MIND / SSC sex-differences-in-tool-agreement (Paper 1, w/ Einat Waizbard)

Pick-up notes so the project can resume on any machine. Update this file as the
work moves.

---

## What this project is

Three-lens analysis of **sex differences in the agreement between autism
diagnostic tools**, across two cohorts — **MIND** (a.k.a. APP, ~2:1 M:F) and
**SSC** (a.k.a. Simons, ~6:1 M:F) — each split into preschool and school-age
(four subsamples). The three lenses:

1. **Fisher r-to-z** — within-domain pair correlations, boys vs girls.
2. **SEM / LRT** — two-group covariance structure, free vs constrained, FIML for missingness.
3. **Hierarchical regression (M1–M4)** — ADOS regressed on tool + IQ + sex×tool; moderation term is β₄.

Domains: **TOTAL** (ADOS_Severity), **SOCIAL** (ADOS_SA), **RRB** (ADOS_RRB),
each predicted by ADI / SRS-parent / SRS-teacher (teacher only in SSC) / RBSR.

---

## Current state (last updated 2026-07-01)

Branch **`resampling-fdr-effectsizes`** (off `main`) adds a **sex-balanced
resampling layer** per the statistical consultant's review. `main` is the
pre-resampling baseline — check out `main` to roll back.

### 2026-07-01 revision — Emilio's two flagged problems fixed
Re-run end-to-end on the Asus laptop (R 4.5.2), n_reps=20. Both issues from the
**2026-06-09 review meeting** (Emilio Ferrer, statistical consultant; transcript
in Downloads) are resolved:
1. **Fisher FDR family bug.** BH-FDR was applied over the full variable
   combinatorics per cohort (~45 pairs incl. cross-domain pairs never reported),
   over-correcting and washing out the signal. Now `filter(!is.na(domain))`
   restricts to the **relevant within-domain pairs (the "41")** BEFORE `p.adjust`.
   Restores the significant pair **SSC-preschool RRB ADI_RRB~RBSR_Total** (girls
   r=.74 vs boys r=.43, p=.003 → p_fdr=.032). In `sex_plots_cohorts_einat_040326.R`.
2. **Resampling now draws once, then tests.** New `resample_all_lenses()` draws
   ONE matched boy subsample per replicate and runs all three lenses on that same
   frame ("resample before any test starts"), replacing the per-lens driver.
   Verified numerically identical to the old seed-aligned output (135/135 terms,
   max Δ=0). Also coerces lavaan-typed SEM outputs to plain numeric so the lenses
   row-bind.
Effect sizes now reported at **every** lens (Fisher Cohen's q / Δr, SEM ΔCFI,
regression Cohen's f² / ΔR²), per Emilio. Report generator `make_report.R` →
`analysis_report_2026-07-01_v2.html` (manuscript order: covariance → Fisher →
regression).

Implemented + verified end-to-end (n_reps=20, real data, 0 failed reps,
non-NA effect size at every lens, girls-higher direction holds):

- **`resample_balance.R`** — generic resampling engine (hold girls fixed, draw a
  matched boy subsample, summarise replicate distribution by the **median**;
  empirical 2.5/97.5 effect-size CI is the **primary inferential object**).
- **`resample_lenses.R`** — contract adapters for the three lenses. **Mirrors the
  canonical SEM/regression model spec + FIML estimator VERBATIM** (does not source
  the canonical scripts, which run their full analysis + load semPlot/writexl at
  source-time). Effect sizes: Fisher → Cohen's q, SEM → ΔCFI, regression →
  Cohen's f² for the sex×tool term.
- **`run_pipeline.R`** — **Stage 5** runs the resampling and writes
  `SEM_output/resampled_lens_results.csv` / `.rds` **beside** existing outputs.
- **Bonferroni → BH-FDR** is now the headline correction in the Fisher and
  regression canonical scripts (Bonferroni columns kept for reference).

### Locked decisions (do not re-litigate)
- Imbalance handled by **resampling, not imputation**. Summarise by **median**.
- **Effect-size empirical CI is primary**; median-p / prop-sig are descriptive only.
- Correction = **Benjamini-Hochberg FDR** (not Bonferroni). Keep raw results.
- Fisher FDR family = the **relevant within-domain pairs only** (the "41"), never
  the full cross-domain combinatorics. Scope (within_subsample vs within_domain)
  still open, but the family excludes unreported pairs either way.
- Resampling = **draw once per replicate, then run all three lenses** on that same
  balanced frame (not one draw per lens).
- **FIML stays** (no listwise). LRT covariances are **joint, not partial**.
- `n_reps = 1000` for the reportable run; small only for smoke tests.

### Surfaced, NOT decided — needs Gal + Einat (named parameters, nothing hardcoded)
- **Correction scope/family**: `RESAMPLE_FDR_SCOPE` in run_pipeline.R, mirrored as
  `FISHER_CORRECTION_SCOPE` / `CORRECTION_SCOPE` in the canonical scripts —
  `within_subsample` (default) | `within_domain` | `whole_table`.
- **IQ metric for MIND Preschool** (ratio vs deviation IQ unresolved, 374/375 MSEL
  ratio): `RESAMPLE_IQ_VAR` (default `"FSIQ"`) + `IQ_RATIO_POLICY` row filter.

---

## How to run

1. Open `MIND.Rproj` (or set WD), then edit `run_pipeline.R` **CONFIG** block:
   - `BASE_DIR` — point at the machine's data root (see "Repo vs data" below).
   - `RESAMPLE_N_REPS <- 1000` for the real run (20 for a smoke test).
   - Confirm `RESAMPLE_FDR_SCOPE`, `RESAMPLE_IQ_VAR`, `IQ_RATIO_POLICY`.
2. `source("run_pipeline.R")`. Stages cache on disk; set `FORCE_STAGE5_RESAMPLE <- TRUE`
   to force a re-run. Output: `SEM_output/resampled_lens_results.csv`.
3. Runtime: ~5 min at n_reps=20; **multi-hour at n_reps=1000** (the regression lens
   fits ~40 lavaan FIML models per replicate).

### Repo vs data layout (IMPORTANT — differs per machine)
- This **git repo holds the scripts only**. On Gal's original PC the **data + a
  synced copy of the scripts** lived at `BASE_DIR =
  C:/Users/.../OneDrive/Documents/Desktop/MIND`, and `run_pipeline.R` sets WD there
  and sources everything relative to it.
- On a new PC: set `BASE_DIR` to wherever the data RDS live, and **copy
  `resample_balance.R` + `resample_lenses.R` to `BASE_DIR` root** alongside the
  other sourced scripts (Stage 5 sources them from `BASE_DIR`).
- Data needed under `BASE_DIR`:
  - `SEM_output/data_with_iq/SEM_{MIND,SSC}_{preschool,schoolage}.rds` (IQ-augmented;
    sex column = `sex`, levels `"Male"`/`"Female"`; clean IQ cols are unsuffixed
    `FSIQ`/`IQ_instrument`/`IQ_source` — the merge left dup `.x/.y` columns that get
    skipped).
  - Eight sex-split correlation-matrix RDS at `BASE_DIR` root feed the Fisher script.

### R environment
- Built/verified on **R 4.6.0**. Required packages: `lavaan` (+ pbivnorm, numDeriv,
  quadprog), dplyr, purrr, tibble, tidyr, withr, tidyverse, tidytext, pacman.
- The canonical SEM/regression scripts additionally need **semPlot** and **writexl**
  for their plot/Excel outputs — install these if running those stages
  (`install.packages(c("semPlot","writexl"))`). The resampling layer itself only
  needs lavaan.

---

## Files map

| File | Role |
|------|------|
| `run_pipeline.R` | Orchestrator; caching; Stage 5 = resampling |
| `resample_balance.R` | Resampling engine (`resample_balanced` + `resample_all_lenses` draw-once) + aggregation + `add_fdr` |
| `resample_lenses.R` | Lens adapters + data loader + draw-once driver + scope-aware FDR |
| `make_report.R` | Generates `analysis_report_2026-07-01_v2.html` from the pipeline outputs |
| `SEM_output/SEM_cov_comparison.R` | Canonical SEM covariance LRT (FIML) — model spec is the source of truth, mirrored in resample_lenses.R |
| `correlation_matrices/sex_plots_cohorts_einat_040326.R` | Canonical Fisher r-to-z (BH-FDR headline) |
| `sem_HLM_100426/section9_v2_pair_moderation.R` | Canonical regression M1–M4 (BH-FDR headline) |
| `IQ_TO_HLMSEM_020526/p2_merge_iq_into_rds.R` | Stage 3 IQ merge into the RDS |

---

## Next steps / open threads

- [x] **Fisher FDR family bug fixed** (2026-07-01) — corrected to within-domain pairs.
- [x] **Resampling refactored to draw-once-then-test** (2026-07-01) — verified identical.
- [x] **Effect sizes at every lens** added (Fisher / SEM / regression).
- [ ] Gal + Einat confirm **FDR correction scope** and **IQ metric/policy**, then
      set the params and run `n_reps = 1000` (reportable run; ~11 min/stage at 1000).
- [ ] Decide whether to merge `resampling-fdr-effectsizes` into `main`.
- [ ] (Lower priority) Write the manuscript prose in Emilio's order (covariance →
      Fisher → regression). Code + report already emit that order; canonical text
      dumps are not reordered.
