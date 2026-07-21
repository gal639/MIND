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

## Current state (last updated 2026-07-21)

> See [`CLAUDE.md`](CLAUDE.md) for the session primer: two-computer location map,
> scientific framing, and the current work plan. This file keeps the deep log.

**Branches integrated + merged onto `main` (2026-07-21).** Both computers' work
was a clean linear chain (`main` → `resampling-fdr-effectsizes` +8 →
`figures-for-einat` +9) and is now all **merged into `main`** — the integrated
trunk to work on. The feature branches are kept as history (fully merged; safe to
delete). All pushed to `origin`.

**Run location.** Scripts are versioned in `Documents\GitHub\einat-paper\MIND`
(code only); the pipeline runs from `BASE_DIR = OneDrive\Documents\Desktop\MIND`
(data + a script copy). Sync scripts git → `BASE_DIR` before running; do not run
git in the OneDrive copy. All data RDS are present in `BASE_DIR` (verified
2026-07-21).

Branch **`resampling-fdr-effectsizes`** (off `main`) adds the **sex-balanced
resampling layer** per Emilio's review; `figures-for-einat` adds the figure suite
(`fig_*.R`, `Figures_for_Einat.pdf`). Check out `main` to roll back.

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

**Stage 6 (Einat's request, 2026-06-30 meeting):** between-subjects MEAN-level
analysis in `sex_means_profile.R` — per-measure boys-vs-girls **t-tests (Cohen's
d + p, uncorrected primary + BH-FDR)** at three levels (all / MIND,SSC / four age
subsamples), on **both** the sex-balanced resample and the raw sample, plus
**z-scored score-profile plots** (`sex_means_profiles.pdf`). Confirmed spec:
per-measure means (not paired gaps), both data sources. Key result: ADOS
(clinician) d≈0, SRS informants d≈+0.4–1.2 (girls higher) → a group-level
**ADOS–SRS gap larger for girls = the masking signature**. Reported as report §6.

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
| `sex_means_profile.R` | Stage 6: between-sex mean t-tests (Cohen's d) + z-scored profile plots |
| `make_profile_by_informant.R` | Symptom profile grouped BY INSTRUMENT (Einat) + SD-ratio variance panel + descriptives table |
| `make_reconciliation_fig.R` | Two-lens figure: LEVEL gap (Cohen's d) vs RANK agreement (Cohen's q) |
| `SEM_output/SEM_cov_comparison.R` | Canonical SEM covariance LRT (FIML) — model spec is the source of truth, mirrored in resample_lenses.R |
| `correlation_matrices/sex_plots_cohorts_einat_040326.R` | Canonical Fisher r-to-z (BH-FDR headline) |
| `sem_HLM_100426/section9_v2_pair_moderation.R` | Canonical regression M1–M4 (BH-FDR headline) |
| `IQ_TO_HLMSEM_020526/p2_merge_iq_into_rds.R` | Stage 3 IQ merge into the RDS |

---

## Next steps / open threads

Priority order per Einat's 2026-07-21 email + Zoom ("close the results, then
write"). Full detail in [`CLAUDE.md`](CLAUDE.md) §6.

- [x] Fisher FDR family bug fixed (2026-07-01); resampling draw-once (2026-07-01);
      effect sizes at every lens.
- [x] Two-computer branches integrated + merged onto `main` and pushed (2026-07-21).
- [x] **(1, TOP) Bland-Altman plots by sex — DONE 2026-07-21** (`fig_bland_altman.R`).
      Focused 6-panel ADOS↔SRS grid + all within-domain pairs per domain, each in
      **raw + sex-balanced-resample** (8 figures). Resampled uniting method: girls
      fixed, boys' bias/LoA/r = median over N draws, cloud = one shared medoid draw
      (`fc_balanced_draws` / `fc_medoid` in `fig_common.R`). Findings → CLAUDE.md §4.
- [x] **Pipeline made a portable one-shot — DONE 2026-07-21.** `run_pipeline.R`
      self-locates `BASE_DIR` (`MIND_BASE_DIR` env / script folder / fallback),
      **installs-and-waits** on missing packages (semPlot/writexl/magick/patchwork/
      ggrepel/…), reordered **resample-first**, new Stage 7 runs the fig suite
      (raw+resampled) + `make_figures_bundle.R`. Partial-Fisher stray `~/Desktop`
      path fixed. Verified end-to-end at n_reps=20 (exit 0, ~22 min). Pushed
      (6eb4255, c335857).
- [ ] **(2) Between-group vs within-child contrast** — mostly built in
      `sex_means_profile.R`; add explicit matched-pair t-tests (SRS-Total↔ADOS-Total,
      SRS-Social↔ADOS-SA, SRS-RRB↔ADOS-RRB) across the 7 slices (effect size + p, on
      the resample); frame the group mean-gap beside the within-child agreement.
- [ ] **(3) Score-profile graphs** per measure — `sex_means_profiles.pdf` +
      `make_profile_by_informant.R` already emit these; confirm/polish for Einat.
- [ ] **(4, lower) Run `n_reps = 1000`** overnight; confirm the story holds.
- [ ] **(5) Email Emilio** — resample once vs. re-resample before each test.
- [ ] Confirm **FDR scope** + **IQ metric/policy**; decide **authorship** (Gal
      1st-author 4th paper vs. Einat 1st + Gal stats/figures).
      → **2026-07-23 12:00** results-closing meeting.
- [ ] (Deferred) Manuscript prose in Emilio's order (covariance → Fisher →
      regression); code + report already emit that order.
