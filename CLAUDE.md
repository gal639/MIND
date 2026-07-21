# CLAUDE.md — session primer

Read this first. It is the fast pick-up for a new working session on **Paper 1:
sex differences in the agreement between autism diagnostic tools** (Gal Or, with
Dr. Einat Waizbard). Deep technical decisions live in [`MEMORY.md`](MEMORY.md);
this file is the map + the current work plan.

_Last updated 2026-07-21 (after integrating the two-computer branches and Einat's
"close the results" email + Zoom)._

---

## 1. Where everything lives (the two-computer map)

| Thing | Location | Role |
|-------|----------|------|
| **Git repo (scripts, source of truth)** | `C:\Users\galor\Documents\GitHub\einat-paper\MIND` | Edit + version scripts HERE. Code only, no data. |
| **Data + run sandbox = `BASE_DIR`** | `C:\Users\galor\OneDrive\Documents\Desktop\MIND` | The pipeline `setwd()`s here and runs HERE. Holds the RDS data. |
| **Remote** | `github.com/gal639/MIND` (HTTPS, no `gh` CLI installed) | Sync point between computers. |

**The seam:** `run_pipeline.R` sets `BASE_DIR` to the OneDrive folder and sources
every stage script *relative to it*. The newest scripts are in the git repo; the
OneDrive folder has an older copy. **Before running anything, sync the scripts
git → `BASE_DIR`** (git repo is source of truth; treat the OneDrive script copies
as disposable). The data (`*.rds`) is git-ignored and lives only in `BASE_DIR`;
it is all present on this machine (verified 2026-07-21).

> Do NOT run git operations inside the OneDrive copy — its `.git` is stale
> (main-only, behind) and OneDrive sync can corrupt `.git`. Git work happens in
> the `Documents\GitHub` repo only.

---

## 2. Branches (integrated + merged onto `main`, 2026-07-21)

The history was a clean linear chain (no divergence); **everything is now merged
into `main`**, which is the integrated trunk to work on:

```
main  ── EVERYTHING: resampling + BH-FDR + effect sizes + Stage 6 means +
         the fig_*.R suite + Figures_for_Einat.pdf + this primer
         (ancestors: resampling-fdr-effectsizes b54ca1c ⊂ figures-for-einat eb549a2)
```

- **Work on `main`.** It contains all of both computers' work plus the figure
  suite and this primer.
- The feature branches `resampling-fdr-effectsizes` and `figures-for-einat` are
  kept as history (fully merged into `main`; safe to delete whenever).
- Optional: cut a fresh `results-closing` branch off `main` for the Bland-Altman
  + remaining work.

---

## 3. How to run

R 4.6.0. Packages: `lavaan` (+ pbivnorm, numDeriv, quadprog), tidyverse family
(dplyr/purrr/tibble/tidyr/ggplot2), withr, ragg; `semPlot` + `writexl` only for
the canonical SEM/regression plot/Excel outputs.

1. **Sync scripts** into `BASE_DIR` (copy changed/new `.R` from the git repo).
2. **Analysis (Stages 2–6):** open `BASE_DIR` in RStudio → edit `run_pipeline.R`
   CONFIG → `source("run_pipeline.R")`. Cache-aware; set `FORCE_STAGE*<-TRUE` to
   re-run. **Set `RESAMPLE_N_REPS <- 1000L` for the reportable run** (currently
   `20L` = smoke test; 1000 is multi-hour — run overnight).
3. **Figures:** the `fig_*.R` / `make_*.R` scripts run individually from
   `BASE_DIR` (each `setwd()`s to it, sources `fig_common.R`, writes PNG+PDF via
   `fc_save()`), then `make_figures_bundle.R` assembles `Figures_for_Einat.pdf`.

---

## 4. Study design + current results (one screen)

Three triangulating **agreement** lenses on *does autism-tool agreement differ by
sex*, across **MIND** (~2:1 M:F) and **SSC** (~6:1 M:F) cohorts, each split
preschool / school-age (four subsamples). Domains **TOTAL / SOCIAL / RRB**;
instruments **ADOS** (clinician), **ADI**, **SRS-Parent**, **SRS-Teacher** (SSC
only), **RBSR**.

1. **Fisher r-to-z** — within-domain pair correlations, boys vs girls.
2. **SEM / LRT** — two-group covariance structure, FIML.
3. **Hierarchical regression (M1–M4)** — ADOS ~ tool + IQ + sex×tool moderation.
- **Stage 5 resampling** balances M:F (hold girls fixed, draw matched boys,
  median summary, empirical effect-size CI is the primary call). **BH-FDR** is
  the headline correction. Effect sizes: Fisher→Cohen's q, SEM→ΔCFI, reg→f².
- **Stage 6 (`sex_means_profile.R`)** — between-group mean comparison (girls−boys
  Cohen's d + Welch t) at 3 levels, raw + resampled, plus z-scored profile plots.

**Results status (holds after FDR + resampling, n_reps=20 preview):** TOTAL and
SOCIAL differ by sex; SSC school-age & preschool carry it; the girls-higher
direction is stable. Fisher pairs show the pattern but mostly don't survive FDR.
Sex×IQ regression: SRS-Social-Parent and SRS-Total-Parent survive FDR. Effect
sizes are modest (a few ~0.5 near-significant, many 0.1–0.2). Balancing the M:F
ratio did **not** change the story → it's the result, not a power artifact.

---

## 5. The scientific framing (Einat's synthesis — the paper's thesis)

Separate two things "agreement" conflates:
- **LEVEL agreement** — do instruments give the same *absolute* score (mean gap).
- **RANK/relative agreement** — do they agree on *which* children are more severe
  (correlation).

These are independent. A **systematic** offset (same direction/size for nearly
every child) creates a **mean gap but preserves correlation** (Bland-Altman).
**Idiosyncratic** child-to-child disagreement destroys correlation without a
group offset.

- **Girls:** large, *systematic* ADOS↔SRS gap → mean-level difference **and**
  preserved rank order (high within-child agreement). This is **camouflage** —
  context-dependent presentation (masked in the clinic, not at home/school),
  patterned across nearly all girls.
- **Boys:** means align, but *idiosyncratic* scatter → lower correlation.

**The contribution:** prior camouflage work showed group-mean differences; we add
that the tool gap is **systematic within essentially every girl** (not boys),
across multiple instruments — a within-child, clinically-meaningful result, not
just a group-mean one. Watch two caveats in the write-up: **SRS has sex-based
norms, ADOS doesn't** (may itself drive tool differences); and **teachers rate
girls at the highest severity** (often the most informative informant).

---

## 6. Work plan — "close the results, then write" (priority order per Einat)

**0. Reconcile the run location (do first).** Sync the integrated scripts into
   `BASE_DIR` so the pipeline + figure scripts run the newest code (see §1/§3).
   Confirm data loads (`build_resample_data("SEM_output/data_with_iq")`).

**1. Bland-Altman plots by sex — TOP priority (Einat: more important than
   resampling).** New `fig_bland_altman.R`. For each ADOS↔SRS pair, per child:
   x = mean(z(ADOS), z(informant)), y = z(informant) − z(ADOS); facet/colour by
   sex; add per-sex mean-bias line + ±1.96 SD limits of agreement. This makes the
   *systematic gap + preserved correlation* dissociation visible in one figure.
   Reuse `fig_common.R` (loader, `SEXCOL`, `theme_einat`, `fc_save`) and the
   pair table from `fig_gap_density.R` (which already computes the y-axis
   difference by sex with Cohen's d — the Bland-Altman precursor).

**2. Between-group vs within-child contrast (Einat's request).** Largely built in
   `sex_means_profile.R` (3 levels/7 slices, raw + resampled, t + Cohen's d,
   uncorrected alpha primary). Finish the framing: put the **between-group mean
   gap** beside the **within-child agreement** (Fisher/SEM) so girls read as
   "bigger group gap yet higher within-child agreement," boys the reverse. Run
   the matched-pair t-tests explicitly (SRS-Total↔ADOS-Total, SRS-Social↔ADOS-SA,
   SRS-RRB↔ADOS-RRB) across the 7 slices, with effect sizes + p, on the resample.

**3. Score-profile graphs per measure** — `sex_means_profile.R` already emits
   `sex_means_profiles.pdf` (boys vs girls z-profile per slice). Polish/confirm
   these are the versions Einat wants for each measure.

**4. Bump resampling `n_reps` 20 → 1000** and run overnight (lower priority —
   Einat: "resampling less significant"). Confirm the story holds.

**5. Email Emilio** — resample **once** vs. **re-resample before each test**
   (Gal expects "once"; Einat deferred to Emilio, who was in Japan, back soon).

**6. Then close results → start writing** at the 2026-07-23 meeting.

---

## 7. Open decisions (for the 2026-07-23 12:00 meeting — "results-closing")

- **FDR correction scope:** `RESAMPLE_FDR_SCOPE` — `within_subsample` (default) |
  `within_domain` | `whole_table`. (Mirrored in the canonical scripts.)
- **IQ metric for MIND Preschool:** ratio vs deviation IQ (374/375 MSEL ratio) —
  `RESAMPLE_IQ_VAR` / `IQ_RATIO_POLICY`.
- ~~Merge the feature branches into `main`?~~ **Done 2026-07-21** — everything is
  on `main`.
- **Authorship** (Einat offered a choice): (1) **Gal first author**, writes it in
  the gap year — a 4th paper, *not* one of the 3 doctorate papers; vs (2) **Einat
  first author**, Gal second providing stats/figures only, freeing Gal for the
  thesis + the new grant project. Decide at the meeting.

---

## 8. People & context

- **Dr. Einat Waizbard** — supervisor / PI; future doctoral advisor (Univ. of
  Haifa, developmental).
- **Emilio Ferrer** — statistical consultant (resampling, FDR, and the
  longitudinal / latent-change-score methods).
- **Anat Zaidman-Zait** — Tel Aviv co-PI on the *future* grant project (below).

**Related future project (separate repo/work, not Paper 1):** a small grant with
Anat Zaidman-Zait — symptom **trajectories** in the Israeli autism database,
social-communication vs RRB domains, using **Latent Change Score / Bivariate Dual
Change Score** and **Growth-Mixture** models (Emilio consulting). Intended to
become Gal's first *doctorate* paper in the gap year (2026→2027). Mentioned here
only so the July 23 agenda and gap-year scope aren't lost; it does not touch this
repo yet.
