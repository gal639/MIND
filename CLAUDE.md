# CLAUDE.md — session primer

Read this first. It is the fast pick-up for a new working session on **Paper 1:
sex differences in the agreement between autism diagnostic tools** (Gal Or, with
Dr. Einat Waizbard). Deep technical decisions live in [`MEMORY.md`](MEMORY.md);
this file is the map + the current work plan.

_Last updated 2026-07-21 (Bland-Altman-by-sex figures built [raw + sex-balanced];
`run_pipeline.R` made a portable one-shot run and verified end-to-end at
n_reps=20. Earlier that day: two-computer branch integration + Einat's "close the
results" email/Zoom.)_

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

R 4.6.0 at `C:\Program Files\R\R-4.6.0\bin\Rscript.exe` (not on PATH).
`run_pipeline.R` is now a **portable, one-shot** run that does everything:

1. **Sync scripts** git → `BASE_DIR` first (the OneDrive copies can be stale).
2. **Run it:** `source("run_pipeline.R")` in RStudio, or `Rscript run_pipeline.R`
   from `BASE_DIR`. It:
   - **self-locates `BASE_DIR`** (`MIND_BASE_DIR` env → the script's folder → known
     fallback, validated by `SEM_output/data_with_iq` being present);
   - **installs any missing packages and WAITS** (install-and-wait; won't run
     partial) — lavaan, tidyverse, semPlot, writexl, magick, patchwork, ggrepel,
     ragg, tidytext, ...;
   - runs **resample-first**: data → Stage 4 resampling → Stage 5 sex-means →
     Stage 6 canonical tests (SEM → Fisher → moderation) → Stage 7 figures
     (raw + sex-balanced) + `make_figures_bundle.R` → `Figures_for_Einat.pdf`.
   - Cache-aware; force a stage with its `FORCE_*` flag.
3. **Set `RESAMPLE_N_REPS <- 1000L` for the reportable run** (currently `20L` =
   smoke test; 1000 is multi-hour — run overnight, and check with Gal first).

Verified end-to-end at n_reps=20 (exit 0, ~22 min): resampling ~931s → SEM ~185s →
Fisher ~10s → moderation ~153s → figures/bundle (18-page `Figures_for_Einat.pdf`).

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

**Bland-Altman-by-sex findings (2026-07-21, from `fig_bland_altman.R`):** the
level-gap / rank-agreement dissociation is now visible per child.
- **Masking gradient** (girls' informant−clinician gap, z units; boys ≈ 0):
  ADI +0.08/+0.20/−0.15 (Total/Social/RRB) < SRS-Parent +0.26/+0.24/+0.18 <
  **SRS-Teacher +0.57/+0.43/+0.54**. Girls−boys Cohen's *d* = teacher
  +0.60/+0.43/+0.56, parent +0.24/+0.23/+0.16. Teacher−ADI (girls) +0.5..+0.7 = the
  largest gaps in the whole set. Informants who see the child in current/naturalistic
  settings show the biggest gap: **teacher > parent-SRS > retrospective ADI interview**.
- **RBSR does NOT show girls-higher** → within RRB the masking signal is SRS-specific.
- **Rank preserved:** within-sex ADOS↔informant *r* is generally higher for girls than
  boys (teacher-RRB .40 vs .24; teacher-Total .32 vs .24) → the gap is systematic
  (camouflage), not idiosyncratic noise.
- **Fresh full-data SEM LRT:** covariance structure differs by sex in every full-sample
  domain (all p<2e-16) and across SSC; MIND TOTAL (p=.008) & SOCIAL (p=.019) but not RRB;
  MIND-preschool n.s. (underpowered).

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

**0. ✅ DONE — run location reconciled + pipeline is a portable one-shot.** Scripts
   synced git → `BASE_DIR`; data loads; `run_pipeline.R` self-locates BASE_DIR,
   installs-and-waits on missing packages, runs resample-first through figures +
   bundle. Verified end-to-end at n_reps=20 (see §3).

**1. ✅ DONE — Bland-Altman plots by sex** (`fig_bland_altman.R`). Emits the focused
   6-panel ADOS↔SRS grid AND all within-domain pairs per domain (Total/Social/RRB),
   each in **raw + sex-balanced-resample** versions (8 figures). Resampled = girls
   fixed, boys' lines = median over N draws, cloud = one shared medoid draw. Findings
   in §4. Remaining choices: bump to n_reps=1000 (only nudges the resampled boys'
   lines), and pick which of the 8 go in Einat's deck.

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
