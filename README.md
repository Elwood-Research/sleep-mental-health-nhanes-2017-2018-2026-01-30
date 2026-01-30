# Sleep Duration and Depressive Symptoms (NHANES 2017-2018)

This repository contains a reproducible cross-sectional analysis of the association between usual weekday/workday sleep duration and clinically relevant depressive symptoms (PHQ-9 >= 10) among U.S. adults using NHANES 2017-2018.

## Key outputs

- Results summary: `results-summary.md`
- Primary figure: `figures/sleep_hours_vs_dep10_curve.png`
- Quarto analysis report (PDF/HTML): `analysis/analysis.pdf`, `analysis/analysis.html`
- Manuscript (LaTeX/PDF): `manuscript/manuscript.tex`, `manuscript/manuscript.pdf`

## Reproduce

1) Generate analysis outputs (downloads NHANES XPT files and caches them):

```bash
Rscript analysis/run_analysis.R
```

2) Render the Quarto report (PDF + HTML):

```bash
../tools/quarto-1.8.27/bin/quarto render analysis/analysis.qmd
```

3) Build the manuscript PDF:

```bash
latexmk -pdf manuscript/manuscript.tex
```

## Data

Raw NHANES XPT files are downloaded from CDC NHANES Public DataFiles and stored under `data/raw/`.
