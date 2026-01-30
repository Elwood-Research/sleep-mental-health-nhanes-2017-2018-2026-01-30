# Analysis Report (No Pandoc)

This environment does not include Pandoc, so the R Markdown report is not rendered to HTML/PDF.
To reproduce the analysis outputs (figure + derived data + results summary), run:

```bash
Rscript studies/sleep-mental-health-nhanes-2017-2018-2026-01-30/analysis/run_analysis.R
```

Outputs:
- data/derived/analytic_model.rds
- figures/sleep_hours_vs_dep10_curve.png
- results-summary.md
