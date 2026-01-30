## Technical
- Language: R (R 4.3+)
- Key packages: haven, dplyr, tidyr, ggplot2, survey, broom, rmarkdown, knitr
- Data acquisition: download NHANES XPT files from `https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/`.

## Reproducibility
- Cache downloaded XPT files under `studies/.../data/raw/`.
- No manual steps required beyond running the render command.

## Outputs
- Analysis report: `studies/.../analysis/analysis.html`
- Figures: `studies/.../figures/*.png`
- Results summary: `studies/.../results-summary.md`
