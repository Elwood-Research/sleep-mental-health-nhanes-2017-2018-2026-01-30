## Study Title
Sleep Duration and Depressive Symptoms in U.S. Adults: NHANES 2017–2018

## Primary Question
Among U.S. adults (18+), what is the association between self-reported usual sleep duration and depressive symptoms (PHQ-9) in NHANES 2017–2018?

## Data Source
National Health and Nutrition Examination Survey (NHANES), 2017–2018 cycle.

## Exposure
Usual sleep duration on workdays/weekdays (hours), from Sleep Disorders Questionnaire (SLQ).

## Outcome
Clinically relevant depressive symptoms defined as PHQ-9 score >= 10, from Depression Screener (DPQ).

## Analysis Plan (High Level)
- Download public NHANES XPT files for DEMO, SLQ, DPQ (and selected covariate files) directly from CDC.
- Merge by respondent sequence number (SEQN).
- Use NHANES interview weights and design variables to fit survey-weighted models.
- Primary model: survey-weighted logistic regression with sleep hours modeled flexibly (natural spline), adjusted for core sociodemographics.
- Secondary analyses: sleep categories (<6, 6-<7, 7-<9 [ref], >=9 hours); descriptive weighted prevalence by sleep category.

## Notes
The user requested the `nhanesA` R package; in this environment it cannot be installed due to missing system libraries (libcurl, openssl, libxml2, fontconfig/freetype). The workflow instead uses direct, reproducible downloads of the official NHANES XPT files via base R.
