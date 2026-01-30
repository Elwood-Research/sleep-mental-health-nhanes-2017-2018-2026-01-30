#!/usr/bin/env Rscript

.libPaths(unique(c(Sys.getenv("R_LIBS_USER"), .libPaths())))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(haven)
  library(survey)
  library(splines)
  library(broom)
})

options(survey.lonely.psu = "adjust")

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
analysis_dir <- if (length(file_arg) > 0) dirname(normalizePath(file_arg[1])) else getwd()

study_dir <- normalizePath(file.path(analysis_dir, ".."), winslash = "/", mustWork = TRUE)
raw_dir <- file.path(study_dir, "data", "raw")
derived_dir <- file.path(study_dir, "data", "derived")
fig_dir <- file.path(study_dir, "figures")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

nhanes_cycle <- "2017-2018"
nhanes_public_year <- 2017
nhanes_base_url <- paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/", nhanes_public_year, "/DataFiles")

download_xpt <- function(file_stem) {
  dest <- file.path(raw_dir, paste0(file_stem, ".XPT"))
  url <- paste0(nhanes_base_url, "/", file_stem, ".XPT")

  needs_download <- !file.exists(dest)
  if (!needs_download) {
    first_line <- tryCatch(readLines(dest, n = 1, warn = FALSE), error = function(e) "")
    if (length(first_line) > 0 && grepl("<!DOCTYPE html>|<html", first_line[1], ignore.case = TRUE)) {
      needs_download <- TRUE
    }
  }

  if (needs_download) {
    download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
  }
  dest
}

read_nhanes <- function(file_stem) {
  path <- download_xpt(file_stem)
  haven::read_xpt(path)
}

as_na_nhanes <- function(x) {
  if (is.numeric(x)) {
    x[x %in% c(7, 9, 77, 99, 777, 999)] <- NA
  }
  x
}

# Data
demo <- read_nhanes("DEMO_J")
slq <- read_nhanes("SLQ_J")
dpq <- read_nhanes("DPQ_J")
bmx <- read_nhanes("BMX_J")
smq <- read_nhanes("SMQ_J")

# Derivations
dpq_items <- c("DPQ010","DPQ020","DPQ030","DPQ040","DPQ050","DPQ060","DPQ070","DPQ080","DPQ090")

dpq2 <- dpq %>%
  select(SEQN, all_of(dpq_items)) %>%
  mutate(across(all_of(dpq_items), as_na_nhanes)) %>%
  mutate(
    phq9 = rowSums(across(all_of(dpq_items)), na.rm = FALSE),
    dep10 = if_else(!is.na(phq9) & phq9 >= 10, 1, if_else(!is.na(phq9), 0, NA_real_))
  )

slq2 <- slq %>%
  select(SEQN, SLD012) %>%
  mutate(
    sleep_hrs = as_na_nhanes(as.numeric(SLD012)),
    sleep_hrs = if_else(!is.na(sleep_hrs) & sleep_hrs >= 1 & sleep_hrs <= 14, sleep_hrs, NA_real_),
    sleep_cat = case_when(
      is.na(sleep_hrs) ~ NA_character_,
      sleep_hrs < 6 ~ "<6",
      sleep_hrs < 7 ~ "6-<7",
      sleep_hrs < 9 ~ "7-<9",
      TRUE ~ ">=9"
    )
  )

demo2 <- demo %>%
  select(
    SEQN,
    SDMVPSU, SDMVSTRA, WTINT2YR,
    RIDAGEYR, RIAGENDR, RIDRETH3,
    DMDEDUC2, INDFMPIR,
    RIDEXPRG
  ) %>%
  mutate(
    age = RIDAGEYR,
    sex = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female")),
    race_eth = factor(
      RIDRETH3,
      levels = c(1, 2, 3, 4, 6, 7),
      labels = c(
        "Mexican American",
        "Other Hispanic",
        "Non-Hispanic White",
        "Non-Hispanic Black",
        "Non-Hispanic Asian",
        "Other/Multiracial"
      )
    ),
    edu = factor(
      DMDEDUC2,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "<9th grade",
        "9-11th grade",
        "High school/GED",
        "Some college/AA",
        "College+"
      )
    ),
    pir = as_na_nhanes(as.numeric(INDFMPIR)),
    pregnant = if_else(RIDEXPRG == 1, 1, if_else(RIDEXPRG == 2, 0, NA_real_))
  )

bmx2 <- bmx %>%
  select(SEQN, BMXBMI) %>%
  mutate(bmi = as_na_nhanes(as.numeric(BMXBMI)))

smq2 <- smq %>%
  select(SEQN, SMQ020) %>%
  mutate(
    smq020 = as_na_nhanes(as.numeric(SMQ020)),
    smoke_ever = case_when(
      smq020 == 1 ~ "Ever",
      smq020 == 2 ~ "Never",
      TRUE ~ NA_character_
    ),
    smoke_ever = factor(smoke_ever, levels = c("Never", "Ever"))
  ) %>%
  select(-smq020)

analytic <- demo2 %>%
  left_join(slq2, by = "SEQN") %>%
  left_join(dpq2, by = "SEQN") %>%
  left_join(bmx2, by = "SEQN") %>%
  left_join(smq2, by = "SEQN") %>%
  filter(age >= 18) %>%
  filter(is.na(pregnant) | pregnant == 0)

analytic_model <- analytic %>%
  transmute(
    SEQN,
    SDMVPSU, SDMVSTRA, WTINT2YR,
    age, sex, race_eth, edu, pir,
    bmi, smoke_ever,
    sleep_hrs, sleep_cat,
    phq9, dep10
  ) %>%
  filter(
    !is.na(WTINT2YR),
    !is.na(SDMVPSU),
    !is.na(SDMVSTRA),
    !is.na(dep10),
    !is.na(sleep_hrs),
    !is.na(age),
    !is.na(sex),
    !is.na(race_eth),
    !is.na(edu),
    !is.na(pir),
    !is.na(bmi),
    !is.na(smoke_ever)
  ) %>%
  mutate(sleep_cat = factor(sleep_cat, levels = c("<6", "6-<7", "7-<9", ">=9")))

saveRDS(analytic_model, file.path(derived_dir, "analytic_model.rds"))

des <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTINT2YR,
  nest = TRUE,
  data = analytic_model
)

prev_by_cat <- svyby(~dep10, ~sleep_cat, des, svymean, na.rm = TRUE, vartype = c("se", "ci"))

fit_spline <- svyglm(
  dep10 ~ ns(sleep_hrs, df = 4) + age + sex + race_eth + edu + pir + bmi + smoke_ever,
  design = des,
  family = quasibinomial()
)

fit_cat <- svyglm(
  dep10 ~ relevel(sleep_cat, ref = "7-<9") + age + sex + race_eth + edu + pir + bmi + smoke_ever,
  design = des,
  family = quasibinomial()
)

wtd_mean <- function(v) {
  as.numeric(coef(svymean(as.formula(paste0("~", v)), des, na.rm = TRUE))[1])
}

wtd_mode <- function(v) {
  tab <- svytable(as.formula(paste0("~", v)), des)
  names(which.max(tab))
}

ref_age <- wtd_mean("age")
ref_pir <- wtd_mean("pir")
ref_bmi <- wtd_mean("bmi")
ref_sex <- wtd_mode("sex")
ref_race <- wtd_mode("race_eth")
ref_edu <- wtd_mode("edu")
ref_smoke <- wtd_mode("smoke_ever")

grid <- tibble(
  sleep_hrs = seq(4, 10, by = 0.1),
  age = ref_age,
  pir = ref_pir,
  bmi = ref_bmi,
  sex = factor(ref_sex, levels = levels(analytic_model$sex)),
  race_eth = factor(ref_race, levels = levels(analytic_model$race_eth)),
  edu = factor(ref_edu, levels = levels(analytic_model$edu)),
  smoke_ever = factor(ref_smoke, levels = levels(analytic_model$smoke_ever))
)

X <- model.matrix(delete.response(terms(fit_spline)), grid)
eta <- as.numeric(X %*% coef(fit_spline))
se_eta <- sqrt(diag(X %*% vcov(fit_spline) %*% t(X)))

grid <- grid %>%
  mutate(
    p = plogis(eta),
    p_low = plogis(eta - 1.96 * se_eta),
    p_high = plogis(eta + 1.96 * se_eta)
  )

p_curve <- ggplot(grid, aes(x = sleep_hrs, y = p)) +
  geom_ribbon(aes(ymin = p_low, ymax = p_high), alpha = 0.2) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Usual sleep duration (hours, workdays/weekdays)",
    y = "Predicted probability of PHQ-9 >= 10",
    title = "Adjusted association between sleep duration and depressive symptoms",
    subtitle = "Survey-weighted logistic regression; reference covariates set to weighted means/modes"
  ) +
  theme_minimal(base_size = 12)

fig_path <- file.path(fig_dir, "sleep_hours_vs_dep10_curve.png")
ggsave(fig_path, p_curve, width = 8, height = 5, dpi = 200)

prev_tbl <- prev_by_cat %>%
  transmute(
    sleep_cat,
    prev = dep10,
    ci_low = ci_l,
    ci_high = ci_u
  )

cat_or <- broom::tidy(fit_cat) %>%
  filter(grepl("sleep_cat", term)) %>%
  mutate(
    or = exp(estimate),
    or_low = exp(estimate - 1.96 * std.error),
    or_high = exp(estimate + 1.96 * std.error)
  )

writeLines(
  c(
    "# Results Summary",
    "",
    sprintf("Cycle: %s", nhanes_cycle),
    sprintf(
      "Analytic sample (unweighted): N = %s adults (18+), non-pregnant, complete-case for model.",
      format(nrow(analytic_model), big.mark = ",")
    ),
    "",
    "Outcome: clinically relevant depressive symptoms (PHQ-9 >= 10).",
    "Exposure: usual sleep duration on workdays/weekdays (hours) and categories (<6, 6-<7, 7-<9, >=9).",
    "",
    "Weighted prevalence of PHQ-9 >= 10 by sleep duration category:",
    paste0(
      "- ",
      prev_tbl$sleep_cat,
      ": ",
      sprintf("%.1f%%", 100 * prev_tbl$prev),
      " (95% CI ",
      sprintf("%.1f%%", 100 * prev_tbl$ci_low),
      ", ",
      sprintf("%.1f%%", 100 * prev_tbl$ci_high),
      ")"
    ),
    "",
    "Adjusted model (sleep categories; ref 7-<9):",
    paste0(
      "- ",
      cat_or$term,
      ": OR ",
      sprintf("%.2f", cat_or$or),
      " (95% CI ",
      sprintf("%.2f", cat_or$or_low),
      ", ",
      sprintf("%.2f", cat_or$or_high),
      ")"
    ),
    "",
    "Figure: figures/sleep_hours_vs_dep10_curve.png"
  ),
  file.path(study_dir, "results-summary.md")
)

writeLines(
  c(
    "# Analysis Report (No Pandoc)",
    "",
    "This environment does not include Pandoc, so the R Markdown report is not rendered to HTML/PDF.",
    "To reproduce the analysis outputs (figure + derived data + results summary), run:",
    "",
    "```bash",
    "Rscript studies/sleep-mental-health-nhanes-2017-2018-2026-01-30/analysis/run_analysis.R",
    "```",
    "",
    "Outputs:",
    "- data/derived/analytic_model.rds",
    "- figures/sleep_hours_vs_dep10_curve.png",
    "- results-summary.md"
  ),
  file.path(analysis_dir, "analysis.md")
)

cat("Done. Wrote:\n")
cat("- ", file.path(study_dir, "results-summary.md"), "\n", sep = "")
cat("- ", fig_path, "\n", sep = "")
