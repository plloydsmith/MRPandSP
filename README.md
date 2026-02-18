# MRP and Stated Preferences

This repository contains the code and analysis pipeline for applying **Multilevel Regression and Poststratification (MRP)** to stated preference valuation surveys. The project compares MRP to conventional approaches (MNL, RPL) using two empirical applications and a Monte Carlo simulation study.

## Paper & Appendices

- [**MRPandSPPaper.pdf**](MRPandSPPaper.pdf) — Main manuscript
- [**AppendixA.pdf**](AppendixA.pdf) — Appendix A: additional tables, figures, and MCMC diagnostics
- [**AppendixB_SurveySRD.pdf**](AppendixB_SurveySRD.pdf) — Appendix B: SRD survey instrument
- [**AppendixC_SurveyWetland.pdf**](AppendixC_SurveyWetland.pdf) — Appendix C: Wetland survey instrument

## Project Structure

```
MRPandSPPaper.qmd   # Main manuscript (Quarto → PDF)
AppendixA.qmd       # Appendix A: additional tables, figures, and diagnostics
code/
├── SRD/            # Saskatchewan River Delta choice experiment
├── Wetland/        # Prairie wetland single binary choice
└── Simulations/    # Monte Carlo simulation study
data/
├── raw/            # Original survey and census data
└── derived/        # Cleaned and prepared datasets
models/             # Fitted brms model objects (.rds)
output/             # Imputed WTP draws and test results
figs/               # Generated figures
tables/             # Model estimate and diagnostic tables
```

## Requirements

- **R 4.5+**
- Key packages: `brms`, `cmdstanr`, `tidyverse`, `tidybayes`, `patchwork`, `ggrepel`, `janitor`, `kableExtra`, `modelsummary`
- Wetland analysis additionally uses: `cansim`
- Simulation study additionally uses: `parallel`, `data.table`, `anesrake`, `posterior`

## Data Setup

### 2021 Census PUMF (Individual File)

The poststratification tables are built from the 2021 Census Public Use Microdata File (PUMF) — Individual File. To obtain it:

1. Go to the [Statistics Canada PUMF page](https://www150.statcan.gc.ca/n1/en/catalogue/98M0001X).
2. Select the **2021 Census — Individual File** and follow the download/order instructions. You may need to create a Statistics Canada account.
3. Extract the downloaded archive. The key file is `data_donnees_2021_ind_v2.csv`.
4. Place `data_donnees_2021_ind_v2.csv` in the `data/raw/` folder.

The `1CreatePostStratTable.R` scripts in both the SRD and Wetland pipelines read from `data/raw/data_donnees_2021_ind_v2.csv`.

## Running the Analyses

Each analysis has a master script (`RunAll.R` or `1CreateSimulations.R`) that runs the full pipeline from data preparation through to figures and tables. All scripts assume the working directory is the project root.

---

### SRD Choice Experiment (`code/SRD/`)

Discrete choice experiment for Saskatchewan River Delta ecosystem services (sturgeon, waterfowl, muskrat, habitat). Estimates WTP in preference-space using brms nonlinear models.

**Run:** `source("code/SRD/RunAll.R")`

| Script | Description |
|--------|-------------|
| `1CreatePostStratTable.R` | Builds poststratification table from 2021 Census PUMF |
| `2SampleComparison.R` | Compares sample demographics to population |
| `3ModifyChoiceData.R` | Normalises cost/attribute columns and prepares analysis dataset |
| `4EstimateModels.R` | Compiles and estimates MNL, RPL, and MRP models via brms |
| `5ImputeWTP.R` | Extracts posterior WTP draws for each model type |
| `6GraphWTP.R` | Produces figures: mean WTP, per-attribute, distributions, breakdowns |
| `7PoeTest.R` | Poe et al. (2005) combinatorial WTP comparison tests |
| `8CreateTables.R` | Model estimate and MCMC diagnostic tables |

**Models:**
- **MNL** — Fixed-effects multinomial logit (WTP-space)
- **MRP** — Multilevel model with demographic random effects (province, age, income, education)
- **RPL** — Random parameters logit with individual-level random effects

---

### Wetland Single Binary Choice (`code/Wetland/`)

Single-bounded dichotomous choice for a prairie wetland restoration program across Alberta, Saskatchewan, and Manitoba.

**Run:** `source("code/Wetland/RunAll.R")`

| Script | Description |
|--------|-------------|
| `1CreatePostStratTable.R` | Builds poststratification table from 2021 Census PUMF |
| `2SampleComparison.R` | Compares sample demographics to population (by province) |
| `3EstimateModels.R` | Compiles and estimates logit and MRP models across provinces/samples |
| `4ImputeWTP.R` | Extracts posterior WTP draws from fitted models |
| `5GraphWTP.R` | Province comparison and restriction comparison figures |
| `6PoeTest.R` | Poe et al. (2005) combinatorial WTP comparison tests |
| `7CreateTables.R` | Model estimate and MCMC diagnostic tables |

---

### Monte Carlo Simulation (`code/Simulations/`)

Compares MRP to conventional approaches (unweighted sample mean, raking, post-stratification) for recovering population WTP under varying sample sizes, sampling bias, and model misspecification.

**Run:** `source("code/Simulations/1CreateSimulations.R")`

| File | Description |
|------|-------------|
| `sim_config.R` | Central configuration: population parameters, sample sizes, bias strengths, MCMC settings |
| `sim_functions.R` | Population generation, biased sampling, bootstrap MLE, MRP estimation, orchestration |
| `sim_plot_helpers.R` | MCSE functions, ggplot theme/colours, and plot helpers |
| `1CreateSimulations.R` | Entry point: loads config, precompiles brms models, runs all scenarios |
| `2SummariseSims.R` | Loads results, computes MCSE summaries, produces simulation figures |

**Methods compared:**
- **Unweighted** — Simple sample mean
- **Weighted** — Raking-weighted estimate (via `anesrake`)
- **Post-stratification** — Cell-based post-stratification
- **MRP** — Bayesian multilevel regression and poststratification

**Model specifications:**
- **Full** — All demographic variables (age, income, education, care)
- **Omit** — Drops care and age
- **Coarse** — Coarsens income to Low/Mid/High
