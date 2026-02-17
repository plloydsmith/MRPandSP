# ============================================================
# Simulation script for
# MRP vs Raking vs Sample vs Post-strat in WTP-space
#
# Entry point: loads config + functions, then runs simulation.
# Edit sim_config.R for parameters, sim_functions.R for logic.
# ============================================================

rm(list = ls(all = TRUE))

# ---- Packages ----
library(tidyverse)
library(brms)
library(cmdstanr)
library(anesrake)
library(posterior)
library(tidybayes)
library(parallel)
library(rlang)
library(data.table)
library(patchwork)

# ---- Load config and functions ----
source("code/Simulations/sim_config.R")
source("code/Simulations/sim_functions.R")

set.seed(123)

# ============================================================
# 1. BUILD POPULATION & PRECOMPILE BRMS MODELS
# ============================================================

pb <- build_population(N_total = CONFIG$N_TOTAL, K_regions = CONFIG$K_REGIONS)
pop <- pb$pop

# Precompile one BRMS model per misspecification (chains = 0 → compile only)
message("Precompiling MRP models...")

dummy_data <- expand_grid(
  age = unique(pop$age),
  income = unique(pop$income),
  education = unique(pop$education),
  care = unique(pop$care),
  region = unique(pop$region)
) |>
  mutate(
    bid = 0, yes = 1,
    income_coarse = income_to_coarse(income)
  ) |>
  sample_n(50)

priors_all <- create_priors(CONFIG$MISSPEC$full)

for (spec in names(CONFIG$MRP_MODELS)) {
  message("  Compiling: ", spec)
  brm(
    create_wtp_formula(CONFIG$MISSPEC[[spec]]),
    data = dummy_data,
    family = bernoulli(),
    prior = priors_all,
    backend = "cmdstanr",
    chains = 0,
    iter = 0,
    file = CONFIG$MRP_MODELS[[spec]]$file,
    file_refit = "on_change"
  )
}

# ============================================================
# 2. RUN SIMULATION
# ============================================================

message("Starting simulation...")

results_all <- run_all_scenarios()
