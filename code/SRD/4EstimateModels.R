# =============================================================================
# Compile and Estimate SRD Choice Experiment Models
# Compiles brms model structures (chains = 0) then fits each model
# using update() with the choice experiment data.
#
# Models:
#   MNL — fixed-effects multinomial logit (WTP-space)
#   MRP — multilevel model with demographic random effects
#   RPL — random parameters logit (individual-level random effects)
#
# Inputs:  data/derived/SRDClean.csv
# Outputs: models/srd/wtps_srd_*.rds (compiled)
#          output/srd/wtps_srd_*.rds  (estimated)
# =============================================================================

# --- Global parameters (if not already set by RunAll.R) -----------------------

if (!exists("n_chains")) {
  n_chains <- 4
}
if (!exists("n_iter")) {
  n_iter <- 2000
}
if (!exists("n_cores")) {
  n_cores <- 4
}
if (!exists("n_threads")) {
  n_threads <- 4
}
if (!exists("seed")) {
  seed <- 13
}

if (!exists("df_ce")) {
  df_ce <- read_csv("data/derived/SRDClean.csv")
}

# --- Shared formula components ------------------------------------------------

# Utility for alternatives 2 and 3 (WTP-space):
#   program intercept + cost * (-cost + attribute effects)
mu_formula <- paste(
  "bprogram + bcost * (-cost_{alt} +",
  "bsturgeon * sturgeon_{alt} +",
  "bhabitat * habitat_{alt} +",
  "bwaterfowl * waterfowl_{alt} +",
  "bmuskrat * muskrat_{alt})"
)
nlf_mu2 <- nlf(
  mu2 ~ bprogram +
    bcost *
      (-cost_2 +
        bsturgeon * sturgeon_2 +
        bhabitat * habitat_2 +
        bwaterfowl * waterfowl_2 +
        bmuskrat * muskrat_2)
)
nlf_mu3 <- nlf(
  mu3 ~ bprogram +
    bcost *
      (-cost_3 +
        bsturgeon * sturgeon_3 +
        bhabitat * habitat_3 +
        bwaterfowl * waterfowl_3 +
        bmuskrat * muskrat_3)
)

prior_fixed <- c(
  prior(normal(0, 1), nlpar = "bcost", lb = 0),
  prior(normal(0, 1), nlpar = "bprogram")
)

prior_attributes <- c(
  prior(normal(0, 1), nlpar = "bsturgeon"),
  prior(normal(0, 1), nlpar = "bhabitat"),
  prior(normal(0, 1), nlpar = "bwaterfowl"),
  prior(normal(0, 1), nlpar = "bmuskrat")
)

prior_sd <- c(
  prior(cauchy(0, 1), class = "sd", nlpar = "bsturgeon"),
  prior(cauchy(0, 1), class = "sd", nlpar = "bhabitat"),
  prior(cauchy(0, 1), class = "sd", nlpar = "bwaterfowl"),
  prior(cauchy(0, 1), class = "sd", nlpar = "bmuskrat")
)

# --- Compile models (chains = 0) ---------------------------------------------

n_chains_compile <- 0
n_iter_compile <- 2000
n_cores_compile <- 1

# MNL: fixed effects only
brm(
  bf(
    choice ~ 1,
    nlf_mu2,
    nlf_mu3,
    bprogram ~ 1,
    bcost ~ 1,
    bsturgeon ~ 1,
    bhabitat ~ 1,
    bwaterfowl ~ 1,
    bmuskrat ~ 1,
    family = categorical(refcat = 1)
  ),
  data = df_ce,
  prior = c(prior_fixed, prior_attributes),
  chains = n_chains_compile,
  iter = n_iter_compile,
  cores = n_cores_compile,
  threads = threading(n_threads),
  backend = "cmdstanr",
  file = "models/srd/wtps_srd_mnl.rds"
)

# MRP
brm(
  bf(
    choice ~ 1,
    nlf_mu2,
    nlf_mu3,
    bprogram ~ 1,
    bcost ~ 1,
    bsturgeon ~ 1 +
      (1 | age_gender) +
      (1 | education_group) +
      (1 | income_group) +
      (1 | province),
    bhabitat ~ 1 +
      (1 | age_gender) +
      (1 | education_group) +
      (1 | income_group) +
      (1 | province),
    bwaterfowl ~ 1 +
      (1 | age_gender) +
      (1 | education_group) +
      (1 | income_group) +
      (1 | province),
    bmuskrat ~ 1 +
      (1 | age_gender) +
      (1 | education_group) +
      (1 | income_group) +
      (1 | province),
    family = categorical(refcat = 1)
  ),
  data = df_ce,
  prior = c(prior_fixed, prior_attributes, prior_sd),
  chains = n_chains_compile,
  iter = n_iter_compile,
  cores = n_cores_compile,
  seed = seed,
  threads = threading(n_threads),
  backend = "cmdstanr",
  control = list(adapt_delta = .95),
  file = "models/srd/wtps_srd_mrp.rds"
)

# RPL: individual-level random effects
brm(
  bf(
    choice ~ 1,
    nlf_mu2,
    nlf_mu3,
    bprogram ~ 1 + (1 | id),
    bcost ~ 1,
    bsturgeon ~ 1 + (1 | id),
    bhabitat ~ 1 + (1 | id),
    bwaterfowl ~ 1 + (1 | id),
    bmuskrat ~ 1 + (1 | id),
    family = categorical(refcat = 1)
  ),
  data = df_ce,
  prior = c(prior_fixed, prior_attributes, prior_sd),
  chains = n_chains_compile,
  iter = n_iter_compile,
  cores = n_cores_compile,
  seed = seed,
  threads = threading(n_threads),
  backend = "cmdstanr",
  control = list(adapt_delta = .95),
  file = "models/srd/wtps_srd_rpl.rds"
)

# --- Estimate models ----------------------------------------------------------

models_ce <- c(
  "wtps_srd_mnl",
  "wtps_srd_mrp",
  "wtps_srd_rpl"
)

for (model_name in models_ce) {
  out_path <- paste0("output/srd/", model_name, ".rds")
  if (!file.exists(out_path)) {
    temp <- update(
      readRDS(paste0("models/srd/", model_name, ".rds")),
      newdata = df_ce,
      chains = n_chains,
      iter = n_iter,
      cores = n_cores,
      threads = threading(n_threads),
      backend = "cmdstanr"
    )
    saveRDS(temp, file = out_path)
  }
}
