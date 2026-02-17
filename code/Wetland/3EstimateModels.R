# =============================================================================
# Compile and Estimate Wetland SBC Models
# First compiles model structures (chains = 0) then fits all SBC model
# variants across provinces and sample types using update().
# =============================================================================

wtp_rescale <- 100
foldername <- paste0("output/wetlands/scale", wtp_rescale, "/")

# --- Load and prepare data ----------------------------------------------------

df_wetland_full <- read_csv("data/derived/WetlandsClean.csv") |>
  mutate(bid = bid / wtp_rescale)

df_wetland_restrict <- df_wetland_full |>
  filter(conseq_none == 0, yea_say == 0)

get_sample_data <- function(sample_name) {
  switch(sample_name, full = df_wetland_full, restrict = df_wetland_restrict)
}

# --- Priors -------------------------------------------------------------------

prior_sbc <- c(
  prior(normal(0, 1), nlpar = "theta", lb = 0),
  prior(normal(0, 1), nlpar = "program")
)

prior_sd_sbc <- c(prior(cauchy(0, 1), class = "sd", nlpar = "program"))

# --- Compile models (chains = 0) ---------------------------------------------

n_chains_compile <- 0
n_iter_compile <- 2000
n_cores_compile <- 1
n_threads_compile <- 4
seed <- 13

# Logit (linear)
brm(
  bf(vote_yes ~ theta * (program - bid), theta ~ 1, program ~ 1, nl = TRUE),
  data = df_wetland_full,
  family = bernoulli(),
  prior = prior_sbc,
  chains = n_chains_compile,
  iter = n_iter_compile,
  cores = n_cores_compile,
  threads = threading(n_threads_compile),
  backend = "cmdstanr",
  file = "models/wetland/wtps_sbc.rds"
)

# MRP (linear, no province)
brm(
  bf(
    vote_yes ~ theta * (program - bid),
    theta ~ 1,
    program ~ 1 + (1 | age_gender) + (1 | education_group) + (1 | income_group),
    nl = TRUE
  ),
  data = df_wetland_full,
  family = bernoulli(),
  prior = c(prior_sbc, prior_sd_sbc),
  chains = n_chains_compile,
  iter = n_iter_compile,
  cores = n_cores_compile,
  seed = seed,
  threads = threading(n_threads_compile),
  backend = "cmdstanr",
  control = list(adapt_delta = .95),
  file = "models/wetland/wtps_sbc_mrp.rds"
)

# MRP (linear, with province)
brm(
  bf(
    vote_yes ~ theta * (program - bid),
    theta ~ 1,
    program ~ 1 +
      (1 | province) +
      (1 | age_gender) +
      (1 | education_group) +
      (1 | income_group),
    nl = TRUE
  ),
  data = df_wetland_full,
  family = bernoulli(),
  prior = c(prior_sbc, prior_sd_sbc),
  chains = n_chains_compile,
  iter = n_iter_compile,
  cores = n_cores_compile,
  seed = seed,
  threads = threading(n_threads_compile),
  backend = "cmdstanr",
  control = list(adapt_delta = .95),
  file = "models/wetland/wtps_sbc_mrp_province.rds"
)

# Logit (exponential)
brm(
  bf(
    vote_yes ~ theta * (exp(program) - bid),
    theta ~ 1,
    program ~ 1,
    nl = TRUE
  ),
  data = df_wetland_full,
  family = bernoulli(),
  prior = prior_sbc,
  chains = n_chains_compile,
  iter = n_iter_compile,
  cores = n_cores_compile,
  threads = threading(n_threads_compile),
  backend = "cmdstanr",
  file = "models/wetland/wtps_sbc_exp.rds"
)

# MRP (exponential, no province)
brm(
  bf(
    vote_yes ~ theta * (exp(program) - bid),
    theta ~ 1,
    program ~ 1 + (1 | age_gender) + (1 | education_group) + (1 | income_group),
    nl = TRUE
  ),
  data = df_wetland_full,
  family = bernoulli(),
  prior = c(prior_sbc, prior_sd_sbc),
  chains = n_chains_compile,
  iter = n_iter_compile,
  cores = n_cores_compile,
  seed = seed,
  threads = threading(n_threads_compile),
  backend = "cmdstanr",
  control = list(adapt_delta = .95),
  file = "models/wetland/wtps_sbc_mrp_exp.rds"
)

# MRP (exponential, with province)
brm(
  bf(
    vote_yes ~ theta * (exp(program) - bid),
    theta ~ 1,
    program ~ 1 +
      (1 | province) +
      (1 | age_gender) +
      (1 | education_group) +
      (1 | income_group),
    nl = TRUE
  ),
  data = df_wetland_full,
  family = bernoulli(),
  prior = c(prior_sbc, prior_sd_sbc),
  chains = n_chains_compile,
  iter = n_iter_compile,
  cores = n_cores_compile,
  seed = seed,
  threads = threading(n_threads_compile),
  backend = "cmdstanr",
  control = list(adapt_delta = .95),
  file = "models/wetland/wtps_sbc_mrp_province_exp.rds"
)

# --- Estimate models ----------------------------------------------------------

n_chains <- 4
n_iter <- 2000
n_cores <- 4
n_threads <- 4

province_list <- c("Alberta", "Saskatchewan", "Manitoba")
sample_list <- c("full", "restrict")

# Models with province random effects (fit to all provinces together)
models_province <- c(
  "wtps_sbc",
  "wtps_sbc_mrp_province",
  "wtps_sbc_exp",
  "wtps_sbc_mrp_province_exp"
)

# Models without province random effects (fit per province)
models_by_province <- c(
  "wtps_sbc",
  "wtps_sbc_mrp",
  "wtps_sbc_exp",
  "wtps_sbc_mrp_exp"
)

# Fit across all provinces
for (model_name in models_province) {
  for (samp in sample_list) {
    temp <- update(
      readRDS(paste0("models/wetland/", model_name, ".rds")),
      newdata = get_sample_data(samp),
      chains = n_chains,
      iter = n_iter,
      cores = n_cores,
      threads = threading(n_threads),
      backend = "cmdstanr"
    )
    saveRDS(temp, paste0(foldername, model_name, "_all_wetland_", samp, ".rds"))
  }
}

# Fit per province
for (model_name in models_by_province) {
  for (prov in province_list) {
    for (samp in sample_list) {
      temp <- update(
        readRDS(paste0("models/wetland/", model_name, ".rds")),
        newdata = get_sample_data(samp) |> filter(province == prov),
        chains = n_chains,
        iter = n_iter,
        cores = n_cores,
        threads = threading(n_threads),
        backend = "cmdstanr"
      )
      saveRDS(
        temp,
        paste0(foldername, model_name, "_", prov, "_wetland_", samp, ".rds")
      )
    }
  }
}
