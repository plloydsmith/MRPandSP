# ============================================================
# Simulation Configuration
# ============================================================

CONFIG <- list(
  # --- Population ---
  N_TOTAL = 1e6,
  K_REGIONS = 20,
  RESPONDENT_VARS = list(age = 8, income = 8, education = 8, care = 8),
  BETA0 = 100,
  RANGE = list(
    age = c(-25, 25),
    income = c(-25, 25),
    education = c(-25, 25),
    care = c(-25, 25)
  ),
  REGION_SD = 25,
  SIGMA_UNSCALED = 50,

  # --- Survey ---
  BID_ACCEPTANCES = c(0.2, 0.4, 0.6, 0.8),
  SAMPLE_SIZES = c(500, 1000, 3000),
  BIAS_STRENGTHS = c(0, 1, 1.5),
  N_REPLICATES = 100,
  N_BOOTSTRAP_DRAWS = 100,

  # --- MCMC ---
  N_CHAINS = 4,
  N_ITER = 1000,
  MAX_RHAT_THRESHOLD = 1.03,

  # --- Parallel ---
  N_CORES = 10
)

# Which predictors each model specification uses
CONFIG$MISSPEC <- list(
  full = names(CONFIG$RESPONDENT_VARS),
  omit = setdiff(names(CONFIG$RESPONDENT_VARS), c("care", "age")),
  coarse = c(setdiff(names(CONFIG$RESPONDENT_VARS), "income"), "income_coarse")
)

CONFIG$MISSPEC_OPTIONS <- c("full", "omit", "coarse")

# Map model specification -> (model_file, seed)
CONFIG$MRP_MODELS <- list(
  full = list(file = "code/simulations/mrp_full", seed = 1005),
  omit = list(file = "code/simulations/mrp_omit", seed = 1006),
  coarse = list(file = "code/simulations/mrp_coarse", seed = 1007)
)
