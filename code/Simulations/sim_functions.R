# ============================================================
# Simulation Functions
#
# Utility, population generation, survey sampling, estimation,
# bootstrap, MRP, and orchestration helpers.
# ============================================================

# ---- Small helpers ------------------------------------------------

#' Weighted quantile
weighted_quantile <- function(x, w, probs) {
  ord <- order(x)
  x_sorted <- x[ord]
  cw <- cumsum(w[ord] / sum(w[ord]))
  sapply(probs, \(p) x_sorted[which.min(abs(cw - p))])
}

#' Coarsen income to Low / Mid / High
income_to_coarse <- function(x) {
  case_when(
    x %in% paste0("income_", 1:3) ~ "Low",
    x %in% paste0("income_", 4:6) ~ "Mid",
    x %in% paste0("income_", 7:8) ~ "High",
    TRUE ~ NA_character_
  )
}

#' Extract WTP from GLM coefficients (-intercept / bid)
glm_to_wtp <- function(fit) {
  co <- coef(fit)
  if (!"bid" %in% names(co)) return(NA_real_)
  if (!is.finite(co["bid"]) || co["bid"] >= 0) return(NA_real_)
  -co["(Intercept)"] / co["bid"]
}


# ---- Summarisation helpers ----------------------------------------

#' Summarise a vector of draws into a single-row tibble
summarise_draws_vec <- function(
  draws,
  true_val,
  method = NA_character_,
  max_rhat = NA_real_,
  coverage = NA_real_,
  region = "Overall"
) {
  draws <- draws[is.finite(draws)]
  q5 <- quantile(draws, 0.05)
  q95 <- quantile(draws, 0.95)

  tibble(
    region = region,
    mean = mean(draws),
    lb90 = q5,
    ub90 = q95,
    width90 = q95 - q5,
    sd_draws = sd(draws),
    rmse_draws = sqrt(mean((draws - true_val)^2)),
    n_draws_used = length(draws),
    max_rhat = max_rhat,
    coverage = coverage,
    Method = method
  )
}


# ---- Parallel setup -----------------------------------------------

setup_cluster_exports <- function(cl) {
  clusterExport(cl, "CONFIG", envir = .GlobalEnv)

  fun_names <- ls(envir = .GlobalEnv)
  is_fun <- vapply(
    fun_names,
    \(x) is.function(get(x, envir = .GlobalEnv)),
    logical(1)
  )
  clusterExport(cl, fun_names[is_fun], envir = .GlobalEnv)

  clusterEvalQ(cl, {
    library(dplyr)
    library(data.table)
    library(tidyr)
    library(purrr)
    library(brms)
    library(anesrake)
    library(posterior)
    library(tidybayes)
  })
}


# ============================================================
# POPULATION GENERATION
# ============================================================

#' Bias weights for sampling (one per respondent variable)
create_bias_weights <- function(pop, respondent_vars) {
  lapply(names(respondent_vars), \(var) {
    lvls <- sort(unique(pop[[var]]))
    setNames(seq(0.5, 3.0, length.out = length(lvls)), lvls)
  }) |>
    setNames(names(respondent_vars))
}

#' Cell-design tables for all model specifications
create_all_cells_designs <- function(pop, misspec_options) {
  cells <- lapply(misspec_options, \(m) make_cells_design(pop, m))
  names(cells) <- misspec_options
  if (!"full" %in% names(cells)) {
    cells$full <- make_cells_design(pop, "full")
  }
  cells
}

#' Build demographic structure for a single region
build_region_demographics <- function(
  region_id,
  region_size,
  base_grid,
  cat_vars,
  region_name,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed + region_id)
  }

  # Generate marginal probabilities for each variable
  marginals <- lapply(cat_vars, \(lvls) {
    p <- rgamma(length(lvls), 1, 1)
    p / sum(p)
  })

  # 4-way joint = age × education × income × care  (all independent)
  joint_prob <- Reduce(outer, marginals) |> as.vector()
  joint_prob <- joint_prob / sum(joint_prob)

  df <- base_grid |>
    mutate(region = region_name, pop_size = round(joint_prob * region_size))

  # Fix rounding to match exact region size
  df$pop_size[1] <- df$pop_size[1] + (region_size - sum(df$pop_size))
  df$pop_size <- pmax(df$pop_size, 0L)
  df
}


#' Assign true WTP parameters to each population cell
calculate_wtp_parameters <- function(
  pop,
  regions,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed + 10000)
  }

  n <- nrow(pop)

  # 1. Main effects
  var_effect_sum <- numeric(n)
  for (var in names(CONFIG$RESPONDENT_VARS)) {
    if (var == "income") {
      z <- pop$income_z - 0.5
      var_effect_sum <- var_effect_sum + 50 * z - 20 * z^2
    } else {
      lvls <- sort(unique(pop[[var]]))
      rng <- CONFIG$RANGE[[var]]
      beta <- setNames(seq(rng[1], rng[2], length.out = length(lvls)), lvls)
      var_effect_sum <- var_effect_sum + beta[pop[[var]]]
    }
  }

  # 2. Region effects
  region_effects <- setNames(
    rnorm(length(regions), 0, CONFIG$REGION_SD),
    regions
  )

  pop <- pop |>
    mutate(
      true_mu_unscaled = CONFIG$BETA0 +
        var_effect_sum +
        region_effects[region],
      true_sigma_unscaled = CONFIG$SIGMA_UNSCALED
    )

  # 3. Normalise so population-weighted mean = 1
  sf <- weighted.mean(pop$true_mu_unscaled, pop$pop_size)
  pop |>
    mutate(
      true_mu = true_mu_unscaled / sf,
      true_sigma = true_sigma_unscaled / sf
    )
}

#' Build full synthetic population
build_population <- function(
  N_total = CONFIG$N_TOTAL,
  K_regions = CONFIG$K_REGIONS,
  seed = 12345
) {
  set.seed(seed)

  cat_vars <- Map(
    \(v, n) paste0(v, "_", seq_len(n)),
    v = names(CONFIG$RESPONDENT_VARS),
    n = CONFIG$RESPONDENT_VARS
  )

  regions <- paste0("Region_", seq_len(K_regions))

  # Region sizes
  shares <- rgamma(K_regions, 1, 1)
  shares <- shares / sum(shares)
  sizes <- round(shares * N_total)
  sizes[1] <- sizes[1] + (N_total - sum(sizes))

  base_grid <- do.call(expand_grid, cat_vars)

  pop <- map_dfr(seq_len(K_regions), \(i) {
    build_region_demographics(
      i,
      sizes[i],
      base_grid,
      cat_vars,
      regions[i],
      seed = seed
    )
  })

  pop <- pop |>
    mutate(
      income_index = as.numeric(gsub("income_", "", income)),
      income_mid = (income_index - 0.5) / CONFIG$RESPONDENT_VARS$income,
      income_z = pmin(1, pmax(0, income_mid + rnorm(n(), 0, 0.05)))
    )

  pop <- calculate_wtp_parameters(pop, regions, seed = seed)

  list(pop = pop, regions = regions, vars = names(CONFIG$RESPONDENT_VARS))
}


# ============================================================
# SURVEY SAMPLING & WEIGHTING
# ============================================================

#' Draw a (possibly biased) survey sample
draw_sample <- function(pop, n_sample, bias_list, bias_strength) {
  weights <- pop$pop_size
  for (var in names(bias_list)) {
    weights <- weights * (bias_list[[var]][pop[[var]]]^bias_strength)
  }
  probs <- weights / sum(weights)

  bid_levels <- weighted_quantile(
    pop$true_mu,
    pop$pop_size,
    CONFIG$BID_ACCEPTANCES
  )

  # Force ≥ 1 observation per region
  regions <- unique(pop$region)
  idx_forced <- map_int(regions, \(r) {
    ri <- which(pop$region == r)
    sample(ri, 1, prob = probs[ri])
  })
  n_rest <- max(0, n_sample - length(regions))
  idx <- c(idx_forced, sample(nrow(pop), n_rest, replace = TRUE, prob = probs))

  pop[idx, ] |>
    mutate(
      respondent_id = row_number(),
      wtp_latent = rnorm(n(), true_mu, true_sigma),
      bid = sample(bid_levels, n(), TRUE),
      yes = as.integer(wtp_latent >= bid),
      probs_sampled = probs[idx]
    )
}

#' Post-stratification cell design for one model specification
make_cells_design <- function(pop, misspec) {
  predictors <- CONFIG$MISSPEC[[misspec]]
  pop_aug <- pop
  if ("income_coarse" %in% predictors && !"income_coarse" %in% names(pop)) {
    pop_aug <- pop_aug |> mutate(income_coarse = income_to_coarse(income))
  }

  pop_aug |>
    group_by(across(all_of(c(predictors, "region")))) |>
    summarise(N = sum(pop_size), .groups = "drop") |>
    mutate(bid = 0) |>
    filter(N > 0)
}

#' Compute rake weights
compute_rake_weights <- function(cells_design, survey, predictors) {
  stopifnot("region" %in% predictors)

  targets <- lapply(predictors, \(var) {
    cells_design |>
      group_by(.data[[var]]) |>
      summarise(N = sum(N), .groups = "drop") |>
      mutate(p = N / sum(N)) %>%
      {
        setNames(.$p, .[[var]])
      }
  }) |>
    setNames(predictors)

  survey_fct <- as.data.frame(survey)
  for (var in predictors) {
    survey_fct[[var]] <- factor(
      survey_fct[[var]],
      levels = names(targets[[var]])
    )
  }
  ok <- complete.cases(survey_fct[, predictors, drop = FALSE])
  survey_ok <- survey_fct[ok, , drop = FALSE]

  if (anyDuplicated(survey_ok$respondent_id)) {
    survey_ok$respondent_id <- paste0(
      survey_ok$respondent_id,
      "_",
      seq_len(nrow(survey_ok))
    )
  }

  raked <- anesrake(
    targets,
    survey_ok,
    caseid = survey_ok$respondent_id,
    cap = 5,
    type = "nolim",
    choosemethod = "total",
    verbose = FALSE
  )

  w <- rep(NA_real_, nrow(survey))
  w[ok] <- as.numeric(raked$weightvec[survey_ok$respondent_id])
  w[!is.finite(w)] <- NA_real_
  w
}

#' Prepare survey: add rake weights for every specification
prepare_survey <- function(survey, cells_design_list, survey_id) {
  if (
    "income_coarse" %in%
      unlist(CONFIG$MISSPEC) &&
      !"income_coarse" %in% names(survey)
  ) {
    survey <- survey |> mutate(income_coarse = income_to_coarse(income))
  }
  survey <- survey |> mutate(survey_id = survey_id)

  for (spec in CONFIG$MISSPEC_OPTIONS) {
    preds <- CONFIG$MISSPEC[[spec]]
    if (!"region" %in% preds) {
      preds <- c(preds, "region")
    }
    survey[[paste0("rake_wt_", spec)]] <-
      compute_rake_weights(cells_design_list[[spec]], survey, preds)
  }
  survey
}


# ============================================================
# GLM-BASED WTP ESTIMATION
# ============================================================

#' Post-stratified WTP from a fixed-effects GLM
wtp_from_fixed_glm <- function(fit, cells_df) {
  co <- coef(fit)
  gamma <- co["bid"]

  na_result <- list(wtp = NA_real_, coverage = NA_real_)
  if (!"bid" %in% names(co)) return(na_result)
  if (!is.finite(gamma) || gamma >= 0) return(na_result)

  total_pop <- sum(cells_df$N)

  # Align factor levels with those seen by the model
  for (var in intersect(names(fit$xlevels), names(cells_df))) {
    cells_df[[var]] <- factor(cells_df[[var]], levels = fit$xlevels[[var]])
    cells_df <- cells_df |> filter(!is.na(.data[[var]]))
  }
  if (nrow(cells_df) == 0) return(na_result)

  coverage <- sum(cells_df$N) / total_pop
  mm <- tryCatch(
    model.matrix(delete.response(terms(fit)), cells_df),
    error = \(e) NULL
  )
  if (is.null(mm) || nrow(mm) != nrow(cells_df)) {
    return(list(wtp = NA_real_, coverage = coverage))
  }

  cn <- intersect(colnames(mm), names(co))
  if (length(cn) == 0) return(list(wtp = NA_real_, coverage = coverage))

  wtp_cells <- -as.numeric(mm[, cn, drop = FALSE] %*% co[cn]) / gamma
  ok <- is.finite(wtp_cells)
  if (!any(ok)) return(list(wtp = NA_real_, coverage = coverage))

  cw <- cells_df$N[ok] / sum(cells_df$N[ok])
  list(wtp = sum(wtp_cells[ok] * cw), coverage = coverage)
}


# ============================================================
# BRMS / MRP
# ============================================================

create_wtp_formula <- function(predictors) {
  re <- paste0("(1|", predictors, ")", collapse = " + ")
  prog_fml <- as.formula(paste("program ~ 1 +", re, "+ (1|region)"))

  f <- bf(
    yes ~ exp(logtheta) * (program - bid),
    logtheta ~ 1,
    program ~ 1,
    nl = TRUE
  )
  f[["pforms"]][["program"]] <- prog_fml
  f
}

create_priors <- function(predictors) {
  c(
    prior(student_t(3, 0, 0.4), class = "sd", nlpar = "program"),
    prior(normal(1, 2), class = "b", nlpar = "program"),
    prior(normal(log(1 / 0.3), 0.5), nlpar = "logtheta")
  )
}

#' Fit, check convergence, and summarise an MRP model
fit_and_summarize_mrp <- function(
  model_file,
  survey,
  cells_design,
  n_draws,
  true_mean,
  true_by_region,
  regions,
  method_name,
  seed_val
) {
  # Fit
  mod <- update(
    readRDS(paste0(model_file, ".rds")),
    newdata = survey,
    chains = CONFIG$N_CHAINS,
    cores = 2,
    iter = CONFIG$N_ITER,
    seed = seed_val,
    refresh = 0
  )

  # Convergence
  summ <- summarise_draws(as_draws_df(mod))
  max_rhat <- max(summ$rhat, na.rm = TRUE)
  if (max_rhat > CONFIG$MAX_RHAT_THRESHOLD) {
    warning(sprintf(
      "%s convergence issue: max Rhat = %.3f",
      method_name,
      max_rhat
    ))
  }

  # Posterior WTP draws
  wtp_draws <- add_linpred_draws(
    mod,
    newdata = cells_design,
    value = "wtp",
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian",
    nlpar = "program",
    ndraws = n_draws,
    re_formula = NULL
  )
  dt <- as.data.table(wtp_draws)

  overall <- dt[,
    .(wtp = stats::weighted.mean(wtp, w = N, na.rm = TRUE)),
    by = .draw
  ][, region := "Overall"]
  regional <- dt[,
    .(wtp = stats::weighted.mean(wtp, w = N, na.rm = TRUE)),
    by = .(.draw, region)
  ]
  all_rows <- rbind(overall, regional, use.names = TRUE)

  result <- as_tibble(all_rows) |>
    group_by(region) |>
    summarise(
      mean = mean(wtp, na.rm = TRUE),
      lb90 = quantile(wtp, 0.05, na.rm = TRUE),
      ub90 = quantile(wtp, 0.95, na.rm = TRUE),
      width90 = ub90 - lb90,
      sd_draws = sd(wtp, na.rm = TRUE),
      rmse_draws = {
        truth <- if (first(region) == "Overall") {
          true_mean
        } else {
          true_by_region[first(region)]
        }
        sqrt(mean((wtp - truth)^2, na.rm = TRUE))
      },
      .groups = "drop"
    ) |>
    mutate(
      n_draws_used = n_draws,
      coverage = NA_real_,
      Method = method_name,
      max_rhat = max_rhat
    )

  rm(mod)
  gc(verbose = FALSE)
  result
}


# ============================================================
# BOOTSTRAP MLE
# ============================================================

#' Single bootstrap estimate — returns list(overall_wtp, overall_cov,
#' regional_wtp, regional_cov). For sample/rake, regional fields are NA vectors.
get_bootstrap_estimate <- function(
  boot_sample, cells_design, type, predictors, weight_col, regions
) {
  na_regional <- setNames(rep(NA_real_, length(regions)), regions)

  if (type == "sample") {
    fit <- glm(yes ~ bid, data = boot_sample, family = binomial)
    return(list(
      overall_wtp = glm_to_wtp(fit), overall_cov = NA_real_,
      regional_wtp = na_regional, regional_cov = na_regional
    ))
  }

  if (type == "rake") {
    bw <- boot_sample |> filter(is.finite(.data[[weight_col]]))
    if (nrow(bw) == 0) {
      return(list(
        overall_wtp = NA_real_, overall_cov = NA_real_,
        regional_wtp = na_regional, regional_cov = na_regional
      ))
    }
    fit <- glm(yes ~ bid, data = bw, family = binomial, weights = bw[[weight_col]])
    return(list(
      overall_wtp = glm_to_wtp(fit), overall_cov = NA_real_,
      regional_wtp = na_regional, regional_cov = na_regional
    ))
  }

  # type == "fixed" (post-stratification)
  na_result <- list(
    overall_wtp = NA_real_, overall_cov = NA_real_,
    regional_wtp = na_regional, regional_cov = na_regional
  )

  fml <- as.formula(paste(
    "yes ~ bid +", paste(predictors, collapse = " + "), "+ region"
  ))
  fit <- try(glm(fml, data = boot_sample, family = binomial), silent = TRUE)
  if (inherits(fit, "try-error")) return(na_result)

  # Keep only cells whose levels appear in sample
  cells_f <- cells_design
  for (p in predictors) {
    cells_f <- cells_f |> filter(.data[[p]] %in% unique(boot_sample[[p]]))
  }

  safe_wtp <- \(f, c) {
    tryCatch(wtp_from_fixed_glm(f, c), error = \(e) {
      list(wtp = NA_real_, coverage = NA_real_)
    })
  }

  overall <- safe_wtp(fit, cells_f)
  reg_wtp <- na_regional
  reg_cov <- na_regional
  for (r in regions) {
    res <- safe_wtp(fit, cells_f |> filter(region == r))
    reg_wtp[r] <- res$wtp
    reg_cov[r] <- res$coverage
  }

  list(
    overall_wtp = overall$wtp, overall_cov = overall$coverage,
    regional_wtp = reg_wtp, regional_cov = reg_cov
  )
}

#' Summarise bootstrap results into a flat tibble (overall + regional rows)
summarize_bootstrap_results <- function(
  boot_results, regions, true_mean, true_by_region, method_name
) {
  overall_draws <- vapply(boot_results, `[[`, numeric(1), "overall_wtp")
  overall_cov <- vapply(boot_results, `[[`, numeric(1), "overall_cov")

  overall_summary <- summarise_draws_vec(
    overall_draws, true_mean, method_name,
    coverage = mean(overall_cov, na.rm = TRUE)
  )

  regional_draws <- do.call(rbind, lapply(boot_results, `[[`, "regional_wtp"))
  regional_cov <- do.call(rbind, lapply(boot_results, `[[`, "regional_cov"))

  regional_summaries <- map_dfr(regions, \(r) {
    summarise_draws_vec(
      regional_draws[, r], true_by_region[[r]],
      method = method_name,
      coverage = mean(regional_cov[, r], na.rm = TRUE),
      region = r
    )
  })

  bind_rows(overall_summary, regional_summaries)
}

#' Run bootstrap MLE for one method and return a flat tibble
bootstrap_mle_summarized <- function(
  survey, boot_indices, cells_design, type, n_draws, misspec,
  true_mean, true_by_region, method_name
) {
  regions <- unique(cells_design$region)
  predictors <- CONFIG$MISSPEC[[misspec]]
  weight_col <- paste0("rake_wt_", misspec)

  boot_results <- lapply(seq_len(n_draws), \(i) {
    est <- get_bootstrap_estimate(
      survey[boot_indices[[i]], , drop = FALSE],
      cells_design, type, predictors, weight_col, regions
    )
    if (i %% 20 == 0) gc(verbose = FALSE)
    est
  })

  summarize_bootstrap_results(
    boot_results, regions, true_mean, true_by_region, method_name
  )
}


# ============================================================
# ORCHESTRATION: run all methods on one survey
# ============================================================

#' Generate bootstrap indices (region-stratified)
make_boot_indices <- function(survey, n_draws) {
  regions <- unique(survey$region)
  n <- nrow(survey)

  lapply(seq_len(n_draws), \(b) {
    forced <- map_int(regions, \(r) {
      ri <- which(survey$region == r)
      if (length(ri) == 1) ri else sample(ri, 1, prob = survey$probs_sampled[ri])
    })
    rest <- sample(
      n, max(0, n - length(regions)),
      replace = TRUE, prob = survey$probs_sampled
    )
    c(forced, rest)
  })
}

#' Run all MLE-based methods (sample, rake, post-strat × each spec)
run_mle_from_survey <- function(
  survey, cells_design_map, n_draws, true_mean, true_by_region
) {
  boot_indices <- make_boot_indices(survey, n_draws)

  # Define all method configurations: (type, spec, label)
  methods <- tibble(
    type = "sample", spec = "full",
    label = "Sample (MLE)"
  )
  for (s in CONFIG$MISSPEC_OPTIONS) {
    methods <- bind_rows(methods, tibble(
      type = c("rake", "fixed"),
      spec = s,
      label = c(
        paste0("Rake (MLE, ", s, ")"),
        paste0("Post-strat (MLE, ", s, ")")
      )
    ))
  }

  map2_dfr(methods$type, seq_len(nrow(methods)), \(type, i) {
    spec <- methods$spec[i]
    label <- methods$label[i]
    message("    ", label)
    bootstrap_mle_summarized(
      survey, boot_indices, cells_design_map[[spec]],
      type, n_draws, spec, true_mean, true_by_region, label
    )
  })
}

#' Run all MRP methods
run_mrp_from_survey <- function(
  survey, cells_design_map, n_draws, true_mean, true_by_region, regions
) {
  specs <- intersect(CONFIG$MISSPEC_OPTIONS, names(CONFIG$MRP_MODELS))

  map_dfr(specs, \(spec) {
    info <- CONFIG$MRP_MODELS[[spec]]
    message("    MRP (Bayesian, ", spec, ")")
    fit_and_summarize_mrp(
      info$file, survey, cells_design_map[[spec]], n_draws,
      true_mean, true_by_region, regions,
      paste0("MRP (Bayesian, ", spec, ")"), info$seed
    )
  })
}


# ============================================================
# SIMULATION LOOP
# ============================================================

setup_scenario <- function(pop) {
  true_mean <- weighted.mean(pop$true_mu, pop$pop_size)
  true_by_region <- pop |>
    group_by(region) |>
    summarise(true = weighted.mean(true_mu, pop_size), .groups = "drop") %>%
    { setNames(.$true, .$region) }

  list(
    true_mean = true_mean,
    true_by_region = true_by_region,
    regions = unique(pop$region),
    bias_list = create_bias_weights(pop, CONFIG$RESPONDENT_VARS),
    cells_design_map = create_all_cells_designs(pop, CONFIG$MISSPEC_OPTIONS)
  )
}

compute_sample_statistics <- function(survey_data) {
  overall <- tibble(
    region = "Overall",
    survey_sample_size = nrow(survey_data),
    sample_wtp_mean = mean(survey_data$wtp_latent, na.rm = TRUE)
  )
  by_region <- survey_data |>
    group_by(region) |>
    summarise(
      survey_sample_size = n(),
      sample_wtp_mean = mean(wtp_latent, na.rm = TRUE)
    )
  bind_rows(overall, by_region)
}

analyze_one_replicate <- function(
  replicate_id, scenario_setup, pop, n, bias_strength, n_draws, include_mrp
) {
  survey_data <- draw_sample(pop, n, scenario_setup$bias_list, bias_strength)
  survey_data <- prepare_survey(
    survey_data, scenario_setup$cells_design_map, replicate_id
  )
  sample_stats <- compute_sample_statistics(survey_data)

  res <- run_mle_from_survey(
    survey_data, scenario_setup$cells_design_map,
    n_draws, scenario_setup$true_mean, scenario_setup$true_by_region
  )
  if (include_mrp) {
    res <- bind_rows(res, run_mrp_from_survey(
      survey_data, scenario_setup$cells_design_map,
      n_draws, scenario_setup$true_mean,
      scenario_setup$true_by_region, scenario_setup$regions
    ))
  }

  res |>
    mutate(survey_id = replicate_id) |>
    left_join(sample_stats, by = "region")
}

replicate_scenario <- function(
  pop, n, bias_strength, R, n_draws = 200,
  include_mrp = TRUE, ncores, base_seed = 12345
) {
  scenario_setup <- setup_scenario(pop)
  set.seed(base_seed)
  rep_seeds <- sample.int(.Machine$integer.max, R)
  message(sprintf("  Running %d replicates on %d cores...", R, ncores))

  worker <- function(replicate_id) {
    set.seed(rep_seeds[replicate_id])
    analyze_one_replicate(
      replicate_id, scenario_setup, pop, n,
      bias_strength, n_draws, include_mrp
    )
  }

  if (.Platform$OS.type == "windows") {
    cl <- makeCluster(ncores)
    on.exit(stopCluster(cl), add = TRUE)
    setup_cluster_exports(cl)
    clusterExport(cl, c(
      "scenario_setup", "pop", "n", "bias_strength",
      "n_draws", "include_mrp", "rep_seeds"
    ), envir = environment())
    results <- parLapply(cl, seq_len(R), worker)
  } else {
    results <- mclapply(seq_len(R), worker, mc.cores = ncores)
  }
  bind_rows(results)
}

run_all_scenarios <- function(
  sample_sizes = CONFIG$SAMPLE_SIZES,
  bias_strengths = CONFIG$BIAS_STRENGTHS,
  R = CONFIG$N_REPLICATES,
  n_draws = CONFIG$N_BOOTSTRAP_DRAWS,
  include_mrp = TRUE,
  ncores = CONFIG$N_CORES,
  output_dir = "simulation_output"
) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  scenarios <- expand_grid(
    sample_size = sample_sizes,
    bias_strength = bias_strengths
  )
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  all_results <- vector("list", nrow(scenarios))

  for (i in seq_len(nrow(scenarios))) {
    n <- scenarios$sample_size[i]
    bs <- scenarios$bias_strength[i]

    pb <- build_population(CONFIG$N_TOTAL, CONFIG$K_REGIONS)
    pop <- pb$pop
    true_mean <- weighted.mean(pop$true_mu, pop$pop_size)

    df_true <- pop |>
      group_by(region) |>
      summarise(true_wtp = weighted.mean(true_mu, pop_size), .groups = "drop") |>
      bind_rows(tibble(region = "Overall", true_wtp = true_mean))

    message(sprintf("Scenario %d/%d: n=%d, bias=%.2f", i, nrow(scenarios), n, bs))
    t0 <- Sys.time()

    out <- replicate_scenario(pop, n, bs, R, n_draws, include_mrp, ncores) |>
      mutate(sample_size = n, bias_strength = bs, scenario_id = i) |>
      left_join(df_true, by = "region") |>
      mutate(bias = mean - true_wtp)

    all_results[[i]] <- out
    elapsed_min <- as.numeric(Sys.time() - t0, units = "mins")

    saveRDS(out, sprintf("%s/scenario_%s_n%d_bs%.2f.rds", output_dir, ts, n, bs))
    saveRDS(bind_rows(all_results[seq_len(i)]),
            sprintf("%s/cumulative_results_%s.rds", output_dir, ts))
    saveRDS(CONFIG, sprintf("%s/config_%s.rds", output_dir, ts))

    message(sprintf("  Finished in %.1f minutes", elapsed_min))
  }

  final <- bind_rows(all_results)
  saveRDS(final, sprintf("%s/final_results_%s.rds", output_dir, ts))
  final
}
