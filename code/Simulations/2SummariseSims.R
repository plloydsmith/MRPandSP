# ============================================================
# Summarise & Plot Simulation Results
#
# Loads scenario results, computes MCSE summaries, and
# produces all simulation figures for the paper.
# ============================================================

options(scipen = 999)

library(tidyverse)
library(patchwork)

source("code/Simulations/sim_config.R")
source("code/Simulations/sim_functions.R")
source("code/Simulations/sim_plot_helpers.R")

setup_sim_theme()

# ============================================================
# 1. LOAD & PREPARE DATA
# ============================================================

run_id <- "20251204_202854"

scenario_files <- list.files(
  "simulation_output",
  pattern = paste0("^scenario_", run_id),
  full.names = TRUE
)

results_all <- bind_rows(lapply(scenario_files, readRDS)) |>
  mutate(
    model = case_when(
      str_detect(Method, "MRP") ~ "MRP",
      str_detect(Method, "Rake") ~ "Weighted",
      str_detect(Method, "Post-strat") ~ "Post-stratification",
      str_detect(Method, "Sample") ~ "Unweighted",
      TRUE ~ "Other"
    ),
    model = factor(
      model,
      levels = c(
        "Unweighted",
        "Weighted",
        "Post-stratification",
        "MRP",
        "Other"
      )
    ),
    misspec = case_when(
      str_detect(Method, "omit") ~ "Omitted variables",
      str_detect(Method, "coarse") ~ "Coarse income categories",
      str_detect(Method, "full") ~ "Correct specification",
      TRUE ~ "Correct specification"
    )
  )

summary_scen <- summarise_mcse_results(results_all)


# ============================================================
# 2. DIAGNOSTIC TABLES
# ============================================================

# Posterior SD diagnostics
results_all |>
  filter(region == "Overall") |>
  summarise(
    mean_sd_draws = round(mean(sd_draws, na.rm = TRUE), 2),
    sd_of_sd_draws = round(sd(sd_draws, na.rm = TRUE), 2),
    .by = c(model, misspec, sample_size, bias_strength)
  )

# RMSE summary (wide format)
summary_scen |>
  filter(
    region == "Overall",
    misspec == "Correct specification",
    sample_size == 1000
  ) |>
  summarise(rmse_est = round(rmse_est, 2), .by = c(model, bias_strength)) |>
  pivot_wider(names_from = "model", values_from = "rmse_est")


# ============================================================
# 3. FIGURE – BIAS STRENGTH × SAMPLE SIZE (overall)
# ============================================================

bias_labels <- c(
  expression(atop("Random sampling", beta[strength] == 0)),
  expression(atop("Moderate bias", beta[strength] == 0.5)),
  expression(atop("High bias", beta[strength] == 1))
)

sample_labels <- function(x) {
  case_when(
    x == "500" ~ "n = 500",
    x == "1000" ~ "n = 1000",
    x == "3000" ~ "n = 3000",
    TRUE ~ x
  )
}

d_bias <- summary_scen |>
  filter(
    region == "Overall",
    misspec == "Correct specification",
    sample_size == 1000,
    bias_strength %in% c(0, 0.5, 1)
  )

d_sample <- summary_scen |>
  filter(
    region == "Overall",
    misspec == "Correct specification",
    bias_strength == 0.5
  )

p1 <- plot_overall(
  d_bias,
  "bias_strength",
  "mean_bias",
  "bias_lb90",
  "bias_ub90",
  ylab = "Mean percent bias",
  title = "Survey bias strength comparison (with n = 1000)",
  xlabels = bias_labels
) +
  ylim(-10, 100)

p3 <- plot_overall(
  d_bias,
  "bias_strength",
  "rmse_est",
  "rmse_est_lb90",
  "rmse_est_ub90",
  ylab = "Root mean squared error",
  xlabels = bias_labels
) +
  ylim(0, 1)

legend_pos <- theme(
  legend.position = c(0.98, 0.98),
  legend.justification = c("right", "top")
)

p2 <- plot_overall(
  d_sample,
  "sample_size",
  "mean_bias",
  "bias_lb90",
  "bias_ub90",
  ylab = "Mean percent bias",
  title = expression(
    "Survey sample size comparison (with " * beta[strength] == .5 * ")"
  ),
  show_legend = TRUE,
  xlabels = sample_labels
) +
  ylim(-10, 100) +
  legend_pos

p4 <- plot_overall(
  d_sample,
  "sample_size",
  "rmse_est",
  "rmse_est_lb90",
  "rmse_est_ub90",
  ylab = "Root mean squared error",
  show_legend = TRUE,
  xlabels = sample_labels
) +
  ylim(0, 1) +
  legend_pos

(p1 + p2) / (p3 + p4)
ggsave(last_plot(), file = "figs/sim_bias_sample.png", height = 12, width = 15)


# ============================================================
# 4. FIGURE – MODEL MISSPECIFICATION (overall)
# ============================================================

d_misspec <- summary_scen |>
  filter(region == "Overall", bias_strength == 1, sample_size == 1000)

p_mis_bias <- plot_overall(
  d_misspec,
  "misspec",
  "mean_bias",
  "bias_lb90",
  "bias_ub90",
  ylab = "Mean percent bias",
  show_legend = TRUE
) +
  legend_pos

p_mis_rmse <- plot_overall(
  d_misspec,
  "misspec",
  "rmse_est",
  "rmse_est_lb90",
  "rmse_est_ub90",
  ylab = "Root mean squared error",
  xlab = "Model specification"
)

p_mis_bias / p_mis_rmse
ggsave(last_plot(), file = "figs/sim_omit.png", width = 15)


# ============================================================
# 5. SUMMARY TABLES
# ============================================================

summary_scen |>
  filter(
    region == "Overall",
    misspec == "Correct specification",
    bias_strength %in% c(0.5, 1)
  ) |>
  summarise(
    count = n(),
    av_bias = mean(mean_bias),
    av_rmse_est = mean(rmse_est),
    av_rmse_draws = mean(rmse_draws),
    .by = c(model, bias_strength, sample_size)
  )

summary_scen |>
  filter(region == "Overall", bias_strength == 1, sample_size == 1000) |>
  summarise(
    count = n(),
    av_bias = mean(mean_bias),
    av_rmse_est = mean(rmse_est),
    av_rmse_draws = mean(rmse_draws),
    .by = c(model, bias_strength, misspec, sample_size)
  )

summary_scen |>
  filter(
    region == "Overall",
    misspec == "Correct specification",
    model != "Unweighted",
    sample_size == 1000
  ) |>
  summarise(
    mean_bias = mean(mean_bias),
    mean_rmse = mean(rmse_est),
    count = n(),
    .by = c(model, sample_size, bias_strength)
  )


# ============================================================
# 6. FIGURES – REGION-LEVEL RESULTS
# ============================================================

# Rebuild population to get region weights (deterministic seed)
pop <- build_population(CONFIG$N_TOTAL, CONFIG$K_REGIONS)$pop

region_weights <- pop |>
  summarise(N = sum(pop_size), .by = region) |>
  arrange(desc(N)) |>
  mutate(
    percent = round(N / sum(N) * 100, 1),
    region_letter = LETTERS[seq_len(n())],
    label = sprintf("%s (%.1f%%)", region_letter, percent)
  ) |>
  arrange(N)

summary_scen_region <- summary_scen |>
  filter(region != "Overall") |>
  left_join(region_weights, join_by(region))

# n = 1000
plot_region_panel(summary_scen_region, sample_size_val = 1000)
ggsave(last_plot(), file = "figs/sim_region.png", height = 15, width = 15)

# n = 3000
plot_region_panel(summary_scen_region, sample_size_val = 3000)
ggsave(last_plot(), file = "figs/sim_region3000.png", height = 15, width = 15)

# Weighted-average RMSE across regions
summary_scen_region |>
  filter(bias_strength %in% c(0, 1), misspec == "Correct specification") |>
  summarise(
    count = n(),
    av_rmse_est = mean(rmse_est),
    av_rmse_est_weight = weighted.mean(rmse_est, w = N),
    av_rmse_draws = mean(rmse_draws),
    av_rmse_draws_weight = weighted.mean(rmse_draws, w = N),
    .by = c(model, bias_strength, sample_size)
  )
