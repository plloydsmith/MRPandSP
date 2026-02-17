# ============================================================
# Simulation Plot Helpers
#
# MCSE functions, summariser, ggplot helpers for simulation
# results. Sourced by 2SummariseSims.R.
# ============================================================

# ---- MCSE helpers -------------------------------------------------

mcse_mean <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x)))

mcse_rmse <- function(est, truth) {
  se2 <- (est - truth)^2
  RMSE <- sqrt(mean(se2, na.rm = TRUE))
  if (!is.finite(RMSE) || RMSE == 0) return(NA_real_)
  sqrt(var(se2, na.rm = TRUE) / sum(is.finite(se2))) * (1 / (2 * RMSE))
}

mcse_coverage <- function(in_cov) {
  in_cov <- in_cov[is.finite(in_cov)]
  p <- mean(in_cov)
  sqrt(p * (1 - p) / length(in_cov))
}


# ---- Scenario-level summariser ------------------------------------

summarise_mcse_results <- function(results_all) {
  results_all |>
    filter(abs(bias) < 3) |>
    mutate(
      err2 = (mean - true_wtp)^2,
      error = mean - true_wtp,
      in_cov = ifelse(true_wtp >= lb90 & true_wtp <= ub90, 1L, 0L)
    ) |>
    summarise(
      R = n_distinct(survey_id),
      true_wtp_mean = mean(true_wtp, na.rm = TRUE),
      mean_est = mean(mean, na.rm = TRUE),
      mcse_mean = mcse_mean(mean),
      posterior_sd_mean = mean(sd_draws, na.rm = TRUE),
      posterior_sd_sd = sd(sd_draws, na.rm = TRUE),
      bias_raw = mean(error, na.rm = TRUE),
      mean_bias = mean(error, na.rm = TRUE) * 100,
      mcse_bias = mcse_mean(error),
      bias_lb90 = quantile(error, 0.05) * 100,
      bias_ub90 = quantile(error, 0.95) * 100,
      rmse_est = sqrt(mean(err2)),
      rmse_est_lb90 = sqrt(quantile(err2, 0.05)),
      rmse_est_ub90 = sqrt(quantile(err2, 0.95)),
      mcse_rmse_est = mcse_rmse(mean, true_wtp),
      rmse_draws_lb90 = quantile(rmse_draws, 0.05),
      rmse_draws_ub90 = quantile(rmse_draws, 0.95),
      rmse_draws = mean(rmse_draws, na.rm = TRUE),
      mcse_rmse_draws = sd(rmse_draws),
      coverage = mean(in_cov, na.rm = TRUE),
      mcse_coverage = mcse_coverage(in_cov),
      n_surveys = n(),
      max_rhat_flag = mean(replace_na(max_rhat, 1), na.rm = TRUE),
      .by = c(region, model, misspec, sample_size, bias_strength)
    )
}


# ---- Theme & colours ----------------------------------------------

my_colors <- c(
  "MRP" = "#004488",
  "Unweighted" = "#EE7733",
  "Weighted" = "#CC3311",
  "Post-stratification" = "#009988"
)

setup_sim_theme <- function() {
  theme_set(
    theme_minimal(base_size = 18) +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold")
      )
  )
}


# ---- Plot functions ------------------------------------------------

#' Dodge-point + errorbar plot (overall-level comparisons)
plot_overall <- function(
  data, x_var, y_var, y_lo, y_hi, ylab,
  xlab = "", title = NULL, show_legend = FALSE, xlabels = waiver()
) {
  p <- ggplot(
    data,
    aes(
      x = factor(.data[[x_var]]), y = .data[[y_var]],
      group = model, color = model
    )
  ) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(
      aes(ymin = .data[[y_lo]], ymax = .data[[y_hi]]),
      width = 0, position = position_dodge(width = 0.3)
    ) +
    geom_hline(yintercept = 0) +
    labs(x = xlab, y = ylab, color = "Model", title = title) +
    scale_colour_manual(
      values = my_colors,
      guide = if (show_legend) "legend" else "none"
    )

  if (!inherits(xlabels, "waiver")) {
    p <- p + scale_x_discrete(labels = xlabels)
  }
  p
}

#' Region-level horizontal dot plot (bias or RMSE by region)
plot_region <- function(
  data, x_var, x_lo, x_hi, xlab,
  ylab = "Region (Population share %)",
  xlimits = NULL, show_legend = FALSE
) {
  pd <- position_dodge(width = 0.6)

  facet_labeller <- labeller(
    bias_strength = as_labeller(c(
      `0`   = "Random~sampling~~(beta[strength]==0)",
      `0.5` = "Moderate~biased~sampling~~(beta[strength]==0.5)",
      `1`   = "High~biased~sampling~~(beta[strength]==1)"
    ), label_parsed)
  )

  p <- data |>
    ggplot(aes(x = .data[[x_var]], y = label, colour = model)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbar(
      aes(xmin = .data[[x_lo]], xmax = .data[[x_hi]]),
      width = 0, position = pd
    ) +
    scale_y_discrete(limits = rev) +
    geom_point(size = 1.8, position = pd) +
    facet_wrap(~bias_strength, labeller = facet_labeller) +
    labs(x = xlab, y = ylab, color = "Model") +
    scale_colour_manual(values = my_colors)

  if (!is.null(xlimits)) p <- p + xlim(xlimits[1], xlimits[2])

  if (show_legend) {
    p + theme(
      legend.position = c(0.98, 0.98),
      legend.justification = c("right", "top")
    )
  } else {
    p + theme(legend.position = "none")
  }
}

#' Build a 2×2 region panel (bias left, RMSE right) for two bias strengths
plot_region_panel <- function(data, sample_size_val) {
  d <- data |>
    filter(misspec == "Correct specification", sample_size == sample_size_val)

  p1 <- d |>
    filter(bias_strength == 0) |>
    plot_region("mean_bias", "bias_lb90", "bias_ub90",
                xlab = "Mean percent bias", xlimits = c(-300, 300))
  p3 <- d |>
    filter(bias_strength == 1) |>
    plot_region("mean_bias", "bias_lb90", "bias_ub90",
                xlab = "Mean percent bias", xlimits = c(-300, 300))
  p2 <- d |>
    filter(bias_strength == 0) |>
    plot_region("rmse_est", "rmse_est_lb90", "rmse_est_ub90",
                xlab = "Root mean squared error", ylab = "",
                xlimits = c(0, 3), show_legend = TRUE) +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  p4 <- d |>
    filter(bias_strength == 1) |>
    plot_region("rmse_est", "rmse_est_lb90", "rmse_est_ub90",
                xlab = "Root mean squared error", ylab = "",
                xlimits = c(0, 3)) +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

  (p1 + p2) / (p3 + p4)
}
