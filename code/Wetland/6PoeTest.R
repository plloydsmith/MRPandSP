# =============================================================================
# Poe et al. (2005) Combinatorial WTP Comparison Tests
# Compares WTP distributions across model types (MRP, MNL, RPL)
# for both SRD choice experiment and Wetland SBC models.
#
# Requires: wtp_poststrat (from SRD 5ImputeWTPSRD.R) to be in environment
# =============================================================================

# --- Poe comparison function --------------------------------------------------

poe_compare_vec <- function(x, y, probs = c(0.025, 0.5, 0.975)) {
  diff_vec <- as.vector(outer(x, y, "-"))
  list(
    p_x_gt_y = mean(diff_vec > 0),
    p_y_gt_x = mean(diff_vec < 0),
    diff_ci = quantile(diff_vec, probs = probs, na.rm = TRUE),
    diff_mean = mean(diff_vec, na.rm = TRUE)
  )
}

poe_compare_models <- function(draws_df, models, draw_col = "average_wtp") {
  get_draws <- function(m) {
    draws_df |> filter(model == m) |> arrange(.draw) |> pull(!!draw_col)
  }

  combn(models, 2, simplify = FALSE) |>
    map_dfr(function(pair) {
      res <- poe_compare_vec(get_draws(pair[1]), get_draws(pair[2]))
      tibble(
        model_1 = pair[1],
        model_2 = pair[2],
        p_m1_gt_m2 = res$p_x_gt_y,
        p_m2_gt_m1 = res$p_y_gt_x,
        diff_mean = res$diff_mean,
        diff_q2.5 = unname(res$diff_ci[1]),
        diff_q50 = unname(res$diff_ci[2]),
        diff_q97.5 = unname(res$diff_ci[3])
      )
    })
}

poe_compare_by_param <- function(draws_df, models) {
  params <- unique(draws_df$parameter)
  map_dfr(params, function(pname) {
    sub <- draws_df |> filter(parameter == pname)
    poe_compare_models(sub, models) |>
      mutate(parameter = pname, poe_p_value = 2 * pmin(p_m1_gt_m2, p_m2_gt_m1))
  })
}

# Wetland SBC Poe Tests (Logit vs MRP)

wtp_poststrat100 <- readRDS("output/wetlands/ImputedWTPWetlands100.rds")

df_draws_wetland <- wtp_poststrat100 |>
  filter(str_detect(model, "exp"), !str_detect(model, "percent")) |>
  filter(sample == "full") |>
  mutate(
    model = case_when(
      model == "Logit exp" ~ "Logit",
      model == "MRP exp" ~ "MRP",
      TRUE ~ "none"
    ),
    province = if_else(
      province == "Priaries",
      "All 3 Prairie Provinces",
      province
    ),
    province = fct_relevel(
      as.factor(province),
      "All 3 Prairie Provinces",
      "Manitoba",
      "Saskatchewan",
      "Alberta"
    )
  ) |>
  group_by(model, province, .draw) |>
  summarise(
    average_wtp = mean(average_wtp * count) / mean(count),
    .groups = "drop"
  ) |>
  rename(parameter = province)

poe_wetland <- poe_compare_by_param(df_draws_wetland, c("Logit", "MRP"))
poe_wetland

saveRDS(
  list(composite = poe_wetland),
  file = "output/wetlands/PoeTest_Wetland.rds"
)
