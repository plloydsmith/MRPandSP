# =============================================================================
# Poe et al. (2005) Combinatorial WTP Comparison Tests
# Compares WTP distributions across SRD model types (MRP, MNL, RPL)
# for both composite WTP and per-attribute WTP.
#
# Inputs:  output/srd/ImputedWTP_SRD.rds
# Outputs: output/srd/PoeTest_SRD.rds
# =============================================================================

# --- Load imputed WTP (if not already in environment) -------------------------

if (!exists("wtp_poststrat")) {
  wtp_poststrat <- readRDS("output/srd/ImputedWTP_SRD.rds")
}

# --- Poe comparison functions -------------------------------------------------

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

rename_models <- function(data) {
  data |>
    mutate(
      model = case_when(
        model == "rpl mean" ~ "RPL",
        model == "mnl" ~ "MNL",
        model == "mrp" ~ "MRP",
        TRUE ~ model
      )
    )
}

exclude_pattern <- "mrp program|mrp level2|same|conditional"

# --- Composite WTP Poe test (25% improvement in all attributes) ---------------

wtp_cv <- wtp_poststrat |>
  select(
    id,
    province,
    income_group,
    education_group,
    age_gender,
    count,
    .draw,
    parameter,
    wtp,
    model
  ) |>
  filter(!is.na(wtp)) |>
  pivot_wider(names_from = "parameter", values_from = "wtp") |>
  mutate(wtp = (sturgeon + habitat + waterfowl + muskrat) * 25) |>
  filter(!str_detect(model, exclude_pattern)) |>
  rename_models()

wtp_draws_cv <- wtp_cv |>
  filter(model %in% c("MRP", "MNL", "RPL")) |>
  group_by(model, .draw) |>
  summarise(average_wtp = mean(wtp * count) / mean(count), .groups = "drop")

poe_composite <- poe_compare_models(wtp_draws_cv, c("MRP", "MNL", "RPL"))
poe_composite

# --- Per-attribute Poe test ---------------------------------------------------

label_attributes <- function(x) {
  case_when(
    x == "waterfowl" ~ "Waterfowl population",
    x == "sturgeon" ~ "Lake sturgeon",
    x == "muskrat" ~ "Muskrat abundance",
    x == "habitat" ~ "Good ecological condition habitat",
    TRUE ~ x
  )
}

wtp_param_draws <- wtp_poststrat |>
  rename_models() |>
  filter(model %in% c("MRP", "RPL"), !str_detect(model, exclude_pattern)) |>
  group_by(model, parameter, .draw) |>
  summarise(average_wtp = mean(wtp * count) / mean(count), .groups = "drop") |>
  mutate(parameter = label_attributes(parameter))

poe_attributes <- poe_compare_by_param(wtp_param_draws, c("MRP", "RPL"))
poe_attributes

saveRDS(
  list(composite = poe_composite, attributes = poe_attributes),
  file = "output/srd/PoeTest_SRD.rds"
)
