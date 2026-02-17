# =============================================================================
# Wetlands Sample vs Population Comparison
# Computes demographic breakdowns (by province and overall) for population,
# full sample, and validity-restricted sample, then produces comparison plots.
# =============================================================================

rm(list = ls(all = TRUE))

# --- Load data ----------------------------------------------------------------

df_wetland <- read_csv("data/derived/WetlandsClean.csv")

df_poststrat <- read_csv("data/derived/PoststratCountsWetland.csv") |>
  filter(province %in% c("Alberta", "Saskatchewan", "Manitoba")) |>
  summarise(
    count = sum(count),
    .by = c(province, age_group, gender, income_group, education_group)
  )

total_population <- sum(df_poststrat$count)
pop_by_province <- df_poststrat |>
  summarise(count = sum(count), .by = province)

# --- Demographic variables to summarise --------------------------------------

demo_vars <- c(
  age_group = "age",
  gender = "gender",
  income_group = "income",
  education_group = "education"
)

# --- Helpers: compute percent by variable -------------------------------------

pct_by_province <- function(data, var, type_label, weight_col = NULL) {
  if (!is.null(weight_col)) {
    data |>
      mutate(prov_total = sum(.data[[weight_col]]), .by = province) |>
      summarise(
        percent = sum(.data[[weight_col]]) / mean(prov_total) * 100,
        .by = c(all_of(var), province)
      ) |>
      rename(group = all_of(var)) |>
      mutate(type = type_label)
  } else {
    data |>
      mutate(prov_total = n(), .by = province) |>
      summarise(
        percent = n() / mean(prov_total) * 100,
        .by = c(all_of(var), province)
      ) |>
      rename(group = all_of(var)) |>
      mutate(type = type_label)
  }
}

pct_overall <- function(data, var, type_label) {
  data |>
    summarise(percent = n() / nrow(data) * 100, .by = all_of(var)) |>
    rename(group = all_of(var)) |>
    mutate(type = type_label, province = "All 3")
}

compute_pcts_by_province <- function(data, weight_col = NULL) {
  map2_dfr(names(demo_vars), demo_vars, \(var, lbl) {
    pct_by_province(data, var, lbl, weight_col)
  })
}

compute_pcts_overall <- function(data) {
  map2_dfr(names(demo_vars), demo_vars, \(var, lbl) {
    pct_overall(data, var, lbl)
  })
}

# --- Population percentages --------------------------------------------------

df_pop_prov <- compute_pcts_by_province(df_poststrat, weight_col = "count")

df_pop_all <- df_pop_prov |>
  left_join(pop_by_province, join_by(province)) |>
  mutate(count = percent / 100 * count) |>
  summarise(
    percent = sum(count) / total_population * 100,
    .by = c(type, group)
  ) |>
  mutate(province = "All 3")

df_pop <- bind_rows(df_pop_prov, df_pop_all) |>
  mutate(comparison = "population")

# --- Sample percentages -------------------------------------------------------

df_sample_full <- bind_rows(
  compute_pcts_by_province(df_wetland),
  compute_pcts_overall(df_wetland)
) |>
  mutate(comparison = "sample (full)")

df_wetland_restrict <- df_wetland |>
  filter(conseq_none == 0, yea_say == 0)

df_sample_validity <- bind_rows(
  compute_pcts_by_province(df_wetland_restrict),
  compute_pcts_overall(df_wetland_restrict)
) |>
  mutate(comparison = "sample (validity)")

# --- Combine and set factor levels --------------------------------------------

education_levels <- c(
  "Advanced degree",
  "Bachelors degree",
  "Some University/College",
  "Vocational/Trade/Technical School",
  "High school or less"
)

income_levels <- c(
  "$150,000 and over",
  "$125,000-$149,999",
  "$100,000-$124,999",
  "$75,000-$99,999",
  "$50,000-$74,999",
  "$25,000-$49,999",
  "$0-$25,000"
)

df_all <- bind_rows(df_pop, df_sample_full, df_sample_validity) |>
  ungroup() |>
  mutate(
    type = fct_relevel(as.factor(type), "gender", "age", "income", "education"),
    group = fct_relevel(as.factor(group), c(education_levels, income_levels))
  )

# --- Plot helpers -------------------------------------------------------------

text_size <- 16

plot_province <- function(
  data,
  prov,
  shapes,
  title = prov,
  show_legend = FALSE,
  show_y = TRUE
) {
  p <- data |>
    filter(province == prov) |>
    mutate(group = as.factor(group)) |>
    ggplot(aes(y = percent, x = group, group = comparison)) +
    geom_point(aes(shape = comparison)) +
    scale_shape_manual(values = shapes) +
    geom_line(aes(linetype = comparison)) +
    facet_wrap(~type, nrow = 4, scales = "free") +
    ylim(0, 60) +
    coord_flip() +
    labs(fill = "", x = "", y = "Percent", title = title) +
    theme_bw() +
    theme(
      axis.title.x = element_text(size = text_size),
      strip.text = element_text(size = text_size)
    )

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p +
      theme(
        legend.text = element_text(size = text_size),
        legend.title = element_text(size = text_size)
      )
  }
  if (!show_y) {
    p <- p + theme(axis.text.y = element_blank())
  }
  p
}

plot_overall_comparison <- function(data, prov = "All 3", shapes) {
  data |>
    filter(province == prov) |>
    mutate(group = as.factor(group)) |>
    ggplot(aes(y = percent, x = group, group = comparison)) +
    geom_point(aes(shape = comparison)) +
    scale_shape_manual(values = shapes) +
    geom_line(aes(linetype = comparison)) +
    facet_wrap(~type, nrow = 1, scales = "free") +
    ylim(0, 60) +
    labs(fill = "", x = NULL, y = "Percent", title = "") +
    theme_bw() +
    theme(
      legend.text = element_text(size = text_size),
      legend.title = element_text(size = text_size),
      axis.text.x = element_text(angle = 45, hjust = 1, size = text_size),
      axis.title.x = element_text(size = text_size),
      strip.text = element_text(size = text_size)
    )
}

# --- Generate plots -----------------------------------------------------------

# Two-group comparison (population + full sample)
df_two <- df_all |> filter(comparison != "sample (validity)")

p4 <- plot_overall_comparison(df_two, shapes = c(16, 1))
ggsave(p4, file = "figs/wetland_comparison.png", width = 15)

p_prov <- plot_province(df_two, "Alberta", c(16, 1)) +
  plot_province(df_two, "Saskatchewan", c(16, 1), show_y = FALSE) +
  plot_province(
    df_two,
    "Manitoba",
    c(16, 1),
    show_y = FALSE,
    show_legend = TRUE
  )
ggsave(p_prov, file = "figs/wetland_comparison_province.png", width = 15)

# Three-group comparison (population + full sample + validity)
shapes_3 <- c(16, 1, 8)

p4_r <- plot_overall_comparison(df_all, shapes = shapes_3)
ggsave(p4_r, file = "figs/wetland_comparison_restrict.png", width = 15)

p_prov_r <- plot_province(df_all, "Alberta", shapes_3) +
  plot_province(df_all, "Saskatchewan", shapes_3, show_y = FALSE) +
  plot_province(
    df_all,
    "Manitoba",
    shapes_3,
    show_y = FALSE,
    show_legend = TRUE
  )
ggsave(
  p_prov_r,
  file = "figs/wetland_comparison_province_restrict.png",
  width = 15
)
