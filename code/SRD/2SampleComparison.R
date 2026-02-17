# =============================================================================
# SRD Sample vs Population Comparison
# Computes demographic breakdowns for population (poststrat) and sample,
# then produces a faceted comparison plot.
# =============================================================================

rm(list = ls(all = TRUE))

df_ce <- read_csv("data/derived/SRDClean.csv") |>
  distinct(id, province, age_group, gender, income_group, education_group)

df_poststrat <- read_csv("data/derived/PoststratCountsCanada.csv") |>
  mutate(age_gender = paste0(age_group, "_", gender)) |>
  summarise(
    count = sum(count),
    .by = c(province, age_group, gender, income_group, education_group)
  )

demo_vars <- c(
  province = "province",
  age_group = "age",
  gender = "gender",
  income_group = "income",
  education_group = "education"
)

pct_by_var <- function(data, var, type_label, weight_col = NULL) {
  if (!is.null(weight_col)) {
    total <- sum(data[[weight_col]])
    data |>
      summarise(
        percent = sum(.data[[weight_col]]) / total * 100,
        .by = all_of(var)
      ) |>
      rename(group = all_of(var)) |>
      mutate(type = type_label)
  } else {
    data |>
      summarise(percent = n() / nrow(data) * 100, .by = all_of(var)) |>
      rename(group = all_of(var)) |>
      mutate(type = type_label)
  }
}

df_pop <- map2_dfr(names(demo_vars), demo_vars, \(var, lbl) {
  pct_by_var(df_poststrat, var, lbl, weight_col = "count")
}) |>
  mutate(comparison = "population")

df_sample <- map2_dfr(names(demo_vars), demo_vars, \(var, lbl) {
  pct_by_var(df_ce, var, lbl)
}) |>
  mutate(comparison = "sample")

df_all <- bind_rows(df_pop, df_sample) |>
  mutate(
    type = fct_relevel(
      as.factor(type),
      "gender",
      "age",
      "province",
      "income",
      "education"
    ),
    group = fct_relevel(
      as.factor(group),
      "Advanced degree",
      "Bachelors degree",
      "Some University/College",
      "Vocational/Trade/Technical School",
      "High school or less",
      "$150,000 and over",
      "$90,000-$149,999",
      "$70,000-$89,999",
      "$50,000-$69,999",
      "$30,000-$49,999",
      "$0-$29,999"
    )
  )

text_size <- 16

p <- ggplot(df_all, aes(y = percent, x = group, group = comparison)) +
  geom_point(aes(shape = comparison)) +
  scale_shape_manual(values = c(16, 1)) +
  geom_line(aes(linetype = comparison)) +
  facet_wrap(~type, nrow = 1, scales = "free") +
  ylim(0, 60) +
  labs(fill = "", x = "", y = "Percent") +
  theme_bw() +
  theme(
    legend.text = element_text(size = text_size),
    legend.title = element_text(size = text_size),
    axis.title.y = element_text(size = text_size),
    axis.text.x = element_text(angle = 45, hjust = 1, size = text_size),
    strip.text = element_text(size = text_size)
  )

p
ggsave(p, file = "figs/srd_comparison.png", width = 15)
