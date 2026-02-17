# =============================================================================
# Graph WTP Results
# Produces 4 figures: main + appendix versions of province and restrict plots.
# =============================================================================

wtp_poststrat100 <- readRDS("output/wetlands/ImputedWTPWetlands100.rds")

text_size <- 24

# --- Plot helper --------------------------------------------------------------

plot_wtp <- function(data, fill_var, fill_label, x_limits = c(150, 700)) {
  label_data <- data |>
    group_by(province, model, sample) |>
    summarise(
      average_wtp = mean(average_wtp * count) / mean(count),
      .groups = "drop"
    )

  data |>
    group_by(model, province, sample, .draw) |>
    summarise(
      average_wtp = mean(average_wtp * count) / mean(count),
      .groups = "drop"
    ) |>
    ggplot(aes(
      x = average_wtp,
      y = province,
      fill = .data[[fill_var]],
      colour = .data[[fill_var]]
    )) +
    stat_slab(alpha = .3) +
    stat_pointinterval(
      position = position_dodge(width = .4, preserve = "single")
    ) +
    geom_text_repel(
      data = label_data,
      aes(label = scales::dollar(round(average_wtp, 0)), size = 12),
      nudge_x = 50,
      nudge_y = .25
    ) +
    scale_x_continuous(labels = scales::dollar_format(), limits = x_limits) +
    labs(fill = fill_label, x = "WTP for wetland restoration program", y = "") +
    theme_bw() +
    guides(colour = "none", size = "none") +
    theme(
      legend.text = element_text(size = text_size),
      legend.title = element_text(size = text_size),
      axis.title.x = element_text(size = text_size),
      axis.text.y = element_text(size = text_size),
      axis.text.x = element_text(size = text_size),
      strip.text = element_text(size = text_size)
    )
}

# --- Helper: prepare province labels ------------------------------------------

prep_province <- function(data) {
  data |>
    mutate(
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
    )
}

# --- Main figures (exp models) ------------------------------------------------

df_exp <- wtp_poststrat100 |>
  filter(str_detect(model, "exp"), !str_detect(model, "percent")) |>
  mutate(
    model = case_when(
      model == "Logit exp" ~ "Logit",
      model == "MRP exp" ~ "MRP",
      TRUE ~ "none"
    )
  )

# Province comparison (Logit vs MRP, full sample)
p1 <- df_exp |>
  filter(sample == "full") |>
  prep_province() |>
  plot_wtp("model", "Model")

ggsave(p1, file = "figs/wetland_province.png", width = 15)

# Restriction comparison (MRP only, full vs validity)
p2 <- df_exp |>
  filter(str_detect(model, "MRP")) |>
  mutate(
    sample = if_else(sample == "full", "Full Sample", "Validity Sample")
  ) |>
  prep_province() |>
  plot_wtp("sample", "Sample", x_limits = c(0, 700))

ggsave(p2, file = "figs/wetland_restrict.png", width = 15)

# --- Appendix figures (linear models) -----------------------------------------

df_lin <- wtp_poststrat100 |>
  filter(!str_detect(model, "exp"), !str_detect(model, "percent"))

p3 <- df_lin |>
  filter(sample == "full") |>
  prep_province() |>
  plot_wtp("model", "Model")

ggsave(p3, file = "figs/wetland_province_appendix.png", width = 15)

p4 <- df_lin |>
  filter(str_detect(model, "MRP")) |>
  mutate(
    sample = if_else(sample == "full", "Full Sample", "Validity Sample")
  ) |>
  prep_province() |>
  plot_wtp("model", "Model", x_limits = c(-100, 700))

ggsave(p4, file = "figs/wetland_restrict_appendix.png", width = 15)
