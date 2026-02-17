# =============================================================================
# Graph WTP Results for SRD Models
# Produces 6 figures: mean WTP, per-attribute WTP, MRP vs RPL distributions,
# province/education breakdowns, and individual cell examples.
#
# Inputs:  output/srd/ImputedWTP_SRD.rds
# Outputs: figs/srd_*.png
# =============================================================================

# --- Load imputed WTP (if not already in environment) -------------------------

if (!exists("wtp_poststrat")) {
  wtp_poststrat <- readRDS("output/srd/ImputedWTP_SRD.rds")
}

text_size <- 24
plot_colors <- c("MRP" = "#00BFC4", "RPL" = "#F8766D", "MNL" = "#7CAE00")

theme_wtp <- theme_bw() +
  theme(
    legend.text = element_text(size = text_size),
    legend.title = element_text(size = text_size),
    axis.title.x = element_text(size = text_size),
    axis.title.y = element_text(size = text_size),
    axis.text.y = element_text(size = text_size),
    axis.text.x = element_text(size = text_size),
    strip.text = element_text(size = text_size)
  )

plot_slab <- function(
  data,
  x_var = "average_wtp",
  y_var,
  fill_var = NULL,
  x_lab,
  label_data = NULL,
  x_limits = NULL,
  label_digits = 0
) {
  p <- data |>
    ggplot(aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      fill = if (!is.null(fill_var)) .data[[fill_var]],
      colour = if (!is.null(fill_var)) .data[[fill_var]]
    )) +
    stat_slab(alpha = .3, side = "top") +
    stat_pointinterval(
      side = "bottom",
      position = position_dodge(width = .4, preserve = "single"),
      .width = c(0.66, 0.95)
    ) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(fill = "Model", x = x_lab, y = "") +
    guides(colour = "none", size = "none") +
    scale_fill_manual(values = plot_colors) +
    scale_colour_manual(values = plot_colors) +
    theme_wtp

  if (!is.null(label_data)) {
    p <- p +
      geom_text_repel(
        data = label_data,
        aes(
          label = scales::dollar(round(average_wtp, label_digits)),
          size = text_size
        ),
        nudge_y = .15
      )
  }

  if (!is.null(x_limits)) {
    p <- p + xlim(x_limits)
  }

  p
}

label_attributes <- function(x) {
  case_when(
    x == "waterfowl" ~ "Waterfowl population",
    x == "sturgeon" ~ "Lake sturgeon",
    x == "muskrat" ~ "Muskrat abundance",
    x == "habitat" ~ "Good ecological condition habitat",
    TRUE ~ x
  )
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

wtp_poststrat_cv <- wtp_poststrat |>
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
  rename_models()

df_graph <- wtp_poststrat_cv |>
  filter(model %in% c("RPL", "MNL", "MRP"))

df_summary <- df_graph |>
  group_by(model, .draw) |>
  summarise(average_wtp = mean(wtp * count) / mean(count), .groups = "drop") |>
  mutate(model = fct_relevel(factor(model), c("MRP", "MNL")))

df_labels <- df_graph |>
  group_by(model) |>
  summarise(average_wtp = mean(wtp * count) / mean(count), .groups = "drop")

p <- plot_slab(
  df_summary,
  y_var = "model",
  x_lab = "WTP for 25% improvement in all SRD attributes",
  label_data = df_labels
)
ggsave(p, file = "figs/srd_mean_all.png", width = 15)

df_graph <- wtp_poststrat |>
  rename_models() |>
  filter(model %in% c("MRP", "RPL"))

df_summary <- df_graph |>
  group_by(model, parameter, .draw) |>
  summarise(average_wtp = mean(wtp * count) / mean(count), .groups = "drop") |>
  mutate(parameter = label_attributes(parameter))

df_labels <- df_graph |>
  mutate(parameter = label_attributes(parameter)) |>
  group_by(model, parameter) |>
  summarise(average_wtp = mean(wtp * count) / mean(count), .groups = "drop")

p <- plot_slab(
  df_summary,
  y_var = "parameter",
  fill_var = "model",
  x_lab = "MWTP for a 1% improvement in each attribute",
  label_data = df_labels,
  label_digits = 2
)
ggsave(p, file = "figs/srd_mean_parameters.png", width = 15)

df_rpl <- wtp_poststrat_cv |>
  filter(model == "rpl individual") |>
  group_by(model, .draw) |>
  summarise(
    average_wtp = mean(wtp),
    count = 1,
    .groups = "drop"
  )

df_mrp_cells <- wtp_poststrat_cv |>
  filter(model == "MRP") |>
  group_by(model, province, age_gender, income_group, education_group, id) |>
  summarise(
    average_wtp = mean(wtp),
    count = round(mean(count), 0),
    .groups = "drop"
  )

p <- bind_rows(df_mrp_cells, df_rpl) |>
  group_by(model) |>
  mutate(percent = count / sum(count)) |>
  plot_slab(
    y_var = "model",
    x_lab = "WTP for 25% improvement in all SRD attributes"
  ) +
  aes(weight = percent)

ggsave(p, file = "figs/srd_distribution_mrp_rpl.png", width = 15)

df_mrp_cv <- wtp_poststrat_cv |>
  filter(str_detect(model, "MRP"))

breakdown_specs <- tribble(
  ~group_var        , ~output_file             ,
  "province"        , "figs/srd_province.png"  ,
  "education_group" , "figs/srd_education.png"
)

walk2(breakdown_specs$group_var, breakdown_specs$output_file, \(gvar, outfile) {
  df_summary <- df_mrp_cv |>
    group_by(model, .data[[gvar]], .draw) |>
    summarise(
      average_wtp = mean(wtp * count) / mean(count),
      .groups = "drop"
    ) |>
    mutate(!!gvar := reorder(as.factor(.data[[gvar]]), average_wtp, FUN = mean))

  df_labels <- df_mrp_cv |>
    group_by(model, .data[[gvar]]) |>
    summarise(average_wtp = mean(wtp * count) / mean(count), .groups = "drop")

  p <- plot_slab(
    df_summary,
    y_var = gvar,
    x_lab = "WTP for 25% improvement in all SRD attributes",
    label_data = df_labels
  )
  ggsave(p, file = outfile, width = 15)
})

cell_specs <- tribble(
  ~education                                                    , ~income             , ~age_gender            , ~province , ~label ,
  "High.school.or.less"                                         , "$0-$29,999"        , "18.to.34.years_Woman" ,
  "Newfoundland.and.Labrador"                                   ,
  "18-34 year old women, income $0-30k, HS or less in NL"       ,
  "Advanced.degree"                                             , "$150,000.and.over" , "65.and.over_Woman"    ,
  "British.Columbia"                                            ,
  "65+ year old women, income $150,000+, Advanced degree in BC"
)

cell_plots <- pmap(
  cell_specs,
  \(education, income, age_gender, province, label) {
    df_cell <- df_mrp_cv |>
      filter(
        education_group == education,
        income_group == income,
        age_gender == !!age_gender,
        province == !!province
      )

    df_summary <- df_cell |>
      group_by(model, .draw) |>
      summarise(average_wtp = mean(wtp * count) / mean(count), .groups = "drop")

    df_labels <- df_cell |>
      group_by(model) |>
      summarise(average_wtp = mean(wtp * count) / mean(count), .groups = "drop")

    plot_slab(
      df_summary,
      y_var = "model",
      x_lab = "",
      label_data = df_labels,
      x_limits = c(-155, 450)
    ) +
      geom_text(x = 150, y = 1.7, label = label, size = 4)
  }
)

p <- cell_plots[[1]] /
  cell_plots[[2]] +
  plot_annotation() &
  labs(x = "WTP for 25% improvement in all SRD attributes")

ggsave(p, file = "figs/srd_individual.png", width = 15)
