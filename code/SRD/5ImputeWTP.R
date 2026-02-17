if (!exists("attribute_multiplier")) {
  attribute_multiplier <- 10
}

if (!exists("df_ce")) {
  df_ce <- read_csv("data/derived/SRDClean.csv")
}

df_poststrat <- read_csv("data/derived/PoststratCountsCanada.csv") |>
  mutate(age_gender = paste0(age_group, "_", gender)) |>
  summarise(
    count = sum(count),
    .by = c(province, age_gender, income_group, education_group)
  ) |>
  mutate(bid = 0, vote_yes = 0)

n_draws_plots <- 1001

models_ce <- c("wtps_srd_mnl", "wtps_srd_mrp", "wtps_srd_rpl")

models <- map(models_ce, \(m) {
  path <- paste0("output/srd/", m, ".rds")
  if (file.exists(path)) readRDS(path) else NULL
}) |>
  set_names(models_ce) |>
  compact()

df_poststrat <- df_poststrat |>
  mutate(across(
    c(age_gender, education_group, income_group, province),
    ~ str_replace_all(.x, " ", ".")
  ))

df_ce <- df_ce |>
  mutate(across(
    c(age_gender, education_group, income_group, province),
    ~ str_replace_all(.x, " ", ".")
  ))

get_fixed_draws <- function(fit) {
  as_tibble(fixef(fit, summary = FALSE)) |>
    mutate(.draw = row_number()) |>
    pivot_longer(-.draw, names_to = "parameter", values_to = "fixed") |>
    mutate(parameter = str_remove(parameter, "_Intercept")) |>
    filter(!(parameter %in% c("bcost", "bprogram"))) |>
    filter(.draw < n_draws_plots)
}

get_mrp_wtp <- function(fit, df_fixed) {
  extract_random <- function(fit, group_var, value_name) {
    pattern <- paste0("^r_", group_var, "__")
    as_draws_df(fit) |>
      select(.draw, matches(pattern)) |>
      pivot_longer(
        -.draw,
        names_to = c("parameter", group_var),
        names_pattern = paste0("r_", group_var, "__(.+)\\[(.+),Intercept\\]"),
        values_to = value_name
      )
  }

  df_age <- extract_random(fit, "age_gender", "random_age_gender")
  df_edu <- extract_random(fit, "education_group", "random_education_group")
  df_inc <- extract_random(fit, "income_group", "random_income_group")
  df_prov <- extract_random(fit, "province", "random_province")

  df_poststrat |>
    cross_join(df_fixed) |>
    left_join(df_age, by = c("age_gender", ".draw", "parameter")) |>
    left_join(df_edu, by = c("education_group", ".draw", "parameter")) |>
    left_join(df_inc, by = c("income_group", ".draw", "parameter")) |>
    left_join(df_prov, by = c("province", ".draw", "parameter")) |>
    mutate(
      wtp = (fixed +
        random_age_gender +
        random_education_group +
        random_income_group +
        random_province) *
        attribute_multiplier
    ) |>
    select(-bid, -vote_yes, -starts_with("random"), -fixed)
}

df_fixed <- get_fixed_draws(models[["wtps_srd_mnl"]])

wtp_mnl <- df_ce |>
  distinct(
    id,
    province,
    gender,
    age_group,
    income_group,
    education_group,
    age_gender
  ) |>
  cross_join(df_fixed) |>
  mutate(wtp = fixed * attribute_multiplier, model = "mnl", count = 1)

# --- RPL: population mean WTP ------------------------------------------------

df_fixed <- get_fixed_draws(models[["wtps_srd_rpl"]])

wtp_rpl_mean <- df_ce |>
  distinct(
    id,
    province,
    gender,
    age_group,
    income_group,
    education_group,
    age_gender
  ) |>
  cross_join(df_fixed) |>
  mutate(
    wtp = fixed * attribute_multiplier,
    model = "rpl mean",
    count = 1
  )

# --- RPL: individual-level WTP (fixed + random) ------------------------------

df_rpl_sd <- as_draws_df(models[["wtps_srd_rpl"]]) |>
  select(.draw, starts_with("sd_id__")) |>
  filter(.draw < n_draws_plots) |>
  pivot_longer(
    -.draw,
    names_to = "parameter",
    values_to = "sd_id"
  ) |>
  mutate(
    parameter = str_remove(parameter, "sd_id__") |> str_remove("_Intercept")
  )

id_random = df_ce$id[1]
set.seed(123)
wtp_rpl_individual <- df_fixed |>
  left_join(df_rpl_sd, by = c(".draw", "parameter")) |>
  mutate(
    id = id_random,
    random_draw = rnorm(n(), mean = 0, sd = sd_id),
    wtp = (fixed + random_draw) * attribute_multiplier,
    model = "rpl individual",
    count = 1
  ) |>
  left_join(
    df_ce |>
      distinct(
        id,
        province,
        gender,
        age_group,
        income_group,
        education_group,
        age_gender
      ),
    by = "id"
  ) |>
  select(-fixed, -sd_id, -random_draw)

# --- MRP: demographic cell-level WTP -----------------------------------------

df_fixed <- get_fixed_draws(models[["wtps_srd_mrp"]])

wtp_mrp <- get_mrp_wtp(models[["wtps_srd_mrp"]], df_fixed) |>
  mutate(model = "mrp")

# --- Combine all WTP draws ----------------------------------------------------

income_levels <- c(
  "$150,000.and.over",
  "$90,000-$149,999",
  "$70,000-$89,999",
  "$50,000-$69,999",
  "$30,000-$49,999",
  "$0-$29,999"
)
education_levels <- c(
  "Advanced.degree",
  "Bachelors.degree",
  "Some.University/College",
  "Vocational/Trade/Technical.School",
  "High.school.or.less"
)

wtp_poststrat <- bind_rows(
  wtp_mnl,
  wtp_rpl_mean,
  wtp_rpl_individual,
  wtp_mrp
) |>
  select(-age_group, -gender, -any_of("fixed")) |>
  mutate(
    income_group = fct_relevel(as.factor(income_group), income_levels),
    education_group = fct_relevel(as.factor(education_group), education_levels),
    parameter = str_sub(parameter, 2)
  ) |>
  separate_wider_delim(
    age_gender,
    names = c("age_group", "gender"),
    delim = "_",
    cols_remove = FALSE
  )

saveRDS(wtp_poststrat, file = "output/srd/ImputedWTP_SRD.rds")
