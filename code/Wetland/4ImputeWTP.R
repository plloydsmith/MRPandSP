# =============================================================================
# Impute WTP from Fitted Wetland Models
# Reads fitted model objects, extracts posterior WTP draws using either
# spread_draws (logit) or add_linpred_draws (MRP), and combines results.
# =============================================================================

n_iter <- 1000
wtp_rescale <- 100

foldername <- paste0("output/wetlands/scale", wtp_rescale, "/")

# --- Load poststratification table --------------------------------------------

df_poststrat <- read_csv("data/derived/PoststratCountsWetland.csv") |>
  mutate(age_gender = paste0(age_group, "_", gender)) |>
  summarise(
    count = sum(count),
    .by = c(province, age_gender, income_group, education_group)
  ) |>
  mutate(bid = 0, vote_yes = 0) |>
  filter(province %in% c("Alberta", "Saskatchewan", "Manitoba"))

# --- Helper: extract WTP draws from a single model ---------------------------

extract_wtp_draws <- function(
  model_path,
  model_label,
  province_label,
  sample_label,
  poststrat = NULL,
  use_exp = FALSE
) {
  fit <- readRDS(model_path)

  if (is.null(poststrat)) {
    # Logit model: extract fixed-effect intercept
    fit |>
      spread_draws(b_program_Intercept, ndraws = n_iter) |>
      mutate(
        average_wtp = if (use_exp) {
          exp(b_program_Intercept) * wtp_rescale
        } else {
          b_program_Intercept * wtp_rescale
        },
        count = 1,
        model = model_label,
        province = province_label,
        sample = sample_label
      ) |>
      select(sample, province, model, count, .draw, average_wtp)
  } else {
    # MRP model: predict on poststrat cells
    fit |>
      add_linpred_draws(
        newdata = poststrat,
        value = "wtp",
        nlpar = "program",
        ndraws = n_iter
      ) |>
      as.data.frame() |>
      mutate(
        wtp = if (use_exp) exp(wtp) * wtp_rescale else wtp * wtp_rescale
      ) |>
      ungroup() |>
      summarise(
        average_wtp = mean(wtp),
        .by = c(
          province,
          age_gender,
          income_group,
          education_group,
          count,
          .draw
        )
      ) |>
      mutate(
        model = model_label,
        province = province_label,
        sample = sample_label
      )
  }
}

# --- Define model specifications ----------------------------------------------

# Each spec: prefix in filename, label, whether it uses exp(), whether it's MRP
model_specs <- tribble(
  ~prefix                     , ~label      , ~use_exp , ~is_mrp ,
  "wtps_sbc"                  , "Logit"     , FALSE    , FALSE   ,
  "wtps_sbc_mrp_province"     , "MRP"       , FALSE    , TRUE    ,
  "wtps_sbc_exp"              , "Logit exp" , TRUE     , FALSE   ,
  "wtps_sbc_mrp_province_exp" , "MRP exp"   , TRUE     , TRUE
)

model_specs_prov <- tribble(
  ~prefix            , ~label      , ~use_exp , ~is_mrp ,
  "wtps_sbc"         , "Logit"     , FALSE    , FALSE   ,
  "wtps_sbc_mrp"     , "MRP"       , FALSE    , TRUE    ,
  "wtps_sbc_exp"     , "Logit exp" , TRUE     , FALSE   ,
  "wtps_sbc_mrp_exp" , "MRP exp"   , TRUE     , TRUE
)

province_list <- c("Alberta", "Saskatchewan", "Manitoba")
sample_list <- c("full", "restrict")

# --- Extract draws: all-province models ---------------------------------------

df_models <- pmap_dfr(model_specs, function(prefix, label, use_exp, is_mrp) {
  map_dfr(sample_list, function(samp) {
    model_path <- paste0(foldername, prefix, "_all_wetland_", samp, ".rds")
    extract_wtp_draws(
      model_path,
      label,
      province_label = "Priaries",
      sample_label = samp,
      use_exp = use_exp,
      poststrat = if (is_mrp) df_poststrat else NULL
    )
  })
})

# --- Extract draws: per-province models ---------------------------------------

df_models_province <- pmap_dfr(
  model_specs_prov,
  function(prefix, label, use_exp, is_mrp) {
    expand_grid(province = province_list, sample = sample_list) |>
      pmap_dfr(function(province, sample) {
        model_path <- paste0(
          foldername,
          prefix,
          "_",
          province,
          "_wetland_",
          sample,
          ".rds"
        )
        extract_wtp_draws(
          model_path,
          label,
          province_label = province,
          sample_label = sample,
          use_exp = use_exp,
          poststrat = if (is_mrp) {
            df_poststrat |> filter(province == !!province)
          } else {
            NULL
          }
        )
      })
  }
)

# --- Combine and save ---------------------------------------------------------

wtp_poststrat <- bind_rows(df_models, df_models_province) |>
  mutate(
    income_group = fct_relevel(
      as.factor(income_group),
      "$150,000 and over",
      "$125,000-$149,999",
      "$100,000-$124,999",
      "$75,000-$99,999",
      "$50,000-$74,999",
      "$25,000-$49,999",
      "$0-$25,000"
    ),
    education_group = fct_relevel(
      as.factor(education_group),
      "Advanced degree",
      "Bachelors degree",
      "Some University/College",
      "Vocational/Trade/Technical School",
      "High school or less"
    ),
    province = fct_relevel(
      as.factor(province),
      "Alberta",
      "Saskatchewan",
      "Manitoba",
      "Priaries"
    )
  ) |>
  separate_wider_delim(
    age_gender,
    names = c("age_group", "gender"),
    delim = "_",
    cols_remove = FALSE
  )

saveRDS(
  wtp_poststrat,
  file = paste0("output/wetlands/ImputedWTPWetlands", wtp_rescale, ".rds")
)
