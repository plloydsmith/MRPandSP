# =============================================================================
# Modify Choice Data for SRD Analysis
# Reads the raw SRD data, normalises cost and attribute columns
# (difference from SQ, scaled), and saves the analysis-ready dataset.
#
# Inputs:  data/derived/SRDModified.csv
# Outputs: data/derived/SRDClean.csv
# =============================================================================

if (!exists("cost_multiplier_linear")) {
  cost_multiplier_linear <- 100
}
if (!exists("attribute_multiplier")) {
  attribute_multiplier <- 10
}

df_ce <- read_csv("data/derived/SRDModified.csv") |>
  mutate(
    across(starts_with("cost_"), ~ . / cost_multiplier_linear),
    across(starts_with("sturgeon"), ~ (.x - sturgeon_1) / attribute_multiplier),
    across(
      starts_with("waterfowl"),
      ~ (.x - waterfowl_1) / attribute_multiplier
    ),
    across(starts_with("muskrat"), ~ (.x - muskrat_1) / attribute_multiplier),
    across(starts_with("habitat"), ~ (.x - habitat_1) / attribute_multiplier)
  ) |>
  select(-c(sturgeon_1, waterfowl_1, muskrat_1, habitat_1, cost_1))

write_csv(df_ce, "data/derived/SRDClean.csv")
