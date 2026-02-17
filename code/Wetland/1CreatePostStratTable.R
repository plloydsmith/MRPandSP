# =============================================================================
# Build Poststratification Table from 2021 Census PUMF
# Recodes raw census variables into the demographic groups used for MRP,
# then aggregates weighted counts and writes to CSV.
#
# Census code labels:
#   PR:     10=NL, 11=PE, 12=NS, 13=NB, 24=QC, 35=ON, 46=MB, 47=SK, 48=AB,
#           59=BC, 70=Territories
#   AGEGRP: 7-10=18-34, 11-14=35-54, 15-21=55+
#   HDGREE: 1-2=HS or less, 3-5=Vocational, 6-8=Some Uni/College,
#           9=Bachelors, 10-13=Advanced
#   HHInc:  1-9=$0-25k, 10-14=$25-50k, 15-19=$50-75k, 20-24=$75-100k,
#           25-27=$100-125k, 28-29=$125-150k, 30-33=$150k+
# =============================================================================

df_census <- read_csv("data/raw/data_donnees_2021_ind_v2.csv") |>
  select(
    gender = Gender,
    province = PR,
    age_group = AGEGRP,
    income_group = HHInc,
    education_group = HDGREE,
    weight = WEIGHT
  ) |>
  mutate(
    gender = if_else(gender == 1, "Woman", "Man"),
    province = case_when(
      province == 10 ~ "Newfoundland and Labrador",
      province == 11 ~ "Prince Edward Island",
      province == 12 ~ "Nova Scotia",
      province == 13 ~ "New Brunswick",
      province == 24 ~ "Quebec",
      province == 35 ~ "Ontario",
      province == 46 ~ "Manitoba",
      province == 47 ~ "Saskatchewan",
      province == 48 ~ "Alberta",
      province == 59 ~ "British Columbia",
      province == 70 ~ "Territories",
      TRUE ~ "other"
    ),
    age_group = case_when(
      age_group %in% 7:10 ~ "18 to 34 years",
      age_group %in% 11:14 ~ "35 to 54 years",
      age_group %in% 15:21 ~ "55 and over",
      TRUE ~ "other"
    ),
    income_group = case_when(
      income_group %in% 1:9 ~ "$0-$25,000",
      income_group %in% 10:14 ~ "$25,000-$49,999",
      income_group %in% 15:19 ~ "$50,000-$74,999",
      income_group %in% 20:24 ~ "$75,000-$99,999",
      income_group %in% 25:27 ~ "$100,000-$124,999",
      income_group %in% 28:29 ~ "$125,000-$149,999",
      income_group %in% 30:33 ~ "$150,000 and over",
      TRUE ~ "other"
    ),
    education_group = case_when(
      education_group %in% 1:2 ~ "High school or less",
      education_group %in% 3:5 ~ "Vocational/Trade/Technical School",
      education_group %in% 6:8 ~ "Some University/College",
      education_group == 9 ~ "Bachelors degree",
      education_group %in% 10:13 ~ "Advanced degree",
      TRUE ~ "other"
    )
  ) |>
  filter(
    age_group != "other",
    income_group != "other",
    education_group != "other"
  ) |>
  summarise(
    count = sum(weight),
    .by = c(gender, province, age_group, income_group, education_group)
  )

write_csv(df_census, "data/derived/PoststratCountsWetland.csv")
