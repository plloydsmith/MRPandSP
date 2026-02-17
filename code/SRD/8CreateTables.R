# =============================================================================
# Create Model Estimate and Diagnostic Tables for SRD Models
# Produces modelsummary tables and MCMC diagnostic tables (Rhat, ESS).
#
# Inputs:  output/srd/wtps_srd_*.rds
# Outputs: tables/srd_main_model_estimates.md,
#          tables/srd_main_model_diagnostics.md
# =============================================================================

models_ce <- c("wtps_srd_mnl", "wtps_srd_mrp", "wtps_srd_rpl")
model_labels <- c("MNL", "MRP", "RPL")

models <- map(models_ce, \(m) readRDS(paste0("output/srd/", m, ".rds"))) |>
  set_names(models_ce)

coef_labels <- c(
  "sturgeon" = "Mean Sturgeon",
  "habitat" = "Mean Habitat",
  "waterfowl" = "Mean Waterfowl",
  "muskrat" = "Mean Muskrat",
  "cost" = "Scale ($\\lambda$)",
  "program" = "Program constant",
  "sd_bsturgeon" = "SD Sturgeon: individual",
  "sd_bhabitat" = "SD Habitat: individual",
  "sd_bwaterfowl" = "SD Waterfowl: individual",
  "sd_bmuskrat" = "SD Muskrat: individual",
  "sd_bprogram" = "SD Program: individual",
  "sd_age_gender__bsturgeon" = "SD Sturgeon: age-gender",
  "sd_education_group__bsturgeon" = "SD Sturgeon: education",
  "sd_income_group__bsturgeon" = "SD Sturgeon: income",
  "sd_province__bsturgeon" = "SD Sturgeon: province",
  "sd_age_gender__bhabitat" = "SD Habitat: age-gender",
  "sd_education_group__bhabitat" = "SD Habitat: education",
  "sd_income_group__bhabitat" = "SD Habitat: income",
  "sd_province__bhabitat" = "SD Habitat: province",
  "sd_age_gender__bwaterfowl" = "SD Waterfowl: age-gender",
  "sd_education_group__bwaterfowl" = "SD Waterfowl: education",
  "sd_income_group__bwaterfowl" = "SD Waterfowl: income",
  "sd_province__bwaterfowl" = "SD Waterfowl: province",
  "sd_age_gender__bmuskrat" = "SD Muskrat: age-gender",
  "sd_education_group__bmuskrat" = "SD Muskrat: education",
  "sd_income_group__bmuskrat" = "SD Muskrat: income",
  "sd_province__bmuskrat" = "SD Muskrat: province"
)

clean_model_summary <- function(fit) {
  tidy <- posterior_summary(fit) |>
    as.data.frame() |>
    tibble::rownames_to_column("term") |>
    filter(grepl("^b_|^sd_", term)) |>
    mutate(
      term = str_remove(term, "^b_b"),
      term = str_remove(term, "_id_"),
      term = str_remove(term, "_Intercept")
    ) |>
    select(term, estimate = Estimate, std.error = Est.Error)

  out <- list(tidy = tidy, glance = data.frame(nObs = nrow(fit[["data"]])))
  class(out) <- "modelsummary_list"
  out
}

model_summaries <- map(models_ce, \(m) clean_model_summary(models[[m]])) |>
  set_names(model_labels)

modelsummary(
  model_summaries[c("MNL", "RPL", "MRP")],
  fmt = 3,
  statistic = NULL,
  estimate = "{estimate} ({std.error})",
  output = "tables/srd_main_model_estimates.md",
  coef_map = coef_labels,
  notes = list(
    paste(
      "The standard deviation of the posterior distribution for the parameter",
      "estimates are presented in parentheses. For the RPL model, the standard",
      "deviation (SD) parameters are specified at the individual level. For the",
      "MRP model, the standard deviation (SD) parameters are specified at the",
      "respondent characteristic level and there are 10 age-gender levels, 5",
      "education group levels, 7 income levels, and 11 province levels.",
      "Parameter means on attribute variables are interpreted as marginal WTP",
      "for a 0.1% increase. Parameter diagnostic metrics for R-hat and",
      "effective sample size are provided in Table A4."
    )
  )
)

clean_term <- function(term) {
  term |>
    str_remove("^b_b") |>
    str_remove("_id_") |>
    str_remove("_Intercept")
}

diag_long <- imap_dfr(
  set_names(models_ce, model_labels),
  \(model_name, label) {
    posterior::summarise_draws(
      models[[model_name]],
      "rhat",
      "ess_bulk",
      "ess_tail"
    ) |>
      as_tibble() |>
      rename(term = variable) |>
      filter(grepl("^b_|^sd_", term)) |>
      mutate(
        term = clean_term(term),
        Term = recode(term, !!!coef_labels),
        Model = label,
        rhat = round(rhat, 2),
        ess_bulk = as.integer(round(ess_bulk)),
        ess_tail = as.integer(round(ess_tail))
      ) |>
      filter(!is.na(Term)) |>
      select(
        Model,
        Term,
        Rhat = rhat,
        `ESS(bulk)` = ess_bulk,
        `ESS(tail)` = ess_tail
      )
  }
) |>
  filter(!(is.na(Rhat) & is.na(`ESS(bulk)`) & is.na(`ESS(tail)`)))

diag_table_md <- diag_long |>
  mutate(across(everything(), \(x) ifelse(is.na(x), "", x))) |>
  knitr::kable(
    format = "pipe",
    align = c("l", "l", "r", "r", "r"),
    col.names = c("Model", "Term", "Rhat", "ESS(bulk)", "ESS(tail)")
  )

writeLines(diag_table_md, "tables/srd_main_model_diagnostics.md")
