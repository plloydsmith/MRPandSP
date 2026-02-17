# =============================================================================
# Create Model Estimate and Diagnostic Tables for Wetland Models
# Produces modelsummary tables and MCMC diagnostic tables (Rhat, ESS).
# =============================================================================

library(kableExtra)

wtp_rescale <- 100
full_path <- paste0("output/wetlands/scale", wtp_rescale, "/")

# --- Load all model files -----------------------------------------------------

model_list <- dir(full_path, pattern = "\\.rds$", full.names = TRUE)
models <- map(model_list, readRDS)
names(models) <- model_list |>
  str_remove(full_path) |>
  str_remove("wtps_sbc_") |>
  str_remove("\\.rds$")

# --- Coefficient rename map ---------------------------------------------------

coef_rename <- c(
  "theta" = "Scale ($\\lambda$)",
  "program" = "Program constant",
  "sd_age_gender" = "SD age-gender",
  "sd_education_group" = "SD education",
  "sd_income_group" = "SD income",
  "sd_province" = "SD province"
)

# --- Helper: clean posterior summary into modelsummary format ------------------

clean_model_summary <- function(fit) {
  tidy <- posterior_summary(fit) |>
    as.data.frame() |>
    tibble::rownames_to_column("term") |>
    filter(grepl("^b_|^sd_", term)) |>
    mutate(
      term = str_remove(term, "^b_"),
      term = str_remove(term, "_id_"),
      term = str_remove(term, "_Intercept"),
      term = str_remove(term, "__program")
    ) |>
    select(term, estimate = Estimate, std.error = Est.Error)

  out <- list(tidy = tidy, glance = data.frame(nObs = nrow(fit[["data"]])))
  class(out) <- "modelsummary_list"
  out
}

# --- Helper: create modelsummary table ----------------------------------------

create_estimate_table <- function(
  model_names,
  column_labels,
  output_file,
  notes
) {
  model_summaries <- map(model_names, \(nm) clean_model_summary(models[[nm]]))
  names(model_summaries) <- column_labels

  modelsummary(
    model_summaries,
    escape = FALSE,
    statistic = NULL,
    estimate = "{estimate} ({std.error})",
    output = output_file,
    coef_map = coef_rename,
    notes = notes
  )
}

# --- Helper: build MCMC diagnostics table -------------------------------------

build_diag_table <- function(model_names, short_labels, output_file) {
  diag_list <- map2(model_names, short_labels, function(mname, label) {
    posterior::summarise_draws(
      models[[mname]],
      "rhat",
      "ess_bulk",
      "ess_tail"
    ) |>
      as_tibble() |>
      rename(term = variable) |>
      filter(grepl("^b_|^sd_", term)) |>
      mutate(
        term = str_remove(term, "^b_"),
        term = str_remove(term, "_id_"),
        term = str_remove(term, "_Intercept"),
        term = str_remove(term, "__program"),
        rhat = round(rhat, 2),
        ess_bulk = as.integer(round(ess_bulk)),
        ess_tail = as.integer(round(ess_tail))
      ) |>
      select(
        term,
        !!paste0(label, "_rhat") := rhat,
        !!paste0(label, "_ess_bulk") := ess_bulk,
        !!paste0(label, "_ess_tail") := ess_tail
      )
  })

  diag_df <- reduce(diag_list, full_join, by = "term") |>
    mutate(Term = recode(term, !!!coef_rename)) |>
    filter(!is.na(Term)) |>
    arrange(Term) |>
    select(
      Term,
      all_of(unlist(map(short_labels, \(l) {
        paste0(l, c("_rhat", "_ess_bulk", "_ess_tail"))
      })))
    )

  saveRDS(diag_df, output_file)
}

# --- Province labels ----------------------------------------------------------

province_labels <- c(
  "Alberta",
  "Saskatchewan",
  "Manitoba",
  "All 3 Priarie Provinces"
)
short_labels <- c("AB", "SK", "MB", "All")

# --- Table specifications ----------------------------------------------------

table_specs <- tribble(
  ~model_names                                                                                                                                                    , ~output_est , ~output_diag , ~note_table_ref ,
  # Logit (full sample)
  list("exp_Alberta_wetland_full", "exp_Saskatchewan_wetland_full", "exp_Manitoba_wetland_full", "exp_all_wetland_full")                                          ,
  "tables/wetland_logit_model_estimates.md"                                                                                                                       ,
  "tables/wetland_logit_model_diagnostics.rds"                                                                                                                    ,
  "A5"                                                                                                                                                            ,

  # MRP (full sample)
  list("mrp_exp_Alberta_wetland_full", "mrp_exp_Saskatchewan_wetland_full", "mrp_exp_Manitoba_wetland_full", "mrp_province_exp_all_wetland_full")                 ,
  "tables/wetland_mrp_model_estimates.md"                                                                                                                         ,
  "tables/wetland_mrp_model_diagnostics.rds"                                                                                                                      ,
  "A6"                                                                                                                                                            ,

  # MRP (validity-restricted sample)
  list("mrp_exp_Alberta_wetland_restrict", "mrp_exp_Saskatchewan_wetland_restrict", "mrp_exp_Manitoba_wetland_restrict", "mrp_province_exp_all_wetland_restrict") ,
  "tables/wetland_restrict_model_estimates.md"                                                                                                                    ,
  "tables/wetland_restrict_model_diagnostics.rds"                                                                                                                 ,
  "A7"
)

# --- Generate all tables ------------------------------------------------------

pwalk(
  table_specs,
  function(model_names, output_est, output_diag, note_table_ref) {
    note_text <- paste0(
      "The program constant can be interpreted as the WTP for ",
      "a provincial wetland restoration program in hundreds of dollars because of ",
      "the cost rescaling for estimation. Parameter diagnostic metrics for R-hat and effective ",
      "sample size are provided in Table ",
      note_table_ref,
      "."
    )

    create_estimate_table(
      model_names = unlist(model_names),
      column_labels = province_labels,
      output_file = output_est,
      notes = list(note_text)
    )

    build_diag_table(
      model_names = unlist(model_names),
      short_labels = short_labels,
      output_file = output_diag
    )
  }
)
