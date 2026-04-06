#!/usr/bin/env Rscript

# Check CSV worksheets for formatting compliance
# This script validates variables.csv and variable_details.csv against
# formatting standards and reports any violations found.
#
# Usage:
#   Rscript exec/check-worksheets.R                              # all variables
#   Rscript exec/check-worksheets.R --subject "Ethnicity,Language,Migration"
#   Rscript exec/check-worksheets.R --variables "SDCGCGT,SDCFIMM"
#   Rscript exec/check-worksheets.R --subject Smoking --variables "COPD_Emph_der"
#
# Exit codes:
#   0 - No formatting violations found
#   1 - Formatting violations detected

suppressPackageStartupMessages({
  library(cchsflow)
  library(cli)
})

variables_path <- "inst/extdata/variables.csv"
variable_details_path <- "inst/extdata/variable_details.csv"

# Parse scope arguments
scope_args <- parse_scope_args(commandArgs(trailingOnly = TRUE))
scope <- scope_worksheets(
  variables_path, variable_details_path,
  variables = scope_args$variables,
  subjects = scope_args$subjects
)

cli_h1("Checking CSV worksheet formatting")
if (scope$scoped) {
  cli_alert_info("Scope: {scope$scope_desc}")
}

# Check variables.csv
cli_alert_info("Checking variables.csv...")
variables_sheet_errors <- check_worksheet(scope$variables_path, "variables")
n_variables_sheet_errors <- length(variables_sheet_errors)
if (n_variables_sheet_errors > 0) {
  cli_alert_danger("Found {cli::no(n_variables_sheet_errors)} error{?s}")
} else {
  cli_alert_success("Found {cli::no(n_variables_sheet_errors)} error{?s}")
}
cli_text("")

# Check variable_details.csv
cli_alert_info("Checking variable_details.csv...")
variable_details_errors <- check_worksheet(
  scope$variable_details_path, "variable_details")
n_variable_details_errors <- length(variable_details_errors)
if (n_variable_details_errors > 0) {
  cli_alert_danger("Found {cli::no(n_variable_details_errors)} error{?s}")
} else {
  cli_alert_success("Found {cli::no(n_variable_details_errors)} error{?s}")
}

# Check recode block overlap in variable_details.csv
cli_alert_info("Checking variable_details.csv recode block consistency...")
recode_block_errors <- check_recode_blocks(scope$variable_details_path)
n_recode_block_errors <- length(recode_block_errors)
if (n_recode_block_errors > 0) {
  cli_alert_danger("Found {cli::no(n_recode_block_errors)} error{?s}")
} else {
  cli_alert_success("Found {cli::no(n_recode_block_errors)} error{?s}")
}
cli_text("")

all_errors <- purrr::flatten(
  list(variables_sheet_errors, variable_details_errors, recode_block_errors))

# Report results
n_all_errors <- length(all_errors)
if (n_all_errors == 0) {
  cli_rule()
  cli_alert_success("All worksheets are properly formatted!")
  cli_rule()
  quit(status = 0)
} else {
  cli_rule()
  cli_alert_danger("Found {cli::no(n_all_errors)} formatting violation{?s}")
  cli_rule()

  # Display each error
  for (i in seq_along(all_errors)) {
    error <- all_errors[[i]]
    cli_alert_danger("{i}. {error$message}")
  }

  cli_text("")

  cli_alert_info("To fix these issues, run: {.run Rscript exec/fix-worksheets.R}")
  quit(status = 1)
}
