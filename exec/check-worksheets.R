#!/usr/bin/env Rscript

# Check CSV worksheets for formatting compliance
# This script validates variables.csv and variable_details.csv against
# formatting standards and reports any violations found.
#
# Usage: Rscript exec/check-worksheets.R
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

cli_h1("Checking CSV worksheet formatting")

# Check variables.csv
cli_alert_info("Checking variables.csv...")
variables_sheet_errors <- check_worksheet(variables_path, "variables")
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
  variable_details_path, "variable_details")
n_variable_details_errors <- length(variable_details_errors)
if (n_variable_details_errors > 0) {
  cli_alert_danger("Found {cli::no(n_variable_details_errors)} error{?s}")
} else {
  cli_alert_success("Found {cli::no(n_variable_details_errors)} error{?s}")
}

all_errors <- purrr::flatten(
  list(variables_sheet_errors, variable_details_errors))

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
