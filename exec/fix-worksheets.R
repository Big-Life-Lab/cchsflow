#!/usr/bin/env Rscript

# Fix CSV worksheet formatting issues
# This script automatically corrects formatting violations in the repo's
# variables and variable details sheet
#
# Usage: Rscript exec/fix-worksheets.R
#
# Exit codes:
#   0 - Fixes applied successfully or no fixes needed
#   1 - Unable to fix (e.g., file not found, invalid CSV)

suppressPackageStartupMessages({
  library(cchsflow)
  library(cli)
})

# Constants
variables_path <- "inst/extdata/variables.csv"
variable_details_path <- "inst/extdata/variable_details.csv"

#' Get list of fixed errors
#'
#' @param errors The result of a \code{fix_worksheet} call
#'
#' @return the list of fixed errors
.keep_fixed_errors <- function(errors) {
  return(purrr::keep(errors, ~ .x$fixed == TRUE))
}

#' Get list of unfixed errors
#'
#' @param errors The result of a \code{fix_worksheet} call
#'
#' @return the list of unfixed errors
.keep_unfixed_errors <- function(errors) {
  return(purrr::keep(errors, ~.x$fixed == FALSE))
}

#' Give the user an update regarding fixing a worksheet
#'
#' @param fix_results The result of a \code{fix_worksheet} call
.log_fix_results <- function(fix_results) {
  num_fixed <- length(.keep_fixed_errors(fix_results))

  num_unfixed <- length(.keep_unfixed_errors(fix_results))

  if (length(fix_results) == 0) {
    cli_alert_success("No issues found - file already compliant")
  } else if (num_unfixed > 0) {
    cli_alert_danger("Unable to fix {num_unfixed} error{?s}:")
    unfixed_errors <- purrr::keep(fix_results, ~ .x$fixed == FALSE)
    for (err in unfixed_errors) {
      cli_text("  {cli::symbol$cross} {err$error_type}: {err$message}")
    }
  } else {
    cli_alert_success("Fixed {num_fixed} error{?s}")
  }
}

cli_h1("Fixing CSV worksheet formatting")

cli_text("")

cli_alert_info("Processing variables.csv...")

result_vars <- fix_worksheet(variables_path, "variables")

.log_fix_results(result_vars)

cli_text("")

cli_alert_info("Processing variable_details.csv...")

result_details <- fix_worksheet(variable_details_path, "variable_details")

.log_fix_results(result_details)

cli_text("")

# Final report
num_fixed <- length(.keep_fixed_errors(result_vars)) +
  length(.keep_fixed_errors(result_details))
num_unfixed <- length(.keep_unfixed_errors(result_vars)) +
  length(.keep_unfixed_errors(result_details))
success <- num_unfixed == 0
if (success) {
  cli_rule()
  if (num_fixed > 0) {
    cli_alert_success("Applied {num_fixed} total fix{?es}")
    cli_rule()
    cli_text("")
    cli_alert_info(
      "The worksheets have been updated. Please review the changes and commit them."
    )
  } else {
    cli_alert_success("All worksheets are already properly formatted!")
    cli_rule()
  }
  quit(status = 0)
} else {
  cli_rule()
  cli_alert_danger("Unable to fix all issues")
  cli_rule()
  cli_alert_info("Please review the errors above and fix them manually.")
  quit(status = 1)
}

