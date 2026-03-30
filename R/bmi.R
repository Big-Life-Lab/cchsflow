# ==============================================================================
# BMI (Body Mass Index) Functions — PUMF
# ==============================================================================
#
# Silver-tier derived variable functions using the v3 3-step architecture.
# PUMF functions operate on grouped (categorical midpoint) height/weight.
# Master equivalents are in bmi-master.R.

# Connor Gorber et al. (2008) bias-correction coefficients
BMI_CORRECTION_MALE <- list(intercept = -1.07575, slope = 1.07592)
BMI_CORRECTION_FEMALE <- list(intercept = -0.12374, slope = 1.05129)

#' Calculate Body Mass Index (BMI) — PUMF
#'
#' Calculates BMI from grouped height and weight variables on CCHS PUMF files
#' using the standard formula: weight (kg) / height (m)^2.
#'
#' @param HWTGHTM Height in metres (CCHS PUMF grouped variable).
#' @param HWTGWTK Weight in kilograms (CCHS PUMF grouped variable).
#' @param output_format Output missing data format: "tagged_na" (default) or
#'   "original".
#'
#' @return Numeric vector of BMI values. Missing inputs produce
#'   `haven::tagged_na("a")` (not applicable) or `haven::tagged_na("b")`
#'   (not stated), with NA::b taking priority when multiple inputs are missing.
#'
#' @examples
#' # Scalar
#' calculate_bmi(HWTGHTM = 1.75, HWTGWTK = 70)
#'
#' # Vector
#' calculate_bmi(
#'   HWTGHTM = c(1.75, 1.60, 996),
#'   HWTGWTK = c(70, 55, 70)
#' )
#'
#' # Dataframe
#' library(dplyr)
#' data.frame(HWTGHTM = c(1.75, 1.60), HWTGWTK = c(70, 55)) %>%
#'   mutate(bmi = calculate_bmi(HWTGHTM, HWTGWTK))
#'
#' @references
#' World Health Organization. (2000). Obesity: preventing and managing the
#' global epidemic. WHO Technical Report Series, 894.
#'
#' @seealso [adjust_bmi()] for bias-corrected BMI, [categorize_bmi()] for
#'   WHO categories, [calculate_bmi_master()] for Master file equivalent.
#' @export
calculate_bmi <- function(HWTGHTM, HWTGWTK, output_format = "tagged_na") {
  # Step 1: Clean inputs — convert CCHS missing codes to tagged_na
  cleaned <- clean_variables(
    vars = list(HWTGHTM = HWTGHTM, HWTGWTK = HWTGWTK),
    output_format = "tagged_na"
  )

  # Step 2: Domain logic
  result <- dplyr::case_when(
    any_missing(cleaned$HWTGHTM, cleaned$HWTGWTK) ~
      get_priority_missing(cleaned$HWTGHTM, cleaned$HWTGWTK,
                           output_format = output_format),
    cleaned$HWTGHTM <= 0 ~
      assign_missing("not_stated", "HWTGBMI_der", output_format),
    .default = cleaned$HWTGWTK / (cleaned$HWTGHTM^2)
  )

  # Step 3: Clean output — validate range and convert to requested format
  output_cleaned <- clean_variables(
    vars = list(HWTGBMI_der = result),
    output_format = output_format
  )
  output_cleaned$HWTGBMI_der
}
