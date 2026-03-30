# ==============================================================================
# BMI (Body Mass Index) Functions — Master files
# ==============================================================================
#
# Silver-tier derived variable functions for continuous height/weight from
# Master (RDC) files. These parallel the PUMF functions in bmi.R but use
# Master variable names (HWTDHTM, HWTDWTK).
#
# Referenced by variable_details.csv as:
#   HWTDBMI_der      -> Func::calculate_bmi_master
#   HWTDCOR_der      -> Func::adjust_bmi_master
#   HWTDBMI_der_cat4 -> Func::categorize_bmi_master

#' Calculate BMI from continuous height and weight — Master files
#'
#' @param HWTDHTM Height in metres (continuous, Master file variable).
#' @param HWTDWTK Weight in kilograms (continuous, Master file variable).
#' @param output_format Output missing data format: "tagged_na" (default) or
#'   "original".
#'
#' @return Numeric vector of BMI values (kg/m^2).
#'
#' @examples
#' calculate_bmi_master(HWTDHTM = 1.75, HWTDWTK = 70)
#'
#' @seealso [calculate_bmi()] for PUMF equivalent.
#' @export
calculate_bmi_master <- function(HWTDHTM, HWTDWTK, output_format = "tagged_na") {
  cleaned <- clean_variables(
    vars = list(HWTDHTM = HWTDHTM, HWTDWTK = HWTDWTK),
    output_format = "tagged_na"
  )

  result <- dplyr::case_when(
    any_missing(cleaned$HWTDHTM, cleaned$HWTDWTK) ~
      get_priority_missing(cleaned$HWTDHTM, cleaned$HWTDWTK,
                           output_format = output_format),
    cleaned$HWTDHTM <= 0 ~
      assign_missing("not_stated", "HWTDBMI_der", output_format),
    .default = cleaned$HWTDWTK / (cleaned$HWTDHTM^2)
  )

  output_cleaned <- clean_variables(
    vars = list(HWTDBMI_der = result),
    output_format = output_format
  )
  output_cleaned$HWTDBMI_der
}

#' Calculate bias-corrected BMI — Master files
#'
#' @param DHH_SEX Sex (1 = male, 2 = female).
#' @param HWTDHTM Height in metres (continuous, Master file variable).
#' @param HWTDWTK Weight in kilograms (continuous, Master file variable).
#' @param output_format Output missing data format: "tagged_na" (default) or
#'   "original".
#'
#' @return Numeric vector of bias-corrected BMI values.
#'
#' @references
#' Connor Gorber, S., et al. (2008). *Obesity*, 16(10), 2326-2332.
#'
#' @examples
#' adjust_bmi_master(DHH_SEX = 1, HWTDHTM = 1.75, HWTDWTK = 70)
#'
#' @seealso [adjust_bmi()] for PUMF equivalent.
#' @export
adjust_bmi_master <- function(DHH_SEX, HWTDHTM, HWTDWTK,
                         output_format = "tagged_na") {
  cleaned <- clean_variables(
    vars = list(DHH_SEX = DHH_SEX, HWTDHTM = HWTDHTM, HWTDWTK = HWTDWTK),
    output_format = "tagged_na"
  )

  raw_bmi <- dplyr::case_when(
    any_missing(cleaned$HWTDHTM, cleaned$HWTDWTK) ~
      get_priority_missing(cleaned$HWTDHTM, cleaned$HWTDWTK,
                           output_format = "tagged_na"),
    cleaned$HWTDHTM <= 0 ~ haven::tagged_na("b"),
    .default = cleaned$HWTDWTK / (cleaned$HWTDHTM^2)
  )

  result <- dplyr::case_when(
    any_missing(raw_bmi) ~
      get_priority_missing(raw_bmi, cleaned$DHH_SEX,
                           output_format = output_format),
    any_missing(cleaned$DHH_SEX) ~
      get_priority_missing(cleaned$DHH_SEX, output_format = output_format),
    cleaned$DHH_SEX == 1 ~
      BMI_CORRECTION_MALE$intercept + BMI_CORRECTION_MALE$slope * raw_bmi,
    cleaned$DHH_SEX == 2 ~
      BMI_CORRECTION_FEMALE$intercept + BMI_CORRECTION_FEMALE$slope * raw_bmi,
    .default = assign_missing("not_stated", "HWTDCOR_der", output_format)
  )

  output_cleaned <- clean_variables(
    vars = list(HWTDCOR_der = result),
    output_format = output_format
  )
  output_cleaned$HWTDCOR_der
}

#' Categorize BMI into WHO categories — Master files
#'
#' @param HWTDBMI_der Continuous BMI value (from [calculate_bmi_master()]).
#' @param output_format Output missing data format: "tagged_na" (default) or
#'   "original".
#'
#' @return Integer vector: 1 = underweight, 2 = normal, 3 = overweight,
#'   4 = obese.
#'
#' @examples
#' categorize_bmi_master(HWTDBMI_der = 27.3)
#'
#' @seealso [categorize_bmi()] for PUMF equivalent.
#' @export
categorize_bmi_master <- function(HWTDBMI_der, output_format = "tagged_na") {
  cleaned <- clean_variables(
    vars = list(HWTDBMI_der = HWTDBMI_der),
    output_format = "tagged_na"
  )

  result <- dplyr::case_when(
    any_missing(cleaned$HWTDBMI_der) ~
      get_priority_missing(cleaned$HWTDBMI_der, output_format = output_format),
    cleaned$HWTDBMI_der < 18.5 ~ 1L,
    cleaned$HWTDBMI_der < 25.0 ~ 2L,
    cleaned$HWTDBMI_der < 30.0 ~ 3L,
    cleaned$HWTDBMI_der >= 30.0 ~ 4L,
    .default = assign_missing("not_stated", "HWTDBMI_der_cat4", output_format)
  )

  output_cleaned <- clean_variables(
    vars = list(HWTDBMI_der_cat4 = result),
    output_format = output_format
  )
  output_cleaned$HWTDBMI_der_cat4
}
