# Master-file BMI functions (continuous height/weight inputs)
#
# These functions calculate BMI from continuous (non-grouped) height and weight
# variables available on Master files. They parallel the PUMF functions
# (bmi_fun, adjusted_bmi_fun, bmi_fun_cat) but use Master variable names
# (HWTDHTM, HWTDWTK) and do not require MAM_037 (pregnancy status).
#
# Referenced by variable_details.csv as:
#   HWTDBMI_der     -> Func::calculate_bmi_D
#   HWTDCOR_der     -> Func::adjust_bmi_D
#   HWTDBMI_der_cat4 -> Func::categorize_bmi_D

#' Calculate BMI from continuous height and weight (Master files)
#'
#' @param HWTDHTM Height in metres (continuous, Master file variable)
#' @param HWTDWTK Weight in kilograms (continuous, Master file variable)
#' @return BMI value (kg/m2). Returns \code{haven::tagged_na("b")} for
#'   missing or invalid inputs.
#' @export
calculate_bmi_D <- function(HWTDHTM, HWTDWTK) {
  dplyr::case_when(
    is.na(HWTDHTM) | is.na(HWTDWTK) ~ haven::tagged_na("b"),
    haven::is_tagged_na(HWTDHTM) ~ haven::tagged_na("b"),
    haven::is_tagged_na(HWTDWTK) ~ haven::tagged_na("b"),
    HWTDHTM <= 0 ~ haven::tagged_na("b"),
    .default = HWTDWTK / (HWTDHTM^2)
  )
}

#' Calculate bias-corrected BMI from continuous height and weight (Master files)
#'
#' Applies sex-specific correction coefficients from Connor Gorber et al. (2008)
#' to account for self-reporting bias in height and weight.
#'
#' @param DHH_SEX Sex (1 = male, 2 = female)
#' @param HWTDHTM Height in metres (continuous, Master file variable)
#' @param HWTDWTK Weight in kilograms (continuous, Master file variable)
#' @return Bias-corrected BMI value (kg/m2). Returns
#'   \code{haven::tagged_na("b")} for missing or invalid inputs.
#' @references
#' Connor Gorber, S., et al. (2008). The accuracy of self-reported height and
#' weight in a nationally representative sample of Canadian adults.
#' @export
adjust_bmi_D <- function(DHH_SEX, HWTDHTM, HWTDWTK) {
  raw_bmi <- dplyr::case_when(
    is.na(HWTDHTM) | is.na(HWTDWTK) ~ haven::tagged_na("b"),
    haven::is_tagged_na(HWTDHTM) ~ haven::tagged_na("b"),
    haven::is_tagged_na(HWTDWTK) ~ haven::tagged_na("b"),
    HWTDHTM <= 0 ~ haven::tagged_na("b"),
    .default = HWTDWTK / (HWTDHTM^2)
  )

  dplyr::case_when(
    is.na(raw_bmi) ~ haven::tagged_na("b"),
    haven::is_tagged_na(raw_bmi) ~ haven::tagged_na("b"),
    is.na(DHH_SEX) ~ haven::tagged_na("b"),
    haven::is_tagged_na(DHH_SEX) ~ haven::tagged_na("b"),
    DHH_SEX == 1 ~ -1.07575 + 1.07592 * raw_bmi,
    DHH_SEX == 2 ~ -0.12374 + 1.05129 * raw_bmi,
    .default = haven::tagged_na("b")
  )
}

#' Categorise BMI into 4 WHO categories (Master files)
#'
#' @param HWTDBMI_der Continuous BMI value (from \code{calculate_bmi_D})
#' @return Integer BMI category: 1 = underweight (< 18.5),
#'   2 = normal weight (18.5-24.9), 3 = overweight (25.0-29.9),
#'   4 = obese (>= 30.0). Returns \code{haven::tagged_na("b")} for
#'   missing inputs.
#' @export
categorize_bmi_D <- function(HWTDBMI_der) {
  dplyr::case_when(
    is.na(HWTDBMI_der) ~ haven::tagged_na("b"),
    haven::is_tagged_na(HWTDBMI_der) ~ haven::tagged_na("b"),
    HWTDBMI_der < 18.5 ~ 1L,
    HWTDBMI_der < 25.0 ~ 2L,
    HWTDBMI_der < 30.0 ~ 3L,
    HWTDBMI_der >= 30.0 ~ 4L,
    .default = haven::tagged_na("b")
  )
}
