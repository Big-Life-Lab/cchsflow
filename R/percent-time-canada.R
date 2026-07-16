#' @title Calculate percent of life spent in Canada
#'
#' @description
#' Calculates the percentage of a respondent's life spent in Canada.
#' For respondents born outside Canada, percent time = years in
#' Canada / age * 100. For respondents born in Canada, returns 100.
#'
#' @details
#' Source-agnostic: the worksheet routes DHHGAGE_cont / SDCGCBG /
#' SDCGRES_cont (PUMF) or DHH_AGE / SDCGCB / SDCDRES (Master) to the
#' same semantic parameters. Values outside 0-100 (indicating
#' inconsistent inputs) are recoded to not stated. Missing-data handling
#' follows the v3 3-step architecture with priority not applicable > not
#' stated.
#'
#' @param age Continuous age (DHHGAGE_cont for PUMF, DHH_AGE for Master).
#' @param born_in_canada Country of birth (1 = Canada, 2 = outside Canada).
#' @param years_in_canada Continuous years in Canada.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector: percentage of life in Canada (0-100). Missing
#'   data: \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' calculate_pct_time(age = 27, born_in_canada = 2, years_in_canada = 4.5)
#'
#' # Vector
#' calculate_pct_time(
#'   age = c(27, 40, 35),
#'   born_in_canada = c(2, 1, 2),
#'   years_in_canada = c(4.5, 4.5, 15)
#' )
#'
#' @seealso \code{\link{categorize_pct_time}},
#'   \code{\link{categorize_immigration}}
#'
#' @export
calculate_pct_time <- function(age, born_in_canada, years_in_canada,
                                output_format = "tagged_na") {
  # === STEP 1: DATA CLEANING ===
  cleaned <- clean_variables(vars = list(
    DHHGAGE_cont = age,
    SDCGCBG = born_in_canada,
    SDCGRES_cont = years_in_canada
  ), output_format = "tagged_na")

  age_c <- cleaned$DHHGAGE_cont
  born <- cleaned$SDCGCBG
  years <- cleaned$SDCGRES_cont

  # === STEP 2: CORE CALCULATION ===
  result <- dplyr::case_when(
    any_missing(born, age_c) ~
      get_priority_missing(born, age_c, output_format = output_format),
    born == 1 ~ 100,
    born == 2 & age_c > 0 & !any_missing(years) ~
      years / age_c * 100,
    .default = assign_missing("not_stated", "pct_time_der", output_format)
  )

  # Output validation: values outside [0, 100] indicate inconsistent inputs
  result <- dplyr::case_when(
    is.na(result) ~ result,
    result < 0 | result > 100 ~
      assign_missing("not_stated", "pct_time_der", output_format),
    TRUE ~ result
  )

  # === STEP 3: OUTPUT VALIDATION ===
  output_cleaned <- clean_variables(vars = list(
    pct_time_der = result
  ), output_format = output_format)

  return(output_cleaned$pct_time_der)
}

#' @title Categorize percent time in Canada into deciles
#'
#' @description
#' Categorizes the derived percent time in Canada variable into 10
#' equal-width intervals (0-10, 11-20, ..., 91-100).
#'
#' @details
#' Each 10-percentage-point band maps to an integer code 1-10. This
#' provides a standard grouping for cross-tabulation and modelling.
#' Missing-data handling follows the v3 3-step architecture with priority
#' not applicable > not stated.
#'
#' @param pct_time_der Derived continuous percent time in Canada (0-100).
#'   See \code{\link{calculate_pct_time}}.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector: 1-10 for categorical percent time in Canada.
#'   Missing data: \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' categorize_pct_time(55.5)
#'
#' # Vector
#' categorize_pct_time(c(5, 25, 55, 85, 100))
#'
#' @seealso \code{\link{calculate_pct_time}}
#'
#' @export
categorize_pct_time <- function(pct_time_der, output_format = "tagged_na") {
  # === STEP 1: DATA CLEANING ===
  cleaned <- clean_variables(vars = list(
    pct_time_der = pct_time_der
  ), output_format = "tagged_na")

  pct <- cleaned$pct_time_der

  # === STEP 2: CATEGORIZATION ===
  result <- dplyr::case_when(
    any_missing(pct) ~
      get_priority_missing(pct, output_format = output_format),
    pct >= 0 & pct <= 10 ~ 1,
    pct > 10 & pct <= 20 ~ 2,
    pct > 20 & pct <= 30 ~ 3,
    pct > 30 & pct <= 40 ~ 4,
    pct > 40 & pct <= 50 ~ 5,
    pct > 50 & pct <= 60 ~ 6,
    pct > 60 & pct <= 70 ~ 7,
    pct > 70 & pct <= 80 ~ 8,
    pct > 80 & pct <= 90 ~ 9,
    pct > 90 & pct <= 100 ~ 10,
    .default = assign_missing("not_stated", "pct_time_der_cat10", output_format)
  )

  # === STEP 3: OUTPUT VALIDATION ===
  output_cleaned <- clean_variables(vars = list(
    pct_time_der_cat10 = result
  ), output_format = output_format)

  return(prep_cat_output(output_cleaned$pct_time_der_cat10))
}
