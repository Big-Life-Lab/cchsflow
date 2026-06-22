#' @title Calculate percent time in Canada
#'
#' @description Calculates the percentage of a respondent's life spent in
#'  Canada. Used by pct_time_der for both PUMF and master databases. The
#'  worksheet maps different feeder variables for each database type, but
#'  the calculation is the same: for respondents born outside Canada,
#'  percent time = years in Canada / age * 100.
#'
#'  For PUMF data, the categorical time-in-Canada variable (SDCGRES) is
#'  converted to continuous midpoints upstream via the SDCGRES_cont intermediate
#'  variable in variable_details.csv (1 = 4.5 years, 2 = 15 years). For master
#'  data, continuous years (SDCDRES) are passed directly.
#'
#' @param age continuous age variable.
#' @param born_in_canada whether or not someone was born in Canada
#'  (1 - born in Canada, 2 - born outside Canada).
#' @param years_in_canada continuous years in Canada. For PUMF data, this is
#'  derived from the categorical SDCGRES via midpoint conversion in
#'  variable_details.csv. For master data, this is the actual continuous
#'  years from SDCDRES.
#'
#' @return Numeric value between 0 and 100 that represents percentage of a
#'  respondent's time in Canada. Returns \code{tagged_na("b")} for invalid or
#'  missing inputs.
#'
#' @examples
#' # Using rec_with_table() across CCHS cycles (PUMF)
#' library(cchsflow)
#' pct_time2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "DHHGAGE_cont", "SDCGCBG",
#'     "SDCGRES_cont", "pct_time_der"
#'   )
#' )
#' head(pct_time2009_2010)
#'
#' pct_time2011_2012 <- rec_with_table(
#'   cchs2011_2012_p, c(
#'     "DHHGAGE_cont", "SDCGCBG",
#'     "SDCGRES_cont", "pct_time_der"
#'   )
#' )
#' tail(pct_time2011_2012)
#'
#' combined_pct_time <- merge_rec_data(pct_time2009_2010, pct_time2011_2012)
#' head(combined_pct_time)
#' tail(combined_pct_time)
#'
#' # Scalar usage (pass continuous years directly)
#' calculate_pct_time(age = 27, born_in_canada = 2, years_in_canada = 4.5)
#'
#' # Vector usage
#' calculate_pct_time(
#'   age = c(27, 40, 35),
#'   born_in_canada = c(2, 1, 2),
#'   years_in_canada = c(4.5, 4.5, 15)
#' )
#' @export
calculate_pct_time <- function(age, born_in_canada, years_in_canada) {
  result <- dplyr::case_when(
    born_in_canada == 1 ~ 100,
    born_in_canada == 2 & age > 0 & !is.na(years_in_canada) ~
      years_in_canada / age * 100,
    TRUE ~ tagged_na("b")
  )
  # Output validation: values outside [0, 100] indicate inconsistent inputs
  # (e.g., years_in_canada > age). Valid range: 0-100.
  # Documentation-only boundaries in variable_details; enforced here.
  dplyr::case_when(
    is.na(result) ~ result,
    result < 0 | result > 100 ~ tagged_na("b"),
    TRUE ~ result
  )
}

#' @title Categorize percent time in Canada
#'
#' @description Categorizes the derived percent time in Canada variable
#' (pct_time_der) into 10 percent intervals for pct_time_der_cat10.
#'
#' @details The percent time in Canada provides an estimated percentage of the
#' time a person's life was spent in Canada. The categorical percent time in
#' Canada divides the continuous value into 10 percent intervals.
#'
#' pct_time_der_cat10 uses the derived variable pct_time_der. pct_time_der uses
#' various variables that have been transformed by cchsflow (see documentation
#' on pct_time_der). In order to categorize percent time in Canada across CCHS
#' cycles, the variables must be transformed and harmonized.
#'
#' @param pct_time_der derived continuous percent time in Canada.
#' See \code{\link{calculate_pct_time}} for documentation on how variable was
#' derived.
#'
#' @return Character value for categorical percent time in Canada ("1" through
#' "10"), "NA(a)" for not applicable, or "NA(b)" for missing/invalid inputs.
#'
#' @examples
#' # Using rec_with_table() across CCHS cycles
#' library(cchsflow)
#' pct_time_cat2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "DHHGAGE_cont", "SDCGCBG",
#'     "SDCGRES_cont", "pct_time_der", "pct_time_der_cat10"
#'   )
#' )
#' head(pct_time_cat2009_2010)
#'
#' pct_time_cat2011_2012 <- rec_with_table(
#'   cchs2011_2012_p, c(
#'     "DHHGAGE_cont", "SDCGCBG",
#'     "SDCGRES_cont", "pct_time_der", "pct_time_der_cat10"
#'   )
#' )
#' tail(pct_time_cat2011_2012)
#'
#' combined_pct_time_cat <- merge_rec_data(pct_time_cat2009_2010,
#' pct_time_cat2011_2012)
#' head(combined_pct_time_cat)
#' tail(combined_pct_time_cat)
#'
#' # Scalar usage
#' categorize_pct_time(55.5)
#'
#' # Vector usage
#' categorize_pct_time(c(5, 25, 55, 85, 100))
#'
#' @export
#'
categorize_pct_time <-
  function(pct_time_der) {
    dplyr::case_when(
      haven::is_tagged_na(pct_time_der, "a") ~ "NA(a)",
      pct_time_der >= 0 & pct_time_der <= 10 ~ "1",
      pct_time_der > 10 & pct_time_der <= 20 ~ "2",
      pct_time_der > 20 & pct_time_der <= 30 ~ "3",
      pct_time_der > 30 & pct_time_der <= 40 ~ "4",
      pct_time_der > 40 & pct_time_der <= 50 ~ "5",
      pct_time_der > 50 & pct_time_der <= 60 ~ "6",
      pct_time_der > 60 & pct_time_der <= 70 ~ "7",
      pct_time_der > 70 & pct_time_der <= 80 ~ "8",
      pct_time_der > 80 & pct_time_der <= 90 ~ "9",
      pct_time_der > 90 & pct_time_der <= 100 ~ "10",
      TRUE ~ "NA(b)"
    )
  }
