#' @title Percent time in Canada
#'
#' @description This function creates a derived variable (pct_time_der) that
#'  provides an estimated percentage of the time a person's life was spent in
#'  Canada.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param SDCGCBG whether or not someone was born in Canada (1 - born in Canada,
#'  2 - born outside Canada)
#'
#' @param SDCGRES how long someone has lived in Canada. Note: in the PUMF CCHS
#'  datasets, this is a categorical variable with two categories (1 - 0-9 years;
#'  2 - 10+ years).
#'
#' @note Since SDCGRES is a categorical variable measuring length of time, we've
#'  set midpoints in the function. A respondent identified as being in Canada
#'  for 0-9 years is assigned a value of 4.5 years, and someone who has been in
#'  Canada for over 10 years is assigned a value of 15 years.
#'
#' @return Numeric value between 0 and 100 that represents
#'  percentage of a respondent's time in Canada
#'
#' @examples
#' # Using pct_time_fun() to create percent time values between CCHS cycles
#' # pct_time_fun() is specified in variable_details.csv along with the CCHS
#' # variables and cycles included.
#'
#' # To transform pct_time_der across cycles, use rec_with_table() for each CCHS
#' # cycle and specify pct_time_der, along with age (DHHGAGE_cont), whether or
#' # not someone was born in Canada (SDCGCBG), how long someone has lived in
#' # Canada (SDCGRES). Then by using merge_rec_data(), you can combine
#' # pct_time_der across cycles
#'
#' library(cchsflow)
#' pct_time2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "DHHGAGE_cont", "SDCGCBG",
#'     "SDCGRES", "pct_time_der"
#'   )
#' )
#' head(pct_time2009_2010)
#'
#' pct_time2011_2012 <- rec_with_table(
#'   cchs2011_2012_p, c(
#'     "DHHGAGE_cont", "SDCGCBG",
#'     "SDCGRES", "pct_time_der"
#'   )
#' )
#' tail(pct_time2011_2012)
#'
#' combined_pct_time <- merge_rec_data(pct_time2009_2010, pct_time2011_2012)
#' head(combined_pct_time)
#' tail(combined_pct_time)
#'
#' # Using pct_time_fun() to generate a value for percent time spent in Canada
#' # with user inputted values Let's say you are 27 years old who was born
#' # outside of Canada and have been living in Canada for less than 10 years.
#' # Your estimated percent time spent in Canada can be calculated as follows:
#'
#' pct_time <- pct_time_fun(DHHGAGE_cont = 27, SDCGCBG = 2, SDCGRES = 1)
#'
#' print(pct_time)
#' @export
pct_time_fun <-
  function(DHHGAGE_cont, SDCGCBG, SDCGRES) {
    if (is_equal(SDCGCBG, 1)) {
      return(100)
    }
    DHHGAGE_cont <- if_else2(
      DHHGAGE_cont > 0, DHHGAGE_cont,
      return(tagged_na("b"))
    )
    SDCGRES <- if_else2(
      SDCGRES == 1, 4.5,
      if_else2(SDCGRES == 2, 15, return(tagged_na("b")))
    )

    if_else2(SDCGCBG == 2, (SDCGRES / DHHGAGE_cont * 100), tagged_na("b"))
  }

#' @title Categorical percent time in Canada
#'
#' @description This function creates a categorical derived variable
#' (pct_time_der_cat10) that categorizes the derived percent time in Canada
#' variable (pct_time_der).
#'
#' @details The percent time in Canada provides an estimated percentage of the
#' time a person's life was spent in Canada.The categorical percent time in
#' Canada divides the continuous value into 10 percent intervals.
#'
#' pct_time_der_cat10 uses the derived variable pct_time_der. pct_time_der uses
#' various variables that have been transformed by cchsflow (see documentation
#' on pct_time_der). In order to categorize percent time in Canada across CCHS
#' cycles, the variables must be transformed and harmonized.
#'
#' @param pct_time_der derived continuous percent time in Canada.
#' See \code{\link{pct_time_fun}} for documentation on how variable was derived.
#'
#' @return value for categorical percent time in Canada  using pct_time_der
#' variable.
#'
#' @examples
#' # Using pct_time_fun_cat() to create categorical percent time values
#' # between CCHS cycles.
#' # pct_time_fun_cat() is specified in variable_details.csv along with the CCHS
#' # variables and cycles included.
#'
#' # To transform pct_time_der_cat10 across cycles, use rec_with_table() for
#' # each CCHS cycle.
#' # Since pct_time_der is a derived variable, you will have to specify the
#' # variables that are derived from it.
#' # Then by using merge_rec_data(), you can combine pct_time_der_cat10 across
#' # cycles.
#'
#' library(cchsflow)
#' pct_time_cat2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "DHHGAGE_cont", "SDCGCBG",
#'     "SDCGRES", "pct_time_der", "pct_time_der_cat10"
#'   )
#' )
#' head(pct_time_cat2009_2010)
#'
#' pct_time_cat2011_2012 <- rec_with_table(
#'   cchs2011_2012_p, c(
#'     "DHHGAGE_cont", "SDCGCBG",
#'     "SDCGRES", "pct_time_der", "pct_time_der_cat10"
#'   )
#' )
#' tail(pct_time_cat2011_2012)
#'
#' combined_pct_time_cat <- merge_rec_data(
#'   pct_time_cat2009_2010,
#'   pct_time_cat2011_2012
#' )
#' head(combined_pct_time_cat)
#' tail(combined_pct_time_cat)
#'
#' @export
#'
pct_time_fun_cat <-
  function(pct_time_der) {
    if_else2(
      pct_time_der >= 0 & pct_time_der <= 10, 1,
      if_else2(
        pct_time_der > 10 & pct_time_der <= 20, 2,
        if_else2(
          pct_time_der > 20 & pct_time_der <= 30, 3,
          if_else2(
            pct_time_der > 30 & pct_time_der <= 40, 4,
            if_else2(
              pct_time_der > 40 & pct_time_der <= 50, 5,
              if_else2(
                pct_time_der > 50 & pct_time_der <= 60, 6,
                if_else2(
                  pct_time_der > 60 & pct_time_der <= 70, 7,
                  if_else2(
                    pct_time_der > 70 & pct_time_der <= 80, 8,
                    if_else2(
                      pct_time_der > 80 & pct_time_der <= 90, 9,
                      if_else2(
                        pct_time_der > 90 & pct_time_der <= 100, 10,
                        if_else2(haven::is_tagged_na(pct_time_der, "a"), "NA(a)", "NA(b)")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }
