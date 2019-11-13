#' Percent time in Canada
#' 
#' This function creates a derived variable that provides a percentage of the time a person's life was spent in Canada
#' 
#' @param Age_cont continuous age variable. 
#' cchsflow variable name: DHHGAGE_cont
#' 
#' @param BirthCountry whether or not someone was born in Canada 
#' cchsflow variable name: SDCGCBG
#' 
#' @param TimeCanada how long someone has lived in Canada
#' cchsflow variable name: SDCGRES
#' 
#' @return value of percentage of time spent living in Canada in the Pct_time_derived variable
#' 
#' @export
Pct_time_fun <-
  function(Age_cont, BirthCountry, TimeCanada) {
    TimeCanada_fun <- function(TimeCanada) {
      ifelse2(TimeCanada == 1, 4.5,
      ifelse2(TimeCanada == 2, 15, NA))
    }
    TimeCanada <- TimeCanada_fun(TimeCanada)
    ifelse2(BirthCountry == 1, 100,
    ifelse2(BirthCountry == 2, (TimeCanada/Age_cont)*100, NA))
  }
