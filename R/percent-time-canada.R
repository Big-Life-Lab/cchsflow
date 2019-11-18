#' Percent time in Canada
#' 
#' This function creates a derived variable that provides a percentage of the time a person's life was spent in 
#' Canada.
#' 
#' @param DHHGAGE_cont continuous age variable. 
#' 
#' @param SDCGCBG whether or not someone was born in Canada 
#' 
#' @param SDCGRES how long someone has lived in Canada
#' 
#' @return value of percentage of life spent living in Canada
#' 
#' @export
Pct_time_fun <-
  function(DHHGAGE_cont, SDCGCBG, SDCGRES) {
    SDCGRES_fun <- function(SDCGRES) {
      ifelse2(SDCGRES == 1, 4.5,
      ifelse2(SDCGRES == 2, 15, NA))
    }
    SDCGRES <- SDCGRES_fun(SDCGRES)
    ifelse2(SDCGCBG == 1, 100,
    ifelse2(SDCGCBG == 2, (SDCGRES/DHHGAGE_cont)*100, NA))
  }
