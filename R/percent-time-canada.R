#' @title Percent time in Canada
#' 
#' @description This function creates a derived variable (Pct_time_der) that provides an estimated percentage of the time 
#'  a person's life was spent in Canada. 
#' 
#' @param DHHGAGE_cont continuous age variable. 
#' 
#' @param SDCGCBG whether or not someone was born in Canada (1 - born in Canada, 2 - born outside Canada)
#' 
#' @param SDCGRES how long someone has lived in Canada. Note in the PUMF CCHS datasets, this is a categorical variable
#'  with two categories (1 - 0-9 years; 2 - 10+ years).
#'  
#' @note Since SDCGRES is a categorical variable measuring length of time, we've set midpoints in the function. A respondent
#'  identified as being in Canada for 0-9 years is assigned a value of 4.5 years, and someone who has been in Canada for over 10 years
#'  is assigned a value of 15 years.
#'  
#' @return Numeric value that is a fraction between 0 and 1 that represents percentage of a respondent's time in Canada
#' 
#' @examples 
#' # Using Pct_time_fun() to create percent time values across CCHS cycles
#' # Pct_time_fun() is specified in variableDetails.csv along with the CCHS variables and cycles included. 
#' 
#' # To transform Pct_time_der across cycles, use RecWTable() for each CCHS cycle and specify Pct_time_der, along with
#' # age (DHHGAGE_cont), whether or not someone was born in Canada (SDCGCBG), how long someone has lived in Canada (SDCGRES).
#' # Then by using bind_rows(), you can combine Pct_time_der across cycles
#' 
#' suppressMessages(library(bllflow))
#' library(cchsflow)
#' pct_time2010 <- RecWTable(dataSource = cchs2010, variableDetails = variableDetails, datasetName = "cchs2010", 
#' variables = c("DHHGAGE_cont", "SDCGCBG", "SDCGRES", "Pct_time_der"))
#' head(pct_time2010)
#' 
#' pct_time2012 <- RecWTable(dataSource = cchs2012, variableDetails = variableDetails, datasetName = "cchs2012", 
#' variables = c("DHHGAGE_cont", "SDCGCBG", "SDCGRES", "Pct_time_der"))
#' tail(pct_time2012)
#' 
#' combined_pct_time <- bind_rows(pct_time2010, pct_time2012)
#' head(combined_pct_time)
#' tail(combined_pct_time)
#' 
#' # Using Pct_time_fun() to generate a value for percent time spent in Canada with user inputted values
#' # Let's say you are 27 years old who was born outside of Canada and have been living in Canada for less than 10 years.
#' # Your estimated percent time spent in Canada can be calculated as follows:
#' 
#' pct_time <- Pct_time_fun(DHHGAGE_cont = 27, SDCGCBG = 2, SDCGRES = 1)
#' 
#' print(pct_time)
#'       
#' @export
Pct_time_fun <-
  function(DHHGAGE_cont, SDCGCBG, SDCGRES) {
    SDCGRES_fun <- function(SDCGRES) {
      ifelse2(SDCGRES == 1, 4.5,
      ifelse2(SDCGRES == 2, 15, NA))
    }
    SDCGRES <- SDCGRES_fun(SDCGRES)
    ifelse2(SDCGCBG == 1, 1,
    ifelse2(SDCGCBG == 2, (SDCGRES/DHHGAGE_cont), NA))
  }