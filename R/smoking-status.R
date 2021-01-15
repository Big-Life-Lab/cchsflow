#' @title Simple smoking status
#'
#' @description This function creates a derived smoking variable (smoke_simple)  
#'  with four categories: 
#'  
#' \itemize{
#'   \item non-smoker (never smoked)
#'   \item current smoker (daily and occasional?)
#'   \item former daily smoker quit ≤5 years or former occasional smoker 
#'   \item former daily smoker quit >5 years
#'  }
#'
#' @param SMKDSTY derived variable that classifies an individual's smoking
#'  status.
#'
#' @param SMK_09A_B number of years since quitting smoking. Variable asked to
#'  former daily smokers who quit <3 years ago.
#'
#' @param SMKG09C number of years since quitting smoking. Variable asked to
#'  former daily smokers who quit ≥3 years ago.
#'
#' @example
#' # Using the 'smoke_simple_fun' function to create the derived smoking   
#' # variable across CCHS cycles.
#' # smoke_simple_fun() is specified in the variable_details.csv
#'
#' # To create a harmonized smoke_simple variable across CCHS cycles, use 
#' # rec_with_table() for each CCHS cycle and specify smoke_simple_fun and 
#' # the required base variables.
#' # Using bind_rows(), you can combine smoke_simple across cycles
#'
#' library(cchsflow)
#'
#' smoke_simple2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "SMKDSTY", "SMK_09A_B", "SMKG09C", "smoke_simple_fun"
#'   )
#' )
#'
#' head(smoke_simple2009_2010)
#'
#' smoke_simple2011_2012 <- rec_with_table(
#'   cchs2011_2012_p,c(
#'     "SMKDSTY", "SMK_09A_B", "SMKG09C", "smoke_simple_fun"
#'   )
#' )
#'
#' tail(smoke_simple2011_2012)
#'
#' combined_smoke_simple <- suppressWarnings(bind_rows(smoke_simple2009_2010,
#'  smoke_simple2011_2012))
#'
#' head(combined_smoke_simple)
#' tail(combined_smoke_simple)
#' @export
smoke_simple_fun <-
  function(SMKDSTY, SMK_09A_B, SMKG09C) {
    
    # Nested function: current smoker status
    derive_current_smoker <- function(SMKDSTY) {
      smoker <-
        ifelse(SMKDSTY %in% c(1, 2, 3), 1,
        ifelse(SMKDSTY %in% c(4, 5, 6), 0, NA))
      return(smoker)
    }
    smoker <- derive_current_smoker(SMKDSTY)
    
    # Nested function: ever smoker status
    derive_ever_smoker <- function(SMKDSTY) {
      eversmoker <-
        ifelse(SMKDSTY %in% c(1, 2, 3, 4, 5), 1,
        ifelse(SMKDSTY == 6, 0, NA))
      return(eversmoker)
    }
    eversmoker <- derive_ever_smoker(SMKDSTY)
    
    # Nested function: time since quit former daily smokers
    tsq_ds_fun <- function(SMK_09A_B, SMKG09C) {
      SMKG09C_cont <-
        if_else2(SMKG09C == 1, 4,
                 if_else2(SMKG09C == 2, 8,
                          if_else2(SMKG09C == 3, 12, NA)))
      tsq_ds <-
        if_else2(SMK_09A_B == 1, 0.5,
                 if_else2(SMK_09A_B == 2, 1.5,
                          if_else2(SMK_09A_B == 3, 2.5,
                                   if_else2(SMK_09A_B == 4, SMKG09C_cont, NA))))
    }
    tsq_ds <- tsq_ds_fun(SMK_09A_B, SMKG09C)
    
    # smoke_simple 0 = non-smoker
    ifelse(smoker == 0 & eversmoker == 0, 0,
    # smoke_simple 1 = current smoker
    ifelse(smoker == 1 & eversmoker == 1, 1,
    # smoke_simple 2 = former daily smoker quit ≤ 5 years or former occasional
    # smoker
    ifelse(smoker == 0 & eversmoker == 1 & tsq_ds <= 5 | SMKDSTY == 5, 2,
    # smoke_simple 3 = former daily smoker quit > 5 years
    ifelse(smoker == 0 & eversmoker == 1 & tsq_ds > 5, 3, NA))))
    return(smoke_simple)
  }