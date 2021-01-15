#' @title Time since quit smoking
#' 
#' @description This function creates a derived variable (time_quit_smoking_der)
#'  that calculates the approximate time a former smoker has quit smoking based
#'  on various CCHS smoking variables. This variable is for CCHS respondents in
#'  CCHS surveys 2003-2014.
#'  
#' @param SMK_09A_B number of years since quitting smoking. Variable asked to
#'  former daily smokers who quit <3 years ago.
#' 
#' @param SMKG09C number of years since quitting smoking. Variable asked to
#'  former daily smokers who quit >=3 years ago.
#'  
#' @return value for time since quit smoking in time_quit_smoking_der.
#' 
#' @examples 
#' # Using time_quit_smoking_fun() to create pack-years values across CCHS 
#' # cycles.
#' # time_quit_smoking_fun() is specified in variable_details.csv along with the
#' # CCHS variables and cycles included.
#'
#' # To transform time_quit_fun across cycles, use rec_with_table() for each
#' # CCHS cycle and specify time_quit_fun, along with each smoking variable.
#' # Then by using bind_rows(), you can combine time_quit_fun across cycles
#' 
#' library(cchsflow)
#' 
#' time_quit2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "SMK_09A_B", "SMKG09C", "time_quit_smoking_der."
#'   )
#' )
#'
#' head(time_quit2009_2010)
#'
#' time_quit2011_2012 <- rec_with_table(
#'   cchs2011_2012_p, c(
#'     "SMK_09A_B", "SMKG09C", "time_quit_smoking_der."
#'   )
#' )
#'
#' tail(time_quit2011_2012)
#'
#' combined_time_quit <- suppressWarnings(bind_rows(time_quit2009_2010,
#'  time_quit2011_2012))
#'
#' head(combined_time_quit)
#' tail(combined_time_quit)
#' @export

time_quit_smoking_fun <- function(SMK_09A_B, SMKG09C) {
  SMKG09C_cont <-
    if_else2(
      SMKG09C == 1, 4,
      if_else2(
        SMKG09C == 2, 8,
        if_else2(SMKG09C == 3, 12,
                 if_else2(SMKG09C == "NA(a)", tagged_na("a"), tagged_na("b")
                 )
        )
      )
    )
  tsq_ds <-
    if_else2(
      SMK_09A_B == 1, 0.5,
      if_else2(
        SMK_09A_B == 2, 1.5,
        if_else2(
          SMK_09A_B == 3, 2.5,
          if_else2(SMK_09A_B == 4, SMKG09C_cont,
                   if_else2(SMK_09A_B == "NA(a)", tagged_na("a"), tagged_na("b")
                   )
          )
        )
      )
    )
  return(tsq_ds)
}

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

#' @title Smoking pack-years
#'
#' @description This function creates a derived variable (pack_years_der) that
#'  measures an individual's smoking pack-years based on various CCHS smoking
#'  variables. This is a popular variable used by researchers to quantify
#'  lifetime exposure to cigarette use.
#'
#' @details pack-years is calculated by multiplying the number of cigarette
#'  packs per day (20 cigarettes per pack) by the number of years. Example 1:
#'  a respondent who is a current smoker who smokes 1 package of cigarettes for
#'  the last 10 years has smoked 10 pack-years. Pack-years is also calculated
#'  for former smokers. Example 2: a respondent who started smoking at age
#'  20 years and smoked half a pack of cigarettes until age 40 years smoked for
#'  10 pack-years.
#'
#' @param SMKDSTY derived variable that classifies an individual's smoking
#'  status.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param SMK_09A_B number of years since quitting smoking. Variable asked to
#'  former daily smokers who quit <3 years ago.
#'
#' @param SMKG09C number of years since quitting smoking. Variable asked to
#'  former daily smokers who quit >=3 years ago.
#'
#' @param SMKG203_cont age started smoking daily. Variable asked to daily
#'  smokers.
#'
#' @param SMKG207_cont age started smoking daily. Variable asked to former
#'  daily smokers.
#'
#' @param SMK_204 number of cigarettes smoked per day. Variable asked to
#'  daily smokers.
#'
#' @param SMK_05B number of cigarettes smoked per day. Variable asked to
#'  occasional smokers
#'
#' @param SMK_208 number of cigarettes smoked per day. Variable asked to former
#'  daily smokers
#'
#' @param SMK_05C number of days smoked at least one cigarette
#'
#' @param SMK_01A smoked 100 cigarettes in lifetime (y/n)
#'
#' @param SMKG01C_cont age smoked first cigarette
#'
#' @return value for smoking pack-years in the Pack_years_der variable
#'
#' @examples
#' # Using pack_years_fun() to create pack-years values across CCHS cycles
#' # pack_years_fun() is specified in variable_details.csv along with the CCHS
#' # variables and cycles included.
#'
#' # To transform pack_years_der across cycles, use rec_with_table() for each
#' # CCHS cycle and specify pack_years_der, along with each smoking variable.
#' # Then by using bind_rows(), you can combine pack_years_der across cycles
#'
#' library(cchsflow)
#'
#' pack_years2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "SMKDSTY", "DHHGAGE_cont", "SMK_09A_B", "SMKG09C",
#'     "SMKG203_cont", "SMKG207_cont", "SMK_204", "SMK_05B", "SMK_208",
#'     "SMK_05C", "SMK_01A", "SMKG01C_cont", "pack_years_der"
#'   )
#' )
#'
#' head(pack_years2009_2010)
#'
#' pack_years2011_2012 <- rec_with_table(
#'   cchs2011_2012_p,c(
#'     "SMKDSTY", "DHHGAGE_cont", "SMK_09A_B", "SMKG09C",
#'     "SMKG203_cont", "SMKG207_cont", "SMK_204", "SMK_05B", "SMK_208",
#'     "SMK_05C", "SMK_01A", "SMKG01C_cont", "pack_years_der"
#'   )
#' )
#'
#' tail(pack_years2011_2012)
#'
#' combined_pack_years <- suppressWarnings(bind_rows(pack_years2009_2010,
#'  pack_years2011_2012))
#'
#' head(combined_pack_years)
#' tail(combined_pack_years)
#' @export
pack_years_fun <-
  function(SMKDSTY, DHHGAGE_cont, SMK_09A_B, SMKG09C, SMKG203_cont,
           SMKG207_cont, SMK_204, SMK_05B,
           SMK_208, SMK_05C, SMKG01C_cont, SMK_01A) {
    # Age verification
    if (is.na(DHHGAGE_cont)) {
      return(tagged_na("b"))
    } else if (DHHGAGE_cont < 0) {
      return(tagged_na("b"))
    }

    # Time since quit for former daily smokers
    tsq_ds_fun <- function(SMK_09A_B, SMKG09C) {
      SMKG09C <-
        if_else2(
          SMKG09C == 1, 4,
          if_else2(
            SMKG09C == 2, 8,
            if_else2(SMKG09C == 3, 12, NA)
          )
        )
      tsq_ds <-
        if_else2(
          SMK_09A_B == 1, 0.5,
          if_else2(
            SMK_09A_B == 2, 1.5,
            if_else2(
              SMK_09A_B == 3, 2.5,
              if_else2(SMK_09A_B == 4, SMKG09C, NA)
            )
          )
        )
    }
    tsq_ds <- tsq_ds_fun(SMK_09A_B, SMKG09C)
    # PackYears for Daily Smoker
    if_else2(
      SMKDSTY == 1, pmax(((DHHGAGE_cont - SMKG203_cont) *
        (SMK_204 / 20)), 0.0137),
      # PackYears for Occasional Smoker (former daily)
      if_else2(
        SMKDSTY == 2, pmax(((DHHGAGE_cont - SMKG207_cont - tsq_ds) *
        (SMK_208 / 20)), 0.0137) + (pmax((SMK_05B * SMK_05C / 30), 1) * tsq_ds),
        # PackYears for Occasional Smoker (never daily)
        if_else2(
          SMKDSTY == 3, (pmax((SMK_05B * SMK_05C / 30), 1) / 20) *
            (DHHGAGE_cont - SMKG01C_cont),
          # PackYears for former daily smoker (non-smoker now)
          if_else2(
            SMKDSTY == 4, pmax(((DHHGAGE_cont - SMKG207_cont - tsq_ds) *
              (SMK_208 / 20)), 0.0137),
            # PackYears for former occasional smoker (non-smoker now) who
            # smoked at least 100 cigarettes lifetime
            if_else2(
              SMKDSTY == 5 & SMK_01A == 1, 0.0137,
              # PackYears for former occasional smoker (non-smoker now) who have
              # not smoked at least 100 cigarettes lifetime
              if_else2(
                SMKDSTY == 5 & SMK_01A == 2, 0.007,
                # Non-smoker
                if_else2(SMKDSTY == 6, 0, tagged_na("b"))
              )
            )
          )
        )
      )
    )
  }
