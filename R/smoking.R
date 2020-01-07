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
#' pack_years2010 <- rec_with_table(
#'   data = cchs2010, variable_details = variable_details,
#'   dataset_name = "cchs2010", variables = c(
#'     "SMKDSTY", "DHHGAGE_cont", "SMK_09A_B", "SMKG09C",
#'     "SMKG203_cont", "SMKG207_cont", "SMK_204", "SMK_05B", "SMK_208",
#'     "SMK_05C", "SMK_01A", "SMKG01C_cont", "pack_years_der"
#'   )
#' )
#'
#' head(pack_years2010)
#'
#' pack_years2012 <- rec_with_table(
#'   data = cchs2012, variable_details = variable_details,
#'   dataset_name = "cchs2012", variables = c(
#'     "SMKDSTY", "DHHGAGE_cont", "SMK_09A_B", "SMKG09C",
#'     "SMKG203_cont", "SMKG207_cont", "SMK_204", "SMK_05B", "SMK_208",
#'     "SMK_05C", "SMK_01A", "SMKG01C_cont", "pack_years_der"
#'   )
#' )
#'
#' tail(pack_years2012)
#'
#' combined_pack_years <- bind_rows(pack_years2010, pack_years2012)
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
      return(NA)
    } else if (DHHGAGE_cont < 0) {
      return(NA)
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
                if_else2(SMKDSTY == 6, 0, NA)
              )
            )
          )
        )
      )
    )
  }
