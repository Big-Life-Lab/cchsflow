#' @title Smoking pack-years
#' 
#' @description Measures an individual smoking pack-years based on various CCHS smoking variables. This is a popular variable
#'  used by researchers to quantify lifetime exposure to cigarette use.
#' 
#' @param SMKDSTY derived variable that classifies an individual's smoking status. 
#' 
#' @param DHHGAGE_cont continuous age variable. 
#' 
#' @param SMK_09A_B number of years since quitting smoking. Variable asked to former daily smokers who quit <3 years ago. 
#' 
#' @param SMKG09C number of years since quitting smoking. Variable asked to former daily smokers who quit >=3 years ago. 
#' 
#' @param SMKG203_cont age started smoking daily. Variable asked to daily smokers. 
#' 
#' @param SMKG207_cont age started smoking daily. Variable asked to former daily smokers. 
#' 
#' @param SMK_204 number of cigarettes smoked per day. Variable asked to daily smokers.
#' 
#' @param SMK_05B number of cigarettes smoked per day. Variable asked to occasional smokers
#' 
#' @param SMK_208 number of cigarettes smoked per day. Variable asked to former daily smokers
#' 
#' @param SMK_05C number of days smoked at least one cigarette
#' 
#' @param SMK_01A smoked 100 cigarettes in lifetime (y/n)
#' 
#' @param SMKG01C_cont age smoked first cigarette
#' 
#' @return value for smoking pack-years in the PackYears_derived variable
#' @export
PackYears_fun <-
  function(SMKDSTY, DHHGAGE_cont, SMK_09A_B, SMKG09C, SMKG203_cont, SMKG207_cont, SMK_204, SMK_05B, SMK_208, SMK_05C, SMKG01C_cont, SMK_01A) {
    #Time since quit for former daily smokers
    tsq_ds_fun <- function(SMK_09A_B, SMKG09C) {
      SMKG09C <-
        ifelse2(SMKG09C==1, 4,
        ifelse2(SMKG09C==2, 8,
        ifelse2(SMKG09C==3, 12, NA)))
      tsq_ds <-
        ifelse2(SMK_09A_B==1, 0.5,
        ifelse2(SMK_09A_B==2, 1.5,
        ifelse2(SMK_09A_B==3, 2.5,
        ifelse2(SMK_09A_B==4, SMKG09C, NA))))
    }
    tsq_ds<-tsq_ds_fun(SMK_09A_B, SMKG09C)
    # PackYears for Daily Smoker
    ifelse2(SMKDSTY==1, pmax(((DHHGAGE_cont - SMKG203_cont)*(SMK_204/20)), 0.0137),
    # PackYears for Occasional Smoker (former daily)     
    ifelse2(SMKDSTY==2, pmax(((DHHGAGE_cont - SMKG207_cont - tsq_ds)*(SMK_208/20)), 0.0137) + (pmax((SMK_05B*SMK_05C/30), 1)*tsq_ds),
    # PackYears for Occasional Smoker (never daily)      
    ifelse2(SMKDSTY==3, (pmax((SMK_05B*SMK_05C/30), 1)/20)*(DHHGAGE_cont - SMKG01C_cont),
    # PackYears for former daily smoker (non-smoker now)      
    ifelse2(SMKDSTY==4, pmax(((DHHGAGE_cont - SMKG207_cont - tsq_ds)*(SMK_208/20)), 0.0137),
    # PackYears for former occasional smoker (non-smoker now) who smoked at least 100 cigarettes lifetime      
    ifelse2(SMKDSTY==5 & SMK_01A==1, 0.0137,
    # PackYears for former occasional smoker (non-smoker now) who have not smoked at least 100 cigarettes lifetime      
    ifelse2(SMKDSTY==5 & SMK_01A==2, 0.007,
    # Non-smoker      
    ifelse2(SMKDSTY==6, 0, NA)))))))
  }