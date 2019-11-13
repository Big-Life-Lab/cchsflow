#' Smoking pack-years
#' 
#' Measures an individual smoking pack-years based on various CCHS smoking variables
#' 
#' @param TypeOfSmoker derived variable that classifies an individual's smoking status. 
#' cchsflow variable name: SMKDSTY 
#' 
#' @param Age_cont continuous age variable. 
#' cchsflow variable name: DHHGAGE_cont
#' 
#' @param stpd number of years since quitting smoking. Variable asked to former daily smokers who quit <3 years ago. 
#' cchsflow variable name: SMK_09A_B
#' 
#' @param stpdy number of years since quitting smoking. Variable asked to former daily smokers who quit >=3 years ago. 
#' cchsflow variable name: SMKG09C 
#' 
#' @param agecigd age started smoking daily. Variable asked to daily smokers. cchsflow variable name: SMKG203_cont 
#' 
#' @param agecigfd age started smoking daily. Variable asked to former daily smokers. 
#' cchsflow variable name: SMKG207_cont 
#' 
#' @param cigdayd number of cigarettes smoked per day. Variable asked to daily smokers.
#' cchsflow variable name: SMK_204 
#' 
#' @param cigdayo number of cigarettes smoked per day. Variable asked to occasional smokers
#' cchsflow variable name: SMK_05B
#' 
#' @param cigdayf number of cigarettes smoked per day. Variable asked to former daily smokers
#' cchsflow variable name: SMK_208
#' 
#' @param dayocc number of days smoked at least one cigarette
#' cchsflow variable name: SMK_05C
#' 
#' @param s100 smoked 100 cigarettes in lifetime (y/n)
#' cchsflow variable name: SMK_01A
#' 
#' @param agec1 age smoked first cigarette
#' cchsflow variable name: SMKG01C_cont
#' 
#' @return value for smoking pack-years in the PackYears_derived variable
#' @export
PackYears_fun <-
  function(TypeOfSmoker, Age_cont, stpd, stpdy, agecigd, agecigfd, cigdayd, cigdayo, cigdayf, dayocc, agec1, s100) {
    #Time since quit for former daily smokers
    tsq_ds_fun <- function(stpd, stpdy) {
      stpdy <-
        ifelse2(stpdy==1, 4,
        ifelse2(stpdy==2, 8,
        ifelse2(stpdy==3, 12, NA)))
      tsq_ds <-
        ifelse2(stpd==1, 0.5,
        ifelse2(stpd==2, 1.5,
        ifelse2(stpd==3, 2.5,
        ifelse2(stpd==4, stpdy, NA))))
    }
    tsq_ds<-tsq_ds_fun(stpd, stpdy)
    # PackYears for Daily Smoker
    ifelse2(TypeOfSmoker==1, pmax(((Age_cont - agecigd)*(cigdayd/20)), 0.0137),
    # PackYears for Occasional Smoker (former daily)     
    ifelse2(TypeOfSmoker==2, pmax(((Age_cont - agecigfd - tsq_ds)*(cigdayf/20)), 0.0137) + (pmax((cigdayo*dayocc/30), 1)*tsq_ds),
    # PackYears for Occasional Smoker (never daily)      
    ifelse2(TypeOfSmoker==3, (pmax((cigdayo*dayocc/30), 1)/20)*(Age_cont - agec1),
    # PackYears for former daily smoker (non-smoker now)      
    ifelse2(TypeOfSmoker==4, pmax(((Age_cont - agecigfd - tsq_ds)*(cigdayf/20)), 0.0137),
    # PackYears for former occasional smoker (non-smoker now) who smoked at least 100 cigarettes lifetime      
    ifelse2(TypeOfSmoker==5 & s100==1, 0.0137,
    # PackYears for former occasional smoker (non-smoker now) who have not smoked at least 100 cigarettes lifetime      
    ifelse2(TypeOfSmoker==5 & s100==2, 0.007,
    # Non-smoker      
    ifelse2(TypeOfSmoker==6, 0, NA)))))))
  }