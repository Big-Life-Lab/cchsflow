#' Custom ifelse function that evaluates missing (NA) values
#' 
#' If the logical argument compares to a value that is NA, it will set `x` to `FALSE`  
#' 
#' @param x A logical argument
#' 
#' @param a value if `x` is `TRUE`
#' 
#' @param b value if `x` is `FALSE`
#' 
#' @return None
#' 
#' @examples ifelse2((age<18),"child", "adult")
#' 
#' @export
ifelse2 <- function(x, a, b) {
  falseifNA <- function(x) {
    ifelse(is.na(x), FALSE, x)
  }
  ifelse(falseifNA(x), a, b)
}

#' Body Mass Index (BMI) derived variable
#' 
#' This function creates a derived BMI variable independent from the BMI variable provided by the CCHS. 
#' 
#' This variable uses the CCHS variables for height and weight.
#' 
#' @param HWTGHTM CCHS variable for height (in meters)
#' 
#' @param HWTGWTK CCHS variable for weight (in kilograms)
#' 
#' @return a value for BMI
#' 
#' @export
BMI_derived <- 
  function(HWTGHTM, 
           HWTGWTK) {
    ifelse2((!is.na(HWTGHTM)) & (!is.na(HWTGWTK)), 
            (HWTGWTK/(HWTGHTM*HWTGHTM)), NA)
  }

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
#' @return value for smoking pack-years
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
#' @return value of percentage of time spent living in Canada
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

#' Respiratory condition variable
#' 
#' This function creates a derived variable that determines if a subject has a respiratory condition
#' 
#' This function is for CCHS cycles that only use COPD and Emphysema as a combined variable
#' 
#' @param Age_cont continuous age variable. 
#' cchsflow variable name: DHHGAGE_cont
#' 
#' @param COPD_Emphys variable indicating if subject has either COPD or Emphysema
#' cchsflow variable name: CCC_091
#' 
#' @return a categorical variable with 3 levels:
#' 1 - subject is over the age of 35 and has a respiratory condition
#' 2 - subject is under the age of 35 and has a respiratory condition
#' 3 - subject does not have a respiratory condition
#' 
#' @export
RespCondition_fun1 <-
  function(Age_cont, COPD_Emphys) {
    ifelse2((Age_cont>35 & COPD_Emphys == 1), 1, 
    ifelse2((Age_cont<35 & COPD_Emphys == 1), 2, 
    ifelse2(COPD_Emphys == 2, 3, NA)))
  }


#' Respiratory condition variable
#' 
#' This function for CCHS cycles that use COPD & Emphysema as separate variables, as well as Bronchitis
#' 
#' @param Age_cont continuous age variable. 
#' cchsflow variable name: DHHGAGE_cont
#' 
#' @param Emphys variable indicating if subject has emphysema
#' cchsflow variable name: CCC_91E 
#' 
#' @param COPD variable indicating if subject has COPD
#' cchsflow variable name: CCC_91F 
#' 
#' @param Bronc variable indicating if subject has chronic bronchitis
#' cchsflow variable name: CCC_91A
#' 
#' @return a categorical variable with 3 levels:
#' 1 - subject is over the age of 35 and has a respiratory condition
#' 2 - subject is under the age of 35 and has a respiratory condition
#' 3 - subject does not have a respiratory condition
#' 
#' @export
RespCondition_fun2 <-
  function(Age_cont, Emphys, COPD, Bronch) {
    ifelse2((Age_cont>35 & (Emphys == 1 | COPD == 1 | Bronch == 1)), 1, 
    ifelse2((Age_cont<35 & Emphys == 1 | COPD == 1 | Bronch == 1), 2,
    ifelse2((Emphys == 2 & COPD == 2 & Bronch == 2), 3, NA)))
  }

#' Respiratory condition variable
#' 
#' This function for CCHS cycles that use COPD and Emphysema as a combined variable, as well as Bronchitis
#' 
#' @param Age_cont continuous age variable. 
#' cchsflow variable name: DHHGAGE_cont
#' 
#' @param COPD_Emphys variable indicating if subject has either COPD or Emphysema
#' cchsflow variable name: CCC_091
#' 
#' @param Bronc variable indicating if subject has chronic bronchitis
#' cchsflow variable name: CCC_91A
#' 
#' @return a categorical variable with 3 levels:
#' 1 - subject is over the age of 35 and has a respiratory condition
#' 2 - subject is under the age of 35 and has a respiratory condition
#' 3 - subject does not have a respiratory condition
#' 
#' @export
RespCondition_fun3 <-
  function(Age_cont, COPD_Emphys, Bronch) {
    ifelse2((Age_cont > 35 & (COPD_Emphys == 1 | Bronch == 1)), 1,
    ifelse2((Age_cont < 35 & (COPD_Emphys == 1 | Bronch == 1)), 2,
    ifelse2((COPD_Emphys == 2 & Bronch == 2), 3, NA)))
  }