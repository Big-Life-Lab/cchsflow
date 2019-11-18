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
#' @return a categorical variable (RespCondition_derived) with 3 levels:
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
#' @return a categorical variable (RespCondition_derived) with 3 levels:
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
#' @return a categorical variable (RespCondition_derived) with 3 levels:
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