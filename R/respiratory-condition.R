#' @title RespCondition_fun1
#' 
#' @description This is one of 3 functions used to create a derived variable (RespCondition_derived) that determines if a respondents has a respirtory condition.
#'  3 different functions have been created to account for the fact that different respiratory variables are used across CCHS cycles.
#'  This function is for CCHS cycles (2009-2014) that only use COPD and Emphysema as a combined variable. 
#' 
#' @param DHHGAGE_cont continuous age variable. 
#' 
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#' 
#' @return a categorical variable (RespCondition_derived) with 3 levels:
#'  1 - respondent is over the age of 35 and has a respiratory condition
#'  2 - respondent is under the age of 35 and has a respiratory condition
#'  3 - respondent does not have a respiratory condition
#' 
#' @seealso RespCondition_fun2, RespCondition_fun3
#' 
#' @export
RespCondition_fun1 <-
  function(DHHGAGE_cont, CCC_091) {
    ifelse2((DHHGAGE_cont>35 & CCC_091 == 1), 1, 
    ifelse2((DHHGAGE_cont<35 & CCC_091 == 1), 2, 
    ifelse2(CCC_091 == 2, 3, NA)))
  }


#' @title RespCondition_fun2
#' 
#' @description This is one of 3 functions used to create a derived variable (RespCondition_derived) that determines if a respondents has a respirtory condition. 
#'  This function is for CCHS cycles (2005-2007) that use COPD & Emphysema as separate variables, as well as Bronchitis.
#' 
#' @param DHHGAGE_cont continuous age variable. 
#' 
#' @param CCC_91E variable indicating if respondent has emphysema
#' 
#' @param CCC_91F variable indicating if respondent has COPD
#' 
#' @param CCC_91A variable indicating if respondent has chronic bronchitis
#' 
#' @return a categorical variable (RespCondition_derived) with 3 levels:
#'  1 - respondent is over the age of 35 and has a respiratory condition
#'  2 - respondent is under the age of 35 and has a respiratory condition
#'  3 - respondent does not have a respiratory condition
#' 
#' @seealso RespCondition_fun1, RespCondition_fun3
#' 
#' @export
RespCondition_fun2 <-
  function(DHHGAGE_cont, CCC_91E, CCC_91F, CCC_91A) {
    ifelse2((DHHGAGE_cont>35 & (CCC_91E == 1 | CCC_91F == 1 | CCC_91A == 1)), 1, 
    ifelse2((DHHGAGE_cont<35 & (CCC_91E == 1 | CCC_91F == 1 | CCC_91A == 1)), 2,
    ifelse2((CCC_91E == 2 & CCC_91F == 2 & CCC_91A == 2), 3, NA)))
  }

#' @title RespCondition_fun3
#' 
#' @description This is one of 3 functions used to create a derived variable (RespCondition_derived) that determines if a respondents has a respirtory condition.
#'  This function for CCHS cycles (2001-2003) that use COPD and Emphysema as a combined variable, as well as Bronchitis
#' 
#' @param DHHGAGE_cont continuous age variable. 
#' 
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#' 
#' @param CCC_91A variable indicating if respondent has chronic bronchitis
#'  cchsflow variable name: CCC_91A
#' 
#' @return a categorical variable (RespCondition_derived) with 3 levels:
#'  1 - respondent is over the age of 35 and has a respiratory condition
#'  2 - respondent is under the age of 35 and has a respiratory condition
#'  3 - respondent does not have a respiratory condition
#' 
#' @seealso RespCondition_fun1, RespCondition_fun2
#' 
#' @export
RespCondition_fun3 <-
  function(DHHGAGE_cont, CCC_091, CCC_91A) {
    ifelse2((DHHGAGE_cont > 35 & (CCC_091 == 1 | CCC_91A == 1)), 1,
    ifelse2((DHHGAGE_cont < 35 & (CCC_091 == 1 | CCC_91A == 1)), 2,
    ifelse2((CCC_091 == 2 & CCC_91A == 2), 3, NA)))
  }