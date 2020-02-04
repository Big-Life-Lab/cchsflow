#' @title resp_condition_fun1
#'
#' @description This is one of 3 functions used to create a derived variable
#'  (resp_condition_der) that determines if a respondents has a respiratory
#'  condition. 3 different functions have been created to account for the fact
#'  that different respiratory variables are used across CCHS cycles. This
#'  function is for CCHS cycles (2009-2014) that only use COPD and Emphysema as
#'  a combined variable.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#'
#' @return a categorical variable (resp_condition_der) with 3 levels:
#'
#'  \enumerate{
#'  \item respondent is over the age of 35 and has a respiratory condition
#'  \item respondent is under the age of 35 and has a respiratory condition
#'  \item respondent does not have a respiratory condition
#'  }
#'
#' @examples
#' # Using resp_condition_fun1() to create values across CCHS cycles
#' # (2009-2014) resp_condition_fun1() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform resp_condition_der, use rec_with_table() for each CCHS cycle
#' # and specify resp_condition_der, along with the various respiratory
#' # variables. Then by using bind_rows() you can combine resp_condition_der
#' # across cycles.
#'
#' library(cchsflow)
#'
#' resp2010 <- rec_with_table(
#'   cchs2010,  c(
#'     "DHHGAGE_cont", "CCC_091",
#'     "resp_condition_der"
#'   )
#' )
#'
#' head(resp2010)
#'
#' resp2012 <- rec_with_table(
#'   cchs2012, c(
#'     "DHHGAGE_cont", "CCC_091",
#'     "resp_condition_der"
#'   )
#' )
#'
#' tail(resp2012)
#'
#' combined_resp <- bind_rows(resp2010, resp2012)
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @seealso \code{\link{resp_condition_fun2}}, \code{\link{resp_condition_fun3}}
#'
#' @export
resp_condition_fun1 <-
  function(DHHGAGE_cont, CCC_091) {
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) & CCC_091 == 1), 1,
      if_else2(
        ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) & CCC_091 == 1), 2,
        if_else2(CCC_091 == 2, 3, NA)
      )
    )
  }

#' @title resp_condition_fun2
#'
#' @description This is one of 3 functions used to create a derived variable
#'  (resp_condition_der) that determines if a respondents has a respiratory
#'  condition. This function is for CCHS cycles (2005-2007) that use COPD &
#'  Emphysema as separate variables, as well as Bronchitis.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_91E variable indicating if respondent has emphysema
#'
#' @param CCC_91F variable indicating if respondent has COPD
#'
#' @param CCC_91A variable indicating if respondent has chronic bronchitis
#'
#' @return a categorical variable (resp_condition_der) with 3 levels:
#'
#'  \enumerate{
#'  \item respondent is over the age of 35 and has a respiratory condition
#'  \item respondent is under the age of 35 and has a respiratory condition
#'  \item respondent does not have a respiratory condition
#'  }
#'
#' @examples
#'
#' # Using resp_condition_fun2() to create values across CCHS cycles
#' # (2005-2007) resp_condition_fun2() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform resp_condition_der, use rec_with_table() for each CCHS cycle
#' # and specify resp_condition_der, along with the various respiratory
#' # variables. Then by using bind_rows() you can combine resp_condition_der
#' # across cycles.
#'
#' library(cchsflow)
#'
#' resp2005 <- rec_with_table(
#'   cchs2005, c(
#'     "DHHGAGE_cont", "CCC_91E", "CCC_91F", "CCC_91A",
#'     "resp_condition_der"
#'   )
#' )
#'
#' head(resp2005)
#'
#' resp2007_2008 <- rec_with_table(
#'   cchs2007_2008,  c(
#'     "DHHGAGE_cont", "CCC_91E", "CCC_91F", "CCC_91A",
#'     "resp_condition_der"
#'   )
#' )
#'
#' tail(resp2007_2008)
#'
#' combined_resp <- bind_rows(resp2005, resp2007_2008)
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @seealso \code{\link{resp_condition_fun1}}, \code{\link{resp_condition_fun3}}
#'
#' @export
resp_condition_fun2 <-
  function(DHHGAGE_cont, CCC_91E, CCC_91F, CCC_91A) {
    # Argument verification
    if ((!is_equal(CCC_91E, 1) &
         !is_equal(CCC_91E, 2)) |
        (!is_equal(CCC_91F, 1) &
         !is_equal(CCC_91F, 2)) |
        (!is_equal(CCC_91A, 1) &
         !is_equal(CCC_91A, 2))) {
      warning(
        paste(
          "In DHHGAGE_cont:",
          DHHGAGE_cont,
          ", CCC_91E:",
          CCC_91E,
          ", CCC_91F:",
          CCC_91F,
          ", CCC_91A:",
          CCC_91A,
          "one or more of the respiratory arguments was outside the 1:2 allowed
          range however the condition is still calculated",
          sep = ""
        ), call. = FALSE
      )
    }

    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
         (CCC_91E == 1 | CCC_91F == 1 | CCC_91A == 1)), 1,
      if_else2(
        ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
           (CCC_91E == 1 | CCC_91F == 1 | CCC_91A == 1)), 2,
        if_else2((CCC_91E == 2 & CCC_91F == 2 & CCC_91A == 2), 3, NA)
      )
    )
  }

#' @title resp_condition_fun3
#'
#' @description This is one of 3 functions used to create a derived variable
#'  (resp_condition_der) that determines if a respondents has a respiratory
#'  condition. This function for CCHS cycles (2001-2003) that use COPD and
#'  Emphysema as a combined variable, as well as Bronchitis
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#'
#' @param CCC_91A variable indicating if respondent has chronic bronchitis
#'  cchsflow variable name: CCC_91A
#'
#' @return a categorical variable (resp_condition_der) with 3 levels:
#'
#'  \enumerate{
#'  \item respondent is over the age of 35 and has a respiratory condition
#'  \item respondent is under the age of 35 and has a respiratory condition
#'  \item respondent does not have a respiratory condition
#'  }
#'
#' @examples
#' # Using resp_condition_fun3() to create values across CCHS cycles
#' # (2001-2003) resp_condition_fun3() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform resp_condition_der, use rec_with_table() for each CCHS cycle
#' # and specify resp_condition_der, along with the various respiratory
#' # variables. Then by using bind_rows() you can combine resp_condition_der
#' # across cycles.
#'
#' library(cchsflow)
#'
#' resp2001 <- rec_with_table(
#'   cchs2001, c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_91A",
#'     "resp_condition_der"
#'   )
#' )
#'
#' head(resp2001)
#'
#' resp2003 <- rec_with_table(
#'   cchs2003,c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_91A",
#'     "resp_condition_der"
#'   )
#' )
#'
#' tail(resp2003)
#'
#' combined_resp <- bind_rows(resp2001, resp2003)
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @seealso \code{\link{resp_condition_fun1}}, \code{\link{resp_condition_fun2}}
#'
#' @export
resp_condition_fun3 <-
  function(DHHGAGE_cont, CCC_091, CCC_91A) {
    # Argument verification
    if ((!is_equal(CCC_091, 1) &
         !is_equal(CCC_091, 2)) |
        (!is_equal(CCC_91A, 1) &
         !is_equal(CCC_91A, 2))) {
      warning(
        paste(
          "In DHHGAGE_cont:",
          DHHGAGE_cont,
          ", CCC_091:",
          CCC_091,
          ", CCC_91A:",
          CCC_91A,
          "one or more of the respiratory arguments was outside the 1:2 allowed
          range however the condition is still calculated",
          sep = ""
        ), call. = FALSE
      )
    }

    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
         (CCC_091 == 1 | CCC_91A == 1)), 1,
      if_else2(
        ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
           (CCC_091 == 1 | CCC_91A == 1)), 2,
        if_else2((CCC_091 == 2 & CCC_91A == 2), 3, NA)
      )
    )
  }
