#' @title COPD_Emph_der_fun1
#'
#' @description This is one of 2 functions used to create a derived variable
#'  (COPD_Emph_der) that determines if a respondents has either COPD or 
#'  Emphysema. 2 different functions have been created to account for the fact
#'  that different respiratory variables are used across CCHS cycles. This
#'  function is for CCHS cycles (2005-2008) that use COPD and Emphysema as
#'  a combined variable. 
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#'
#' @param CCC_91E variable indicating if respondent has Emphysema
#'
#' @param CCC_91F variable indicating if respondent has COPD
#'
#' @return a categorical variable (COPD_Emph_der) with 3 levels:
#'
#'  \enumerate{
#'  \item respondent is over the age of 35 and has a respiratory condition
#'  \item respondent is under the age of 35 and has a respiratory condition
#'  \item respondent does not have a respiratory condition
#'  }
#'
#' @examples
#' # COPD_Emph_der_fun1() to create values across CCHS cycles
#' # (2005-2008) COPD_Emph_der_fun1() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform COPD_Emph_der, use rec_with_table() for each CCHS cycle
#' # and specify COPD_Emph_der, along with the various respiratory
#' # variables. Then by using bind_rows() you can combine COPD_Emph_der
#' # across cycles.
#'
#' library(cchsflow)
#'
#' COPD2005 <- suppressWarnings(rec_with_table(
#'   cchs2005_p,  c(
#'     "DHHGAGE_cont", "CCC_91E", "CCC_91F",
#'     "COPD_Emph_der"
#'   )
#' ))
#'
#' head(COPD2005)
#'
#' COPD2007_2008 <- suppressWarnings(rec_with_table(
#'   cchs2007_2008_p, c(
#'     "DHHGAGE_cont", "CCC_91E", "CCC_91F", 
#'     "COPD_Emph_der"
#'   )
#' ))
#'
#' tail(COPD2007_2008)
#'
#' combined_COPD <- suppressWarnings(bind_rows(COPD2005, COPD2007_2008))
#'
#' head(combined_COPD)
#' tail(combined_COPD)
#' @seealso \code{\link{COPD_Emph_der_fun2}}
#'
#' @export
#' 
COPD_Emph_der_fun1 <-
  function(DHHGAGE_cont, CCC_91E, CCC_91F) {
    # Argument verification
    if ((!is_equal(CCC_91E, 1) &
         !is_equal(CCC_91E, 2)) |
        (!is_equal(CCC_91F, 1) &
         !is_equal(CCC_91F, 2))) {
      warning(
        paste(
          "In DHHGAGE_cont:",
          DHHGAGE_cont,
          ", CCC_91E:",
          CCC_91E,
          ", CCC_91F:",
          CCC_91F,
          "one or more of the COPD_Emph arguments was outside the 1:2 allowed
          range however the condition is still calculated",
          sep = ""
        ), call. = FALSE
      )
    }
    if_else2(
      (DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
        (CCC_91E == 1 | CCC_91F == 1), 1,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
        (CCC_91E == 1) | CCC_91F == 1), 2,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
        (CCC_91E == 2 & CCC_91F == 2)), 3,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
        (CCC_91E == 2 & CCC_91F == 2 )), 3, "NA(b)"
                )
          )
      )
    )
  }

#' @title COPD_Emph_der_fun2
#'
#' @description This is one of 2 functions used to create a derived variable
#'  (COPD_Emph_der) that determines if a respondents has either COPD or 
#'  Emphysema. 2 different functions have been created to account for the fact
#'  that different respiratory variables are used across CCHS cycles. This
#'  function is for CCHS cycles (2001-2003, 2009-2014) that use COPD and Emphysema as
#'  a combined variable. 
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#'
#' @return a categorical variable (COPD_Emph_der) with 3 levels:
#'
#'  \enumerate{
#'  \item respondent is over the age of 35 and has a respiratory condition
#'  \item respondent is under the age of 35 and has a respiratory condition
#'  \item respondent does not have a respiratory condition
#'  }
#'
#' @examples
#' # COPD_Emph_der_fun2() to create values across CCHS cycles
#' # (2001-2003, 2009-2014) COPD_Emph_der_fun2() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform COPD_Emph_der, use rec_with_table() for each CCHS cycle
#' # and specify COPD_Emph_der, along with the various respiratory
#' # variables. Then by using bind_rows() you can combine COPD_Emph_der
#' # across cycles.
#'
#' library(cchsflow)
#'
#' COPD2001 <- suppressWarnings(rec_with_table(
#'   cchs2001_p,  c(
#'     "DHHGAGE_cont", "CCC_091",
#'     "COPD_Emph_der"
#'   )
#' ))
#'
#' head(COPD2001)
#'
#' COPD2014 <- suppressWarnings(rec_with_table(
#'   cchs2007_2008_p, c(
#'     "DHHGAGE_cont", "CCC_091", 
#'     "COPD_Emph_der"
#'   )
#' ))
#'
#' tail(COPD2014)
#'
#' combined_COPD <- suppressWarnings(bind_rows(COPD2001, COPD2014))
#'
#' head(combined_COPD)
#' tail(combined_COPD)
#' @seealso \code{\link{COPD_Emph_der_fun2}}
#'
#' @export
#' 

COPD_Emph_der_fun2 <-
  function(DHHGAGE_cont, CCC_091) {
    # Argument verification
    if ((!is_equal(CCC_091, 1) &
         !is_equal(CCC_091, 2))) {
      warning(
        paste(
          "In DHHGAGE_cont:",
          DHHGAGE_cont,
          ", CCC_091:",
          CCC_091,
          "one or more of the COPD/Emphysema arguments was outside the 1:2 allowed
          range however the condition is still calculated",
          sep = ""
        ), call. = FALSE
      )
    }
    if_else2(
      (DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
        (CCC_091 == 1), 1,
      if_else2(
        ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
           (CCC_091 == 1)), 2,
        if_else2(
          ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
             (CCC_091 == 2)), 3,
          if_else2(
            ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
               (CCC_091 == 2)), 3, "NA(b)"
          )
        )
      )
    )
  }
