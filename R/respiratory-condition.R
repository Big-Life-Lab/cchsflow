#' @title resp_condition_fun1
#'
#' @description This is one of 3 functions used to create a derived variable
#'  (resp_condition_der) that determines if a respondents has a respiratory
#'  condition. 3 different functions have been created to account for the fact
#'  that different respiratory variables are used across CCHS cycles. This
#'  function is for CCHS cycles (2009-2014) that only use COPD and Emphysema as
#'  a combined variable. Asthma is used across CCHS cycles as a separate
#'  variable.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#' 
#' @param CCC_031 variable indicating if respondent has asthma
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
#' # variables. Then by using merge_rec_data() you can combine
#' # resp_condition_der across cycles.
#'
#' library(cchsflow)
#'
#' resp2009_2010 <- suppressWarnings(rec_with_table(
#'   cchs2009_2010_p,  c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' head(resp2009_2010)
#'
#' resp2011_2012 <- suppressWarnings(rec_with_table(
#'   cchs2011_2012_p, c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' tail(resp2011_2012)
#'
#' combined_resp <-
#'  suppressWarnings(merge_rec_data(resp2009_2010, resp2011_2012))
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @seealso \code{\link{resp_condition_fun2}}, \code{\link{resp_condition_fun3}}
#'
#' @export
resp_condition_fun1 <-
  function(DHHGAGE_cont, CCC_091, CCC_031) {
    # Argument verification
    if ((CCC_091 %notin% 1:2) |
        (CCC_031 %notin% 1:2)) {
      warning(
        paste(
          "In DHHGAGE_cont:",
          DHHGAGE_cont,
          ", CCC_091:",
          CCC_091,
          ", CCC_031:",
          CCC_031,
          "one or more of the respiratory arguments was outside the 1:2 allowed
          range however the condition is still calculated",
          sep = ""
        ), call. = FALSE
      )
    }
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
        (CCC_091 == 1 | CCC_031 == 1)), 1,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
        (CCC_091 == 1 | CCC_031 == 1)), 2,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont < 35) &
        (CCC_091 == 2 | CCC_031 == 2)), 3,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
        (CCC_091 == 2 & CCC_031 == 2)), 3, 
      if_else2((CCC_091 == "NA(a)" & CCC_031 == "NA(a)"), "NA(a)", "NA(b)")
                )
          )
      )
    )
  }

#' @title resp_condition_fun2
#'
#' @description This is one of 3 functions used to create a derived variable
#'  (resp_condition_der) that determines if a respondents has a respiratory
#'  condition. This function is for CCHS cycles (2005-2007) that use COPD &
#'  Emphysema as separate variables, as well as Bronchitis. Asthma is used
#'  across CCHS cycles as a separate variable.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_91E variable indicating if respondent has emphysema
#'
#' @param CCC_91F variable indicating if respondent has COPD
#'
#' @param CCC_91A variable indicating if respondent has chronic bronchitis
#' 
#' @param CCC_031 variable indicating if respondent has asthma
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
#' # variables. Then by using merge_rec_data() you can combine
#' # resp_condition_der across cycles.
#'
#' library(cchsflow)
#'
#' resp2005 <- suppressWarnings(rec_with_table(
#'   cchs2005_p, c(
#'     "DHHGAGE_cont", "CCC_91E", "CCC_91F", "CCC_91A", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' head(resp2005)
#'
#' resp2007_2008 <- suppressWarnings(rec_with_table(
#'   cchs2007_2008_p,  c(
#'     "DHHGAGE_cont", "CCC_91E", "CCC_91F", "CCC_91A", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' tail(resp2007_2008)
#'
#' combined_resp <- suppressWarnings(merge_rec_data(resp2005, resp2007_2008))
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @seealso \code{\link{resp_condition_fun1}}, \code{\link{resp_condition_fun3}}
#'
#' @export
resp_condition_fun2 <-
  function(DHHGAGE_cont, CCC_91E, CCC_91F, CCC_91A, CCC_031) {
    # Argument verification
    if ((CCC_91E %notin% 1:2) |
        (CCC_91F %notin% 1:2) |
        (CCC_91A %notin% 1:2) |
        (CCC_031 %notin% 1:2)) {
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
          ", CCC_031:",
          CCC_031,
          "one or more of the respiratory arguments was outside the 1:2 allowed
          range however the condition is still calculated",
          sep = ""
        ), call. = FALSE
      )
    }

    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
        (CCC_91E == 1 | CCC_91F == 1 | CCC_91A == 1 | CCC_031 == 1)), 1,
    if_else2(
      ((DHHGAGE_cont >= 30 & DHHGAGE_cont < 35) &
        (CCC_91E == 1 | CCC_91F == 1 | CCC_91A == 1 | CCC_031 == 1)), 2,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont < 30) &
        (CCC_91A == 1 | CCC_031 == 1)), 2,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont < 30) &
        (CCC_91A == 2 & CCC_031 == 2)), 3,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 30) &
        (CCC_91E == 2 & CCC_91F == 2 & CCC_91A == 2 & CCC_031 == 2)), 3,
    if_else2((CCC_91E == "NA(a)" & CCC_91F == "NA(a)" &
                 CCC_91A == "NA(a)" & CCC_031 == "NA(a)"), "NA(a)", "NA(b)")
            )
          )
        )
      )
    )
  }

#' @title resp_condition_fun3
#'
#' @description This is one of 3 functions used to create a derived variable
#'  (resp_condition_der) that determines if a respondents has a respiratory
#'  condition. This function for CCHS cycles (2001-2003) that use COPD and
#'  Emphysema as a combined variable, as well as Bronchitis. Asthma is used
#'  across CCHS cycles as a separate variable.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#'
#' @param CCC_91A variable indicating if respondent has chronic bronchitis
#'  
#' @param CCC_031 variable indicating if respondent has asthma
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
#' # variables. Then by using merge_rec_data() you can combine
#' # resp_condition_der across cycles.
#'
#' library(cchsflow)
#'
#' resp2001 <- suppressWarnings(rec_with_table(
#'   cchs2001_p, c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_91A", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' head(resp2001)
#'
#' resp2003 <- suppressWarnings(rec_with_table(
#'   cchs2003_p,c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_91A", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' tail(resp2003)
#'
#' combined_resp <- suppressWarnings(merge_rec_data(resp2001, resp2003))
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @seealso \code{\link{resp_condition_fun1}}, \code{\link{resp_condition_fun2}}
#'
#' @export
resp_condition_fun3 <-
  function(DHHGAGE_cont, CCC_091, CCC_91A, CCC_031) {
    # Argument verification
    if ((CCC_091 %notin% 1:2) |
        (CCC_91A %notin% 1:2) |
        (CCC_031 %notin% 1:2)) {
      warning(
        paste(
          "In DHHGAGE_cont:",
          DHHGAGE_cont,
          ", CCC_091:",
          CCC_091,
          ", CCC_91A:",
          CCC_91A,
          ", CCC_031:",
          CCC_031,
          "one or more of the respiratory arguments was outside the 1:2 allowed
          range however the condition is still calculated",
          sep = ""
        ), call. = FALSE
      )
    }

    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 35) &
        (CCC_091 == 1 | CCC_91A == 1 | CCC_031 == 1)), 1,
    if_else2(
      ((DHHGAGE_cont >= 30 & DHHGAGE_cont < 35) &
        (CCC_091 == 1 | CCC_91A == 1 | CCC_031 == 1)), 2,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont < 30) &
        (CCC_91A == 1 | CCC_031 == 1)), 2,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont < 30) &
        (CCC_91A == 2 & CCC_031 == 2)), 3,
    if_else2(
      ((DHHGAGE_cont > 0 & DHHGAGE_cont >= 30) & 
        (CCC_091 == 2 & CCC_91A == 2 & CCC_031 == 2)), 3,
    if_else2((CCC_091 == "NA(a)" & CCC_91A == "NA(a)" & CCC_031 == "NA(a)"), 
             "NA(a)", "NA(b)")
            )
          )
        )
      )
    )
  }

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
#' # variables. Then by using merge_rec_data() you can combine COPD_Emph_der
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
#' combined_COPD <- suppressWarnings(merge_rec_data(COPD2005, COPD2007_2008))
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
    if ((CCC_91E %notin% 1:2) |
        (CCC_91F %notin% 1:2)) {
      warning(
        paste(
          "In DHHGAGE_cont:",
          DHHGAGE_cont,
          ", CCC_91E:",
          CCC_91E,
          ", CCC_91F:",
          CCC_91F,
          "one or more of the arguments was outside the 1:2 allowed
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
               (CCC_91E == 2 & CCC_91F == 2)), 3,
            if_else2(
              (CCC_91E == "NA(a)" & CCC_91F == "NA(a)"), "NA(a)", "NA(b)")
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
#'  function is for CCHS cycles (2001-2003, 2009-2014) that use COPD and
#'  Emphysema as a combined variable. 
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
#' # variables. Then by using merge_rec_data() you can combine COPD_Emph_der
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
#' combined_COPD <- suppressWarnings(merge_rec_data(COPD2001, COPD2014))
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
    if (CCC_091 %notin% 1:2) {
      warning(
        paste(
          "In DHHGAGE_cont:",
          DHHGAGE_cont,
          ", CCC_091:",
          CCC_091,
          "one or more of the arguments was outside the 1:2 allowed
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
               (CCC_091 == 2)), 3,
            if_else2(
              (CCC_091 == "NA(a)"),
              "NA(a)", "NA(b)"
            )
          )
        )
      )
    )
  }
