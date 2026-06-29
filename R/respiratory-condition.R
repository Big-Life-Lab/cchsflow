#' @title CCC_091_fun1
#'
#' @description This is one of 2 functions used to derive CCC_091
#'  (COPD/emphysema/chronic bronchitis) for CCHS cycles (2001-2003) in which
#'  the concept is split across two separate questions: CCC_91A (chronic
#'  bronchitis) and CCC_91B (emphysema/COPD combined). A respondent is
#'  considered to have the condition if they answered yes to either question.
#'
#' @param CCC_91A variable indicating if respondent has chronic bronchitis
#'
#' @param CCC_91B variable indicating if respondent has emphysema or COPD
#'
#' @return a categorical variable (CCC_091) with 2 levels:
#'
#'  \enumerate{
#'  \item respondent has COPD, emphysema, or chronic bronchitis
#'  \item respondent does not have COPD, emphysema, or chronic bronchitis
#'  }
#'
#' @examples
#' # Using CCC_091_fun1() to derive CCC_091 for 2001/2003 cycles.
#' # CCC_091_fun1() is specified in variable_details.csv along with the
#' # CCHS variables and cycles included.
#'
#' library(cchsflow)
#'
#' CCC_091_2001 <- suppressWarnings(rec_with_table(
#'   cchs2001_p, c("CCC_91A", "CCC_91B", "CCC_091")
#' ))
#'
#' head(CCC_091_2001)
#' @seealso \code{\link{CCC_091_fun2}}
#'
#' @export
derive_CCC_091_2001to2003 <- function(CCC_91A, CCC_91B,
                                       output_format = "tagged_na") {
  # === STEP 1: DATA CLEANING ===
  cleaned <- clean_variables(vars = list(
    CCC_91A = CCC_91A,
    CCC_91B = CCC_91B
  ), output_format = "tagged_na")

  a <- cleaned$CCC_91A
  b <- cleaned$CCC_91B

  # === STEP 2: DERIVE CCC_091 ===
  result <- dplyr::case_when(
    a == 1 | b == 1 ~ 1L,
    a == 2 & b == 2 ~ 2L,
    any_missing(a, b) ~
      get_priority_missing(a, b, output_format = output_format),
    .default = assign_missing("not_stated", "CCC_091", output_format)
  )

  # === STEP 3: OUTPUT VALIDATION ===
  output_cleaned <- clean_variables(vars = list(
    CCC_091 = result
  ), output_format = output_format)

  return(prep_cat_output(output_cleaned$CCC_091))
}

#' @title CCC_091_fun2
#'
#' @description This is one of 2 functions used to derive CCC_091
#'  (COPD/emphysema/chronic bronchitis) for CCHS cycles (2005-2008) in which
#'  the concept is split across three separate questions: CCC_91A (chronic
#'  bronchitis), CCC_91E (emphysema), and CCC_91F (COPD). A respondent is
#'  considered to have the condition if they answered yes to any question.
#'
#' @param CCC_91A variable indicating if respondent has chronic bronchitis
#'
#' @param CCC_91E variable indicating if respondent has emphysema
#'
#' @param CCC_91F variable indicating if respondent has COPD
#'
#' @return a categorical variable (CCC_091) with 2 levels:
#'
#'  \enumerate{
#'  \item respondent has COPD, emphysema, or chronic bronchitis
#'  \item respondent does not have COPD, emphysema, or chronic bronchitis
#'  }
#'
#' @examples
#' # Using CCC_091_fun2() to derive CCC_091 for 2005/2007-08 cycles.
#' # CCC_091_fun2() is specified in variable_details.csv along with the
#' # CCHS variables and cycles included.
#'
#' library(cchsflow)
#'
#' CCC_091_2005 <- suppressWarnings(rec_with_table(
#'   cchs2005_p, c("CCC_91A", "CCC_91E", "CCC_91F", "CCC_091")
#' ))
#'
#' head(CCC_091_2005)
#' @seealso \code{\link{CCC_091_fun1}}
#'
#' @export
derive_CCC_091_2005to2008 <- function(CCC_91A, CCC_91E, CCC_91F,
                                       output_format = "tagged_na") {
  # === STEP 1: DATA CLEANING ===
  cleaned <- clean_variables(vars = list(
    CCC_91A = CCC_91A,
    CCC_91E = CCC_91E,
    CCC_91F = CCC_91F
  ), output_format = "tagged_na")

  a <- cleaned$CCC_91A
  e <- cleaned$CCC_91E
  f <- cleaned$CCC_91F

  # === STEP 2: DERIVE CCC_091 ===
  result <- dplyr::case_when(
    a == 1 | e == 1 | f == 1 ~ 1L,
    a == 2 & e == 2 & f == 2 ~ 2L,
    any_missing(a, e, f) ~
      get_priority_missing(a, e, f, output_format = output_format),
    .default = assign_missing("not_stated", "CCC_091", output_format)
  )

  # === STEP 3: OUTPUT VALIDATION ===
  output_cleaned <- clean_variables(vars = list(
    CCC_091 = result
  ), output_format = output_format)

  return(prep_cat_output(output_cleaned$CCC_091))
}

# ==============================================================================
# CCC_091_der — Age-stratified COPD/emphysema/bronchitis (3 categories)
# ==============================================================================

#' @title Age-stratified COPD/emphysema/bronchitis indicator
#'
#' @description Creates a 3-category derived variable from CCC_091
#'   (COPD/emphysema/bronchitis) stratified by age 35 threshold.
#'
#' @param age Continuous age variable (DHHGAGE_cont for PUMF, DHH_AGE for
#'   Master). Worksheet routes the appropriate source.
#' @param CCC_091 Harmonized COPD/emphysema/bronchitis indicator (1 = yes,
#'   2 = no). Available for all cycles via direct question or derivation.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Categorical variable (CCC_091_der) with 3 levels:
#'   \enumerate{
#'     \item Age >= 35 with COPD/emphysema/bronchitis
#'     \item Age < 35 with COPD/emphysema/bronchitis
#'     \item No COPD/emphysema/bronchitis
#'   }
#'
#' @note v3.0.0, last updated: 2026-06-29, status: active. Replaces
#'   COPD_Emph_der_fun1/fun2 which used inconsistent source variables
#'   across eras.
#' @export
categorize_CCC_091 <- function(age, CCC_091, output_format = "tagged_na") {
  # === STEP 1: DATA CLEANING ===
  cleaned <- clean_variables(vars = list(
    DHHGAGE_cont = age,
    CCC_091 = CCC_091
  ), output_format = "tagged_na")

  a <- cleaned$DHHGAGE_cont
  c091 <- cleaned$CCC_091

  # === STEP 2: DOMAIN LOGIC ===
  result <- dplyr::case_when(
    any_missing(a, c091) ~
      get_priority_missing(a, c091, output_format = output_format),
    a >= 35 & c091 == 1 ~ 1L,
    a < 35  & c091 == 1 ~ 2L,
    c091 == 2           ~ 3L,
    .default = assign_missing("not_stated", "CCC_091_der", output_format)
  )

  # === STEP 3: OUTPUT VALIDATION ===
  output_cleaned <- clean_variables(vars = list(
    CCC_091_der = result
  ), output_format = output_format)

  prep_cat_output(output_cleaned$CCC_091_der)
}

#' @title resp_condition_fun
#'
#' @description This function is used to create a derived variable
#'  (resp_condition_der) that determines if a respondent has a respiratory
#'  condition. It uses the combined COPD, Emphysema, and Bronchitis variable
#'  (CCC_091) and Asthma (CCC_031) across all CCHS cycles.
#'
#' @param age continuous age variable (DHHGAGE_cont for _p cycles, DHH_AGE for
#'  _m cycles).
#'
#' @param CCC_091 variable indicating if respondent has COPD, emphysema, or
#'  bronchitis
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
#' # Using resp_condition_fun() to create values across CCHS cycles
#' # (2009-2014) resp_condition_fun() is specified in
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
#'   cchs2009_2010_p, c(
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
#'   suppressWarnings(merge_rec_data(resp2009_2010, resp2011_2012))
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @export
resp_condition_fun <-
  function(age, CCC_091, CCC_031) {
    `%notin%` <- Negate(`%in%`)
    # Argument verification
    if ((CCC_091 %notin% 1:2) |
      (CCC_031 %notin% 1:2)) {
      warning(
        paste(
          "In age:",
          age,
          ", CCC_091:",
          CCC_091,
          ", CCC_031:",
          CCC_031,
          "one or more of the respiratory arguments was outside the 1:2 allowed
          range however the condition is still calculated",
          sep = ""
        ),
        call. = FALSE
      )
    }
    resp_condition <-
      if_else2(
        ((age > 0 & age >= 35) &
          (CCC_091 == 1 | CCC_031 == 1)), 1,
        if_else2(
          ((age > 0 & age < 35) &
            (CCC_091 == 1 | CCC_031 == 1)), 2,
          if_else2(
            ((age > 0 & age < 35) &
              (CCC_091 == 2 | CCC_031 == 2)), 3,
            if_else2(
              ((age > 0 & age >= 35) &
                (CCC_091 == 2 & CCC_031 == 2)), 3,
              if_else2(
                (CCC_091 == "NA(a)" & CCC_031 == "NA(a)"), "NA(a)",
                "NA(b)"
              )
            )
          )
        )
      )
    return(resp_condition)
  }
