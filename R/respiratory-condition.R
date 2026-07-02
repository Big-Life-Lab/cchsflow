#' @title Derive COPD/emphysema/bronchitis indicator for 2001-2003 cycles
#'
#' @description
#' Derives CCC_091 (COPD/emphysema/chronic bronchitis) for CCHS cycles
#' 2001-2003, where the concept is split across two source variables.
#'
#' @details
#' In CCHS 2001-2003, chronic bronchitis (CCC_91A) and emphysema/COPD
#' (CCC_91B) are asked as separate questions. This function combines
#' them into a single indicator: positive if either condition is
#' reported. Missing-data handling follows the v3 3-step architecture
#' with priority not applicable > not stated.
#'
#' @param CCC_91A Chronic bronchitis indicator (1 = yes, 2 = no).
#' @param CCC_91B Emphysema or COPD indicator (1 = yes, 2 = no).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector: 1 = has COPD/emphysema/bronchitis, 2 = does
#'   not. Missing data: \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' derive_CCC_091_2001to2003(1, 2)  # 1 (has condition)
#' derive_CCC_091_2001to2003(2, 2)  # 2 (no condition)
#'
#' @seealso \code{\link{derive_CCC_091_2005to2008}},
#'   \code{\link{categorize_CCC_091}}
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

#' @title Derive COPD/emphysema/bronchitis indicator for 2005-2008 cycles
#'
#' @description
#' Derives CCC_091 (COPD/emphysema/chronic bronchitis) for CCHS cycles
#' 2005-2008, where the concept is split across three source variables.
#'
#' @details
#' In CCHS 2005-2008, chronic bronchitis (CCC_91A), emphysema (CCC_91E),
#' and COPD (CCC_91F) are asked as separate questions. This function
#' combines them into a single indicator: positive if any condition is
#' reported. Missing-data handling follows the v3 3-step architecture
#' with priority not applicable > not stated.
#'
#' @param CCC_91A Chronic bronchitis indicator (1 = yes, 2 = no).
#' @param CCC_91E Emphysema indicator (1 = yes, 2 = no).
#' @param CCC_91F COPD indicator (1 = yes, 2 = no).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector: 1 = has COPD/emphysema/bronchitis, 2 = does
#'   not. Missing data: \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' derive_CCC_091_2005to2008(1, 2, 2)  # 1 (has condition)
#' derive_CCC_091_2005to2008(2, 2, 2)  # 2 (no condition)
#'
#' @seealso \code{\link{derive_CCC_091_2001to2003}},
#'   \code{\link{categorize_CCC_091}}
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

#' @title Categorize COPD/emphysema/bronchitis by age group
#'
#' @description
#' Creates a 3-category derived variable from CCC_091 (COPD/emphysema/
#' bronchitis) stratified by the age-35 threshold.
#'
#' @details
#' Clinically, COPD diagnosed before age 35 may reflect different
#' aetiology (e.g. alpha-1 antitrypsin deficiency) than later-onset
#' disease. The age-35 cutpoint follows the original CCHS derived
#' variable convention. Source-agnostic: the worksheet routes
#' DHHGAGE_cont (PUMF) or DHH_AGE (Master) to the age parameter.
#' Missing-data handling follows the v3 3-step architecture with
#' priority not applicable > not stated.
#'
#' @param age Continuous age variable (DHHGAGE_cont for PUMF, DHH_AGE for
#'   Master).
#' @param CCC_091 Harmonized COPD/emphysema/bronchitis indicator (1 = yes,
#'   2 = no).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector: 1 = age >= 35 with condition, 2 = age < 35
#'   with condition, 3 = no condition. Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' categorize_CCC_091(40, 1)  # 1 (age >= 35 with condition)
#' categorize_CCC_091(25, 1)  # 2 (age < 35 with condition)
#' categorize_CCC_091(40, 2)  # 3 (no condition)
#'
#' @seealso \code{\link{derive_CCC_091_2001to2003}},
#'   \code{\link{derive_CCC_091_2005to2008}}
#'
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
#' resp_condition_fun(40, 1, 1)  # 1 (over 35, has condition)
#' resp_condition_fun(25, 1, 1)  # 2 (under 35, has condition)
#' resp_condition_fun(40, 2, 2)  # 3 (no condition)
#'
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
