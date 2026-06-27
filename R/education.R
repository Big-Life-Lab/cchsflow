#' @title Derive 4-category education variable (EDUDR04) for 2015+ cycles
#'
#' @description Derives the 4-category highest-education variable (EDUDR04) for
#'   CCHS cycles 2015-2016 onwards, where the pre-2015 derived variable EDUDR04
#'   is no longer provided. Reconstructs the equivalent 4-level classification
#'   from the four raw education-module questions (EHG2_01 through EHG2_04).
#'
#' @param EHG2_01 Highest grade of elementary or high school. Codes:
#'   1 = Grade 8 or lower; 2 = Grade 9-10; 3 = Grade 11-13;
#'   6 = not applicable; 7/8/9 = don't know / refusal / not stated.
#' @param EHG2_02 Completed high school diploma or equivalent. Codes:
#'   1 = Yes; 2 = No; 6 = not applicable; 7/8/9 = missing.
#' @param EHG2_03 Other education - certificate/diploma/degree. Codes:
#'   1 = Yes; 2 = No; 6 = not applicable; 7/8/9 = missing.
#' @param EHG2_04 Highest certificate, diploma or degree. Codes:
#'   1-7 = credential levels; 96 = not applicable; 97-99 = missing.
#' @param output_format Output missing data format: "tagged_na" (default) or "original".
#'
#' @return Integer 1-4, or tagged NA:
#'   \enumerate{
#'     \item Less than secondary school graduation
#'     \item Secondary school graduation, no post-secondary
#'     \item Some post-secondary education
#'     \item Post-secondary graduation
#'   }
#'
#' @examples
#' # Post-secondary graduate with bachelor's degree
#' derive_EDUDR04_2015plus(3, 1, 1, 6)
#'
#' # Less than high school
#' derive_EDUDR04_2015plus(1, 2, 2, 96)
#'
#' @export
derive_EDUDR04_2015plus <- function(EHG2_01, EHG2_02, EHG2_03, EHG2_04,
                                     output_format = "tagged_na") {
  # === STEP 1: DATA CLEANING ===
  cleaned <- clean_variables(vars = list(
    EHG2_01 = EHG2_01,
    EHG2_02 = EHG2_02,
    EHG2_03 = EHG2_03,
    EHG2_04 = EHG2_04
  ), output_format = "tagged_na")

  e1 <- cleaned$EHG2_01
  e2 <- cleaned$EHG2_02
  e3 <- cleaned$EHG2_03
  e4 <- cleaned$EHG2_04

  # === STEP 2: DERIVATION LOGIC ===
  # Note: EHG2_01 missing does NOT block derivation if EHG2_02/03/04 are valid.
  # Category logic is ordered by priority (post-secondary first).
  result <- dplyr::case_when(
    # All valid skip — education module not asked
    any_missing(e1) & any_missing(e2) & any_missing(e3) & any_missing(e4) ~
      assign_missing("not_applicable", "EDUDR04", output_format),
    # EHG2_02 or EHG2_03 missing — can't determine education level
    any_missing(e2) | any_missing(e3) ~
      assign_missing("not_stated", "EDUDR04", output_format),
    # Cat 4: Post-secondary graduation (EHG2_04 between 3 and 7)
    e4 %in% c(3, 4, 5, 6, 7) ~ 4L,
    # Cat 3: Some post-secondary (EHG2_03 == 1 but highest credential is HS or less)
    e3 == 1 & e4 %in% c(1, 2) ~ 3L,
    # Cat 2: Secondary graduation, no post-secondary
    e2 == 1 & e3 == 2 ~ 2L,
    # Cat 1: Less than secondary
    # If e1 is missing AND no HS diploma, result is missing (can't determine grade)
    any_missing(e1) & e2 == 2 & e3 == 2 ~
      assign_missing("not_stated", "EDUDR04", output_format),
    # If e1 is valid low grade OR no HS diploma, it's less than secondary
    e2 == 2 & e3 == 2 ~ 1L,
    # Fallback
    .default = assign_missing("not_stated", "EDUDR04", output_format)
  )

  # === STEP 3: OUTPUT VALIDATION ===
  output_cleaned <- clean_variables(vars = list(
    EDUDR04 = result
  ), output_format = output_format)

  return(prep_cat_output(output_cleaned$EDUDR04))
}
