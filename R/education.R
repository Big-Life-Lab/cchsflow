#' @title Derive 4-category education variable (EDUDR04)
#'
#' @description Derives the 4-category highest-education variable (EDUDR04) for
#'   CCHS cycles 2015-2016 onwards, where the pre-2015 derived variable EDUDR04
#'   is no longer provided. It reconstructs the equivalent 4-level
#'   classification from the four raw education-module questions introduced in
#'   2015-2016 (EHG2_01 through EHG2_04).
#'
#' @param EHG2_01 Highest grade of elementary or high school. Codes:
#'   1 = Grade 8 or lower; 2 = Grade 9-10; 3 = Grade 11-13;
#'   6 = not applicable / valid skip (NA(a));
#'   7/8/9 = don't know / refusal / not stated (NA(b)).
#' @param EHG2_02 Completed a high school diploma or equivalent. Codes:
#'   1 = Yes; 2 = No;
#'   6 = not applicable / valid skip (NA(a));
#'   7/8/9 = don't know / refusal / not stated (NA(b)).
#' @param EHG2_03 Other education - certificate/diploma/degree. Codes:
#'   1 = Yes; 2 = No;
#'   6 = not applicable / valid skip (NA(a));
#'   7/8/9 = don't know / refusal / not stated (NA(b)).
#' @param EHG2_04 Highest certificate, diploma or degree completed. Codes:
#'   \itemize{
#'     \item 1 = Less than high school diploma or its equivalent
#'     \item 2 = High school diploma or equivalency certificate
#'     \item 3 = Trade certificate or diploma
#'     \item 4 = College/CEGEP/other non-university certificate or diploma
#'     \item 5 = University certificate or diploma below bachelor's level
#'     \item 6 = Bachelor's degree
#'     \item 7 = University certificate/diploma/degree above bachelor's level
#'     \item 96 = not applicable / valid skip (NA(a))
#'     \item 97/98/99 = don't know / refusal / not stated (NA(b))
#'   }
#'
#' @return a categorical variable (EDUDR04) with 4 levels, or a tagged NA:
#'   \enumerate{
#'     \item Less than secondary school graduation
#'     \item Secondary school graduation, no post-secondary
#'     \item Some post-secondary education
#'     \item Post-secondary graduation
#'   }
#'   Returns \code{tagged_na("a")} when the education module was not applicable
#'   and \code{tagged_na("b")} when a required input is missing or no rule
#'   applies.
#'
#' @examples
#' # Post-secondary graduate with bachelor's degree
#' EDUDR04_fun(3, 1, 1, 6)
#'
#' # Less than high school
#' EDUDR04_fun(1, 2, 2, 96)
#'
#' # High school graduate, no post-secondary
#' EDUDR04_fun(3, 1, 2, 96)
#'
#' @export
EDUDR04_fun <- function(EHG2_01, EHG2_02, EHG2_03, EHG2_04) {
  # NA(a): valid skip — only when ALL inputs are valid skip,
  # meaning the entire education module was not asked.
  is_valid_skip <- (EHG2_01 == 6 | EHG2_01 == "NA(a)") &
    (EHG2_02 == 6 | EHG2_02 == "NA(a)") &
    (EHG2_03 == 6 | EHG2_03 == "NA(a)") &
    (EHG2_04 == 96 | EHG2_04 == "NA(a)")

  # NA(b): DK/RF/NS on any required input
  is_missing <- (EHG2_01 %in% c(7, 8, 9) & EHG2_02 == 2) |
    EHG2_02 %in% c(7, 8, 9) |
    EHG2_03 %in% c(7, 8, 9) |
    EHG2_04 %in% c(97, 98, 99) |
    EHG2_01 == "NA(b)" | EHG2_02 == "NA(b)" |
    EHG2_03 == "NA(b)" | EHG2_04 == "NA(b)"

  # Cat 4: Post-secondary graduation (EHG2_04 between 3 and 7)
  is_cat4 <- EHG2_04 %in% c(3, 4, 5, 6, 7)

  # Cat 3: Some post-secondary (EHG2_03 == 1 but highest credential is HS or less)
  is_cat3 <- EHG2_03 == 1 & EHG2_04 %in% c(1, 2)

  # Cat 2: Secondary graduation, no post-secondary (EHG2_02 == 1 & EHG2_03 == 2)
  is_cat2 <- EHG2_02 == 1 & EHG2_03 == 2

  # Cat 1: Less than secondary ((EHG2_01 in 1,2 or EHG2_02 == 2) & EHG2_03 == 2)
  is_cat1 <- (EHG2_01 %in% c(1, 2) | EHG2_02 == 2) & EHG2_03 == 2

  if_else2(
    is_valid_skip, "NA(a)",
    if_else2(
      is_missing, "NA(b)",
      if_else2(
        is_cat4, 4,
        if_else2(
          is_cat3, 3,
          if_else2(
            is_cat2, 2,
            if_else2(
              is_cat1, 1,
              "NA(b)"
            )
          )
        )
      )
    )
  )
}
