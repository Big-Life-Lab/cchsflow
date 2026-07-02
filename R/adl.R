# ==============================================================================
# ADL (Activities of Daily Living) derived variables
# ==============================================================================
#
# Canonical v3 3-step architecture:
#   Step 1 - clean_variables() with output_format = "tagged_na" (metadata-driven
#            missing-code conversion and out-of-range validation)
#   Step 2 - dplyr::case_when() with any_missing() as the first arm
#   Step 3 - clean_variables() on the derived variable with the user's format
#
# Worksheet derived variables: ADL_der, ADL_score_5, ADL_score_6
# Validation bounds come from variable_details.csv via clean_variables();
# there are no hardcoded missing codes or bounds in this file.

#' Activities of Daily Living (ADL) help indicator
#'
#' @description
#' Creates a binary indicator for needing help with activities of daily
#' living, using the 5 ADL tasks asked consistently across CCHS cycles.
#'
#' @details
#' The five core tasks (meal preparation, getting to appointments/errands,
#' housework, personal care, moving inside the house) are available in all
#' CCHS cycles from 2001 onward, making this indicator suitable for
#' cross-cycle analyses. The original CCHS derived variable ADLF6R uses
#' different components across cycles; this function deliberately uses the
#' consistent 5-item subset.
#'
#' ADL questions are typically restricted to respondents with activity
#' limitations; not-applicable responses are preserved as
#' \code{haven::tagged_na("a")}.
#'
#' Missing-data handling follows the v3 3-step architecture: input codes
#' (6, 7, 8, 9) are converted using variable_details.csv metadata, with
#' priority not applicable > not stated when inputs mix missing types.
#' Out-of-range inputs receive the worksheet's else rule
#' (\code{haven::tagged_na("b")}).
#'
#' @param ADL_01 Help needed preparing meals (1 = yes, 2 = no). Accepts raw
#'   CCHS codes, tagged NAs, or labelled strings.
#' @param ADL_02 Help needed getting to appointments/errands (1 = yes, 2 = no).
#' @param ADL_03 Help needed doing housework (1 = yes, 2 = no).
#' @param ADL_04 Help needed doing personal care (1 = yes, 2 = no).
#' @param ADL_05 Help needed moving inside the house (1 = yes, 2 = no).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector: 1 = needs help with at least one task,
#'   2 = no help needed with any task. Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' assess_adl(1, 2, 2, 2, 2) # 1 (needs help with meals)
#' assess_adl(2, 2, 2, 2, 2) # 2 (no help needed)
#'
#' # Vector with CCHS missing codes
#' assess_adl(
#'   c(1, 2, 6, 7),
#'   c(2, 2, 2, 2), c(2, 2, 2, 2), c(2, 2, 2, 2), c(2, 2, 2, 2)
#' ) # 1, 2, tagged_na("a"), tagged_na("b")
#'
#' # Dataframe
#' library(dplyr)
#' data.frame(
#'   ADL_01 = c(1, 2), ADL_02 = c(2, 2), ADL_03 = c(2, 2),
#'   ADL_04 = c(2, 2), ADL_05 = c(2, 2)
#' ) %>%
#'   mutate(adl_help = assess_adl(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05))
#'
#' \dontrun{
#' # Standard cchsflow workflow
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_der")
#' )
#' }
#'
#' @seealso \code{\link{score_adl}}, \code{\link{score_adl_6}}
#'
#' @export
assess_adl <- function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05,
                       output_format = "tagged_na") {
  inputs <- normalize_input_lengths(list(
    ADL_01 = ADL_01, ADL_02 = ADL_02, ADL_03 = ADL_03,
    ADL_04 = ADL_04, ADL_05 = ADL_05
  ))
  if (inputs$n == 0) {
    return(numeric(0))
  }

  # Step 1: metadata-driven cleaning (always tagged_na so Step 2 detects missing)
  cleaned <- clean_variables(vars = inputs$vars, output_format = "tagged_na")

  # Step 2: domain logic
  result <- dplyr::case_when(
    any_missing(
      cleaned$ADL_01, cleaned$ADL_02, cleaned$ADL_03,
      cleaned$ADL_04, cleaned$ADL_05
    ) ~
      get_priority_missing(
        cleaned$ADL_01, cleaned$ADL_02, cleaned$ADL_03,
        cleaned$ADL_04, cleaned$ADL_05
      ),
    cleaned$ADL_01 == 1 | cleaned$ADL_02 == 1 | cleaned$ADL_03 == 1 |
      cleaned$ADL_04 == 1 | cleaned$ADL_05 == 1 ~ 1,
    .default = 2
  )

  # Step 3: validate against ADL_der metadata and apply the requested format
  out <- clean_variables(
    vars = list(ADL_der = result),
    output_format = output_format
  )
  prep_cat_output(out$ADL_der)
}

#' Activities of Daily Living (ADL) 5-item help score
#'
#' @description
#' Counts the number of ADL tasks (0-5) for which the respondent needs help,
#' using the 5 tasks asked consistently across CCHS cycles.
#'
#' @details
#' Provides a graduated disability measure: 0 (independent) to
#' 5 (needs help with all tasks). Missing data in any item invalidates the
#' score, with priority not applicable > not stated. See
#' \code{\link{assess_adl}} for the task list and cross-cycle notes.
#'
#' @inheritParams assess_adl
#'
#' @return Numeric count 0-5. Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' score_adl(2, 2, 2, 2, 2) # 0 (independent)
#' score_adl(1, 2, 1, 2, 2) # 2 (help with two tasks)
#'
#' # Vector
#' score_adl(c(1, 2), c(1, 2), c(2, 2), c(1, 2), c(2, 2)) # 3, 0
#'
#' # Dataframe
#' library(dplyr)
#' data.frame(
#'   ADL_01 = c(1, 2), ADL_02 = c(1, 2), ADL_03 = c(2, 2),
#'   ADL_04 = c(1, 2), ADL_05 = c(2, 2)
#' ) %>%
#'   mutate(adl_score = score_adl(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05))
#'
#' \dontrun{
#' # Standard cchsflow workflow
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_score_5")
#' )
#' }
#'
#' @seealso \code{\link{assess_adl}}, \code{\link{score_adl_6}}
#'
#' @export
score_adl <- function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05,
                      output_format = "tagged_na") {
  inputs <- normalize_input_lengths(list(
    ADL_01 = ADL_01, ADL_02 = ADL_02, ADL_03 = ADL_03,
    ADL_04 = ADL_04, ADL_05 = ADL_05
  ))
  if (inputs$n == 0) {
    return(numeric(0))
  }

  # Step 1: metadata-driven cleaning
  cleaned <- clean_variables(vars = inputs$vars, output_format = "tagged_na")

  # Step 2: domain logic — count of tasks needing help
  result <- dplyr::case_when(
    any_missing(
      cleaned$ADL_01, cleaned$ADL_02, cleaned$ADL_03,
      cleaned$ADL_04, cleaned$ADL_05
    ) ~
      get_priority_missing(
        cleaned$ADL_01, cleaned$ADL_02, cleaned$ADL_03,
        cleaned$ADL_04, cleaned$ADL_05
      ),
    .default = (cleaned$ADL_01 == 1) + (cleaned$ADL_02 == 1) +
      (cleaned$ADL_03 == 1) + (cleaned$ADL_04 == 1) + (cleaned$ADL_05 == 1)
  )

  # Step 3: validate against ADL_score_5 metadata and apply requested format
  out <- clean_variables(
    vars = list(ADL_score_5 = result),
    output_format = output_format
  )
  prep_cat_output(out$ADL_score_5)
}

#' Activities of Daily Living (ADL) 6-item help score
#'
#' @description
#' Counts the number of ADL tasks (0-6) for which the respondent needs help,
#' extending the 5-item score with financial management.
#'
#' @details
#' ADL_06 (help managing finances) is available in CCHS 2003-2014 cycles
#' only; use \code{\link{score_adl}} for 2001 or 2015+ analyses. Missing
#' data handling matches the 5-item version.
#'
#' @inheritParams assess_adl
#' @param ADL_06 Help needed managing finances (1 = yes, 2 = no). Accepts raw
#'   CCHS codes, tagged NAs, or labelled strings.
#'
#' @return Numeric count 0-6. Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' score_adl_6(2, 2, 2, 2, 2, 2) # 0 (independent)
#' score_adl_6(1, 2, 1, 2, 2, 1) # 3 (help with three tasks)
#'
#' # Vector
#' score_adl_6(
#'   c(1, 2), c(1, 2), c(2, 2), c(1, 2), c(2, 2), c(1, 2)
#' ) # 4, 0
#'
#' \dontrun{
#' # Standard cchsflow workflow (2003-2014 cycles)
#' result <- rec_with_table(
#'   cchs2007_2008_p,
#'   c(
#'     "ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_06",
#'     "ADL_score_6"
#'   )
#' )
#' }
#'
#' @seealso \code{\link{assess_adl}}, \code{\link{score_adl}}
#'
#' @export
score_adl_6 <- function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05, ADL_06,
                        output_format = "tagged_na") {
  inputs <- normalize_input_lengths(list(
    ADL_01 = ADL_01, ADL_02 = ADL_02, ADL_03 = ADL_03,
    ADL_04 = ADL_04, ADL_05 = ADL_05, ADL_06 = ADL_06
  ))
  if (inputs$n == 0) {
    return(numeric(0))
  }

  # Step 1: metadata-driven cleaning
  cleaned <- clean_variables(vars = inputs$vars, output_format = "tagged_na")

  # Step 2: domain logic — count of tasks needing help
  result <- dplyr::case_when(
    any_missing(
      cleaned$ADL_01, cleaned$ADL_02, cleaned$ADL_03,
      cleaned$ADL_04, cleaned$ADL_05, cleaned$ADL_06
    ) ~
      get_priority_missing(
        cleaned$ADL_01, cleaned$ADL_02, cleaned$ADL_03,
        cleaned$ADL_04, cleaned$ADL_05, cleaned$ADL_06
      ),
    .default = (cleaned$ADL_01 == 1) + (cleaned$ADL_02 == 1) +
      (cleaned$ADL_03 == 1) + (cleaned$ADL_04 == 1) +
      (cleaned$ADL_05 == 1) + (cleaned$ADL_06 == 1)
  )

  # Step 3: validate against ADL_score_6 metadata and apply requested format
  out <- clean_variables(
    vars = list(ADL_score_6 = result),
    output_format = output_format
  )
  prep_cat_output(out$ADL_score_6)
}
