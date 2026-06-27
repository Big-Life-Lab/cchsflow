# ==============================================================================
# Alcohol consumption derived variables
# ==============================================================================
#
# Canonical v3 3-step architecture:
#   Step 1 - clean_variables() with output_format = "tagged_na" (metadata-driven
#            missing-code conversion and out-of-range validation)
#   Step 2 - dplyr::case_when() with any_missing() as the first arm
#   Step 3 - clean_variables() on the derived variable with the user's format
#
# Worksheet derived variables: binge_drinker, ALWDVSTR_der, ALWDVLTR_der
# Input validation bounds come from variable_details.csv via clean_variables().
# The constants below are epidemiological thresholds (domain logic), not
# data-cleaning bounds.

# Binge drinking thresholds by sex (drinks on a single day)
BINGE_THRESHOLDS <- list(
  male = 5,
  female = 4
)

# Canada's Low-Risk Alcohol Drinking Guidelines thresholds.
# Short-term (acute) risk exceeds the low_risk values strictly (>);
# long-term (chronic) risk reaches the high_risk values inclusively (>=).
DRINKING_LIMITS <- list(
  weekly = list(
    male = list(low_risk = 15, high_risk = 20),
    female = list(low_risk = 10, high_risk = 15)
  ),
  daily = list(
    male = list(low_risk = 3, high_risk = 4),
    female = list(low_risk = 2, high_risk = 3)
  )
)

# ==============================================================================
# Internal helpers
# ==============================================================================

#' Drinking risk assessment shared by short- and long-term functions
#'
#' Steps 2-3 of the 3-step pattern for the two risk functions, which differ
#' only in thresholds (low_risk vs high_risk) and comparison strictness.
#'
#' @param cleaned Named list of cleaned inputs from Step 1
#' @param risk_type "short" (acute; strict > on low_risk thresholds) or
#'   "long" (chronic; inclusive >= on high_risk thresholds)
#' @param derived_var Worksheet name of the derived variable (Step 3 target)
#' @param output_format Output missing data format
#' @return Risk indicator vector in the requested format
#' @noRd
assess_drinking_risk <- function(cleaned, risk_type, derived_var,
                                 output_format) {
  max_daily <- pmax(
    cleaned$ALW_2A1, cleaned$ALW_2A2, cleaned$ALW_2A3, cleaned$ALW_2A4,
    cleaned$ALW_2A5, cleaned$ALW_2A6, cleaned$ALW_2A7
  )

  if (risk_type == "short") {
    exceeds_male <- max_daily > DRINKING_LIMITS$daily$male$low_risk |
      cleaned$ALWDWKY > DRINKING_LIMITS$weekly$male$low_risk
    exceeds_female <- max_daily > DRINKING_LIMITS$daily$female$low_risk |
      cleaned$ALWDWKY > DRINKING_LIMITS$weekly$female$low_risk
  } else {
    exceeds_male <- max_daily >= DRINKING_LIMITS$daily$male$high_risk |
      cleaned$ALWDWKY >= DRINKING_LIMITS$weekly$male$high_risk
    exceeds_female <- max_daily >= DRINKING_LIMITS$daily$female$high_risk |
      cleaned$ALWDWKY >= DRINKING_LIMITS$weekly$female$high_risk
  }

  # Step 2: domain logic
  result <- dplyr::case_when(
    any_missing(
      cleaned$DHH_SEX, cleaned$ALWDWKY, cleaned$ALC_1, cleaned$ALW_1,
      cleaned$ALW_2A1, cleaned$ALW_2A2, cleaned$ALW_2A3, cleaned$ALW_2A4,
      cleaned$ALW_2A5, cleaned$ALW_2A6, cleaned$ALW_2A7
    ) ~
      get_priority_missing(
        cleaned$DHH_SEX, cleaned$ALWDWKY, cleaned$ALC_1, cleaned$ALW_1,
        cleaned$ALW_2A1, cleaned$ALW_2A2, cleaned$ALW_2A3, cleaned$ALW_2A4,
        cleaned$ALW_2A5, cleaned$ALW_2A6, cleaned$ALW_2A7
      ),
    # Non-drinkers (past year or past week) are not applicable
    cleaned$ALC_1 == 2 | cleaned$ALW_1 == 2 ~
      assign_missing("not_applicable", derived_var),
    cleaned$DHH_SEX == 1 & exceeds_male ~ 1,
    cleaned$DHH_SEX == 2 & exceeds_female ~ 1,
    .default = 2
  )

  # Step 3: validate against derived-variable metadata, apply requested format
  out <- clean_variables(
    vars = stats::setNames(list(result), derived_var),
    output_format = output_format
  )
  prep_cat_output(out[[derived_var]])
}

# ==============================================================================
# Public API
# ==============================================================================

#' Binge drinking indicator with sex-specific thresholds
#'
#' @description
#' Identifies binge drinking from daily consumption over the past week:
#' 5 or more drinks on any single day for males, 4 or more for females.
#'
#' @details
#' Only respondents who drank in the past week (\code{ALW_1} = 1) are
#' assessed; non-drinkers receive \code{haven::tagged_na("a")} (not
#' applicable). Missing-data handling follows the v3 3-step architecture:
#' input codes (6-9 single-digit, 996-999 triple-digit) are converted using
#' variable_details.csv metadata, with priority not applicable > not stated.
#' Out-of-range inputs receive the worksheet's else rule
#' (\code{haven::tagged_na("b")}).
#'
#' @param DHH_SEX Sex (1 = male, 2 = female). Accepts raw CCHS codes,
#'   tagged NAs, or labelled strings.
#' @param ALW_1 Had drinks in the past week (1 = yes, 2 = no).
#' @param ALW_2A1 Number of drinks on Sunday.
#' @param ALW_2A2 Number of drinks on Monday.
#' @param ALW_2A3 Number of drinks on Tuesday.
#' @param ALW_2A4 Number of drinks on Wednesday.
#' @param ALW_2A5 Number of drinks on Thursday.
#' @param ALW_2A6 Number of drinks on Friday.
#' @param ALW_2A7 Number of drinks on Saturday.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector: 1 = binge drinker, 2 = non-binge drinker.
#'   Missing data: \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar: male with 6 drinks on Tuesday
#' calculate_binge_drinking(1, 1, 3, 1, 6, 0, 3, 2, 2) # 1 (binge)
#'
#' # Scalar: female, at most 3 drinks on any day
#' calculate_binge_drinking(2, 1, 1, 2, 2, 3, 0, 1, 2) # 2 (non-binge)
#'
#' # Vector: drinker, non-drinker
#' calculate_binge_drinking(
#'   c(1, 1), c(1, 2), c(6, 0), c(0, 0), c(0, 0), c(0, 0),
#'   c(0, 0), c(0, 0), c(0, 0)
#' ) # 1, tagged_na("a")
#'
#' # Dataframe
#' library(dplyr)
#' data.frame(
#'   DHH_SEX = c(1, 2), ALW_1 = c(1, 1), ALW_2A1 = c(5, 2),
#'   ALW_2A2 = c(0, 2), ALW_2A3 = c(0, 2), ALW_2A4 = c(0, 2),
#'   ALW_2A5 = c(0, 2), ALW_2A6 = c(0, 2), ALW_2A7 = c(0, 2)
#' ) %>%
#'   mutate(binge = calculate_binge_drinking(
#'     DHH_SEX, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4,
#'     ALW_2A5, ALW_2A6, ALW_2A7
#'   ))
#'
#' \dontrun{
#' # Standard cchsflow workflow
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c(
#'     "DHH_SEX", "ALW_1", "ALW_2A1", "ALW_2A2", "ALW_2A3",
#'     "ALW_2A4", "ALW_2A5", "ALW_2A6", "ALW_2A7", "binge_drinker"
#'   )
#' )
#' }
#'
#' @seealso \code{\link{calculate_drinking_risk_short}},
#'   \code{\link{calculate_drinking_risk_long}}
#'
#' @references
#' Butt, P., et al. (2011). Alcohol and health in Canada: a summary of
#' evidence and guidelines for low-risk drinking. Canadian Centre on
#' Substance Abuse.
#'
#' @note v3.0.0, last updated: 2026-06-10, status: active. Canonical 3-step
#'   implementation; replaces the abandoned helper-based draft.
#' @export
calculate_binge_drinking <- function(DHH_SEX, ALW_1, ALW_2A1, ALW_2A2,
                                     ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6,
                                     ALW_2A7, output_format = "tagged_na") {
  inputs <- normalize_input_lengths(list(
    DHH_SEX = DHH_SEX, ALW_1 = ALW_1,
    ALW_2A1 = ALW_2A1, ALW_2A2 = ALW_2A2, ALW_2A3 = ALW_2A3,
    ALW_2A4 = ALW_2A4, ALW_2A5 = ALW_2A5, ALW_2A6 = ALW_2A6,
    ALW_2A7 = ALW_2A7
  ))
  if (inputs$n == 0) {
    return(numeric(0))
  }

  # Step 1: metadata-driven cleaning
  cleaned <- clean_variables(vars = inputs$vars, output_format = "tagged_na")

  max_daily <- pmax(
    cleaned$ALW_2A1, cleaned$ALW_2A2, cleaned$ALW_2A3, cleaned$ALW_2A4,
    cleaned$ALW_2A5, cleaned$ALW_2A6, cleaned$ALW_2A7
  )

  # Step 2: domain logic
  result <- dplyr::case_when(
    any_missing(
      cleaned$DHH_SEX, cleaned$ALW_1,
      cleaned$ALW_2A1, cleaned$ALW_2A2, cleaned$ALW_2A3, cleaned$ALW_2A4,
      cleaned$ALW_2A5, cleaned$ALW_2A6, cleaned$ALW_2A7
    ) ~
      get_priority_missing(
        cleaned$DHH_SEX, cleaned$ALW_1,
        cleaned$ALW_2A1, cleaned$ALW_2A2, cleaned$ALW_2A3, cleaned$ALW_2A4,
        cleaned$ALW_2A5, cleaned$ALW_2A6, cleaned$ALW_2A7
      ),
    # No drinks in the past week: not applicable
    cleaned$ALW_1 == 2 ~ assign_missing("not_applicable", "binge_drinker"),
    cleaned$DHH_SEX == 1 & max_daily >= BINGE_THRESHOLDS$male ~ 1,
    cleaned$DHH_SEX == 2 & max_daily >= BINGE_THRESHOLDS$female ~ 1,
    .default = 2
  )

  # Step 3: validate against binge_drinker metadata, apply requested format
  out <- clean_variables(
    vars = list(binge_drinker = result),
    output_format = output_format
  )
  prep_cat_output(out$binge_drinker)
}

#' Short-term (acute) drinking risk under the Low-Risk Guidelines
#'
#' @description
#' Assesses short-term health risk from drinking patterns following Canada's
#' Low-Risk Alcohol Drinking Guidelines: risk is flagged when daily
#' consumption exceeds 3 drinks (males) / 2 drinks (females) on any day, or
#' weekly consumption exceeds 15 drinks (males) / 10 drinks (females).
#'
#' @details
#' Non-drinkers in the past year (\code{ALC_1} = 2) or past week
#' (\code{ALW_1} = 2) receive \code{haven::tagged_na("a")} (not applicable).
#' Thresholds are exceeded strictly (e.g. 4+ drinks on a day for males).
#' Missing-data handling follows the v3 3-step architecture with priority
#' not applicable > not stated; out-of-range inputs receive the worksheet's
#' else rule.
#'
#' @inheritParams calculate_binge_drinking
#' @param ALWDWKY Total drinks in the past week.
#' @param ALC_1 Had drinks in the past year (1 = yes, 2 = no).
#'
#' @return Numeric vector: 1 = increased short-term risk, 2 = no increased
#'   short-term risk. Missing data: \code{haven::tagged_na("a")} (not
#'   applicable), \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar: male exceeding daily and weekly thresholds
#' calculate_drinking_risk_short(1, 20, 1, 1, 0, 0, 5, 0, 0, 0, 0) # 1
#'
#' # Scalar: female within thresholds
#' calculate_drinking_risk_short(2, 8, 1, 1, 2, 1, 2, 1, 1, 1, 0) # 2
#'
#' # Vector: at-risk male, non-drinker
#' calculate_drinking_risk_short(
#'   c(1, 1), c(25, 0), c(1, 2), c(1, 2), c(5, 0), c(5, 0),
#'   c(5, 0), c(5, 0), c(5, 0), c(0, 0), c(0, 0)
#' ) # 1, tagged_na("a")
#'
#' \dontrun{
#' # Standard cchsflow workflow
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c(
#'     "DHH_SEX", "ALWDWKY", "ALC_1", "ALW_1", "ALW_2A1", "ALW_2A2",
#'     "ALW_2A3", "ALW_2A4", "ALW_2A5", "ALW_2A6", "ALW_2A7", "ALWDVSTR_der"
#'   )
#' )
#' }
#'
#' @seealso \code{\link{calculate_binge_drinking}},
#'   \code{\link{calculate_drinking_risk_long}}
#'
#' @references
#' Butt, P., et al. (2011). Alcohol and health in Canada: a summary of
#' evidence and guidelines for low-risk drinking. Canadian Centre on
#' Substance Abuse.
#'
#' @note v3.0.0, last updated: 2026-06-10, status: active. Canonical 3-step
#'   implementation; replaces the abandoned helper-based draft.
#' @export
calculate_drinking_risk_short <- function(DHH_SEX, ALWDWKY, ALC_1, ALW_1,
                                          ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4,
                                          ALW_2A5, ALW_2A6, ALW_2A7,
                                          output_format = "tagged_na") {
  inputs <- normalize_input_lengths(list(
    DHH_SEX = DHH_SEX, ALWDWKY = ALWDWKY, ALC_1 = ALC_1, ALW_1 = ALW_1,
    ALW_2A1 = ALW_2A1, ALW_2A2 = ALW_2A2, ALW_2A3 = ALW_2A3,
    ALW_2A4 = ALW_2A4, ALW_2A5 = ALW_2A5, ALW_2A6 = ALW_2A6,
    ALW_2A7 = ALW_2A7
  ))
  if (inputs$n == 0) {
    return(numeric(0))
  }

  # Step 1: metadata-driven cleaning
  cleaned <- clean_variables(vars = inputs$vars, output_format = "tagged_na")

  assess_drinking_risk(cleaned, "short", "ALWDVSTR_der", output_format)
}

#' Long-term (chronic) drinking risk under the Low-Risk Guidelines
#'
#' @description
#' Assesses long-term health risk from drinking patterns following Canada's
#' Low-Risk Alcohol Drinking Guidelines: risk is flagged when daily
#' consumption reaches 4 drinks (males) / 3 drinks (females) on any day, or
#' weekly consumption reaches 20 drinks (males) / 15 drinks (females).
#'
#' @details
#' Non-drinkers in the past year (\code{ALC_1} = 2) or past week
#' (\code{ALW_1} = 2) receive \code{haven::tagged_na("a")} (not applicable).
#' Thresholds are reached inclusively (e.g. exactly 20 drinks/week flags a
#' male respondent). Missing-data handling follows the v3 3-step
#' architecture with priority not applicable > not stated; out-of-range
#' inputs receive the worksheet's else rule.
#'
#' @inheritParams calculate_drinking_risk_short
#'
#' @return Numeric vector: 1 = increased long-term risk, 2 = no increased
#'   long-term risk. Missing data: \code{haven::tagged_na("a")} (not
#'   applicable), \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar: male reaching the weekly threshold
#' calculate_drinking_risk_long(1, 20, 1, 1, 3, 3, 3, 3, 3, 3, 2) # 1
#'
#' # Scalar: female within thresholds
#' calculate_drinking_risk_long(2, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1) # 2
#'
#' # Vector: at-risk male, non-drinker
#' calculate_drinking_risk_long(
#'   c(1, 1), c(20, 0), c(1, 2), c(1, 2), c(3, 0), c(3, 0),
#'   c(3, 0), c(3, 0), c(3, 0), c(3, 0), c(2, 0)
#' ) # 1, tagged_na("a")
#'
#' \dontrun{
#' # Standard cchsflow workflow
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c(
#'     "DHH_SEX", "ALWDWKY", "ALC_1", "ALW_1", "ALW_2A1", "ALW_2A2",
#'     "ALW_2A3", "ALW_2A4", "ALW_2A5", "ALW_2A6", "ALW_2A7", "ALWDVLTR_der"
#'   )
#' )
#' }
#'
#' @seealso \code{\link{calculate_binge_drinking}},
#'   \code{\link{calculate_drinking_risk_short}}
#'
#' @references
#' Butt, P., et al. (2011). Alcohol and health in Canada: a summary of
#' evidence and guidelines for low-risk drinking. Canadian Centre on
#' Substance Abuse.
#'
#' @note v3.0.0, last updated: 2026-06-10, status: active. Canonical 3-step
#'   implementation; replaces the abandoned helper-based draft.
#' @export
calculate_drinking_risk_long <- function(DHH_SEX, ALWDWKY, ALC_1, ALW_1,
                                         ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4,
                                         ALW_2A5, ALW_2A6, ALW_2A7,
                                         output_format = "tagged_na") {
  inputs <- normalize_input_lengths(list(
    DHH_SEX = DHH_SEX, ALWDWKY = ALWDWKY, ALC_1 = ALC_1, ALW_1 = ALW_1,
    ALW_2A1 = ALW_2A1, ALW_2A2 = ALW_2A2, ALW_2A3 = ALW_2A3,
    ALW_2A4 = ALW_2A4, ALW_2A5 = ALW_2A5, ALW_2A6 = ALW_2A6,
    ALW_2A7 = ALW_2A7
  ))
  if (inputs$n == 0) {
    return(numeric(0))
  }

  # Step 1: metadata-driven cleaning
  cleaned <- clean_variables(vars = inputs$vars, output_format = "tagged_na")

  assess_drinking_risk(cleaned, "long", "ALWDVLTR_der", output_format)
}
