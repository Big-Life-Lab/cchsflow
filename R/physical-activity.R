#' @title Calculate daily energy expenditure from leisure activity
#'
#' @description
#' Calculates daily leisure energy expenditure in kcal/kg/day using
#' age-specific activity variables and MET-weighted intensity scoring.
#'
#' @details
#' In CCHS 2001-2014, PACDEE measures daily leisure energy expenditure
#' for all ages. In CCHS 2015-2018, ages 12-17 use PAY_XXX variables
#' and ages 18+ use PAA_XXX variables; this function combines both
#' age groups. The formula is:
#' EE = ((N * D * MET_value) / 60) / 7, where N = frequency over 7
#' days, D = duration in minutes, and MET_value = 3 for leisure or 6
#' for vigorous activity.
#'
#' Missing-data handling follows the v3 3-step architecture: input codes
#' are converted using variable_details.csv metadata, with priority
#' not applicable > not stated.
#'
#' @param age Continuous age variable.
#' @param PAA_045 Hours of vigorous sports/fitness activity (18+).
#' @param PAA_050 Minutes of vigorous sports/fitness activity (18+).
#' @param PAA_075 Hours of other physical activity (18+).
#' @param PAA_080 Minutes of other physical activity (18+).
#' @param PAADVDYS Number of active days over 7 days (18+).
#' @param PAADVVIG Minutes of vigorous activity over 7 days (18+).
#' @param PAYDVTOA Total minutes of other activities over 7 days (12-17).
#' @param PAYDVADL Total minutes of leisure physical activity over 7
#'   days (12-17).
#' @param PAYDVVIG Total minutes of vigorous physical activity over 7
#'   days (12-17).
#' @param PAYDVDYS Total days physically active over 7 days (12-17).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of daily energy expenditure (kcal/kg/day).
#'   Missing data: \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar (adult, 25 years old)
#' calculate_energy_exp(25, 1, 30, 0, 45, 3, 60, 0, 0, 0, 0)
#'
#' @seealso \code{\link{categorize_energy_exp}}
#'
#' @export

calculate_energy_exp <-
  function(age, PAA_045, PAA_050, PAA_075, PAA_080, PAADVDYS,
           PAADVVIG, PAYDVTOA, PAYDVADL, PAYDVVIG, PAYDVDYS,
           output_format = "tagged_na") {
    # === STEP 1: DATA CLEANING ===
    cleaned <- clean_variables(vars = list(
      DHHGAGE_cont = age,
      PAA_045 = PAA_045, PAA_050 = PAA_050,
      PAA_075 = PAA_075, PAA_080 = PAA_080,
      PAADVDYS = PAADVDYS, PAADVVIG = PAADVVIG,
      PAYDVTOA = PAYDVTOA, PAYDVADL = PAYDVADL,
      PAYDVVIG = PAYDVVIG, PAYDVDYS = PAYDVDYS
    ), output_format = "tagged_na")

    age_c <- cleaned$DHHGAGE_cont

    # === STEP 2: CORE CALCULATION ===
    # Adult leisure activity (age >= 18)
    leisure_adult <- dplyr::case_when(
      age_c >= 18 & !any_missing(cleaned$PAA_045, cleaned$PAA_050,
                                  cleaned$PAA_075, cleaned$PAA_080) ~
        cleaned$PAA_045 * 60 + cleaned$PAA_050 +
        cleaned$PAA_075 * 60 + cleaned$PAA_080,
      any_missing(cleaned$PAA_045, cleaned$PAA_050,
                  cleaned$PAA_075, cleaned$PAA_080) ~
        get_priority_missing(cleaned$PAA_045, cleaned$PAA_050,
                             cleaned$PAA_075, cleaned$PAA_080,
                             output_format = output_format),
      .default = assign_missing("not_applicable", "energy_exp", output_format)
    )

    # Youth leisure activity (age < 18)
    leisure_youth <- dplyr::case_when(
      age_c < 18 & !any_missing(cleaned$PAYDVTOA, cleaned$PAYDVADL) ~
        cleaned$PAYDVTOA + cleaned$PAYDVADL,
      any_missing(cleaned$PAYDVTOA, cleaned$PAYDVADL) ~
        get_priority_missing(cleaned$PAYDVTOA, cleaned$PAYDVADL,
                             output_format = output_format),
      .default = assign_missing("not_applicable", "energy_exp", output_format)
    )

    # Energy expenditure: youth path first, then adult
    result <- dplyr::case_when(
      # Youth path
      !any_missing(cleaned$PAYDVVIG, leisure_youth, cleaned$PAYDVDYS) ~
        ((leisure_youth - cleaned$PAYDVVIG) * 3 +
         cleaned$PAYDVVIG * 6) / 7 * cleaned$PAYDVDYS / 60,
      # Adult path
      !any_missing(cleaned$PAADVDYS, cleaned$PAADVVIG, leisure_adult) ~
        ((leisure_adult - cleaned$PAADVVIG) * 3 +
         cleaned$PAADVVIG * 6) / 7 * cleaned$PAADVDYS / 60,
      # Missing propagation
      any_missing(leisure_youth, cleaned$PAYDVDYS, cleaned$PAYDVVIG,
                  leisure_adult, cleaned$PAADVDYS, cleaned$PAADVVIG) ~
        get_priority_missing(leisure_youth, leisure_adult,
                             output_format = output_format),
      .default = assign_missing("not_stated", "energy_exp", output_format)
    )

    # === STEP 3: OUTPUT VALIDATION ===
    output_cleaned <- clean_variables(vars = list(
      energy_exp = result
    ), output_format = output_format)

    return(output_cleaned$energy_exp)
  }

#' @title Categorize energy expenditure into activity levels
#'
#' @description
#' Categorizes continuous energy expenditure into 3 physical activity
#' levels using CCHS cutpoints: inactive, moderately active, and active.
#'
#' @details
#' The cutpoints follow the standard CCHS physical activity
#' classification: < 1.5 kcal/kg/day = inactive, 1.5 to < 3.0 =
#' moderately active, >= 3.0 = active. Missing-data handling follows
#' the v3 3-step architecture with priority not applicable > not stated.
#'
#' @param energy_exp Continuous energy expenditure in kcal/kg/day. See
#'   \code{\link{calculate_energy_exp}}.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector: 1 = inactive (< 1.5), 2 = moderately active
#'   (1.5 to < 3.0), 3 = active (>= 3.0). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' categorize_energy_exp(0.5)  # 1 (inactive)
#' categorize_energy_exp(2.0)  # 2 (moderately active)
#' categorize_energy_exp(4.0)  # 3 (active)
#'
#' @seealso \code{\link{calculate_energy_exp}}
#'
#' @export
categorize_energy_exp <- function(energy_exp, output_format = "tagged_na") {
  # === STEP 1: DATA CLEANING ===
  cleaned <- clean_variables(vars = list(
    energy_exp = energy_exp
  ), output_format = "tagged_na")

  ee <- cleaned$energy_exp

  # === STEP 2: CATEGORIZATION ===
  result <- dplyr::case_when(
    any_missing(ee) ~
      get_priority_missing(ee, output_format = output_format),
    ee >= 0 & ee < 1.5 ~ 1,
    ee >= 1.5 & ee < 3 ~ 2,
    ee >= 3 ~ 3,
    .default = assign_missing("not_stated", "energy_exp_cat3", output_format)
  )

  # === STEP 3: OUTPUT VALIDATION ===
  output_cleaned <- clean_variables(vars = list(
    energy_exp_cat3 = result
  ), output_format = output_format)

  return(prep_cat_output(output_cleaned$energy_exp_cat3))
}
