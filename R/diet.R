#' @title Calculate diet quality score
#'
#' @description
#' Calculates a diet quality score (0-10) based on daily consumption of
#' fruit, salad, potatoes, carrots, other vegetables, and juice, with
#' sex-specific penalties for excess potato and juice intake.
#'
#' @details
#' The scoring algorithm starts from a 2-point baseline and adds or
#' subtracts based on dietary patterns:
#' +1 point per daily fruit/vegetable serving (excluding juice, capped
#' at 8); -2 points if daily juice consumption exceeds 1 serving;
#' -2 points if daily potato consumption exceeds the sex-specific limit
#' (1 serving for males, 5/7 for females); -2 points if zero carrot
#' consumption. Negative totals are floored to 0.
#'
#' Missing-data handling follows the v3 3-step architecture: input codes
#' are converted using variable_details.csv metadata, with priority
#' not applicable > not stated.
#'
#' @param FVCDFRU Daily fruit consumption (servings/day).
#' @param FVCDSAL Daily salad consumption (servings/day).
#' @param FVCDPOT Daily potato consumption (servings/day).
#' @param FVCDCAR Daily carrot consumption (servings/day).
#' @param FVCDVEG Daily other vegetable consumption (servings/day).
#' @param FVCDJUI Daily fruit juice consumption (servings/day).
#' @param DHH_SEX Sex (1 = male, 2 = female).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric diet score between 0 and 10. Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' calculate_diet_score(2, 1, 0.5, 0.5, 1, 0.5, 1)
#'
#' @seealso \code{\link{categorize_diet_score}}
#'
#' @export
calculate_diet_score <-
  function(FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG, FVCDJUI, DHH_SEX,
           output_format = "tagged_na") {
    # === STEP 1: DATA CLEANING ===
    cleaned <- clean_variables(vars = list(
      FVCDFRU = FVCDFRU, FVCDSAL = FVCDSAL, FVCDPOT = FVCDPOT,
      FVCDCAR = FVCDCAR, FVCDVEG = FVCDVEG, FVCDJUI = FVCDJUI,
      DHH_SEX = DHH_SEX
    ), output_format = "tagged_na")

    fru <- cleaned$FVCDFRU
    sal <- cleaned$FVCDSAL
    pot <- cleaned$FVCDPOT
    car <- cleaned$FVCDCAR
    veg <- cleaned$FVCDVEG
    jui <- cleaned$FVCDJUI
    sex <- cleaned$DHH_SEX

    # === STEP 2: CORE CALCULATION ===
    # Check for missing inputs
    all_missing <- any_missing(fru, sal, pot, car, veg, jui)

    # Total fruit and vegetables, excluding fruit juice (capped at 8)
    total_fruitveg <- dplyr::case_when(
      all_missing ~ NA_real_,
      TRUE ~ pmin(fru + sal + pot + car + veg, 8)
    )

    # High potato intake flag (sex-specific limit)
    daily_pot_limit <- dplyr::case_when(
      sex == 1 ~ 1,
      sex == 2 ~ 5 / 7,
      TRUE ~ NA_real_
    )
    FVCDPOT_high <- dplyr::case_when(
      is.na(pot) ~ NA_real_,
      pot >= daily_pot_limit ~ 1,
      TRUE ~ 0
    )

    # No carrot intake flag
    FVCDCAR_nil <- dplyr::case_when(
      is.na(car) ~ NA_real_,
      car == 0 ~ 1,
      TRUE ~ 0
    )

    # High juice intake penalty
    FVCDJUI_high <- dplyr::case_when(
      is.na(jui) ~ NA_real_,
      jui <= 1 ~ 0,
      TRUE ~ jui - 1
    )

    # Raw score
    diet_raw <- dplyr::case_when(
      is.na(total_fruitveg) | is.na(FVCDPOT_high) |
        is.na(FVCDCAR_nil) | is.na(FVCDJUI_high) ~ NA_real_,
      TRUE ~ 2 + total_fruitveg - (2 * FVCDPOT_high) -
        (2 * FVCDCAR_nil) - (2 * FVCDJUI_high)
    )

    # Clamp to [0, 10]
    result <- dplyr::case_when(
      all_missing ~
        get_priority_missing(fru, sal, pot, car, veg, jui,
                             output_format = output_format),
      is.na(diet_raw) ~
        assign_missing("not_stated", "diet_score", output_format),
      diet_raw < 0 ~ 0,
      diet_raw > 10 ~ 10,
      TRUE ~ diet_raw
    )

    # === STEP 3: OUTPUT VALIDATION ===
    output_cleaned <- clean_variables(vars = list(
      diet_score = result
    ), output_format = output_format)

    return(output_cleaned$diet_score)
  }


#' @title Categorize diet quality score
#'
#' @description
#' Categorizes the derived diet score into 3 levels: poor (0 to < 2),
#' fair (2 to < 8), and adequate (8 to 10).
#'
#' @details
#' The cutpoints follow the scoring scheme from
#' \code{\link{calculate_diet_score}}: scores below 2 indicate poor diet
#' quality, 2-7 indicate fair quality, and 8-10 indicate adequate
#' consumption of fruits and vegetables. Missing-data handling follows
#' the v3 3-step architecture with priority not applicable > not stated.
#'
#' @param diet_score Derived diet score (0-10). See
#'   \code{\link{calculate_diet_score}}.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector: 1 = poor (< 2), 2 = fair (2 to < 8),
#'   3 = adequate (8 to 10). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' categorize_diet_score(1)   # 1 (poor)
#' categorize_diet_score(5)   # 2 (fair)
#' categorize_diet_score(9)   # 3 (adequate)
#'
#' @seealso \code{\link{calculate_diet_score}}
#'
#' @export
categorize_diet_score <- function(diet_score, output_format = "tagged_na") {
  # === STEP 1: DATA CLEANING ===
  cleaned <- clean_variables(vars = list(
    diet_score = diet_score
  ), output_format = "tagged_na")

  ds <- cleaned$diet_score

  # === STEP 2: CATEGORIZATION ===
  result <- dplyr::case_when(
    any_missing(ds) ~
      get_priority_missing(ds, output_format = output_format),
    ds >= 0 & ds < 2 ~ 1,
    ds >= 2 & ds < 8 ~ 2,
    ds >= 8 & ds <= 10 ~ 3,
    .default = assign_missing("not_stated", "diet_score_cat3", output_format)
  )

  # === STEP 3: OUTPUT VALIDATION ===
  output_cleaned <- clean_variables(vars = list(
    diet_score_cat3 = result
  ), output_format = output_format)

  return(prep_cat_output(output_cleaned$diet_score_cat3))
}
