# ===============================================================================
# Pack-Years Calculation Functions
# ===============================================================================
#
# This file implements pack-years calculation using the modular 3-step architecture:
#
# - calculate_pack_years() - Primary interface using unified derived feeders
#   (age_start_smoking, cigs_per_day, time_quit_smoking)
#
# The function works with both PUMF and Master data. The difference is in the
# precision of input variables, not the calculation itself:
# - PUMF: Midpoint-derived ages, capped CPD (~15-20% relative error)
# - Master: True continuous values (higher precision)
#
# ==============================================================================

# ------------------------------------------------------------------------------
# PACK-YEARS CONSTANTS
# ------------------------------------------------------------------------------
# Calculation constants are defined in R/smoking-validation-constants.R
# Output validation bounds [0, 165] are in variable_details.csv (recEnd field)
# and are applied automatically by clean_variables() in Step 3.
#
# This file sources PACK_YEARS_CONSTANTS from smoking-validation-constants.R
# which contains: min_pack_years, min_pack_years_alt, cigarettes_per_pack,
# days_per_month, max_pack_years
# ------------------------------------------------------------------------------

# ==============================================================================
# MODULAR ARCHITECTURE - PRIMARY INTERFACE
# ==============================================================================

#' @title Calculate cumulative pack-years of smoking exposure
#'
#' @description
#' Derive pack-years from smoking status, age, intensity, and cessation
#' timing. Source-agnostic: the worksheet routes PUMF or Master source
#' variables to the same semantic parameters.
#'
#' @details
#' Formula varies by 6-category smoking status: daily smokers use
#' (age - age_start) * (cigs/20); former daily smokers subtract
#' time_quit; occasional smokers use days-per-month weighting; former
#' occasional smokers receive a minimum constant; never smokers
#' receive 0. PUMF estimates carry roughly 15-20% relative error
#' versus Master due to midpoint imputation and capped intensity.
#'
#' @param smoking_status Numeric. 6-category smoking status
#'   (1 = daily, 2 = occasional former daily, 3 = occasional never
#'   daily, 4 = former daily, 5 = former occasional, 6 = never).
#' @param age Numeric. Current age in years (continuous).
#' @param age_start_smoking Numeric. Age started smoking daily.
#' @param cigs_per_day Numeric. Cigarettes per day when smoking daily.
#' @param time_quit_smoking Numeric. Years since quit smoking.
#' @param cigs_occasional Numeric. Cigarettes per occasion (occasional
#'   smokers). NULL if not available.
#' @param days_per_month Numeric. Days smoked per month (occasional
#'   smokers). NULL if not available.
#' @param age_first_cigarette Numeric. Age of first cigarette
#'   (status 3 only). NULL if not available.
#' @param smoked_100_lifetime Numeric. Smoked 100+ cigarettes
#'   (1 = yes, 2 = no; status 5 only). NULL if not available.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of pack-years (0-165). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar: daily smoker, 25 pack-years
#' calculate_pack_years(
#'   smoking_status = 1, age = 45,
#'   age_start_smoking = 20, cigs_per_day = 20,
#'   time_quit_smoking = NA
#' )
#'
#' # Scalar: never smoker
#' calculate_pack_years(
#'   smoking_status = 6, age = 50,
#'   age_start_smoking = NA, cigs_per_day = NA,
#'   time_quit_smoking = NA
#' )
#'
#' @seealso \code{\link{calculate_age_start_smoking}},
#'   \code{\link{calculate_cigs_per_day}},
#'   \code{\link{calculate_pack_years_categorical}}
#'
#' @export
calculate_pack_years <- function(smoking_status,
                                 age,
                                 age_start_smoking,
                                 cigs_per_day,
                                 time_quit_smoking,
                                 cigs_occasional = NULL,
                                 days_per_month = NULL,
                                 age_first_cigarette = NULL,
                                 smoked_100_lifetime = NULL,
                                 output_format = "tagged_na") {

  # Determine vector length from primary inputs
  n <- length(smoking_status)

  # Handle NULL optional inputs - convert to NA vectors
  optional <- expand_null_inputs(list(
    cigs_occasional = cigs_occasional,
    days_per_month = days_per_month,
    age_first_cigarette = age_first_cigarette,
    smoked_100_lifetime = smoked_100_lifetime
  ), n)
  cigs_occasional <- optional$cigs_occasional
  days_per_month <- optional$days_per_month
  age_first_cigarette <- optional$age_first_cigarette
  smoked_100_lifetime <- optional$smoked_100_lifetime

  # === STEP 1: DATA CLEANING ===
  # List names must match variable_details.csv for pattern lookup.
  # Age uses DHHGAGE_cont for PUMF and DHH_AGE for Master — both have
  # identical missing codes (96=NA::a) so DHHGAGE_cont works for either.
  cleaned_raw <- clean_variables(vars = list(
    SMKDSTY_original = smoking_status,
    DHHGAGE_cont = age,
    age_start_smoking = age_start_smoking,
    cigs_per_day = cigs_per_day,
    time_quit_smoking = time_quit_smoking,
    cigs_occasional = cigs_occasional,
    days_per_month = days_per_month,
    age_first_cigarette = age_first_cigarette,
    smoked_100_lifetime = smoked_100_lifetime
  ), output_format = "tagged_na")

  # Map worksheet names to semantic names for core logic
  cleaned <- cleaned_raw
  cleaned$smoking_status <- cleaned_raw$SMKDSTY_original
  cleaned$age <- cleaned_raw$DHHGAGE_cont

  # === STEP 2: CORE CALCULATION ===
  result <- .calculate_pack_years_core(cleaned, output_format)

  # === STEP 3: OUTPUT VALIDATION ===
  output_cleaned <- clean_variables(vars = list(
    pack_years_der = result
  ), output_format = output_format)

  return(output_cleaned$pack_years_der)
}


# ==============================================================================
# CATEGORICAL PACK-YEARS (5-CATEGORY SCHEME)
# ==============================================================================

#' @title Categorize pack-years into 5 exposure groups
#'
#' @description
#' Convert continuous pack-years into a 5-category ordinal variable for
#' epidemiological stratification.
#'
#' @details
#' Cut-points are defined in PACK_YEARS_CONSTANTS and match the
#' recStart/recEnd ranges in variable_details.csv for pack_years_cat.
#' Categories: 0 = never (0), 1 = light (0-10), 2 = moderate (10-20),
#' 3 = heavy (20-30), 4 = very heavy (30+). Cut-points are pending
#' epidemiological review.
#'
#' @param pack_years_der Numeric. Continuous pack-years from
#'   \code{\link{calculate_pack_years}}.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector (0 = never, 1 = light, 2 = moderate,
#'   3 = heavy, 4 = very heavy). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar inputs
#' calculate_pack_years_categorical(0)
#' calculate_pack_years_categorical(15.0)
#'
#' @seealso \code{\link{calculate_pack_years}}
#'
#' @export
calculate_pack_years_categorical <- function(pack_years_der,
                                              output_format = "tagged_na") {
  if (length(pack_years_der) == 0) return(numeric(0))

  breaks <- PACK_YEARS_CONSTANTS$pack_years_cat_breaks

  # === STEP 1: DATA CLEANING ===
  cleaned <- clean_variables(vars = list(
    pack_years_der = pack_years_der
  ), output_format = "tagged_na")

  py <- cleaned$pack_years_der

  # === STEP 2: CATEGORISATION ===
  result <- dplyr::case_when(
    any_missing(py) ~
      get_priority_missing(py, output_format = output_format),
    py == 0                       ~ 0,
    py > 0  & py < breaks[2]     ~ 1,
    py >= breaks[2] & py < breaks[3] ~ 2,
    py >= breaks[3] & py < breaks[4] ~ 3,
    py >= breaks[4]               ~ 4,
    .default = assign_missing("not_stated", "pack_years_cat", output_format)
  )

  # === STEP 3: OUTPUT VALIDATION ===
  output_cleaned <- clean_variables(vars = list(
    pack_years_cat = result
  ), output_format = output_format)

  return(prep_cat_output(output_cleaned$pack_years_cat))
}


# ==============================================================================
# CORE MATHEMATICAL LOGIC (DATA SOURCE AGNOSTIC)
# ==============================================================================

#' Core Pack-Years Calculation Logic
#'
#' Internal function containing the mathematical formulas for pack-years
#' calculation. Source-agnostic — works with cleaned semantic variables.
#'
#' @param cleaned_vars List of cleaned variables from clean_variables().
#' @param output_format Character. Output format for missing values
#' @return Numeric vector with pack-years values
#' @noRd
.calculate_pack_years_core <- function(cleaned_vars, output_format = "tagged_na") {

  # Extract semantic variables
  status <- cleaned_vars$smoking_status
  age <- cleaned_vars$age
  age_started <- cleaned_vars$age_start_smoking
  cpd <- cleaned_vars$cigs_per_day
  time_quit <- cleaned_vars$time_quit_smoking
  cigs_occ <- cleaned_vars$cigs_occasional
  days_month <- cleaned_vars$days_per_month
  age_first_cig <- cleaned_vars$age_first_cigarette
  smoked_100 <- cleaned_vars$smoked_100_lifetime

  # Core mathematical logic
  result <- dplyr::case_when(
    # Missing data detection and priority processing
    any_missing(status, age) ~
      get_priority_missing(status, age, output_format = output_format),

    # 1 - Daily smokers: (age - age_started) * (cigs_per_day / 20)
    status == 1 ~ pmax(
      (age - age_started) * (cpd / 20),
      PACK_YEARS_CONSTANTS$min_pack_years
    ),

    # 2 - Occasional smokers (former daily): daily_period + occasional_period
    status == 2 ~ pmax(
      (age - age_started - time_quit) * (cpd / 20),
      PACK_YEARS_CONSTANTS$min_pack_years
    ) + ((pmax(cigs_occ * days_month / PACK_YEARS_CONSTANTS$days_per_month, 1) / 20) * time_quit),

    # 3 - Occasional smokers (never daily): (cigs * days/30) / 20 * duration
    status == 3 ~ (pmax(cigs_occ * days_month / PACK_YEARS_CONSTANTS$days_per_month, 1) / 20) *
      (age - age_first_cig),

    # 4 - Former daily smokers: (age - age_started - time_quit) * (cpd / 20)
    status == 4 ~ pmax(
      (age - age_started - time_quit) * (cpd / 20),
      PACK_YEARS_CONSTANTS$min_pack_years
    ),

    # 5 - Former occasional smokers: minimum pack-years based on 100+ cigs
    status == 5 & smoked_100 == 1 ~ PACK_YEARS_CONSTANTS$min_pack_years,
    status == 5 & smoked_100 == 2 ~ PACK_YEARS_CONSTANTS$min_pack_years_alt,

    # 6 - Never smokers
    status == 6 ~ 0.0,

    # Default
    .default = assign_missing("not_stated", "pack_years_der", output_format)
  )

  return(result)
}

