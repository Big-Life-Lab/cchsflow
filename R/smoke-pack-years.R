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

#' Pack-Years Calculation
#'
#' Calculates cumulative smoking exposure in pack-years. All parameters use
#' semantic names — the worksheet routes the appropriate source variables
#' depending on database type (PUMF vs Master).
#'
#' @param smoking_status Numeric. 6-category smoking status:
#'   1=daily, 2=occasional (former daily), 3=occasional (never daily),
#'   4=former daily, 5=former occasional, 6=never.
#' @param age Numeric. Current age in years (continuous).
#' @param age_start_smoking Numeric. Age started smoking daily.
#' @param cigs_per_day Numeric. Cigarettes per day when smoking daily.
#' @param time_quit_smoking Numeric. Years since quit smoking.
#' @param cigs_occasional Numeric. Cigarettes per occasion (occasional smokers).
#'   Required for status 2 and 3. NULL if not available.
#' @param days_per_month Numeric. Days smoked per month (occasional smokers).
#'   Required for status 2 and 3. NULL if not available.
#' @param age_first_cigarette Numeric. Age of first cigarette.
#'   Required for status 3 only. NULL if not available.
#' @param smoked_100_lifetime Numeric. Ever smoked 100+ cigarettes (1=yes, 2=no).
#'   Required for status 5 only. NULL if not available.
#' @param output_format Character. Output format for missing values
#'   ("tagged_na" or "numeric").
#'
#' @return Numeric vector with pack-years values (continuous, range 0-165)
#'
#' @details
#' ## PUMF vs Master
#'
#' This function is source-agnostic. The worksheet routes different source
#' variables to the same semantic parameters depending on database type.
#' The function produces identical calculations; the difference is in the
#' **precision of input variables**:
#'
#' | Input | PUMF | Master |
#' |-------|------|--------|
#' | age | Midpoint from grouped (~+/-2.5 yr) | True continuous |
#' | age_start_smoking | Midpoint (~+/-3 yr) | True continuous |
#' | cigs_per_day | Capped at 50 | Uncapped |
#' | time_quit_smoking | Midpoint (~+/-1.5 yr) | Near-continuous |
#'
#' PUMF pack-years estimates have approximately 15-20% relative error compared
#' to Master. For most epidemiological analyses this is acceptable; for precise
#' dose-response modelling, Master files are preferred.
#'
#' ## Formula by smoking status
#'
#' | Status | Description | Formula |
#' |--------|-------------|---------|
#' | 1 | Daily smoker | (age - age_start) * (cigs_per_day / 20) |
#' | 2 | Occasional (former daily) | daily_period + occasional_period |
#' | 3 | Occasional (never daily) | (cigs * days/30) / 20 * (age - age_first_cig) |
#' | 4 | Former daily | (age - age_start - time_quit) * (cigs_per_day / 20) |
#' | 5 | Former occasional | 0.0137 (100+ cigs) or 0.007 (under 100) |
#' | 6 | Never smoker | 0 |
#'
#' @examples
#' \dontrun{
#' # Daily smoker: 45yo, started at 20, 20 cigs/day
#' calculate_pack_years(
#'   smoking_status = 1, age = 45,
#'   age_start_smoking = 20, cigs_per_day = 20,
#'   time_quit_smoking = NA
#' )
#' # Returns: 25.0
#'
#' # Former daily: 55yo, started at 20, quit 10 years ago
#' calculate_pack_years(
#'   smoking_status = 4, age = 55,
#'   age_start_smoking = 20, cigs_per_day = 20,
#'   time_quit_smoking = 10
#' )
#' # Returns: 25.0
#'
#' # Never smoker
#' calculate_pack_years(
#'   smoking_status = 6, age = 50,
#'   age_start_smoking = NA, cigs_per_day = NA,
#'   time_quit_smoking = NA
#' )
#' # Returns: 0
#' }
#'
#' @seealso [calculate_age_start_smoking()], [calculate_cigs_per_day()],
#'   [calculate_time_quit_smoking()]
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
  if (is.null(cigs_occasional)) cigs_occasional <- rep(NA_real_, n)
  if (is.null(days_per_month)) days_per_month <- rep(NA_real_, n)
  if (is.null(age_first_cigarette)) age_first_cigarette <- rep(NA_real_, n)
  if (is.null(smoked_100_lifetime)) smoked_100_lifetime <- rep(NA_real_, n)

  # === STEP 1: DATA CLEANING ===
  # List names must match variable_details.csv for pattern lookup
  cleaned_raw <- clean_variables(vars = list(
    SMKDSTY_A = smoking_status,
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
  cleaned$smoking_status <- cleaned_raw$SMKDSTY_A
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

#' Categorise Pack-Years into 5 Groups
#'
#' Converts continuous pack-years values into a 5-category ordinal variable.
#' This function is called by `rec_with_table()` via the
#' `Func::calculate_pack_years_categorical` reference in `variable_details.csv`.
#'
#' @param pack_years_der Numeric. Continuous pack-years from [calculate_pack_years()].
#' @param output_format Character. Output format for missing values
#'   ("tagged_na" or "numeric").
#'
#' @return Numeric vector with categories 0-4:
#'   - 0: Never smoker (pack-years == 0)
#'   - 1: Light (0 < pack-years < 10)
#'   - 2: Moderate (10 <= pack-years < 20)
#'   - 3: Heavy (20 <= pack-years < 30)
#'   - 4: Very heavy (pack-years >= 30)
#'
#' @details
#' Cut-points are defined in `PACK_YEARS_CONSTANTS$pack_years_cat_breaks` and
#' match the `recStart`/`recEnd` ranges in `variable_details.csv` for
#' `pack_years_cat`. The cut-points are pending epidemiological review;
#' the worksheet status is `pending_review`.
#'
#' @examples
#' \dontrun{
#' # Single values
#' calculate_pack_years_categorical(0)      # 0 (never smoker)
#' calculate_pack_years_categorical(5.2)    # 1 (light: 0-10)
#' calculate_pack_years_categorical(15.0)   # 2 (moderate: 10-20)
#' calculate_pack_years_categorical(25.0)   # 3 (heavy: 20-30)
#' calculate_pack_years_categorical(40.0)   # 4 (very heavy: 30+)
#'
#' # Boundary values
#' calculate_pack_years_categorical(9.999)  # 1 (light)
#' calculate_pack_years_categorical(10.0)   # 2 (moderate)
#'
#' # Vector input with mixed categories
#' calculate_pack_years_categorical(c(0, 5, 15, 25, 40, NA))
#'
#' # Tagged NA pass-through
#' calculate_pack_years_categorical(tagged_na("a"))  # NA::a (not applicable)
#' }
#'
#' @seealso [calculate_pack_years()], [PACK_YEARS_CONSTANTS]
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

  return(output_cleaned$pack_years_cat)
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

