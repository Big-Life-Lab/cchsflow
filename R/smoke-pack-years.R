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

#' Pack-Years Calculation (Modular Architecture)
#'
#' Calculates cumulative smoking exposure in pack-years using unified derived
#' variables. This is the recommended interface as it reuses existing feeder
#' functions rather than duplicating their logic.
#'
#' This function works with both CCHS PUMF and Master files. The parameter names
#' follow CCHS conventions but accept any appropriately formatted data. The key
#' difference between PUMF and Master is the precision of input variables, not
#' the calculation itself.
#'
#' @param SMKDSTY_A Numeric. 6-category smoking status (1=daily, 2=occasional former daily,
#'   3=always occasional, 4=former daily, 5=former occasional, 6=never). In CCHS this
#'   comes from calculate_SMKDSTY_A(), but any 1-6 coded status variable works.
#' @param DHHGAGE_cont Numeric. Current age in years (continuous). Named after CCHS
#'   demographic variable, but accepts any continuous age. For PUMF data, use midpoint-
#'   derived age from grouped categories; for Master data, use true reported age.
#' @param age_start_smoking Numeric. Age started smoking daily (from calculate_age_start_smoking).
#'
#'   Unified variable that handles both current and former daily smokers.
#' @param cigs_per_day Numeric. Cigarettes per day (from calculate_cigs_per_day).
#'   Unified variable that routes SMK_204/SMK_208 based on smoking status.
#' @param time_quit_smoking Numeric. Years since quit (from calculate_time_quit_smoking).
#' @param cigs_occasional Numeric. Cigarettes per occasion for occasional smokers (SMK_05B).
#'   Required for status 2 (occasional, former daily) and status 3 (occasional, never daily).
#' @param days_per_month Numeric. Days smoked per month for occasional smokers (SMK_05C).
#'   Required for status 2 and status 3.
#' @param age_first_cigarette Numeric. Age of first cigarette (SMKG01C_cont).
#'   Required for status 3 (occasional, never daily) only.
#' @param smoked_100_lifetime Numeric. Ever smoked 100+ cigarettes (SMK_01A: 1=yes, 2=no).
#'
#'   Required for status 5 (former occasional) to determine minimum pack-years.
#' @param output_format Character. Output format for missing values ("tagged_na" or "numeric").
#'
#' @return Numeric vector with pack-years values (continuous, range 0-165)
#'
#' @details
#' ## PUMF vs Master file usage
#'
#' This function produces identical calculations for both file types. The difference
#' is in the **precision of input variables**:
#'
#' | Input | PUMF | Master |
#' |-------|------|--------|
#' | Current age | Midpoint from grouped (~±2.5 yr) | True continuous |
#' | Age started | Midpoint from SMKG203/207 (~±3 yr) | True SMK_040 |
#' | CPD | Capped at 50 | Uncapped |
#' | Time quit | Midpoint (~±1.5 yr) | Near-continuous |
#'
#' PUMF pack-years estimates have approximately 15-20% relative error compared to
#' Master. For most epidemiological analyses this is acceptable; for precise
#' dose-response modelling, Master files are preferred.
#'
#' ## Formula by smoking status
#'
#' | Status | Description | Formula |
#' |--------|-------------|---------|
#' | 1 | Daily smoker | (age - age_start) × (cigs_per_day / 20) |
#' | 2 | Occasional (former daily) | daily_period + occasional_period |
#' | 3 | Occasional (never daily) | (cigs × days/30) / 20 × (age - age_first_cig) |
#' | 4 | Former daily | (age - age_start - time_quit) × (cigs_per_day / 20) |
#' | 5 | Former occasional | 0.0137 (100+ cigs) or 0.007 (under 100) |
#' | 6 | Never smoker | 0 |
#'
#' ## Advantages over legacy functions
#'
#' - **Single source of truth**: Uses calculate_age_start_smoking() and calculate_cigs_per_day()
#'   instead of duplicating their routing logic

#' - **Era-agnostic**: Unified feeders handle 2001-2023 variable naming variations
#' - **Cleaner API**: 9 semantic parameters vs 11 raw parameters
#' - **Better testability**: Each feeder can be tested independently
#'
#' @examples
#' \dontrun{
#' # ===========================================================================
#' # Example 1: PUMF data (grouped categories, midpoint-derived values)
#' # ===========================================================================
#' # PUMF variables use category midpoints for age variables, resulting in
#' # ~15-20% relative error in pack-years estimates.
#'
#' # Compute unified feeder variables from PUMF
#' age_start_pumf <- calculate_age_start_smoking(
#'   SMKG203_cont = pumf_data$SMKG203_cont,  # Midpoint-derived (current daily)
#'   SMKG207_cont = pumf_data$SMKG207_cont   # Midpoint-derived (former daily)
#' )
#' cigs_pumf <- calculate_cigs_per_day(
#'   pumf_data$SMKDSTY_A,
#'   pumf_data$SMK_204,  # Capped at 50 in PUMF
#'   pumf_data$SMK_208
#' )
#' time_quit_pumf <- calculate_time_quit_smoking(
#'   pumf_data$SMK_09A_cont,
#'   pumf_data$SMK_06A_cont
#' )
#'
#' # Calculate pack-years (PUMF)
#' pack_years_pumf <- calculate_pack_years(
#'   SMKDSTY_A = pumf_data$SMKDSTY_A,
#'   DHHGAGE_cont = pumf_data$DHHGAGE_cont,  # Midpoint from age groups
#'   age_start_smoking = age_start_pumf,
#'   cigs_per_day = cigs_pumf,
#'   time_quit_smoking = time_quit_pumf,
#'   cigs_occasional = pumf_data$SMK_05B,
#'   days_per_month = pumf_data$SMK_05C,
#'   age_first_cigarette = pumf_data$SMKG01C_cont,
#'   smoked_100_lifetime = pumf_data$SMK_01A
#' )
#'
#' # ===========================================================================
#' # Example 2: Master data (true continuous values, higher precision)
#' # ===========================================================================
#' # Master files have true reported values without grouping/capping.
#'
#' # Compute unified feeder variables from Master
#' age_start_master <- calculate_age_start_smoking(
#'   SMK_040 = master_data$SMK_040  # True reported age (no midpoint)
#' )
#' cigs_master <- calculate_cigs_per_day(
#'   master_data$SMKDSTY_A,
#'   master_data$SMK_204,  # Uncapped in Master
#'   master_data$SMK_208
#' )
#' time_quit_master <- calculate_time_quit_smoking(
#'   master_data$SMK_09A_cont,
#'   master_data$SMK_06A_cont
#' )
#'
#' # Calculate pack-years (Master) - same function, higher precision inputs
#' pack_years_master <- calculate_pack_years(
#'   SMKDSTY_A = master_data$SMKDSTY_A,
#'   DHHGAGE_cont = master_data$DHH_AGE,     # True age (not grouped)
#'   age_start_smoking = age_start_master,
#'   cigs_per_day = cigs_master,
#'   time_quit_smoking = time_quit_master,
#'   cigs_occasional = master_data$SMK_05B,
#'   days_per_month = master_data$SMK_05C,
#'   age_first_cigarette = master_data$SMK01C_cont,
#'   smoked_100_lifetime = master_data$SMK_01A
#' )
#' }
#'
#' @seealso [calculate_age_start_smoking()], [calculate_cigs_per_day()],
#'   [calculate_time_quit_smoking()]
#'
#' @export
calculate_pack_years <- function(SMKDSTY_A,
                                 DHHGAGE_cont,
                                 age_start_smoking,
                                 cigs_per_day,
                                 time_quit_smoking,
                                 cigs_occasional = NULL,
                                 days_per_month = NULL,
                                 age_first_cigarette = NULL,
                                 smoked_100_lifetime = NULL,
                                 output_format = "tagged_na") {

  # Determine vector length from primary inputs
  n <- length(SMKDSTY_A)

  # Handle NULL optional inputs - convert to NA vectors
  if (is.null(cigs_occasional)) cigs_occasional <- rep(NA_real_, n)
  if (is.null(days_per_month)) days_per_month <- rep(NA_real_, n)
  if (is.null(age_first_cigarette)) age_first_cigarette <- rep(NA_real_, n)
  if (is.null(smoked_100_lifetime)) smoked_100_lifetime <- rep(NA_real_, n)

  # === STEP 1: DATA CLEANING ===
  cleaned <- clean_variables(vars = list(
    SMKDSTY_A = SMKDSTY_A,
    DHHGAGE_cont = DHHGAGE_cont,
    age_start_smoking = age_start_smoking,
    cigs_per_day = cigs_per_day,
    time_quit_smoking = time_quit_smoking,
    cigs_occasional = cigs_occasional,
    days_per_month = days_per_month,
    age_first_cigarette = age_first_cigarette,
    smoked_100_lifetime = smoked_100_lifetime
  ), output_format = output_format)

  # === STEP 2: CORE CALCULATION ===
  result <- .calculate_pack_years_core(cleaned, output_format)

  # === STEP 3: OUTPUT VALIDATION ===
  # Bounds [0, 165] are defined in variable_details.csv recEnd field
  # clean_variables() reads metadata and applies range validation
  output_cleaned <- clean_variables(vars = list(
    pack_years_der = result
  ), output_format = output_format)

  return(output_cleaned$pack_years_der)
}


# ==============================================================================
# CORE MATHEMATICAL LOGIC (DATA SOURCE AGNOSTIC)
# ==============================================================================

#' Core Pack-Years Calculation Logic
#'
#' Internal function containing the mathematical formulas for pack-years calculation.
#' This function is data-source agnostic and works with cleaned variables using
#' semantic names.
#'
#' @param cleaned_vars List of cleaned variables from clean_variables().
#'   Expected names depend on caller (modular vs legacy).
#' @param output_format Character. Output format for missing values
#' @return Numeric vector with pack-years values
#' @noRd
.calculate_pack_years_core <- function(cleaned_vars, output_format = "tagged_na") {

  # Extract variables - support both modular and legacy naming
  smoking_status <- cleaned_vars$SMKDSTY_A
  current_age <- cleaned_vars$DHHGAGE_cont

  # Modular naming (unified feeders)
  if (!is.null(cleaned_vars$age_start_smoking)) {
    age_started <- cleaned_vars$age_start_smoking
    daily_intensity <- cleaned_vars$cigs_per_day
    time_quit <- cleaned_vars$time_quit_smoking
    cigs_occasional <- cleaned_vars$cigs_occasional
    days_monthly <- cleaned_vars$days_per_month
    age_first_cig <- cleaned_vars$age_first_cigarette
    smoked_100 <- cleaned_vars$smoked_100_lifetime
  } else {
    # Legacy naming (separate current/former variables)
    # For status 1: use current; for status 2, 4: use former
    age_started <- dplyr::case_when(
      smoking_status == 1 ~ cleaned_vars$age_started_daily_current,
      smoking_status %in% c(2, 4) ~ cleaned_vars$age_started_daily_former,
      .default = NA_real_
    )
    daily_intensity <- dplyr::case_when(
      smoking_status == 1 ~ cleaned_vars$SMK_204,
      smoking_status %in% c(2, 4) ~ cleaned_vars$SMK_208,
      .default = NA_real_
    )
    time_quit <- cleaned_vars$time_quit_smoking
    cigs_occasional <- cleaned_vars$SMK_05B
    days_monthly <- cleaned_vars$SMK_05C
    age_first_cig <- cleaned_vars$age_first_cigarette
    smoked_100 <- cleaned_vars$SMK_01A
  }

  # Core mathematical logic - same formulas regardless of data source
  result <- dplyr::case_when(
    # Missing data detection and priority processing
    any_missing(smoking_status, current_age) ~
      get_priority_missing(smoking_status, current_age, output_format = output_format),

    # 1 - Daily smokers: (age - age_started) × (cigs_per_day / 20)
    smoking_status == 1 ~ pmax(
      (current_age - age_started) * (daily_intensity / 20),
      PACK_YEARS_CONSTANTS$min_pack_years
    ),

    # 2 - Occasional smokers (former daily): Daily period + Occasional period
    smoking_status == 2 ~ pmax(
      (current_age - age_started - time_quit) * (daily_intensity / 20),
      PACK_YEARS_CONSTANTS$min_pack_years
    ) + ((pmax(cigs_occasional * days_monthly / PACK_YEARS_CONSTANTS$days_per_month, 1) / 20) * time_quit),

    # 3 - Occasional smokers (never daily): (cigs × days/30) / 20 × (age - age_first_cig)
    smoking_status == 3 ~ (pmax(cigs_occasional * days_monthly / PACK_YEARS_CONSTANTS$days_per_month, 1) / 20) *
      (current_age - age_first_cig),

    # 4 - Former daily smokers: (age - age_started - time_quit) × (cigs_per_day / 20)
    smoking_status == 4 ~ pmax(
      (current_age - age_started - time_quit) * (daily_intensity / 20),
      PACK_YEARS_CONSTANTS$min_pack_years
    ),

    # 5 - Former occasional smokers: Different minimums based on 100+ cigarettes
    smoking_status == 5 & smoked_100 == 1 ~ PACK_YEARS_CONSTANTS$min_pack_years,
    smoking_status == 5 & smoked_100 == 2 ~ PACK_YEARS_CONSTANTS$min_pack_years_alt,

    # 6 - Never smokers
    smoking_status == 6 ~ 0.0,

    # Handle missing smoking_status values with tagged_na priority
    haven::is_tagged_na(smoking_status, "a") ~ assign_missing(output_format, "a"),

    # Default case for any other values
    .default = assign_missing(output_format, "b")
  )

  return(result)
}

