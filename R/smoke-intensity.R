# ================================================================================
# Smoking Intensity Classification Functions
# ================================================================================
#
# This file contains functions for smoking intensity variables:
#
# 1. cigs_per_day - Unified daily smoking intensity (cigarettes per day)
#    CCHS cycles: 2001 - 2023 (continuous values)
#    Universe: Ever-daily smokers (SMKDSTY_original 1, 2, 4)
#    Routes SMK_204 (current daily) or SMK_208 (former daily) based on status
#
# 2. SMK_204 - Cigarettes per day (current daily smokers)
#    CCHS cycles: 2001 - 2023 (continuous values)
#    Universe: Current daily smokers (SMKDSTY_original == 1)
#
# 3. SMK_208 - Cigarettes per day (former daily smokers)
#    CCHS cycles: 2001 - 2023 (continuous values)
#    Universe: Former daily smokers (SMKDSTY_original 2, 4)
#
# 4. SMK_05B - Cigarettes per day (occasional smokers)
#    CCHS cycles: 2001 - 2023 (continuous values)
#    Universe: Current occasional smokers
#
# 5. SMK_05C - Days smoked per month
#    CCHS cycles: 2001 - 2023 (continuous values)
#    Universe: Current occasional smokers
#
# ================================================================================

# Dependencies (haven, dplyr) come via DESCRIPTION Depends.
# Helper functions (clean_variables, missing-data-functions) loaded automatically in package context.

# ================================================================================
# cigs_per_day - Unified Daily Smoking Intensity
# ================================================================================

#' @title Unified Daily Smoking Intensity - cigs_per_day
#' @description Calculate unified cigarettes per day for ever-daily smokers
#'
#' Creates a unified `cigs_per_day` variable that combines SMK_204 (current daily
#' smokers) and SMK_208 (former daily smokers) into a single derived variable.
#' This follows the same unification pattern as `age_start_smoking` and
#' `time_quit_smoking`.
#'
#' @details
#' **Rationale**: SMK_204 and SMK_208 are mutually exclusive by design:
#' - **SMK_204**: Current daily smokers (status 1) - "How many cigarettes do you currently smoke per day?"
#' - **SMK_208**: Former daily smokers (status 2, 4) - "When you smoked daily, how many cigarettes did you usually smoke per day?"
#'
#' Both measure the **same concept**: daily smoking intensity. The difference is only
#' timing (current vs recalled). A unified variable simplifies the mental model and
#' aligns with how `age_start_smoking` and `time_quit_smoking` work.
#'
#' **Routing Logic**:
#' \itemize{
#'   \item SMKDSTY_original == 1 (current daily) -> uses SMK_204
#'   \item SMKDSTY_original == 2 (occasional, former daily) -> uses SMK_208
#'   \item SMKDSTY_original == 4 (former daily, non-smoker now) -> uses SMK_208
#'   \item SMKDSTY_original %in% c(3, 5, 6) (never daily) -> NA::a (not applicable)
#'   \item Missing SMKDSTY_original -> NA::b (missing)
#' }
#'
#' **Coverage**: Full PUMF 2001-2023, Full Master 2001-2023.
#'
#' @param SMKDSTY_original Numeric vector. Smoking status (6-category, pre-2015 definitions).
#'   1=Daily, 2=Occasional (former daily), 3=Occasional (never daily),
#'   4=Former daily, 5=Former occasional, 6=Never smoked.
#' @param SMK_204 Numeric vector. Cigarettes per day for current daily smokers.
#'   Valid range: 1-99 cigarettes.
#' @param SMK_208 Numeric vector. Cigarettes per day for former daily smokers.
#'   Valid range: 1-99 cigarettes.
#' @param output_format Character. Output format for missing values.
#'   Options: "tagged_na" (default) or "original".
#'
#' @return Numeric vector with unified cigarettes per day values.
#'   - Valid values: 1-99 cigarettes
#'   - NA::a: Not applicable (never-daily smokers)
#'   - NA::b: Missing data
#'
#' @examples
#' \dontrun{
#' # Current daily smoker (status 1) - uses SMK_204
#' cigs <- calculate_cigs_per_day(
#'   SMKDSTY_original = 1,
#'   SMK_204 = 20,
#'   SMK_208 = NA
#' )
#' # Returns: 20
#'
#' # Former daily smoker (status 4) - uses SMK_208
#' cigs <- calculate_cigs_per_day(
#'   SMKDSTY_original = 4,
#'   SMK_204 = NA,
#'   SMK_208 = 15
#' )
#' # Returns: 15
#'
#' # Never-daily smoker (status 3) - not applicable
#' cigs <- calculate_cigs_per_day(
#'   SMKDSTY_original = 3,
#'   SMK_204 = NA,
#'   SMK_208 = NA
#' )
#' # Returns: NA::a (not applicable)
#'
#' # Vector inputs with mixed status values
#' status <- c(1, 2, 3, 4, 5, 6)
#' smk_204 <- c(20, NA, NA, NA, NA, NA)
#' smk_208 <- c(NA, 15, NA, 25, NA, NA)
#' result <- calculate_cigs_per_day(status, smk_204, smk_208)
#' # Returns: c(20, 15, NA::a, 25, NA::a, NA::a)
#' }
#'
#' @seealso
#' \code{\link{calculate_age_start_smoking}} for age started smoking unification,
#' \code{\link{calculate_time_quit_smoking}} for time quit smoking unification,
#' \code{\link{calculate_pack_years_pumf}} for pack-years calculation using intensity.
#'
#' @note v3.0.0-alpha, last updated: 2026-01-09, status: active - Unified daily intensity
#' @export
calculate_cigs_per_day <- function(SMKDSTY_original,
                                   SMK_204,
                                   SMK_208,
                                   output_format = "tagged_na") {

  # Handle empty input vectors
  if (length(SMKDSTY_original) == 0) return(numeric(0))

  # === STEP 1: DATA CLEANING ===
  # Clean input variables (includes automatic length validation)
  cleaned <- clean_variables(
    vars = list(
      SMKDSTY_original = SMKDSTY_original,
      SMK_204 = SMK_204,
      SMK_208 = SMK_208
    ),
    output_format = "tagged_na"
  )

  # === STEP 2: DOMAIN LOGIC - Route based on smoking status ===
  result <- dplyr::case_when(
    # Handle missing status first
    any_missing(cleaned$SMKDSTY_original) ~
      get_priority_missing(cleaned$SMKDSTY_original, output_format = output_format),

    # Status 1: Current daily smoker - use SMK_204
    cleaned$SMKDSTY_original == 1 ~ cleaned$SMK_204,

    # Status 2: Occasional smoker (former daily) - use SMK_208
    cleaned$SMKDSTY_original == 2 ~ cleaned$SMK_208,

    # Status 4: Former daily smoker - use SMK_208
    cleaned$SMKDSTY_original == 4 ~ cleaned$SMK_208,

    # Status 3, 5, 6: Never-daily smokers - not applicable
    cleaned$SMKDSTY_original %in% c(3, 5, 6) ~
      assign_missing("not_applicable", "cigs_per_day", output_format),

    # Default: missing
    .default = assign_missing("not_stated", "cigs_per_day", output_format)
  )

  # === STEP 3: OUTPUT CLEANING ===
  output_cleaned <- clean_variables(vars = list(
    cigs_per_day = result
  ), output_format = output_format)

  return(output_cleaned$cigs_per_day)
}


# ================================================================================
# SMK_204 - Cigarettes per day (current daily smokers) - DOCUMENTATION ONLY
# ================================================================================

#' @title Cigarettes per Day - SMK_204 (current daily smokers)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#'
#' Harmonizes SMK_204 variable across CCHS cycles 2001-2023.
#' This variable captures daily cigarette consumption for current daily smokers.
#'
#' @details
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**:
#'   - 2001: SMKA_204
#'   - 2003: SMKC_204
#'   - 2005: SMKE_204
#'   - 2007-2014: SMK_204
#'   - 2015-2021: SMK_045
#'   - 2022-2023: CSS_25
#'
#' **Values**: Continuous (1-99 cigarettes per day)
#'
#' **Universe**: Current daily smokers (SMKDSTY_original == 1)
#'
#' **Recommendation**: Use `cigs_per_day` for unified daily intensity analysis.
#' SMK_204 is available as a secondary variable for specific use cases requiring
#' separation of current vs former daily intensity.
#'
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values
#'
#' @return Numeric vector with cigarettes per day (1-99, plus missing codes)
#'
#' @seealso \code{\link{calculate_cigs_per_day}} for unified daily intensity
#'
#' @export
calculate_SMK_204 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_204') for implementation")
}


# ================================================================================
# SMK_208 - Cigarettes per day (former daily smokers) - DOCUMENTATION ONLY
# ================================================================================

#' @title Cigarettes per Day - SMK_208 (former daily smokers)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#'
#' Harmonizes SMK_208 variable across CCHS cycles 2001-2023.
#' This variable captures recalled daily cigarette consumption for former daily smokers.
#'
#' @details
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**:
#'   - 2001: SMKA_208
#'   - 2003: SMKC_208
#'   - 2005: SMKE_208
#'   - 2007-2014: SMK_208
#'   - 2015-2021: SMK_075
#'   - 2022-2023: SPU_20
#'
#' **Values**: Continuous (1-99 cigarettes per day when they smoked daily)
#'
#' **Universe**: Former daily smokers (SMKDSTY_original %in% c(2, 4))
#'
#' **Recommendation**: Use `cigs_per_day` for unified daily intensity analysis.
#' SMK_208 is available as a secondary variable for specific use cases requiring
#' separation of current vs former daily intensity.
#'
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values
#'
#' @return Numeric vector with cigarettes per day (1-99, plus missing codes)
#'
#' @seealso \code{\link{calculate_cigs_per_day}} for unified daily intensity
#'
#' @export
calculate_SMK_208 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_208') for implementation")
}


# ================================================================================
# SMK_05B - Cigarettes per day (occasional smokers) - DOCUMENTATION ONLY
# ================================================================================

#' @title Cigarettes per Day - SMK_05B (occasional smokers)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#'
#' Harmonizes SMK_05B variable across CCHS cycles 2001-2023.
#' This variable captures daily cigarette consumption on days when occasional
#' smokers do smoke.
#'
#' @details
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**:
#'   - 2001: SMKA_05B
#'   - 2003: SMKC_05B
#'   - 2005: SMKE_05B
#'   - 2007-2014: SMK_05B
#'   - 2015-2021: SMK_050
#'   - 2022-2023: CSS_30
#'
#' **Values**: Continuous (1-99 cigarettes per day when smoking)
#'
#' **Universe**: Current occasional smokers (SMKDSTY_original %in% c(2, 3))
#'
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values
#'
#' @return Numeric vector with cigarettes per day (1-99, plus missing codes)
#'
#' @export
calculate_SMK_05B <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_05B') for implementation")
}


# ================================================================================
# SMK_05C - Days smoked per month - DOCUMENTATION ONLY
# ================================================================================

#' @title Days Smoked per Month - SMK_05C
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#'
#' Harmonizes SMK_05C variable across CCHS cycles 2001-2023.
#' This variable captures the number of days in the past month when occasional
#' smokers smoked at least one cigarette.
#'
#' @details
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**:
#'   - 2001: SMKA_05C
#'   - 2003: SMKC_05C
#'   - 2005: SMKE_05C
#'   - 2007-2014: SMK_05C
#'   - 2015-2021: SMK_055
#'   - 2022-2023: CSS_35
#'
#' **Values**: Continuous (0-31 days)
#'
#' **Universe**: Current occasional smokers (SMKDSTY_original %in% c(2, 3))
#'
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values
#'
#' @return Numeric vector with days per month (0-31, plus missing codes)
#'
#' @export
calculate_SMK_05C <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_05C') for implementation")
}
