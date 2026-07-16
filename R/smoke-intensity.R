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

#' @title Calculate unified daily cigarette intensity (cigs_per_day)
#'
#' @description
#' Combine SMK_204 (current daily) and SMK_208 (former daily) into a
#' single cigarettes-per-day variable, routed by 6-category smoking status.
#'
#' @details
#' SMK_204 and SMK_208 are mutually exclusive: SMK_204 applies to current
#' daily smokers (status 1), SMK_208 to former daily smokers (status 2
#' and 4). Both capture the same concept (daily intensity) at different
#' time points. This function unifies them into a single variable,
#' following the same pattern as age_start_smoking and
#' time_quit_smoking. Never-daily smokers (status 3, 5, 6) receive
#' tagged_na("a"). Coverage: PUMF and Master 2001-2023.
#'
#' @param SMKDSTY_original Numeric. 6-category smoking status
#'   (1 = daily, 2 = occasional former daily, 3 = occasional never
#'   daily, 4 = former daily, 5 = former occasional, 6 = never).
#' @param SMK_204 Numeric. Cigarettes per day, current daily smokers
#'   (1-99).
#' @param SMK_208 Numeric. Cigarettes per day, former daily smokers
#'   (1-99).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of cigarettes per day (1-99). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar: current daily smoker
#' calculate_cigs_per_day(SMKDSTY_original = 1, SMK_204 = 20)
#'
#' # Scalar: former daily smoker
#' calculate_cigs_per_day(SMKDSTY_original = 4, SMK_208 = 15)
#'
#' @seealso \code{\link{calculate_SMK_204}},
#'   \code{\link{calculate_SMK_208}},
#'   \code{\link{calculate_pack_years}}
#'
#' @export
calculate_cigs_per_day <- function(SMKDSTY_original = NULL,
                                   SMK_204 = NULL,
                                   SMK_208 = NULL,
                                   output_format = "tagged_na") {

  # Handle all-NULL inputs (variable not collected in this cycle)
  if (is.null(SMKDSTY_original) && is.null(SMK_204) && is.null(SMK_208)) {
    return(haven::tagged_na("c"))
  }
  # Determine vector length and expand NULLs
  n <- max(length(SMKDSTY_original), length(SMK_204), length(SMK_208))
  optional <- expand_null_inputs(list(
    SMKDSTY_original = SMKDSTY_original,
    SMK_204 = SMK_204,
    SMK_208 = SMK_208
  ), n)
  SMKDSTY_original <- optional$SMKDSTY_original
  SMK_204 <- optional$SMK_204
  SMK_208 <- optional$SMK_208

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

#' @title Derive cigarettes per day for current daily smokers (SMK_204)
#'
#' @description
#' Harmonize SMK_204 (daily cigarette count for current daily smokers)
#' across CCHS cycles 2001-2023. Implemented via rec_with_table().
#'
#' @details
#' Source variables vary by era: SMKA_204 (2001), SMKC_204 (2003),
#' SMKE_204 (2005), SMK_204 (2007-2014), SMK_045 (2015-2021),
#' CSS_25 (2022-2023). Universe: current daily smokers
#' (SMKDSTY_original == 1). For unified daily intensity analysis,
#' prefer \code{\link{calculate_cigs_per_day}}.
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of cigarettes per day (1-99). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMK_204")
#' }
#'
#' @seealso \code{\link{calculate_cigs_per_day}},
#'   \code{\link{calculate_SMK_208}}
#'
#' @export
calculate_SMK_204 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_204') for implementation")
}


# ================================================================================
# SMK_208 - Cigarettes per day (former daily smokers) - DOCUMENTATION ONLY
# ================================================================================

#' @title Derive cigarettes per day for former daily smokers (SMK_208)
#'
#' @description
#' Harmonize SMK_208 (recalled daily cigarette count for former daily
#' smokers) across CCHS cycles 2001-2023. Implemented via
#' rec_with_table().
#'
#' @details
#' Source variables vary by era: SMKA_208 (2001), SMKC_208 (2003),
#' SMKE_208 (2005), SMK_208 (2007-2014), SMK_075 (2015-2021),
#' SPU_20 (2022-2023). Universe: former daily smokers
#' (SMKDSTY_original in 2, 4). For unified daily intensity analysis,
#' prefer \code{\link{calculate_cigs_per_day}}.
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of cigarettes per day (1-99). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMK_208")
#' }
#'
#' @seealso \code{\link{calculate_cigs_per_day}},
#'   \code{\link{calculate_SMK_204}}
#'
#' @export
calculate_SMK_208 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_208') for implementation")
}


# ================================================================================
# SMK_05B - Cigarettes per day (occasional smokers) - DOCUMENTATION ONLY
# ================================================================================

#' @title Derive cigarettes per day for occasional smokers (SMK_05B)
#'
#' @description
#' Harmonize SMK_05B (cigarettes per day on smoking days for occasional
#' smokers) across CCHS cycles 2001-2023. Implemented via
#' rec_with_table().
#'
#' @details
#' Source variables vary by era: SMKA_05B (2001), SMKC_05B (2003),
#' SMKE_05B (2005), SMK_05B (2007-2014), SMK_050 (2015-2021),
#' CSS_30 (2022-2023). Universe: current occasional smokers
#' (SMKDSTY_original in 2, 3).
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of cigarettes per day (1-99). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMK_05B")
#' }
#'
#' @seealso \code{\link{calculate_SMK_05C}},
#'   \code{\link{calculate_cigs_per_day}}
#'
#' @export
calculate_SMK_05B <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_05B') for implementation")
}


# ================================================================================
# SMK_05C - Days smoked per month - DOCUMENTATION ONLY
# ================================================================================

#' @title Derive days smoked per month for occasional smokers (SMK_05C)
#'
#' @description
#' Harmonize SMK_05C (days smoked in the past month for occasional
#' smokers) across CCHS cycles 2001-2023. Implemented via
#' rec_with_table().
#'
#' @details
#' Source variables vary by era: SMKA_05C (2001), SMKC_05C (2003),
#' SMKE_05C (2005), SMK_05C (2007-2014), SMK_055 (2015-2021),
#' CSS_35 (2022-2023). Universe: current occasional smokers
#' (SMKDSTY_original in 2, 3).
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of days per month (0-31). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMK_05C")
#' }
#'
#' @seealso \code{\link{calculate_SMK_05B}},
#'   \code{\link{calculate_cigs_per_day}}
#'
#' @export
calculate_SMK_05C <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_05C') for implementation")
}
