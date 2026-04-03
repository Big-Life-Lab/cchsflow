# ================================================================================
# Smoking Cessation Classification Functions
# ================================================================================
#
# There are six smoking cessation variables harmonized between CCHS 2001 and 2023:
#
# 1. SMK_06A_2001 - "When stopped smoking - occasional smoker, 2001 categories (categorical)"
#    CCHS cycles: 2001 (4 categories; different boundaries than 2003+)
#
# 2. SMK_06A_2003plus - "When stopped smoking - occasional smoker, 2003+ categories (categorical)"
#    CCHS cycles: 2003 -> 2023 (4 categories: 1=<1yr, 2=1-2yr, 3=2-3yr, 4=3+yr)
#
# 3. SMK_06A_cont - "When stopped smoking - occasional smoker (continuous)"
#    CCHS cycles: 2001 -> 2023 (continuous years via midpoint imputation)
#
# 4. SMK_09A_2001 - "When stopped smoking daily - former daily smoker, 2001 categories (categorical)"
#    CCHS cycles: 2001 (4 categories; different boundaries than 2003+)
#
# 5. SMK_09A_2003plus - "When stopped smoking daily - former daily smoker, 2003+ categories (categorical)"
#    CCHS cycles: 2003 -> 2023 (4 categories: 1=<1yr, 2=1-2yr, 3=2-3yr, 4=3+yr)
#
# 6. SMK_09A_cont - "When stopped smoking daily - former daily smoker (continuous, PUMF only)"
#    CCHS cycles: PUMF 2001 -> 2023 (continuous years via midpoint imputation)
#
# Combined outputs (DV functions in smoking-cessation.R):
# - time_quit_smoking_daily: years since stopped daily smoking (PUMF+Master 2001-2023)
# - time_quit_smoking_complete: years since stopped smoking completely (PUMF+Master 2001-2023)
#
# IMPLEMENTATION: Canonical functions are in R/smoking-cessation.R.
# This file contains documentation stubs only.
#
# ================================================================================

# Package dependencies are declared in DESCRIPTION and loaded via NAMESPACE
# Functions used: haven::tagged_na(), haven::is_tagged_na(), dplyr::case_when()
# Internal functions: clean_variables(), any_missing(), get_priority_missing()

# ================================================================================

# SMK_06A_cat4 - When stopped smoking - occasional/never daily (categorical) - DOCUMENTATION ONLY
# ================================================================================

#' @title When Stopped Smoking - Occasional/Never Daily - SMK_06A_cat4 (categorical)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation.
#'
#' Harmonized SMK_06A across CCHS cycles 2001-2023, with 4 categories.
#' The _cat4 suffix indicates that 2001 categories differ from 2003+
#' (different interval boundaries) but are harmonized to 4 common categories.
#'
#' @details
#' **Implementation**: Direct harmonization via rec_with_table().
#' rec_with_table() reads the worksheet rows and applies recStart→recEnd mappings.
#'
#' **Categories (4)**:
#' \itemize{
#'   \item 1 = Less than one year ago
#'   \item 2 = 1 year to less than 2 years ago
#'   \item 3 = 2 years to less than 3 years ago (2001: 3-5 years)
#'   \item 4 = 3 or more years ago (2001: 5+ years)
#' }
#'
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#'
#' @return Vector of time period classifications (1-4, plus missing value codes)
#'
#' @examples
#' \dontrun{
#' harmonized_data <- rec_with_table(cchs_data, "SMK_06A_cat4")
#' }
#'
#' @export
calculate_SMK_06A_cat4 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_06A_cat4') for implementation")
}

# ================================================================================

# SMK_06A_cont - When stopped smoking - occasional/never daily (continuous) - DOCUMENTATION ONLY
# ================================================================================
# REMOVED: calculate_SMK_06A_cont_stub() deleted — worksheet-first principle.
# SMK_06A_cont uses direct recode rows (recStart → recEnd midpoints) like
# DHHGAGE_cont. No R function needed. Use rec_with_table(data, "SMK_06A_cont").

# ================================================================================

# SMK_09A_2003plus - When stopped smoking daily - former daily (categorical, 2003+) - DOCUMENTATION ONLY
# ================================================================================

#' @title When Stopped Smoking Daily - Former Daily Smoker - SMK_09A_2003plus (categorical)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation.
#'
#' Categorical when-stopped-smoking-daily for former daily smokers, 2003–2023 cycles.
#' Source variables use the same 1–4 scale across all cycles; no recoding of category
#' boundaries. The era suffix distinguishes this from SMK_09A_2001 (different boundaries).
#'
#' @details
#' **Implementation**: Direct harmonization via rec_with_table().
#'
#' **Categories (4)**:
#' \itemize{
#'   \item 1 = Less than one year ago
#'   \item 2 = 1 year to less than 2 years ago
#'   \item 3 = 2 years to less than 3 years ago
#'   \item 4 = 3 or more years ago
#' }
#'
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values
#'
#' @return Vector of time period classifications (1-4, plus missing value codes)
#'
#' @examples
#' \dontrun{
#' harmonized_data <- rec_with_table(cchs_data, "SMK_09A_2003plus")
#' }
#'
#' @export
calculate_SMK_09A_2003plus <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_09A_2003plus') for implementation")
}

# ================================================================================

# TIME_QUIT_SMOKING_COMPLETE - Combined cessation timeframe (complete quit) - DOCUMENTATION ONLY
# ================================================================================

#' @title Years Since Stopped Smoking Completely - TIME_QUIT_SMOKING_COMPLETE (continuous)
#' @description DOCUMENTATION ONLY - canonical implementation is in smoking-cessation.R.
#'
#' Pathway-aware years since completely quit smoking. Uses SMKDVSTP (Master)
#' when available, then routes by quit pathway on PUMF.
#'
#' @param SMKDSTY_cat5 Numeric vector. 5-category smoking status
#' @param SMK_10_gate Numeric vector. Quit timing gate (1 or 2)
#' @param SMK_06A_cont Numeric vector. Years since quit (former occasional)
#' @param SMK_09A_cont Numeric vector. Years since stopped daily
#' @param SMK_10A_cont Numeric vector. Years since quit completely (gradual)
#' @param SMKDVSTP Numeric vector. Master continuous years since quit completely
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Vector of continuous years since cessation
#'
#' @examples
#' \dontrun{
#' # See calculate_time_quit_smoking_complete() in smoking-cessation.R
#' }
#'
#' @export
calculate_time_quit_smoking_complete_stub <- function(SMKDSTY_cat5, SMK_10_gate,
                                                       SMK_06A_cont, SMK_09A_cont,
                                                       SMK_10A_cont, SMKDVSTP,
                                                       output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use calculate_time_quit_smoking_complete() from smoking-cessation.R")
}

# ================================================================================

# TIME_QUIT_SMOKING_DAILY - Years since stopped daily smoking - DOCUMENTATION ONLY
# ================================================================================

#' @title Years Since Stopped Smoking Daily - TIME_QUIT_SMOKING_DAILY (continuous)
#' @description DOCUMENTATION ONLY - canonical implementation is in smoking-cessation.R.
#'
#' Years since former daily smokers stopped smoking daily. Uses SMK_09C
#' (Master exact years) when available, falls back to SMK_09A_cont (PUMF midpoint).
#'
#' @param SMKDSTY_cat5 Numeric vector. 5-category smoking status
#' @param SMK_09A_cont Numeric vector. Midpoint-imputed years (PUMF building block)
#' @param SMK_09C Numeric vector. Exact years since stopped daily (Master)
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Vector of continuous years since stopped daily smoking
#'
#' @examples
#' \dontrun{
#' # See calculate_time_quit_smoking_daily() in smoking-cessation.R
#' }
#'
#' @export
calculate_time_quit_smoking_daily_stub <- function(SMKDSTY_cat5, SMK_09A_cont,
                                                    SMK_09C = NULL,
                                                    output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use calculate_time_quit_smoking_daily() from smoking-cessation.R")
}