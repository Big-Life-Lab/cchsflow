# ================================================================================
# Smoking Cessation Classification Functions
# ================================================================================
#
# There are four smoking cessation variables harmonized between CCHS 2001 and 2023:
#
# 1. SMK_06A_cat4 - "When did you stop smoking - occasional/never daily smoker (categorical)"
#    CCHS cycles: 2001 -> 2023 (4 harmonized categories)
#    Categories: 1=<1yr ago, 2=1-2yr ago, 3=2-3yr ago, 4=3+yr ago
#
# 2. SMK_06A_cont - "When did you stop smoking - occasional/never daily smoker (continuous)"
#    CCHS cycles: 2001 -> 2023 (continuous years via midpoint imputation)
#
# 3. SMK_09A_cat4 - "When did you stop smoking daily - former daily smoker (categorical)"
#    CCHS cycles: 2001 -> 2023 (4 harmonized categories)
#    Categories: 1=<1yr ago, 2=1-2yr ago, 3=2-3yr ago, 4=3+yr ago
#
# 4. SMK_09A_cont - "When did you stop smoking daily - former daily smoker (continuous)"
#    CCHS cycles: 2001 -> 2023 (continuous years via midpoint imputation)
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

#' @title When Stopped Smoking - Occasional/Never Daily - SMK_06A_cont (continuous)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation.
#'
#' Continuous years since quit for former occasional/never daily smokers.
#' rec_with_table() converts categorical codes to midpoints via worksheet recEnd values.
#' The canonical DV function is in smoking-cessation.R (used by combining functions).
#'
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values
#'
#' @return Vector of continuous years values (numeric, plus missing value codes)
#'
#' @examples
#' \dontrun{
#' harmonized_data <- rec_with_table(cchs_data, "SMK_06A_cont")
#' }
#'
#' @export
calculate_SMK_06A_cont_stub <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_06A_cont') for implementation. ",
       "Canonical DV function is calculate_SMK_06A_cont() in smoking-cessation.R")
}

# ================================================================================

# SMK_09A_cat4 - When stopped smoking daily - former daily (categorical) - DOCUMENTATION ONLY
# ================================================================================

#' @title When Stopped Smoking Daily - Former Daily Smoker - SMK_09A_cat4 (categorical)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation.
#'
#' Harmonized SMK_09A across CCHS cycles 2001-2023, with 4 categories.
#' The _cat4 suffix indicates that 2001 categories differ from 2003+
#' (different interval boundaries) but are harmonized to 4 common categories.
#'
#' @details
#' **Implementation**: Direct harmonization via rec_with_table().
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
#' @param output_format Character. Output format for missing values
#'
#' @return Vector of time period classifications (1-4, plus missing value codes)
#'
#' @examples
#' \dontrun{
#' harmonized_data <- rec_with_table(cchs_data, "SMK_09A_cat4")
#' }
#'
#' @export
calculate_SMK_09A_cat4 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_09A_cat4') for implementation")
}

# ================================================================================
# SMK_09A_cont - When stopped smoking daily - former daily (continuous) - DOCUMENTATION ONLY
# ================================================================================

#' @title When Stopped Smoking Daily - Former Daily Smoker - SMK_09A_cont (continuous)
#' @description DOCUMENTATION ONLY - canonical implementation is in smoking-cessation.R.
#'
#' Continuous years since stopped daily for former daily smokers.
#' For passthrough use: rec_with_table() handles midpoint conversion via worksheet rows.
#' For DV function use (e.g., by calculate_time_quit_smoking): see smoking-cessation.R.
#'
#' @param SMK_09A_cat4 Numeric vector. Categorical time stopped smoking daily (1-4)
#' @param SMKG09C Numeric vector. Continuous years for category 4 (3+ years)
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Vector of continuous years values
#'
#' @examples
#' \dontrun{
#' harmonized_data <- rec_with_table(cchs_data, "SMK_09A_cont")
#' }
#'
#' @export
calculate_SMK_09A_cont_stub <- function(SMK_09A_cat4, SMKG09C = NULL, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_09A_cont') for passthrough, ",
       "or calculate_SMK_09A_cont() from smoking-cessation.R for DV function use")
}

# ================================================================================

# TIME_QUIT_SMOKING - Combined cessation timeframe - DOCUMENTATION ONLY
# ================================================================================

#' @title Combined Smoking Cessation Timeframe - TIME_QUIT_SMOKING (continuous)
#' @description DOCUMENTATION ONLY - canonical implementation is in smoking-cessation.R.
#'
#' Combines cessation timing from multiple sources with priority logic.
#' This is a combining function (Pattern 2) — it requires the foundational
#' cessation DV functions as inputs.
#'
#' @param SMK_09A_cont Numeric vector. Years since stopped daily smoking
#' @param SMK_06A_cont Numeric vector. Years since stopped smoking (occasional)
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Vector of continuous years since cessation
#'
#' @examples
#' \dontrun{
#' # See calculate_time_quit_smoking() in smoking-cessation.R
#' }
#'
#' @export
calculate_time_quit_smoking_stub <- function(SMK_09A_cont, SMK_06A_cont,
                                             output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use calculate_time_quit_smoking() from smoking-cessation.R")
}