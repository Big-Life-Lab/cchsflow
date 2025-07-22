# ==============================================================================
# BMI Functions - Configuration-Driven Architecture
# ==============================================================================
#
# Refactored BMI functions following the configuration-driven missing data
# architecture specified in flexible-missing-data-specs.md
#
# @note v3.1.0, refactored: 2025-07-22 (vectorized)
# ==============================================================================

# Source the flexible missing handler
# Note: Relative paths depend on where this is sourced from
# If sourced from QMD, path will be resolved relative to QMD location

#' Calculate BMI Core - Configuration-Driven Version
#'
#' Core BMI calculation using the new configuration-driven missing data handler.
#' This function demonstrates the clean, semantic approach specified in the
#' flexible-missing-data-specs.md document.
#'
#' @param height Height in meters
#' @param weight Weight in kilograms  
#' @param handle_missing_data Missing data handling strategy
#' @return BMI values with proper missing data propagation
#'
#' @examples
#' # With tagged_na data (from rec_with_table)
#' height_tagged <- c(1.75, 1.60, haven::tagged_na("a"))
#' weight_tagged <- c(70, 55, haven::tagged_na("b"))
#' calculate_bmi_core_v3(height_tagged, weight_tagged)
#'
#' # With original numeric codes
#' height_original <- c(1.75, 1.60, 996)
#' weight_original <- c(70, 55, 998)
#' calculate_bmi_core_v3(height_original, weight_original, "original")
#'
#' @export
calculate_bmi_core_v3 <- function(height, weight, handle_missing_data = "auto") {
  # 1. Get the handler for the correct pattern
  handler <- create_missing_handler(
    height, weight,
    handle_missing_data = handle_missing_data,
    pattern_type = "triple_digit_missing"
  )
  
  # 2. TRUE VECTORIZATION using case_when (exactly as recommended in review)
  dplyr::case_when(
    handler$is_missing(height) | handler$is_missing(weight) ~ 
      handler$propagate(height, weight),
    .default = weight / (height^2)
  )
}

#' Calculate BMI - Configuration-Driven Version
#'
#' Standard BMI calculation using configuration-driven architecture.
#' Automatically detects data format and applies appropriate missing data rules.
#'
#' @param HWTGHTM Height in meters (CCHS variable)
#' @param HWTGWTK Weight in kilograms (CCHS variable)
#' @param handle_missing_data Missing data strategy: "auto", "original", "tagged_na"
#' @param log_level Logging level
#' @return BMI values with semantic missing data handling
#'
#' @examples
#' # Auto-detection with mixed data
#' test_data <- data.frame(
#'   HWTGHTM = c(1.75, 1.60, haven::tagged_na("a"), 996, 1.70),
#'   HWTGWTK = c(70, 55, haven::tagged_na("b"), 70, 65)
#' )
#' 
#' bmi_results <- calculate_bmi_v3(test_data$HWTGHTM, test_data$HWTGWTK)
#'
#' @export
calculate_bmi_v3 <- function(HWTGHTM = NULL, 
                            HWTGWTK = NULL,
                            handle_missing_data = "auto",
                            log_level = "silent") {
  
  # Use the new configuration-driven approach
  calculate_bmi_core_v3(HWTGHTM, HWTGWTK, handle_missing_data)
}

#' Adjust BMI - Configuration-Driven Version
#'
#' Bias-corrected BMI calculation using configuration-driven architecture.
#' Applies sex-specific corrections with semantic missing data handling.
#'
#' @param DHH_SEX Sex variable (1=male, 2=female)
#' @param HWTGHTM Height in meters
#' @param HWTGWTK Weight in kilograms
#' @param handle_missing_data Missing data strategy
#' @param log_level Logging level
#' @param male_correction Male correction coefficients
#' @param female_correction Female correction coefficients
#' @return Bias-corrected BMI with semantic missing data handling
#'
#' @examples
#' # Auto-detection example
#' test_data <- data.frame(
#'   DHH_SEX = c(1, 2, haven::tagged_na("a"), 6, 1),
#'   HWTGHTM = c(1.75, 1.65, 1.80, haven::tagged_na("b"), 1.70),
#'   HWTGWTK = c(70, 60, 80, 70, 65)
#' )
#' 
#' corrected_bmi <- adjust_bmi_v3(
#'   test_data$DHH_SEX, 
#'   test_data$HWTGHTM, 
#'   test_data$HWTGWTK
#' )
#'
#' @export
adjust_bmi_v3 <- function(DHH_SEX = NULL,
                         HWTGHTM = NULL,
                         HWTGWTK = NULL,
                         handle_missing_data = "auto",
                         log_level = "silent",
                         male_correction = list(intercept = -1.07575, slope = 1.07592),
                         female_correction = list(intercept = -0.12374, slope = 1.05129)) {
  
  # 1. Get handlers for different pattern types
  anthro_handler <- create_missing_handler(
    HWTGHTM, HWTGWTK,
    handle_missing_data = handle_missing_data,
    pattern_type = "triple_digit_missing"
  )
  
  sex_handler <- create_missing_handler(
    DHH_SEX,
    handle_missing_data = handle_missing_data,
    pattern_type = "single_digit_missing"
  )
  
  # 2. TRUE VECTORIZATION using case_when (exactly as recommended in review)
  # Pre-calculate raw BMI for all cases (fully vectorized)
  raw_bmi <- HWTGWTK / (HWTGHTM^2)
  
  # Fully vectorized case_when approach - handlers work directly on vectors
  dplyr::case_when(
    # Missing data cases - use the vectorized propagate function
    anthro_handler$is_missing(HWTGHTM) | anthro_handler$is_missing(HWTGWTK) | 
    sex_handler$is_missing(DHH_SEX) ~ 
      anthro_handler$propagate(HWTGHTM, HWTGWTK, DHH_SEX),
    
    # Male correction (fully vectorized)
    DHH_SEX == 1 ~ male_correction$intercept + male_correction$slope * raw_bmi,
    
    # Female correction (fully vectorized)
    DHH_SEX == 2 ~ female_correction$intercept + female_correction$slope * raw_bmi,
    
    # Invalid sex values - propagate the missing sex value
    .default = sex_handler$propagate(DHH_SEX)
  )
}