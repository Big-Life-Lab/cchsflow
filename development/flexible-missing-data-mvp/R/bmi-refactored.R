# ==============================================================================
# BMI Functions - Configuration-Driven Architecture
# ==============================================================================
#
# Refactored BMI functions following the configuration-driven missing data
# architecture specified in flexible-missing-data-specs.md
#
# @note v3.8.0, last updated: 2025-07-24 (proper bounds lookup)
# ==============================================================================

# Note: Automatic bounds lookup handled by clean_variables() internally

#' Calculate BMI Core
#'
#' Core BMI calculation using the new configuration-driven missing data handler.
#' This function demonstrates the clean, semantic approach specified in the
#' flexible-missing-data-specs.md document.
#'
#' @param HWTGHTM Height in meters (CCHS variable)
#' @param HWTGWTK Weight in kilograms (CCHS variable)
#' @param output_format Missing data output format
#' @return BMI values with proper missing data propagation
#'
#' @export
calculate_bmi_core <- function(HWTGHTM, HWTGWTK, output_format = "tagged_na") {
  # 1. Clean the variables using original clean_variables from missing-data-helpers.R
  cleaned <- clean_variables(
    continuous_vars = list(HWTGHTM = HWTGHTM, HWTGWTK = HWTGWTK),
    continuous_pattern = "triple_digit_missing"
  )
  
  # 2. Make the missing data handler to allow checks based on the chosen output_format
  check <- handle_missing(
    cleaned$HWTGHTM_clean, cleaned$HWTGWTK_clean,
    output_format = output_format
  )
  
  # 3. Calculate BMI 
  raw_bmi <- dplyr::case_when(
    check$is_missing(cleaned$HWTGHTM_clean) | check$is_missing(cleaned$HWTGWTK_clean) ~ 
      check$which_missing(cleaned$HWTGHTM_clean, cleaned$HWTGWTK_clean),
    .default = cleaned$HWTGWTK_clean / (cleaned$HWTGHTM_clean^2)
  )
  
  # 4. Validate final BMI result using bounds validation
  bmi_validated <- clean_variables(
    continuous_vars = list(HWTGBMI_der = raw_bmi),
    continuous_pattern = "triple_digit_missing"
  )
  
  return(bmi_validated$HWTGBMI_der_clean)
}

#' Calculate BMI
#'
#' Standard BMI calculation using configuration-driven architecture.
#' Automatically detects data format and applies appropriate missing data rules.
#'
#' @param HWTGHTM Height in meters (CCHS variable)
#' @param HWTGWTK Weight in kilograms (CCHS variable)
#' @param output_format Missing data output format: "auto", "original", "tagged_na"
#' @return BMI values with semantic missing data handling
#'
#' @export
calculate_bmi <- function(HWTGHTM = NULL, 
                            HWTGWTK = NULL,
                            output_format = "auto") {
  
  # Use the new configuration-driven approach
  calculate_bmi_core(HWTGHTM, HWTGWTK, output_format)
}

#' Adjust BMI
#'
#' Bias-corrected BMI calculation using configuration-driven architecture.
#' Applies sex-specific corrections with semantic missing data handling.
#'
#' @param DHH_SEX Sex variable (1=male, 2=female)
#' @param HWTGHTM Height in meters
#' @param HWTGWTK Weight in kilograms
#' @param output_format Missing data output format
#' @param male_correction Male correction coefficients
#' @param female_correction Female correction coefficients
#' @return Bias-corrected BMI with semantic missing data handling
#'
#' @export
adjust_bmi <- function(DHH_SEX = NULL,
                         HWTGHTM = NULL,
                         HWTGWTK = NULL,
                         output_format = "tagged_na",
                         male_correction = list(intercept = -1.07575, slope = 1.07592),
                         female_correction = list(intercept = -0.12374, slope = 1.05129)) {

  # 1. Clean the sex variable
  cleaned <- clean_variables(
    categorical_vars = list(DHH_SEX = DHH_SEX),
    categorical_pattern = "single_digit_missing"
  )

  # 2. Calculate raw BMI using the core function
  raw_bmi <- calculate_bmi(HWTGHTM, HWTGWTK, output_format)

  # 3. Get handler for checking sex missingness
  sex_check <- handle_missing(cleaned$DHH_SEX_clean, output_format = output_format)

  # 4. Apply sex-specific bias correction
  corrected_bmi <- dplyr::case_when(
    is.na(raw_bmi) | haven::is_tagged_na(raw_bmi) ~ raw_bmi,
    sex_check$is_missing(cleaned$DHH_SEX_clean) ~ sex_check$which_missing(cleaned$DHH_SEX_clean),
    cleaned$DHH_SEX_clean == 1 ~ male_correction$intercept + male_correction$slope * raw_bmi,
    cleaned$DHH_SEX_clean == 2 ~ female_correction$intercept + female_correction$slope * raw_bmi,
    .default = haven::tagged_na("b")
  )
  
  # 5. Validate final corrected BMI result using bounds validation
  bmi_validated <- clean_variables(
    continuous_vars = list(HWTGCOR_der = corrected_bmi),
    continuous_pattern = "triple_digit_missing"
  )
  
  return(bmi_validated$HWTGCOR_der_clean)
}