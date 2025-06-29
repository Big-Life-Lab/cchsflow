# ==============================================================================
# BMI Functions - Reorganized Structure
# ==============================================================================

# REQUIRED DEPENDENCIES:
# Before sourcing this file, ensure these packages are loaded:
#   library(haven)   # for haven::tagged_na() and haven::is_tagged_na()
#   library(dplyr)   # for dplyr::case_when() and dplyr::if_else()
#
# Package loading order matters! Load haven and dplyr BEFORE sourcing this file.
#
# For testing, run:
#   library(haven); library(dplyr); library(testthat)
#   source('R/bmi.R')
#   test_file('tests/testthat/test-bmi.R')

# ==============================================================================
# 1. CONSTANTS AND CONFIGURATION
# ==============================================================================

# BMI correction coefficients from Connor Gorber et al. 2008
BMI_CORRECTION_MALE <- list(intercept = -1.07575, slope = 1.07592)
BMI_CORRECTION_FEMALE <- list(intercept = -0.12374, slope = 1.05129)

# ==============================================================================
# 2. CORE UTILITY FUNCTIONS
# ==============================================================================

#' Calculate raw BMI from height and weight
#' @param height Height in meters
#' @param weight Weight in kilograms
#' @return BMI value (weight/height^2)
#' @noRd
calculate_raw_bmi <- function(height, weight) {
  weight / (height * height)
}

#' Validate height and weight inputs
#' @param height Height in meters
#' @param weight Weight in kilograms
#' @param min_height Minimum valid height
#' @param max_height Maximum valid height
#' @param min_weight Minimum valid weight
#' @param max_weight Maximum valid weight
#' @return Logical vector indicating valid measurements
#' @noRd
validate_height_weight <- function(height, weight, min_height, max_height,
                                   min_weight, max_weight) {
  (!is.na(height)) & (!is.na(weight)) &
    height > min_height & height < max_height &
    weight > min_weight & weight < max_weight
}

#' Validate pregnancy status and exclude pregnant individuals
#' @param pregnancy_status Pregnancy variable (0=not pregnant, 1=pregnant)
#' @return Logical indicating if individual should be included (not pregnant)
#' @noRd
validate_pregnancy_status <- function(pregnancy_status) {
  !is.na(pregnancy_status) & pregnancy_status != 1
}

# ==============================================================================
# 3. SPECIALIZED HELPER FUNCTIONS
# ==============================================================================

#' Apply BMI outlier detection and capping
#' @param bmi BMI values
#' @param bmi_min Minimum valid BMI
#' @param bmi_max Maximum valid BMI
#' @return BMI values with outliers capped
#' @noRd
apply_bmi_outlier_detection <- function(bmi, bmi_min, bmi_max) {
  dplyr::case_when(
    is.na(bmi) ~ haven::tagged_na("b"),
    bmi < bmi_min ~ bmi_min,
    bmi > bmi_max ~ bmi_max,
    .default = bmi
  )
}

#' Apply sex-specific BMI correction
#' @param raw_bmi Raw BMI values
#' @param sex Sex variable (1=male, 2=female)
#' @param bmi_min Minimum valid BMI for outlier detection
#' @param bmi_max Maximum valid BMI for outlier detection
#' @return Corrected BMI values
#' @noRd
apply_bmi_correction <- function(raw_bmi, sex, bmi_min, bmi_max) {
  dplyr::case_when(
    is.na(raw_bmi) | is.na(sex) ~ haven::tagged_na("b"),
    sex == 1 ~ apply_bmi_outlier_detection(
      BMI_CORRECTION_MALE$intercept + BMI_CORRECTION_MALE$slope * raw_bmi,
      bmi_min, bmi_max
    ),
    sex == 2 ~ apply_bmi_outlier_detection(
      BMI_CORRECTION_FEMALE$intercept + BMI_CORRECTION_FEMALE$slope * raw_bmi,
      bmi_min, bmi_max
    ),
    .default = haven::tagged_na("b")
  )
}

#' Calculate validated BMI with outlier detection
#' @param height Height in meters
#' @param weight Weight in kilograms
#' @param min_height Minimum valid height
#' @param max_height Maximum valid height
#' @param min_weight Minimum valid weight
#' @param max_weight Maximum valid weight
#' @param bmi_min Minimum valid BMI
#' @param bmi_max Maximum valid BMI
#' @return BMI values with validation and outlier detection applied
#' @noRd
calculate_validated_bmi <- function(height, weight, min_height, max_height,
                                    min_weight, max_weight, bmi_min, bmi_max) {
  # Use helper function for validation
  is_valid <- validate_height_weight(
    height, weight, min_height, max_height, min_weight, max_weight
  )
  
  dplyr::case_when(
    !is_valid ~ haven::tagged_na("b"),
    .default = apply_bmi_outlier_detection(
      calculate_raw_bmi(height, weight), bmi_min, bmi_max
    )
  )
}

# ==============================================================================
# 4. PUBLIC API FUNCTIONS
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.1 Basic BMI Functions (PUMF version and Master/Shared version)
# ------------------------------------------------------------------------------

#' @title Body Mass Index (BMI) derived variable
#' @description This function creates a harmonized BMI variable across CCHS cycles.
#' @param HWTGHTM CCHS variable for height (in meters)
#' @param HWTGWTK CCHS variable for weight (in kilograms)
#' @param min_HWTGHTM,max_HWTGHTM,min_HWTGWTK,max_HWTGWTK,BMI_min,BMI_max Validation parameters
#' @return numeric value for BMI. Returns tagged_na("b") for invalid measurements.
#' @export
bmi_fun <-
  function(HWTGHTM, HWTGWTK, min_HWTGHTM = 0.82, max_HWTGHTM = 2.50,
           min_HWTGWTK = 25.0, max_HWTGWTK = 145.0, BMI_min = 8.07,
           BMI_max = 137.46) {
    calculate_validated_bmi(
      HWTGHTM, HWTGWTK, min_HWTGHTM, max_HWTGHTM,
      min_HWTGWTK, max_HWTGWTK, BMI_min, BMI_max
    )
  }

#' @title Body Mass Index (BMI) derived variable (Master/Shared versions)
#' @description BMI calculation with pregnancy status handling for master/shared data versions.
#' @param MAM_037 CCHS pregnancy status variable (0=not pregnant, 1=pregnant)
#' @param HWTDHTM CCHS variable for height (in meters)
#' @param HWTDWTK CCHS variable for weight (in kilograms)
#' @param min_HWTGHTM,max_HWTGHTM,min_HWTGWTK,max_HWTGWTK,BMI_min,BMI_max Validation parameters
#' @return numeric value for BMI. Returns tagged_na("b") for pregnant individuals or invalid measurements.
#' @export
bmi_fun_D <-
  function(MAM_037, HWTDHTM, HWTDWTK, min_HWTGHTM = 0.82,
           max_HWTGHTM = 2.50, min_HWTGWTK = 25.0, max_HWTGWTK = 145.0,
           BMI_min = 8.07, BMI_max = 137.46) {
    # Handle pregnancy status and BMI calculation
    dplyr::case_when(
      is.na(MAM_037) ~ haven::tagged_na("b"), # Missing pregnancy status
      MAM_037 == 1 ~ haven::tagged_na("b"), # Pregnant - excluded
      .default = calculate_validated_bmi(
        HWTDHTM, HWTDWTK, min_HWTGHTM, max_HWTGHTM,
        min_HWTGWTK, max_HWTGWTK, BMI_min, BMI_max
      )
    )
  }

# ------------------------------------------------------------------------------
# 4.2 Adjusted BMI Functions (with bias correction)
# ------------------------------------------------------------------------------

#' @title Adjusted Body Mass Index (BMI) derived variable
#' @description BMI with sex-specific corrections for self-reporting bias.
#' @param DHH_SEX CCHS variable for sex (1=male, 2=female)
#' @param HWTGHTM CCHS variable for height (in meters)  
#' @param HWTGWTK CCHS variable for weight (in kilograms)
#' @param min_HWTGHTM,max_HWTGHTM,min_HWTGWTK,max_HWTGWTK,BMI_min,BMI_max Validation parameters
#' @return numeric value for adjusted BMI. Returns tagged_na("b") for invalid measurements.
#' @export
adjusted_bmi_fun <-
  function(DHH_SEX, HWTGHTM, HWTGWTK, min_HWTGHTM = 0.82,
           max_HWTGHTM = 2.50, min_HWTGWTK = 25.0, max_HWTGWTK = 145.0,
           BMI_min = 8.07, BMI_max = 137.46) {
    # Use helper function for height/weight validation, then apply sex-specific correction
    is_valid <- validate_height_weight(
      HWTGHTM, HWTGWTK, min_HWTGHTM, max_HWTGHTM, min_HWTGWTK, max_HWTGWTK
    )
    
    dplyr::case_when(
      is.na(DHH_SEX) ~ haven::tagged_na("b"),
      !is_valid ~ haven::tagged_na("b"),
      .default = apply_bmi_correction(
        calculate_raw_bmi(HWTGHTM, HWTGWTK), DHH_SEX, BMI_min, BMI_max
      )
    )
  }

#' @title Adjusted BMI derived variable (Master/Shared versions)
#' @description Adjusted BMI with pregnancy status handling for master/shared data versions.
#' @param MAM_037 CCHS pregnancy status variable (0=not pregnant, 1=pregnant)
#' @param DHH_SEX CCHS variable for sex (1=male, 2=female)
#' @param HWTDHTM CCHS variable for height (in meters)
#' @param HWTDWTK CCHS variable for weight (in kilograms)
#' @param min_HWTGHTM,max_HWTGHTM,min_HWTGWTK,max_HWTGWTK,BMI_min,BMI_max Validation parameters
#' @return numeric value for adjusted BMI. Returns tagged_na("b") for pregnant individuals or invalid measurements.
#' @export
adjusted_bmi_fun_D <-
  function(MAM_037, DHH_SEX, HWTDHTM, HWTDWTK, min_HWTGHTM = 0.82,
           max_HWTGHTM = 2.50, min_HWTGWTK = 25.0, max_HWTGWTK = 145.0,
           BMI_min = 8.07, BMI_max = 137.46) {
    # Use helper function for height/weight validation
    is_valid <- validate_height_weight(
      HWTDHTM, HWTDWTK, min_HWTGHTM, max_HWTGHTM, min_HWTGWTK, max_HWTGWTK
    )
    
    # Validation with pregnancy, sex, and measurement checks
    dplyr::case_when(
      is.na(MAM_037) ~ haven::tagged_na("b"), # Missing pregnancy status
      MAM_037 == 1 ~ haven::tagged_na("b"), # Pregnant - excluded
      is.na(DHH_SEX) ~ haven::tagged_na("b"), # Missing sex
      !is_valid ~ haven::tagged_na("b"), # Invalid height/weight
      .default = apply_bmi_correction(
        calculate_raw_bmi(HWTDHTM, HWTDWTK), DHH_SEX, BMI_min, BMI_max
      )
    )
  }

# ------------------------------------------------------------------------------
# 4.3 BMI Categorization Functions
# ------------------------------------------------------------------------------

#' @title Categorical BMI (international standard)
#' @description Categorizes BMI into 4 standard categories.
#' @param HWTGBMI_der derived BMI variable
#' @return BMI category (1=underweight, 2=normal, 3=overweight, 4=obese)
#' @export
bmi_fun_cat <-
  function(HWTGBMI_der) {
    # Categorize BMI using international standards
    dplyr::case_when(
      HWTGBMI_der < 18.5 ~ 1L, # Underweight
      HWTGBMI_der < 25.0 ~ 2L, # Normal weight (18.5 ≤ BMI < 25)
      HWTGBMI_der < 30.0 ~ 3L, # Overweight (25 ≤ BMI < 30)
      HWTGBMI_der >= 30.0 ~ 4L, # Obese (BMI ≥ 30)
      haven::is_tagged_na(HWTGBMI_der, "a") ~ haven::tagged_na("a"),
      .default = haven::tagged_na("b")
    )
  }

#' @title Categorical BMI (Master/Shared versions)
#' @description BMI categorization with pregnancy status handling for master/shared data versions.
#' @param MAM_037 CCHS pregnancy status variable (0=not pregnant, 1=pregnant)
#' @param HWTDBMI_der derived BMI variable
#' @return BMI category. Returns tagged_na("b") for pregnant individuals.
#' @export
bmi_fun_cat_D <-
  function(MAM_037, HWTDBMI_der) {
    # Handle pregnancy status and BMI categorization
    dplyr::case_when(
      is.na(MAM_037) ~ haven::tagged_na("b"), # Missing pregnancy status
      MAM_037 == 1 ~ haven::tagged_na("b"), # Pregnant - excluded
      HWTDBMI_der < 18.5 ~ 1L, # Underweight
      HWTDBMI_der < 25.0 ~ 2L, # Normal weight
      HWTDBMI_der < 30.0 ~ 3L, # Overweight
      HWTDBMI_der >= 30.0 ~ 4L, # Obese
      haven::is_tagged_na(HWTDBMI_der, "a") ~ haven::tagged_na("a"),
      .default = haven::tagged_na("b")
    )
  }