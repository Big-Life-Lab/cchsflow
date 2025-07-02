# ==============================================================================
# Enhanced BMI Functions - Following v3.0.0 Development Guide
# ==============================================================================

# REQUIRED DEPENDENCIES:
# Before sourcing this file, ensure these packages are loaded:
#   library(haven)   # for haven::tagged_na() and haven::is_tagged_na()
#   library(dplyr)   # for dplyr::case_when() and dplyr::if_else()
#   source("R/missing-data-helpers.R")  # for preprocessing functions
#
# For testing, run:
#   library(haven); library(dplyr); library(testthat)
#   source('R/bmi-enhanced.R')
#   test_file('tests/testthat/test-bmi-enhanced.R')

# ==============================================================================
# 1. CONSTANTS AND CONFIGURATION
# ==============================================================================

# BMI correction coefficients from Connor Gorber et al. 2008
BMI_CORRECTION_MALE <- list(intercept = -1.07575, slope = 1.07592)
BMI_CORRECTION_FEMALE <- list(intercept = -0.12374, slope = 1.05129)

# Default validation bounds (derived from CCHS documentation)
BMI_VALIDATION_BOUNDS <- list(
  height = list(min = 0.82, max = 2.50),    # meters
  weight = list(min = 22.7, max = 209.1),   # kilograms (updated for master files)
  bmi = list(min = 10, max = 100)           # BMI units
)

# ==============================================================================
# 2. CORE UTILITY FUNCTIONS (Enhanced)
# ==============================================================================

#' Enhanced input validation for BMI functions
#' @param param_name Parameter name for error messages
#' @param param_value Parameter value to validate
#' @param required Logical indicating if parameter is required
#' @return Validated parameter or error
#' @noRd
validate_bmi_parameter <- function(param_name, param_value, required = TRUE) {
  if (required && missing(param_value)) {
    stop(paste("Required parameter", param_name, "must be provided"), call. = FALSE)
  }
  return(param_value)
}

#' Check vector length compatibility for BMI inputs
#' @param ... Input vectors to check
#' @return Logical indicating compatibility
#' @noRd
check_bmi_length_compatibility <- function(...) {
  lengths <- lengths(list(...))
  # All lengths must be equal OR some can be length 1 (scalar)
  return(length(unique(lengths[lengths > 1])) <= 1)
}

#' Auto-detect if BMI input needs preprocessing
#' @param input_var Input variable to check
#' @return Logical indicating if preprocessing needed
#' @noRd
needs_preprocessing <- function(input_var) {
  any(input_var %in% c(6, 7, 8, 9, 96:99, 996:999), na.rm = TRUE) ||
    any(is.character(input_var) & input_var %in% 
        c("Not applicable", "Missing", "Don't know"), na.rm = TRUE)
}

# ==============================================================================
# 3. SPECIALIZED HELPER FUNCTIONS (Enhanced)
# ==============================================================================

#' Enhanced height/weight validation with comprehensive checks
#' @param height Height values (preprocessed)
#' @param weight Weight values (preprocessed) 
#' @param min_height,max_height,min_weight,max_weight Validation bounds
#' @return Logical vector indicating valid measurements
#' @noRd
validate_height_weight_enhanced <- function(height, weight, 
                                           min_height, max_height,
                                           min_weight, max_weight) {
  dplyr::case_when(
    # Handle tagged NAs first (preserve semantics)
    haven::is_tagged_na(height, "a") | haven::is_tagged_na(weight, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(height, "b") | haven::is_tagged_na(weight, "b") ~ haven::tagged_na("b"),
    
    # Handle regular NAs
    is.na(height) | is.na(weight) ~ haven::tagged_na("b"),
    
    # Validation bounds
    height < min_height | height > max_height ~ haven::tagged_na("b"),
    weight < min_weight | weight > max_weight ~ haven::tagged_na("b"),
    
    # Valid measurements
    .default = as.numeric(height)  # Return height as indicator of validity
  )
}

#' Enhanced BMI calculation with comprehensive missing data handling
#' @param height Height in meters (preprocessed)
#' @param weight Weight in kilograms (preprocessed)
#' @param min_height,max_height,min_weight,max_weight,bmi_min,bmi_max Validation parameters
#' @return BMI values with complete validation
#' @noRd
calculate_bmi_enhanced <- function(height, weight, 
                                  min_height, max_height,
                                  min_weight, max_weight,
                                  bmi_min, bmi_max) {
  
  # Enhanced validation preserving tagged NA semantics
  bmi_value <- dplyr::case_when(
    # Preserve input tagged NAs first
    haven::is_tagged_na(height, "a") | haven::is_tagged_na(weight, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(height, "b") | haven::is_tagged_na(weight, "b") ~ haven::tagged_na("b"),
    
    # Handle regular NAs
    is.na(height) | is.na(weight) ~ haven::tagged_na("b"),
    
    # Validation bounds
    height < min_height | height > max_height ~ haven::tagged_na("b"),
    weight < min_weight | weight > max_weight ~ haven::tagged_na("b"),
    
    # Valid calculation
    .default = weight / (height^2)
  )
  
  # Final BMI validation
  dplyr::case_when(
    haven::is_tagged_na(bmi_value, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(bmi_value, "b") ~ haven::tagged_na("b"),
    is.na(bmi_value) ~ haven::tagged_na("b"),
    bmi_value < bmi_min | bmi_value > bmi_max ~ haven::tagged_na("b"),
    .default = bmi_value
  )
}

# ==============================================================================
# 4. PUBLIC API FUNCTIONS (Enhanced with v3.0.0 Patterns)
# ==============================================================================

#' Calculate Body Mass Index (BMI) with comprehensive validation
#'
#' @description 
#' Calculate harmonized BMI across CCHS cycles using the standard formula BMI = weight(kg) / height(m)^2.
#' Includes comprehensive validation, outlier detection, and missing data preprocessing following WHO guidelines.
#'
#' @param HWTGHTM CCHS variable for height (in meters). Accepts raw CCHS codes or preprocessed values.
#' @param HWTGWTK CCHS variable for weight (in kilograms). Accepts raw CCHS codes or preprocessed values.
#' @param min_HWTGHTM Minimum valid height in meters (default 0.82)
#' @param max_HWTGHTM Maximum valid height in meters (default 2.50)
#' @param min_HWTGWTK Minimum valid weight in kilograms (default 22.7)
#' @param max_HWTGWTK Maximum valid weight in kilograms (default 209.1)
#' @param BMI_min Minimum valid BMI (default 10)
#' @param BMI_max Maximum valid BMI (default 100)
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#'
#' @return Numeric vector of BMI values. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (age restrictions)
#'     \item \code{haven::tagged_na("b")} for missing/invalid measurements
#'   }
#'   Valid range typically 10-100 for realistic measurements.
#'
#' @details
#' This function uses harmonized height/weight variables and may produce different results 
#' than original CCHS derived BMI variables which had cycle-specific validation rules.
#' Enhanced validation bounds are optimized for master file data and may be more restrictive 
#' than PUMF versions.
#' 
#' Calculation method: BMI = weight(kg) / height(m)^2. Original CCHS codes (6,7,8,9) are 
#' automatically converted to haven::tagged_na(). Validation bounds: Height [0.82-2.50m], 
#' Weight [22.7-209.1kg], BMI [10-100]. Extreme values are marked as haven::tagged_na("b").
#'
#' @examples
#' # Standard cchsflow workflow (primary usage)
#' library(cchsflow)
#' result <- rec_with_table(cchs2013_2014_p, 
#'                         c("HWTGHTM", "HWTGWTK", "HWTGBMI_der"))
#'
#' # Basic usage
#' calculate_bmi(1.75, 70)
#'
#' # Vector processing with missing data
#' calculate_bmi(c(1.75, 1.80, 6), c(70, 75, 7))
#'
#' # Research mode with custom validation
#' calculate_bmi(1.75, 70, min_HWTGHTM = 1.0, max_HWTGHTM = 2.0)
#'
#' @seealso
#' \code{\link{adjust_bmi}} for bias-corrected BMI calculations
#' \code{\link{categorize_bmi}} for BMI category classification
#'
#' @references
#' World Health Organization. (2000). Obesity: preventing and managing the global epidemic. 
#' Report of a WHO consultation. World Health Organization technical report series, 894, i-xii.
#' 
#' Connor Gorber, S., et al. (2008). The accuracy of self-reported height and weight 
#' in a nationally representative sample of Canadian adults. Obesity, 16(10), 2326-2332.
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced with comprehensive preprocessing and validation following development guide
#' @export
calculate_bmi <- function(HWTGHTM, HWTGWTK, 
                        min_HWTGHTM = BMI_VALIDATION_BOUNDS$height$min, 
                        max_HWTGHTM = BMI_VALIDATION_BOUNDS$height$max,
                        min_HWTGWTK = BMI_VALIDATION_BOUNDS$weight$min, 
                        max_HWTGWTK = BMI_VALIDATION_BOUNDS$weight$max,
                        BMI_min = BMI_VALIDATION_BOUNDS$bmi$min, 
                        BMI_max = BMI_VALIDATION_BOUNDS$bmi$max,
                        validate_params = NULL) {
  
  # 1. Enhanced input validation
  validate_bmi_parameter("HWTGHTM", HWTGHTM, required = TRUE)
  validate_bmi_parameter("HWTGWTK", HWTGWTK, required = TRUE)
  
  # 2. Length compatibility check
  if (!check_bmi_length_compatibility(HWTGHTM, HWTGWTK)) {
    stop("Input vectors must have compatible lengths (equal or one can be scalar)", call. = FALSE)
  }
  
  # 3. Type validation with informative messages
  if (!is.numeric(HWTGHTM) && !needs_preprocessing(HWTGHTM)) {
    warning("HWTGHTM contains unexpected values. Expected: numeric, CCHS codes, or character NAs", 
            call. = FALSE)
  }
  
  # 4. Auto-detect validation mode
  if (is.null(validate_params)) {
    validate_params <- !all(c(min_HWTGHTM, max_HWTGHTM, min_HWTGWTK, max_HWTGWTK, 
                              BMI_min, BMI_max) == 
                           c(BMI_VALIDATION_BOUNDS$height$min, BMI_VALIDATION_BOUNDS$height$max,
                             BMI_VALIDATION_BOUNDS$weight$min, BMI_VALIDATION_BOUNDS$weight$max,
                             BMI_VALIDATION_BOUNDS$bmi$min, BMI_VALIDATION_BOUNDS$bmi$max))
  }
  
  # 5. Handle edge cases gracefully  
  if (all(is.na(HWTGHTM)) || all(is.na(HWTGWTK))) {
    warning("All input values are missing. Results will be entirely NA.", call. = FALSE)
  }
  
  # 6. Preprocess missing data (dual input handling)
  if (needs_preprocessing(HWTGHTM)) {
    HWTGHTM <- preprocess_continuous_standard(HWTGHTM)
  }
  if (needs_preprocessing(HWTGWTK)) {
    HWTGWTK <- preprocess_continuous_standard(HWTGWTK)
  }
  
  # 7. Enhanced BMI calculation with validation
  calculate_bmi_enhanced(HWTGHTM, HWTGWTK, 
                        min_HWTGHTM, max_HWTGHTM,
                        min_HWTGWTK, max_HWTGWTK,
                        BMI_min, BMI_max)
}

#' Calculate bias-corrected BMI for self-reported data
#'
#' @description 
#' Calculate bias-corrected BMI accounting for self-reporting errors in height and weight measurements.
#' Applies sex-specific correction coefficients from Connor Gorber et al. 2008 for more accurate 
#' BMI estimates in population health research using self-reported data.
#'
#' @param DHH_SEX CCHS variable for sex (1=male, 2=female). Accepts raw CCHS codes or preprocessed values.
#' @param HWTGHTM CCHS variable for height (in meters). Accepts raw CCHS codes or preprocessed values.
#' @param HWTGWTK CCHS variable for weight (in kilograms). Accepts raw CCHS codes or preprocessed values.
#' @param min_HWTGHTM,max_HWTGHTM,min_HWTGWTK,max_HWTGWTK,BMI_min,BMI_max Validation parameters
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#'
#' @return Numeric vector of bias-corrected BMI values. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases
#'     \item \code{haven::tagged_na("b")} for missing sex or invalid measurements
#'   }
#'   Valid range typically 10-100 for corrected measurements.
#'
#' @details
#' Correction coefficients are based on Canadian data and may not be appropriate for other populations.
#' Enhanced validation may produce different results than legacy adjusted BMI functions.
#' 
#' Calculation formulas: Males: Adjusted BMI = -1.07575 + 1.07592 × Raw BMI; 
#' Females: Adjusted BMI = -0.12374 + 1.05129 × Raw BMI. Original CCHS codes (6,7,8,9) are 
#' comprehensively preprocessed. Validation uses same bounds as basic BMI with post-correction validation.
#'
#' @examples
#' # Standard cchsflow workflow (primary usage)
#' library(cchsflow)
#' result <- rec_with_table(cchs2013_2014_p, 
#'                         c("DHH_SEX", "HWTGHTM", "HWTGWTK", "HWTGBMI_adjusted_der"))
#'
#' # Basic usage
#' adjust_bmi(1, 1.75, 70)  # Male
#' adjust_bmi(2, 1.65, 60)  # Female
#'
#' # Vector processing with missing data
#' adjust_bmi(c(1, 2, 6), c(1.75, 1.65, 1.80), c(70, 60, 7))
#'
#' @references
#' Connor Gorber, S., et al. (2008). The accuracy of self-reported height and weight 
#' in a nationally representative sample of Canadian adults. Obesity, 16(10), 2326-2332.
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced with comprehensive preprocessing and sex-specific bias correction
#' @export
adjust_bmi <- function(DHH_SEX, HWTGHTM, HWTGWTK, 
                                 min_HWTGHTM = BMI_VALIDATION_BOUNDS$height$min, 
                                 max_HWTGHTM = BMI_VALIDATION_BOUNDS$height$max,
                                 min_HWTGWTK = BMI_VALIDATION_BOUNDS$weight$min, 
                                 max_HWTGWTK = BMI_VALIDATION_BOUNDS$weight$max,
                                 BMI_min = BMI_VALIDATION_BOUNDS$bmi$min, 
                                 BMI_max = BMI_VALIDATION_BOUNDS$bmi$max,
                                 validate_params = NULL) {
  
  # 1. Enhanced input validation
  validate_bmi_parameter("DHH_SEX", DHH_SEX, required = TRUE)
  validate_bmi_parameter("HWTGHTM", HWTGHTM, required = TRUE)
  validate_bmi_parameter("HWTGWTK", HWTGWTK, required = TRUE)
  
  # 2. Length compatibility check
  if (!check_bmi_length_compatibility(DHH_SEX, HWTGHTM, HWTGWTK)) {
    stop("Input vectors must have compatible lengths", call. = FALSE)
  }
  
  # 3. Preprocess missing data for all inputs
  if (needs_preprocessing(DHH_SEX)) {
    DHH_SEX <- preprocess_standard_response(DHH_SEX)
  }
  if (needs_preprocessing(HWTGHTM)) {
    HWTGHTM <- preprocess_continuous_standard(HWTGHTM)
  }
  if (needs_preprocessing(HWTGWTK)) {
    HWTGWTK <- preprocess_continuous_standard(HWTGWTK)
  }
  
  # 4. Calculate raw BMI first
  raw_bmi <- dplyr::case_when(
    # Preserve tagged NAs from any input
    haven::is_tagged_na(DHH_SEX, "a") | haven::is_tagged_na(HWTGHTM, "a") | haven::is_tagged_na(HWTGWTK, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(DHH_SEX, "b") | haven::is_tagged_na(HWTGHTM, "b") | haven::is_tagged_na(HWTGWTK, "b") ~ haven::tagged_na("b"),
    
    # Handle regular NAs
    is.na(DHH_SEX) | is.na(HWTGHTM) | is.na(HWTGWTK) ~ haven::tagged_na("b"),
    
    # Validation bounds
    HWTGHTM < min_HWTGHTM | HWTGHTM > max_HWTGHTM ~ haven::tagged_na("b"),
    HWTGWTK < min_HWTGWTK | HWTGWTK > max_HWTGWTK ~ haven::tagged_na("b"),
    
    # Valid BMI calculation
    .default = HWTGWTK / (HWTGHTM^2)
  )
  
  # 5. Apply sex-specific correction
  adjusted_bmi <- dplyr::case_when(
    # Preserve tagged NAs
    haven::is_tagged_na(raw_bmi, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(raw_bmi, "b") ~ haven::tagged_na("b"),
    is.na(raw_bmi) ~ haven::tagged_na("b"),
    
    # Invalid sex codes
    !(DHH_SEX %in% c(1, 2)) ~ haven::tagged_na("b"),
    
    # Sex-specific corrections
    DHH_SEX == 1 ~ BMI_CORRECTION_MALE$intercept + BMI_CORRECTION_MALE$slope * raw_bmi,
    DHH_SEX == 2 ~ BMI_CORRECTION_FEMALE$intercept + BMI_CORRECTION_FEMALE$slope * raw_bmi,
    
    .default = haven::tagged_na("b")
  )
  
  # 6. Final adjusted BMI validation
  dplyr::case_when(
    haven::is_tagged_na(adjusted_bmi, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(adjusted_bmi, "b") ~ haven::tagged_na("b"),
    is.na(adjusted_bmi) ~ haven::tagged_na("b"),
    adjusted_bmi < BMI_min | adjusted_bmi > BMI_max ~ haven::tagged_na("b"),
    .default = adjusted_bmi
  )
}

#' Categorize BMI values using WHO standard categories
#'
#' @description 
#' Categorize BMI values into WHO standard obesity categories with comprehensive missing data handling.
#' Uses international BMI categories: Underweight (<18.5), Normal (18.5-24.9), Overweight (25.0-29.9), 
#' Obese (≥30.0) for standard population health assessment.
#'
#' @param bmi_value BMI values (from calculate_bmi or adjust_bmi). Accepts raw values or preprocessed.
#' @param categorical_labels Return factor labels instead of numeric codes (default TRUE)
#'
#' @return Factor with BMI category labels or numeric codes. Missing data handled as:
#'   \itemize{
#'     \item Preserves \code{haven::tagged_na("a")} for not applicable cases
#'     \item Returns \code{haven::tagged_na("b")} for missing/invalid BMI values
#'   }
#'   Categories: Underweight, Normal weight, Overweight, Obese.
#'
#' @details
#' BMI categories are not appropriate for children, pregnant women, or certain medical conditions.
#' WHO standards are used consistently, but interpretation may vary across health guidelines and time periods.
#' 
#' Categorization thresholds: Underweight (BMI < 18.5), Normal weight (18.5 ≤ BMI < 25.0), 
#' Overweight (25.0 ≤ BMI < 30.0), Obese (BMI ≥ 30.0). Comprehensive preprocessing is applied 
#' if raw BMI values are provided.
#'
#' @examples
#' # Standard cchsflow workflow (primary usage)
#' library(cchsflow)
#' result <- rec_with_table(cchs2013_2014_p, 
#'                         c("HWTGHTM", "HWTGWTK", "HWTGBMI_cat_der"))
#'
#' # Basic categorization
#' categorize_bmi(c(17, 22, 27, 32))
#'
#' # With missing data
#' categorize_bmi(c(22, haven::tagged_na("a"), 27, 997))
#'
#' # Numeric codes instead of labels
#' categorize_bmi(c(17, 22, 27, 32), categorical_labels = FALSE)
#'
#' @references
#' World Health Organization. (2000). Obesity: preventing and managing the global epidemic. 
#' WHO Technical Report Series, 894.
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced categorization with comprehensive missing data handling
#' @export
categorize_bmi <- function(bmi_value, categorical_labels = TRUE) {
  
  # 1. Input validation
  validate_bmi_parameter("bmi_value", bmi_value, required = TRUE)
  
  # 2. Preprocess if needed
  if (needs_preprocessing(bmi_value)) {
    bmi_value <- preprocess_continuous_standard(bmi_value)
  }
  
  # 3. Categorize BMI with comprehensive missing data handling
  if (categorical_labels) {
    # Return character labels
    bmi_category <- dplyr::case_when(
      # Preserve tagged NAs with explicit checks
      haven::is_tagged_na(bmi_value, "a") ~ "NA(a)",
      haven::is_tagged_na(bmi_value, "b") ~ "NA(b)",
      
      # Handle regular NAs
      is.na(bmi_value) ~ "NA(b)",
      
      # BMI categories (WHO classification)
      bmi_value < 18.5 ~ "Underweight",
      bmi_value >= 18.5 & bmi_value < 25.0 ~ "Normal weight",
      bmi_value >= 25.0 & bmi_value < 30.0 ~ "Overweight",
      bmi_value >= 30.0 ~ "Obese",
      
      # Catch-all for any remaining edge cases
      .default = "NA(b)"
    )
  } else {
    # Return numeric codes
    bmi_category <- dplyr::case_when(
      # Preserve tagged NAs with explicit checks
      haven::is_tagged_na(bmi_value, "a") ~ haven::tagged_na("a"),
      haven::is_tagged_na(bmi_value, "b") ~ haven::tagged_na("b"),
      
      # Handle regular NAs
      is.na(bmi_value) ~ haven::tagged_na("b"),
      
      # BMI categories (WHO classification)
      bmi_value < 18.5 ~ 1L,
      bmi_value >= 18.5 & bmi_value < 25.0 ~ 2L,
      bmi_value >= 25.0 & bmi_value < 30.0 ~ 3L,
      bmi_value >= 30.0 ~ 4L,
      
      # Catch-all for any remaining edge cases
      .default = haven::tagged_na("b")
    )
  }
  
  return(bmi_category)
}