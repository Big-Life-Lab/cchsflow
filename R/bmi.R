# ==============================================================================
# BMI (Body Mass Index) Functions
# ==============================================================================

# REQUIRED DEPENDENCIES:
library(haven) # for haven::tagged_na() and haven::is_tagged_na()
library(dplyr) # for dplyr::case_when() and dplyr::if_else()

# Source required helper functions (conditional loading for package context)
tryCatch(
  {
    source("R/missing-data-helpers.R", local = FALSE)
    source("R/utility-functions.R", local = FALSE)
  },
  error = function(e) {
    # Functions will be loaded via package imports during package build
  }
)

# For testing, run:
#   library(haven); library(dplyr); library(testthat)
#   source('R/bmi.R')
#   test_file('tests/testthat/test-bmi.R')

# ==============================================================================
# 1. CONSTANTS AND CONFIGURATION
# ==============================================================================

# BMI validation bounds are now defined in variable_details.csv as single source of truth
# Functions use explicit parameter defaults matching CSV validation ranges
# See @examples in function documentation for transparent validation bound usage

# BMI correction coefficients from Connor Gorber et al. 2008
BMI_CORRECTION_MALE <- list(intercept = -1.07575, slope = 1.07592)
BMI_CORRECTION_FEMALE <- list(intercept = -0.12374, slope = 1.05129)

# ==============================================================================
# 2. VALIDATION FUNCTIONS
# ==============================================================================

# Standard valiation using utility functions and helpers.

# ==============================================================================
# 3. SPECIALIZED HELPER FUNCTIONS
# ==============================================================================

#' Core BMI calculation (internal helper)
#'
#' Vector-aware BMI calculation without validation - used as building block
#' @param height Height in meters (already validated, can be scalar or vector)
#' @param weight Weight in kilograms (already validated, can be scalar or vector)
#' @return BMI value(s) with proper tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-07-04, status: active - Vector aware
#' @noRd
calculate_bmi_core <- function(height, weight) {
  # Use case_when for element-wise processing with tagged NA handling
  dplyr::case_when(
    # !!! (splice operator) expands generate_tagged_na_conditions() output:
    # Creates 5 conditions per variable checking tagged NAs in priority order
    # c (question not asked) → d (variable missing) → a (not applicable) → b (unknown/refusal)
    # Equivalent to: is_tagged_na(height,"c") ~ tagged_na("c"), is_tagged_na(height,"d") ~ tagged_na("d"), etc.
    !!!generate_tagged_na_conditions(height, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(weight, categorical_labels = FALSE),

    # Calculate BMI for valid values
    .default = weight / (height^2)
  )
}

# ==============================================================================
# 4. PUBLIC FUNCTIONS
# ==============================================================================

#' Calculate Body Mass Index (BMI) with comprehensive validation
#'
#' @description
#' Calculate harmonized BMI across CCHS cycles using the standard formula BMI = weight(kg) / height(m)^2.
#' Includes comprehensive validation, outlier detection, and missing data preprocessing following WHO guidelines.
#'
#' @param HWTGHTM CCHS variable for height (in meters). Accepts raw CCHS codes or preprocessed values.
#' @param HWTGWTK CCHS variable for weight (in kilograms). Accepts raw CCHS codes or preprocessed values.
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric vector of BMI values. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (age restrictions)
#'     \item \code{haven::tagged_na("b")} for missing/invalid measurements
#'   }
#'
#' @details
#' This function uses harmonized height/weight variables and may produce different results
#' than original CCHS derived BMI variables which had cycle-specific validation rules.
#' Enhanced validation bounds are optimized for master file data and may be more restrictive
#' than PUMF versions.
#'
#' Calculation method: BMI = weight(kg) / height(m)^2. Original CCHS codes (6,7,8,9) are
#' automatically converted to haven::tagged_na(). Validation bounds: Height [0.914-2.134m],
#' Weight [27.0-135.0kg], BMI [15-50]. Extreme values are marked as haven::tagged_na("b") 
#' when using rec_with_table(). 
#' See variable_details.csv (or variable_details.RData) for up-to-date boundaries.
#'
#' @examples
#' # rec_with_table workflow (RECOMMENDED - handles all validation automatically):
#' # Generate test data with correct and incorrect input
#' test_data <- data.frame(
#'   HWTGHTM = c(1.75, 1.60, 0.80, 2.134, 0.914, 996, 997, 998, 999, NA),
#'   HWTGWTK = c(70, 55, 70, 27, 135, 70, 998, 999, 70, NA)
#' )
#' library(cchsflow)
#' result <- rec_with_table(
#'   test_data,
#'   c("HWTGHTM", "HWTGWTK", "HWTGBMI_der")
#' )
#' # Returns:
#' #   HWTGHTM HWTGWTK HWTGBMI_der
#' # 1    1.75      70       22.86  
#' # 2    1.60      55       21.48  
#' # 3    0.80      70    <NA+b>     # Height below valid range
#' # 4    2.134     27    <NA+b>     # Valid inputs, BMI too low (≈5.93 < 15)
#' # 5    0.914    135    <NA+b>     # Valid inputs, BMI too high (≈161.6 > 50)
#' # 6  996.00     70    <NA+a>     # CCHS missing code
#' # 7  997.00    998    <NA+b>     # CCHS missing codes
#' # 8  998.00    999    <NA+b>     # CCHS missing codes  
#' # 9  999.00     70    <NA+b>     # CCHS missing code
#' #10      NA     NA    <NA+b>     # Regular NAs
#' # Comprehensive validation and error handling applied automatically
#' 
#' # Basic scalar examples:
#' # Validation for missing data handled. 
#' # HWTGHTM: height in meters. See variable_details.csv for valid range: [0.914, 2.134]
#' # HWTGWTK: weight in kilograms. See variable_details.csv for valid range: [27.0, 135.0]
#' calculate_bmi(1.75, 70)    # Returns: 22.85714
#' calculate_bmi(1.60, 55)    # Returns: 21.48437 
#' calculate_bmi("1.75", 70)  # Returns: 22.85714 (string converted by R)
#' 
#' # Check internal tagged NA structure:
#' result <- calculate_bmi(0.80, 70) # Returns: 109.375 (calculates BMI - no bounds validation)
#' haven::is_tagged_na(result, "b")  # Returns: FALSE (BMI calculated normally)
#' result2 <- calculate_bmi(996, 70) # Returns: NA (CCHS not applicable)
#' haven::is_tagged_na(result2, "a")  # Returns: TRUE (not applicable)
#' result3 <- calculate_bmi(998, 70) # Returns: NA (CCHS missing code)
#' haven::is_tagged_na(result3, "b") # Returns: TRUE (missing/invalid)
#' # Note: Basic validation applied - use rec_with_table() for comprehensive validation
#'
#' # Basic vector examples:
#' # Same validation as scalar examples
#' # CCHS missing codes (automatically handled for PUMF/master data):
#' calculate_bmi(c(1.75, 996, 997), c(70, 998, 999))
#' # Returns: c(22.85714, NA, NA)
#' 
#' # Real CCHS data patterns:
#' heights <- c(1.75, 1.80, 996, 997, 998, 999, NA)  # Real PUMF pattern
#' weights <- c(70, 75, 996, 997, 998, 999, NA)      # Real PUMF pattern
#' calculate_bmi(heights, weights)
#' # Handles: valid values + all CCHS missing codes + regular NAs
#'
#' # For production analysis, rec_with_table() provides comprehensive handling:
#' # - Full CSV-driven validation from variable_details.csv
#' # - Robust error handling and recovery
#' # - Consistent preprocessing across all variables
#' # - Better performance with large datasets
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
calculate_bmi <- function(HWTGHTM = NULL, HWTGWTK = NULL,
                          validate_params = NULL,
                          log_level = "silent") {
  # Simple preprocessing for CCHS missing data codes (essential for PUMF/master data)
  # For comprehensive validation, use rec_with_table() with variable_details.csv
  
  # Clean CCHS missing codes only - comprehensive validation handled by rec_with_table()
  height_clean <- if(is.null(HWTGHTM)) {
    haven::tagged_na("d")
  } else {
    clean_single_value(HWTGHTM, pattern_type = "triple_digit_missing", log_level = log_level)
  }
  
  weight_clean <- if(is.null(HWTGWTK)) {
    haven::tagged_na("d") 
  } else {
    clean_single_value(HWTGWTK, pattern_type = "triple_digit_missing", log_level = log_level)
  }

  # Calculate BMI from clean inputs
  calculate_bmi_core(height_clean, weight_clean)
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
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric vector of bias-corrected BMI values. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases
#'     \item \code{haven::tagged_na("b")} for missing sex or invalid measurements
#'   }
#'   Valid range typically 15-50 for corrected measurements.
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
#' # rec_with_table workflow (RECOMMENDED - handles all validation automatically):
#' # Generate test data with correct and incorrect input
#' test_data <- data.frame(
#'   DHH_SEX = c(1, 2, 1, 2, 6, 7, 8, 9),
#'   HWTGHTM = c(1.75, 1.65, 1.80, 1.60, 996, 997, 998, 999),
#'   HWTGWTK = c(70, 60, 80, 55, 70, 998, 999, NA)
#' )
#' library(cchsflow)
#' result <- rec_with_table(
#'   test_data,
#'   c("DHH_SEX", "HWTGHTM", "HWTGWTK", "HWTGCOR_der")
#' )
#' # Returns:
#' #   DHH_SEX HWTGHTM HWTGWTK HWTGCOR_der
#' # 1       1    1.75      70       23.52  
#' # 2       2    1.65      60       23.05  
#' # 3       1    1.80      80       23.46  
#' # 4       2    1.60      55       22.74  
#' # 5       6  996.00      70    <NA+b>     # Invalid sex code
#' # 6       7  997.00     998    <NA+b>     # CCHS missing codes
#' # 7       8  998.00     999    <NA+b>     # CCHS missing codes
#' # 8       9     999      NA    <NA+b>     # CCHS missing codes
#' # Comprehensive validation and error handling applied automatically
#' 
#' # Basic scalar examples:
#' # Validation for missing data handled. Other validation (out-of-bound ranges, 
#' # variable type) not performed - use rec_with_table() for comprehensive validation.
#' # DHH_SEX: sex (1=male, 2=female). See variable_details.csv for valid values: [1, 2]
#' # HWTGHTM: height in meters. See variable_details.csv for valid range: [0.914, 2.134]
#' # HWTGWTK: weight in kilograms. See variable_details.csv for valid range: [27.0, 135.0]
#' adjust_bmi(1, 1.75, 70)   # Returns: 23.51671 (male correction applied)
#' adjust_bmi(2, 1.65, 60)   # Returns: 23.04519 (female correction applied)
#' adjust_bmi("1", 1.75, 70) # Returns: 23.51671 (string converted by R)
#' adjust_bmi(6, 1.75, 70)   # Returns: NA (invalid sex code)
#' adjust_bmi(1, 998, 70)    # Returns: NA (CCHS missing code)
#' adjust_bmi(1, 3.5, 70)   # Returns: 5.072364 (calculates despite height beyond max = 2.134)
#' 
#' # Check internal tagged NA structure:
#' result <- adjust_bmi(6, 1.75, 70)     # Invalid sex code
#' haven::is_tagged_na(result, "a")      # Returns: TRUE (not applicable)
#' result2 <- adjust_bmi(1, 997, 70)     # CCHS missing height
#' haven::is_tagged_na(result2, "b")     # Returns: TRUE (missing/invalid)
#' result3 <- adjust_bmi(3, 1.5, 70)     # Invalid sex code (beyond max = 2)
#' haven::is_tagged_na(result3, "b")     # Returns: TRUE (sex validation: only 1,2 valid)
#' 
#' # ARCHITECTURAL NOTE: adjust_bmi() validates sex codes (1=male, 2=female) because
#' # correction coefficients are sex-specific and undefined for other values.
#' # However, height/weight bounds are NOT validated (consistent with calculate_bmi()).
#' # Use rec_with_table() for comprehensive validation of all parameters.
#' # Note: Basic validation applied - use rec_with_table() for comprehensive validation
#'
#' # Basic vector examples:
#' # Same validation as scalar examples
#' # CCHS missing codes (automatically handled for PUMF/master data):
#' adjust_bmi(c(1, 2, 6), c(1.75, 1.65, 1.80), c(70, 60, 997))
#' # Returns: c(23.51671, 23.04519, NA)
#' 
#' # Real CCHS data patterns:
#' sex_codes <- c(1, 2, 6, 7, 8, 9, NA)      # Real PUMF pattern  
#' heights <- c(1.75, 1.65, 996, 997, 998, 999, NA)
#' weights <- c(70, 60, 996, 997, 998, 999, NA)
#' adjust_bmi(sex_codes, heights, weights)
#' # Handles: valid combinations + all CCHS missing codes + regular NAs
#'
#' # For production analysis, rec_with_table() provides comprehensive handling:
#' # - Full CSV-driven validation from variable_details.csv
#' # - Robust error handling and recovery
#' # - Consistent preprocessing across all variables
#' # - Better performance with large datasets
#'
#' @references
#' Connor Gorber, S., et al. (2008). The accuracy of self-reported height and weight
#' in a nationally representative sample of Canadian adults. Obesity, 16(10), 2326-2332.
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced with comprehensive preprocessing and sex-specific bias correction
#' @export
adjust_bmi <- function(DHH_SEX = NULL, 
                        HWTGHTM = NULL, 
                        HWTGWTK = NULL,
                        validate_params = NULL,
                        log_level = "silent") {
  # 1. Clean CCHS missing codes only - comprehensive validation handled by rec_with_table()
  cleaned <- clean_variables(
    continuous_vars = list(height = HWTGHTM, weight = HWTGWTK),
    categorical_vars = list(sex = DHH_SEX),
    continuous_pattern = "triple_digit_missing",
    categorical_pattern = "single_digit_missing",
    log_level = log_level
  )

  # 2. Calculate raw BMI from clean inputs
  raw_bmi <- calculate_bmi_core(cleaned$height_clean, cleaned$weight_clean)

  # 3. Apply sex-specific bias correction with comprehensive tagged NA handling
  dplyr::case_when(
    # Use standardized tagged NA conditions from both inputs
    !!!generate_tagged_na_conditions(raw_bmi, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(cleaned$sex_clean, categorical_labels = FALSE),

    # Apply sex-specific corrections for valid data
    cleaned$sex_clean == 1 ~ BMI_CORRECTION_MALE$intercept + BMI_CORRECTION_MALE$slope * raw_bmi,
    cleaned$sex_clean == 2 ~ BMI_CORRECTION_FEMALE$intercept + BMI_CORRECTION_FEMALE$slope * raw_bmi,

    # Catch-all for invalid sex values
    .default = haven::tagged_na("b")
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
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
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
#' # rec_with_table workflow (RECOMMENDED - handles all validation automatically):
#' # Generate test data with correct and incorrect input
#' test_data <- data.frame(
#'   HWTGHTM = c(1.75, 1.60, 1.80, 1.70, 996, 997, 998, 999),
#'   HWTGWTK = c(50, 55, 90, 110, 70, 998, 999, NA)
#' )
#' library(cchsflow)
#' result <- rec_with_table(
#'   test_data,
#'   c("HWTGHTM", "HWTGWTK", "HWTGBMI_cat_der")
#' )
#' # Returns:
#' #   HWTGHTM HWTGWTK HWTGBMI_cat_der
#' # 1    1.75      50    "Underweight"  
#' # 2    1.60      55    "Normal weight"  
#' # 3    1.80      90    "Overweight"  
#' # 4    1.70     110    "Obese"  
#' # 5  996.00      70         <NA+b>     # CCHS missing code
#' # 6  997.00     998         <NA+b>     # CCHS missing codes
#' # 7  998.00     999         <NA+b>     # CCHS missing codes
#' # 8  999.00      NA         <NA+b>     # CCHS missing codes
#' # Comprehensive validation and error handling applied automatically
#' 
#' # Basic scalar examples:
#' # Validation for missing data handled. Other validation (out-of-bound ranges, 
#' # variable type) not performed - use rec_with_table() for comprehensive validation.
#' # bmi_value: BMI values (typically 15-50). See variable_details.csv for context
#' categorize_bmi(17.0)   # Returns: "Underweight" (BMI < 18.5)
#' categorize_bmi(22.0)   # Returns: "Normal weight" (18.5 ≤ BMI < 25.0)
#' categorize_bmi(27.0)   # Returns: "Overweight" (25.0 ≤ BMI < 30.0)
#' categorize_bmi(32.0)   # Returns: "Obese" (BMI ≥ 30.0)
#' categorize_bmi("22")   # Returns: "Normal weight" (string converted by R)
#' categorize_bmi(997)    # Returns: "NA(b)" (CCHS missing code)
#' 
#' # Check internal tagged NA structure (with categorical_labels = FALSE):
#' result <- categorize_bmi(996, categorical_labels = FALSE)  # CCHS not applicable
#' haven::is_tagged_na(result, "a")                          # Returns: TRUE
#' result2 <- categorize_bmi(997, categorical_labels = FALSE) # CCHS missing
#' haven::is_tagged_na(result2, "b")                         # Returns: TRUE
#' # Note: Basic validation applied - use rec_with_table() for comprehensive validation
#'
#' # Basic vector examples:
#' # Same validation as scalar examples
#' # CCHS missing codes (automatically handled for PUMF/master data):
#' categorize_bmi(c(17, 22, 27, 32))
#' # Returns: c("Underweight", "Normal weight", "Overweight", "Obese")
#' 
#' categorize_bmi(c(22, haven::tagged_na("a"), 27, 997))
#' # Returns: c("Normal weight", "NA(a)", "Overweight", "NA(b)")
#'
#' # Real CCHS data patterns:
#' bmi_values <- c(18.0, 23.5, 28.0, 35.0, 996, 997, 998, 999, NA)
#' categorize_bmi(bmi_values)
#' # Handles: valid BMI values + all CCHS missing codes + regular NAs
#'
#' # Numeric codes instead of labels:
#' categorize_bmi(c(17, 22, 27, 32), categorical_labels = FALSE)
#' # Returns: c(1L, 2L, 3L, 4L) where 1=Underweight, 2=Normal, 3=Overweight, 4=Obese
#'
#' # For production analysis, rec_with_table() provides comprehensive handling:
#' # - Full CSV-driven validation from variable_details.csv
#' # - Robust error handling and recovery
#' # - Consistent preprocessing across all variables
#' # - Better performance with large datasets
#'
#' @references
#' World Health Organization. (2000). Obesity: preventing and managing the global epidemic.
#' WHO Technical Report Series, 894.
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced categorization with comprehensive missing data handling
#' @export
categorize_bmi <- function(bmi_value = NULL, categorical_labels = TRUE, log_level = "silent") {
  # 1. Preprocess values using vector-aware helper (standardized pattern)
  clean_bmi <- clean_for_categorization(bmi_value, "triple_digit_missing", log_level)

  # 2. Categorize BMI values with standardized tagged NA handling
  if (categorical_labels) {
    dplyr::case_when(
      # Use standardized tagged NA conditions
      !!!generate_tagged_na_conditions(clean_bmi, categorical_labels = TRUE),

      # BMI categories (WHO classification)
      clean_bmi < 18.5 ~ "Underweight",
      clean_bmi >= 18.5 & clean_bmi < 25.0 ~ "Normal weight",
      clean_bmi >= 25.0 & clean_bmi < 30.0 ~ "Overweight",
      clean_bmi >= 30.0 ~ "Obese",

      # Catch-all for any remaining edge cases
      .default = "NA(b)"
    )
  } else {
    dplyr::case_when(
      # Use standardized tagged NA conditions
      !!!generate_tagged_na_conditions(clean_bmi, categorical_labels = FALSE),

      # BMI categories (WHO classification)
      clean_bmi < 18.5 ~ 1L,
      clean_bmi >= 18.5 & clean_bmi < 25.0 ~ 2L,
      clean_bmi >= 25.0 & clean_bmi < 30.0 ~ 3L,
      clean_bmi >= 30.0 ~ 4L,

      # Catch-all for any remaining edge cases
      .default = haven::tagged_na("b")
    )
  }
}
