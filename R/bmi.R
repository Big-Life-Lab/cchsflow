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

# BMI validation bounds for standalone function use
# NOTE: These constants ensure BMI functions work independently ("cut and paste")
# while rec_with_table() uses variable_details.csv for CSV-driven validation.
# IMPORTANT: Keep synchronized with variable_details.csv ground truth values
BMI_VALIDATION_BOUNDS <- list(
  height = list(min = 0.914, max = 2.134), # Height in meters [0.914,2.134] from variable_details.csv
  weight = list(min = 27.0, max = 135.0), # Weight in kilograms [27.0,135.0] from variable_details.csv
  bmi = list(min = 10, max = 100) # BMI valid range [10,100] from variable_details.csv
)

# BMI correction coefficients from Connor Gorber et al. 2008
BMI_CORRECTION_MALE <- list(intercept = -1.07575, slope = 1.07592)
BMI_CORRECTION_FEMALE <- list(intercept = -0.12374, slope = 1.05129)

# ==============================================================================
# 2. VALIDATION FUNCTIONS
# ==============================================================================




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
    # Use standardized tagged NA conditions
    !!!generate_tagged_na_conditions(height, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(weight, categorical_labels = FALSE),

    # Calculate BMI for valid values
    .default = weight / (height^2)
  )
}


# ==============================================================================
# 4. PUBLIC API FUNCTIONS
# ==============================================================================

#' Calculate Body Mass Index (BMI) with comprehensive validation
#'
#' @description
#' Calculate harmonized BMI across CCHS cycles using the standard formula BMI = weight(kg) / height(m)^2.
#' Includes comprehensive validation, outlier detection, and missing data preprocessing following WHO guidelines.
#'
#' @param HWTGHTM CCHS variable for height (in meters). Accepts raw CCHS codes or preprocessed values.
#' @param HWTGWTK CCHS variable for weight (in kilograms). Accepts raw CCHS codes or preprocessed values.
#' @param min_HWTGHTM Minimum valid height in meters (default 0.914)
#' @param max_HWTGHTM Maximum valid height in meters (default 2.134)
#' @param min_HWTGWTK Minimum valid weight in kilograms (default 27.0)
#' @param max_HWTGWTK Maximum valid weight in kilograms (default 135.0)
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
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
#' automatically converted to haven::tagged_na(). Validation bounds: Height [0.914-2.134m],
#' Weight [27.0-135.0kg], BMI [10-100]. Extreme values are marked as haven::tagged_na("b").
#'
#' @examples
#' # Standard cchsflow workflow (primary usage)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c("HWTGHTM", "HWTGWTK", "HWTGBMI_der")
#' )
#'
#' # Basic usage
#' calculate_bmi(1.75, 70)
#'
#' # Vector processing with missing data
#' calculate_bmi(c(1.75, 1.80, 996), c(70, 75, 997))
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
calculate_bmi <- function(HWTGHTM = NULL, HWTGWTK = NULL,
                          # Validation bounds: defaults from variable_details.csv for standalone use
                          # Use rec_with_table() for full CSV-driven validation workflow
                          min_HWTGHTM = BMI_VALIDATION_BOUNDS$height$min,
                          max_HWTGHTM = BMI_VALIDATION_BOUNDS$height$max,
                          min_HWTGWTK = BMI_VALIDATION_BOUNDS$weight$min,
                          max_HWTGWTK = BMI_VALIDATION_BOUNDS$weight$max,
                          validate_params = NULL,
                          log_level = "silent") {
  # Clean continuous variables (handles missing variables with tagged_na("d"))
  cleaned <- clean_continuous_variables(
    height = HWTGHTM,
    weight = HWTGWTK,
    min_values = list(height = min_HWTGHTM, weight = min_HWTGWTK),
    max_values = list(height = max_HWTGHTM, weight = max_HWTGWTK),
    log_level = log_level
  )

  # Calculate BMI from clean inputs
  calculate_bmi_core(cleaned$height_clean, cleaned$weight_clean)
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
#' @param min_HWTGHTM,max_HWTGHTM,min_HWTGWTK,max_HWTGWTK Validation parameters (defaults from variable_details.csv)
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
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
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c("DHH_SEX", "HWTGHTM", "HWTGWTK", "HWTGCOR_der")
#' )
#'
#' # Basic usage
#' adjust_bmi(1, 1.75, 70) # Male
#' adjust_bmi(2, 1.65, 60) # Female
#'
#' # Vector processing with missing data
#' adjust_bmi(c(1, 2, 6), c(1.75, 1.65, 1.80), c(70, 60, 997))
#'
#' @references
#' Connor Gorber, S., et al. (2008). The accuracy of self-reported height and weight
#' in a nationally representative sample of Canadian adults. Obesity, 16(10), 2326-2332.
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced with comprehensive preprocessing and sex-specific bias correction
#' @export
adjust_bmi <- function(DHH_SEX = NULL, HWTGHTM = NULL, HWTGWTK = NULL,
                       # Validation bounds: defaults from variable_details.csv for standalone use
                       # Use rec_with_table() for full CSV-driven validation workflow
                       min_HWTGHTM = BMI_VALIDATION_BOUNDS$height$min,
                       max_HWTGHTM = BMI_VALIDATION_BOUNDS$height$max,
                       min_HWTGWTK = BMI_VALIDATION_BOUNDS$weight$min,
                       max_HWTGWTK = BMI_VALIDATION_BOUNDS$weight$max,
                       validate_params = NULL,
                       log_level = "silent") {
  # 1. Clean all variables together with unified length compatibility checking
  cleaned <- clean_variables(
    continuous_vars = list(height = HWTGHTM, weight = HWTGWTK),
    categorical_vars = list(sex = DHH_SEX),
    min_values = list(height = min_HWTGHTM, weight = min_HWTGWTK),
    max_values = list(height = max_HWTGHTM, weight = max_HWTGWTK),
    valid_values = list(sex = c(1, 2)),
    continuous_pattern = "continuous_standard",
    categorical_pattern = "standard_response",
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
#' # Standard cchsflow workflow (primary usage)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c("HWTGHTM", "HWTGWTK", "HWTGBMI_cat_der")
#' )
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
categorize_bmi <- function(bmi_value = NULL, categorical_labels = TRUE, log_level = "silent") {
  # 1. Preprocess values using vector-aware helper (standardized pattern)
  clean_bmi <- clean_for_categorization(bmi_value, "continuous_standard", log_level)

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
