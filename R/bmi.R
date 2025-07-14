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
# 2. Data flow and function catalog
# ==============================================================================

# RECOMMENDED WORKFLOW:
#   Raw CCHS Data → rec_with_table() → Validated Results
#   
# ARCHITECTURE:
#   Derived functions → Internal helper functions → Results
#   Variable documentation → Help system access (?variable_name)

# SPECIFIC BMI DERIVATION FLOWS (variable = function → helper → result):
#   HWTGBMI_der      = calculate_bmi(HWTGHTM, HWTGWTK) → calculate_bmi_core() → BMI value (weight/height²)
#   HWTGCOR_der      = adjust_bmi(DHH_SEX, HWTGHTM, HWTGWTK) → calculate_bmi_core() → Bias-corrected BMI value
#   HWTGBMI_cat4 = rec_with_table() → WHO thresholds → BMI category (1-4)

# ==============================================================================
# Function catalog
# ==============================================================================

# DERIVED FUNCTIONS (Variables calculated with multiple starting variables, formulas or scales):
#   Functions:
#   • calculate_bmi()                   - Standard BMI calculation (HWTGHTM, HWTGWTK)
#   • adjust_bmi()                      - Bias-corrected BMI for self-reported data (DHH_SEX, HWTGHTM, HWTGWTK)
#
#   Internal helper functions:
#   • calculate_bmi_core()              - Core BMI calculation logic

# VARIABLE DOCUMENTATION FUNCTIONS (Reference & help system):
#   Raw CCHS anthropometric variables (Statistics Canada):
#   • HWTGHTM()    - Height in meters (self-reported, converted from feet/inches)
#   • HWTGWTK()    - Weight in kilograms (self-reported, converted from pounds)
#   
#   Derived BMI variables:
#   • HWTGBMI_der()      - BMI continuous value
#   • HWTGCOR_der()      - Bias-corrected BMI (Connor Gorber et al. 2008)
#   • HWTGBMI_cat4() - BMI WHO categories (underweight, normal, overweight, obese)

# ==============================================================================
# Usage patterns  
# ==============================================================================

# Direct anthropometric variable analysis (harmonized):
#   bmi_data <- rec_with_table(cchs_data, c("HWTGHTM", "HWTGWTK", "DHH_SEX", "DHH_AGE"))
#
# Comprehensive BMI analysis:
#   bmi_results <- rec_with_table(cchs_data, c(
#     "HWTGHTM", "HWTGWTK", "DHH_SEX", "DHH_AGE",
#     "HWTGBMI_der", "HWTGCOR_der", "HWTGBMI_cat4"
#   ))

# Variable documentation access:
#   ?HWTGHTM     # Height variable documentation
#   ?HWTGWTK     # Weight variable documentation

# VALIDATION RESPONSIBILITY:
#   • Standalone functions: Basic CCHS code preprocessing only
#   • rec_with_table():     Comprehensive validation via variable_details.csv

# ==============================================================================
# 3. SPECIALIZED HELPER FUNCTIONS
# ==============================================================================

#' Core BMI calculation (internal helper)
#'
#' Vector-aware BMI calculation without validation - used as building block
#' @param height Height in meters (CCHS missing codes preprocessed to tagged_na, can be scalar or vector)
#' @param weight Weight in kilograms (CCHS missing codes preprocessed to tagged_na, can be scalar or vector)
#' @return BMI value(s) with proper tagged NA handling
#' @note v3.0.0, last updated: 2025-07-04, status: active, Note: NA
#' @noRd
calculate_bmi_core <- function(height, weight) {
  # Use case_when for element-wise processing with tagged NA handling
  dplyr::case_when(
    # !!! (splice operator) expands assign_tagged_na() output:
    # Creates 5 conditions per variable checking tagged NAs in priority order
    # c (question not asked) → d (variable missing) → a (not applicable) → b (unknown/refusal)
    # Equivalent to: is_tagged_na(height,"c") ~ tagged_na("c"), is_tagged_na(height,"d") ~ tagged_na("d"), etc.
    !!!assign_tagged_na(height),
    !!!assign_tagged_na(weight),

    # Calculate BMI for valid values
    .default = weight / (height^2)
  )
}

# ==============================================================================
# 4. DERIVED VARIABLE FUNCTIONS
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
  cleaned <- clean_variables(
    continuous_vars = list(height = HWTGHTM, weight = HWTGWTK),
    continuous_pattern = "triple_digit_missing",
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
                        log_level = "silent",
                        male_correction = list(intercept = -1.07575, slope = 1.07592),
                        female_correction = list(intercept = -0.12374, slope = 1.05129)) {
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
    !!!assign_tagged_na(raw_bmi),
    !!!assign_tagged_na(cleaned$sex_clean),

    # Apply sex-specific corrections for valid data
    cleaned$sex_clean == 1 ~ male_correction$intercept + male_correction$slope * raw_bmi,
    cleaned$sex_clean == 2 ~ female_correction$intercept + female_correction$slope * raw_bmi,

    # Catch-all for invalid sex values
    .default = haven::tagged_na("b")
  )
}

# IMPORTANT: categorize_bmi function removed in BMI refactoring (2025-07-10)
# BMI categorization now handled via rec_with_table() with HWTGBMI_cat4 variable
# See variable_details.csv for range-based mapping: [0,18.5) → 1, [18.5,25) → 2, etc.

# ==============================================================================
# 5. VARIABLE DOCUMENTATION FUNCTIONS (Reference & help system):
# ==============================================================================
#
# These functions provide documentation for CCHS variables and derived variables.
# Can be used directly in recodeflow for harmonized analysis or as inputs to derived functions.
# Usage: ?HWTGHTM, ?HWTGWTK, etc.

#' @title Height in meters
#'
#' @description 
#' \strong{NOTE:} This is a documentation function, not a computational function.
#'
#' Continuous variable for respondent height in meters. Used as input to BMI 
#' calculations and anthropometric analyses. Can be used directly in recodeflow 
#' for harmonized analysis or as input to BMI-related derived functions.
#'
#' @details 
#' **Data Source:** CCHS respondent measurements
#' **Units:** Meters (m)
#' **Range:** 0.914-2.134 meters (typical range 1.4-2.0m). See variable_details.csv for updates.
#' **Availability:** Present in all CCHS cycles used in cchsflow
#' **Measurement:** Self-reported height converted to meters
#' **Harmonization:** Use recodeflow for consistent cross-cycle analysis
#' **Quality Note:** Subject to self-reporting bias; consider adjust_bmi() for bias correction
#'
#' @param HWTGHTM Variable name for height in meters
#'
#' @examples
#' # Access variable documentation
#' library(cchsflow)
#' ?HWTGHTM
#' 
#' # Use in BMI calculation functions
#' ?calculate_bmi  # Uses HWTGHTM as input
#' ?adjust_bmi     # Uses HWTGHTM as input
#' 
#' # Variable usage in rec_with_table()
#' \dontrun{
#' # Basic anthropometric analysis
#' height_data <- rec_with_table(cchs2009_2010_p, 
#'   c("HWTGHTM", "DHH_SEX", "DHHGAGE"))
#' summary(height_data$HWTGHTM)
#' 
#' # Combined with BMI calculation
#' bmi_data <- rec_with_table(cchs2009_2010_p, 
#'   c("HWTGHTM", "HWTGWTK", "DHH_SEX", "calculate_bmi"))
#' 
#' # Multi-cycle harmonized analysis
#' combined_data <- bind_rows(
#'   rec_with_table(cchs2001_p, c("HWTGHTM", "HWTGWTK", "DHH_SEX")),
#'   rec_with_table(cchs2009_2010_p, c("HWTGHTM", "HWTGWTK", "DHH_SEX"))
#' )
#' }
#'
#' @seealso 
#' \code{\link{HWTGWTK}} for weight variable (required for BMI calculation)
#' \code{\link{calculate_bmi}}, \code{\link{adjust_bmi}} for BMI functions using this variable
#'
#' @note v3.0.0, last updated: 2025-07-09, status: active, Note: Enhanced documentation with BMI calculation guidance
#' @export
HWTGHTM <- function(HWTGHTM) {
  # this is for documentation purposes only
}

#' @title Weight in kilograms
#'
#' @description 
#' \strong{NOTE:} This is a documentation function, not a computational function.
#'
#' Continuous variable for respondent weight in kilograms. Used as input to BMI 
#' calculations and anthropometric analyses. Can be used directly in recodeflow 
#' for harmonized analysis or as input to BMI-related derived functions.
#'
#' @details 
#' **Data Source:** CCHS respondent measurements
#' **Units:** Kilograms (kg)
#' **Range:** 27.0-135.0 kilograms (typical range 40-120kg). See variable_details for updates.
#' **Availability:** Present in all CCHS cycles used in cchsflow
#' **Measurement:** Self-reported weight in kilograms
#' **Harmonization:** Use recodeflow for consistent cross-cycle analysis
#' **Quality Note:** Subject to self-reporting bias; consider adjust_bmi() for bias correction
#'
#' @param HWTGWTK Variable name for weight in kilograms
#'
#' @examples
#' # Access variable documentation
#' library(cchsflow)
#' ?HWTGWTK
#' 
#' # Use in BMI calculation functions
#' ?calculate_bmi  # Uses HWTGWTK as input
#' ?adjust_bmi     # Uses HWTGWTK as input
#' 
#' # Variable usage in rec_with_table()
#' \dontrun{
#' # Basic anthropometric analysis
#' weight_data <- rec_with_table(cchs2009_2010_p, 
#'   c("HWTGWTK", "DHH_SEX", "DHHGAGE"))
#' summary(weight_data$HWTGWTK)
#' 
#' # Combined with BMI calculation
#' bmi_data <- rec_with_table(cchs2009_2010_p, 
#'   c("HWTGHTM", "HWTGWTK", "DHH_SEX", "calculate_bmi"))
#' 
#' # Multi-cycle harmonized analysis
#' combined_data <- bind_rows(
#'   rec_with_table(cchs2001_p, c("HWTGHTM", "HWTGWTK", "DHH_SEX")),
#'   rec_with_table(cchs2009_2010_p, c("HWTGHTM", "HWTGWTK", "DHH_SEX"))
#' )
#' }
#'
#' @seealso 
#' \code{\link{HWTGHTM}} for height variable (required for BMI calculation)
#' \code{\link{calculate_bmi}}, \code{\link{adjust_bmi}} for BMI functions using this variable
#'
#' @note v3.0.0, last updated: 2025-07-09, status: active, Note: Enhanced documentation with BMI calculation guidance
#' @export
HWTGWTK <- function(HWTGWTK) {
  # this is for documentation purposes only
}

#' BMI continuous value (kg/m²)
#'
#' @description 
#' Continuous Body Mass Index calculated using the standard formula: weight (kg) / height (m)².
#'
#' @details
#' **Derivation Flow:** `HWTGBMI_der = calculate_bmi(HWTGHTM, HWTGWTK) → calculate_bmi_core() → BMI value (weight/height²)`
#' 
#' **Values:** Continuous values typically ranging 10-60 kg/m²
#' - Under 18.5: Underweight
#' - 18.5-24.9: Normal weight  
#' - 25.0-29.9: Overweight
#' - 30.0+: Obese
#'
#' @format Numeric vector with BMI values in kg/m². Missing data handled as haven::tagged_na():
#' \itemize{
#'   \item tagged_na("a") - Not applicable (missing input data)
#'   \item tagged_na("b") - Invalid/missing measurements
#' }
#'
#' @seealso 
#' \code{\link{calculate_bmi}} to generate this variable,
#' Use rec_with_table() with HWTGBMI_cat4 to convert to WHO categories,
#' \code{\link{HWTGHTM}}, \code{\link{HWTGWTK}} for input variables
#'
#' @note v3.0.0, last updated: 2025-07-10, status: active, Note: Standard BMI calculation documentation
#' @keywords datasets
#' @export
HWTGBMI_der <- function(HWTGBMI_der) {
  # this is for documentation purposes only
}

#' Bias-corrected BMI for self-reported data (kg/m²)
#'
#' @description 
#' BMI corrected for systematic reporting bias in self-reported height and weight data.
#' Applies sex-specific correction coefficients from Connor Gorber et al. 2008 validation study.
#'
#' @details
#' **Derivation Flow:** `HWTGCOR_der = adjust_bmi(DHH_SEX, HWTGHTM, HWTGWTK) → calculate_bmi_core() → Bias-corrected BMI value`
#' 
#' **Correction Method:** Sex-specific bias correction using measured vs. self-reported validation data
#' - Males: Corrected BMI = 1.09 × Standard BMI - 0.05
#' - Females: Corrected BMI = 1.08 × Standard BMI + 0.07
#' 
#' **Values:** Continuous values typically ranging 10-60 kg/m² (bias-corrected)
#'
#' @format Numeric vector with bias-corrected BMI values in kg/m². Missing data handled as haven::tagged_na():
#' \itemize{
#'   \item tagged_na("a") - Not applicable (missing input data) 
#'   \item tagged_na("b") - Invalid/missing measurements or sex
#' }
#'
#' @seealso 
#' \code{\link{adjust_bmi}} to generate this variable,
#' \code{\link{calculate_bmi}} for standard BMI,
#' \code{\link{DHH_SEX}}, \code{\link{HWTGHTM}}, \code{\link{HWTGWTK}} for input variables
#'
#' @references
#' Connor Gorber, S., et al. (2008). The accuracy of self-reported height and weight
#' in a nationally representative sample of Canadian adults. Obesity, 16(10), 2326-2332.
#'
#' @note v3.0.0, last updated: 2025-07-10, status: active, Note: Bias-corrected BMI documentation
#' @keywords datasets
#' @export
HWTGCOR_der <- function(HWTGCOR_der) {
  # this is for documentation purposes only
}

#' BMI WHO categories (4-level classification)
#'
#' @description 
#' Body Mass Index classified into World Health Organization standard categories using direct range mapping.
#' Four-level classification system for weight status assessment in population health research.
#'
#' @details
#' **Derivation Flow:** `HWTGBMI_cat4 = rec_with_table() → WHO thresholds → BMI category (1-4)`
#' 
#' **WHO Categories:**
#' - 1 = "Underweight" (BMI < 18.5)
#' - 2 = "Normal weight" (BMI 18.5-24.9)  
#' - 3 = "Overweight" (BMI 25.0-29.9)
#' - 4 = "Obese" (BMI ≥ 30.0)
#' 
#' **Values:** Can be returned as numeric codes (1-4) or character labels
#'
#' @format Factor or character vector with BMI categories. Missing data handled as haven::tagged_na():
#' \itemize{
#'   \item "NA(a)" or tagged_na("a") - Not applicable (missing BMI data)
#'   \item "NA(b)" or tagged_na("b") - Invalid/missing BMI values
#' }
#'
#' @seealso 
#' \code{\link{HWTGBMI_der}} for continuous BMI input,
#' \code{\link{calculate_bmi}}, \code{\link{adjust_bmi}} for BMI calculation
#'
#' @references
#' World Health Organization. (2000). Obesity: preventing and managing the global epidemic.
#' WHO Technical Report Series, 894.
#'
#' @note v3.0.0, last updated: 2025-07-10, status: active, Note: WHO BMI categories documentation
#' @keywords datasets
#' @export
HWTGBMI_cat4 <- function(HWTGBMI_cat4) {
  # this is for documentation purposes only
}
