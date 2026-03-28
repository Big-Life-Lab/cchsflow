# ==============================================================================
# ADL (Activities of Daily Living) Functions
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
#   source('R/adl.R')
#   test_file('tests/testthat/test-assess-adl.R')

# ==============================================================================
# 1. CONSTANTS AND CONFIGURATION
# ==============================================================================

# ADL validation bounds for standalone function use
# NOTE: These constants ensure ADL functions work independently ("cut and paste")
# while rec_with_table() uses variable_details.csv for CSV-driven validation.
# IMPORTANT: Keep synchronized with variable_details.csv ground truth values
ADL_VALIDATION_BOUNDS <- list(
  response = list(min = 1, max = 2) # 1 = needs help, 2 = does not need help
)

# ==============================================================================
# 2. SPECIALIZED HELPER FUNCTIONS
# ==============================================================================

#' Core ADL binary assessment (internal helper)
#'
#' Vector-aware ADL assessment without validation - used as building block
#' @param adl_01,adl_02,adl_03,adl_04,adl_05 Cleaned ADL variables (already validated)
#' @return Binary ADL indicator with proper tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-07-04, status: active - Vector aware
#' @noRd
calculate_adl_binary_core <- function(adl_01, adl_02, adl_03, adl_04, adl_05) {
  # Use case_when for element-wise processing with tagged NA handling
  dplyr::case_when(
    # !!! (splice operator) expands generate_tagged_na_conditions() output for each variable
    !!!generate_tagged_na_conditions(adl_01, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_02, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_03, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_04, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_05, categorical_labels = FALSE),

    # Any help needed (any value = 1)
    (adl_01 == 1 | adl_02 == 1 | adl_03 == 1 | adl_04 == 1 | adl_05 == 1) ~ 1L,

    # No help needed (all values = 2)
    .default = 2L
  )
}

#' Core ADL score calculation (internal helper)
#'
#' Vector-aware ADL score calculation without validation - used as building block
#' @param adl_01,adl_02,adl_03,adl_04,adl_05 Cleaned ADL variables (already validated)
#' @return ADL score (0-5) with proper tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-07-04, status: active - Vector aware
#' @noRd
calculate_adl_score_core <- function(adl_01, adl_02, adl_03, adl_04, adl_05) {
  # Use case_when for element-wise processing with tagged NA handling
  dplyr::case_when(
    # !!! (splice operator) expands generate_tagged_na_conditions() output for each variable
    !!!generate_tagged_na_conditions(adl_01, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_02, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_03, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_04, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_05, categorical_labels = FALSE),

    # Calculate help needed count (sum of 1s)
    .default = as.numeric(
      (adl_01 == 1) + (adl_02 == 1) + (adl_03 == 1) + (adl_04 == 1) + (adl_05 == 1)
    )
  )
}

#' Core 6-item ADL score calculation (internal helper)
#'
#' Vector-aware 6-item ADL score calculation without validation - used as building block
#' @param adl_01,adl_02,adl_03,adl_04,adl_05,adl_06 Cleaned ADL variables (already validated)
#' @return ADL score (0-6) with proper tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-07-04, status: active - Vector aware
#' @noRd
calculate_adl_score_6_core <- function(adl_01, adl_02, adl_03, adl_04, adl_05, adl_06) {
  # Use case_when for element-wise processing with tagged NA handling
  dplyr::case_when(
    # !!! (splice operator) expands generate_tagged_na_conditions() output for each variable
    !!!generate_tagged_na_conditions(adl_01, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_02, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_03, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_04, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_05, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(adl_06, categorical_labels = FALSE),

    # Calculate help needed count (sum of 1s)
    .default = as.numeric(
      (adl_01 == 1) + (adl_02 == 1) + (adl_03 == 1) +
        (adl_04 == 1) + (adl_05 == 1) + (adl_06 == 1)
    )
  )
}

# ==============================================================================
# 3. PUBLIC API FUNCTIONS
# ==============================================================================

#' Enhanced Activities of Daily Living (ADL) help indicator
#'
#' @description
#' **Purpose**: Create binary indicator for ADL help requirements using variables consistent across CCHS cycles
#' **Method**: Examines 5 core ADL tasks available in all cycles (2001-2018)
#' **Clinical Context**: Functional limitation assessment for population health research
#'
#' @param ADL_01 Help needed preparing meals (1=yes, 2=no). Accepts raw CCHS codes or preprocessed values.
#' @param ADL_02 Help needed getting to appointments/errands (1=yes, 2=no). Accepts raw CCHS codes or preprocessed values.
#' @param ADL_03 Help needed doing housework (1=yes, 2=no). Accepts raw CCHS codes or preprocessed values.
#' @param ADL_04 Help needed doing personal care (1=yes, 2=no). Accepts raw CCHS codes or preprocessed values.
#' @param ADL_05 Help needed moving inside house (1=yes, 2=no). Accepts raw CCHS codes or preprocessed values.
#' @param min_ADL_01,max_ADL_01,min_ADL_02,max_ADL_02,min_ADL_03,max_ADL_03,min_ADL_04,max_ADL_04,min_ADL_05,max_ADL_05 Validation parameters (defaults from variable_details.csv)
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return
#' **Data Type**: Numeric binary indicator
#' **Missing Data Handling**:
#' - Returns `haven::tagged_na("a")` for not applicable cases
#' - Returns `haven::tagged_na("b")` for missing/invalid responses
#' **Values**: 1 = needs help with ≥1 task, 2 = no help needed with any task
#'
#' @details
#' **Transformation Warnings**:
#' ⚠️ **CAUTION - Temporal Changes**: Original CCHS derived variable ADLF6R uses
#' different ADL components across cycles. This function uses consistent 5-item
#' subset for cross-cycle comparability.
#'
#' ⚠️ **CAUTION - Population Restrictions**: ADL questions typically restricted
#' to respondents with activity limitations. Not applicable responses preserved.
#'
#' **Calculation Details**:
#' - Uses ADL_01 through ADL_05 (consistent across all CCHS cycles)
#' - Any "needs help" (=1) response results in overall score of 1
#' - All "no help needed" (=2) responses result in overall score of 2
#' - Missing Data: Comprehensive preprocessing of CCHS codes (6,7,8,9)
#' - Priority: Not applicable > Missing > Valid responses
#'
#' @examples
#' # Standard cchsflow workflow (primary usage)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADLF6R_der")
#' )
#'
#' # Scalar usage examples
#' # assess_adl(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05)
#' #           (meals, appts,  house,  care,   move)
#' assess_adl(1, 2, 2, 2, 2) # Returns: 1 (needs help with meals only)
#' assess_adl(2, 2, 2, 2, 2) # Returns: 2 (no help needed with any task)
#' assess_adl(1, 1, 1, 1, 1) # Returns: 1 (needs help with all tasks)
#'
#' # Vector processing with missing data and edge cases
#' assess_adl(
#'   c(1, 2, 6, 7, 8, 9), # Mixed valid/CCHS codes
#'   c(2, 2, 2, 2, 2, 2),
#'   c(2, 2, 2, 2, 2, 2),
#'   c(2, 2, 2, 2, 2, 2),
#'   c(2, 2, 2, 2, 2, 2)
#' ) # Returns: c(1, 2, tagged_na("a"), tagged_na("b"), tagged_na("b"), tagged_na("b"))
#'
#' # Vector with string missing values
#' assess_adl(
#'   c(1, 2, "Not applicable"),
#'   c(2, 2, 2),
#'   c(2, 2, 2),
#'   c(2, 2, 2),
#'   c(2, 2, 2)
#' ) # Returns: c(1, 2, tagged_na("a"))
#'
#' @seealso
#' \code{\link{score_adl}} for count-based ADL scoring
#' \code{\link{score_adl_6}} for 6-item ADL scoring
#'
#' @references
#' Katz, S., et al. (1963). Studies of illness in the aged: The index of ADL.
#' JAMA, 185(12), 914-919.
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced with comprehensive preprocessing and modern missing data handling
#' @export
assess_adl <- function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05,
                       # Validation bounds: defaults from variable_details.csv for standalone use
                       # Use rec_with_table() for full CSV-driven validation workflow
                       min_ADL_01 = ADL_VALIDATION_BOUNDS$response$min,
                       max_ADL_01 = ADL_VALIDATION_BOUNDS$response$max,
                       min_ADL_02 = ADL_VALIDATION_BOUNDS$response$min,
                       max_ADL_02 = ADL_VALIDATION_BOUNDS$response$max,
                       min_ADL_03 = ADL_VALIDATION_BOUNDS$response$min,
                       max_ADL_03 = ADL_VALIDATION_BOUNDS$response$max,
                       min_ADL_04 = ADL_VALIDATION_BOUNDS$response$min,
                       max_ADL_04 = ADL_VALIDATION_BOUNDS$response$max,
                       min_ADL_05 = ADL_VALIDATION_BOUNDS$response$min,
                       max_ADL_05 = ADL_VALIDATION_BOUNDS$response$max,
                       validate_params = NULL,
                       log_level = "silent") {
  # Clean categorical variables (handles missing variables with tagged_na("d"))
  cleaned <- clean_categorical_variables(
    ADL_01 = ADL_01,
    ADL_02 = ADL_02,
    ADL_03 = ADL_03,
    ADL_04 = ADL_04,
    ADL_05 = ADL_05,
    valid_values = list(
      ADL_01 = min_ADL_01:max_ADL_01,
      ADL_02 = min_ADL_02:max_ADL_02,
      ADL_03 = min_ADL_03:max_ADL_03,
      ADL_04 = min_ADL_04:max_ADL_04,
      ADL_05 = min_ADL_05:max_ADL_05
    ),
    pattern_type = "single_digit_missing",
    log_level = log_level
  )

  # Calculate binary ADL indicator from clean inputs
  calculate_adl_binary_core(
    cleaned$ADL_01_clean, cleaned$ADL_02_clean, cleaned$ADL_03_clean,
    cleaned$ADL_04_clean, cleaned$ADL_05_clean
  )
}

#' Enhanced ADL help score (count of tasks requiring help)
#'
#' @description
#' **Purpose**: Count number of ADL tasks requiring help (0-5 scale) for granular functional assessment
#' **Method**: Sums "needs help" responses across 5 consistent ADL items
#' **Clinical Context**: Graduated disability measure for research and clinical assessment
#'
#' @param ADL_01,ADL_02,ADL_03,ADL_04,ADL_05 ADL variables as in assess_adl()
#' @param min_ADL_01,max_ADL_01,min_ADL_02,max_ADL_02,min_ADL_03,max_ADL_03,min_ADL_04,max_ADL_04,min_ADL_05,max_ADL_05 Validation parameters (defaults from variable_details.csv)
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return
#' **Data Type**: Numeric count (0-5)
#' **Missing Data Handling**:
#' - Returns `haven::tagged_na("a")` if any input is not applicable
#' - Returns `haven::tagged_na("b")` if any input is missing/invalid
#' **Values**: 0-5 representing count of tasks requiring help
#'
#' @details
#' **Transformation Warnings**:
#' ⚠️ **CAUTION - Methodological Changes**: Count-based scoring provides more
#' granular assessment than binary indicator but requires complete data for valid interpretation.
#'
#' **Calculation Details**:
#' - Counts number of ADL tasks with "needs help" response (=1)
#' - Range: 0 (independent) to 5 (maximum dependence)
#' - Missing data in ANY item invalidates entire score
#' - Preserves not applicable semantics across all items
#'
#' @examples
#' # Standard cchsflow workflow (primary usage)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADLSCORE_der")
#' )
#'
#' # Scalar usage examples
#' # score_adl(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05)
#' #          (meals, appts,  house,  care,   move)
#' score_adl(2, 2, 2, 2, 2) # Returns: 0 (no help needed)
#' score_adl(1, 2, 1, 2, 2) # Returns: 2 (help with 2 tasks: meals and housework)
#' score_adl(1, 1, 1, 1, 1) # Returns: 5 (complete dependence)
#' score_adl(1, 2, 2, 2, 2) # Returns: 1 (help with 1 task: meals only)
#'
#' # Vector processing with missing data
#' score_adl(
#'   c(1, 2, 6, 7), # Mixed valid/missing codes
#'   c(1, 2, 2, 2),
#'   c(2, 1, 2, 2),
#'   c(2, 2, 2, 2),
#'   c(2, 2, 2, 2)
#' ) # Returns: c(2, 1, tagged_na("a"), tagged_na("b"))
#'
#' # Vector with mixed missing types
#' score_adl(
#'   c(1, 2, "Missing", haven::tagged_na("a")),
#'   c(1, 2, 2, 2),
#'   c(2, 2, 2, 2),
#'   c(2, 2, 2, 2),
#'   c(2, 2, 2, 2)
#' ) # Returns: c(2, 1, tagged_na("b"), tagged_na("a"))
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced scoring with comprehensive missing data validation
#' @export
score_adl <- function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05,
                      # Validation bounds: defaults from variable_details.csv for standalone use
                      # Use rec_with_table() for full CSV-driven validation workflow
                      min_ADL_01 = ADL_VALIDATION_BOUNDS$response$min,
                      max_ADL_01 = ADL_VALIDATION_BOUNDS$response$max,
                      min_ADL_02 = ADL_VALIDATION_BOUNDS$response$min,
                      max_ADL_02 = ADL_VALIDATION_BOUNDS$response$max,
                      min_ADL_03 = ADL_VALIDATION_BOUNDS$response$min,
                      max_ADL_03 = ADL_VALIDATION_BOUNDS$response$max,
                      min_ADL_04 = ADL_VALIDATION_BOUNDS$response$min,
                      max_ADL_04 = ADL_VALIDATION_BOUNDS$response$max,
                      min_ADL_05 = ADL_VALIDATION_BOUNDS$response$min,
                      max_ADL_05 = ADL_VALIDATION_BOUNDS$response$max,
                      validate_params = NULL,
                      log_level = "silent") {
  # Clean categorical variables (handles missing variables with tagged_na("d"))
  cleaned <- clean_categorical_variables(
    ADL_01 = ADL_01,
    ADL_02 = ADL_02,
    ADL_03 = ADL_03,
    ADL_04 = ADL_04,
    ADL_05 = ADL_05,
    valid_values = list(
      ADL_01 = min_ADL_01:max_ADL_01,
      ADL_02 = min_ADL_02:max_ADL_02,
      ADL_03 = min_ADL_03:max_ADL_03,
      ADL_04 = min_ADL_04:max_ADL_04,
      ADL_05 = min_ADL_05:max_ADL_05
    ),
    pattern_type = "single_digit_missing",
    log_level = log_level
  )

  # Calculate ADL score from clean inputs
  calculate_adl_score_core(
    cleaned$ADL_01_clean, cleaned$ADL_02_clean, cleaned$ADL_03_clean,
    cleaned$ADL_04_clean, cleaned$ADL_05_clean
  )
}

#' Enhanced 6-item ADL help score with comprehensive validation
#'
#' @description
#' **Purpose**: Count ADL help requirements using 6-item scale including financial management
#' **Method**: Extends 5-item ADL to include financial management (available 2003+)
#' **Clinical Context**: Comprehensive functional assessment including instrumental ADL component
#'
#' @param ADL_01,ADL_02,ADL_03,ADL_04,ADL_05 Core ADL variables as in other functions
#' @param ADL_06 Help needed doing finances (1=yes, 2=no). Accepts raw CCHS codes or preprocessed values.
#' @param min_ADL_01,max_ADL_01,min_ADL_02,max_ADL_02,min_ADL_03,max_ADL_03,min_ADL_04,max_ADL_04,min_ADL_05,max_ADL_05,min_ADL_06,max_ADL_06 Validation parameters (defaults from variable_details.csv)
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return
#' **Data Type**: Numeric count (0-6)
#' **Missing Data Handling**: Same as 5-item version
#' **Values**: 0-6 representing count of tasks requiring help
#'
#' @details
#' **Transformation Warnings**:
#' ⚠️ **CAUTION - Temporal Changes**: ADL_06 (finances) available only in CCHS
#' 2003-2014 cycles. Use 5-item version for 2001 or 2015+ analyses.
#'
#' ⚠️ **CAUTION - Population Restrictions**: Financial management questions may
#' have different age restrictions or applicability rules across cycles.
#'
#' **Calculation Details**:
#' - Extends 5-item ADL with financial management component
#' - Range: 0 (independent) to 6 (maximum dependence)
#' - Same missing data handling as 5-item version
#' - Recommended for 2003-2014 cycle analyses
#'
#' @examples
#' # Standard cchsflow workflow (primary usage)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2007_2008_p,
#'   c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_06", "ADLSCORE6_der")
#' )
#'
#' # Scalar usage examples
#' # score_adl_6(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05, ADL_06)
#' #            (meals, appts,  house,  care,   move,   finances)
#' score_adl_6(2, 2, 2, 2, 2, 2) # Returns: 0 (independent with all tasks)
#' score_adl_6(1, 2, 1, 2, 2, 1) # Returns: 3 (help with 3 tasks: meals, housework, finances)
#' score_adl_6(1, 1, 1, 1, 1, 1) # Returns: 6 (complete dependence)
#' score_adl_6(2, 2, 2, 2, 2, 1) # Returns: 1 (help only with finances)
#'
#' # Vector processing with missing data and edge cases
#' score_adl_6(
#'   c(1, 2, 6, 96), # Mixed valid/categorical age codes
#'   c(1, 2, 2, 2),
#'   c(2, 1, 2, 2),
#'   c(2, 2, 2, 2),
#'   c(2, 2, 2, 2),
#'   c(2, 2, 2, 97) # "Don't know" for finances
#' ) # Returns: c(2, 1, tagged_na("a"), tagged_na("b"))
#'
#' # Vector demonstrating financial component (ADL_06)
#' score_adl_6(
#'   c(2, 2, 2), # No help with basic ADLs
#'   c(2, 2, 2),
#'   c(2, 2, 2),
#'   c(2, 2, 2),
#'   c(2, 2, 2),
#'   c(1, "Refusal", 2) # Different financial help needs
#' ) # Returns: c(1, tagged_na("b"), 0)
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced 6-item scoring for comprehensive functional assessment
#' @export
score_adl_6 <- function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05, ADL_06,
                        # Validation bounds: defaults from variable_details.csv for standalone use
                        # Use rec_with_table() for full CSV-driven validation workflow
                        min_ADL_01 = ADL_VALIDATION_BOUNDS$response$min,
                        max_ADL_01 = ADL_VALIDATION_BOUNDS$response$max,
                        min_ADL_02 = ADL_VALIDATION_BOUNDS$response$min,
                        max_ADL_02 = ADL_VALIDATION_BOUNDS$response$max,
                        min_ADL_03 = ADL_VALIDATION_BOUNDS$response$min,
                        max_ADL_03 = ADL_VALIDATION_BOUNDS$response$max,
                        min_ADL_04 = ADL_VALIDATION_BOUNDS$response$min,
                        max_ADL_04 = ADL_VALIDATION_BOUNDS$response$max,
                        min_ADL_05 = ADL_VALIDATION_BOUNDS$response$min,
                        max_ADL_05 = ADL_VALIDATION_BOUNDS$response$max,
                        min_ADL_06 = ADL_VALIDATION_BOUNDS$response$min,
                        max_ADL_06 = ADL_VALIDATION_BOUNDS$response$max,
                        validate_params = NULL,
                        log_level = "silent") {
  # Clean categorical variables (handles missing variables with tagged_na("d"))
  cleaned <- clean_categorical_variables(
    ADL_01 = ADL_01,
    ADL_02 = ADL_02,
    ADL_03 = ADL_03,
    ADL_04 = ADL_04,
    ADL_05 = ADL_05,
    ADL_06 = ADL_06,
    valid_values = list(
      ADL_01 = min_ADL_01:max_ADL_01,
      ADL_02 = min_ADL_02:max_ADL_02,
      ADL_03 = min_ADL_03:max_ADL_03,
      ADL_04 = min_ADL_04:max_ADL_04,
      ADL_05 = min_ADL_05:max_ADL_05,
      ADL_06 = min_ADL_06:max_ADL_06
    ),
    pattern_type = "single_digit_missing",
    log_level = log_level
  )

  # Calculate 6-item ADL score from clean inputs
  calculate_adl_score_6_core(
    cleaned$ADL_01_clean, cleaned$ADL_02_clean, cleaned$ADL_03_clean,
    cleaned$ADL_04_clean, cleaned$ADL_05_clean, cleaned$ADL_06_clean
  )
}
