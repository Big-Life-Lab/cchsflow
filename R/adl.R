# ==============================================================================
# Enhanced ADL Functions - Following v3.0.0 Development Guide
# ==============================================================================

# REQUIRED DEPENDENCIES:
#   library(haven)   # for haven::tagged_na() and haven::is_tagged_na()
#   library(dplyr)   # for dplyr::case_when() and dplyr::if_else()
#   source("R/missing-data-helpers.R")  # for preprocessing functions

# ==============================================================================
# 1. CONSTANTS AND CONFIGURATION
# ==============================================================================

# ADL validation constants
ADL_VALID_RESPONSES <- c(1, 2) # 1 = needs help, 2 = does not need help
ADL_ITEM_NAMES <- list(
  five_item = c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05"),
  six_item = c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_06")
)

# ==============================================================================
# 2. CORE UTILITY FUNCTIONS
# ==============================================================================

#' Enhanced ADL input validation
#' @param param_name Parameter name for error messages
#' @param param_value Parameter value to validate
#' @param required Logical indicating if parameter is required
#' @return Validated parameter or error
#' @noRd
validate_adl_parameter <- function(param_name, param_value, required = TRUE) {
  if (required && missing(param_value)) {
    stop(paste("Required ADL parameter", param_name, "must be provided"), call. = FALSE)
  }
  return(param_value)
}

#' Check vector length compatibility for ADL inputs
#' @param ... Input vectors to check
#' @return Logical indicating compatibility
#' @noRd
check_adl_length_compatibility <- function(...) {
  lengths <- lengths(list(...))
  # All lengths must be equal OR some can be length 1 (scalar)
  return(length(unique(lengths[lengths > 1])) <= 1)
}

#' Preprocess single ADL variable with comprehensive missing data handling
#' @param adl_var Single ADL variable
#' @return Preprocessed ADL variable with haven::tagged_na()
#' @noRd
preprocess_adl_variable <- function(adl_var) {
  dplyr::case_when(
    # Handle original CCHS missing codes
    adl_var == 6 ~ haven::tagged_na("a"), # Not applicable
    adl_var %in% c(7, 8, 9) ~ haven::tagged_na("b"), # Don't know, refusal, not stated

    # Handle string-based missing values (legacy support)
    is.character(adl_var) & adl_var == "NA(a)" ~ haven::tagged_na("a"),
    is.character(adl_var) & adl_var == "NA(b)" ~ haven::tagged_na("b"),
    is.character(adl_var) & adl_var %in% c("Not applicable", "not applicable") ~ haven::tagged_na("a"),
    is.character(adl_var) & adl_var %in% c("Missing", "Don't know", "Refusal") ~ haven::tagged_na("b"),

    # Handle existing haven::tagged_na (passthrough)
    haven::is_tagged_na(adl_var, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(adl_var, "b") ~ haven::tagged_na("b"),

    # Handle regular NAs
    is.na(adl_var) ~ haven::tagged_na("b"),

    # Valid responses (1, 2) pass through
    adl_var %in% ADL_VALID_RESPONSES ~ as.numeric(adl_var),

    # Invalid responses become missing
    .default = haven::tagged_na("b")
  )
}

# ==============================================================================
# 3. SPECIALIZED HELPER FUNCTIONS
# ==============================================================================

#' Calculate ADL score with comprehensive missing data propagation
#' @param adl_list List of preprocessed ADL variables
#' @param total_items Total number of ADL items (5 or 6)
#' @return ADL score with proper missing data handling
#' @noRd
calculate_adl_score <- function(adl_list, total_items) {
  # Check for any not applicable values (highest priority)
  has_not_applicable <- any(sapply(adl_list, function(x) haven::is_tagged_na(x, "a")))

  # Check for any missing values
  has_missing <- any(sapply(adl_list, function(x) haven::is_tagged_na(x, "b") | is.na(x)))

  # Count valid "needs help" responses (value = 1)
  valid_responses <- sapply(adl_list, function(x) !haven::is_tagged_na(x) & !is.na(x))
  help_needed_count <- sum(sapply(adl_list, function(x) {
    if (!haven::is_tagged_na(x) & !is.na(x)) x == 1 else FALSE
  }))

  # Return score based on missing data priority
  dplyr::case_when(
    has_not_applicable ~ haven::tagged_na("a"),
    has_missing ~ haven::tagged_na("b"),
    all(valid_responses) ~ as.numeric(help_needed_count),
    .default = haven::tagged_na("b")
  )
}

#' Create ADL binary indicator with enhanced logic
#' @param adl_list List of preprocessed ADL variables
#' @return Binary ADL indicator (1 = needs help, 2 = no help needed)
#' @noRd
calculate_adl_binary <- function(adl_list) {
  # Check for any not applicable values
  has_not_applicable <- any(sapply(adl_list, function(x) haven::is_tagged_na(x, "a")))

  # Check for any missing values
  has_missing <- any(sapply(adl_list, function(x) haven::is_tagged_na(x, "b") | is.na(x)))

  # Check if any ADL requires help (value = 1)
  needs_help <- any(sapply(adl_list, function(x) {
    if (!haven::is_tagged_na(x) & !is.na(x)) x == 1 else FALSE
  }))

  # Check if all ADLs are "no help needed" (value = 2)
  all_no_help <- all(sapply(adl_list, function(x) {
    if (!haven::is_tagged_na(x) & !is.na(x)) x == 2 else FALSE
  }))

  # Return binary indicator
  dplyr::case_when(
    has_not_applicable ~ haven::tagged_na("a"),
    has_missing ~ haven::tagged_na("b"),
    needs_help ~ 1L,
    all_no_help ~ 2L,
    .default = haven::tagged_na("b")
  )
}

# ==============================================================================
# 4. PUBLIC API FUNCTIONS (Enhanced)
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
#' # Basic usage - needs help with one task
#' assess_adl(1, 2, 2, 2, 2) # Returns: 1
#'
#' # No help needed with any task
#' assess_adl(2, 2, 2, 2, 2) # Returns: 2
#'
#' # Vector processing with missing data
#' assess_adl(c(1, 2, 6), c(2, 2, 7), c(2, 2, 2), c(2, 2, 2), c(2, 2, 2))
#'
#' # Integration with rec_with_table()
#' \dontrun{
#' result <- rec_with_table(cchs2010_p,
#'   variables = c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_der")
#' )
#' }
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
assess_adl <- function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05) {
  # 1. Enhanced input validation
  validate_adl_parameter("ADL_01", ADL_01, required = TRUE)
  validate_adl_parameter("ADL_02", ADL_02, required = TRUE)
  validate_adl_parameter("ADL_03", ADL_03, required = TRUE)
  validate_adl_parameter("ADL_04", ADL_04, required = TRUE)
  validate_adl_parameter("ADL_05", ADL_05, required = TRUE)

  # 2. Length compatibility check
  if (!check_adl_length_compatibility(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05)) {
    stop("Input vectors must have compatible lengths", call. = FALSE)
  }

  # 3. Handle edge cases gracefully
  if (all(is.na(c(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05)))) {
    warning("All ADL inputs are missing. Results will be entirely NA.", call. = FALSE)
  }

  # 4. Preprocess all ADL variables
  adl_01_processed <- preprocess_adl_variable(ADL_01)
  adl_02_processed <- preprocess_adl_variable(ADL_02)
  adl_03_processed <- preprocess_adl_variable(ADL_03)
  adl_04_processed <- preprocess_adl_variable(ADL_04)
  adl_05_processed <- preprocess_adl_variable(ADL_05)

  # 5. Calculate binary ADL indicator
  adl_list <- list(
    adl_01_processed, adl_02_processed, adl_03_processed,
    adl_04_processed, adl_05_processed
  )

  calculate_adl_binary(adl_list)
}

#' Enhanced ADL help score (count of tasks requiring help)
#'
#' @description
#' **Purpose**: Count number of ADL tasks requiring help (0-5 scale) for granular functional assessment
#' **Method**: Sums "needs help" responses across 5 consistent ADL items
#' **Clinical Context**: Graduated disability measure for research and clinical assessment
#'
#' @param ADL_01,ADL_02,ADL_03,ADL_04,ADL_05 ADL variables as in assess_adl()
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
#' # Scalar usage - No help needed (score = 0)
#' score_adl(2, 2, 2, 2, 2)
#'
#' # Scalar usage - Help needed with 2 tasks (score = 2)
#' score_adl(1, 2, 1, 2, 2)
#'
#' # Scalar usage - Complete dependence (score = 5)
#' score_adl(1, 1, 1, 1, 1)
#'
#' # Vector processing with missing data and outliers
#' adl_01 <- c(1, 2, 2, 6, "NA(a)") # Includes not applicable and missing
#' adl_02 <- c(2, 2, 1, 2, 7) # Includes don't know
#' adl_03 <- c(2, 2, 2, 2, 2)
#' adl_04 <- c(2, 2, 2, 2, 2)
#' adl_05 <- c(2, 2, 2, 2, 2)
#' score_adl(adl_01, adl_02, adl_03, adl_04, adl_05)
#'
#' # rec_with_table() integration for production use
#' \dontrun{
#' library(cchsflow)
#' result <- rec_with_table(
#'   data = cchs2015_2016_p,
#'   variables = "ADL_score_5",
#'   database_name = "cchs2015_2016_p"
#' )
#' head(result$ADL_score_5)
#' }
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced scoring with comprehensive missing data validation
#' @export
score_adl <- function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05) {
  # 1. Input validation (reuse from binary function)
  validate_adl_parameter("ADL_01", ADL_01, required = TRUE)
  validate_adl_parameter("ADL_02", ADL_02, required = TRUE)
  validate_adl_parameter("ADL_03", ADL_03, required = TRUE)
  validate_adl_parameter("ADL_04", ADL_04, required = TRUE)
  validate_adl_parameter("ADL_05", ADL_05, required = TRUE)

  # 2. Length compatibility check
  if (!check_adl_length_compatibility(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05)) {
    stop("Input vectors must have compatible lengths", call. = FALSE)
  }

  # 3. Preprocess all ADL variables
  adl_01_processed <- preprocess_adl_variable(ADL_01)
  adl_02_processed <- preprocess_adl_variable(ADL_02)
  adl_03_processed <- preprocess_adl_variable(ADL_03)
  adl_04_processed <- preprocess_adl_variable(ADL_04)
  adl_05_processed <- preprocess_adl_variable(ADL_05)

  # 4. Calculate ADL score
  adl_list <- list(
    adl_01_processed, adl_02_processed, adl_03_processed,
    adl_04_processed, adl_05_processed
  )

  calculate_adl_score(adl_list, total_items = 5)
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
#' # Scalar usage - Independent across all 6 tasks
#' score_adl_6(2, 2, 2, 2, 2, 2)
#'
#' # Scalar usage - Help needed with 3 tasks including finances
#' score_adl_6(1, 2, 1, 2, 2, 1)
#'
#' # Vector processing with missing data
#' adl_01 <- c(1, 2, 2, "NA(a)") # Mixed responses with not applicable
#' adl_02 <- c(2, 2, 1, 7) # Includes don't know
#' adl_03 <- c(2, 2, 2, 2)
#' adl_04 <- c(2, 2, 2, 2)
#' adl_05 <- c(2, 2, 2, 2)
#' adl_06 <- c(1, 2, 1, 8) # Finances task with refusal
#' score_adl_6(adl_01, adl_02, adl_03, adl_04, adl_05, adl_06)
#'
#' # rec_with_table() integration for DemPoRT analysis
#' \dontrun{
#' library(cchsflow)
#' result <- rec_with_table(
#'   data = cchs2015_2016_p,
#'   variables = "ADL_score_6",
#'   database_name = "cchs2015_2016_p"
#' )
#' summary(result$ADL_score_6)
#' }
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced 6-item scoring for comprehensive functional assessment
#' @export
score_adl_6 <- function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05, ADL_06) {
  # 1. Enhanced input validation
  validate_adl_parameter("ADL_01", ADL_01, required = TRUE)
  validate_adl_parameter("ADL_02", ADL_02, required = TRUE)
  validate_adl_parameter("ADL_03", ADL_03, required = TRUE)
  validate_adl_parameter("ADL_04", ADL_04, required = TRUE)
  validate_adl_parameter("ADL_05", ADL_05, required = TRUE)
  validate_adl_parameter("ADL_06", ADL_06, required = TRUE)

  # 2. Length compatibility check
  if (!check_adl_length_compatibility(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05, ADL_06)) {
    stop("Input vectors must have compatible lengths", call. = FALSE)
  }

  # 3. Preprocess all ADL variables
  adl_01_processed <- preprocess_adl_variable(ADL_01)
  adl_02_processed <- preprocess_adl_variable(ADL_02)
  adl_03_processed <- preprocess_adl_variable(ADL_03)
  adl_04_processed <- preprocess_adl_variable(ADL_04)
  adl_05_processed <- preprocess_adl_variable(ADL_05)
  adl_06_processed <- preprocess_adl_variable(ADL_06)

  # 4. Calculate 6-item ADL score
  adl_list <- list(
    adl_01_processed, adl_02_processed, adl_03_processed,
    adl_04_processed, adl_05_processed, adl_06_processed
  )

  calculate_adl_score(adl_list, total_items = 6)
}
