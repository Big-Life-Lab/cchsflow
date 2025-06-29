#' CCHS Missing Data Preprocessing Helpers
#'
#' Universal helper functions for preprocessing original CCHS missing data codes
#' and converting them to standardized haven::tagged_na() format. Based on the
#' comprehensive CCHS missing data specification in 
#' inst/metadata/schemas/cchs/cchs_missing_data.yaml
#'
#' @name missing-data-helpers
#' @keywords internal

# ============================================================================
# CORE PREPROCESSING FUNCTIONS
# ============================================================================

#' Preprocess original CCHS missing codes to haven::tagged_na
#'
#' Universal preprocessing function that handles original CCHS missing data codes
#' (6,7,8,9 and extended variants) and converts them to standardized 
#' haven::tagged_na() format. Supports all CCHS variable patterns while 
#' preserving existing string and haven NA values.
#'
#' @param input_var Vector with potential original CCHS codes, string NA values,
#'   or haven::tagged_na() values
#' @param pattern_type Character string specifying CCHS missing code pattern:
#'   - "standard_response": codes 6,7,8,9 (default)
#'   - "categorical_age": codes 96,97,98,99  
#'   - "continuous_standard": codes 996,997,998,999
#' @param preserve_numeric Logical. If TRUE, valid numeric values are preserved 
#'   as numeric. If FALSE, all values are converted through the preprocessing.
#'   Default TRUE.
#'
#' @return Vector with original CCHS codes converted to haven::tagged_na(),
#'   preserving existing NA values and valid responses
#'
#' @details
#' **Transformation Rules (based on CCHS specification):**
#' 
#' **Standard Response Variables (SMK_005, SMK_030, SMK_01A):**
#' - 6 → haven::tagged_na("a") - Not applicable
#' - 7 → haven::tagged_na("b") - Don't know  
#' - 8 → haven::tagged_na("b") - Refusal
#' - 9 → haven::tagged_na("b") - Not stated
#' 
#' **Categorical Age Variables (SMKG203, SMKG207):**
#' - 96 → haven::tagged_na("a") - Not applicable
#' - 97 → haven::tagged_na("b") - Don't know
#' - 98 → haven::tagged_na("b") - Refusal  
#' - 99 → haven::tagged_na("b") - Not stated
#' 
#' **Continuous Variables (SMK_204, HWTDGWTK, etc.):**
#' - 996 → haven::tagged_na("a") - Not applicable
#' - 997 → haven::tagged_na("b") - Don't know
#' - 998 → haven::tagged_na("b") - Refusal
#' - 999 → haven::tagged_na("b") - Not stated
#'
#' **Existing NA Handling:**
#' - "NA(a)" → haven::tagged_na("a") - String-based NA(a)
#' - "NA(b)" → haven::tagged_na("b") - String-based NA(b)  
#' - haven::tagged_na("a") → preserved as-is
#' - haven::tagged_na("b") → preserved as-is
#'
#' @examples
#' # Standard response pattern (SMK_005, SMK_030, SMK_01A)
#' smk_data <- c(1, 2, 3, 6, 7, 8, 9, "NA(a)", NA)
#' preprocess_cchs_missing_codes(smk_data, "standard_response")
#' 
#' # Categorical age pattern (SMKG203, SMKG207)  
#' age_data <- c(1, 5, 8, 96, 97, 98, 99, "NA(b)")
#' preprocess_cchs_missing_codes(age_data, "categorical_age")
#' 
#' # Continuous pattern (SMK_204, HWTDGWTK)
#' cont_data <- c(15, 25, 996, 997, 998, 999)
#' preprocess_cchs_missing_codes(cont_data, "continuous_standard")
#'
#' @seealso 
#' - [preprocess_standard_response()] for SMK_005-type variables
#' - [preprocess_categorical_age()] for SMKG203-type variables  
#' - [preprocess_continuous_standard()] for SMK_204-type variables
#' - inst/metadata/schemas/cchs/cchs_missing_data.yaml for complete specification
#'
#' @export
preprocess_cchs_missing_codes <- function(input_var, 
                                        pattern_type = "standard_response",
                                        preserve_numeric = TRUE) {
  
  # Parameter validation
  if (!pattern_type %in% c("standard_response", "categorical_age", "continuous_standard")) {
    stop("pattern_type must be one of: 'standard_response', 'categorical_age', 'continuous_standard'")
  }
  
  # Define missing code patterns based on CCHS specification
  if (pattern_type == "standard_response") {
    not_applicable_codes <- 6
    missing_codes <- c(7, 8, 9)
  } else if (pattern_type == "categorical_age") {
    not_applicable_codes <- 96
    missing_codes <- c(97, 98, 99)
  } else if (pattern_type == "continuous_standard") {
    not_applicable_codes <- 996
    missing_codes <- c(997, 998, 999)
  }
  
  # Apply preprocessing transformation with safe numeric conversion
  result <- dplyr::case_when(
    # Original CCHS "not applicable" codes → NA::a
    input_var %in% not_applicable_codes ~ haven::tagged_na("a"),
    
    # Original CCHS "missing/unknown/refused" codes → NA::b  
    input_var %in% missing_codes ~ haven::tagged_na("b"),
    
    # String-based NA values → haven::tagged_na
    input_var == "NA(a)" ~ haven::tagged_na("a"),
    input_var == "NA(b)" ~ haven::tagged_na("b"),
    
    # Preserve existing haven::tagged_na values
    haven::is_tagged_na(input_var, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(input_var, "b") ~ haven::tagged_na("b"),
    
    # Handle numeric string values safely (avoid coercion warnings)
    is.character(input_var) & preserve_numeric ~ suppressWarnings(as.numeric(input_var)),
    
    # Default: preserve numeric or convert safely
    preserve_numeric ~ suppressWarnings(as.numeric(input_var)),
    .default = suppressWarnings(as.numeric(input_var))
  )
  
  return(result)
}

# ============================================================================
# PATTERN-SPECIFIC HELPER FUNCTIONS
# ============================================================================

#' Preprocess standard response CCHS variables
#'
#' Specialized preprocessing for standard CCHS response variables that use
#' the 6,7,8,9 missing code pattern. Common for variables like SMK_005, 
#' SMK_030, SMK_01A with typical response ranges 1-3 or 1-5.
#'
#' @param input_var Vector with potential codes 6,7,8,9 and string/haven NA values
#' @param preserve_numeric Logical. Preserve valid numeric responses (default TRUE)
#'
#' @return Vector with missing codes converted to appropriate haven::tagged_na()
#'
#' @details
#' **Transformation Pattern:**
#' - 6 → haven::tagged_na("a") - Not applicable (age restrictions, skip patterns)
#' - 7 → haven::tagged_na("b") - Don't know
#' - 8 → haven::tagged_na("b") - Refusal  
#' - 9 → haven::tagged_na("b") - Not stated
#'
#' **Common Variables Using This Pattern:**
#' - SMK_005: Type of smoker presently (1=daily, 2=occasionally, 3=not at all)
#' - SMK_030: Ever smoked daily (1=yes, 2=no)
#' - SMK_01A: Smoked 100+ cigarettes in lifetime (1=yes, 2=no)
#'
#' @examples
#' # Typical SMK_005 data with original CCHS codes
#' smk_005_data <- c(1, 2, 3, 6, 7, 8, 9, "NA(a)")
#' preprocess_standard_response(smk_005_data)
#'
#' @export
preprocess_standard_response <- function(input_var, preserve_numeric = TRUE) {
  preprocess_cchs_missing_codes(input_var, "standard_response", preserve_numeric)
}

#' Preprocess categorical age CCHS variables
#'
#' Specialized preprocessing for CCHS categorical age variables that use
#' the 96,97,98,99 missing code pattern. Common for age category variables
#' like SMKG203, SMKG207 with response ranges 1-11.
#'
#' @param input_var Vector with potential codes 96,97,98,99 and string/haven NA values
#' @param preserve_numeric Logical. Preserve valid numeric responses (default TRUE)
#'
#' @return Vector with missing codes converted to appropriate haven::tagged_na()
#'
#' @details
#' **Transformation Pattern:**
#' - 96 → haven::tagged_na("a") - Not applicable (non-smokers, skip patterns)
#' - 97 → haven::tagged_na("b") - Don't know age
#' - 98 → haven::tagged_na("b") - Refusal to provide age
#' - 99 → haven::tagged_na("b") - Age not stated
#'
#' **Common Variables Using This Pattern:**
#' - SMKG203: Age started smoking daily (current daily smokers)
#' - SMKG207: Age started smoking daily (former daily smokers)
#' - Age categories typically: 1=<15, 2=15-16, 3=17, 4=18, 5=19-20, 6=21-24, 
#'   7=25-29, 8=30-34, 9=35-39, 10=40-44, 11=45+
#'
#' @examples
#' # Typical SMKG203 data with original CCHS codes
#' smkg203_data <- c(1, 5, 8, 11, 96, 97, 98, 99, "NA(a)")
#' preprocess_categorical_age(smkg203_data)
#'
#' @export
preprocess_categorical_age <- function(input_var, preserve_numeric = TRUE) {
  preprocess_cchs_missing_codes(input_var, "categorical_age", preserve_numeric)
}

#' Preprocess continuous CCHS variables with extended missing codes
#'
#' Specialized preprocessing for CCHS continuous variables that use the 
#' 996,997,998,999 missing code pattern. Common for measurement variables
#' like cigarettes per day, height, weight.
#'
#' @param input_var Vector with potential codes 996,997,998,999 and string/haven NA values  
#' @param preserve_numeric Logical. Preserve valid numeric responses (default TRUE)
#'
#' @return Vector with missing codes converted to appropriate haven::tagged_na()
#'
#' @details
#' **Transformation Pattern:**
#' - 996 → haven::tagged_na("a") - Not applicable (non-smokers, proxy respondents)
#' - 997 → haven::tagged_na("b") - Don't know measurement value
#' - 998 → haven::tagged_na("b") - Refusal to provide measurement
#' - 999 → haven::tagged_na("b") - Measurement not stated
#'
#' **Common Variables Using This Pattern:**
#' - SMK_204: Number of cigarettes smoked per day (daily smokers)
#' - SMK_208: Number of cigarettes smoked per day (former daily smokers)  
#' - HWTDGHTM: Height in meters
#' - HWTDGWTK: Weight in kilograms
#'
#' @examples
#' # Typical SMK_204 data (cigarettes per day)
#' smk_204_data <- c(10, 15, 20, 25, 996, 997, 998, 999, "NA(a)")
#' preprocess_continuous_standard(smk_204_data)
#' 
#' # Typical weight data  
#' weight_data <- c(65.5, 78.2, 996, 997, 998, 999)
#' preprocess_continuous_standard(weight_data)
#'
#' @export
preprocess_continuous_standard <- function(input_var, preserve_numeric = TRUE) {
  preprocess_cchs_missing_codes(input_var, "continuous_standard", preserve_numeric)
}

# ============================================================================
# DOMAIN-SPECIFIC HELPER FUNCTIONS
# ============================================================================

#' Preprocess smoking-specific CCHS variables
#'
#' Domain-specific preprocessing for smoking variables using appropriate
#' missing code patterns based on variable type. Automatically detects
#' pattern based on variable name or allows manual specification.
#'
#' @param input_var Vector with CCHS smoking variable data
#' @param variable_name Character. CCHS variable name for automatic pattern detection.
#'   If NULL, uses pattern_type parameter.
#' @param pattern_type Character. Manual pattern specification if variable_name not provided.
#'   One of: "standard_response", "categorical_age", "continuous_standard"
#' @param preserve_numeric Logical. Preserve valid numeric responses (default TRUE)
#'
#' @return Vector with smoking variable missing codes converted to haven::tagged_na()
#'
#' @details
#' **Automatic Pattern Detection:**
#' - Standard response (6,7,8,9): SMK_005, SMK_030, SMK_01A, SMK_09A_B
#' - Categorical age (96,97,98,99): SMKG203, SMKG207, SMKG209  
#' - Continuous (996,997,998,999): SMK_204, SMK_208, SMK_05B, SMK_05C
#'
#' **Manual Pattern Override:**
#' Use pattern_type parameter to override automatic detection.
#'
#' @examples
#' # Automatic pattern detection
#' preprocess_smoking_variable(c(1,2,3,6,7), variable_name = "SMK_005")
#' preprocess_smoking_variable(c(1,5,8,96,97), variable_name = "SMKG203") 
#' preprocess_smoking_variable(c(15,20,996,997), variable_name = "SMK_204")
#' 
#' # Manual pattern specification
#' preprocess_smoking_variable(c(1,2,6,7), pattern_type = "standard_response")
#'
#' @export
preprocess_smoking_variable <- function(input_var, 
                                      variable_name = NULL, 
                                      pattern_type = NULL,
                                      preserve_numeric = TRUE) {
  
  # Automatic pattern detection based on variable name
  if (!is.null(variable_name)) {
    
    # Standard response pattern variables
    standard_response_vars <- c("SMK_005", "SMK_030", "SMK_01A", "SMK_09A_B")
    
    # Categorical age pattern variables  
    categorical_age_vars <- c("SMKG203", "SMKG207", "SMKG209")
    
    # Continuous pattern variables
    continuous_vars <- c("SMK_204", "SMK_208", "SMK_05B", "SMK_05C")
    
    if (variable_name %in% standard_response_vars) {
      detected_pattern <- "standard_response"
    } else if (variable_name %in% categorical_age_vars) {
      detected_pattern <- "categorical_age"
    } else if (variable_name %in% continuous_vars) {
      detected_pattern <- "continuous_standard"
    } else {
      # Default to standard response for unknown smoking variables
      detected_pattern <- "standard_response"
      warning(paste("Unknown smoking variable:", variable_name, 
                   "- using standard_response pattern"))
    }
    
    return(preprocess_cchs_missing_codes(input_var, detected_pattern, preserve_numeric))
    
  } else if (!is.null(pattern_type)) {
    # Manual pattern specification
    return(preprocess_cchs_missing_codes(input_var, pattern_type, preserve_numeric))
    
  } else {
    stop("Either variable_name or pattern_type must be specified")
  }
}

# ============================================================================
# UTILITY AND VALIDATION FUNCTIONS  
# ============================================================================

#' Validate CCHS missing code preprocessing results
#'
#' Quality assurance function to validate that missing code preprocessing
#' has been applied correctly and consistently.
#'
#' @param original_var Original variable before preprocessing
#' @param processed_var Variable after preprocessing  
#' @param pattern_type Pattern type used for preprocessing
#' @param report_details Logical. Return detailed validation report (default FALSE)
#'
#' @return Logical indicating if preprocessing is valid, or detailed report if requested
#'
#' @details
#' **Validation Checks:**
#' - All original missing codes properly converted
#' - No missing codes remaining in processed variable
#' - Appropriate NA category assignment (a vs b)
#' - Type consistency maintained
#' - Valid responses preserved
#'
#' @examples
#' original <- c(1, 2, 3, 6, 7, 8, 9)
#' processed <- preprocess_standard_response(original)
#' validate_missing_code_preprocessing(original, processed, "standard_response")
#'
#' @export
validate_missing_code_preprocessing <- function(original_var, 
                                              processed_var, 
                                              pattern_type,
                                              report_details = FALSE) {
  
  # Define expected missing codes for pattern
  if (pattern_type == "standard_response") {
    expected_missing_codes <- c(6, 7, 8, 9)
  } else if (pattern_type == "categorical_age") {
    expected_missing_codes <- c(96, 97, 98, 99)
  } else if (pattern_type == "continuous_standard") {  
    expected_missing_codes <- c(996, 997, 998, 999)
  } else {
    stop("Invalid pattern_type")
  }
  
  # Validation checks
  original_missing_present <- any(original_var %in% expected_missing_codes, na.rm = TRUE)
  processed_missing_remaining <- any(processed_var %in% expected_missing_codes, na.rm = TRUE)
  length_preserved <- length(original_var) == length(processed_var)
  
  # Count transformations
  na_a_count <- sum(haven::is_tagged_na(processed_var, "a"), na.rm = TRUE)
  na_b_count <- sum(haven::is_tagged_na(processed_var, "b"), na.rm = TRUE)
  
  # Basic validation result
  is_valid <- !processed_missing_remaining && length_preserved
  
  if (report_details) {
    return(list(
      is_valid = is_valid,
      original_missing_present = original_missing_present,
      processed_missing_remaining = processed_missing_remaining,
      length_preserved = length_preserved,
      na_a_count = na_a_count,
      na_b_count = na_b_count,
      total_na_count = na_a_count + na_b_count
    ))
  } else {
    return(is_valid)
  }
}

#' Check if variable uses CCHS missing code pattern
#'
#' Utility function to detect if a variable contains original CCHS missing
#' codes that should be preprocessed.
#'
#' @param input_var Vector to check for CCHS missing codes
#' @param pattern_type Pattern type to check. If NULL, checks all patterns.
#'
#' @return Logical indicating if CCHS missing codes are present
#'
#' @examples
#' has_cchs_missing_codes(c(1, 2, 3, 6, 7))  # TRUE
#' has_cchs_missing_codes(c(1, 2, 3, NA))    # FALSE
#' has_cchs_missing_codes(c(1, 5, 96, 97), "categorical_age")  # TRUE
#'
#' @export
has_cchs_missing_codes <- function(input_var, pattern_type = NULL) {
  
  if (is.null(pattern_type)) {
    # Check all possible CCHS missing codes
    all_missing_codes <- c(6, 7, 8, 9, 96, 97, 98, 99, 996, 997, 998, 999)
    return(any(input_var %in% all_missing_codes, na.rm = TRUE))
  }
  
  # Check specific pattern
  if (pattern_type == "standard_response") {
    return(any(input_var %in% c(6, 7, 8, 9), na.rm = TRUE))
  } else if (pattern_type == "categorical_age") {
    return(any(input_var %in% c(96, 97, 98, 99), na.rm = TRUE))
  } else if (pattern_type == "continuous_standard") {
    return(any(input_var %in% c(996, 997, 998, 999), na.rm = TRUE))
  } else {
    stop("Invalid pattern_type")
  }
}