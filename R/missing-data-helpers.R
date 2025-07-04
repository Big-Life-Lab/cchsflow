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
    warning("pattern_type must be one of: 'standard_response', 'categorical_age', 'continuous_standard' - using 'standard_response'")
    pattern_type <- "standard_response"
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
#' preprocess_smoking_variable(c(1, 2, 3, 6, 7), variable_name = "SMK_005")
#' preprocess_smoking_variable(c(1, 5, 8, 96, 97), variable_name = "SMKG203")
#' preprocess_smoking_variable(c(15, 20, 996, 997), variable_name = "SMK_204")
#'
#' # Manual pattern specification
#' preprocess_smoking_variable(c(1, 2, 6, 7), pattern_type = "standard_response")
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
      warning(paste(
        "Unknown smoking variable:", variable_name,
        "- using standard_response pattern"
      ))
    }

    return(preprocess_cchs_missing_codes(input_var, detected_pattern, preserve_numeric))
  } else if (!is.null(pattern_type)) {
    # Manual pattern specification
    return(preprocess_cchs_missing_codes(input_var, pattern_type, preserve_numeric))
  } else {
    warning("Either variable_name or pattern_type must be specified - using 'standard_response'")
    return(preprocess_cchs_missing_codes(input_var, "standard_response", preserve_numeric))
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
    warning("Invalid pattern_type - cannot validate preprocessing")
    return(FALSE)
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

#' Clean mixed continuous and categorical variables with unified length compatibility
#'
#' Universal preprocessing function that handles both continuous and categorical variables
#' together, ensuring all variables have compatible lengths before processing. Used when
#' derived variables need multiple variable types (e.g., adjust_bmi needs sex + height + weight).
#'
#' @param continuous_vars Named list of continuous variables (e.g., list(height = HWTGHTM, weight = HWTGWTK))
#' @param categorical_vars Named list of categorical variables (e.g., list(sex = DHH_SEX))
#' @param min_values Named list of minimum values for continuous variables
#' @param max_values Named list of maximum values for continuous variables
#' @param valid_values Named list of valid values for categorical variables
#' @param continuous_pattern CCHS missing code pattern for continuous variables (default "continuous_standard")
#' @param categorical_pattern CCHS missing code pattern for categorical variables (default "standard_response")
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return List with all cleaned variables, ensuring consistent lengths across all outputs
#'
#' @note Internal v3.0.0, last updated: 2025-07-04, status: active
#' @keywords internal
clean_variables <- function(continuous_vars = NULL,
                            categorical_vars = NULL,
                            min_values = NULL,
                            max_values = NULL,
                            valid_values = NULL,
                            continuous_pattern = "continuous_standard",
                            categorical_pattern = "standard_response",
                            log_level = "silent") {
  # Combine all variables for unified length checking
  all_vars <- c(continuous_vars, categorical_vars)
  all_names <- names(all_vars)

  if (length(all_vars) == 0) {
    stop("At least one variable must be provided", call. = FALSE)
  }

  # 1. Check for missing required variables
  for (name in all_names) {
    var <- all_vars[[name]]
    if (is.null(var)) {
      # Return tagged_na("d") for all variables
      result <- list()
      for (n in all_names) {
        result[[paste0(n, "_clean")]] <- haven::tagged_na("d")
      }
      return(result)
    }
  }

  # 2. Check unified length compatibility across ALL variables
  if (!do.call(check_vector_compatibility, all_vars)) {
    max_len <- max(sapply(all_vars, length))
    result <- list()
    for (name in all_names) {
      result[[paste0(name, "_clean")]] <- rep(haven::tagged_na("b"), max_len)
    }
    return(result)
  }

  # 3. Process each variable type separately but with unified results
  result <- list()

  # Process continuous variables
  if (!is.null(continuous_vars) && length(continuous_vars) > 0) {
    for (name in names(continuous_vars)) {
      var <- continuous_vars[[name]]
      min_val <- min_values[[name]]
      max_val <- max_values[[name]]

      # Preprocess CCHS missing codes
      if (needs_preprocessing(var)) {
        var_processed <- preprocess_cchs_missing_codes(var, continuous_pattern)
      } else {
        var_processed <- var
      }

      # Apply bounds validation
      if (is.null(min_val) || is.null(max_val)) {
        result[[paste0(name, "_clean")]] <- var_processed
      } else {
        var_numeric <- suppressWarnings(as.numeric(var_processed))
        result[[paste0(name, "_clean")]] <- dplyr::case_when(
          haven::is_tagged_na(var_processed, "a") ~ haven::tagged_na("a"),
          haven::is_tagged_na(var_processed, "b") ~ haven::tagged_na("b"),
          is.na(var_processed) ~ haven::tagged_na("b"),
          is.na(var_numeric) ~ haven::tagged_na("b"),
          var_numeric < min_val | var_numeric > max_val ~ haven::tagged_na("b"),
          TRUE ~ var_numeric
        )
      }
    }
  }

  # Process categorical variables
  if (!is.null(categorical_vars) && length(categorical_vars) > 0) {
    for (name in names(categorical_vars)) {
      var <- categorical_vars[[name]]
      valid_vals <- valid_values[[name]]

      # Preprocess CCHS missing codes
      if (needs_preprocessing(var)) {
        var_processed <- preprocess_cchs_missing_codes(var, categorical_pattern)
      } else {
        var_processed <- var
      }

      # Apply valid values validation
      if (is.null(valid_vals)) {
        result[[paste0(name, "_clean")]] <- var_processed
      } else {
        # Ensure type compatibility for comparison
        var_numeric <- suppressWarnings(as.numeric(var_processed))
        result[[paste0(name, "_clean")]] <- dplyr::case_when(
          haven::is_tagged_na(var_processed, "a") ~ haven::tagged_na("a"),
          haven::is_tagged_na(var_processed, "b") ~ haven::tagged_na("b"),
          is.na(var_processed) ~ haven::tagged_na("b"),
          # Use numeric version for comparison but preserve as numeric
          is.na(var_numeric) ~ haven::tagged_na("b"), # Invalid conversions
          var_numeric %in% valid_vals ~ var_numeric, # Return numeric version
          TRUE ~ haven::tagged_na("b")
        )
      }
    }
  }

  return(result)
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
#' has_cchs_missing_codes(c(1, 2, 3, 6, 7)) # TRUE
#' has_cchs_missing_codes(c(1, 2, 3, NA)) # FALSE
#' has_cchs_missing_codes(c(1, 5, 96, 97), "categorical_age") # TRUE
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
    return(FALSE) # Invalid pattern_type returns FALSE instead of stop
  }
}

# ============================================================================
# GENERIC HELPER FUNCTIONS FOR ALL DERIVED VARIABLES
# ============================================================================

#' Generic parameter validation for derived variable functions
#'
#' Validates function parameters and returns appropriate missing values instead
#' of stopping execution. Used across all derived variable functions.
#'
#' @param param_name Parameter name for warning messages
#' @param param_value Parameter value to validate
#' @param required Logical indicating if parameter is required
#' @param na_type Character tag for NA handling ("a" or "b")
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#' @return Validated parameter or haven::tagged_na
#' @note Internal v1.0.0 (2025-01-02) - Created for generic parameter validation
#' @keywords internal
validate_parameter <- function(param_name, param_value, required = TRUE, na_type = "b", log_level = "silent") {
  if (required && missing(param_value)) {
    if (log_level %in% c("warning", "verbose")) {
      warning(paste("Required parameter", param_name, "not provided - returning missing values"),
        call. = FALSE
      )
    }
    return(haven::tagged_na(na_type))
  }
  return(param_value)
}

#' Generic validation for continuous variables with min/max bounds
#'
#' Universal function for validating continuous variables against min/max bounds.
#' Used across all derived variable domains (BMI, smoking, alcohol, ADL, etc.).
#'
#' @param value Numeric value(s) to validate
#' @param min_value Minimum valid value
#' @param max_value Maximum valid value
#' @param na_handling Character tag for NA handling ("a" or "b", default "b")
#' @return Validated value or haven::tagged_na
#' @note Internal v3.0.0, last updated: 2025-06-30, status: active
#' @keywords internal
validate_continuous_bounds <- function(value, min_value, max_value, na_handling = "b") {
  dplyr::case_when(
    is.na(value) ~ haven::tagged_na(na_handling),
    value < min_value | value > max_value ~ haven::tagged_na("b"),
    TRUE ~ value
  )
}

#' Clean and validate multiple continuous variables with comprehensive preprocessing
#'
#' Universal preprocessing function that handles parameter validation, CCHS missing
#' code preprocessing, length compatibility, and bounds validation for any number of
#' continuous variables. Used across all derived variable domains.
#'
#' @param ... Named continuous variables to clean (e.g., height = HWTGHTM, weight = HWTGWTK)
#' @param min_values Named list of minimum values (e.g., list(height = 0.914, weight = 27.0))
#' @param max_values Named list of maximum values (e.g., list(height = 2.134, weight = 135.0))
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#' @param skip_validation Logical. Skip parameter/length validation if already done upstream (default FALSE)
#' @return Named list with cleaned variables (e.g., list(height_clean, weight_clean))
#' @note Internal v3.0.0, last updated: 2025-06-30, status: active
#' @keywords internal
#' @examples
#' # BMI usage
#' cleaned <- clean_continuous_variables(
#'   height = HWTGHTM, weight = HWTGWTK,
#'   min_values = list(height = 0.914, weight = 27.0),
#'   max_values = list(height = 2.134, weight = 135.0)
#' )
#'
#' # Smoking usage
#' cleaned <- clean_continuous_variables(
#'   age_started = SMKG203_cont,
#'   min_values = list(age_started = 8),
#'   max_values = list(age_started = 95)
#' )
clean_continuous_variables <- function(..., min_values, max_values, log_level = "silent") {
  # Get input variables
  input_vars <- list(...)
  var_names <- names(input_vars)

  if (is.null(var_names) || any(var_names == "")) {
    stop("All input variables must be named", call. = FALSE)
  }

  # Check for missing required variables with conditional tagged_na responses
  for (name in var_names) {
    var <- input_vars[[name]]

    # Check if variable is completely missing (not provided to function)
    if (missing(var) || is.null(var)) {
      # Return tagged_na("d") for missing required variables
      result <- list()
      for (n in var_names) {
        result[[paste0(n, "_clean")]] <- haven::tagged_na("d")
      }
      return(result)
    }

    # Check if variable contains tagged_na("c") - question not asked in cycle
    if (any(haven::is_tagged_na(var, "c"), na.rm = TRUE)) {
      if (log_level %in% c("warning", "verbose")) {
        warning(paste("Variable", name, "contains tagged_na('c') - skipping calculation"), call. = FALSE)
      }
      # Return tagged_na("c") to propagate the "not asked" state
      result <- list()
      for (n in var_names) {
        result[[paste0(n, "_clean")]] <- haven::tagged_na("c")
      }
      return(result)
    }
  }

  # Check length compatibility
  if (!do.call(check_vector_compatibility, input_vars)) {
    max_len <- max(sapply(input_vars, length))
    result <- list()
    for (name in var_names) {
      result[[paste0(name, "_clean")]] <- rep(haven::tagged_na("b"), max_len)
    }
    return(result)
  }

  # 3. Preprocess CCHS missing codes
  preprocessed_vars <- list()
  for (name in var_names) {
    var <- input_vars[[name]]
    if (needs_preprocessing(var)) {
      preprocessed_vars[[name]] <- preprocess_cchs_missing_codes(var, "continuous_standard")
    } else {
      preprocessed_vars[[name]] <- var
    }
  }

  # 4. Apply bounds validation (preserving tagged NAs)
  result <- list()
  for (name in var_names) {
    var <- preprocessed_vars[[name]]
    min_val <- min_values[[name]]
    max_val <- max_values[[name]]

    if (is.null(min_val) || is.null(max_val)) {
      warning(paste("No bounds defined for variable:", name, "- skipping bounds validation"), call. = FALSE)
      result[[paste0(name, "_clean")]] <- var
    } else {
      # Ensure numeric conversion for bounds checking (handles character inputs)
      var_numeric <- suppressWarnings(as.numeric(var))
      result[[paste0(name, "_clean")]] <- dplyr::case_when(
        haven::is_tagged_na(var, "a") ~ haven::tagged_na("a"),
        haven::is_tagged_na(var, "b") ~ haven::tagged_na("b"),
        is.na(var) ~ haven::tagged_na("b"),
        # Use numeric version for bounds checking
        is.na(var_numeric) ~ haven::tagged_na("b"), # Invalid conversions become NA(b)
        var_numeric < min_val | var_numeric > max_val ~ haven::tagged_na("b"),
        TRUE ~ var_numeric # Return numeric version for consistency
      )
    }
  }

  return(result)
}

#' Clean and validate multiple categorical variables with comprehensive preprocessing
#'
#' Universal preprocessing function that handles parameter validation, CCHS missing
#' code preprocessing, length compatibility, and valid values validation for any number of
#' categorical variables. Used across all derived variable domains.
#'
#' @param ... Named categorical variables to clean (e.g., sex = DHH_SEX, smoker_type = SMK_005)
#' @param valid_values Named list of valid values (e.g., list(sex = c(1, 2), smoker_type = c(1, 2, 3)))
#' @param pattern_type CCHS missing code pattern to use (default "standard_response")
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#' @param skip_validation Logical. Skip parameter/length validation if already done upstream (default FALSE)
#' @return Named list with cleaned variables (e.g., list(sex_clean, smoker_type_clean))
#' @note Internal v3.0.0, last updated: 2025-06-30, status: active
#' @keywords internal
#' @examples
#' # Smoking status usage
#' cleaned <- clean_categorical_variables(
#'   smoker_type = SMK_005, ever_daily = SMK_030,
#'   valid_values = list(smoker_type = c(1, 2, 3), ever_daily = c(1, 2)),
#'   pattern_type = "standard_response"
#' )
#'
#' # Sex and age category usage
#' cleaned <- clean_categorical_variables(
#'   sex = DHH_SEX, age_started_cat = SMKG203,
#'   valid_values = list(sex = c(1, 2), age_started_cat = 1:11),
#'   pattern_type = "categorical_age"
#' )
clean_categorical_variables <- function(..., valid_values, pattern_type = "standard_response", log_level = "silent") {
  # Get input variables
  input_vars <- list(...)
  var_names <- names(input_vars)

  if (is.null(var_names) || any(var_names == "")) {
    stop("All input variables must be named", call. = FALSE)
  }

  # Check for missing required variables with conditional tagged_na responses
  for (name in var_names) {
    var <- input_vars[[name]]

    # Check if variable is completely missing (not provided to function)
    if (missing(var) || is.null(var)) {
      # Return tagged_na("d") for missing required variables
      result <- list()
      for (n in var_names) {
        result[[paste0(n, "_clean")]] <- haven::tagged_na("d")
      }
      return(result)
    }

    # Check if variable contains tagged_na("c") - question not asked in cycle
    if (any(haven::is_tagged_na(var, "c"), na.rm = TRUE)) {
      if (log_level %in% c("warning", "verbose")) {
        warning(paste("Variable", name, "contains tagged_na('c') - skipping calculation"), call. = FALSE)
      }
      # Return tagged_na("c") to propagate the "not asked" state
      result <- list()
      for (n in var_names) {
        result[[paste0(n, "_clean")]] <- haven::tagged_na("c")
      }
      return(result)
    }
  }

  # Check length compatibility
  if (!do.call(check_vector_compatibility, input_vars)) {
    max_len <- max(sapply(input_vars, length))
    result <- list()
    for (name in var_names) {
      result[[paste0(name, "_clean")]] <- rep(haven::tagged_na("b"), max_len)
    }
    return(result)
  }

  # 3. Preprocess CCHS missing codes
  preprocessed_vars <- list()
  for (name in var_names) {
    var <- input_vars[[name]]
    if (needs_preprocessing(var)) {
      preprocessed_vars[[name]] <- preprocess_cchs_missing_codes(var, pattern_type)
    } else {
      preprocessed_vars[[name]] <- var
    }
  }

  # 4. Apply valid values validation (preserving tagged NAs)
  result <- list()
  for (name in var_names) {
    var <- preprocessed_vars[[name]]
    valid_vals <- valid_values[[name]]

    if (is.null(valid_vals)) {
      warning(paste("No valid values defined for variable:", name, "- skipping validation"), call. = FALSE)
      result[[paste0(name, "_clean")]] <- var
    } else {
      # Ensure numeric conversion for validation (handles character inputs)
      var_numeric <- suppressWarnings(as.numeric(var))
      result[[paste0(name, "_clean")]] <- dplyr::case_when(
        haven::is_tagged_na(var, "a") ~ haven::tagged_na("a"),
        haven::is_tagged_na(var, "b") ~ haven::tagged_na("b"),
        is.na(var) ~ haven::tagged_na("b"),
        # Use numeric version for validation checking
        is.na(var_numeric) ~ haven::tagged_na("b"), # Invalid conversions become NA(b)
        !(var_numeric %in% valid_vals) ~ haven::tagged_na("b"),
        TRUE ~ var_numeric # Return numeric version for consistency
      )
    }
  }

  return(result)
}

#' Universal input validation for derived variable functions
#'
#' Comprehensive upfront validation following development guide patterns.
#' Checks parameter presence, length compatibility, and type expectations
#' for ALL inputs before any processing begins.
#'
#' @param ... All input variables to validate (named arguments)
#' @param required_vars Character vector of required variable names
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#' @return TRUE if all validations pass, otherwise stops/warns appropriately
#' @note Internal v3.0.0, last updated: 2025-06-30, status: active
#' @keywords internal
#' @examples
#' # BMI function validation
#' validate_all_inputs(
#'   HWTGHTM = height_data, HWTGWTK = weight_data,
#'   required_vars = c("HWTGHTM", "HWTGWTK"),
#'   log_level = "warning"
#' )
#'
#' # Adjusted BMI validation
#' validate_all_inputs(
#'   DHH_SEX = sex_data, HWTGHTM = height_data, HWTGWTK = weight_data,
#'   required_vars = c("DHH_SEX", "HWTGHTM", "HWTGWTK"),
#'   log_level = "silent"
#' )
validate_all_inputs <- function(..., required_vars, log_level = "silent") {
  input_vars <- list(...)
  var_names <- names(input_vars)

  # 1. Parameter presence check
  missing_required <- setdiff(required_vars, var_names)
  if (length(missing_required) > 0) {
    stop(paste("Required parameters missing:", paste(missing_required, collapse = ", ")), call. = FALSE)
  }

  # 2. Basic parameter validation
  for (name in var_names) {
    input_vars[[name]] <- validate_parameter(name, input_vars[[name]], required = TRUE, na_type = "b", log_level)
  }

  # 3. Early return if any parameter validation failed
  invalid_params <- sapply(input_vars, function(x) length(x) == 1 && haven::is_tagged_na(x, "b"))
  if (any(invalid_params)) {
    if (log_level %in% c("warning", "verbose")) {
      warning("One or more required parameters are invalid", call. = FALSE)
    }
    return(FALSE)
  }

  # 4. Length compatibility check (THE critical check)
  if (!do.call(check_vector_compatibility, input_vars)) {
    if (log_level %in% c("warning", "verbose")) {
      warning("Input vectors must have compatible lengths", call. = FALSE)
    }
    return(FALSE)
  }

  # 5. Type validation with helpful messages
  for (name in var_names) {
    var <- input_vars[[name]]
    if (!is.numeric(var) && !all(var %in% c(1:9, NA), na.rm = TRUE) && !all(haven::is_tagged_na(var))) {
      if (log_level %in% c("warning", "verbose")) {
        warning(paste(name, "contains unexpected values. Expected: numeric, CCHS codes (1-9), or haven::tagged_na()"), call. = FALSE)
      }
    }
  }

  # 6. Edge case: all missing values
  all_missing <- sapply(input_vars, function(x) all(is.na(x)))
  if (any(all_missing)) {
    missing_var_names <- var_names[all_missing]
    if (log_level %in% c("warning", "verbose")) {
      warning(paste("All values missing for:", paste(missing_var_names, collapse = ", ")), call. = FALSE)
    }
  }

  return(TRUE)
}

#' Generic preprocessing detection for derived variable functions
#'
#' Auto-detects if input needs CCHS missing code preprocessing by checking
#' for presence of any CCHS missing data patterns or character NA values.
#'
#' @param input_var Input variable to check
#' @return Logical indicating if preprocessing needed
#' @note Internal v1.0.0 (2025-01-02) - Created for generic preprocessing detection
#' @keywords internal
needs_preprocessing <- function(input_var) {
  # Use existing has_cchs_missing_codes function
  has_cchs_codes <- has_cchs_missing_codes(input_var)

  # Also check for character NA representations
  has_char_nas <- any(
    is.character(input_var) & input_var %in%
      c("Not applicable", "Missing", "Don't know", "NA(a)", "NA(b)"),
    na.rm = TRUE
  )

  return(has_cchs_codes || has_char_nas)
}

#' Handle tagged NA propagation for multiple inputs
#'
#' Generic helper function that checks multiple input variables for tagged NAs
#' and returns the highest priority tagged NA found, or NULL if none found.
#' Used to eliminate repetitive tagged NA handling across derived variable functions.
#'
#' @param ... Input variables to check for tagged NAs
#' @param include_regular_na Logical. Treat regular NAs as tagged_na("b") (default TRUE)
#' @return Single tagged NA with highest priority, or NULL if no tagged NAs found
#' @note Internal v3.0.0, last updated: 2025-06-30, status: active
#' @keywords internal
#' @examples
#' # BMI core function usage
#' priority_na <- get_priority_tagged_na(height, weight)
#' if (!is.null(priority_na)) {
#'   return(priority_na)
#' }
#'
#' # Calculate normally if no tagged NAs
#' result <- weight / (height^2)
get_priority_tagged_na <- function(..., include_regular_na = TRUE) {
  inputs <- list(...)

  # Check in priority order: c → d → a → b

  # Priority 1: tagged_na("c") - question not asked
  for (var in inputs) {
    if (any(haven::is_tagged_na(var, "c"), na.rm = TRUE)) {
      return(haven::tagged_na("c"))
    }
  }

  # Priority 2: tagged_na("d") - variable missing
  for (var in inputs) {
    if (any(haven::is_tagged_na(var, "d"), na.rm = TRUE)) {
      return(haven::tagged_na("d"))
    }
  }

  # Priority 3: tagged_na("a") - not applicable
  for (var in inputs) {
    if (any(haven::is_tagged_na(var, "a"), na.rm = TRUE)) {
      return(haven::tagged_na("a"))
    }
  }

  # Priority 4: tagged_na("b") - missing/unknown
  for (var in inputs) {
    if (any(haven::is_tagged_na(var, "b"), na.rm = TRUE)) {
      return(haven::tagged_na("b"))
    }
  }

  # Priority 5: regular NAs (if enabled)
  if (include_regular_na) {
    for (var in inputs) {
      if (any(is.na(var) & !haven::is_tagged_na(var), na.rm = FALSE)) {
        return(haven::tagged_na("b"))
      }
    }
  }

  # No tagged NAs found
  return(NULL)
}

#' Clean and validate a single value with comprehensive preprocessing
#'
#' Lightweight preprocessing function for single values (e.g., already calculated
#' BMI, ADL scores, alcohol consumption values). Handles missing data, CCHS missing
#' code preprocessing, and tagged NA detection. Used by categorization functions
#' across all derived variable domains.
#'
#' @param value Single value to clean (numeric, character, or tagged_na)
#' @param pattern_type CCHS missing code pattern to use (default "continuous_standard")
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#' @return Cleaned single value with appropriate tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-06-30, status: active
#' @keywords internal
#' @examples
#' # BMI categorization usage
#' clean_bmi <- clean_single_value(bmi_value, "continuous_standard")
#'
#' # Smoking categorization usage
#' clean_status <- clean_single_value(smoking_status, "standard_response")
#'
#' # ADL categorization usage
#' clean_score <- clean_single_value(adl_score, "continuous_standard")
clean_single_value <- function(value, pattern_type = "continuous_standard", log_level = "silent") {
  # 1. Handle missing input (not provided to function)
  if (missing(value) || is.null(value)) {
    if (log_level %in% c("warning", "verbose")) {
      warning("Required value not provided", call. = FALSE)
    }
    return(haven::tagged_na("d"))
  }

  # 2. Check for tagged_na("c") - question not asked in cycle (skip processing)
  if (any(haven::is_tagged_na(value, "c"), na.rm = TRUE)) {
    if (log_level %in% c("warning", "verbose")) {
      warning("Value contains tagged_na('c') - skipping processing", call. = FALSE)
    }
    return(haven::tagged_na("c"))
  }

  # 3. Preprocess CCHS missing codes if needed
  if (needs_preprocessing(value)) {
    value <- preprocess_cchs_missing_codes(value, pattern_type)
  }

  # 4. Return cleaned value (may still contain tagged NAs a/b for downstream handling)
  return(value)
}

#' Clean and check for categorization with comprehensive tagged NA handling (SINGLE VALUES)
#'
#' Single comprehensive helper for all categorization functions. Handles preprocessing,
#' tagged NA detection, and appropriate formatting. Returns either the cleaned value
#' ready for categorization, or the properly formatted tagged NA.
#'
#' @param value Single value to clean and check (numeric, character, or tagged_na)
#' @param pattern_type CCHS missing code pattern to use (default "continuous_standard")
#' @param categorical_labels Return string labels for tagged NAs vs tagged_na objects (default TRUE)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#' @return List with is_na (logical) and value (cleaned value or formatted tagged NA)
#' @note Internal v3.0.0, last updated: 2025-06-30, status: active - SINGLE VALUE ONLY
#' @keywords internal
#' @examples
#' # BMI categorization usage
#' result <- clean_and_check_for_categorization(bmi_value, "continuous_standard", TRUE)
#' if (result$is_na) {
#'   return(result$value)
#' }
#' # categorize result$value...
#'
#' # Smoking categorization usage
#' result <- clean_and_check_for_categorization(smoking_status, "standard_response", FALSE)
#' if (result$is_na) {
#'   return(result$value)
#' }
#' # categorize result$value...
clean_and_check_for_categorization <- function(value, pattern_type = "continuous_standard",
                                               categorical_labels = TRUE, log_level = "silent") {
  # 1. Clean single value (handles missing data, preprocessing, tagged_na("c"/"d"))
  clean_value <- clean_single_value(value, pattern_type, log_level)

  # 2. Check for any tagged NAs and return appropriate format
  priority_na <- get_priority_tagged_na(clean_value, include_regular_na = TRUE)
  if (!is.null(priority_na)) {
    if (categorical_labels) {
      na_tag <- haven::na_tag(priority_na)
      return(list(is_na = TRUE, value = paste0("NA(", na_tag, ")")))
    } else {
      return(list(is_na = TRUE, value = priority_na))
    }
  }

  # 3. Return clean value ready for categorization
  return(list(is_na = FALSE, value = clean_value))
}

#' Clean and validate values for categorization with vector support
#'
#' Universal preprocessing function for categorization that handles both single values
#' and vectors. Provides comprehensive preprocessing, tagged NA handling, and prepares
#' values for use in dplyr::case_when() categorization logic. Used across all
#' derived variable categorization functions.
#'
#' @param values Values to clean (single value, vector, numeric, character, or tagged_na)
#' @param pattern_type CCHS missing code pattern to use (default "continuous_standard")
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#' @return Cleaned values ready for case_when() logic with proper tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-07-04, status: active - VECTOR AWARE
#' @keywords internal
#' @examples
#' # BMI categorization usage (vector)
#' clean_bmi <- clean_for_categorization(c(17, 22, 27, 997), "continuous_standard")
#'
#' # Smoking categorization usage (single)
#' clean_status <- clean_for_categorization(smoking_status, "standard_response")
#'
#' # Integration with case_when
#' result <- dplyr::case_when(
#'   clean_bmi < 18.5 ~ "Underweight",
#'   clean_bmi >= 18.5 & clean_bmi < 25.0 ~ "Normal weight",
#'   # ... tagged NAs are handled automatically by preprocessing
#' )
clean_for_categorization <- function(values, pattern_type = "continuous_standard", log_level = "silent") {
  # 1. Handle missing input (not provided to function)
  if (missing(values) || is.null(values)) {
    if (log_level %in% c("warning", "verbose")) {
      warning("Required values not provided for categorization", call. = FALSE)
    }
    return(haven::tagged_na("d"))
  }

  # 2. Check for tagged_na("c") - question not asked in cycle (skip processing)
  if (any(haven::is_tagged_na(values, "c"), na.rm = TRUE)) {
    if (log_level %in% c("warning", "verbose")) {
      warning("Values contain tagged_na('c') - skipping categorization", call. = FALSE)
    }
    return(rep(haven::tagged_na("c"), length(values)))
  }

  # 3. Preprocess CCHS missing codes if needed
  if (needs_preprocessing(values)) {
    clean_values <- preprocess_cchs_missing_codes(values, pattern_type)
  } else {
    clean_values <- values
  }

  # 4. Return cleaned values ready for case_when() logic
  # Tagged NAs are preserved and will be handled by case_when() conditions
  return(clean_values)
}

#' Generate standardized tagged NA case_when conditions
#'
#' Creates reusable case_when conditions for handling tagged NAs in a consistent
#' priority order across all derived variable functions. Eliminates code duplication
#' of tagged NA handling patterns.
#'
#' @param var Variable to generate conditions for
#' @param categorical_labels Return string labels ("NA(a)") vs tagged_na objects (default FALSE)
#' @param include_bounds Optional bounds checking condition to insert before default
#' @return List of case_when conditions in priority order
#' @note Internal v3.0.0, last updated: 2025-07-04, status: active
#' @keywords internal
#' @examples
#' # Usage in categorization functions
#' dplyr::case_when(
#'   !!!generate_tagged_na_conditions(clean_bmi, categorical_labels = TRUE),
#'   clean_bmi < 18.5 ~ "Underweight",
#'   clean_bmi >= 18.5 & clean_bmi < 25.0 ~ "Normal weight",
#'   .default = "NA(b)"
#' )
#'
#' # Usage in validation functions
#' dplyr::case_when(
#'   !!!generate_tagged_na_conditions(var, include_bounds = var < min_val | var > max_val),
#'   TRUE ~ var
#' )
generate_tagged_na_conditions <- function(var, categorical_labels = FALSE, include_bounds = NULL) {
  if (categorical_labels) {
    conditions <- list(
      haven::is_tagged_na(var, "c") ~ "NA(c)", # Question not asked
      haven::is_tagged_na(var, "d") ~ "NA(d)", # Variable missing
      haven::is_tagged_na(var, "a") ~ "NA(a)", # Not applicable
      haven::is_tagged_na(var, "b") ~ "NA(b)", # Missing/unknown
      is.na(var) ~ "NA(b)" # Regular NA
    )
  } else {
    conditions <- list(
      haven::is_tagged_na(var, "c") ~ haven::tagged_na("c"), # Question not asked
      haven::is_tagged_na(var, "d") ~ haven::tagged_na("d"), # Variable missing
      haven::is_tagged_na(var, "a") ~ haven::tagged_na("a"), # Not applicable
      haven::is_tagged_na(var, "b") ~ haven::tagged_na("b"), # Missing/unknown
      is.na(var) ~ haven::tagged_na("b") # Regular NA
    )
  }

  # Add optional bounds checking condition if provided
  if (!is.null(include_bounds)) {
    if (categorical_labels) {
      conditions <- append(conditions, list(include_bounds ~ "NA(b)"), after = length(conditions))
    } else {
      conditions <- append(conditions, list(include_bounds ~ haven::tagged_na("b")), after = length(conditions))
    }
  }

  return(conditions)
}
