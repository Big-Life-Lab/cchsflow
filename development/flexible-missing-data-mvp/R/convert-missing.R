# ==============================================================================
# Convert Missing Data Format - Universal Conversion Function
# ==============================================================================
#
# **Purpose**: Eliminates code duplication across derived variable functions by
# providing a single, flexible function for missing data format conversion.
# Supports forward conversion (original -> tagged_na) and pass-through modes.
#
# @note v1.0.0, created: 2025-07-23 (development/testing)
# ==============================================================================

# Required dependencies
if (!requireNamespace("haven", quietly = TRUE)) {
  stop("Package 'haven' is required for missing data format conversion")
}

#' Convert Missing Data (Forward Conversion Only)
#'
#' Universal missing data format converter that eliminates code duplication
#' across derived functions. Supports forward conversion (original -> tagged_na)
#' and pass-through modes for maximum flexibility without lossy reverse conversion.
#'
#' @param data Input data (any format: original codes, tagged_na, mixed)
#' @param pattern_type Pattern type ("single_digit_missing", "double_digit_missing", 
#'   "triple_digit_missing") or NULL for auto-detection from variable name
#' @param output_format Desired output format:
#'   \itemize{
#'     \item "tagged_na" - Convert original codes to tagged_na format (default)
#'     \item "original" - Pass-through, preserve original codes
#'     \item "auto" - Pass-through for auto-detection by handler
#'   }
#' @return Data converted to the specified format
#'
#' @details
#' This function provides three key benefits:
#' 1. **Eliminates code duplication**: All derived functions use the same conversion pattern
#' 2. **Variable pattern detection**: Auto-detects missing data patterns from variable names
#' 3. **Forward-only conversion**: Avoids lossy reverse conversions (tagged_na -> original)
#'
#' The function performs forward conversion only (original codes -> tagged_na) because
#' reverse conversion is lossy: tagged_na("a") could represent 6, 96, or 996, making
#' perfect reconstruction impossible.
#'
#' @examples
#' # Auto-detect pattern from variable name
#' height_data <- c(1.75, 1.60, 996, 997)
#' convert_missing_data_format(height_data)  # Auto-detects triple_digit_missing
#'
#' # Explicit pattern specification
#' sex_data <- c(1, 2, 6, 7)
#' convert_missing_data_format(sex_data, "single_digit_missing", "tagged_na")
#'
#' # Pass-through modes
#' convert_missing_data_format(height_data, "triple_digit_missing", "original")
#' convert_missing_data_format(height_data, "triple_digit_missing", "auto")
#'
#' # Usage in derived functions (eliminates duplication)
#' calculate_derived_variable <- function(var1, var2, handle_missing_data = "tagged_na") {
#'   var1_clean <- convert_missing_data_format(var1, NULL, handle_missing_data)
#'   var2_clean <- convert_missing_data_format(var2, NULL, handle_missing_data)
#'   # ... rest of function logic
#' }
#'
#' @export
convert_missing <- function(data, 
                           pattern_type = NULL,
                           output_format = "tagged_na") {
  
  # Auto-detect pattern from variable name if not specified
  if (is.null(pattern_type)) {
    var_name <- deparse(substitute(data))
    pattern_type <- detect_pattern_from_variable(var_name)
    
    # Fallback to reasonable default if detection fails
    if (is.null(pattern_type)) {
      pattern_type <- "triple_digit_missing"
      warning(sprintf("Could not detect pattern for variable '%s', using default: %s", 
                     var_name, pattern_type))
    }
  }
  
  # Validate pattern_type
  available_patterns <- get_missing_patterns()
  if (!pattern_type %in% available_patterns) {
    stop(sprintf("Pattern type '%s' not found. Available patterns: %s", 
                pattern_type, paste(available_patterns, collapse = ", ")))
  }
  
  # Convert based on output format
  switch(output_format,
    "tagged_na" = {
      # Forward conversion: original codes -> tagged_na
      tryCatch({
        convert_to_tagged_na(data, pattern_type)
      }, error = function(e) {
        stop(sprintf("Error converting to tagged_na format: %s", e$message))
      })
    },
    "original" = {
      # Pass-through: preserve original codes
      data
    },
    "auto" = {
      # Pass-through: let handler auto-detect format
      data
    },
    {
      # Default case: invalid output_format
      stop(sprintf("Invalid output_format '%s'. Valid options: 'tagged_na', 'original', 'auto'", 
                  output_format))
    }
  )
}

#' Detect Missing Data Pattern from Variable Name
#'
#' Auto-detects the appropriate missing data pattern based on variable naming
#' conventions commonly used in CCHS data. This provides the same flexibility
#' as create_missing_handler() for pattern detection.
#'
#' @param var_name Character string of variable name
#' @return Character string of detected pattern type, or NULL if no pattern detected
#'
#' @details
#' Detection rules based on common CCHS variable patterns:
#' - Variables ending in numeric codes (e.g., "SMKG040", "ALCDTYP") -> appropriate digit pattern
#' - Demographic variables (e.g., "DHH_SEX", "GEO_PRV") -> single_digit_missing
#' - Anthropometric variables (e.g., "HWTGHTM", "HWTGWTK") -> triple_digit_missing
#' - Scale variables (e.g., variables with "SCL", "SCORE") -> double_digit_missing
#'
#' @examples
#' detect_pattern_from_variable("HWTGHTM")     # "triple_digit_missing"
#' detect_pattern_from_variable("DHH_SEX")     # "single_digit_missing"  
#' detect_pattern_from_variable("SMKG040")     # "double_digit_missing"
#' detect_pattern_from_variable("custom_var")  # NULL (unknown pattern)
#'
#' @noRd
detect_pattern_from_variable <- function(var_name) {
  # Use enhanced pattern detection with CSV lookup
  if (exists("detect_pattern_enhanced")) {
    return(detect_pattern_enhanced(var_name))
  }
  
  # Fallback to original logic if enhanced version not available
  if (is.null(var_name) || length(var_name) == 0 || var_name == "") {
    return(NULL)
  }
  
  # Convert to uppercase for consistent matching
  var_upper <- toupper(var_name)
  
  # Single digit missing patterns (demographic, binary variables)
  single_digit_patterns <- c(
    "^DHH_",       # Demographics
    "^GEO_",       # Geography
    "_SEX$",       # Sex variables
    "^SMK_005$",   # Smoking status
    "^ALCTYPE",    # Alcohol type variables
    "_01$", "_02$", "_03$", "_04$", "_05$"  # Binary response variables
  )
  
  # Triple digit missing patterns (continuous measurements)
  triple_digit_patterns <- c(
    "^HWTG",       # Height/weight
    "^PACDEE",     # Physical activity energy expenditure
    "HTMCM$",      # Height in cm
    "WTKG$",       # Weight in kg
    "_der$",       # Derived continuous variables
    "BMI"          # BMI variables
  )
  
  # Double digit missing patterns (scales, multi-category)
  double_digit_patterns <- c(
    "^SMKG0[0-9]+", # Smoking quantity variables
    "^SMKG[0-9]+",  # Smoking variables like SMKG040
    "^ALC.*TYP",    # Alcohol type variables like ALCDTYP
    "^ALC[A-Z]+[0-9]+", # Alcohol quantity variables
    "^DPSD",       # Depression scale
    "^FSTRESS",    # Stress scale
    "SCL",         # Scale variables
    "SCORE"        # Score variables
  )
  
  # Check patterns in order of specificity
  for (pattern in single_digit_patterns) {
    if (grepl(pattern, var_upper)) {
      return("single_digit_missing")
    }
  }
  
  for (pattern in triple_digit_patterns) {
    if (grepl(pattern, var_upper)) {
      return("triple_digit_missing")
    }
  }
  
  for (pattern in double_digit_patterns) {
    if (grepl(pattern, var_upper)) {
      return("double_digit_missing")
    }
  }
  
  # Default fallback based on variable naming convention
  if (grepl("^[A-Z]+_[0-9]{1,2}[A-Z]*$", var_upper)) {
    return("single_digit_missing")  # Pattern like ABC_01, XYZ_12A
  } else if (grepl("^[A-Z]+[0-9]{2,3}[A-Z]*$", var_upper)) {
    return("double_digit_missing")  # Pattern like ABC040, XYZ203
  }
  
  # No pattern detected
  return(NULL)
}

#' Get Missing Data Patterns (Safe Wrapper)
#'
#' Safe wrapper around get_missing_patterns() that provides fallback
#' if the configuration cannot be loaded.
#'
#' @return Character vector of available missing data patterns
#' @noRd
get_missing_patterns_safe <- function() {
  tryCatch({
    if (exists("get_missing_patterns")) {
      get_missing_patterns()
    } else {
      # Fallback to standard patterns
      c("single_digit_missing", "double_digit_missing", "triple_digit_missing")
    }
  }, error = function(e) {
    # Fallback to standard patterns
    c("single_digit_missing", "double_digit_missing", "triple_digit_missing")
  })
}

# Override get_missing_patterns with safe version if needed
if (!exists("get_missing_patterns")) {
  get_missing_patterns <- get_missing_patterns_safe
}