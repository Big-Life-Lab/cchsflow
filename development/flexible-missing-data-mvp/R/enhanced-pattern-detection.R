# ==============================================================================
# Enhanced Pattern Detection - CSV Lookup + Pattern Matching
# ==============================================================================
#
# **Purpose**: Enhances detect_pattern_from_variable() to use variables.csv
# lookup table as the primary source, with pattern matching as fallback.
# This ensures consistency with the authoritative variable metadata.
#
# @note v1.0.0, created: 2025-07-23 (development/testing)
# ==============================================================================

# Required dependencies
if (!requireNamespace("utils", quietly = TRUE)) {
  stop("Package 'utils' is required for CSV reading")
}

#' Load Variables CSV for Pattern Lookup
#'
#' Loads the variables.csv file and caches it for pattern detection.
#' Uses similar caching approach as the CCHS config loading.
#'
#' @return Data frame with variable metadata including missingType column
#' @noRd
load_variables_csv <- function() {
  # Simple caching - check if already loaded in internal environment
  if (exists(".variables_csv_cache", envir = ._flexible_missing_data_env) && 
      !is.null(._flexible_missing_data_env$.variables_csv_cache)) {
    return(._flexible_missing_data_env$.variables_csv_cache)
  }
  
  # Try system.file first (robust for installed packages)
  csv_path <- system.file("extdata", "variables.csv", package = "cchsflow")
  
  # Fallback for development environment (if not installed as package)
  if (!nzchar(csv_path) && requireNamespace("here", quietly = TRUE)) {
    dev_path <- here::here("inst", "extdata", "variables.csv")
    if (file.exists(dev_path)) {
      csv_path <- dev_path
    }
  }
  
  variables_csv <- NULL
  if (nzchar(csv_path) && file.exists(csv_path)) {
    tryCatch({
      variables_csv <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
    }, error = function(e) {
      warning(sprintf("Error reading variables.csv from %s: %s", csv_path, e$message))
    })
  }
  
  if (is.null(variables_csv)) {
    warning("Could not load variables.csv - falling back to pattern matching only")
    return(NULL)
  }
  
  # Cache the loaded CSV
  ._flexible_missing_data_env$.variables_csv_cache <- variables_csv
  
  return(variables_csv)
}

#' Enhanced Pattern Detection with CSV Lookup
#'
#' Enhanced version of detect_pattern_from_variable() that uses variables.csv
#' as the primary source for missing data patterns, with pattern matching
#' as fallback for variables not in the CSV.
#'
#' @param var_name Character string of variable name
#' @return Character string of detected pattern type, or NULL if no pattern detected
#'
#' @details
#' Detection hierarchy:
#' 1. **CSV Lookup (Primary)**: Check variables.csv missingType column
#' 2. **Pattern Matching (Fallback)**: Use naming convention patterns
#' 3. **Default Patterns**: Final fallback based on naming structure
#'
#' This ensures consistency with the authoritative variable metadata while
#' still providing pattern detection for variables not yet in the CSV.
#'
#' @examples
#' detect_pattern_enhanced("HWTGHTM")     # "triple_digit_missing" from CSV
#' detect_pattern_enhanced("DHH_SEX")     # "single_digit_missing" from pattern
#' detect_pattern_enhanced("SMKG040")     # "double_digit_missing" from pattern
#' detect_pattern_enhanced("new_var")     # NULL or fallback pattern
#'
#' @export
detect_pattern_enhanced <- function(var_name) {
  if (is.null(var_name) || length(var_name) == 0 || var_name == "") {
    return(NULL)
  }
  
  # 1. PRIMARY: Check variables.csv lookup
  variables_csv <- load_variables_csv()
  if (!is.null(variables_csv) && "missingType" %in% names(variables_csv)) {
    # Find the variable in the CSV
    var_row <- variables_csv[variables_csv$variable == var_name, ]
    if (nrow(var_row) > 0 && !is.na(var_row$missingType) && 
        nzchar(var_row$missingType)) {
      return(var_row$missingType)
    }
  }
  
  # 2. FALLBACK: Use pattern matching (original logic)
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
  
  # 3. DEFAULT: Final fallback based on variable naming convention
  if (grepl("^[A-Z]+_[0-9]{1,2}[A-Z]*$", var_upper)) {
    return("single_digit_missing")  # Pattern like ABC_01, XYZ_12A
  } else if (grepl("^[A-Z]+[0-9]{2,3}[A-Z]*$", var_upper)) {
    return("double_digit_missing")  # Pattern like ABC040, XYZ203
  }
  
  # No pattern detected
  return(NULL)
}

#' Clear Variables CSV Cache
#'
#' Clears the cached variables.csv data, forcing the next call to reload from disk.
#' Useful for testing or when the CSV file changes.
#'
#' @export
clear_variables_csv_cache <- function() {
  if (exists(".variables_csv_cache", envir = ._flexible_missing_data_env)) {
    rm(.variables_csv_cache, envir = ._flexible_missing_data_env)
  }
  invisible(NULL)
}

#' Get Pattern Detection Info
#'
#' Shows information about how pattern detection works with CSV lookup.
#'
#' @return List with pattern detection information
#' @export
get_pattern_detection_info <- function() {
  variables_csv <- load_variables_csv()
  csv_available <- !is.null(variables_csv)
  
  if (csv_available) {
    csv_vars_with_missing_type <- sum(!is.na(variables_csv$missingType) & 
                                     nzchar(variables_csv$missingType))
    total_vars <- nrow(variables_csv)
  } else {
    csv_vars_with_missing_type <- 0
    total_vars <- 0
  }
  
  list(
    csv_available = csv_available,
    csv_variables_total = total_vars,
    csv_variables_with_missing_type = csv_vars_with_missing_type,
    detection_hierarchy = c(
      "1. CSV Lookup (Primary)",
      "2. Pattern Matching (Fallback)", 
      "3. Default Patterns (Final fallback)"
    ),
    benefits = c(
      "Authoritative source from variables.csv",
      "Consistent with metadata",
      "Fallback for new variables",
      "Cached for performance"
    )
  )
}