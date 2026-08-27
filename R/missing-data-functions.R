# ==============================================================================
# Level 5: Missing Data Functions (Universal, Level 1-4 Integrated)
# ==============================================================================
#
# Universal missing data detection and priority processing functions that work 
# with any survey by integrating with Level 1-4 infrastructure and configurable
# priority rules via YAML configuration.
#
# DEPENDENCY LEVEL: 5 (Missing Data Processing - depends on Levels 1-4)
# DEPENDS ON: Level 1 (file-sourcing.R), Level 2A (database-config-loader.R), 
#             Level 2B (worksheet-metadata-loaders.R), Level 3 (worksheet-getters.R),
#             Level 4 (missing-pattern-cache.R)
# 
# FUNCTIONS IN THIS FILE:
# - Core API: any_missing(), get_priority_missing() (universal, vectorized)
# - Configuration: get_missing_config(), load_priority_rules()
# - Helpers: extract_variable_name(), detect_missing_vectorized(), apply_priority_hierarchy()
#
# USED BY LEVEL 6 FUNCTIONS:
# - clean_variables() (uses any_missing and get_priority_missing)
# - Derived variable functions in R/ (BMI, smoking, etc.)
#
# DEVELOPMENT STATUS: Level 5 missing data functions (Level 1-4 integrated)
# LOCATION: development/flexible-missing-data-mvp/R/missing-data-functions.R
# VERSION: v5.0.0, created 2025-08-03
#
# REPLACES: universal-missing-helpers.R (now archived)

# In package context, all R/ files are loaded automatically.
# Dependencies (dplyr, haven, yaml) come via DESCRIPTION Depends.

# ==============================================================================
# CORE API FUNCTIONS (Universal, Level 1-4 Integrated)
# ==============================================================================

#' Universal Missing Data Detection (Level 1-4 Integrated)
#'
#' Detects missing data across any survey type using Level 4's pattern extraction
#' and configurable priority rules. Works element-wise with dplyr::case_when().
#'
#' @param ... Variables to check for missing data
#' @param auto_detect Logical. Auto-detect patterns from metadata (default TRUE)
#' @param output_format Character. Output format control ("tagged_na", "original")
#' @return Logical vector indicating missing data presence
#'
#' @examples
#' # Universal usage (works with any survey)
#' \dontrun{
#' result <- dplyr::case_when(
#'   any_missing(var1, var2) ~ get_priority_missing(var1, var2),
#'   # ... domain logic
#' )
#' }
#'
#' @export
any_missing <- function(..., auto_detect = TRUE, output_format = "tagged_na") {
  
  vars <- list(...)
  if (length(vars) == 0) return(logical(0))
  
  if (auto_detect) {
    # Use Level 4 integration for pattern detection
    var_name <- extract_variable_name(vars)
    if (is.null(var_name)) {
      config <- default_missing_config()
    } else {
      config <- get_missing_config(var_name)
    }
  } else {
    config <- default_missing_config()
  }

  # Apply vectorized detection
  detect_missing_vectorized(vars, config)
}

#' Universal Priority Missing Value Processing (Level 1-4 Integrated)
#'
#' Returns missing values with highest priority based on configurable hierarchy
#' defined in YAML configuration. Works element-wise with dplyr::case_when().
#'
#' @param ... Variables to process for missing data priority
#' @param auto_detect Logical. Auto-detect patterns from metadata (default TRUE)
#' @param output_format Character. Output format control ("tagged_na", "original")
#' @return Vector with highest priority missing values
#'
#' @examples
#' # CCHS priority: "Not Applicable" (NA::a) > "Not Stated" (NA::b)
#' get_priority_missing(haven::tagged_na("a"), haven::tagged_na("b"))
#'
#' \dontrun{
#' # Universal usage in derived variables
#' get_priority_missing(height, weight, output_format = "original")
#' }
#'
#' @export
get_priority_missing <- function(..., auto_detect = TRUE, output_format = "tagged_na") {
  
  vars <- list(...)
  if (length(vars) == 0) return(logical(0))
  
  if (auto_detect) {
    # Use Level 4 integration for pattern detection
    var_name <- extract_variable_name(vars)
    if (is.null(var_name)) {
      config <- default_missing_config()
    } else {
      config <- get_missing_config(var_name)
    }
  } else {
    config <- default_missing_config()
  }

  # Apply priority hierarchy
  apply_priority_hierarchy(vars, config, output_format)
}

# ==============================================================================
# CONFIGURATION FUNCTIONS (Level 1-4 Integrated)
# ==============================================================================

#' Get Missing Configuration (Level 4 Integrated)
#'
#' Combines Level 4 pattern data with priority rules to create processing
#' configuration for vectorized operations.
#'
#' @param variable_name Character. Variable to get config for
#' @param database Character. Optional database specification
#' @return List with codes and priority rules
#' @noRd
get_missing_config <- function(variable_name, database = NULL) {
  
  # Step 1: Get raw pattern data from Level 4 (uses variable_details.csv)
  pattern_data <- tryCatch({
    get_missing_pattern(variable_name, database)
  }, error = function(e) {
    warning("Could not get pattern for '", variable_name, "', using default CCHS fallback")
    default_missing_config(pattern_only = TRUE)
  })
  
  # Step 2: Load priority rules from YAML configuration
  priority_rules <- load_priority_rules()
  
  # Step 3: Combine into processing config
  config <- list(
    na_a_codes = pattern_data$na_a_codes,
    na_b_codes = pattern_data$na_b_codes,
    na_a_priority = priority_rules$na_a,
    na_b_priority = priority_rules$na_b
  )
  
  return(config)
}

# Package-level cache for priority rules
.priority_rules_cache <- new.env(parent = emptyenv())

#' Load Missing Data Priority Rules
#'
#' Loads priority configuration from YAML with CCHS-specific and fallback options.
#' Results are cached for performance in a package-level environment.
#'
#' @return List with priority rules (na_a, na_b)
#' @noRd
load_priority_rules <- function() {
  
  # Check cache first
  if (exists("rules", envir = .priority_rules_cache)) {
    return(get("rules", envir = .priority_rules_cache))
  }
  
  # Try CCHS-specific rules first
  cchs_path <- system.file("metadata", "schemas", "cchs", "missing_priority_rules.yaml", package = "cchsflow")
  core_path <- system.file("metadata", "schemas", "core", "missing_priority_rules.yaml", package = "cchsflow")
  # Fall back to inst/ paths for dev context
  if (!nzchar(cchs_path)) cchs_path <- file.path("inst", "metadata", "schemas", "cchs", "missing_priority_rules.yaml")
  if (!nzchar(core_path)) core_path <- file.path("inst", "metadata", "schemas", "core", "missing_priority_rules.yaml")
  
  rules <- NULL
  
  # Try CCHS-specific configuration
  if (file.exists(cchs_path)) {
    tryCatch({
      rules <- yaml::read_yaml(cchs_path)$priority_rules
    }, error = function(e) {
      warning("Could not load CCHS priority rules: ", e$message)
    })
  }
  
  # Try core fallback configuration
  if (is.null(rules) && file.exists(core_path)) {
    tryCatch({
      rules <- yaml::read_yaml(core_path)$priority_rules
    }, error = function(e) {
      warning("Could not load core priority rules: ", e$message)
    })
  }

  # Read the declared priority hierarchy from the CCHS missing-data schema:
  # every pattern family declares not_applicable wins over missing_data
  # (inst/metadata/schemas/cchs/cchs_missing_data.yaml)
  if (is.null(rules)) {
    tryCatch({
      schema <- load_cchs_missing_data()
      fams <- schema$pattern_definitions$patterns
      if (length(fams) > 0 &&
          !is.null(fams[[1]]$priority_hierarchy$not_applicable)) {
        rules <- list(na_a = 1, na_b = 2)
      }
    }, error = function(e) NULL)
  }

  # Built-in fallback (general data handling: "Not Applicable" > "Not Stated")
  if (is.null(rules)) {
    rules <- list(na_a = 1, na_b = 2)  # na_a higher priority as general fallback
    warning("Priority rules YAML files not found, using built-in general fallback")
  }
  
  # Cache for performance in package-level environment
  assign("rules", rules, envir = .priority_rules_cache)
  
  return(rules)
}

#' Default Missing Configuration from YAML Schema
#'
#' Builds a fallback missing-data config from the triple_digit_missing pattern
#' in cchs_missing_data.yaml. This is database-agnostic (no worksheet lookup)
#' and safe as a default because triple-digit codes (996, 999, etc.) never
#' collide with valid response values.
#'
#' @param pattern_only If TRUE, return only the pattern data (na_a_codes,
#'   na_b_codes) for use inside get_missing_config()'s error handler.
#'   If FALSE (default), return the full config with priority rules.
#' @return List with missing code config
#' @noRd
default_missing_config <- function(pattern_only = FALSE) {
  # Try to read from YAML schema
  pattern <- tryCatch({
    schema <- load_cchs_missing_data()
    triple <- schema$pattern_definitions$patterns$triple_digit_missing$priority_hierarchy
    list(
      na_a_codes = c(triple$not_applicable$original_codes,
                     triple$not_applicable$decimal_codes),
      na_b_codes = c(triple$missing_data$original_codes,
                     triple$missing_data$decimal_codes)
    )
  }, error = function(e) {
    # Hardcoded fallback if YAML can't be loaded
    list(
      na_a_codes = c(996, 999.6),
      na_b_codes = c(997, 998, 999, 999.7, 999.8, 999.9)
    )
  })

  if (pattern_only) {
    return(pattern)
  }

  priority_rules <- load_priority_rules()

  list(
    na_a_codes = pattern$na_a_codes,
    na_b_codes = pattern$na_b_codes,
    na_a_priority = priority_rules$na_a,
    na_b_priority = priority_rules$na_b
  )
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Extract Variable Name from Function Arguments
#'
#' Attempts to extract variable name from ... arguments for metadata lookup.
#' Uses fallback strategy for derived variables and complex expressions.
#'
#' @param vars List of variables from ... arguments
#' @return Character variable name for metadata lookup
#' @noRd
extract_variable_name <- function(vars) {
  
  # Try to get variable name from call stack
  tryCatch({
    call_info <- sys.call(-1)
    if (!is.null(call_info) && length(call_info) > 1) {
      # Extract first argument name
      var_name <- as.character(call_info[[2]])
      if (nchar(var_name) > 0 && 
          !var_name %in% c("auto_detect", "output_format", "...", "vars")) {
        return(var_name)
      }
    }
  }, error = function(e) {
    # Silent failure - continue to fallback
  })
  
  # No variable name could be extracted from the call stack
  return(NULL)
}

#' Vectorized Missing Data Detection
#'
#' Applies missing data detection using configuration from Level 4 integration.
#' Works element-wise for use with dplyr::case_when().
#'
#' @param vars List of variables to check
#' @param config Configuration from get_missing_config()
#' @return Logical vector indicating missing data presence
#' @noRd
detect_missing_vectorized <- function(vars, config) {
  
  if (length(vars) == 0) return(logical(0))
  
  # Find the maximum length of the input vectors
  n <- max(sapply(vars, length))
  if (n == 0) return(logical(0))
  
  result <- logical(n)
  
  # Get all missing codes from config
  all_missing_codes <- c(config$na_a_codes, config$na_b_codes)
  
  # Element-wise detection across all variables
  for (i in 1:n) {
    element_missing <- FALSE
    
    for (var in vars) {
      # Ensure the vector has this element
      if (i <= length(var)) {
        val <- var[[i]] # Use [[i]] for safer extraction
        
        # 1. Check for tagged NA values first
        if (haven::is_tagged_na(val)) {
          element_missing <- TRUE
          break
        }
        
        # 2. Check for standard NA values
        if (is.na(val)) {
          element_missing <- TRUE
          break
        }
        
        # 3. Check for specific numeric missing codes
        if (is.numeric(val) && val %in% all_missing_codes) {
          element_missing <- TRUE
          break
        }
      }
    }
    
    result[i] <- element_missing
  }
  
  return(result)
}

#' Apply Priority Hierarchy
#'
#' Applies configurable priority hierarchy to return highest priority missing values.
#' Uses Level 4 pattern data with YAML priority configuration.
#'
#' @param vars List of variables to process
#' @param config Configuration from get_missing_config()
#' @param output_format Output format ("tagged_na" or "original")
#' @return Vector with highest priority missing values
#' @noRd
apply_priority_hierarchy <- function(vars, config, output_format = "tagged_na") {
  
  if (length(vars) == 0) return(logical(0))
  
  n <- length(vars[[1]])
  result <- rep(NA, n)
  
  # Element-wise priority processing
  for (i in 1:n) {
    highest_priority <- Inf
    highest_value <- NA
    
    for (var in vars) {
      if (i <= length(var)) {
        val <- var[i]
        priority <- get_value_priority(val, config)
        
        if (!is.na(priority) && priority < highest_priority) {
          highest_priority <- priority
          highest_value <- val
        }
      }
    }
    
    result[i] <- highest_value
  }
  
  # Convert to requested output format
  if (output_format == "original") {
    result <- convert_to_original_format(result, config)
  }
  
  return(result)
}

#' Get Value Priority
#'
#' Determines priority of a value based on configuration rules.
#'
#' @param val Value to check
#' @param config Configuration with priority rules
#' @return Numeric priority (lower = higher priority) or NA if not missing
#' @noRd
get_value_priority <- function(val, config) {
  
  # Handle regular NA
  if (is.na(val) && !haven::is_tagged_na(val)) {
    return(config$na_b_priority)  # Treat regular NA as "Not Stated"
  }
  
  # Handle tagged NA
  if (haven::is_tagged_na(val)) {
    if (haven::is_tagged_na(val, "a")) {
      return(config$na_a_priority)
    } else if (haven::is_tagged_na(val, "b")) {
      return(config$na_b_priority)
    } else {
      return(config$na_b_priority)  # Other tags default to "Not Stated"
    }
  }
  
  # Handle specific missing codes
  if (!is.na(val)) {
    if (val %in% config$na_a_codes) {
      return(config$na_a_priority)
    } else if (val %in% config$na_b_codes) {
      return(config$na_b_priority)
    }
  }
  
  # Not a missing value
  return(NA)
}

#' Convert to Original Format
#'
#' Converts tagged NA values back to original numeric codes when requested.
#'
#' @param values Vector of values to convert
#' @param config Configuration with code mappings
#' @return Vector in original format
#' @noRd
convert_to_original_format <- function(values, config) {
  
  result <- values
  
  for (i in seq_along(values)) {
    val <- values[i]
    
    if (haven::is_tagged_na(val, "a") && length(config$na_a_codes) > 0) {
      result[i] <- config$na_a_codes[1]  # Use first code as representative
    } else if (haven::is_tagged_na(val, "b") && length(config$na_b_codes) > 0) {
      result[i] <- config$na_b_codes[1]  # Use first code as representative
    }
  }
  
  return(result)
}

# ==============================================================================
# OUTPUT FORMAT HELPERS
# ==============================================================================

#' Assign Missing Value for Variable and Output Format
#'
#' Returns the appropriate missing value using variable-specific missing patterns.
#' Uses Level 4 pattern detection to get the correct codes for the variable.
#'
#' @param missing_type Character. Type of missing value ("not_applicable" or "not_stated")
#' @param variable_name Character. Name of variable to get missing pattern for
#' @param output_format Character. Output format ("tagged_na" or "original")
#' @return Single missing value in requested format using variable's pattern
#' 
#' @details
#' **Missing Value Types**:
#' - **"not_applicable"**: Question doesn't apply to this respondent (NA::a codes)
#' - **"not_stated"**: Respondent refusal, missing data, unknown (NA::b codes)
#'
#' **Architecture Integration**:
#' - Uses `get_missing_config()` to get variable-specific missing codes
#' - Supports different missing patterns (single-digit: 6,9; triple-digit: 996,999; etc.)
#' - Falls back to BMI pattern if variable not found
#'
#' **Usage Pattern**:
#' Use in `case_when()` `.default` clauses where domain logic determines
#' the appropriate missing type for non-matching cases.
#'
#' @examples
#' \dontrun{
#' # Domain logic assigns appropriate missing type
#' dplyr::case_when(
#'   smoking_status == "former_daily" ~ age_started_smoking,
#'   .default = assign_missing("not_applicable", "SMKG207_cont", output_format)
#' )
#' }
#'
#' # Different variables may have different missing patterns
#' assign_missing("not_applicable", "SMKG207_cont", "tagged_na")
#' assign_missing("not_applicable", "SMKG207_cont", "original")
#' assign_missing("not_stated", "SMKG207_cont", "original")
#'
#' @export
assign_missing <- function(missing_type, variable_name, output_format = "tagged_na") {
  
  # Input validation
  if (!missing_type %in% c("not_applicable", "not_stated")) {
    stop("missing_type must be 'not_applicable' or 'not_stated'")
  }
  if (!output_format %in% c("tagged_na", "original")) {
    stop("output_format must be 'tagged_na' or 'original'")
  }
  if (is.null(variable_name) || variable_name == "") {
    stop("variable_name cannot be NULL or empty")
  }
  
  # For tagged_na format, return the semantic tagged NA
  if (output_format == "tagged_na") {
    if (missing_type == "not_applicable") {
      return(haven::tagged_na("a"))  # NA::a
    } else {  # "not_stated"
      return(haven::tagged_na("b"))  # NA::b
    }
  }
  
  # For original format, try to get variable-specific missing codes
  config <- tryCatch({
    get_missing_config(variable_name)
  }, error = function(e) {
    NULL  # Will trigger fallback below
  })
  
  # Return appropriate original code based on missing type
  if (missing_type == "not_applicable") {
    # Use first NA::a code from variable's pattern, or fallback
    if (!is.null(config) && !is.null(config$na_a_codes) && length(config$na_a_codes) > 0) {
      return(as.integer(config$na_a_codes[1]))
    } else {
      return(996L)  # Standard CCHS fallback
    }
  } else {  # "not_stated"
    # Use first NA::b code from variable's pattern, or fallback
    if (!is.null(config) && !is.null(config$na_b_codes) && length(config$na_b_codes) > 0) {
      return(as.integer(config$na_b_codes[1]))
    } else {
      return(999L)  # Standard CCHS fallback
    }
  }
}