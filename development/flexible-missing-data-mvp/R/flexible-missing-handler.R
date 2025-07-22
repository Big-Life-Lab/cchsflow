# ==============================================================================
# Flexible Missing Data Handler - Configuration-Driven Architecture
# ==============================================================================
#
# **Purpose**: Optional tools for cleaner missing data handling in derived variables.
# Provides semantic functions like is_missing() and is_tag() that work with any 
# CCHS data format. Additive feature - doesn't change existing functionality.
#
# @note v3.1.0, refactored: 2025-07-22 (performance and vectorization)
# ==============================================================================

# Required dependencies
if (!requireNamespace("haven", quietly = TRUE)) {
  stop("Package 'haven' is required for flexible missing data handling")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Package 'dplyr' is required for flexible missing data handling")
}
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required for loading CCHS configuration")
}
if (!requireNamespace("purrr", quietly = TRUE)) {
  stop("Package 'purrr' is required for vectorized propagation")
}
# Note: 'here' package is optional but recommended for robust path resolution

# ==============================================================================
# CONFIGURATION LOADING (REFACTORED)
# ==============================================================================

#' Find CCHS Missing Data Configuration File
#'
#' Locates the cchs_missing_data.yaml file using a robust, project-aware
#' strategy. It prioritizes the development version and falls back to the
#' installed package version.
#'
#' @return A file path to the configuration file, or NULL if not found.
#' @noRd
find_cchs_config_path <- function() {
  # 1. Development path (using `here` for robustness)
  # Requires the `here` package to be installed.
  if (requireNamespace("here", quietly = TRUE)) {
    dev_path <- here::here("inst", "metadata", "schemas", "cchs", "cchs_missing_data.yaml")
    if (file.exists(dev_path)) {
      return(dev_path)
    }
  }

  # 2. Installed package path (for users of the package)
  prod_path <- system.file("metadata", "schemas", "cchs", "cchs_missing_data.yaml", package = "cchsflow")
  if (nzchar(prod_path)) { # nzchar is a safe way to check for non-empty strings
    return(prod_path)
  }

  # 3. Fallback if neither is found
  return(NULL)
}


#' Load CCHS Missing Data Configuration
#'
#' Reads the central configuration file that defines how different CCHS missing
#' codes should be handled.
#'
#' @return A list containing all missing data patterns and transformation rules.
#' @export
load_cchs_config <- function() {
  config_file <- find_cchs_config_path()

  if (is.null(config_file)) {
    stop("Cannot find cchs_missing_data.yaml. Checked development path (with `here`) and installed package path (with `system.file`).")
  }

  tryCatch({
    yaml::read_yaml(config_file)
  }, error = function(e) {
    stop(sprintf("Error loading CCHS configuration from %s: %s", config_file, e$message))
  })
}

#' Diagnose CCHS Configuration Path
#'
#' Helper function to diagnose which configuration file path is being used.
#'
#' @return Character string showing the path being used, for diagnostic purposes.
#' @export
diagnose_cchs_config_path <- function() {
  cat("=== CCHS Configuration Path Diagnosis ===\n")
  cat("Current working directory:", getwd(), "\n\n")

  # Test development path
  cat("1. Testing development path with `here` package...\n")
  if (requireNamespace("here", quietly = TRUE)) {
    dev_path <- here::here("inst", "metadata", "schemas", "cchs", "cchs_missing_data.yaml")
    cat("   - Path constructed:", dev_path, "\n")
    if (file.exists(dev_path)) {
      cat("   - \u2713 Found file at development path.\n")
      cat("\n\u2705 Using development path:", dev_path, "\n")
      return(invisible(dev_path))
    } else {
      cat("   - \u2717 File not found at this path.\n")
    }
  } else {
    cat("   - \u2717 `here` package not installed. Cannot check development path.\n")
  }

  # Test installed package path
  cat("\n2. Testing installed package path with `system.file`...\n")
  prod_path <- system.file("metadata", "schemas", "cchs", "cchs_missing_data.yaml", package = "cchsflow")
  cat("   - Path constructed:", prod_path, "\n")
  if (nzchar(prod_path) && file.exists(prod_path)) {
    cat("   - \u2713 Found file in installed package.\n")
    cat("\n\u2705 Using installed package path:", prod_path, "\n")
    return(invisible(prod_path))
  } else {
    cat("   - \u2717 File not found in installed package location.\n")
  }

  cat("\n\u274c No configuration file found!\n")
  return(invisible(NULL))
}

#' Convert Original CCHS Codes to Tagged NA Format
#'
#' Converts original CCHS missing codes (996, 997, etc.) to tagged_na format
#' using the YAML configuration. This is what rec_with_table() would do.
#'
#' @param data Vector containing original CCHS codes
#' @param pattern_type Pattern type from YAML config (e.g. "triple_digit_missing")
#' @return Vector with missing codes converted to tagged_na format
#'
#' @examples
#' height_data <- c(1.75, 1.60, 996, 997, 998)
#' height_tagged <- convert_to_tagged_na(height_data, "triple_digit_missing")
#' @export
convert_to_tagged_na <- function(data, pattern_type = "triple_digit_missing") {
  # Load configuration
  config <- load_cchs_config()
  
  # Get pattern configuration
  if ("pattern_definitions" %in% names(config) && "patterns" %in% names(config$pattern_definitions)) {
    pattern_config <- config$pattern_definitions$patterns[[pattern_type]]
  } else {
    pattern_config <- config[[pattern_type]]
  }
  
  if (is.null(pattern_config)) {
    available_patterns <- get_missing_patterns()
    stop(sprintf("Pattern type '%s' not found. Available patterns: %s", 
                 pattern_type, paste(available_patterns, collapse = ", ")))
  }
  
  # Get priority hierarchy for mapping
  priority_config <- pattern_config$priority_hierarchy
  
  # Convert each value
  sapply(data, function(x) {
    # Check each category to find the right tagged_na mapping
    for (category_name in names(priority_config)) {
      category_config <- priority_config[[category_name]]
      
      # Check original codes
      if (!is.null(category_config$original_codes) && x %in% category_config$original_codes) {
        return(haven::tagged_na(category_config$tagged_na[1]))
      }
      
      # Check decimal codes
      if (!is.null(category_config$decimal_codes) && x %in% category_config$decimal_codes) {
        return(haven::tagged_na(category_config$tagged_na[1]))
      }
    }
    
    # If not a missing code, return original value
    return(x)
  })
}

#' Get Available Missing Data Patterns
#'
#' Returns a character vector of all available pattern names from the configuration.
#' This helps with discoverability as the number of patterns grows.
#'
#' @return Character vector of available pattern names
#' @export
get_missing_patterns <- function() {
  tryCatch({
    config <- load_cchs_config()
    if ("pattern_definitions" %in% names(config) && "patterns" %in% names(config$pattern_definitions)) {
      return(names(config$pattern_definitions$patterns))
    } else {
      # Fallback for older config structure
      pattern_names <- names(config)
      # Remove non-pattern keys
      pattern_names <- pattern_names[!pattern_names %in% c("schema_version", "schema_date", "description", "propagation_rules", "exception_patterns", "implementation")]
      return(pattern_names)
    }
  }, error = function(e) {
    warning("Could not load configuration file. Returning default patterns.")
    return(c("single_digit_missing", "double_digit_missing", "triple_digit_missing"))
  })
}

# ==============================================================================
# HANDLER FACTORY
# ==============================================================================

#' Create Missing Data Handler
#'
#' Factory function that inspects input data, reads YAML configuration,
#' and returns a handler with semantic tools tailored to the data format.
#' 
#' This means you can write `handler$is_missing(x)` or `handler$is_tag(x, "not_applicable")` 
#' without worrying whether your data uses original CCHS codes (996, 997) or tagged_na values - 
#' the handler automatically adapts to your data format and applies the correct logic.
#'
#' @param ... Input variables to inspect for format detection
#' @param handle_missing_data Character string: "auto", "original", or "tagged_na"
#' @param pattern_type Character string: pattern type from YAML config
#' @return List of handler tools (is_tag, is_missing, propagate)
#'
#' @examples
#' # Auto-detection example
#' height <- c(1.75, 1.60, haven::tagged_na("a"))
#' weight <- c(70, 55, haven::tagged_na("b"))
#' handler <- create_missing_handler(height, weight, 
#'                                  pattern_type = "triple_digit_missing")
#' handler$is_missing(height[3])  # TRUE
#' 
#' @export
create_missing_handler <- function(..., 
                                  handle_missing_data = "auto",
                                  pattern_type = "triple_digit_missing") {
  
  # Load configuration
  config <- load_cchs_config()
  
  # Get pattern configuration with better error handling
  # Support both new and old config structures
  if ("pattern_definitions" %in% names(config) && "patterns" %in% names(config$pattern_definitions)) {
    pattern_config <- config$pattern_definitions$patterns[[pattern_type]]
  } else {
    pattern_config <- config[[pattern_type]]
  }
  
  if (is.null(pattern_config)) {
    available_patterns <- get_missing_patterns()
    stop(sprintf("Pattern type '%s' not found in configuration. Available patterns: %s", 
                 pattern_type, paste(available_patterns, collapse = ", ")))
  }
  
  # Validate required configuration fields
  required_fields <- c("transformation_map", "priority_hierarchy")
  missing_fields <- required_fields[!required_fields %in% names(pattern_config)]
  if (length(missing_fields) > 0) {
    stop(sprintf("Pattern '%s' is missing required fields: %s", 
                 pattern_type, paste(missing_fields, collapse = ", ")))
  }
  
  # Collect input variables for format detection
  input_vars <- list(...)
  
  # Auto-detect data format
  data_format <- detect_data_format(input_vars, handle_missing_data)
  
  # Create handler tools based on detected format
  create_handler_tools(pattern_config, data_format)
}

#' Detect Data Format
#' 
#' @param input_vars List of input variables
#' @param handle_missing_data User preference
#' @return Character string: "original" or "tagged_na"
detect_data_format <- function(input_vars, handle_missing_data) {
  if (handle_missing_data %in% c("original", "tagged_na")) {
    return(handle_missing_data)
  }
  
  # Auto-detection: check for both tagged_na AND original codes
  has_tagged_na <- any(sapply(input_vars, function(x) {
    inherits(x, "haven_labelled") || any(haven::is_tagged_na(x))
  }))
  
  # Check for original CCHS missing codes (996, 997, 998, 999, etc.)
  has_original_codes <- any(sapply(input_vars, function(x) {
    if (is.numeric(x)) {
      # Check for common CCHS missing patterns
      any(x %in% c(6:9, 96:99, 996:999, 999.6, 999.7, 999.8, 999.9), na.rm = TRUE)
    } else {
      FALSE
    }
  }))
  
  # Mixed data handling: if both formats present, use "mixed" mode
  if (has_tagged_na && has_original_codes) {
    return("mixed")
  } else if (has_tagged_na) {
    return("tagged_na") 
  } else {
    return("original") 
  }
}

#' Create Handler Tools
#'
#' @param pattern_config Configuration for the specific pattern
#' @param data_format Detected data format
#' @return List of handler functions
create_handler_tools <- function(pattern_config, data_format) {
  
  priority_config <- pattern_config$priority_hierarchy
  
  # Pre-calculate all missing codes for original format (including mixed mode)
  all_missing_codes <- if (data_format %in% c("original", "mixed")) {
    unique(unlist(lapply(priority_config, function(category) {
      c(category$original_codes, category$decimal_codes)
    })))
  } else {
    c()
  }
  
  # Pre-calculate priority-ordered categories
  priority_ordered_categories <- get_priority_ordered_categories(priority_config)
  
  # The single-value prioritization function, to be used by the vectorized wrapper
  prioritize_single <- if (data_format == "tagged_na") {
    function(values) prioritize_tagged_na_optimized(values, priority_config, priority_ordered_categories)
  } else if (data_format == "mixed") {
    # Mixed mode: try tagged_na first, then original codes
    function(values) prioritize_mixed_optimized(values, priority_config, priority_ordered_categories)
  } else {
    function(values) prioritize_original_codes_optimized(values, priority_config, priority_ordered_categories)
  }
  
  list(
    # Check if value matches semantic tags
    is_tag = function(x, ...) {
      tags <- list(...)
      sapply(x, function(val) {
          if (data_format == "tagged_na") {
            check_tagged_na_tags(val, tags, priority_config)
          } else if (data_format == "mixed") {
            # Mixed mode: check both tagged_na and original codes
            check_tagged_na_tags(val, tags, priority_config) || 
            check_original_tags(val, tags, priority_config)
          } else {
            check_original_tags(val, tags, priority_config)
          }
      })
    },
    
    # Check if value is any missing type
    is_missing = function(x) {
      if (data_format == "tagged_na") {
        haven::is_tagged_na(x) | is.na(x)
      } else if (data_format == "mixed") {
        # Mixed mode: check both tagged_na and original codes
        haven::is_tagged_na(x) | is.na(x) | x %in% all_missing_codes
      } else {
        is.na(x) | x %in% all_missing_codes
      }
    },
    
    # Return highest priority missing value using YAML configuration
    propagate = function(...) {
      values <- list(...)
      
      # Vectorized propagation for case_when compatibility
      # Use purrr for safe vectorized element-wise processing
      result <- purrr::pmap(values, function(...) {
        single_values <- list(...)
        prioritize_single(single_values)
      })
      
      # Convert list back to appropriate vector type
      if (data_format == "tagged_na") {
        # pmap returns a list of haven_labelled objects, so we c() them together
        return(do.call(c, result))
      } else {
        # For original codes, it's a list of numbers, so unlist
        return(unlist(result))
      }
    }
  )
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Check Tagged NA Tags Using YAML Configuration
check_tagged_na_tags <- function(x, tags, priority_config) {
  # Build semantic tag mapping dynamically from YAML priority_hierarchy
  tag_map <- list()
  
  for (category_name in names(priority_config)) {
    category_config <- priority_config[[category_name]]
    if (!is.null(category_config$tagged_na)) {
      # Map category name to its tagged_na values
      tag_map[[category_name]] <- category_config$tagged_na
    }
  }
  
  for (tag in unlist(tags)) {
    if (tag %in% names(tag_map)) {
      # Check if x matches any of the tagged_na codes for this semantic category
      for (tag_code in tag_map[[tag]]) {
        if (haven::is_tagged_na(x, tag_code)) {
          return(TRUE)
        }
      }
    }
  }
  FALSE
}

#' Check Original Tags Using YAML Configuration
check_original_tags <- function(x, tags, priority_config) {
  # Build semantic tag mapping dynamically from YAML priority_hierarchy
  tag_map <- list()
  
  for (category_name in names(priority_config)) {
    category_config <- priority_config[[category_name]]
    codes <- c()
    
    # Collect original codes
    if (!is.null(category_config$original_codes)) {
      codes <- c(codes, category_config$original_codes)
    }
    
    # Collect decimal codes if present
    if (!is.null(category_config$decimal_codes)) {
      codes <- c(codes, category_config$decimal_codes)
    }
    
    # Map category name to its original codes
    tag_map[[category_name]] <- codes
  }
  
  for (tag in unlist(tags)) {
    if (tag %in% names(tag_map)) {
      if (x %in% tag_map[[tag]]) {
        return(TRUE)
      }
    }
  }
  FALSE
}

#' Get Priority-Ordered Categories from YAML Configuration
#' 
#' Reads explicit priority values from YAML and sorts categories by priority.
#' Lower priority numbers = higher priority (1 beats 2, etc.)
#' 
#' @param priority_config Priority hierarchy section from YAML
#' @return Character vector of category names in priority order (highest first)
get_priority_ordered_categories <- function(priority_config) {
  categories <- names(priority_config)
  
  # Extract priority values, defaulting to 999 for missing priorities
  priorities <- sapply(categories, function(cat) {
    priority_val <- priority_config[[cat]]$priority
    if (is.null(priority_val)) 999 else priority_val
  })
  
  # Sort by priority (lower numbers = higher priority)
  ordered_categories <- categories[order(priorities)]
  
  return(ordered_categories)
}

#' Prioritize Tagged NA Values
#' @param values Vector of values to prioritize
#' @param priority_config Priority configuration from YAML pattern
#' @param priority_categories Pre-calculated priority-ordered categories
prioritize_tagged_na_optimized <- function(values, priority_config, priority_categories) {
  # Check each priority level in order
  for (category_name in priority_categories) {
    category_config <- priority_config[[category_name]]
    
    if (!is.null(category_config$tagged_na)) {
      # Check if any value matches this category's tagged_na codes
      for (tag_code in category_config$tagged_na) {
        for (val in values) {
          if (haven::is_tagged_na(val, tag_code)) {
            return(haven::tagged_na(tag_code))
          }
        }
      }
    }
  }
  
  # Check for regular NA
  for (val in values) {
    if (is.na(val)) {
      # Default to lowest priority category's first tagged_na code
      lowest_category <- priority_categories[length(priority_categories)]
      default_tag <- priority_config[[lowest_category]]$tagged_na[1]
      return(haven::tagged_na(default_tag))
    }
  }
  
  # Return first valid value if no missing found
  for (val in values) {
    if (!is.na(val) && !haven::is_tagged_na(val)) return(val)
  }
  
  # Final fallback to lowest priority category
  lowest_category <- priority_categories[length(priority_categories)]
  default_tag <- priority_config[[lowest_category]]$tagged_na[1]
  haven::tagged_na(default_tag)
}

#' Prioritize Original Codes 
#' @param values Vector of values to prioritize
#' @param priority_config Priority configuration from YAML pattern
#' @param priority_categories Pre-calculated priority-ordered categories
prioritize_original_codes_optimized <- function(values, priority_config, priority_categories) {
  # Check each priority level in order
  for (category_name in priority_categories) {
    category_config <- priority_config[[category_name]]
    codes <- c()
    
    # Collect all codes for this category
    if (!is.null(category_config$original_codes)) {
      codes <- c(codes, category_config$original_codes)
    }
    if (!is.null(category_config$decimal_codes)) {
      codes <- c(codes, category_config$decimal_codes)
    }
    
    # Check if any value matches this category's codes
    for (code in codes) {
      for (val in values) {
        if (length(val) == 1 && !is.na(val) && val == code) {
          return(code)
        }
      }
    }
  }
  
  # Check for regular NA
  for (val in values) {
    if (is.na(val)) {
      # Default to lowest priority category's first code
      lowest_category <- priority_categories[length(priority_categories)]
      lowest_config <- priority_config[[lowest_category]]
      default_code <- if (!is.null(lowest_config$original_codes)) {
        lowest_config$original_codes[1]
      } else {
        lowest_config$decimal_codes[1]
      }
      return(default_code)
    }
  }
  
  # Return first valid value if no missing found
  for (val in values) {
    if (!is.na(val)) return(val)
  }
  
  # Final fallback to lowest priority category's first code
  lowest_category <- priority_categories[length(priority_categories)]
  lowest_config <- priority_config[[lowest_category]]
  default_code <- if (!is.null(lowest_config$original_codes)) {
    lowest_config$original_codes[1]
  } else {
    lowest_config$decimal_codes[1]
  }
  default_code
}

#' Prioritize Mixed Format Values (both tagged_na and original codes)
#' @param values Vector of values to prioritize (mix of tagged_na and original codes)
#' @param priority_config Priority configuration from YAML pattern
#' @param priority_categories Pre-calculated priority-ordered categories
prioritize_mixed_optimized <- function(values, priority_config, priority_categories) {
  # Check each priority level in order
  for (category_name in priority_categories) {
    category_config <- priority_config[[category_name]]
    
    # Check tagged_na codes first
    if (!is.null(category_config$tagged_na)) {
      for (tag_code in category_config$tagged_na) {
        for (val in values) {
          if (haven::is_tagged_na(val, tag_code)) {
            return(haven::tagged_na(tag_code))
          }
        }
      }
    }
    
    # Then check original codes
    codes <- c()
    if (!is.null(category_config$original_codes)) {
      codes <- c(codes, category_config$original_codes)
    }
    if (!is.null(category_config$decimal_codes)) {
      codes <- c(codes, category_config$decimal_codes)
    }
    
    # Check if any value matches this category's codes
    for (code in codes) {
      for (val in values) {
        if (length(val) == 1 && !is.na(val) && !haven::is_tagged_na(val) && val == code) {
          # Convert original code to tagged_na for consistency
          return(haven::tagged_na(category_config$tagged_na[1]))
        }
      }
    }
  }
  
  # Check for regular NA
  for (val in values) {
    if (is.na(val)) {
      # Default to lowest priority category's tagged_na
      lowest_category <- priority_categories[length(priority_categories)]
      default_tag <- priority_config[[lowest_category]]$tagged_na[1]
      return(haven::tagged_na(default_tag))
    }
  }
  
  # Return first valid value if no missing found
  for (val in values) {
    if (!is.na(val) && !haven::is_tagged_na(val)) return(val)
  }
  
  # Final fallback to lowest priority category's tagged_na
  lowest_category <- priority_categories[length(priority_categories)]
  default_tag <- priority_config[[lowest_category]]$tagged_na[1]
  haven::tagged_na(default_tag)
}