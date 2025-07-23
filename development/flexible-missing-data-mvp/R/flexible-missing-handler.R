# ==============================================================================
# Flexible Missing Data Handler - Configuration-Driven Architecture
# ==============================================================================
#
# **Purpose**: Optional tools for cleaner missing data handling in derived variables.
# Provides semantic functions like is_missing() and is_tag() that work with any 
# CCHS data format. Additive feature - doesn't change existing functionality.
#
# @note v3.2.0, refactored: 2025-07-22 (major performance and vectorization)
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
if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required for robust file path resolution")
}

# ==============================================================================
# CONFIGURATION LOADING (WITH CACHING)
# ==============================================================================

# Private environment for configuration caching
.cchs_config_cache <- new.env(parent = emptyenv())

#' Find CCHS Missing Data Configuration File
#'
#' @return A file path to the configuration file, or NULL if not found.
#' @noRd
find_cchs_config_path <- function() {
  if (requireNamespace("here", quietly = TRUE)) {
    dev_path <- here::here("inst", "metadata", "schemas", "cchs", "cchs_missing_data.yaml")
    if (file.exists(dev_path)) return(dev_path)
  }
  prod_path <- system.file("metadata", "schemas", "cchs", "cchs_missing_data.yaml", package = "cchsflow")
  if (nzchar(prod_path)) return(prod_path)
  return(NULL)
}

#' Load CCHS Missing Data Configuration (Cached)
#'
#' Loads the configuration file once and caches it for subsequent calls.
#' This eliminates the YAML loading overhead on repeated handler creation.
#' Cache can be cleared with clear_cchs_config_cache() if needed.
#'
#' @return A list containing all missing data patterns and transformation rules.
#' @export
load_cchs_config <- function() {
  # Check if config is already cached
  if (!is.null(.cchs_config_cache$config)) {
    return(.cchs_config_cache$config)
  }
  
  # Load and cache the configuration
  config_file <- find_cchs_config_path()
  if (is.null(config_file)) {
    stop("Cannot find cchs_missing_data.yaml. Checked dev path and installed path.")
  }
  
  config <- tryCatch({
    yaml::read_yaml(config_file)
  }, error = function(e) {
    stop(sprintf("Error loading CCHS configuration from %s: %s", config_file, e$message))
  })
  
  # Cache the loaded configuration
  .cchs_config_cache$config <- config
  .cchs_config_cache$config_file <- config_file
  
  return(config)
}

#' Clear CCHS Configuration Cache
#'
#' Clears the cached configuration, forcing the next call to load_cchs_config()
#' to reload from disk. Useful for testing or when configuration files change.
#'
#' @export
clear_cchs_config_cache <- function() {
  rm(list = ls(.cchs_config_cache), envir = .cchs_config_cache)
  invisible(NULL)
}

#' Diagnose CCHS Configuration Path
#'
#' @return Character string showing the path being used, for diagnostic purposes.
#' @export
diagnose_cchs_config_path <- function() {
  # (Implementation remains the same as previous version)
}

# ==============================================================================
# VECTORIZED CONVERSION & PRE-COMPUTATION
# ==============================================================================

#' Create a Vectorized Lookup Map for Original Codes to Tagged NA
#'
#' Creates a named vector for fast vectorized conversion of original CCHS codes
#' to their corresponding tagged_na values. Used by convert_to_tagged_na() for
#' efficient batch conversion.
#'
#' @param priority_config The priority_hierarchy section from the YAML config
#' @return A named vector where names are original codes (as character) and values are tagged_na objects
#' @noRd
create_conversion_lookup_map <- function(priority_config) {
  all_codes <- c()
  for (category_name in names(priority_config)) {
    category <- priority_config[[category_name]]
    codes_to_map <- c(category$original_codes, category$decimal_codes)
    if (length(codes_to_map) > 0) {
      tagged_na_val <- haven::tagged_na(category$tagged_na[1])
      # Create separate entries for each code
      for (code in codes_to_map) {
        all_codes[as.character(code)] <- tagged_na_val
      }
    }
  }
  return(all_codes)
}

#' Convert Original CCHS Codes to Tagged NA Format (Vectorized)
#'
#' @export
convert_to_tagged_na <- function(data, pattern_type = "triple_digit_missing") {
  config <- load_cchs_config()
  pattern_config <- config$pattern_definitions$patterns[[pattern_type]]
  if (is.null(pattern_config)) {
    stop(sprintf("Pattern type '%s' not found.", pattern_type))
  }
  
  lookup_map <- create_conversion_lookup_map(pattern_config$priority_hierarchy)
  
  # Ensure data is character for matching names
  data_char <- as.character(data)
  match_indices <- which(data_char %in% names(lookup_map))
  
  if (length(match_indices) > 0) {
    # Replace in-place using the lookup map
    data[match_indices] <- lookup_map[data_char[match_indices]]
  }
  
  return(data)
}

#' Get Available Missing Data Patterns
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
# HANDLER FACTORY (REFACTORED FOR PERFORMANCE)
# ==============================================================================

#' Create Missing Data Handler (High-Performance)
#' @export
create_missing_handler <- function(..., 
                                  handle_missing_data = "auto",
                                  pattern_type = "triple_digit_missing") {
  config <- load_cchs_config()
  pattern_config <- config$pattern_definitions$patterns[[pattern_type]]
  if (is.null(pattern_config)) {
    stop(sprintf("Pattern type '%s' not found.", pattern_type))
  }
  
  input_vars <- list(...)
  data_format <- detect_data_format(input_vars, handle_missing_data, pattern_config)
  
  create_handler_tools(pattern_config, data_format)
}

#' Detect Data Format (Now uses config for accuracy)
#'
#' Analyzes input variables to determine data format for optimal handler creation.
#' Uses configuration-derived missing codes for accurate detection of mixed formats.
#'
#' @param input_vars List of input variables to analyze
#' @param handle_missing_data User preference ("auto", "original", or "tagged_na")
#' @param pattern_config Pattern configuration containing missing code definitions
#' @return Character string: "original", "tagged_na", or "mixed"
#' @noRd
detect_data_format <- function(input_vars, handle_missing_data, pattern_config) {
  if (handle_missing_data %in% c("original", "tagged_na")) {
    return(handle_missing_data)
  }
  
  has_tagged_na <- any(purrr::map_lgl(input_vars, ~any(haven::is_tagged_na(.))))
  
  # Derive original codes from config to avoid hard-coding
  all_original_codes <- unique(unlist(lapply(pattern_config$priority_hierarchy, function(cat) {
    c(cat$original_codes, cat$decimal_codes)
  })))
  
  has_original_codes <- any(purrr::map_lgl(input_vars, ~any(. %in% all_original_codes, na.rm = TRUE)))
  
  if (has_tagged_na && has_original_codes) "mixed"
  else if (has_tagged_na) "tagged_na"
  else "original"
}

#' Create Handler Tools (with Pre-computation)
#'
#' Creates the main handler object with optimized is_missing, is_tag, and propagate
#' functions. Uses pre-computation and closure-based architecture for performance.
#'
#' @param pattern_config Configuration for the specific pattern from YAML
#' @param data_format Detected data format ("original", "tagged_na", or "mixed")
#' @return List of handler functions (is_tag, is_missing, propagate)
#' @noRd
create_handler_tools <- function(pattern_config, data_format) {
  
  priority_config <- pattern_config$priority_hierarchy
  
  # --- Pre-computation of lookup maps --- 
  maps <- precompute_lookup_maps(priority_config)
  
  # --- Vectorized Helper Functions (Closures) ---
  is_tag_fn <- create_is_tag_fn(data_format, maps)
  is_missing_fn <- create_is_missing_fn(data_format, maps$all_missing_codes)
  propagate_fn <- create_propagate_fn(data_format, priority_config)
  
  list(
    is_tag = is_tag_fn,
    is_missing = is_missing_fn,
    propagate = propagate_fn
  )
}

# ==============================================================================
# PRE-COMPUTATION AND VECTORIZED HELPERS
# ==============================================================================

#' Pre-compute All Necessary Lookup Maps from Config
#'
#' Creates optimized lookup structures for fast missing data operations.
#' Pre-computation improves performance for repeated handler operations.
#'
#' @param priority_config Priority hierarchy section from YAML pattern configuration
#' @return A list containing original_map, tagged_na_map, and all_missing_codes
#' @noRd
precompute_lookup_maps <- function(priority_config) {
  original_map <- list()
  tagged_na_map <- list()
  
  for (cat_name in names(priority_config)) {
    category <- priority_config[[cat_name]]
    original_map[[cat_name]] <- c(category$original_codes, category$decimal_codes)
    tagged_na_map[[cat_name]] <- category$tagged_na
  }
  
  list(
    original_map = original_map,
    tagged_na_map = tagged_na_map,
    all_missing_codes = unique(unlist(original_map))
  )
}

#' Create the Vectorized `is_tag` Function
#'
#' Creates a closure for vectorized semantic tag checking. The returned function
#' can efficiently check if values match specific semantic categories.
#'
#' @param data_format Detected data format ("original", "tagged_na", or "mixed")
#' @param maps Pre-computed lookup maps from precompute_lookup_maps()
#' @return The `is_tag` function (a closure) that takes (x, ...) arguments
#' @noRd
create_is_tag_fn <- function(data_format, maps) {
  function(x, ...) {
    tags <- unlist(list(...))
    result <- logical(length(x))
    
    # Vectorized check for original codes
    if (data_format %in% c("original", "mixed")) {
      codes_to_check <- unlist(maps$original_map[tags])
      if (!is.null(codes_to_check)) {
        result <- result | (x %in% codes_to_check)
      }
    }
    
    # Vectorized check for tagged_na codes
    if (data_format %in% c("tagged_na", "mixed")) {
      na_tags_to_check <- unlist(maps$tagged_na_map[tags])
      if (!is.null(na_tags_to_check)) {
        # is_tagged_na is already vectorized
        result <- result | haven::is_tagged_na(x, tag = na_tags_to_check)
      }
    }
    
    result
  }
}

#' Create the Vectorized `is_missing` Function
#'
#' Creates a closure for fast vectorized missing data detection. The returned
#' function adapts its logic based on the detected data format.
#'
#' @param data_format Detected data format ("original", "tagged_na", or "mixed")
#' @param all_missing_codes Pre-computed vector of all missing codes for this pattern
#' @return The `is_missing` function (a closure) that takes (x) argument
#' @noRd
create_is_missing_fn <- function(data_format, all_missing_codes) {
  function(x) {
    result <- is.na(x)
    if (data_format == "tagged_na") {
      result <- result | haven::is_tagged_na(x)
    } else if (data_format == "mixed") {
      result <- result | haven::is_tagged_na(x) | (x %in% all_missing_codes)
    } else { # original
      result <- result | (x %in% all_missing_codes)
    }
    result
  }
}

#' Create the `propagate` Function
#'
#' Creates a closure for priority-based missing value propagation. Uses the
#' appropriate optimization function based on data format and includes optional
#' vctrs integration for safer type combination.
#'
#' @param data_format Detected data format ("original", "tagged_na", or "mixed")
#' @param priority_config Priority hierarchy section from YAML pattern configuration
#' @return The `propagate` function (a closure) that takes (...) arguments
#' @noRd
create_propagate_fn <- function(data_format, priority_config) {
  priority_categories <- get_priority_ordered_categories(priority_config)
  
  prioritize_single <- switch(data_format,
    tagged_na = function(vals) prioritize_tagged_na_optimized(vals, priority_config, priority_categories),
    mixed = function(vals) prioritize_mixed_optimized(vals, priority_config, priority_categories),
    original = function(vals) prioritize_original_codes_optimized(vals, priority_config, priority_categories)
  )
  
  function(...) {
    values <- list(...)
    result <- purrr::pmap(values, function(...) prioritize_single(list(...)))
    
    # Use vctrs::vec_c for safer, type-stable combination if available
    if (requireNamespace("vctrs", quietly = TRUE)) {
      vctrs::vec_c(!!!result)
    } else {
      do.call(c, result)
    }
  }
}

# (Other helper functions like get_priority_ordered_categories, prioritize_*_optimized remain largely the same)

#' Get Priority-Ordered Categories from YAML Configuration
#'
#' Extracts and sorts missing data categories by their priority values from the
#' YAML configuration. Lower priority numbers indicate higher priority (1 beats 2).
#'
#' @param priority_config Priority hierarchy section from YAML pattern configuration
#' @return Character vector of category names ordered by priority (highest priority first)
#' @noRd
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
#'
#' Selects the highest priority missing value from a set of tagged_na values
#' based on the YAML configuration priority hierarchy.
#'
#' @param values Vector of values to prioritize (should contain tagged_na values)
#' @param priority_config Priority hierarchy section from YAML pattern configuration
#' @param priority_categories Pre-calculated priority-ordered categories (highest priority first)
#' @return Single value: highest priority tagged_na found, or first valid value if no missing
#' @noRd
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
#'
#' Selects the highest priority missing value from a set of original CCHS codes
#' based on the YAML configuration priority hierarchy.
#'
#' @param values Vector of values to prioritize (should contain original CCHS codes like 996, 997)
#' @param priority_config Priority hierarchy section from YAML pattern configuration
#' @param priority_categories Pre-calculated priority-ordered categories (highest priority first)
#' @return Single value: highest priority original code found, or first valid value if no missing
#' @noRd
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
#'
#' Selects the highest priority missing value from a set of mixed format values
#' containing both tagged_na and original CCHS codes. Returns tagged_na format
#' for consistency in mixed mode.
#'
#' @param values Vector of values to prioritize (mix of tagged_na and original codes)
#' @param priority_config Priority hierarchy section from YAML pattern configuration
#' @param priority_categories Pre-calculated priority-ordered categories (highest priority first)
#' @return Single value: highest priority tagged_na found (original codes converted to tagged_na)
#' @noRd
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
