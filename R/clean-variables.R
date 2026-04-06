# ==============================================================================
# Variable Cleaning - Preprocessing with Output Format Control
# ==============================================================================
#
# clean_variables() function for:
# - Preprocessing: Converting raw missing codes to detectable format
# - Output Format Control: Supporting both "tagged_na" and "original" formats
# - Integration: Working with any_missing() and get_priority_missing()
#
# @note Depends on: missing-pattern-cache.R (get_complete_pattern)

# Session-level cache for pattern detection warnings
# Use an environment that persists across function calls
.cchsflow_cache <- new.env(parent = emptyenv())
.cchsflow_cache$pattern_warnings <- new.env(parent = emptyenv())

.get_pattern_warnings_cache <- function() {
  .cchsflow_cache$pattern_warnings
}

# ==============================================================================
# MAIN FUNCTION - LEVEL 6 API
# ==============================================================================

#' Variable Cleaning with Output Format Control
#'
#' Preprocessing function that converts raw missing codes to detectable format
#' while preserving output format choice. Uses Level 4 pattern detection to
#' handle mixed missing code patterns (6,7,8,9 OR 996,997,998,999).
#'
#' @param vars Named list of variables to clean
#' @param output_format Character. Output format ("tagged_na" or "original")
#' @param check_length Logical. Whether to validate all variables have same length (default TRUE)
#' @return Named list with cleaned variables ready for Step 2 domain logic
#' @export
#'
#' @examples
#' # PUMF data with single-digit codes
#' clean_variables(vars = list(height = c(1.75, 6, 7)), output_format = "tagged_na")
#' 
#' # Master data with triple-digit codes  
#' clean_variables(vars = list(height = c(1.75, 996, 997)), output_format = "original")
#' 
#' # Multiple variables with automatic length validation
#' clean_variables(vars = list(
#'   height = c(1.75, 1.80, 999), 
#'   weight = c(70, 997, 85)
#' ), output_format = "tagged_na")
#' 
#' # Disable length validation when needed
#' clean_variables(vars = list(
#'   var1 = c(1, 2), 
#'   var2 = c(3, 4, 5)
#' ), output_format = "tagged_na", check_length = FALSE)
clean_variables <- function(vars, output_format = "tagged_na", check_length = TRUE) {
  
  # Enhanced input validation
  if (is.null(vars) || length(vars) == 0) {
    stop("vars parameter is required and must be a non-empty list")
  }
  
  if (!is.list(vars)) {
    stop("vars must be a named list")
  }
  
  if (is.null(names(vars)) || any(names(vars) == "")) {
    stop("All variables in vars list must be named")
  }
  
  if (!output_format %in% c("tagged_na", "original")) {
    stop("output_format must be 'tagged_na' or 'original'")
  }
  
  if (!is.logical(check_length) || length(check_length) != 1) {
    stop("check_length must be a single logical value (TRUE or FALSE)")
  }
  
  # Validate variable names are character strings
  var_names <- names(vars)
  if (!all(nzchar(var_names))) {
    stop("All variable names must be non-empty character strings")
  }
  
  # Validate each variable contains data that can be processed
  for (var_name in var_names) {
    var_data <- vars[[var_name]]
    # Skip validation for NULL values (they're handled in length validation)
    if (is.null(var_data)) {
      next
    }
    if (!is.vector(var_data) && !is.factor(var_data)) {
      stop("Variable '", var_name, "' must be a vector or factor, not ", class(var_data)[1])
    }
    if (length(var_data) == 0) {
      warning("Variable '", var_name, "' is empty and will return empty result")
    }
  }
  
  # Vector length validation (when check_length = TRUE)
  if (check_length && length(vars) > 1) {
    # Handle NULL values (exclude from length check)
    non_null_vars <- vars[!sapply(vars, is.null)]
    
    if (length(non_null_vars) > 1) {
      lengths <- sapply(non_null_vars, length)
      
      # Check if all lengths are the same
      if (length(unique(lengths)) > 1) {
        var_names <- names(non_null_vars)
        
        # Generate appropriate error message
        if (length(var_names) == 2) {
          stop(paste(var_names, collapse = " and "), " must have the same length")
        } else {
          stop("All input vectors (", paste(var_names, collapse = ", "), ") must have the same length")
        }
      }
    }
  }
  
  result <- list()
  
  for (var_name in names(vars)) {
    var_data <- vars[[var_name]]
    
    # Use Level 4 infrastructure to get missing pattern
    pattern <- tryCatch({
      get_complete_pattern(var_name)
    }, error = function(e) {
      # Fallback to default CCHS pattern when metadata lookup fails
      # This allows functions to work before worksheets are fully migrated
      warning_key <- paste0("pattern_fallback_", var_name)
      cache <- .get_pattern_warnings_cache()
      if (!exists(warning_key, envir = cache)) {
        assign(warning_key, TRUE, envir = cache)
        warning("Using default CCHS pattern for '", var_name,
                "' (metadata lookup failed). ", call. = FALSE)
      }
      # Return default CCHS single-digit pattern (most common for categorical vars)
      list(
        na_a_codes = c(6, 96, 996),   # Not applicable
        na_b_codes = c(7, 8, 9, 97, 98, 99, 997, 998, 999),  # Not stated/don't know/refusal
        copy_mappings = list(list(min = 1, max = 95)),  # Valid range for most vars
        else_mappings = list()
      )
    })
    
    # Process data using pattern and output format
    processed_data <- process_missing_codes(var_data, pattern, output_format)
    
    result[[var_name]] <- processed_data
  }
  
  return(result)
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Process Missing Codes with Pattern and Output Format
#'
#' Central processing function that handles both input conversion and output format.
#' Always converts to tagged_na internally for processing, then formats output.
#'
#' @param var_data Vector of variable data
#' @param pattern List with na_a_codes and na_b_codes from Level 4
#' @param output_format Character output format
#' @return Vector in requested format
#' @noRd
process_missing_codes <- function(var_data, pattern, output_format) {
  
  # Input validation
  if (length(var_data) == 0) {
    return(var_data)
  }
  
  if (is.null(pattern)) {
    stop("pattern parameter cannot be NULL")
  }
  
  if (!is.list(pattern)) {
    stop("pattern must be a list structure")
  }
  
  if (!output_format %in% c("tagged_na", "original")) {
    stop("output_format must be 'tagged_na' or 'original'")
  }
  
  # Coerce character/factor inputs to numeric (rec_with_table passes factor

  # levels as character strings; gold-tier functions expect numeric)
  if (is.factor(var_data)) {
    var_data <- as.numeric(levels(var_data))[var_data]
  } else if (is.character(var_data)) {
    var_data <- suppressWarnings(as.numeric(var_data))
  }

  # Step 1: Convert input codes to tagged_na (for processing)
  tagged_data <- convert_input_to_tagged_na(var_data, pattern)
  
  # Step 2: Apply else logic if needed (for values not yet converted)
  tagged_data <- apply_else_logic(tagged_data, var_data, pattern)
  
  # Step 3: Return in requested format
  if (output_format == "tagged_na") {
    return(tagged_data)
  } else if (output_format == "original") {
    return(convert_tagged_na_to_original_codes(tagged_data, pattern))
  } else {
    stop("Invalid output_format: ", output_format)
  }
}

#' Convert Input Missing Codes to Tagged NA
#'
#' Converts various missing code formats to tagged_na for internal processing.
#' Uses Level 4 pattern data to handle mixed input patterns (6,7 vs 996,997).
#'
#' @param var_data Vector of variable data
#' @param pattern List with na_a_codes and na_b_codes from Level 4
#' @return Vector with tagged_na values
#' @noRd
convert_input_to_tagged_na <- function(var_data, pattern) {
  
  if (is.null(pattern) || (is.null(pattern$na_a_codes) && is.null(pattern$na_b_codes))) {
    return(var_data)
  }
  
  result <- var_data
  
  # Convert NA::a codes (Not Applicable)
  if (!is.null(pattern$na_a_codes) && length(pattern$na_a_codes) > 0) {
    for (code in pattern$na_a_codes) {
      if (is.numeric(code)) {
        result[!is.na(var_data) & var_data == code] <- haven::tagged_na("a")
      }
    }
  }
  
  # Convert NA::b codes (Not Stated)
  if (!is.null(pattern$na_b_codes) && length(pattern$na_b_codes) > 0) {
    for (code in pattern$na_b_codes) {
      if (is.numeric(code)) {
        result[!is.na(var_data) & var_data == code] <- haven::tagged_na("b")
      }
    }
  }
  
  return(result)
}

#' Convert Tagged NA Back to Original Missing Codes
#'
#' Converts tagged_na back to original missing codes for "original" output format.
#' Preserves original format while having processed internally for Step 2 compatibility.
#'
#' @param tagged_data Vector with tagged_na values
#' @param pattern List with na_a_codes and na_b_codes from Level 4
#' @return Vector with original missing codes
#' @noRd
convert_tagged_na_to_original_codes <- function(tagged_data, pattern) {
  
  result <- tagged_data
  
  # Get representative codes (use first from each list as representative)
  na_a_code <- if (!is.null(pattern$na_a_codes) && length(pattern$na_a_codes) > 0) {
    pattern$na_a_codes[1]
  } else {
    996  # Default fallback
  }
  
  na_b_code <- if (!is.null(pattern$na_b_codes) && length(pattern$na_b_codes) > 0) {
    pattern$na_b_codes[1]
  } else {
    999  # Default fallback
  }
  
  # Convert back to original codes
  result[haven::is_tagged_na(tagged_data, "a")] <- na_a_code
  result[haven::is_tagged_na(tagged_data, "b")] <- na_b_code
  
  return(as.numeric(result))
}

#' Apply Else Logic to Out-of-Range Values
#'
#' Processes values that fall outside valid ranges using else mappings.
#' This function handles the core else functionality by checking values against
#' copy_mappings (valid ranges) and applying else_mappings rules.
#'
#' @param tagged_data Vector with tagged_na values from missing code conversion
#' @param original_data Vector with original raw data for else logic reference
#' @param complete_pattern List with copy_mappings and else_mappings from Level 4
#' @return Vector with else logic applied to out-of-range values
#' @noRd
apply_else_logic <- function(tagged_data, original_data, complete_pattern) {
  
  # Early return if no else mappings or copy mappings available
  if (is.null(complete_pattern$else_mappings) || 
      is.null(complete_pattern$copy_mappings) ||
      length(complete_pattern$else_mappings) == 0 ||
      length(complete_pattern$copy_mappings) == 0) {
    return(tagged_data)
  }
  
  result <- tagged_data
  
  # Check each value that hasn't been converted to tagged_na yet
  for (i in seq_along(result)) {
    
    # Skip if already converted to tagged_na
    if (haven::is_tagged_na(result[i])) {
      next
    }
    
    current_value <- original_data[i]
    
    # Skip if current value is NA
    if (is.na(current_value)) {
      next
    }
    
    # Check if value falls within any valid range (copy_mappings)
    in_valid_range <- FALSE
    
    for (copy_mapping in complete_pattern$copy_mappings) {
      if (is_value_in_range(current_value, copy_mapping)) {
        in_valid_range <- TRUE
        break
      }
    }
    
    # If not in valid range, apply else logic
    if (!in_valid_range) {
      else_result <- apply_else_rule(current_value, complete_pattern$else_mappings)
      if (!is.null(else_result)) {
        result[i] <- else_result
      }
    }
  }
  
  return(result)
}

#' Check if Value is in Valid Range
#'
#' Helper function to determine if a value falls within a specified range.
#'
#' @param value Numeric value to check
#' @param range_spec List with range specification from copy_mappings
#' @return Logical indicating if value is in range
#' @noRd
is_value_in_range <- function(value, range_spec) {

  if (is.null(range_spec) || is.null(value) || is.na(value)) {
    return(FALSE)
  }

  # Handle different range specification formats
  if (!is.null(range_spec$min) && !is.null(range_spec$max)) {
    # Continuous range: min to max
    return(value >= range_spec$min && value <= range_spec$max)

  } else if (!is.null(range_spec$recStart) && is.character(range_spec$recStart)) {
    # "copy" means pass-through: any numeric value is valid (continuous source)
    if (range_spec$recStart == "copy") {
      return(TRUE)
    }
    # copy_mapping format from map_recStart_to_recEnd: parse recStart notation
    parsed <- parse_range_notation(range_spec$recStart)
    if (!is.null(parsed) && parsed$type == "continuous") {
      lower_ok <- if (parsed$min_inclusive) value >= parsed$min else value > parsed$min
      upper_ok <- if (parsed$max_inclusive) value <= parsed$max else value < parsed$max
      return(lower_ok && upper_ok)
    }
    if (!is.null(parsed) && parsed$type == "integer") {
      # Integer range: use min/max as inclusive boundaries
      return(value >= parsed$min && value <= parsed$max)
    }
    if (!is.null(parsed) && parsed$type == "single_value") {
      return(value == parsed$value)
    }
    # Fallback: use recStart_values if available
    if (!is.null(range_spec$recStart_values) && length(range_spec$recStart_values) > 0) {
      return(value >= min(range_spec$recStart_values) && value <= max(range_spec$recStart_values))
    }
    return(FALSE)

  } else if (!is.null(range_spec$values)) {
    # Discrete values: specific allowed values
    return(value %in% range_spec$values)

  } else if (!is.null(range_spec$pattern)) {
    # Pattern-based range (could be implemented for complex patterns)
    # For now, return FALSE as this would require pattern matching
    return(FALSE)

  } else {
    # Unknown range format
    return(FALSE)
  }
}

#' Apply Else Rule to Out-of-Range Value
#'
#' Applies the appropriate else mapping rule to a value that falls outside valid ranges.
#'
#' @param value Numeric value that is out of range
#' @param else_mappings List of else rules from complete_pattern
#' @return Tagged NA value or NULL if no rule applies
#' @noRd
apply_else_rule <- function(value, else_mappings) {
  
  if (is.null(else_mappings) || length(else_mappings) == 0) {
    return(NULL)
  }
  
  # Apply first matching else rule
  for (else_rule in else_mappings) {

    # Support both $action (expected) and $recEnd (from map_recStart_to_recEnd)
    action <- else_rule$action
    if (is.null(action)) {
      action <- else_rule$recEnd
    }

    if (is.null(action)) {
      next
    }

    # Handle different else actions
    if (action == "NA::a") {
      return(haven::tagged_na("a"))

    } else if (action == "NA::b") {
      return(haven::tagged_na("b"))

    } else if (action == "skip" || action == "SKIP") {
      # Keep original value unchanged
      return(NULL)

    } else if (grepl("^[0-9]+$", action)) {
      # Numeric replacement value
      replacement_value <- as.numeric(action)
      return(replacement_value)

    } else {
      # Unknown action, skip
      next
    }
  }
  
  # If no rule matched, return NULL (keep original value)
  return(NULL)
}


# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

#' Pass-through for worksheet-routed derived variables
#'
#' Standard implementation for derived variables where the worksheet handles
#' PUMF/Master source routing. The function receives a single input, cleans it
#' using `clean_variables()` metadata, and returns the result.
#'
#' This eliminates boilerplate for variables like `age_start_smoking`,
#' `age_first_cigarette`, and `smoked_100_lifetime` where the function body
#' is identical: NULL check, empty check, clean, return.
#'
#' @param value Numeric vector. The input value(s) from the worksheet-routed
#'   source variable. NULL if the variable is not in the dataset.
#' @param variable_name Character. The cchsflow variable name (e.g.,
#'   "age_start_smoking"). Used for `clean_variables()` pattern lookup and
#'   `assign_missing()` labelling.
#' @param output_format Character. Output format ("tagged_na" or "original").
#'
#' @return Cleaned numeric vector with missing codes in the requested format.
#'   Returns a single NA::b for NULL input, `numeric(0)` for empty input.
#'
#' @examples
#' \dontrun{
#' # Inside a derived variable function:
#' calculate_age_start_smoking <- function(age_start_smoking = NULL,
#'                                         output_format = "tagged_na") {
#'   derive_passthrough(age_start_smoking, "age_start_smoking", output_format)
#' }
#' }
#'
#' @noRd
derive_passthrough <- function(value, variable_name, output_format = "tagged_na") {

  # NULL input — variable not in dataset
  if (is.null(value)) {
    return(assign_missing("not_stated", variable_name, output_format))
  }

  # Empty input
  if (length(value) == 0) return(numeric(0))

  # Clean using variable_details.csv metadata and return
  cleaned <- clean_variables(
    vars = stats::setNames(list(value), variable_name),
    output_format = output_format
  )

  return(cleaned[[variable_name]])
}


#' Range notation parser for variable_details.csv
#'
#' Parses range notation from variable_details.csv supporting both integer ranges
#' (like [7,9] meaning integers 7,8,9) and continuous ranges (like [18.5,25) meaning
#' 18.5 ≤ x < 25).
#'
#' @param range_string Character string containing range notation
#' @param range_type Character indicating expected range type:
#'   - "auto" (default): Auto-detect based on bracket notation and decimal values
#'   - "integer": Force integer range interpretation (generates sequence)
#'   - "continuous": Force continuous range interpretation
#' @param expand_integers Logical. If TRUE and range_type is "integer",
#'   returns all integers in the range as a vector
#'
#' @return For continuous ranges: List with min, max, min_inclusive, max_inclusive
#'   For integer ranges: List with min, max, values (if expand_integers=TRUE)
#'   Returns NULL if parsing fails
#'
#' @details
#' **Supported Patterns:**
#' - Integer ranges: `[7,9]` → integers 7,8,9
#' - Continuous ranges: `[18.5,25)` → 18.5 ≤ x < 25
#' - Continuous ranges: `[18.5,25]` → 18.5 ≤ x ≤ 25
#' - Infinity ranges: `[30,inf)` → x ≥ 30
#' - Special codes: `NA::a`, `NA::b`, `copy`, `else` (passed through unchanged)
#' - Function calls: `Func::function_name` (passed through unchanged)
#'
#' **Mathematical Bracket Notation:**
#' - `[a,b]` - Closed interval: a ≤ x ≤ b
#' - `[a,b)` - Half-open interval: a ≤ x < b
#' - `(a,b]` - Half-open interval: a < x ≤ b
#' - `(a,b)` - Open interval: a < x < b
#'
#' **Auto-Detection Logic:**
#' - Contains decimal values → continuous range
#' - Uses mathematical bracket notation `[a,b)` → continuous range
#' - Simple `[integer,integer]` → integer range (generates sequence)
#' - Contains "inf" → continuous range
#'
#' @examples
#' # Integer ranges (existing pattern)
#' parse_range_notation("[7,9]")
#' # Returns: list(min=7, max=9, values=c(7,8,9), type="integer")
#'
#' # Continuous ranges (new functionality)
#' parse_range_notation("[18.5,25)")
#' # Returns: list(min=18.5, max=25, min_inclusive=TRUE, max_inclusive=FALSE, type="continuous")
#'
#' parse_range_notation("[30,inf)")
#' # Returns: list(min=30, max=Inf, min_inclusive=TRUE, max_inclusive=FALSE, type="continuous")
#'
#' # Special cases
#' parse_range_notation("NA::a")   # Returns: list(type="special", value="NA::a")
#' parse_range_notation("copy")    # Returns: list(type="special", value="copy")
#' parse_range_notation("else")    # Returns: list(type="special", value="else")
#'
#' @keywords internal
parse_range_notation <- function(range_string, range_type = "auto", expand_integers = TRUE) {
  # Handle NULL, NA, or empty inputs
  if (is.null(range_string) || is.na(range_string) || range_string == "" || range_string == "N/A") {
    return(NULL)
  }

  # Clean input
  range_clean <- trimws(range_string)

  # Handle special codes (NA::a, NA::b, copy, else, etc.)
  if (grepl("^(NA::[ab]|copy|else)$", range_clean)) {
    return(list(
      type = "special",
      value = range_clean
    ))
  }

  # Handle function calls (Func::function_name)
  if (grepl("^Func::", range_clean)) {
    return(list(
      type = "function",
      value = range_clean
    ))
  }

  # Handle single numeric values (not ranges)
  if (grepl("^[0-9]+\\.?[0-9]*$", range_clean)) {
    numeric_val <- as.numeric(range_clean)
    return(list(
      type = "single_value",
      value = numeric_val,
      min = numeric_val,
      max = numeric_val
    ))
  }

  # Parse bracket notation ranges using simple character analysis
  first_char <- substr(range_clean, 1, 1)
  last_char <- substr(range_clean, nchar(range_clean), nchar(range_clean))

  if (!first_char %in% c("[", "(") || !last_char %in% c("]", ")")) {
    return(NULL)
  }

  # Extract bracket types
  left_bracket <- first_char
  right_bracket <- last_char

  # Extract content between brackets
  inner_content <- substr(range_clean, 2, nchar(range_clean) - 1)

  # Find comma position
  comma_pos <- regexpr(",", inner_content)
  if (comma_pos[1] == -1) {
    return(NULL)
  }

  min_str <- trimws(substr(inner_content, 1, comma_pos[1] - 1))
  max_str <- trimws(substr(inner_content, comma_pos[1] + 1, nchar(inner_content)))

  # Parse min value (handle "inf" and numeric values)
  if (tolower(min_str) == "inf") {
    min_val <- Inf
  } else {
    min_val <- suppressWarnings(as.numeric(min_str))
    if (is.na(min_val)) {
      return(NULL)
    }
  }

  # Parse max value (handle "inf" and numeric values)
  if (tolower(max_str) == "inf") {
    max_val <- Inf
  } else {
    max_val <- suppressWarnings(as.numeric(max_str))
    if (is.na(max_val)) {
      return(NULL)
    }
  }

  # Determine inclusivity from bracket types
  min_inclusive <- (left_bracket == "[")
  max_inclusive <- (right_bracket == "]")

  # Auto-detect range type if not specified
  if (range_type == "auto") {
    has_mathematical_notation <- (!min_inclusive || !max_inclusive)
    has_decimals <- (min_val != floor(min_val)) || (max_val != floor(max_val))
    has_infinity <- is.infinite(min_val) || is.infinite(max_val)

    if (has_mathematical_notation || has_decimals || has_infinity) {
      range_type <- "continuous"
    } else {
      range_type <- "integer"
    }
  }

  # Build result based on detected/specified type
  if (range_type == "integer") {
    if (expand_integers && is.finite(min_val) && is.finite(max_val)) {
      integer_values <- seq(from = as.integer(min_val), to = as.integer(max_val), by = 1)
    } else {
      integer_values <- NULL
    }

    return(list(
      type = "integer",
      min = as.integer(min_val),
      max = as.integer(max_val),
      values = integer_values,
      min_inclusive = min_inclusive,
      max_inclusive = max_inclusive
    ))

  } else if (range_type == "continuous") {
    return(list(
      type = "continuous",
      min = min_val,
      max = max_val,
      min_inclusive = min_inclusive,
      max_inclusive = max_inclusive
    ))
  }

  # Fallback for unrecognized type
  return(NULL)
}
