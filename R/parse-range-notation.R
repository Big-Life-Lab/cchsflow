# ==============================================================================
# Superior Range Parser for Variable Details
# ==============================================================================
#
# This file contains the BEST VERSION of parse_range_notation() function
# copied from development/convert_cont_to_cat_infrastructure/R/convert_cont_to_cat.R
# 
# This is our foundational bottom-level function that supports both:
# - Integer ranges: [7,9] → integers 7,8,9  
# - Continuous ranges: [18.5,25) → 18.5 ≤ x < 25 with mathematical bracket notation
#
# DEVELOPMENT STATUS: Ready for use as foundation function
# LOCATION: development/flexible-missing-data-mvp/R/parse_range_notation.R
# VERSION: v3.0.0, copied 2025-07-30

# REQUIRED DEPENDENCIES:
library(haven) # for haven::tagged_na() and haven::is_tagged_na()

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
#' @note v3.0.0, last updated: 2025-07-30, status: active, Note: Superior version with mathematical bracket notation
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
  # Support both [] and () bracket types for mathematical notation
  
  # Check for bracket structure
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
    # Detect continuous ranges by:
    # 1. Mathematical bracket notation (half-open intervals)
    # 2. Decimal values
    # 3. Infinity values
    # 4. Explicitly non-inclusive brackets
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
    # Generate integer sequence if requested and bounds are finite
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