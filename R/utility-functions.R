#' @title if_else2 (DEPRECATED)
#'
#' @description **DEPRECATED**: This function is deprecated and will be removed 
#'  in a future version. Please use \code{dplyr::if_else()} instead.
#'  
#'  Custom ifelse function that evaluates missing (NA) values. If
#'  the logical argument (x) compares to a value that is `NA`, it is set to
#'  `FALSE`
#'
#' @details unlike the base ifelse() function, if_else2() is able to evaluate NA
#'  as either a or b. In base ifelse(), anything compared to NA will produce
#'  NA, which can break a function. When dealing with large datasets like the
#'  CCHS, there are many missing (NA) values. That means a special ifelse
#'  function like if_else2() is needed in order for other functions to not break
#'
#' @param x A logical argument
#'
#' @param a value if `x` is `TRUE`
#'
#' @param b value if `x` is `FALSE`
#'
#' @return a or b based on the evaluation of x
#'
#' @examples
#' age <- 12
#' status <- if_else2((age < 18), "child", "invalid age")
#' print(status)
#'
#' age <- NA
#' status <- if_else2((age < 18), "child", "invalid age")
#' print(status)
#' @export
if_else2 <- function(x, a, b) {
  # Deprecation warning - show only every 6 hours to avoid spam
  if (!exists(".if_else2_last_warning", envir = .GlobalEnv)) {
    assign(".if_else2_last_warning", Sys.time(), envir = .GlobalEnv)
    warning("if_else2() is deprecated and will be removed in a future version. ", 
            "Please use dplyr::if_else() instead for better type safety and NA handling.",
            call. = FALSE)
  } else {
    last_warning <- get(".if_else2_last_warning", envir = .GlobalEnv)
    if (difftime(Sys.time(), last_warning, units = "hours") >= 6) {
      assign(".if_else2_last_warning", Sys.time(), envir = .GlobalEnv)
      warning("if_else2() is deprecated and will be removed in a future version. ", 
              "Please use dplyr::if_else() instead for better type safety and NA handling.",
              call. = FALSE)
    }
  }
  
  falseifNA <- function(x) {
    ifelse(is.na(x), FALSE, x)
  }
  ifelse(falseifNA(x), a, b)
}

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

# REQUIRED DEPENDENCIES for modern functions:
# library(vctrs)  # for vec_size_common() and tidyverse recycling rules

#' Check vector length compatibility using tidyverse standards
#' 
#' Wrapper around vctrs::vec_size_common() to check if input vectors have 
#' compatible lengths for vectorized operations. Uses tidyverse recycling rules.
#' 
#' @param ... Input vectors to check for length compatibility
#' @return Logical indicating if vectors can be safely recycled together
#' @details 
#' Uses vctrs package recycling rules (which underlie the tidyverse):
#' - Compatible: All vectors have the same length
#' - Compatible: Some vectors are scalars (length 1) which can be recycled  
#' - Compatible: Mixed scalars and vectors of same length
#' - Incompatible: Vectors with different lengths > 1
#' 
#' This function provides the same recycling behavior as dplyr, tidyr, and
#' other tidyverse functions.
#' 
#' @note Internal v3.0.0, last updated: 2025-06-30, status: active
#' @keywords internal
#' @examples
#' # Compatible cases
#' check_vector_compatibility(c(1,2,3), c(4,5,6))        # TRUE: same length
#' check_vector_compatibility(c(1,2,3), 4)               # TRUE: scalar + vector
#' check_vector_compatibility(1, 2, 3)                   # TRUE: all scalars
#' 
#' # Incompatible cases  
#' check_vector_compatibility(c(1,2), c(4,5,6))          # FALSE: length 2 vs 3
#' check_vector_compatibility(c(1,2), c(4,5,6), c(7,8))  # FALSE: mixed lengths
check_vector_compatibility <- function(...) {
  # Use vctrs (tidyverse foundation) to check compatibility
  tryCatch({
    vctrs::vec_size_common(...)
    TRUE
  }, error = function(e) FALSE)
}