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