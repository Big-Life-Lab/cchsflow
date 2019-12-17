#' @title if_else2
#'
#' @description Custom ifelse function that evaluates missing (NA) values
#'
#' @details If the logical argument (x) compares to a value that is `NA`, it is
#'  set to `FALSE`
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
  falseifNA <- function(x) {
    ifelse(is.na(x), FALSE, x)
  }
  ifelse(falseifNA(x), a, b)
}
