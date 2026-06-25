# Legacy respiratory condition functions
# Preserved for backward compatibility — use derive_CCC_091_2001to2003()
# and derive_CCC_091_2005to2008() from R/respiratory-condition.R instead.

#'
#' @export
CCC_091_fun1 <- function(CCC_91A, CCC_91B) {
  derive_CCC_091_2001to2003(CCC_91A, CCC_91B)
}

#'
#' @export
CCC_091_fun2 <- function(CCC_91A, CCC_91E, CCC_91F) {
  derive_CCC_091_2005to2008(CCC_91A, CCC_91E, CCC_91F)
}
