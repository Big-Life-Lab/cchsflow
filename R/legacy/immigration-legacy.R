# Legacy immigration functions
# Preserved for backward compatibility — use categorize_immigration()
# from R/immigration.R instead.

#'
#' @export
immigration_fun <- function(immigrant_status, born_canada, ethnicity, years) {
  categorize_immigration(immigrant_status, born_canada, ethnicity, years)
}
