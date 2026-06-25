# Legacy diet functions
# Preserved for backward compatibility — use calculate_diet_score()
# and categorize_diet_score() from R/diet.R instead.

#'
#' @export
diet_score_fun <- function(FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG,
                           FVCDJUI, DHH_SEX) {
  calculate_diet_score(FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG,
                       FVCDJUI, DHH_SEX)
}

#'
#' @export
diet_score_fun_cat <- function(diet_score) {
  categorize_diet_score(diet_score)
}
