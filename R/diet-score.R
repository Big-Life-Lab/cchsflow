FVCDPOT_add <- function(FVCDPOT) {
  if_else2(FVCDPOT>1, 1, FVCDPOT)
}

FVCDPOT_minus <- function(FVCDPOT) {
  if_else2(FVCDPOT>1, (FVCDPOT-1), 0)
}