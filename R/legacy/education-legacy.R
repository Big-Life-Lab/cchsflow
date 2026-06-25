# Legacy education functions
# Preserved for backward compatibility — use derive_EDUDR04_2015plus()
# from R/education.R instead.

#'
#' @export
EDUDR04_fun <- function(EHG2_01, EHG2_02, EHG2_03, EHG2_04) {
  derive_EDUDR04_2015plus(EHG2_01, EHG2_02, EHG2_03, EHG2_04)
}
