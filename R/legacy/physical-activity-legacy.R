# Legacy physical activity functions
# Preserved for backward compatibility — use calculate_energy_exp()
# and categorize_energy_exp() from R/physical-activity.R instead.

#'
#' @export
energy_exp_fun <- function(age, PAA_045, PAA_050, PAA_075, PAA_080,
                           PAADVDYS, PAADVVIG, PAYDVTOA, PAYDVADL,
                           PAYDVVIG, PAYDVDYS) {
  calculate_energy_exp(age, PAA_045, PAA_050, PAA_075, PAA_080,
                       PAADVDYS, PAADVVIG, PAYDVTOA, PAYDVADL,
                       PAYDVVIG, PAYDVDYS)
}

#'
#' @export
energy_exp_fun_cat <- function(energy_exp) {
  categorize_energy_exp(energy_exp)
}
