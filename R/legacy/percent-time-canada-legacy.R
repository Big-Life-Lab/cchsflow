# Legacy percent-time-canada functions
# Preserved for backward compatibility — use calculate_pct_time() and
# categorize_pct_time() from R/percent-time-canada.R instead.

#'
#'
pct_time_fun <- function(age, born_in_canada, years_in_canada) {
  calculate_pct_time(age, born_in_canada, years_in_canada)
}

#'
#'
pct_time_fun_cat <- function(pct_time_der) {
  categorize_pct_time(pct_time_der)
}
