# ==============================================================================
# DEPRECATED FUNCTION ALIASES - v3.0.0 Function Renaming
# ==============================================================================
#
# This file provides backward compatibility for the v3.0.0 function renaming.
# These aliases will be maintained for one major version before removal.
#
# New function names follow R/Tidyverse conventions:
# - Verb-first naming (calculate_, assess_, categorize_, score_)
# - Clear, descriptive function names
# - Consistent patterns across all functions
#
# Migration timeline:
# - v3.0.0: New names introduced, old names deprecated with warnings
# - v3.1.0: Deprecation warnings continue
# - v4.0.0: Old names removed entirely

# ==============================================================================
# BMI FUNCTION ALIASES
# ==============================================================================

#' @rdname calculate_bmi
#' @export
bmi_fun <- function(...) {
  .Deprecated("calculate_bmi", 
              msg = "bmi_fun() is deprecated. Use calculate_bmi() instead. See ?calculate_bmi for details.")
  calculate_bmi(...)
}

#' @rdname adjust_bmi
#' @export
adjusted_bmi_fun <- function(...) {
  .Deprecated("adjust_bmi", 
              msg = "adjusted_bmi_fun() is deprecated. Use adjust_bmi() instead. See ?adjust_bmi for details.")
  adjust_bmi(...)
}

#' @rdname categorize_bmi
#' @export
bmi_fun_cat <- function(...) {
  .Deprecated("categorize_bmi", 
              msg = "bmi_fun_cat() is deprecated. Use categorize_bmi() instead. See ?categorize_bmi for details.")
  categorize_bmi(...)
}

# ==============================================================================
# ADL FUNCTION ALIASES
# ==============================================================================

#' @rdname assess_adl
#' @export
adl_fun <- function(...) {
  .Deprecated("assess_adl", 
              msg = "adl_fun() is deprecated. Use assess_adl() instead. See ?assess_adl for details.")
  assess_adl(...)
}

#' @rdname score_adl
#' @export
adl_score_5_fun <- function(...) {
  .Deprecated("score_adl", 
              msg = "adl_score_5_fun() is deprecated. Use score_adl() instead. See ?score_adl for details.")
  score_adl(...)
}

#' @rdname score_adl_6
#' @export
adl_score_6_fun <- function(...) {
  .Deprecated("score_adl_6", 
              msg = "adl_score_6_fun() is deprecated. Use score_adl_6() instead. See ?score_adl_6 for details.")
  score_adl_6(...)
}

# ==============================================================================
# ALCOHOL FUNCTION ALIASES
# ==============================================================================

#' @rdname assess_binge_drinking
#' @export
binge_drinker_fun <- function(...) {
  .Deprecated("assess_binge_drinking", 
              msg = "binge_drinker_fun() is deprecated. Use assess_binge_drinking() instead. See ?assess_binge_drinking for details.")
  assess_binge_drinking(...)
}

#' @rdname assess_drinking_risk_short
#' @export
low_drink_short_fun <- function(...) {
  .Deprecated("assess_drinking_risk_short", 
              msg = "low_drink_short_fun() is deprecated. Use assess_drinking_risk_short() instead. See ?assess_drinking_risk_short for details.")
  assess_drinking_risk_short(...)
}

#' @rdname assess_drinking_risk_long
#' @export
low_drink_long_fun <- function(...) {
  .Deprecated("assess_drinking_risk_long", 
              msg = "low_drink_long_fun() is deprecated. Use assess_drinking_risk_long() instead. See ?assess_drinking_risk_long for details.")
  assess_drinking_risk_long(...)
}

# ==============================================================================
# PHYSICAL ACTIVITY FUNCTION ALIASES
# ==============================================================================

#' @rdname calculate_energy_expenditure
#' @export
energy_exp_fun <- function(...) {
  .Deprecated("calculate_energy_expenditure", 
              msg = "energy_exp_fun() is deprecated. Use calculate_energy_expenditure() instead. See ?calculate_energy_expenditure for details.")
  calculate_energy_expenditure(...)
}

# ==============================================================================
# MIGRATION GUIDANCE
# ==============================================================================

# For users migrating code, use find/replace with these patterns:
#
# BMI Functions:
#   bmi_fun(           → calculate_bmi(
#   adjusted_bmi_fun(  → adjust_bmi(
#   bmi_fun_cat(       → categorize_bmi(
#
# ADL Functions:
#   adl_fun(           → assess_adl(
#   adl_score_5_fun(   → score_adl(
#   adl_score_6_fun(   → score_adl_6(
#
# Alcohol Functions:
#   binge_drinker_fun(       → assess_binge_drinking(
#   low_drink_short_fun(     → assess_drinking_risk_short(
#   low_drink_long_fun(      → assess_drinking_risk_long(
#
# Physical Activity:
#   energy_exp_fun(    → calculate_energy_expenditure(