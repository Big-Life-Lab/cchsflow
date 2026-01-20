# ==============================================================================
# Smoking Domain Validation Constants - v3.0.0 Architecture
# ==============================================================================
#
# REQUIRED DEPENDENCIES:
#   None - this file contains only constants
#
# PURPOSE:
# Smoking-specific constants that cannot be expressed in variable_details.csv.
# These include calculation constants, derived thresholds, and evidence-based
# parameters used by smoking derived variable functions.
#
# IMPORTANT:
# - Validation bounds for recoding (recStart/recEnd) are defined in
#   variable_details.csv - that file is authoritative for those values
# - This file contains only constants needed for function calculations
# - Evidence base documented in harmonization-development/smoking/05-pack-years/
#
# ==============================================================================

# ==============================================================================
# 1. PACK-YEARS CALCULATION CONSTANTS
# ==============================================================================

#' Pack-years calculation constants
#'
#' These constants are used in calculate_pack_years_pumf() and
#' calculate_pack_years_master() functions. They cannot be expressed in
#' variable_details.csv because they are calculation parameters, not
#' recoding bounds.
#'
#' Evidence base:
#' - MIN_PACK_YEARS: 100 cigarettes / 7300 = 0.0137 (NHIS/BRFSS "established smoker")
#' - MIN_PACK_YEARS_ALT: 50 cigarettes / 7300 = 0.007 (youth/experimental smoker)
#' - MAX_PACK_YEARS: Empirical ceiling from ATBC (162) and Pain & Health (165) studies
#'
#' @seealso harmonization-development/smoking/05-pack-years/L2_semantic_mapping.md
PACK_YEARS_CONSTANTS <- list(

  # Calculation constants

  cigarettes_per_pack = 20,

  days_per_month = 30,


  # Minimum values (floor for trace smokers)
  # Critical for: preserving "established smoker" status, log transformations

  min_pack_years = 0.0137,

  min_pack_years_alt = 0.007,


  # Output validation bounds

  # Values outside [0, max_pack_years] are set to NA::b
  max_pack_years = 165
)

# ==============================================================================
# 2. VALIDATION BOUNDS REFERENCE
# ==============================================================================
#
# All validation bounds are defined in variable_details.csv (authoritative).
# This section documents where to find them - do not duplicate as R constants.
#
# SMOKING INITIATION AGE (Holford et al. evidence):
#   - SMK_203, SMK_207, SMK_01C, SMK_040: recStart [8;99]
#   - SMKG203_cont, SMKG207_cont, SMKG01C_cont: categorical recStart (1-11)
#   - Evidence: Holford et al. Smoking History Generator (min age 8)
#
# TIME SINCE QUIT:
#   - time_quit_smoking: output validated in Func::calculate_time_quit_smoking
#   - Valid range: 0.5 to 82 years (enforced in function, not CSV)
#   - SMKDSTP: recStart [0;79], [0;82], [0;88] per DDI cycle
#
# SMOKING INTENSITY:
#   - SMK_204, SMK_208, SMK_05B: recStart [1;99] (cigarettes per day)
#   - SMK_05C: recStart [0;31] (days per month)
#
# PACK-YEARS OUTPUT:
#   - pack_years_der: recStart [0;165] (ATBC/Pain & Health evidence)
#
# CURRENT AGE (in R/validation-constants.R):
#   - DEMOGRAPHIC_BOUNDS$DHHGAGE_cont: [12, 102]
#
# To update bounds, modify:
#   - inst/extdata/variable_details.csv (production)
#   - harmonization-development/smoking/*/variable_details_draft.csv (development)
#
