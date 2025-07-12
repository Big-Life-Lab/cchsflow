# ==============================================================================
# Smoking Functions
# ==============================================================================

# REQUIRED DEPENDENCIES:
library(haven) # for haven::tagged_na() and haven::is_tagged_na()
library(dplyr) # for dplyr::case_when() and dplyr::if_else()

# Source required helper functions (conditional loading for package context)
tryCatch(
  {
    source("R/missing-data-helpers.R", local = FALSE)
    source("R/utility-functions.R", local = FALSE)
  },
  error = function(e) {
    # Functions will be loaded via package imports during package build
  }
)

# For testing, run:
#   library(haven); library(dplyr); library(testthat)
#   source('R/smoking.R')
#   test_file('tests/testthat/test-smoking.R')

# ==============================================================================
# 1. CONSTANTS AND CONFIGURATION
# ==============================================================================

# Pack-years calculation constants (methodological, not validation)
PACK_YEARS_CONSTANTS <- list(
  cigarettes_per_pack = 20,
  min_pack_years = 0.0137, # Minimum for former occasional smokers
  min_pack_years_never = 0.007 # Minimum for never-daily smokers
)

# Smoking validation bounds (to be migrated to variable_details.csv)
# These define logical constraints for intermediate calculations
SMOKING_VALIDATION_BOUNDS <- list(
  pack_years = list(min = 0.0, max = 80.0),  # Pack-years cannot be negative, max ~80 years * 1 pack/day
  current_age = list(min = 12, max = 102),   # CCHS respondent age range
  age_started = list(min = 5, max = 95),     # Minimum plausible smoking initiation age
  time_quit = list(min = 0, max = 82),       # Time since quitting (years)
  cigarettes_daily = list(min = 1, max = 200) # Cigarettes per day range
)

# Note: These bounds will be moved to variable_details.csv as single source of truth
# Functions use explicit parameter defaults matching these validation ranges
# See @examples in function documentation for transparent validation bound usage

# ==============================================================================
# 2. DATA FLOW AND FUNCTION CATALOG
# ==============================================================================

# RECOMMENDED WORKFLOW:
#   Raw CCHS Data → rec_with_table() → Validated Results
#   
# ARCHITECTURE:
#   Derived functions → Internal helper functions → Results
#   Variable documentation → Help system access (?variable_name)

# ==============================================================================
# SMOKING VARIABLE FAMILY RELATIONSHIPS & SURVEY EVOLUTION
# ==============================================================================

# CRITICAL DESIGN PATTERN: Age Started Smoking Variable Evolution
# The SMKG variable families reflect CCHS questionnaire evolution across survey years:
#
# SMKG040 FAMILY (Age Started Smoking Daily - Combined):
#   • SMKG040 (categorical): 2015-2024 only (11 categories)
#     - Available when CCHS introduced unified question for daily/former daily
#     - Categories: 1-11 representing age ranges (5-11, 12-14, 15-17, 18-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+)
#   • SMKG040_cont (continuous): 2001-2024 (harmonized across all cycles)
#     - 2001-2014: Derived from SMKG203_cont + SMKG207_cont using legacy SMKG040_fun
#     - 2015-2024: Converted from categorical SMKG040 using midpoint values
#   • SMKG040_I (continuous): 2015-2024 only (raw continuous data from master files)
#     - Direct copy from raw CCHS [SMK_040] variable, range [5,80] years
#
# SMKG203 FAMILY (Age Started Smoking Daily - Current Daily Smokers):
#   • SMKG203_A (categorical): 2001-2014 (10 categories)
#     - Maps to age ranges: 5-11, 12-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
#   • SMKG203_B (categorical): 2005-2014 (11 categories - MORE GRANULAR than _A)
#     - Maps to age ranges: 5-11, 12-14, 15-17, 18-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
#     - Key difference: Splits 15-19 into 15-17 and 18-19 (categories 3 & 4)
#   • SMKG203_cont (continuous): 2001-2024 (harmonized continuous values)
#     - Uses midpoint conversion from categorical versions
#   • SMKG203_I (continuous): 2001-2014 (raw continuous data, equivalent to _cont)
#
# SMKG207 FAMILY (Age Started Smoking Daily - Former Daily Smokers):
#   • SMKG207_A (categorical): 2001-2014 (10 categories)
#     - Same category structure as SMKG203_A
#   • SMKG207_B (categorical): 2005-2014 (11 categories - MORE GRANULAR than _A)
#     - Same category structure as SMKG203_B
#   • SMKG207_cont (continuous): 2001-2024 (harmonized continuous values)
#   • SMKG207_I (continuous): 2001-2014 (raw continuous data, equivalent to _cont)
#
# SMKG01C FAMILY (Age Smoked First Cigarette):
#   • SMKG01C_A (categorical): 2001-2024 (10 categories)
#   • SMKG01C_B (categorical): 2005-2024 (11 categories - MORE GRANULAR than _A)
#   • SMKG01C_cont (continuous): 2001-2024 (harmonized continuous values)
#
# KEY INSIGHTS FOR RESEARCHERS:
# 1. _A vs _B Variables: _B variants have MORE categories (11 vs 10) and are MORE GRANULAR
# 2. _cont vs _I Variables: Essentially equivalent for overlapping cycles
# 3. Survey Evolution: 2015+ introduced unified questions, requiring harmonization functions
# 4. Cycle Coverage: Continuous versions often have broader coverage due to harmonization
# 5. Missing Data: All functions use haven::tagged_na() for structured missing data handling

# SPECIFIC SMOKING FUNCTION FLOWS:
#   calculate_smoking_status() → calculate_smoking_status_core() → Status category (1-6)
#   calculate_time_quit_smoking() → calculate_time_quit_core() → Time since quitting

# PACK-YEARS CALCULATION LOGIC FLOW (Complex Multi-Stage Process):
#
# ┌─────────────────────────────────────────────────────────────────────────────┐
# │                        PACK-YEARS CALCULATION FLOW                          │
# └─────────────────────────────────────────────────────────────────────────────┘
#
# Stage 1: Raw CCHS Variables → Intermediate Variables (1:1 Transformations)
#
# Smoking Status Classification:
# SMK_005 (1=daily, 2=occasional, 3=not at all), SMK_030 (1=yes, 2=no), SMK_01A (1=yes ≥100 cigs, 2=no) 
#   → SMKDSTY_A (1=daily, 2=occ-former-daily, 3=occ-never-daily, 4=former-daily, 5=former-occ, 6=never)
#
# Current Age (direct copy):
# DHHGAGE → DHHGAGE_cont (years, range: 12-102)
#
# Time Since Quit Smoking:
# SMK_09A_B (1=<1yr, 2=1-2yr, 3=2-3yr, 4=3+yr), SMKG09C (1=3-5yr, 2=6-10yr, 3=11+yr) 
#   → time_quit_smoking (continuous years since quit, SMKG09C takes precedence for 3+yr)
#
# Age Started Daily Smoking:
# SMKG203 (1-10: age ranges for current daily), SMKG207 (1-10: age ranges for former daily)
#   → SMKG203_cont, SMKG207_cont (continuous years, midpoint conversion from categories)
#
# Age First Cigarette:
# SMKG01C (1-10: age ranges) → SMKG01C_cont (continuous years, midpoint conversion)
#
# Cigarettes Per Day (context-dependent, used raw in pack-years):
# SMK_204 (daily smokers), SMK_05B (occasional), SMK_208 (former daily), SMK_05C (days/month occasional)
#
# Stage 2: Pack-Years Calculation Decision Tree
#                    ┌─────────────────┐
#                    │   SMKDSTY_A     │
#                    │ (smoking type)  │
#                    └─────────────────┘
#                             │
#         ┌───────────────────┼───────────────────┐
#         │                   │                   │
#         ▼                   ▼                   ▼
# ┌─────────────┐    ┌─────────────┐     ┌─────────────┐
# │SMKDSTY_A=1  │    │SMKDSTY_A=2  │     │SMKDSTY_A=3  │
# │Daily Smoker │    │Occ(ex-daily)│     │Occ(never)   │
# └─────────────┘    └─────────────┘     └─────────────┘
#         │                   │                   │
#         ▼                   ▼                   ▼
# ┌─────────────┐    ┌─────────────┐     ┌─────────────┐
# │(age_current │    │Complex calc │     │Lifetime exp │
# │- age_start) │    │with quit    │     │from age     │
# │* cigs/20    │    │period       │     │first smoke  │
# └─────────────┘    └─────────────┘     └─────────────┘
#         │                   │                   │
#         ▼                   ▼                   ▼
#         │       ┌───────────┼───────────────────┐
#         │       │           │                   │
#         │       ▼           ▼                   ▼
#         │ ┌─────────────┐ ┌─────────────┐     ┌─────────────┐
#         │ │SMKDSTY_A=4  │ │SMKDSTY_A=5  │     │SMKDSTY_A=6  │
#         │ │Former Daily │ │Former Occ   │     │Never Smoker │
#         │ └─────────────┘ └─────────────┘     └─────────────┘
#         │       │               │                   │
#         │       ▼               ▼                   ▼
#         │ ┌─────────────┐ ┌─────────────┐     ┌─────────────┐
#         │ │Years smoked │ │Minimum      │     │    0.0      │
#         │ │* avg cigs/20│ │pack-years   │     │pack-years   │
#         │ └─────────────┘ │(0.0137)     │     └─────────────┘
#         │       │         └─────────────┘           │
#         │       │               │                   │
#         │       │               ▼                   │
#         │       │       ┌─────────────┐             │
#         │       │       │SMK_01A≥100? │             │
#         │       │       │cigs lifetime│             │
#         │       │       └─────────────┘             │
#         │       │               │                   │
#         └───────┼───────────────┼───────────────────┘
#                 │               │
#                 ▼               ▼
#         ┌─────────────────────────────────────┐
#         │          pack_years_der             │
#         │      (Continuous, Years)            │
#         │   Final pack-years calculation      │
#         └─────────────────────────────────────┘
#
# Stage 3: Modular Function Architecture with Intermediate Variables (PROPOSED)
#
# ┌─────────────────────────────────────────────────────────────────────────────────────┐
# │                            PROPOSED INTERMEDIATE VARIABLES                          │
# │                              (Currently Missing from variables.csv)                │
# └─────────────────────────────────────────────────────────────────────────────────────┘
#
# pack_years_daily              - Pack-years for daily smokers (SMKDSTY_A=1)
# pack_years_occasional_former  - Pack-years for occasional ex-daily (SMKDSTY_A=2)  
# pack_years_occasional_never   - Pack-years for occasional never-daily (SMKDSTY_A=3)
# pack_years_former_daily       - Pack-years for former daily (SMKDSTY_A=4)
# pack_years_former_occasional  - Pack-years for former occasional (SMKDSTY_A=5)
# pack_years_never              - Pack-years for never smokers (SMKDSTY_A=6, always 0.0)
#
# ┌─────────────────────────────────────────────────────────────────────────────────────┐
# │                            FUNCTION ARCHITECTURE                                    │
# └─────────────────────────────────────────────────────────────────────────────────────┘
#
# calculate_pack_years(SMKDSTY_A, DHHGAGE_cont, time_quit_smoking, 
#                     SMKG203_cont, SMKG207_cont, SMK_204, SMK_05B, 
#                     SMK_208, SMK_05C, SMKG01C_cont, SMK_01A)
#      │
#      ├─ SMKDSTY_A=1 → calculate_pack_years_daily() → pack_years_daily
#      │               (DHHGAGE_cont, SMKG203_cont, SMK_204)
#      │
#      ├─ SMKDSTY_A=2 → calculate_pack_years_occasional_former() → pack_years_occasional_former
#      │               (DHHGAGE_cont, SMKG207_cont, time_quit_smoking, SMK_05B, SMK_208, SMK_05C)
#      │
#      ├─ SMKDSTY_A=3 → calculate_pack_years_occasional_never() → pack_years_occasional_never
#      │               (DHHGAGE_cont, SMKG01C_cont, SMK_05B, SMK_05C)
#      │
#      ├─ SMKDSTY_A=4 → calculate_pack_years_former_daily() → pack_years_former_daily
#      │               (DHHGAGE_cont, SMKG207_cont, time_quit_smoking, SMK_208)
#      │
#      ├─ SMKDSTY_A=5 → calculate_pack_years_former_occasional() → pack_years_former_occasional
#      │               (SMK_01A)
#      │
#      └─ SMKDSTY_A=6 → return 0.0 → pack_years_never
#
#                                        ↓
#                    ┌─────────────────────────────────────┐
#                    │          pack_years_der             │
#                    │    (Final composite variable)       │
#                    └─────────────────────────────────────┘
#
# INTERMEDIATE VARIABLES IDENTIFIED FOR MODULAR IMPLEMENTATION:
# ✓ SMKDSTY_A              - calculate_smoking_status() [EXISTS]
# ✓ time_quit_smoking      - calculate_time_quit_smoking() [EXISTS]  
# ✓ SMKG203_cont           - calculate_age_started_daily_current() [EXISTS]
# ✓ SMKG207_cont           - calculate_age_started_daily_former() [EXISTS]
# ✓ SMKG01C_cont           - calculate_age_first_cigarette() [NEEDS IMPLEMENTATION]
# ○ Daily pack-years       - calculate_pack_years_daily() [NEEDS IMPLEMENTATION]
# ○ Occasional pack-years  - calculate_pack_years_occasional_*() [NEEDS IMPLEMENTATION]
# ○ Former pack-years      - calculate_pack_years_former_*() [NEEDS IMPLEMENTATION]

# SMOKING HISTORY GENERATOR MODELS (SMKDSTY Functions):
#   • calculate_smoking_status_detailed() - SMKDSTY_A (6 categories: daily, occasional-former daily, occasional-never daily, former daily, former occasional, never)
#   • calculate_smoking_status_simplified() - SMKDSTY_B (6 categories: daily, occasional-former daily, occasional-never daily, former daily, former occasional, never)
#   • calculate_smoking_status_basic() - SMKDSTY_cat3 (3 categories: current, former, never)
#   • calculate_smoking_status_intermediate() - SMKDSTY_cat5 (5 categories: daily, occasional-former daily, occasional-never daily, former occasional, never)

# AGE-RELATED CONVERSION FUNCTIONS:
#   calculate_age_started_daily_current() → age_started_current_core() → SMKG203_cont (current daily)
#   calculate_age_started_daily_former() → age_started_former_core() → SMKG207_cont (former daily)
#   calculate_age_started_categorical_current() → age_started_categorical_current_core() → SMKG203_A (current daily, categorical)
#   calculate_age_started_categorical_former() → age_started_categorical_former_core() → SMKG207_A (former daily, categorical)

# ==============================================================================
# 3. INTERNAL HELPER FUNCTIONS
# ==============================================================================

#' Core smoking status assessment (internal helper)
#'
#' Smoking status assessment without validation - used as building block
#' @param smk_005_clean,smk_030_clean,smk_01a_clean Cleaned smoking variables (already validated)
#' @return Smoking status indicator with proper tagged NA handling
#' @note v3.0.0, last updated: 2025-07-09, status: active, Note: Enhanced with comprehensive v3.0.0 modernization
#' @noRd
calculate_smoking_status_core <- function(smk_005_clean, smk_030_clean, smk_01a_clean) {
  # Use case_when for element-wise processing with tagged NA handling
  dplyr::case_when(
    # Use standardized tagged NA conditions from all inputs
    !!!assign_tagged_na(smk_005_clean),
    !!!assign_tagged_na(smk_030_clean),
    !!!assign_tagged_na(smk_01a_clean),

    # Smoking status classification logic
    smk_005_clean == 1 ~ 1L, # Daily smoker
    smk_005_clean == 2 & smk_030_clean == 1 ~ 2L, # Occasional smoker (former daily)
    smk_005_clean == 2 & (smk_030_clean == 2 | haven::is_tagged_na(smk_030_clean, "a") | haven::is_tagged_na(smk_030_clean, "b")) ~ 3L, # Occasional smoker (never daily)
    smk_005_clean == 3 & smk_030_clean == 1 ~ 4L, # Former daily
    smk_005_clean == 3 & smk_030_clean == 2 & smk_01a_clean == 1 ~ 5L, # Former occasional
    smk_005_clean == 3 & smk_01a_clean == 2 ~ 6L, # Never smoked

    # Default to missing for any unhandled cases
    .default = haven::tagged_na("b")
  )
}

#' Core age started smoking daily logic for current daily smokers
#'
#' @description Internal core function for current daily smoker age conversion
#' @param smk_005_clean SMK_005 values (preprocessed)
#' @param smkg040_clean SMKG040 values (preprocessed)
#' @return Numeric continuous age or tagged_na
#' @keywords internal
age_started_current_core <- function(smk_005_clean, smkg040_clean) {
  
  # Age category midpoints (methodological constants)
  # TODO: Move to variable_details.csv in future iteration
  midpoints <- c(
    "1" = 8, "2" = 13, "3" = 16, "4" = 18.5, "5" = 22, "6" = 27,
    "7" = 32, "8" = 37, "9" = 42, "10" = 47, "11" = 55
  )
  
  dplyr::case_when(
    # Use standardized tagged NA conditions
    !!!assign_tagged_na(smk_005_clean),
    !!!assign_tagged_na(smkg040_clean),
    
    # Only current daily smokers get age values
    smk_005_clean != 1 ~ haven::tagged_na("a"),
    
    # Convert categories to continuous age using metadata midpoints
    smk_005_clean == 1 & smkg040_clean == 1 ~ midpoints[["1"]],
    smk_005_clean == 1 & smkg040_clean == 2 ~ midpoints[["2"]],
    smk_005_clean == 1 & smkg040_clean == 3 ~ midpoints[["3"]],
    smk_005_clean == 1 & smkg040_clean == 4 ~ midpoints[["4"]],
    smk_005_clean == 1 & smkg040_clean == 5 ~ midpoints[["5"]],
    smk_005_clean == 1 & smkg040_clean == 6 ~ midpoints[["6"]],
    smk_005_clean == 1 & smkg040_clean == 7 ~ midpoints[["7"]],
    smk_005_clean == 1 & smkg040_clean == 8 ~ midpoints[["8"]],
    smk_005_clean == 1 & smkg040_clean == 9 ~ midpoints[["9"]],
    smk_005_clean == 1 & smkg040_clean == 10 ~ midpoints[["10"]],
    smk_005_clean == 1 & smkg040_clean == 11 ~ midpoints[["11"]],
    
    # Default to missing for any unhandled cases
    .default = haven::tagged_na("b")
  )
}

#' Core age started smoking daily logic for former daily smokers
#'
#' @description Internal core function for former daily smoker age conversion
#' @param smk_030_clean SMK_030 values (preprocessed)
#' @param smkg040_clean SMKG040 values (preprocessed)
#' @return Numeric continuous age or tagged_na
#' @keywords internal
age_started_former_core <- function(smk_030_clean, smkg040_clean) {
  
  # Age category midpoints (same as current daily smokers)
  # TODO: Move to variable_details.csv in future iteration
  midpoints <- c(
    "1" = 8, "2" = 13, "3" = 16, "4" = 18.5, "5" = 22, "6" = 27,
    "7" = 32, "8" = 37, "9" = 42, "10" = 47, "11" = 55
  )
  
  dplyr::case_when(
    # Use standardized tagged NA conditions
    !!!assign_tagged_na(smk_030_clean),
    !!!assign_tagged_na(smkg040_clean),
    
    # Only former daily smokers get age values
    smk_030_clean != 1 ~ haven::tagged_na("a"),
    
    # Convert categories to continuous age using metadata midpoints
    smk_030_clean == 1 & smkg040_clean == 1 ~ midpoints[["1"]],
    smk_030_clean == 1 & smkg040_clean == 2 ~ midpoints[["2"]],
    smk_030_clean == 1 & smkg040_clean == 3 ~ midpoints[["3"]],
    smk_030_clean == 1 & smkg040_clean == 4 ~ midpoints[["4"]],
    smk_030_clean == 1 & smkg040_clean == 5 ~ midpoints[["5"]],
    smk_030_clean == 1 & smkg040_clean == 6 ~ midpoints[["6"]],
    smk_030_clean == 1 & smkg040_clean == 7 ~ midpoints[["7"]],
    smk_030_clean == 1 & smkg040_clean == 8 ~ midpoints[["8"]],
    smk_030_clean == 1 & smkg040_clean == 9 ~ midpoints[["9"]],
    smk_030_clean == 1 & smkg040_clean == 10 ~ midpoints[["10"]],
    smk_030_clean == 1 & smkg040_clean == 11 ~ midpoints[["11"]],
    
    # Default to missing for any unhandled cases
    .default = haven::tagged_na("b")
  )
}

#' Core time since quit smoking logic
#'
#' @description Internal core function for time since quit calculation
#' @param smk_09a_b_clean SMK_09A_B values (preprocessed)
#' @param smkg09c_clean SMKG09C values (preprocessed)
#' @return Numeric time in years or tagged_na
#' @keywords internal
calculate_time_quit_core <- function(smk_09a_b_clean, smkg09c_clean) {
  
  # Time period mappings (methodological constants)
  # TODO: Move to variable_details.csv in future iteration
  time_mappings <- c("1" = 0.5, "2" = 1.5, "3" = 2.5, "4" = 3.5)
  
  # Main time calculation - SMKG09C (continuous) takes precedence when available
  dplyr::case_when(
    # Use standardized tagged NA conditions for SMKG09C first
    !!!assign_tagged_na(smkg09c_clean),
    
    # Use SMKG09C directly as continuous years when available (takes precedence)
    !is.na(smkg09c_clean) ~ as.numeric(smkg09c_clean),
    
    # Fall back to SMK_09A_B categorical mapping using metadata
    !!!assign_tagged_na(smk_09a_b_clean),
    smk_09a_b_clean == 1 ~ time_mappings[["1"]],   # <1 year
    smk_09a_b_clean == 2 ~ time_mappings[["2"]],   # 1-2 years  
    smk_09a_b_clean == 3 ~ time_mappings[["3"]],   # 2-3 years
    smk_09a_b_clean == 4 ~ time_mappings[["4"]],   # 3+ years
    
    # Default to missing for any unhandled cases
    .default = haven::tagged_na("b")
  )
}

#' Core pack-years calculation logic
#'
#' @description Internal core function for pack-years calculation
#' @param smoking_status_clean Smoking status (preprocessed)
#' @param current_age_clean Current age (preprocessed)
#' @param time_quit_clean Time since quit (preprocessed)
#' @param age_started_clean Age started smoking (preprocessed)
#' @param cigarettes_daily_clean Cigarettes per day (preprocessed)
#' @return Numeric pack-years or tagged_na
#' @keywords internal
calculate_pack_years_core <- function(smoking_status_clean, current_age_clean, time_quit_clean, age_started_clean, cigarettes_daily_clean) {
  # This is a simplified version - the full implementation would need multiple smoking variables
  # For now, return a placeholder that shows the structure
  dplyr::case_when(
    # Handle never smokers FIRST (smoking_status = 6) - they become tagged_na("a") after preprocessing
    # but should return 0 pack-years by definition, not missing data
    haven::is_tagged_na(smoking_status_clean, "a") ~ 0,
    
    # Use standardized tagged NA conditions for all other variables  
    !!!assign_tagged_na(smoking_status_clean),
    !!!assign_tagged_na(current_age_clean),
    !!!assign_tagged_na(time_quit_clean),
    !!!assign_tagged_na(age_started_clean),
    !!!assign_tagged_na(cigarettes_daily_clean),
    
    # Basic pack-years calculation with non-negative constraint
    smoking_status_clean == 1 ~ pmax(0, (current_age_clean - age_started_clean) * (cigarettes_daily_clean / 20)),
    
    # Default for other cases would need more complex logic
    .default = haven::tagged_na("b")
  )
}

#' Core SMKG040 combination logic
#'
#' @description Internal core function for combining SMKG203 and SMKG207
#' @param smkg203_clean SMKG203_cont values (preprocessed)
#' @param smkg207_clean SMKG207_cont values (preprocessed)
#' @return Numeric age or tagged_na
#' @keywords internal
smkg040_core <- function(smkg203_clean, smkg207_clean) {
  
  dplyr::case_when(
    # Use standardized tagged NA conditions
    !!!assign_tagged_na(smkg203_clean),
    !!!assign_tagged_na(smkg207_clean),
    
    # Use available value (daily smoker or former daily smoker)
    !is.na(smkg203_clean) ~ smkg203_clean,
    !is.na(smkg207_clean) ~ smkg207_clean,
    
    # Default to missing if both are missing
    .default = haven::tagged_na("b")
  )
}

#' Core pack-years categorization logic
#'
#' @description Internal core function for pack-years categorization
#' @param pack_years_clean Pack-years continuous values (preprocessed)
#' @return Integer category (1-8) or tagged_na
#' @keywords internal
pack_years_cat_core <- function(pack_years_clean) {
  
  # Pack-years cutoffs (CVD Risk Tool, Manuel et al. 2018)
  # TODO: Move to variable_details.csv in future iteration
  cutoffs <- list(
    "1" = list(min = 0, max = 0, exclusive_min = FALSE, exclusive_max = FALSE),
    "2" = list(min = 0, max = 0.01, exclusive_min = TRUE, exclusive_max = FALSE),
    "3" = list(min = 0.01, max = 3.0, exclusive_min = TRUE, exclusive_max = FALSE),
    "4" = list(min = 3.0, max = 9.0, exclusive_min = TRUE, exclusive_max = FALSE),
    "5" = list(min = 9.0, max = 16.2, exclusive_min = TRUE, exclusive_max = FALSE),
    "6" = list(min = 16.2, max = 25.7, exclusive_min = TRUE, exclusive_max = FALSE),
    "7" = list(min = 25.7, max = 40.0, exclusive_min = TRUE, exclusive_max = FALSE),
    "8" = list(min = 40.0, max = Inf, exclusive_min = TRUE, exclusive_max = FALSE)
  )
  
  # Extract cutoff values for case_when (maintaining original logic structure)
  c1 <- cutoffs[["1"]]  # 0
  c2 <- cutoffs[["2"]]  # 0 to 0.01
  c3 <- cutoffs[["3"]]  # 0.01 to 3.0
  c4 <- cutoffs[["4"]]  # 3.0 to 9.0
  c5 <- cutoffs[["5"]]  # 9.0 to 16.2
  c6 <- cutoffs[["6"]]  # 16.2 to 25.7
  c7 <- cutoffs[["7"]]  # 25.7 to 40.0
  c8 <- cutoffs[["8"]]  # 40.0+
  
  dplyr::case_when(
    # Use standardized tagged NA conditions
    !!!assign_tagged_na(pack_years_clean),
    
    # Pack-years categories from metadata (CVD Risk Tool, Manuel et al. 2018)
    pack_years_clean == c1$min ~ 1L,
    pack_years_clean > c2$min & pack_years_clean <= c2$max ~ 2L,
    pack_years_clean > c3$min & pack_years_clean <= c3$max ~ 3L,
    pack_years_clean > c4$min & pack_years_clean <= c4$max ~ 4L,
    pack_years_clean > c5$min & pack_years_clean <= c5$max ~ 5L,
    pack_years_clean > c6$min & pack_years_clean <= c6$max ~ 6L,
    pack_years_clean > c7$min & pack_years_clean <= c7$max ~ 7L,
    pack_years_clean > c8$min ~ 8L,
    
    # Default to missing for any unhandled cases
    .default = haven::tagged_na("b")
  )
}

#' Core simple smoking status logic
#'
#' @description Internal core function for simple smoking status classification
#' @param smkdsty_cat5_clean 5-category smoking status (preprocessed)
#' @param time_quit_smoking_clean Time since quit (preprocessed)
#' @return Integer simple status (0-3) or tagged_na
#' @keywords internal
smoke_simple_core <- function(smkdsty_cat5_clean, time_quit_smoking_clean) {
  
  dplyr::case_when(
    # Simple smoking status classification logic (process valid values first)
    smkdsty_cat5_clean == 5 ~ 0L, # Never smoked
    smkdsty_cat5_clean %in% c(1, 2) ~ 1L, # Current smoker (daily and occasional)
    smkdsty_cat5_clean == 4 ~ 2L, # Former occasional smoker
    smkdsty_cat5_clean == 3 & !is.na(time_quit_smoking_clean) & !haven::is_tagged_na(time_quit_smoking_clean) & time_quit_smoking_clean < 5 ~ 2L, # Former daily smoker quit <5 years
    smkdsty_cat5_clean == 3 & !is.na(time_quit_smoking_clean) & !haven::is_tagged_na(time_quit_smoking_clean) & time_quit_smoking_clean >= 5 ~ 3L, # Former daily smoker quit >=5 years
    smkdsty_cat5_clean == 3 & (is.na(time_quit_smoking_clean) | haven::is_tagged_na(time_quit_smoking_clean)) ~ 2L, # Former daily smoker, time unknown - default to category 2

    # Use standardized tagged NA conditions for both inputs
    !!!assign_tagged_na(smkdsty_cat5_clean),
    !!!assign_tagged_na(time_quit_smoking_clean),

    # Default to missing for any unhandled cases
    .default = haven::tagged_na("b")
  )
}

# ==============================================================================
# 4. MAIN USER-FACING FUNCTIONS
# ==============================================================================

#' Calculate smoking status (6-category SMKDSTY classification)
#'
#' @description
#' Calculate detailed smoking status using CCHS smoking variables. Creates 6-category
#' smoking status: daily smoker, occasional smoker (former daily), occasional smoker
#' (never daily), former daily smoker, former occasional smoker, never smoked.
#'
#' @param SMK_005 Type of smoker presently (1=daily, 2=occasional, 3=former/never). Accepts raw CCHS codes.
#' @param SMK_030 Ever smoked daily in lifetime (1=yes, 2=no). Accepts raw CCHS codes.
#' @param SMK_01A Smoked 100+ cigarettes in lifetime (1=yes, 2=no). Accepts raw CCHS codes.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Integer smoking status classification. Categories:
#'   \itemize{
#'     \item \code{1} Daily smoker
#'     \item \code{2} Occasional smoker (former daily)
#'     \item \code{3} Occasional smoker (never daily)
#'     \item \code{4} Former daily smoker
#'     \item \code{5} Former occasional smoker
#'     \item \code{6} Never smoked
#'     \item \code{haven::tagged_na("a")} Not applicable
#'     \item \code{haven::tagged_na("b")} Missing/invalid response
#'   }
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' 
#' result <- rec_with_table(
#'   data = your_data,
#'   variables = "SMKDSTY_A", 
#'   database_name = "cchs2017_2018_p",
#'   variable_details = variable_details
#' )
#' 
#' # Direct function usage (advanced users)
#' smoking_status <- calculate_smoking_status(
#'   SMK_005 = c(1, 2, 3, 1, 3), # Daily, occasional, former, daily, former
#'   SMK_030 = c(1, 1, 1, 1, 2), # All ever daily except last
#'   SMK_01A = c(1, 1, 1, 1, 2)  # All 100+ cigarettes except last
#' )
#' # Result: c(1, 2, 4, 1, 6) - daily, occasional-former daily, former daily, daily, never
#'
#' @note v3.0.0, last updated: 2025-07-10, status: active - Enhanced SMKDSTY classification
#' @export
calculate_smoking_status <- function(SMK_005, SMK_030, SMK_01A, log_level = "silent") {
  
  # Convert CCHS missing codes to tagged_na for all inputs
  smk_005_clean <- preprocess_cchs_missing_codes(SMK_005, "single_digit_missing")
  smk_030_clean <- preprocess_cchs_missing_codes(SMK_030, "single_digit_missing")
  smk_01a_clean <- preprocess_cchs_missing_codes(SMK_01A, "single_digit_missing")
  
  # Calculate smoking status using core function
  calculate_smoking_status_core(smk_005_clean, smk_030_clean, smk_01a_clean)
}

#' Calculate time since quit smoking
#'
#' @description
#' Calculate time since quit smoking for former daily smokers using CCHS variables.
#' Combines categorical recent quit times with continuous longer-term quit times.
#'
#' @param SMK_09A_B Time since quit categories for recent quitters (1-4). Accepts raw CCHS codes.
#' @param SMKG09C Time since quit categories for long-term quitters (1-3). Accepts raw CCHS codes.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric time in years since quitting. Values:
#'   \itemize{
#'     \item \code{0.5} Less than 1 year
#'     \item \code{1.5} 1-2 years
#'     \item \code{2.5} 2-3 years
#'     \item \code{4, 8, 12} 3+ years (converted from SMKG09C categories)
#'     \item \code{haven::tagged_na("a")} Not applicable
#'     \item \code{haven::tagged_na("b")} Missing/invalid response
#'   }
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' 
#' result <- rec_with_table(
#'   data = your_data,
#'   variables = "time_quit_smoking", 
#'   database_name = "cchs2011_2012_p",
#'   variable_details = variable_details
#' )
#'
#' @note v3.0.0, last updated: 2025-07-10, status: active - Time calculation support
#' @export
calculate_time_quit_smoking <- function(SMK_09A_B, SMKG09C, log_level = "silent") {
  
  # Convert CCHS missing codes to tagged_na for all inputs
  smk_09a_b_clean <- preprocess_cchs_missing_codes(SMK_09A_B, "single_digit_missing")
  smkg09c_clean <- preprocess_cchs_missing_codes(SMKG09C, "single_digit_missing")
  
  # Calculate time since quit using core function
  calculate_time_quit_core(smk_09a_b_clean, smkg09c_clean)
}

#' Calculate pack-years (simplified version)
#'
#' @description
#' Calculate pack-years of smoking exposure. This is a simplified version for demonstration.
#' The full implementation would require multiple smoking variables for comprehensive calculation.
#'
#' @param smoking_status Smoking status (from calculate_smoking_status). Accepts raw or processed values.
#' @param current_age Current age in years. Accepts raw CCHS codes.
#' @param time_quit Time since quit smoking (from calculate_time_quit_smoking). Optional.
#' @param age_started Age started smoking daily. Optional.
#' @param cigarettes_daily Number of cigarettes smoked per day. Optional.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric pack-years value or tagged_na for missing/invalid data
#'
#' @note v3.0.0, last updated: 2025-07-10, status: active - Simplified pack-years calculation
#' @export
calculate_pack_years <- function(smoking_status, current_age, time_quit = NA, age_started = NA, cigarettes_daily = NA, log_level = "silent") {
  
  # Clean CCHS missing codes only - comprehensive validation handled by rec_with_table()
  cleaned <- clean_variables(
    categorical_vars = list(smoking_status = smoking_status),
    continuous_vars = list(current_age = current_age, time_quit = time_quit, age_started = age_started, cigarettes_daily = cigarettes_daily),
    categorical_pattern = "single_digit_missing",
    continuous_pattern = "triple_digit_missing",
    log_level = log_level
  )
  
  # Calculate pack-years using core function
  calculate_pack_years_core(cleaned$smoking_status_clean, cleaned$current_age_clean, cleaned$time_quit_clean, cleaned$age_started_clean, cleaned$cigarettes_daily_clean)
}

#' Convert age started smoking daily for current daily smokers (categorical to continuous)
#'
#' @description
#' Convert categorical age started smoking daily to continuous age for current daily smokers.
#' Filters for current daily smokers (SMK_005 = 1) and converts SMKG040 categories to 
#' continuous age values using midpoint mappings for harmonization across CCHS cycles.
#'
#' @param SMK_005 Type of smoker presently (1=daily, 2=occasional, 3=former/never). Accepts raw CCHS codes.
#' @param SMKG040 Age started smoking daily categories (1-11 representing age ranges). Accepts raw CCHS codes.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric continuous age for current daily smokers. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (non-daily smokers)
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Age values: 8, 13, 16, 18.5, 22, 27, 32, 37, 42, 47, 55 (category midpoints)
#'
#' @details
#' **Conversion Logic:**
#' - **Filter**: Only current daily smokers (SMK_005 = 1) get age values
#' - **Mapping**: SMKG040 categories converted to continuous age using midpoints:
#'   - Category 1 → 8 years (5-11 years)
#'   - Category 2 → 13 years (12-14 years)  
#'   - Category 3 → 16 years (15-17 years)
#'   - Category 4 → 18.5 years (18-19 years)
#'   - Category 5 → 22 years (20-24 years)
#'   - Category 6 → 27 years (25-29 years)
#'   - Category 7 → 32 years (30-34 years)
#'   - Category 8 → 37 years (35-39 years)
#'   - Category 9 → 42 years (40-44 years)
#'   - Category 10 → 47 years (45-49 years)
#'   - Category 11 → 55 years (50+ years)
#'
#' **Cycle coverage:** 2015-2018 (specific cycles requiring this conversion)
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' 
#' result <- rec_with_table(
#'   data = your_data,
#'   variables = "SMKG203_cont", 
#'   database_name = "cchs2015_2016_p",
#'   variable_details = variable_details
#' )
#' 
#' # Direct function usage (advanced users)
#' smkg203_cont <- calculate_age_started_daily_current(
#'   SMK_005 = c(1, 2, 1, 3, 1), # Daily, occasional, daily, former, daily
#'   SMKG040 = c(3, 5, 7, 4, 11) # Age categories
#' )
#' # Result: c(16, tagged_na("a"), 32, tagged_na("a"), 55)
#'
#' @note v3.0.0, last updated: 2025-07-10, status: active - Age conversion support
#' @export
calculate_age_started_daily_current <- function(SMK_005, SMKG040, log_level = "silent") {
  
  # Clean CCHS missing codes only - comprehensive validation handled by rec_with_table()
  cleaned <- clean_variables(
    categorical_vars = list(smk_005 = SMK_005, smkg040 = SMKG040),
    categorical_pattern = "single_digit_missing",
    log_level = log_level
  )
  
  # Calculate age started for current daily smokers
  age_started_current_core(cleaned$smk_005_clean, cleaned$smkg040_clean)
}

#' Convert age started smoking daily for former daily smokers (categorical to continuous)
#'
#' @description
#' Convert categorical age started smoking daily to continuous age for former daily smokers.
#' Filters for former daily smokers (SMK_030 = 1) and converts SMKG040 categories to 
#' continuous age values using midpoint mappings for harmonization across CCHS cycles.
#'
#' @param SMK_030 Ever smoked daily in lifetime (1=yes, 2=no). Accepts raw CCHS codes.
#' @param SMKG040 Age started smoking daily categories (1-11 representing age ranges). Accepts raw CCHS codes.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric continuous age for former daily smokers. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (never daily smokers)
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Age values: 8, 13, 16, 18.5, 22, 27, 32, 37, 42, 47, 55 (category midpoints)
#'
#' @details
#' **Conversion Logic:**
#' - **Filter**: Only former daily smokers (SMK_030 = 1) get age values
#' - **Mapping**: SMKG040 categories converted to continuous age using midpoints:
#'   - Category 1 → 8 years (5-11 years)
#'   - Category 2 → 13 years (12-14 years)  
#'   - Category 3 → 16 years (15-17 years)
#'   - Category 4 → 18.5 years (18-19 years)
#'   - Category 5 → 22 years (20-24 years)
#'   - Category 6 → 27 years (25-29 years)
#'   - Category 7 → 32 years (30-34 years)
#'   - Category 8 → 37 years (35-39 years)
#'   - Category 9 → 42 years (40-44 years)
#'   - Category 10 → 47 years (45-49 years)
#'   - Category 11 → 55 years (50+ years)
#'
#' **Cycle coverage:** 2015-2018 (specific cycles requiring this conversion)
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' 
#' result <- rec_with_table(
#'   data = your_data,
#'   variables = "SMKG207_cont", 
#'   database_name = "cchs2015_2016_p",
#'   variable_details = variable_details
#' )
#' 
#' # Direct function usage (advanced users)
#' smkg207_cont <- calculate_age_started_daily_former(
#'   SMK_030 = c(1, 2, 1, 1, 2), # Ever daily, never daily, ever daily, ever daily, never daily
#'   SMKG040 = c(3, 5, 7, 4, 11) # Age categories
#' )
#' # Result: c(16, tagged_na("a"), 32, 18.5, tagged_na("a"))
#'
#' @note v3.0.0, last updated: 2025-07-10, status: active - Age conversion support
#' @export
calculate_age_started_daily_former <- function(SMK_030, SMKG040, log_level = "silent") {
  
  # Clean CCHS missing codes only - comprehensive validation handled by rec_with_table()
  cleaned <- clean_variables(
    categorical_vars = list(smk_030 = SMK_030, smkg040 = SMKG040),
    categorical_pattern = "single_digit_missing",
    log_level = log_level
  )
  
  # Calculate age started for former daily smokers
  age_started_former_core(cleaned$smk_030_clean, cleaned$smkg040_clean)
}

#' Calculate simple smoking status (4-category classification)
#'
#' @description
#' Calculate simple smoking status with 4 categories: never smoker (0), current smoker (1),
#' former daily smoker quit ≤5 years or former occasional smoker (2), 
#' former daily smoker quit >5 years (3).
#'
#' @param SMKDSTY_cat5 5-category smoking status (from calculate_smoking_status). Accepts raw or processed values.
#' @param time_quit_smoking Time since quit smoking (from calculate_time_quit_smoking). Accepts raw or processed values.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Integer simple smoking status classification. Categories:
#'   \itemize{
#'     \item \code{0} Never smoker
#'     \item \code{1} Current smoker (daily and occasional)
#'     \item \code{2} Former daily smoker quit ≤5 years or former occasional smoker
#'     \item \code{3} Former daily smoker quit >5 years
#'     \item \code{haven::tagged_na("a")} Not applicable
#'     \item \code{haven::tagged_na("b")} Missing/invalid response
#'   }
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' 
#' result <- rec_with_table(
#'   data = your_data,
#'   variables = "smoke_simple", 
#'   database_name = "cchs2017_2018_p",
#'   variable_details = variable_details
#' )
#'
#' @note v3.0.0, last updated: 2025-07-10, status: active - Simple smoking classification
#' @export
calculate_smoke_simple <- function(SMKDSTY_cat5, time_quit_smoking, log_level = "silent") {
  
  # Convert CCHS missing codes to tagged_na for all inputs
  smkdsty_cat5_clean <- preprocess_cchs_missing_codes(SMKDSTY_cat5, "single_digit_missing")
  time_quit_smoking_clean <- preprocess_cchs_missing_codes(time_quit_smoking, "triple_digit_missing")
  
  # Calculate simple smoking status using core function
  smoke_simple_core(smkdsty_cat5_clean, time_quit_smoking_clean)
}

#' Calculate pack-years categories (8-category classification)
#'
#' @description
#' Convert continuous pack-years values to 8 standardized categories based on
#' the Cardiovascular Disease Population Risk Tool (Manuel et al. 2018).
#'
#' @param pack_years_der Continuous pack-years values (from calculate_pack_years). Accepts raw or processed values.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Integer pack-years category (1-8). Categories:
#'   \itemize{
#'     \item \code{1} 0 pack-years (never smoker)
#'     \item \code{2} >0 to 0.01 pack-years (minimal exposure)
#'     \item \code{3} >0.01 to 3.0 pack-years (light exposure)
#'     \item \code{4} >3.0 to 9.0 pack-years (moderate exposure)
#'     \item \code{5} >9.0 to 16.2 pack-years (high exposure)
#'     \item \code{6} >16.2 to 25.7 pack-years (very high exposure)
#'     \item \code{7} >25.7 to 40.0 pack-years (extreme exposure)
#'     \item \code{8} >40.0 pack-years (maximum exposure)
#'     \item \code{haven::tagged_na("a")} Not applicable
#'     \item \code{haven::tagged_na("b")} Missing/invalid response
#'   }
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' 
#' result <- rec_with_table(
#'   data = your_data,
#'   variables = "pack_years_cat", 
#'   database_name = "cchs2017_2018_p",
#'   variable_details = variable_details
#' )
#'
#' @note v3.0.0, last updated: 2025-07-10, status: active - Pack-years categorization
#' @export
calculate_pack_years_categorical <- function(pack_years_der, log_level = "silent") {
  
  # Convert CCHS missing codes to tagged_na for all inputs
  pack_years_clean <- preprocess_cchs_missing_codes(pack_years_der, "triple_digit_missing")
  
  # Calculate pack-years categories using core function
  pack_years_cat_core(pack_years_clean)
}

#' Calculate combined age started smoking daily (SMKG040 harmonization)
#'
#' @description
#' Combine age started smoking daily for daily and former daily smokers.
#' This function harmonizes SMKG203_cont (current daily) and SMKG207_cont (former daily)
#' to create a unified SMKG040_cont variable across all CCHS cycles.
#'
#' @param SMKG203_cont Age started smoking daily for current daily smokers. Accepts raw or processed values.
#' @param SMKG207_cont Age started smoking daily for former daily smokers. Accepts raw or processed values.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric age in years when started smoking daily. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} when both inputs are tagged_na("a")
#'     \item \code{haven::tagged_na("b")} when both inputs are missing/invalid
#'   }
#'   Returns the available value when one input is valid and the other is missing.
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' 
#' result <- rec_with_table(
#'   data = your_data,
#'   variables = "SMKG040_cont", 
#'   database_name = "cchs2009_2010_p",
#'   variable_details = variable_details
#' )
#'
#' @note v3.0.0, last updated: 2025-07-10, status: active - Age harmonization across cycles
#' @export
calculate_SMKG040 <- function(SMKG203_cont, SMKG207_cont, log_level = "silent") {
  
  # Convert CCHS missing codes to tagged_na for all inputs
  smkg203_clean <- preprocess_cchs_missing_codes(SMKG203_cont, "triple_digit_missing")
  smkg207_clean <- preprocess_cchs_missing_codes(SMKG207_cont, "triple_digit_missing")
  
  # Calculate combined age using core function
  smkg040_core(smkg203_clean, smkg207_clean)
}

#' Calculate detailed smoking status (6-category SMKDSTY_A classification)
#'
#' @description
#' Calculate detailed smoking status using CCHS smoking variables. Creates 6-category
#' smoking status: daily smoker, occasional smoker (former daily), occasional smoker
#' (never daily), former daily smoker, former occasional smoker, never smoked.
#' This is the SMKDSTY_A variable for 2001-2023 cycles.
#'
#' @param SMK_005 Type of smoker presently (1=daily, 2=occasional, 3=former/never). Accepts raw CCHS codes.
#' @param SMK_030 Ever smoked daily in lifetime (1=yes, 2=no). Accepts raw CCHS codes.
#' @param SMK_01A Smoked 100+ cigarettes in lifetime (1=yes, 2=no). Accepts raw CCHS codes.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Integer smoking status classification (SMKDSTY_A). Categories:
#'   \itemize{
#'     \item \code{1} Daily smoker
#'     \item \code{2} Occasional smoker (former daily)
#'     \item \code{3} Occasional smoker (never daily)
#'     \item \code{4} Former daily smoker
#'     \item \code{5} Former occasional smoker
#'     \item \code{6} Never smoked
#'     \item \code{haven::tagged_na("a")} Not applicable
#'     \item \code{haven::tagged_na("b")} Missing/invalid response
#'   }
#'
#' @note v3.0.0, last updated: 2025-07-12, status: active - Detailed SMKDSTY_A classification
#' @export
calculate_smoking_status_detailed <- function(SMK_005, SMK_030, SMK_01A, log_level = "silent") {
  
  # Convert CCHS missing codes to tagged_na for all inputs
  smk_005_clean <- preprocess_cchs_missing_codes(SMK_005, "single_digit_missing")
  smk_030_clean <- preprocess_cchs_missing_codes(SMK_030, "single_digit_missing")
  smk_01a_clean <- preprocess_cchs_missing_codes(SMK_01A, "single_digit_missing")
  
  # Calculate smoking status using core function
  calculate_smoking_status_core(smk_005_clean, smk_030_clean, smk_01a_clean)
}

# ==============================================================================
# 5. USAGE PATTERNS AND WORKFLOWS
# ==============================================================================

# Direct smoking variable analysis (harmonized):
# result <- rec_with_table(
#   data = your_data,
#   variables = c("SMKDSTY_A", "time_quit_smoking", "pack_years_der"),
#   database_name = "cchs2017_2018_p",
#   variable_details = variable_details
# )

# Comprehensive smoking analysis:
# result <- rec_with_table(
#   data = your_data, 
#   variables = c("SMK_005", "SMK_030", "SMK_01A", "SMKDSTY_A",
#                 "SMK_09A_B", "SMKG09C", "time_quit_smoking",
#                 "pack_years_der", "pack_years_cat", "smoke_simple"),
#   database_name = "cchs2011_2012_p",
#   variable_details = variable_details
# )

# Age started smoking analysis:
# result <- rec_with_table(
#   data = your_data,
#   variables = c("SMK_005", "SMK_030", "SMKG040", 
#                 "SMKG203_cont", "SMKG207_cont", "SMKG040_cont"),
#   database_name = "cchs2015_2016_p", 
#   variable_details = variable_details
# )

# SMOKING HISTORY GENERATOR MODEL ANALYSIS:
# result <- rec_with_table(
#   data = your_data,
#   variables = c("SMKDSTY_A", "SMKDSTY_B", "SMKDSTY_cat3", "SMKDSTY_cat5",
#                 "SMKG203_A", "SMKG207_A"),
#   database_name = "cchs2009_2010_p",
#   variable_details = variable_details  
# )

# Multi-cycle smoking harmonization:
# cycle1 <- rec_with_table(data1, smoking_vars, "cchs2009_2010_p", variable_details)
# cycle2 <- rec_with_table(data2, smoking_vars, "cchs2017_2018_p", variable_details)
# combined <- merge_rec_data(cycle1, cycle2)

# Variable documentation access:
# ?calculate_smoking_status
# ?calculate_time_quit_smoking
# ?calculate_pack_years

# VALIDATION RESPONSIBILITY:
# - rec_with_table() handles validation via variable_details.csv
# - Direct function calls require user validation of inputs
# - All functions accept raw CCHS codes and handle missing data appropriately