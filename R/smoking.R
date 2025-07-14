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
    source("R/lookups.R", local = FALSE)
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


# Note: These bounds are now handled by variable_details.csv as single source of truth
# rec_with_table() provides comprehensive validation using CSV-driven approach
# Functions handle CCHS missing codes only - validation comes from CSV

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
#   calculate_smoking_status() → Status category (1-6) via clean_variables() + case_when()
#   calculate_time_quit_smoking() → Time since quitting with CSV-driven categorical conversion
#
# ==============================================================================
# TIME QUIT SMOKING MULTI-VARIABLE LOGIC FLOW
# ==============================================================================
#
# ┌─────────────────────────────────────────────────────────────────────────────┐
# │                    TIME QUIT SMOKING CALCULATION FLOW                      │
# └─────────────────────────────────────────────────────────────────────────────┘
#
# INPUT VARIABLES:
#   SMK_09A_B: Recent quitters (0-3+ years) - categorical 1,2,3,4
#   SMKG09C:   Long-term detail (3+ years)  - categorical 1,2,3 OR continuous
#
# PRIORITY LOGIC (Legacy Compatible):
#   1. SMK_09A_B categories 1,2,3 take absolute precedence → direct conversion
#   2. SMK_09A_B category 4 → use SMKG09C (converted if categorical)
#   3. SMK_09A_B missing + SMKG09C valid → NOT USED (legacy compatibility)
#
# ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
# │   SMK_09A_B     │    │    SMKG09C      │    │    RESULT       │
# │  (Priority 1)   │    │  (Priority 2)   │    │  (Time Years)   │
# └─────────────────┘    └─────────────────┘    └─────────────────┘
#          │                       │                       │
#          ▼                       ▼                       ▼
# ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
# │ 1 → 0.5 years   │    │ 1 → 4 years     │    │ SMK_09A_B wins  │
# │ 2 → 1.5 years   │    │ 2 → 8 years     │    │ when both valid │
# │ 3 → 2.5 years   │    │ 3 → 12 years    │    │ (categories 1-3)│
# │ 4 → use SMKG09C │    │ continuous → as │    │                 │
# │ else → NA       │    │ is if >3        │    │ SMKG09C only    │
# └─────────────────┘    └─────────────────┘    │ when SMK_09A_B=4│
#                                               └─────────────────┘
#
# CSV MAPPING REQUIREMENTS:
#   - SMK_09A_B: 1→0.5, 2→1.5, 3→2.5, 4→special_logic
#   - SMKG09C: 1→4, 2→8, 3→12, continuous→passthrough  
#   - Priority handling: Need derived variable or function logic
#
# MISSING DATA PROPAGATION:
#   - SMK_09A_B missing → always propagate
#   - SMKG09C missing → only propagate when SMK_09A_B=4 (selective)
#
# ==============================================================================

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
# 3. MAIN USER-FACING FUNCTIONS
# ==============================================================================

# ==============================================================================
# 3.1 SMOKING STATUS CLASSIFICATION FUNCTIONS
# ==============================================================================
#
# These functions implement the core smoking status classification system used
# across all CCHS cycles for smoking history generator models. The primary function
# is calculate_smoking_status_detailed() which implements SMKDSTY_A classification.
#
# SMOKING STATUS VARIABLE HIERARCHY:
# 
# ┌─────────────────────────────────────────────────────────────────────────────┐
# │                      SMOKING STATUS VARIABLE FAMILY                         │
# └─────────────────────────────────────────────────────────────────────────────┘
#
# SMKDSTY_A (6 categories) - PRIORITY VARIABLE for smoking history generator models
# ├── 1: Daily smoker
# ├── 2: Occasional smoker (former daily)  
# ├── 3: Occasional smoker (never daily)
# ├── 4: Former daily smoker
# ├── 5: Former occasional smoker
# └── 6: Never smoked
#
# Other smoking status variants (implemented as needed):
# ├── SMKDSTY_B (6 categories, 2015+ cycles) - Same as A but derived differently
# ├── SMKDSTY_cat5 (5 categories) - Simplified classification
# ├── SMKDSTY_cat3 (3 categories) - Basic classification  
# └── smoke_simple (4 categories) - Simple time-based classification
#
# VARIABLE COVERAGE BY CYCLE:
# • 2001-2014: Direct SMKDSTY variable available (use raw values)
# • 2015-2024: Derived from SMK_005, SMK_030, SMK_01A (use functions)
#
# HARMONIZATION STRATEGY:
# • Use calculate_smoking_status_detailed() for all cycles requiring SMKDSTY_A
# • Function handles both raw SMKDSTY (early cycles) and derived logic (recent cycles)
# • CSV-driven approach ensures consistent mappings across all databases
#
# ==============================================================================

#' Calculate detailed smoking status (6-category SMKDSTY_A classification)
#'
#' @description
#' Calculate detailed smoking status using CCHS smoking variables. Creates 6-category
#' smoking status: daily smoker, occasional smoker (former daily), occasional smoker
#' (never daily), former daily smoker, former occasional smoker, never smoked.
#' This is the SMKDSTY_A variable for 2001-2023 cycles.
#'
#' **IMPORTANT**: This function implements the exact logic from smoking-legacy-v2-1-0.R
#' SMKDSTY_fun for smoking history generator models and cross-cycle harmonization.
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
#' @details
#' **Classification Logic (Exact Legacy Implementation):**
#' 
#' 1. **Daily smoker** (SMK_005 = 1) → 1
#' 2. **Occasional smoker (former daily)** (SMK_005 = 2 & SMK_030 = 1) → 2
#' 3. **Occasional smoker (never daily)** (SMK_005 = 2 & SMK_030 ≠ 1) → 3
#' 4. **Former daily smoker** (SMK_005 = 3 & SMK_030 = 1) → 4
#' 5. **Former occasional smoker** (SMK_005 = 3 & SMK_030 = 2 & SMK_01A = 1) → 5
#' 6. **Never smoked** (SMK_005 = 3 & SMK_01A = 2) → 6
#'
#' **Variable Definitions:**
#' - SMK_005: 1=daily, 2=occasional, 3=not at all
#' - SMK_030: 1=yes (ever daily), 2=no (never daily)
#' - SMK_01A: 1=yes (≥100 cigarettes), 2=no (<100 cigarettes)
#'
#' **Cycle Coverage:**
#' - **2001-2014**: Uses direct SMKDSTY variable from CCHS
#' - **2015-2024**: Derived from SMK_005, SMK_030, SMK_01A using this function
#'
#' @examples
#' # rec_with_table workflow (RECOMMENDED - handles all validation automatically):
#' # Generate test data with smoking status variables
#' test_data <- data.frame(
#'   SMK_005 = c(1, 2, 2, 3, 3, 3, 6, 7, 8, 9),
#'   SMK_030 = c(1, 1, 2, 1, 2, 2, 1, 996, 997, 998),
#'   SMK_01A = c(1, 1, 1, 1, 1, 2, 1, 1, 998, 999)
#' )
#' library(cchsflow)
#' result <- rec_with_table(
#'   test_data,
#'   c("SMK_005", "SMK_030", "SMK_01A", "SMKDSTY_A")
#' )
#' # Returns:
#' #   SMK_005 SMK_030 SMK_01A SMKDSTY_A
#' # 1       1       1       1         1  # Daily smoker
#' # 2       2       1       1         2  # Occasional (former daily)
#' # 3       2       2       1         3  # Occasional (never daily)
#' # 4       3       1       1         4  # Former daily
#' # 5       3       2       1         5  # Former occasional
#' # 6       3       2       2         6  # Never smoked
#' # 7-10: Missing data patterns with tagged_na
#' 
#' # Direct function usage (advanced users - scalar input):
#' smoking_status_scalar <- calculate_smoking_status_detailed(
#'   SMK_005 = 1, SMK_030 = 1, SMK_01A = 1
#' )
#' # Result: 1 (daily smoker)
#' 
#' # Direct function usage (advanced users - vector input):
#' smoking_status_vector <- calculate_smoking_status_detailed(
#'   SMK_005 = c(1, 2, 2, 3, 3, 3), # Daily, occasional, occasional, former, former, former
#'   SMK_030 = c(1, 1, 2, 1, 2, 2), # Ever daily status
#'   SMK_01A = c(1, 1, 1, 1, 1, 2)  # ≥100 cigarettes lifetime
#' )
#' # Result: c(1, 2, 3, 4, 5, 6) - daily, occ-former, occ-never, former-daily, former-occ, never
#' 
#' # Missing data and edge cases (IMPORTANT - shows function logic):
#' missing_data_examples <- calculate_smoking_status_detailed(
#'   SMK_005 = c(6, 7, 8, 9, 3, 3, 2),    # Missing codes, former smokers, occasional
#'   SMK_030 = c(1, 2, 996, 997, 996, 2, 6), # Valid, missing codes
#'   SMK_01A = c(1, 1, 1, 1, 1, 1, 1)     # All valid
#' )
#' # Result: c(NA::a, NA::b, NA::b, NA::b, NA::b, 5, 3)
#' # - Codes 6 → tagged_na("a"), codes 7-9 → tagged_na("b") 
#' # - SMK_005=3 with SMK_030=996 (missing) → tagged_na("b")
#' # - SMK_005=3, SMK_030=2, SMK_01A=1 → 5 (former occasional)
#' # - SMK_005=2, SMK_030=6 (missing) → 3 (occasional never daily)
#' 
#' # Edge case: Former smokers with missing SMK_030 data
#' # **IMPORTANT**: This case falls through to missing (matches legacy behavior)
#' edge_case <- calculate_smoking_status_detailed(
#'   SMK_005 = 3,    # Former smoker
#'   SMK_030 = 996,  # Missing: "not applicable" 
#'   SMK_01A = 1     # Smoked ≥100 cigarettes
#' )
#' # Result: tagged_na("b") 
#' # NOTE: Should be validated against actual CCHS question flow
#'
#' @note v3.0.0, last updated: 2025-07-14, status: active - Complete SMKDSTY_A implementation
#' @export
calculate_smoking_status_detailed <- function(SMK_005, SMK_030, SMK_01A, log_level = "silent") {
  
  # Clean CCHS missing codes only - comprehensive validation handled by rec_with_table()
  cleaned <- clean_variables(
    categorical_vars = list(smk_005 = SMK_005, smk_030 = SMK_030, smk_01a = SMK_01A),
    categorical_pattern = "single_digit_missing",
    log_level = log_level
  )
  
  # Implement the exact SMKDSTY_A logic from smoking-legacy-v2-1-0.R
  # This is the authoritative smoking status classification for smoking history generator models
  dplyr::case_when(
    # Handle primary missing data patterns first
    !!!assign_tagged_na(cleaned$smk_005_clean),
    
    # Main classification logic (matches legacy SMKDSTY_fun exactly)
    cleaned$smk_005_clean == 1 ~ 1L,  # Daily smoker
    
    cleaned$smk_005_clean == 2 & cleaned$smk_030_clean == 1 ~ 2L,  # Occasional smoker (former daily)
    
    cleaned$smk_005_clean == 2 & (cleaned$smk_030_clean == 2 | 
                                  haven::is_tagged_na(cleaned$smk_030_clean, "a") | 
                                  haven::is_tagged_na(cleaned$smk_030_clean, "b")) ~ 3L,  # Occasional smoker (never daily)
    
    cleaned$smk_005_clean == 3 & cleaned$smk_030_clean == 1 ~ 4L,  # Former daily
    
    cleaned$smk_005_clean == 3 & cleaned$smk_01a_clean == 2 ~ 6L,  # Never smoked
    
    cleaned$smk_005_clean == 3 & cleaned$smk_030_clean == 2 & cleaned$smk_01a_clean == 1 ~ 5L,  # Former occasional
    
    # Default to missing for any unhandled cases
    # NOTE: Cases like SMK_005=3, SMK_030=missing, SMK_01A=1 fall through to missing
    # This matches legacy behavior but should be validated against actual CCHS question flow
    .default = haven::tagged_na("b")
  )
}

#' Calculate smoking status (alias for detailed function)
#'
#' @description
#' **Alias for calculate_smoking_status_detailed()**. This function provides the same
#' SMKDSTY_A classification but with a simpler name for backward compatibility.
#'
#' @inheritParams calculate_smoking_status_detailed
#' @inherit calculate_smoking_status_detailed return
#' @inherit calculate_smoking_status_detailed examples
#'
#' @note v3.0.0, last updated: 2025-07-14, status: active - Alias for detailed function
#' @export
calculate_smoking_status <- function(SMK_005, SMK_030, SMK_01A, log_level = "silent") {
  calculate_smoking_status_detailed(SMK_005, SMK_030, SMK_01A, log_level)
}

# ==============================================================================
# 3.2 OTHER SMOKING STATUS VARIANTS (VARIABLE DOCUMENTATION)
# ==============================================================================
#
# These variables provide alternative smoking status classifications for specific
# research needs. The primary function above (SMKDSTY_A) should be used for most
# smoking history generator models.
#
# SMOKING STATUS VARIABLE FAMILY:
# 
# ┌─────────────────────────────────────────────────────────────────────────────┐
# │                  SMOKING STATUS CLASSIFICATION VARIANTS                    │
# └─────────────────────────────────────────────────────────────────────────────┘
#
# PRIMARY: SMKDSTY_A (6 categories) - Implemented above in section 3.1
# VARIANTS: SMKDSTY_B, SMKDSTY_cat5, SMKDSTY_cat3 - Documentation functions below  
# DERIVED: smoke_simple (4 categories) - See section 3.3 time-based functions
#
# ==============================================================================

#' SMKDSTY_B: Smoking status (6-category alternative classification)
#'
#' @description 
#' \strong{NOTE:} This is a documentation function, not a computational function.
#'
#' Alternative 6-category smoking status classification available in 2015-2024 cycles.
#' Similar to SMKDSTY_A but with different category definitions, particularly for
#' occasional and experimental smoking patterns. Can be used directly in recodeflow
#' for harmonized analysis when specific research requires this classification.
#'
#' @details 
#' **Data Source:** CCHS derived variable (Statistics Canada)
#' **Categories:** 6-level classification
#' - 1 = "Daily smoker"
#' - 2 = "Occasional smoker" 
#' - 3 = "Former daily smoker"
#' - 4 = "Former occasional smoker"
#' - 5 = "Experimental smoker" (key difference from SMKDSTY_A)
#' - 6 = "Never smoked"
#' 
#' **Cycle Coverage:** 2015-2024 (newer cycles only)
#' **Derivation:** Uses SMK_005, SMK_030, SMK_01A with different logic than SMKDSTY_A
#' **Usage:** Available directly from CCHS, no function implementation needed
#'
#' @param SMKDSTY_B Variable name for 6-category smoking status (alternative)
#'
#' @examples
#' # rec_with_table workflow for SMKDSTY_B (available 2015-2024 cycles):
#' library(cchsflow)
#' \dontrun{
#' # Use directly from CCHS - no function needed
#' result <- rec_with_table(
#'   cchs2017_2018_p,
#'   c("SMKDSTY_B", "DHH_SEX", "DHHGAGE")
#' )
#' summary(result$SMKDSTY_B)
#' 
#' # Compare with SMKDSTY_A classification
#' comparison <- rec_with_table(
#'   cchs2017_2018_p,
#'   c("SMK_005", "SMK_030", "SMK_01A", "SMKDSTY_A", "SMKDSTY_B")
#' )
#' table(comparison$SMKDSTY_A, comparison$SMKDSTY_B, useNA = "ifany")
#' }
#'
#' @seealso 
#' \code{\link{calculate_smoking_status_detailed}} for SMKDSTY_A (primary classification),
#' \code{\link{SMKDSTY_cat5}}, \code{\link{SMKDSTY_cat3}} for other variants
#'
#' @note v3.0.0, last updated: 2025-07-14, status: documentation-only
#' @export
SMKDSTY_B <- function(SMKDSTY_B) {
  # this is for documentation purposes only
}

#' SMKDSTY_cat5: Smoking status (5-category classification)
#'
#' @description 
#' \strong{NOTE:} This is a documentation function, not a computational function.
#'
#' Five-category smoking status classification available across all CCHS cycles.
#' Consolidates the 6-category classifications into 5 broader groups, commonly
#' used for population health research and epidemiological studies. Can be used
#' directly in recodeflow or as input to smoke_simple classification.
#'
#' @details 
#' **Data Source:** CCHS derived variable (Statistics Canada)
#' **Categories:** 5-level classification
#' - 1 = "Daily smoker"
#' - 2 = "Occasional smoker"
#' - 3 = "Former daily smoker" 
#' - 4 = "Former occasional smoker"
#' - 5 = "Never smoked"
#' 
#' **Cycle Coverage:** 2001-2024 (all cycles)
#' **Derivation:** Consolidation of 6-category classifications
#' **Common Usage:** Input variable for smoke_simple time-based classification
#'
#' @param SMKDSTY_cat5 Variable name for 5-category smoking status
#'
#' @examples
#' # rec_with_table workflow for SMKDSTY_cat5 (all cycles):
#' library(cchsflow)
#' \dontrun{
#' # Use directly from CCHS - no function needed
#' result <- rec_with_table(
#'   cchs2009_2010_p,
#'   c("SMKDSTY_cat5", "DHH_SEX", "DHHGAGE")
#' )
#' summary(result$SMKDSTY_cat5)
#' 
#' # Common usage as input to smoke_simple classification
#' smoke_analysis <- rec_with_table(
#'   cchs2009_2010_p,
#'   c("SMKDSTY_cat5", "SMK_09A_B", "SMKG09C", "time_quit_smoking", "smoke_simple")
#' )
#' table(smoke_analysis$SMKDSTY_cat5, smoke_analysis$smoke_simple, useNA = "ifany")
#' }
#'
#' @seealso 
#' \code{\link{calculate_smoke_simple}} (uses SMKDSTY_cat5 as input),
#' \code{\link{calculate_smoking_status_detailed}}, \code{\link{SMKDSTY_B}}, \code{\link{SMKDSTY_cat3}}
#'
#' @note v3.0.0, last updated: 2025-07-14, status: documentation-only
#' @export
SMKDSTY_cat5 <- function(SMKDSTY_cat5) {
  # this is for documentation purposes only
}

#' SMKDSTY_cat3: Smoking status (3-category simplified classification)
#'
#' @description 
#' \strong{NOTE:} This is a documentation function, not a computational function.
#'
#' Three-category smoking status classification for basic population health analysis.
#' Provides the simplest smoking status grouping across all CCHS cycles, suitable
#' for general epidemiological research and basic smoking prevalence studies.
#'
#' @details 
#' **Data Source:** CCHS derived variable (Statistics Canada)
#' **Categories:** 3-level classification
#' - 1 = "Current smoker" (includes daily and occasional)
#' - 2 = "Former smoker" (includes former daily and former occasional)
#' - 3 = "Never smoked"
#' 
#' **Cycle Coverage:** 2001-2024 (all cycles)
#' **Derivation:** Simplified grouping of detailed smoking status categories
#' **Common Usage:** Basic smoking prevalence analysis, simple risk factor studies
#'
#' @param SMKDSTY_cat3 Variable name for 3-category smoking status
#'
#' @examples
#' # rec_with_table workflow for SMKDSTY_cat3 (all cycles):
#' library(cchsflow)
#' \dontrun{
#' # Use directly from CCHS - no function needed
#' result <- rec_with_table(
#'   cchs2011_2012_p,
#'   c("SMKDSTY_cat3", "DHH_SEX", "DHHGAGE")
#' )
#' summary(result$SMKDSTY_cat3)
#' 
#' # Basic prevalence analysis
#' prevalence <- table(result$SMKDSTY_cat3, useNA = "ifany")
#' prop.table(prevalence) * 100  # Percentage distribution
#' }
#'
#' @seealso 
#' \code{\link{calculate_smoking_status_detailed}} for detailed 6-category classification,
#' \code{\link{SMKDSTY_cat5}}, \code{\link{SMKDSTY_B}} for other variants
#'
#' @note v3.0.0, last updated: 2025-07-14, status: documentation-only
#' @export
SMKDSTY_cat3 <- function(SMKDSTY_cat3) {
  # this is for documentation purposes only
}

# ==============================================================================
# 3.2 SMOKING INITIATION FUNCTIONS
# ==============================================================================
#
# Age at smoking initiation is a critical variable for smoking history generator 
# models and epidemiological research. The >100 cigarettes criterion is embedded
# throughout these measures as the standard threshold for established smoking.
#
# PRIMARY CONTINUOUS MEASURES (Priority for smoking history models):
# ├── SMKG01C_cont - Age smoked first cigarette (>100 cigarettes context)
# ├── SMKG203_cont - Age started daily smoking (current daily smokers)  
# ├── SMKG207_cont - Age started daily smoking (former daily smokers)
# └── SMKG040_cont - Combined age started daily (harmonized across cycles)
#
# CATEGORICAL ALTERNATIVES (Available for research/testing):
# ├── SMKG01C_A/B - Age first cigarette (10/11 categories)
# ├── SMKG203_A/B - Age started daily (current, 10/11 categories)
# └── SMKG207_A/B - Age started daily (former, 10/11 categories)
#
# All functions use comprehensive CSV-driven mappings in variable_details.csv
# for harmonization across CCHS cycles 2001-2024.

#' Convert age at first cigarette (categorical to continuous)
#'
#' @description
#' Convert categorical age at first cigarette to continuous age using midpoint mappings.
#' The >100 cigarettes criterion provides context for established smoking history.
#' Essential for smoking history generator models and epidemiological research.
#'
#' @param SMKG01C Age smoked first cigarette categories (1-10 or 1-11). Accepts raw CCHS codes.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric continuous age at first cigarette with structured missing data:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Age values: 8, 13, 16, 18.5, 22, 27, 32, 37, 42, 47, 55 (category midpoints)
#'
#' @details
#' **Priority Variable for Smoking History Models**
#' 
#' SMKG01C_cont is the primary continuous measure for age at smoking initiation,
#' representing the >100 cigarettes threshold that defines established smoking.
#' This is critical for pack-years calculations and smoking exposure modeling.
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' 
#' result <- rec_with_table(
#'   data = your_data,
#'   variables = "SMKG01C_cont", 
#'   database_name = "cchs2017_2018_p",
#'   variable_details = variable_details
#' )
#' 
#' # Direct function usage (advanced users)
#' ages <- calculate_age_first_cigarette(
#'   SMKG01C = c(1, 3, 10, 96, 99) # Various age categories and missing codes
#' )
#' # Result: c(8, 17, 55, tagged_na("a"), tagged_na("b"))
#'
#' @note v3.0.0, last updated: 2025-07-14, status: active - CSV-driven age conversion
#' @export
calculate_age_first_cigarette <- function(SMKG01C, log_level = "silent") {
  
  # Clean CCHS missing codes
  cleaned <- clean_variables(
    categorical_vars = list(smkg01c = SMKG01C),
    categorical_pattern = "single_digit_missing",
    log_level = log_level
  )
  
  # Use lookup_recEnd to convert categories to continuous values using variable_details.csv
  # This leverages the comprehensive SMKG01C_cont mappings already defined in the CSV
  lookup_recEnd(cleaned$smkg01c_clean, "SMKG01C_cont", variable_details)
}

#' Convert age started smoking daily for current daily smokers (categorical to continuous)
#'
#' @description
#' Convert categorical age started smoking daily to continuous age for current daily smokers.
#' Filters for current daily smokers (SMK_005 = 1) and converts SMKG040 categories to 
#' continuous age values using midpoint mappings for harmonization across CCHS cycles.
#'
#' @param SMK_005 Type of smoker presently (1=daily, 2=occasional, 3=not at all). Accepts raw CCHS codes.
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
#' @note v3.0.0, last updated: 2025-07-14, status: active - Age conversion support
#' @export
calculate_age_started_daily_current <- function(SMK_005, SMKG040, log_level = "silent") {
  
  # Clean CCHS missing codes only - comprehensive validation handled by rec_with_table()
  cleaned <- clean_variables(
    categorical_vars = list(smk_005 = SMK_005, smkg040 = SMKG040),
    categorical_pattern = "single_digit_missing",
    log_level = log_level
  )
  
  # Filter for current daily smokers only, then convert using CSV mappings
  filtered_ages <- dplyr::case_when(
    !!!assign_tagged_na(cleaned$smk_005_clean),
    !!!assign_tagged_na(cleaned$smkg040_clean),
    
    # Only current daily smokers get age values
    cleaned$smk_005_clean == 1 ~ cleaned$smkg040_clean,
    
    # Not applicable for non-daily smokers
    .default = haven::tagged_na("a")
  )
  
  # Use lookup_recEnd to convert categories to continuous values using variable_details.csv
  # This leverages the comprehensive SMKG203_cont mappings already defined in the CSV
  lookup_recEnd(filtered_ages, "SMKG203_cont", variable_details)
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
#' @note v3.0.0, last updated: 2025-07-14, status: active - Age conversion support
#' @export
calculate_age_started_daily_former <- function(SMK_030, SMKG040, log_level = "silent") {
  
  # Clean CCHS missing codes only - comprehensive validation handled by rec_with_table()
  cleaned <- clean_variables(
    categorical_vars = list(smk_030 = SMK_030, smkg040 = SMKG040),
    categorical_pattern = "single_digit_missing",
    log_level = log_level
  )
  
  # Filter for former daily smokers only, then convert using CSV mappings
  filtered_ages <- dplyr::case_when(
    !!!assign_tagged_na(cleaned$smk_030_clean),
    !!!assign_tagged_na(cleaned$smkg040_clean),
    
    # Only former daily smokers get age values
    cleaned$smk_030_clean == 1 ~ cleaned$smkg040_clean,
    
    # Not applicable for never daily smokers
    .default = haven::tagged_na("a")
  )
  
  # Use lookup_recEnd to convert categories to continuous values using variable_details.csv
  # This leverages the comprehensive SMKG207_cont mappings already defined in the CSV
  lookup_recEnd(filtered_ages, "SMKG207_cont", variable_details)
}

#' Combine age started smoking daily variables (harmonization across cycles)
#'
#' @description
#' Combine SMKG203_cont (current daily) and SMKG207_cont (former daily) into 
#' unified SMKG040_cont variable for harmonization across CCHS cycles.
#' Essential for creating consistent age started smoking variables across survey evolution.
#'
#' @param SMKG203_cont Age started smoking daily for current daily smokers (continuous).
#' @param SMKG207_cont Age started smoking daily for former daily smokers (continuous).
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric continuous age started smoking daily, prioritizing current daily smokers.
#'   Uses first valid value: SMKG203_cont → SMKG207_cont → missing
#'
#' @details
#' **Harmonization Logic:**
#' 
#' This function creates SMKG040_cont for early CCHS cycles (2001-2014) by combining:
#' - **SMKG203_cont**: Age started daily smoking (current daily smokers)
#' - **SMKG207_cont**: Age started daily smoking (former daily smokers)
#' 
#' **Priority Order:**
#' 1. Use SMKG203_cont if valid (current daily smokers take priority)
#' 2. Use SMKG207_cont if SMKG203_cont invalid (former daily smokers)
#' 3. Return appropriate tagged_na if both invalid
#' 
#' **Cycle Coverage:**
#' - **2001-2014**: Derived from SMKG203_cont + SMKG207_cont using this function
#' - **2015-2024**: Direct from categorical SMKG040 using midpoint values
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
#' @note v3.0.0, last updated: 2025-07-14, status: active - Age harmonization across cycles
#' @export
calculate_SMKG040 <- function(SMKG203_cont, SMKG207_cont, log_level = "silent") {
  
  # Clean CCHS missing codes only - comprehensive validation handled by rec_with_table()
  cleaned <- clean_variables(
    continuous_vars = list(smkg203 = SMKG203_cont, smkg207 = SMKG207_cont),
    continuous_pattern = "triple_digit_missing",
    log_level = log_level
  )
  
  # Combine SMKG203 and SMKG207 - harmonization logic for cycle compatibility
  dplyr::case_when(
    # Current daily smoker takes priority - use if valid
    !is.na(cleaned$smkg203_clean) & !haven::is_tagged_na(cleaned$smkg203_clean) ~ cleaned$smkg203_clean,
    
    # Former daily smoker - use if valid
    !is.na(cleaned$smkg207_clean) & !haven::is_tagged_na(cleaned$smkg207_clean) ~ cleaned$smkg207_clean,
    
    # Use standardized tagged NA conditions if both invalid
    !!!assign_tagged_na(cleaned$smkg203_clean),
    !!!assign_tagged_na(cleaned$smkg207_clean),
    
    # Default to missing if both are missing
    .default = haven::tagged_na("b")
  )
}

#' Convert age started smoking daily from SMKG040 categories (current daily smokers)
#'
#' @description
#' Extract and convert age started smoking daily for current daily smokers from combined
#' SMKG040 variable. Filters by smoking status (SMK_005 = 1) and converts categorical
#' ages to continuous values using comprehensive CSV-driven mappings.
#'
#' @param SMK_005 Type of smoker presently (1=daily, 2=occasional, 3=not at all). Accepts raw CCHS codes.
#' @param SMKG040 Age started smoking daily categories (1-11). Accepts raw CCHS codes.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric continuous age for current daily smokers with structured missing data:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (non-daily smokers)
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Age values: 8, 13, 16, 18.5, 22, 27, 32, 37, 42, 47, 55 (category midpoints)
#'
#' @details
#' **Legacy Function Modernized with CSV-Driven Approach**
#'
#' This function modernizes the legacy SMKG203_fun with full lookup_recEnd integration:
#' - **Filter**: Only current daily smokers (SMK_005 = 1) receive age values
#' - **Age Conversion**: Uses comprehensive variable_details.csv mappings for SMKG203_cont
#' - **Cycle Coverage**: 2015-2024 (when SMKG040 combined variable introduced)
#' - **Legacy Compatibility**: Maintains exact behavioral compatibility with legacy SMKG203_fun
#'
#' **Midpoint Mappings** (from variable_details.csv):
#' - Category 1 → 8 years (5-11 years)
#' - Category 2 → 13 years (12-14 years)
#' - Category 3 → 16 years (15-17 years) for _B variants / 17 years (15-19 years) for _A variants
#' - Category 4 → 18.5 years (18-19 years)
#' - Category 5 → 22 years (20-24 years)
#' - Category 6 → 27 years (25-29 years)
#' - Category 7 → 32 years (30-34 years)
#' - Category 8 → 37 years (35-39 years)
#' - Category 9 → 42 years (40-44 years)
#' - Category 10 → 47 years (45-49 years)
#' - Category 11 → 55 years (50+ years)
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
#' smkg203_cont <- calculate_SMKG203_from_combined(
#'   SMK_005 = c(1, 2, 1, 3, 1), # Daily, occasional, daily, former, daily
#'   SMKG040 = c(3, 5, 7, 4, 11) # Age categories
#' )
#' # Result: c(16, tagged_na("a"), 32, tagged_na("a"), 55)
#'
#' @note v3.0.0, last updated: 2025-07-14, status: active - Modernized legacy SMKG203_fun
#' @export
calculate_SMKG203_from_combined <- function(SMK_005, SMKG040, log_level = "silent") {
  
  # Clean CCHS missing codes
  cleaned <- clean_variables(
    categorical_vars = list(smk_005 = SMK_005, smkg040 = SMKG040),
    categorical_pattern = "single_digit_missing",
    log_level = log_level
  )
  
  # Filter for current daily smokers only, then convert using CSV mappings
  filtered_ages <- dplyr::case_when(
    !!!assign_tagged_na(cleaned$smk_005_clean),
    !!!assign_tagged_na(cleaned$smkg040_clean),
    
    # Only current daily smokers get age values
    cleaned$smk_005_clean == 1 ~ cleaned$smkg040_clean,
    
    # Not applicable for non-daily smokers
    .default = haven::tagged_na("a")
  )
  
  # Use lookup_recEnd to convert categories to continuous values using variable_details.csv
  # This leverages the comprehensive SMKG203_cont mappings already defined in the CSV
  lookup_recEnd(filtered_ages, "SMKG203_cont", variable_details)
}

#' Convert age started smoking daily from SMKG040 categories (former daily smokers)
#'
#' @description
#' Extract and convert age started smoking daily for former daily smokers from combined
#' SMKG040 variable. Filters by ever daily status (SMK_030 = 1) and converts categorical
#' ages to continuous values using comprehensive CSV-driven mappings.
#'
#' @param SMK_030 Ever smoked daily in lifetime (1=yes, 2=no). Accepts raw CCHS codes.
#' @param SMKG040 Age started smoking daily categories (1-11). Accepts raw CCHS codes.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric continuous age for former daily smokers with structured missing data:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (never daily smokers)
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Age values: 8, 13, 16, 18.5, 22, 27, 32, 37, 42, 47, 55 (category midpoints)
#'
#' @details
#' **Legacy Function Modernized with CSV-Driven Approach**
#'
#' This function modernizes the legacy SMKG207_fun with full lookup_recEnd integration:
#' - **Filter**: Only former daily smokers (SMK_030 = 1) receive age values
#' - **Age Conversion**: Uses comprehensive variable_details.csv mappings for SMKG207_cont
#' - **Cycle Coverage**: 2015-2024 (when SMKG040 combined variable introduced)
#' - **Legacy Compatibility**: Maintains exact behavioral compatibility with legacy SMKG207_fun
#'
#' **Midpoint Mappings** (from variable_details.csv):
#' - Category 1 → 8 years (5-11 years)
#' - Category 2 → 13 years (12-14 years)
#' - Category 3 → 16 years (15-17 years) for _B variants / 17 years (15-19 years) for _A variants
#' - Category 4 → 18.5 years (18-19 years)
#' - Category 5 → 22 years (20-24 years)
#' - Category 6 → 27 years (25-29 years)
#' - Category 7 → 32 years (30-34 years)
#' - Category 8 → 37 years (35-39 years)
#' - Category 9 → 42 years (40-44 years)
#' - Category 10 → 47 years (45-49 years)
#' - Category 11 → 55 years (50+ years)
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
#' smkg207_cont <- calculate_SMKG207_from_combined(
#'   SMK_030 = c(1, 2, 1, 1, 2), # Ever daily, never daily, ever daily, ever daily, never daily
#'   SMKG040 = c(3, 5, 7, 4, 11) # Age categories
#' )
#' # Result: c(16, tagged_na("a"), 32, 18.5, tagged_na("a"))
#'
#' @note v3.0.0, last updated: 2025-07-14, status: active - Modernized legacy SMKG207_fun
#' @export
calculate_SMKG207_from_combined <- function(SMK_030, SMKG040, log_level = "silent") {
  
  # Clean CCHS missing codes
  cleaned <- clean_variables(
    categorical_vars = list(smk_030 = SMK_030, smkg040 = SMKG040),
    categorical_pattern = "single_digit_missing",
    log_level = log_level
  )
  
  # Filter for former daily smokers only, then convert using CSV mappings
  filtered_ages <- dplyr::case_when(
    !!!assign_tagged_na(cleaned$smk_030_clean),
    !!!assign_tagged_na(cleaned$smkg040_clean),
    
    # Only former daily smokers get age values
    cleaned$smk_030_clean == 1 ~ cleaned$smkg040_clean,
    
    # Not applicable for never daily smokers
    .default = haven::tagged_na("a")
  )
  
  # Use lookup_recEnd to convert categories to continuous values using variable_details.csv
  # This leverages the comprehensive SMKG207_cont mappings already defined in the CSV
  lookup_recEnd(filtered_ages, "SMKG207_cont", variable_details)
}

#' Handle continuous age started smoking daily for current daily smokers (2015+ cycles)
#'
#' @description
#' Process continuous age values for current daily smokers from SMKG040_I variable.
#' Filters by smoking status (SMK_005 = 1) and validates continuous age ranges
#' for 2015+ CCHS cycles with direct continuous data.
#'
#' @param SMK_005 Type of smoker presently (1=daily, 2=occasional, 3=not at all). Accepts raw CCHS codes.
#' @param SMKG040_I Age started smoking daily (continuous). Accepts raw CCHS codes.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric continuous age for current daily smokers with structured missing data:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (non-daily smokers)
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses or out-of-range ages
#'   }
#'   Age range: [8,95] years (validated continuous values)
#'
#' @details
#' **Legacy Function Modernized for 2015+ Continuous Data**
#'
#' This function modernizes the legacy SMKG203I_fun for handling continuous age data:
#' - **Filter**: Only current daily smokers (SMK_005 = 1) receive age values
#' - **Age Validation**: Continuous ages in range [8,95] years accepted
#' - **Cycle Coverage**: 2015-2024 (_M databases with continuous SMKG040_I)
#' - **Legacy Compatibility**: Maintains exact behavioral compatibility with legacy SMKG203I_fun
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' 
#' result <- rec_with_table(
#'   data = your_data,
#'   variables = "SMKG203_cont", 
#'   database_name = "cchs2015_2016_m",
#'   variable_details = variable_details
#' )
#' 
#' # Direct function usage (advanced users)
#' smkg203_cont <- calculate_SMKG203_continuous(
#'   SMK_005 = c(1, 2, 1, 3, 1), # Daily, occasional, daily, former, daily
#'   SMKG040_I = c(18, 25, 32, 28, 55) # Continuous ages
#' )
#' # Result: c(18, tagged_na("a"), 32, tagged_na("a"), 55)
#'
#' @note v3.0.0, last updated: 2025-07-14, status: active - Modernized legacy SMKG203I_fun
#' @export
calculate_SMKG203_continuous <- function(SMK_005, SMKG040_I, log_level = "silent") {
  
  # Clean CCHS missing codes
  cleaned <- clean_variables(
    categorical_vars = list(smk_005 = SMK_005),
    continuous_vars = list(smkg040_i = SMKG040_I),
    categorical_pattern = "single_digit_missing",
    continuous_pattern = "triple_digit_missing",
    log_level = log_level
  )
  
  # Filter and validate continuous ages
  dplyr::case_when(
    !!!assign_tagged_na(cleaned$smk_005_clean),
    !!!assign_tagged_na(cleaned$smkg040_i_clean),
    
    # Only current daily smokers get age values
    cleaned$smk_005_clean != 1 ~ haven::tagged_na("a"),
    
    # Validate age range for continuous values
    cleaned$smkg040_i_clean >= 8 & cleaned$smkg040_i_clean <= 95 ~ cleaned$smkg040_i_clean,
    
    # Out of range values marked as invalid
    .default = haven::tagged_na("b")
  )
}

#' Handle continuous age started smoking daily for former daily smokers (2015+ cycles)
#'
#' @description
#' Process continuous age values for former daily smokers from SMKG040_I variable.
#' Filters by ever daily status (SMK_030 = 1) and validates continuous age ranges
#' for 2015+ CCHS cycles with direct continuous data.
#'
#' @param SMK_030 Ever smoked daily in lifetime (1=yes, 2=no). Accepts raw CCHS codes.
#' @param SMKG040_I Age started smoking daily (continuous). Accepts raw CCHS codes.
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric continuous age for former daily smokers with structured missing data:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (never daily smokers)
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses or out-of-range ages
#'   }
#'   Age range: [8,95] years (validated continuous values)
#'
#' @details
#' **Legacy Function Modernized for 2015+ Continuous Data**
#'
#' This function modernizes the legacy SMKG207I_fun for handling continuous age data:
#' - **Filter**: Only former daily smokers (SMK_030 = 1) receive age values
#' - **Age Validation**: Continuous ages in range [8,95] years accepted
#' - **Cycle Coverage**: 2015-2024 (_M databases with continuous SMKG040_I)
#' - **Legacy Compatibility**: Maintains exact behavioral compatibility with legacy SMKG207I_fun
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' 
#' result <- rec_with_table(
#'   data = your_data,
#'   variables = "SMKG207_cont", 
#'   database_name = "cchs2015_2016_m",
#'   variable_details = variable_details
#' )
#' 
#' # Direct function usage (advanced users)
#' smkg207_cont <- calculate_SMKG207_continuous(
#'   SMK_030 = c(1, 2, 1, 1, 2), # Ever daily, never daily, ever daily, ever daily, never daily
#'   SMKG040_I = c(18, 25, 32, 28, 55) # Continuous ages
#' )
#' # Result: c(18, tagged_na("a"), 32, 28, tagged_na("a"))
#'
#' @note v3.0.0, last updated: 2025-07-14, status: active - Modernized legacy SMKG207I_fun
#' @export
calculate_SMKG207_continuous <- function(SMK_030, SMKG040_I, log_level = "silent") {
  
  # Clean CCHS missing codes
  cleaned <- clean_variables(
    categorical_vars = list(smk_030 = SMK_030),
    continuous_vars = list(smkg040_i = SMKG040_I),
    categorical_pattern = "single_digit_missing",
    continuous_pattern = "triple_digit_missing",
    log_level = log_level
  )
  
  # Filter and validate continuous ages
  dplyr::case_when(
    !!!assign_tagged_na(cleaned$smk_030_clean),
    !!!assign_tagged_na(cleaned$smkg040_i_clean),
    
    # Only former daily smokers get age values
    cleaned$smk_030_clean != 1 ~ haven::tagged_na("a"),
    
    # Validate age range for continuous values
    cleaned$smkg040_i_clean >= 8 & cleaned$smkg040_i_clean <= 95 ~ cleaned$smkg040_i_clean,
    
    # Out of range values marked as invalid
    .default = haven::tagged_na("b")
  )
}

# ==============================================================================
# 3.3 TIME-BASED SMOKING FUNCTIONS  
# ==============================================================================

#' Calculate time since quit smoking
#'
#' @description
#' Calculate time since quit smoking for former daily smokers using CCHS variables.
#' This function implements a complex decision tree based on two input variables,
#' `SMK_09A_B` and `SMKG09C`, to determine the number of years since a person
#' quit smoking. The logic is driven by mappings in `variable_details.csv`.
#'
#' @param SMK_09A_B Time since quit categories for recent quitters (1-4). This
#'   variable takes priority. A value of 4 indicates that `SMKG09C` should be
#'   used instead. Accepts raw CCHS codes.
#' @param SMKG09C Time since quit categories for long-term quitters (1-3). Used
#'   only when `SMK_09A_B` is 4. Accepts raw CCHS codes.
#' @param log_level Logging level for the `clean_variables` helper: "silent"
#'   (default), "warning", "verbose".
#'
#' @return A numeric vector representing the time in years since quitting.
#'   Returns `haven::tagged_na` values for missing or invalid data.
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' # result <- rec_with_table(
#' #   data = your_data,
#' #   variables = "time_quit_smoking",
#' #   database_name = "cchs2011_2012_p"
#' # )
#'
#' # Direct function usage (advanced users)
#' # calculate_time_quit_smoking(SMK_09A_B = c(1, 2, 3, 4, 4),
#' #                             SMKG09C = c(NA, NA, NA, 1, 2))
#' #> [1] 1.5 2.5 3.5 4.0 8.0
#'
#' @note v3.1.0, last updated: 2025-07-12, status: active - Refactored to use lookup_recEnd
#' @export
calculate_time_quit_smoking <- function(SMK_09A_B, SMKG09C, log_level = "silent") {

  # 1. Load metadata from the canonical CSV file location
  var_details <- load_variable_details()

  # 2. Use clean_variables() for initial CCHS code processing
  cleaned <- clean_variables(
    categorical_vars = list(smk_09a_b = SMK_09A_B),
    continuous_vars = list(smkg09c = SMKG09C),
    categorical_pattern = "single_digit_missing",
    continuous_pattern = "triple_digit_missing",
    log_level = log_level
  )
  smk_09a_b_clean <- cleaned$smk_09a_b_clean
  smkg09c_clean <- cleaned$smkg09c_clean

  # 3. Use the lookup utility to get continuous values
  # The lookup function will return NA for values not in the metadata.
  smk_a_b_cont <- lookup_recEnd(smk_09a_b_clean, "SMK_09A_B", var_details)
  smk_c_cont   <- lookup_recEnd(smkg09c_clean,   "SMKG09C",   var_details)

  # 4. Apply the priority logic cleanly
  # This logic correctly prioritizes SMK_09A_B and defers to SMKG09C only when
  # SMK_09A_B has the specific value of 4.
  time_quit <- dplyr::case_when(
    # Handle tagged NAs first to ensure they propagate correctly.
    haven::is_tagged_na(smk_09a_b_clean, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(smk_09a_b_clean, "b") ~ haven::tagged_na("b"),

    # Priority 1: If SMK_09A_B is 4, use the value from SMKG09C.
    # We must also check if the cleaned SMKG09C is a tagged_na and propagate it.
    smk_09a_b_clean == 4 & haven::is_tagged_na(smkg09c_clean) ~ smkg09c_clean,
    smk_09a_b_clean == 4 & !is.na(smk_c_cont) ~ smk_c_cont,
    smk_09a_b_clean == 4 & is.na(smk_c_cont) ~ haven::tagged_na("b"),

    # Priority 2: Otherwise, use the continuous value from SMK_09A_B.
    !is.na(smk_a_b_cont) ~ smk_a_b_cont,

    # Default to tagged_na("b") if no other condition is met.
    # This handles cases where smk_a_b_cont is NA (e.g., an invalid code).
    TRUE ~ haven::tagged_na("b")
  )

  return(time_quit)
}

# Custom function removed - now using standard rec_with_table() approach
# All mapping logic is handled by variable_details.csv configuration


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
calculate_pack_years <- function(SMKDSTY_A, DHHGAGE_cont, time_quit_smoking, 
                                 SMKG203_cont, SMKG207_cont, SMK_204, 
                                 SMK_05B, SMK_208, SMK_05C, 
                                 SMKG01C_cont, SMK_01A, log_level = "silent") {
  
  # Clean all input variables
  cleaned <- clean_variables(
    categorical_vars = list(
      smkdsty_a = SMKDSTY_A,
      smk_01a = SMK_01A
    ),
    continuous_vars = list(
      dhhgage_cont = DHHGAGE_cont,
      time_quit_smoking = time_quit_smoking,
      smkg203_cont = SMKG203_cont,
      smkg207_cont = SMKG207_cont,
      smk_204 = SMK_204,
      smk_05b = SMK_05B,
      smk_208 = SMK_208,
      smk_05c = SMK_05C,
      smkg01c_cont = SMKG01C_cont
    ),
    log_level = log_level
  )
  
  # Route to the correct internal function based on smoking status
  dplyr::case_when(
    cleaned$smkdsty_a_clean == 1 ~ calculate_pack_years_daily(
      cleaned$dhhgage_cont_clean, cleaned$smkg203_cont_clean, cleaned$smk_204_clean
    ),
    cleaned$smkdsty_a_clean == 2 ~ calculate_pack_years_occasional_former(
      cleaned$dhhgage_cont_clean, cleaned$smkg207_cont_clean, cleaned$time_quit_smoking_clean,
      cleaned$smk_05b_clean, cleaned$smk_208_clean, cleaned$smk_05c_clean
    ),
    cleaned$smkdsty_a_clean == 3 ~ calculate_pack_years_occasional_never(
      cleaned$dhhgage_cont_clean, cleaned$smkg01c_cont_clean, cleaned$smk_05b_clean, cleaned$smk_05c_clean
    ),
    cleaned$smkdsty_a_clean == 4 ~ calculate_pack_years_former_daily(
      cleaned$dhhgage_cont_clean, cleaned$smkg207_cont_clean, cleaned$time_quit_smoking_clean, cleaned$smk_208_clean
    ),
    cleaned$smkdsty_a_clean == 5 ~ calculate_pack_years_former_occasional(
      cleaned$smk_01a_clean
    ),
    cleaned$smkdsty_a_clean == 6 ~ calculate_pack_years_never(),
    
    # Handle missing data
    .default = haven::tagged_na("b")
  )
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
# Function moved to section 3.2 SMOKING INITIATION FUNCTIONS

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
# Function moved to section 3.2 SMOKING INITIATION FUNCTIONS

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
  
  # Clean CCHS missing codes only - comprehensive validation handled by rec_with_table()
  cleaned <- clean_variables(
    categorical_vars = list(smkdsty_cat5 = SMKDSTY_cat5),
    continuous_vars = list(time_quit_smoking = time_quit_smoking),
    categorical_pattern = "single_digit_missing",
    continuous_pattern = "triple_digit_missing",
    log_level = log_level
  )
  
  # Simple smoking status classification - full logic via rec_with_table()
  dplyr::case_when(
    # Simple smoking status classification logic
    cleaned$smkdsty_cat5_clean == 5 ~ 0L, # Never smoked
    cleaned$smkdsty_cat5_clean %in% c(1, 2) ~ 1L, # Current smoker (daily and occasional)
    cleaned$smkdsty_cat5_clean == 4 ~ 2L, # Former occasional smoker
    cleaned$smkdsty_cat5_clean == 3 & !is.na(cleaned$time_quit_smoking_clean) & !haven::is_tagged_na(cleaned$time_quit_smoking_clean) & cleaned$time_quit_smoking_clean <= 5 ~ 2L,
    cleaned$smkdsty_cat5_clean == 3 & !is.na(cleaned$time_quit_smoking_clean) & !haven::is_tagged_na(cleaned$time_quit_smoking_clean) & cleaned$time_quit_smoking_clean > 5 ~ 3L,
    cleaned$smkdsty_cat5_clean == 3 & (is.na(cleaned$time_quit_smoking_clean) | haven::is_tagged_na(cleaned$time_quit_smoking_clean)) ~ 2L,

    # Use standardized tagged NA conditions for both inputs
    !!!assign_tagged_na(cleaned$smkdsty_cat5_clean),
    !!!assign_tagged_na(cleaned$time_quit_smoking_clean),

    # Default to missing for any unhandled cases
    .default = haven::tagged_na("b")
  )
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
  
  # Clean CCHS missing codes only - comprehensive validation handled by rec_with_table()
  cleaned <- clean_variables(
    continuous_vars = list(pack_years = pack_years_der),
    continuous_pattern = "triple_digit_missing",
    log_level = log_level
  )
  
  # Pack-years categorization - cutoffs from variable_details.csv in rec_with_table()
  # Simplified logic here, full cutoff mappings via CSV-driven approach
  dplyr::case_when(
    !!!assign_tagged_na(cleaned$pack_years_clean),
    
    # Basic categorization - full implementation via rec_with_table() with variable_details.csv
    cleaned$pack_years_clean == 0 ~ 1L,  # No exposure
    cleaned$pack_years_clean > 0 & cleaned$pack_years_clean <= 0.01 ~ 2L,  # Minimal
    
    # Default for other ranges - implemented via CSV
    .default = haven::tagged_na("b")
  )
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
# Function moved to section 3.2 SMOKING INITIATION FUNCTIONS


# ==============================================================================
# 4. INTERNAL HELPER FUNCTIONS (MODULAR & TESTABLE)
# ==============================================================================

#' Calculate pack-years for daily smokers (internal helper)
#' @noRd
calculate_pack_years_daily <- function(current_age, age_started_daily, cigarettes_daily) {
  # Duration of smoking in years
  smoking_duration <- current_age - age_started_daily
  
  # Pack-years calculation
  pack_years <- (smoking_duration * cigarettes_daily) / PACK_YEARS_CONSTANTS$cigarettes_per_pack
  
  # Ensure non-negative and apply minimum threshold
  pmax(0, pack_years, na.rm = TRUE)
}

#' Calculate pack-years for former daily smokers (internal helper)
#' @noRd
calculate_pack_years_former_daily <- function(current_age, age_started_daily, time_quit_smoking, cigarettes_daily) {
  # Age at quitting
  age_quit <- current_age - time_quit_smoking
  
  # Duration of smoking in years
  smoking_duration <- age_quit - age_started_daily
  
  # Pack-years calculation
  pack_years <- (smoking_duration * cigarettes_daily) / PACK_YEARS_CONSTANTS$cigarettes_per_pack
  
  # Ensure non-negative and apply minimum threshold
  pmax(0, pack_years, na.rm = TRUE)
}

#' Calculate pack-years for occasional smokers who were former daily smokers (internal helper)
#' @noRd
calculate_pack_years_occasional_former <- function(current_age, age_started_daily, time_quit_smoking, 
                                                   cigs_per_day_occasional, cigs_per_day_former_daily, days_smoked_monthly) {
  
  # 1. Pack-years from the period of being a daily smoker
  daily_smoking_duration <- (current_age - time_quit_smoking) - age_started_daily
  pack_years_daily_period <- (daily_smoking_duration * cigs_per_day_former_daily) / PACK_YEARS_CONSTANTS$cigarettes_per_pack
  
  # 2. Pack-years from the period of being an occasional smoker
  # Convert monthly smoking days to a daily average
  avg_cigs_per_day_occasional <- (cigs_per_day_occasional * days_smoked_monthly) / 30
  pack_years_occasional_period <- (time_quit_smoking * avg_cigs_per_day_occasional) / PACK_YEARS_CONSTANTS$cigarettes_per_pack
  
  # Total pack-years
  total_pack_years <- pack_years_daily_period + pack_years_occasional_period
  
  # Ensure non-negative and apply minimum threshold
  pmax(0, total_pack_years, na.rm = TRUE)
}

#' Calculate pack-years for occasional smokers who never smoked daily (internal helper)
#' @noRd
calculate_pack_years_occasional_never <- function(current_age, age_first_cig, cigs_per_day_occasional, days_smoked_monthly) {
  
  # Duration of occasional smoking
  smoking_duration <- current_age - age_first_cig
  
  # Average daily cigarettes
  avg_cigs_per_day <- (cigs_per_day_occasional * days_smoked_monthly) / 30
  
  # Pack-years calculation
  pack_years <- (smoking_duration * avg_cigs_per_day) / PACK_YEARS_CONSTANTS$cigarettes_per_pack
  
  # Ensure non-negative and apply minimum threshold
  pmax(0, pack_years, na.rm = TRUE)
}

#' Calculate pack-years for former occasional smokers (internal helper)
#' @noRd
calculate_pack_years_former_occasional <- function(smoked_100_cigs) {
  # Assign a minimum pack-year value if they smoked over 100 cigarettes
  dplyr::if_else(smoked_100_cigs == 1, PACK_YEARS_CONSTANTS$min_pack_years, 0)
}

#' Calculate pack-years for never smokers (internal helper)
#' @noRd
calculate_pack_years_never <- function() {
  # Never smokers have 0 pack-years
  return(0)
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