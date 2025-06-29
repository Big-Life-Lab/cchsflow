# Smoking Derived Variable Functions (Modernized)
# 
# This file contains smoking derived variable functions using modern tidyverse patterns.
# Legacy version preserved in R/smoking_legacy.R for reference.
# 
# Modernization completed: 2025-01 (Phases 1-3 of smoking refactoring plan)
#
# Refactoring Goals:
# - Replace deeply nested ifelse() with readable case_when() patterns
# - Consolidate base functions with _A variants using unified validation approach
# - Standardize parameter validation and age bounds checking
# - Preserve all existing functionality and backward compatibility
# - Maintain support for both basic CCHS processing and research validation modes
# - Follow copy-paste philosophy: functions work standalone with explicit package references
#
# Note: Uses explicit haven::tagged_na() and dplyr::case_when() for clarity to users
# unfamiliar with cchsflow who might copy-paste these functions.

library(dplyr)
library(haven)

# Load missing data preprocessing helpers
source("R/missing-data-helpers.R")

# ============================================================================
# PHASE 1: FOUNDATION - HELPER FUNCTIONS AND VALIDATION FRAMEWORK
# ============================================================================

# ----------------------------------------------------------------------------
# Age Validation Constants and Bounds
# ----------------------------------------------------------------------------

# FUTURE WORK: These constants should migrate to variable_details.csv
# with columns like min_value, max_value, validation_rules
# See smoking-refactoring.md "Phase 5: Constants Migration to variable_details.csv"
#
# Current approach follows cchsflow constants management philosophy:
# ✅ Constants declared outside functions (not embedded in logic)
# ✅ Function parameter defaults are acceptable
# ❌ Avoid hard-coded constants within function bodies

# Minimum smoking initiation age (corrects implausible early ages)
MIN_SMOKING_INITIATION_AGE <- 8

# Maximum plausible smoking initiation age  
MAX_SMOKING_INITIATION_AGE <- 95

# Age bounds for different CCHS variables
SMOKING_AGE_BOUNDS <- list(
  SMKG203_cont = list(min = 8, max = 84),    # Age started daily (daily smokers)
  SMKG207_cont = list(min = 8, max = 84),    # Age started daily (former daily)
  SMKG040_cont = list(min = 8, max = 95),    # Combined age started daily
  SMKG01C_cont = list(min = 8, max = 84),    # Age first cigarette
  current_age = list(min = 12, max = 102)    # Current age bounds
)

# Time since quit bounds
TIME_QUIT_BOUNDS <- list(
  min = 0.5,   # Minimum time since quitting (6 months)
  max = 82     # Maximum plausible time since quitting
)

# Pack-years calculation constants
PACK_YEARS_CONSTANTS <- list(
  cigarettes_per_pack = 20,
  min_pack_years = 0.0137,      # Minimum for former occasional smokers  
  min_pack_years_never = 0.007  # Minimum for never-daily smokers
)

# ----------------------------------------------------------------------------
# Core Helper Functions
# ----------------------------------------------------------------------------

#' Validate age with bounds checking
#' 
#' @param age Numeric age value to validate
#' @param min_age Minimum valid age (default 8, aligns with Holford et al. smoking initiation standards)
#' @param max_age Maximum valid age (default 95)
#' @param na_handling Character tag for NA handling ("a" or "b")
#' @return Validated age or haven::tagged_na
validate_age_bounds <- function(age, min_age = 8, max_age = 95, na_handling = "b") {
  dplyr::case_when(
    is.na(age) ~ haven::tagged_na(na_handling),
    age < min_age | age > max_age ~ haven::tagged_na("b"),
    TRUE ~ age
  )
}

#' Generic age bounds validation using predefined bounds
#' 
#' @param age Numeric age value to validate
#' @param variable_name Name of CCHS variable for bounds lookup
#' @return Validated age or haven::tagged_na with appropriate bounds
validate_age_variable <- function(age, variable_name) {
  bounds <- SMOKING_AGE_BOUNDS[[variable_name]]
  if (is.null(bounds)) {
    warning(paste("No bounds defined for variable:", variable_name))
    return(age)
  }
  
  dplyr::case_when(
    is.na(age) ~ haven::tagged_na("b"),
    age == "NA(a)" ~ haven::tagged_na("a"),  
    age < bounds$min ~ haven::tagged_na("b"),
    age > bounds$max ~ haven::tagged_na("b"),
    TRUE ~ age
  )
}

#' Apply smoking initiation age bounds
#' 
#' @param age Age when smoking initiation occurred
#' @return Validated age with smoking-specific bounds
validate_smoking_initiation_age <- function(age) {
  dplyr::case_when(
    is.na(age) ~ haven::tagged_na("b"),
    age == "NA(a)" ~ haven::tagged_na("a"),
    age < MIN_SMOKING_INITIATION_AGE ~ MIN_SMOKING_INITIATION_AGE,
    age > MAX_SMOKING_INITIATION_AGE ~ haven::tagged_na("b"),
    TRUE ~ age
  )
}

#' Time categorization helper for smoking cessation
#' 
#' @param time_value Continuous time value
#' @param breaks Numeric vector of break points
#' @param labels Character vector of labels for categories
#' @return Categorized time value
categorize_time_ranges <- function(time_value, breaks, labels) {
  dplyr::case_when(
    is.na(time_value) ~ haven::tagged_na("b"),
    time_value < breaks[1] ~ labels[1],
    time_value >= breaks[length(breaks)] ~ labels[length(labels)],
    TRUE ~ labels[findInterval(time_value, breaks)]
  )
}

#' Pack-years calculation helper
#' 
#' @param cigarettes_per_day Number of cigarettes smoked per day
#' @param years_smoked Number of years smoking
#' @param min_pack_years Minimum pack-years value (default 0.0137)
#' @return Pack-years calculation with minimum threshold
calculate_pack_years <- function(cigarettes_per_day, years_smoked, min_pack_years = 0.0137) {
  pack_years <- (cigarettes_per_day / 20) * years_smoked
  pmax(pack_years, min_pack_years, na.rm = TRUE)
}

#' CCHS variable recoding helper
#' 
#' @param variable Variable to recode
#' @param recode_map Named vector mapping old values to new values
#' @return Recoded variable with proper NA handling
recode_cchs_categories <- function(variable, recode_map) {
  dplyr::case_when(
    variable == "NA(a)" ~ haven::tagged_na("a"),
    variable == "NA(b)" ~ haven::tagged_na("b"),
    variable %in% names(recode_map) ~ recode_map[as.character(variable)],
    TRUE ~ haven::tagged_na("b")
  )
}

# ----------------------------------------------------------------------------
# Smoking-Specific Helper Functions
# ----------------------------------------------------------------------------

#' Current vs former smoker classification
#' 
#' @param smkdsty_cat5 5-category smoking status variable
#' @return Binary current smoker indicator (1 = current, 0 = not current)
derive_current_smoker <- function(smkdsty_cat5) {
  dplyr::case_when(
    smkdsty_cat5 %in% c(1, 2) ~ 1L,  # Current smoker (daily + occasional)
    smkdsty_cat5 %in% c(3, 4, 5) ~ 0L,  # Not current (former + never)
    smkdsty_cat5 == "NA(a)" ~ haven::tagged_na("a"),  # String-based NA(a)
    haven::is_tagged_na(smkdsty_cat5, "a") ~ haven::tagged_na("a"),  # haven tagged NA(a)
    .default = haven::tagged_na("b")
  )
}

#' Ever smoker classification  
#' 
#' @param smkdsty_cat5 5-category smoking status variable
#' @return Binary ever smoker indicator (1 = ever smoked, 0 = never)
derive_ever_smoker <- function(smkdsty_cat5) {
  dplyr::case_when(
    smkdsty_cat5 %in% c(1, 2, 3, 4) ~ 1L,  # Ever smoked (current + former)
    smkdsty_cat5 == 5 ~ 0L,  # Never smoked
    smkdsty_cat5 == "NA(a)" ~ haven::tagged_na("a"),  # String-based NA(a)
    haven::is_tagged_na(smkdsty_cat5, "a") ~ haven::tagged_na("a"),  # haven tagged NA(a)
    .default = haven::tagged_na("b")
  )
}

#' Time since quit categorization for former smokers
#' 
#' @param time_value Continuous time since quitting value
#' @return Categorized time with standard CCHS categories
categorize_quit_time <- function(time_value) {
  dplyr::case_when(
    is.na(time_value) ~ haven::tagged_na("b"),
    time_value == "NA(a)" ~ haven::tagged_na("a"),
    time_value < 1 ~ 0.5,      # <1 year
    time_value >= 1 & time_value < 2 ~ 1.5,  # 1-2 years
    time_value >= 2 & time_value < 3 ~ 2.5,  # 2-3 years
    TRUE ~ time_value  # Use continuous value for 3+ years
  )
}

#' Smoking status classification helper
#' 
#' @param smkdsty_value SMKDSTY smoking status value
#' @return Character classification of smoking status
classify_smoking_status <- function(smkdsty_value) {
  dplyr::case_when(
    smkdsty_value %in% c(1, 2) ~ "current",
    smkdsty_value %in% c(3, 4) ~ "former", 
    smkdsty_value == 5 ~ "never",
    haven::is_tagged_na(smkdsty_value, "a") ~ haven::tagged_na("a"),
    .default = haven::tagged_na("b")
  )
}

# ============================================================================
# PHASE 2: CORE FUNCTION REFACTORING - TIME AND STATUS FUNCTIONS
# ============================================================================

# ----------------------------------------------------------------------------
# Time Since Quit Smoking Functions (Modernized)
# ----------------------------------------------------------------------------

#' Time since quit smoking (unified approach)
#'
#' @description This function creates a derived variable (time_quit_smoking_der)
#'  that calculates the approximate time a former smoker has quit smoking based
#'  on various CCHS smoking variables. This variable is for CCHS respondents in
#'  CCHS surveys 2003-2014.
#'
#' @param SMK_09A_B number of years since quitting smoking. Variable asked to
#'  former daily smokers who quit <3 years ago.
#' @param SMKG09C number of years since quitting smoking. Variable asked to
#'  former daily smokers who quit >=3 years ago. Can be categorical (1,2,3) or continuous.
#' @param min_SMKG09C minimum valid value for SMKG09C (NULL for no validation)
#' @param max_SMKG09C maximum valid value for SMKG09C (NULL for no validation)
#' @param validate_params logical, enable parameter validation (auto-detected if NULL)
#'
#' @return value for time since quit smoking in time_quit_smoking_der.
#'
#' @examples
#' # Standard CCHS processing (basic mode)
#' time_quit_smoking_fun(SMK_09A_B = 4, SMKG09C = 2)  # Returns 8 years
#'
#' # Research with validation (enhanced mode)
#' time_quit_smoking_fun(SMK_09A_B = 4, SMKG09C_cont = 15,
#'                       min_SMKG09C = 3, max_SMKG09C = 82)  # Returns 15 years
#'
#' @references 
#' Holford, T.R., et al. (2014). Patterns of Birth Cohort–Specific Smoking Histories, 1965–2009. 
#' \emph{Am J Prev Med}, 46(2), e31-7. \doi{10.1016/j.amepre.2013.10.022}
#' 
#' Manuel, D.G., et al. (2020). Smoking Patterns Based on Birth-Cohort-Specific Histories from 1965 to 2013, with Projections to 2041. 
#' \emph{Health Reports}, 31(11), 16-31. 
#' \url{https://www150.statcan.gc.ca/n1/pub/82-003-x/2020011/article/00002-eng.htm}
#'
#' @export
time_quit_smoking_fun <- function(SMK_09A_B, SMKG09C, 
                                  min_SMKG09C = NULL, max_SMKG09C = NULL,
                                  validate_params = NULL) {
  
  # Preprocess original CCHS missing codes
  SMK_09A_B <- preprocess_smoking_variable(SMK_09A_B, variable_name = "SMK_09A_B")
  # SMKG09C can be categorical age or continuous - determine pattern automatically
  if (any(SMKG09C %in% c(96, 97, 98, 99), na.rm = TRUE)) {
    SMKG09C <- preprocess_categorical_age(SMKG09C)
  } else {
    # Handle as standard response or continuous values
    SMKG09C <- preprocess_smoking_variable(SMKG09C, pattern_type = "standard_response")
  }
  
  # Auto-detect validation mode based on parameters
  if (is.null(validate_params)) {
    validate_params <- !is.null(min_SMKG09C) || !is.null(max_SMKG09C)
  }
  
  # Set default bounds for validation mode
  if (validate_params && is.null(min_SMKG09C)) min_SMKG09C <- 3
  if (validate_params && is.null(max_SMKG09C)) max_SMKG09C <- 82
  
  # Convert categorical SMKG09C to continuous if needed
  SMKG09C_cont <- dplyr::case_when(
    is.numeric(SMKG09C) ~ as.numeric(SMKG09C),  # Already continuous (research use)
    SMKG09C == 1 ~ 4,   # 3-5 years category (standard CCHS)
    SMKG09C == 2 ~ 8,   # 6-10 years category  
    SMKG09C == 3 ~ 12,  # 11+ years category
    haven::is_tagged_na(SMKG09C, "a") ~ haven::tagged_na("a"),  # haven tagged NA(a)
    .default = haven::tagged_na("b")
  )
  
  # Apply parameter validation for research use cases
  if (validate_params) {
    SMKG09C_cont <- dplyr::case_when(
      SMKG09C_cont < min_SMKG09C | SMKG09C_cont > max_SMKG09C ~ haven::tagged_na("b"),
      .default = SMKG09C_cont
    )
  }
  
  # Main calculation using case_when
  dplyr::case_when(
    SMK_09A_B == 1 ~ 0.5,   # <1 year
    SMK_09A_B == 2 ~ 1.5,   # 1-2 years
    SMK_09A_B == 3 ~ 2.5,   # 2-3 years
    SMK_09A_B == 4 ~ SMKG09C_cont,  # Use continuous value
    haven::is_tagged_na(SMK_09A_B, "a") ~ haven::tagged_na("a"),  # haven tagged NA(a)
    .default = haven::tagged_na("b")
  )
}

#' Time since quit smoking (enhanced validation wrapper)
#'
#' @description Enhanced version with robust parameter validation and bounds checking.
#'  Maintained for backward compatibility.
#'
#' @param SMK_09A_B number of years since quitting smoking (categorical)
#' @param SMKG09C_cont continuous years since quitting smoking
#' @param min_SMKG09C_cont minimum valid value (default 3)
#' @param max_SMKG09C_cont maximum valid value (default 82)
#'
#' @return value for time since quit smoking
#'
#' @export
time_quit_smoking_fun_A <- function(SMK_09A_B, SMKG09C_cont,
                                    min_SMKG09C_cont = 3, max_SMKG09C_cont = 82) {
  time_quit_smoking_fun(SMK_09A_B, SMKG09C_cont, 
                        min_SMKG09C = min_SMKG09C_cont, 
                        max_SMKG09C = max_SMKG09C_cont,
                        validate_params = TRUE)
}

# ----------------------------------------------------------------------------
# Smoking Status Classification Functions (Modernized)
# ----------------------------------------------------------------------------

#' Simple smoking status (unified approach)
#'
#' @description This function creates a derived smoking variable (smoke_simple)
#'  with four categories: non-smoker, current smoker, former daily smoker quit <=5 years
#'  or former occasional smoker, former daily smoker quit >5 years
#'
#' @param SMKDSTY_cat5 derived variable that classifies an individual's smoking status
#' @param time_quit_smoking derived variable that calculates the approximate time a former smoker has quit smoking
#' @param min_time_quit minimum valid time since quitting (NULL for no validation)  
#' @param max_time_quit maximum valid time since quitting (NULL for no validation)
#' @param validate_params logical, enable parameter validation (auto-detected if NULL)
#'
#' @return value for simple smoking status categories
#'
#' @export
smoke_simple_fun <- function(SMKDSTY_cat5, time_quit_smoking,
                            min_time_quit = NULL, max_time_quit = NULL,
                            validate_params = NULL) {
  
  # Convert string-based NAs to proper types at the start (vectorized)
  SMKDSTY_cat5 <- dplyr::case_when(
    SMKDSTY_cat5 == "NA(a)" ~ haven::tagged_na("a"),
    SMKDSTY_cat5 == "NA(b)" ~ haven::tagged_na("b"),
    is.character(SMKDSTY_cat5) ~ as.numeric(SMKDSTY_cat5),  # Convert other character values to numeric
    .default = as.numeric(SMKDSTY_cat5)  # Ensure all values are numeric
  )
  
  time_quit_smoking <- dplyr::case_when(
    time_quit_smoking == "NA(a)" ~ haven::tagged_na("a"),
    time_quit_smoking == "NA(b)" ~ haven::tagged_na("b"),
    is.character(time_quit_smoking) ~ as.numeric(time_quit_smoking),  # Convert other character values to numeric
    .default = as.numeric(time_quit_smoking)  # Ensure all values are numeric
  )
  
  # Auto-detect validation mode
  if (is.null(validate_params)) {
    validate_params <- !is.null(min_time_quit) || !is.null(max_time_quit)
  }
  
  # Set default bounds for validation mode
  if (validate_params && is.null(min_time_quit)) min_time_quit <- 0.5
  if (validate_params && is.null(max_time_quit)) max_time_quit <- 82
  
  # Apply validation if requested
  if (validate_params) {
    time_quit_smoking <- dplyr::case_when(
      haven::is_tagged_na(time_quit_smoking, "a") ~ haven::tagged_na("a"),  # Preserve tagged NA(a)
      haven::is_tagged_na(time_quit_smoking, "b") ~ haven::tagged_na("b"),  # Preserve tagged NA(b)
      is.na(time_quit_smoking) ~ haven::tagged_na("b"),
      time_quit_smoking < min_time_quit | time_quit_smoking > max_time_quit ~ haven::tagged_na("b"),
      .default = time_quit_smoking  # Valid numeric value
    )
  }
  
  # Use helper functions for classification
  smoker <- derive_current_smoker(SMKDSTY_cat5)
  eversmoker <- derive_ever_smoker(SMKDSTY_cat5)
  
  # Main smoking status classification using case_when
  dplyr::case_when(
    # Handle NA cases for smoking status variables first
    haven::is_tagged_na(smoker, "a") | haven::is_tagged_na(eversmoker, "a") ~ haven::tagged_na("a"),
    
    # Non-smoker (never smoked)
    smoker == 0L & eversmoker == 0L ~ 0L,
    
    # Current smoker (daily and occasional) - time_quit_smoking not relevant
    smoker == 1L & eversmoker == 1L ~ 1L,
    
    # Former occasional smoker (always category 2, regardless of time)
    SMKDSTY_cat5 == 4 ~ 2L,
    
    # Handle time_quit_smoking NA for former smokers only
    smoker == 0L & eversmoker == 1L & haven::is_tagged_na(time_quit_smoking, "a") ~ haven::tagged_na("a"),
    
    # Former daily smoker quit <=5 years 
    smoker == 0L & eversmoker == 1L & !is.na(time_quit_smoking) & time_quit_smoking <= 5 ~ 2L,
    
    # Former daily smoker quit >5 years
    smoker == 0L & eversmoker == 1L & !is.na(time_quit_smoking) & time_quit_smoking > 5 ~ 3L,
    
    .default = haven::tagged_na("b")
  )
}

#' Simple smoking status (enhanced validation wrapper)
#'
#' @description Enhanced version with parameter validation.
#'  Maintained for backward compatibility.
#'
#' @param SMKDSTY_cat5 smoking status category variable
#' @param time_quit_smoking time since quitting smoking
#' @param min_time_quit_smoking minimum valid time (default 0.5)
#' @param max_time_quit_smoking maximum valid time (default 82)
#'
#' @return simple smoking status value
#'
#' @export  
smoke_simple_fun_A <- function(SMKDSTY_cat5, time_quit_smoking,
                              min_time_quit_smoking = 0.5, max_time_quit_smoking = 82) {
  smoke_simple_fun(SMKDSTY_cat5, time_quit_smoking,
                   min_time_quit = min_time_quit_smoking,
                   max_time_quit = max_time_quit_smoking,
                   validate_params = TRUE)
}

# ----------------------------------------------------------------------------
# Smoking Type Classification Function (Modernized)
# ----------------------------------------------------------------------------

#' Type of smokers (unified approach)
#'
#' @description This function creates a derived variable (SMKDSTY_A) for
#' smoker type with 6 categories: daily smoker, current occasional smoker (former daily),
#' current occasional smoker (never daily), current nonsmoker (former daily),
#' current nonsmoker (never daily), nonsmoker
#'
#' @param SMK_005 type of smoker presently
#' @param SMK_030 smoked daily - lifetime (occasional/former smoker)
#' @param SMK_01A smoked 100 or more cigarettes in lifetime
#'
#' @return value for smoker type in the SMKDSTY_A variable
#'
#' @export
SMKDSTY_fun <- function(SMK_005, SMK_030, SMK_01A) {
  # Preprocess original CCHS missing codes before main logic
  SMK_005 <- preprocess_smoking_variable(SMK_005, variable_name = "SMK_005")
  SMK_030 <- preprocess_smoking_variable(SMK_030, variable_name = "SMK_030")
  SMK_01A <- preprocess_smoking_variable(SMK_01A, variable_name = "SMK_01A")
  
  dplyr::case_when(
    SMK_005 == 1 ~ 1L,  # Daily smoker
    
    SMK_005 == 2 & SMK_030 == 1 ~ 2L,  # Occasional smoker (former daily)
    
    SMK_005 == 2 & (SMK_030 == 2 | haven::is_tagged_na(SMK_030, "a") | haven::is_tagged_na(SMK_030, "b")) ~ 3L,  # Occasional smoker (never daily)
    
    SMK_005 == 3 & SMK_030 == 1 ~ 4L,  # Former daily
    
    SMK_005 == 3 & SMK_030 == 2 & SMK_01A == 1 ~ 5L,  # Former occasional
    
    SMK_005 == 3 & SMK_01A == 2 ~ 6L,  # Never smoked
    
    haven::is_tagged_na(SMK_005, "a") ~ haven::tagged_na("a"),
    .default = haven::tagged_na("b")
  )
}

# ----------------------------------------------------------------------------
# Pack-Years Calculation Functions (Modernized)
# ----------------------------------------------------------------------------

#' Smoking pack-years (unified approach)
#'
#' @description This function creates a derived variable (pack_years_der) that
#'  measures an individual's smoking pack-years based on various CCHS smoking
#'  variables. This is a popular variable used by researchers to quantify
#'  lifetime exposure to cigarette use.
#'
#' @param SMKDSTY_A variable used in CCHS cycles 2001-2014 that classifies smoking status
#' @param DHHGAGE_cont continuous age variable
#' @param time_quit_smoking derived variable for time since quitting smoking
#' @param SMKG203_cont age started smoking daily (daily smokers)
#' @param SMKG207_cont age started smoking daily (former daily smokers)
#' @param SMK_204 number of cigarettes smoked per day (daily smokers)
#' @param SMK_05B number of cigarettes smoked per day (occasional smokers)
#' @param SMK_208 number of cigarettes smoked per day (former daily smokers)
#' @param SMK_05C number of days smoked at least one cigarette
#' @param SMKG01C_cont age smoked first cigarette
#' @param SMK_01A smoked 100 cigarettes in lifetime (y/n)
#' @param validate_params logical, enable parameter validation (default FALSE)
#' @param min_DHHGAGE_cont minimum valid age (default 12)
#' @param max_DHHGAGE_cont maximum valid age (default 102)
#' @param min_time_quit_smoking minimum valid time since quitting (default 0.5)
#' @param max_time_quit_smoking maximum valid time since quitting (default 82)
#' @param min_SMKG203_cont minimum valid age started daily (default 5)
#' @param max_SMKG203_cont maximum valid age started daily (default 84)
#' @param min_SMKG207_cont minimum valid age started daily former (default 5)
#' @param max_SMKG207_cont maximum valid age started daily former (default 80)
#' @param min_SMK_204 minimum cigarettes per day daily (default 1)
#' @param max_SMK_204 maximum cigarettes per day daily (default 99)
#' @param min_SMK_05B minimum cigarettes per day occasional (default 1)
#' @param max_SMK_05B maximum cigarettes per day occasional (default 99)
#' @param min_SMK_208 minimum cigarettes per day former (default 1)
#' @param max_SMK_208 maximum cigarettes per day former (default 99)
#' @param min_SMK_05C minimum days smoked (default 0)
#' @param max_SMK_05C maximum days smoked (default 31)
#' @param min_SMKG01C_cont minimum age first cigarette (default 8)
#' @param max_SMKG01C_cont maximum age first cigarette (default 84)
#'
#' @return value for smoking pack-years in the pack_years_der variable
#'
#' @examples
#' # Standard CCHS processing (no validation)
#' pack_years_fun(1, 45, NA, 18, NA, 20, NA, NA, NA, NA, 1)
#'
#' # RDC with validation for data quality
#' pack_years_fun(1, 45, NA, 18, NA, 20, NA, NA, NA, NA, 1,
#'                validate_params = TRUE, min_DHHGAGE_cont = 12, max_DHHGAGE_cont = 102)
#'
#' @references 
#' Holford, T.R., et al. (2014). Patterns of Birth Cohort–Specific Smoking Histories, 1965–2009. 
#' \emph{Am J Prev Med}, 46(2), e31-7. \doi{10.1016/j.amepre.2013.10.022}
#' 
#' Manuel, D.G., et al. (2020). Smoking Patterns Based on Birth-Cohort-Specific Histories from 1965 to 2013, with Projections to 2041. 
#' \emph{Health Reports}, 31(11), 16-31. 
#' \url{https://www150.statcan.gc.ca/n1/pub/82-003-x/2020011/article/00002-eng.htm}
#'
#' @export
pack_years_fun <- function(SMKDSTY_A, DHHGAGE_cont, time_quit_smoking, SMKG203_cont,
                          SMKG207_cont, SMK_204, SMK_05B, SMK_208, SMK_05C, 
                          SMKG01C_cont, SMK_01A,
                          validate_params = FALSE,
                          min_DHHGAGE_cont = 12, max_DHHGAGE_cont = 102,
                          min_time_quit_smoking = 0.5, max_time_quit_smoking = 82,
                          min_SMKG203_cont = 8, max_SMKG203_cont = 84,
                          min_SMKG207_cont = 8, max_SMKG207_cont = 84,
                          min_SMK_204 = 1, max_SMK_204 = 99, 
                          min_SMK_05B = 1, max_SMK_05B = 99,
                          min_SMK_208 = 1, max_SMK_208 = 99, 
                          min_SMK_05C = 0, max_SMK_05C = 31,
                          min_SMKG01C_cont = 8, max_SMKG01C_cont = 84) {
  
  # Preprocess original CCHS missing codes for relevant variables
  SMK_01A <- preprocess_smoking_variable(SMK_01A, variable_name = "SMK_01A")
  SMK_204 <- preprocess_smoking_variable(SMK_204, variable_name = "SMK_204")
  SMK_208 <- preprocess_smoking_variable(SMK_208, variable_name = "SMK_204")  # Same pattern as SMK_204
  SMK_05B <- preprocess_smoking_variable(SMK_05B, variable_name = "SMK_204")  # Same pattern as SMK_204
  SMK_05C <- preprocess_smoking_variable(SMK_05C, variable_name = "SMK_204")  # Same pattern as SMK_204
  
  # Age verification logic (will be integrated into main case_when)
  age_valid <- dplyr::case_when(
    is.na(DHHGAGE_cont) ~ FALSE,
    DHHGAGE_cont < 0 ~ FALSE,
    validate_params & (DHHGAGE_cont < min_DHHGAGE_cont | DHHGAGE_cont > max_DHHGAGE_cont) ~ FALSE,
    .default = TRUE
  )
  
  # Apply validation if requested
  if (validate_params) {
    # Validate all input parameters
    time_quit_smoking <- dplyr::case_when(
      time_quit_smoking < min_time_quit_smoking | time_quit_smoking > max_time_quit_smoking ~ haven::tagged_na("b"),
      TRUE ~ time_quit_smoking
    )
    
    SMKG203_cont <- validate_age_variable(SMKG203_cont, "SMKG203_cont")
    SMKG207_cont <- validate_age_variable(SMKG207_cont, "SMKG207_cont") 
    SMKG01C_cont <- validate_age_variable(SMKG01C_cont, "SMKG01C_cont")
  }
  
  # Calculate pack-years based on smoking status using case_when
  dplyr::case_when(
    # Age validation check first
    !age_valid ~ haven::tagged_na("b"),
    
    # Daily Smoker
    SMKDSTY_A == 1 & (!validate_params | (SMK_204 >= min_SMK_204 & SMK_204 <= max_SMK_204 & SMKG203_cont >= min_SMKG203_cont & SMKG203_cont <= max_SMKG203_cont)) ~ 
      pmax(((DHHGAGE_cont - SMKG203_cont) * (SMK_204 / 20)), 0.0137),
    
    # Occasional Smoker (former daily)
    SMKDSTY_A == 2 & (!validate_params | (all(c(SMKG207_cont, time_quit_smoking, SMK_05B, SMK_208) >= c(min_SMKG207_cont, min_time_quit_smoking, min_SMK_05B, min_SMK_208) & 
                                          c(SMKG207_cont, time_quit_smoking, SMK_05B, SMK_208) <= c(max_SMKG207_cont, max_time_quit_smoking, max_SMK_05B, max_SMK_208)))) ~
      pmax(((DHHGAGE_cont - SMKG207_cont - time_quit_smoking) * (SMK_208 / 20)), 0.0137) + 
      ((pmax((SMK_05B * SMK_05C / 30), 1) / 20) * time_quit_smoking),
    
    # Occasional Smoker (never daily)  
    SMKDSTY_A == 3 & (!validate_params | (SMK_05C >= min_SMK_05C & SMK_05C <= max_SMK_05C & SMKG01C_cont >= min_SMKG01C_cont & SMKG01C_cont <= max_SMKG01C_cont)) ~
      (pmax((SMK_05B * SMK_05C / 30), 1) / 20) * (DHHGAGE_cont - SMKG01C_cont),
    
    # Former daily smoker (non-smoker now)
    SMKDSTY_A == 4 & (!validate_params | (all(c(SMKG207_cont, time_quit_smoking, SMK_208) >= c(min_SMKG207_cont, min_time_quit_smoking, min_SMK_208) & 
                                          c(SMKG207_cont, time_quit_smoking, SMK_208) <= c(max_SMKG207_cont, max_time_quit_smoking, max_SMK_208)))) ~
      pmax(((DHHGAGE_cont - SMKG207_cont - time_quit_smoking) * (SMK_208 / 20)), 0.0137),
    
    # Former occasional smoker (non-smoker now) who smoked at least 100 cigarettes lifetime
    SMKDSTY_A == 5 & SMK_01A == 1 ~ 0.0137,
    
    # Former occasional smoker (non-smoker now) who have not smoked at least 100 cigarettes lifetime
    SMKDSTY_A == 5 & SMK_01A == 2 ~ 0.007,
    
    # Non-smoker
    SMKDSTY_A == 6 ~ 0,
    
    # Account for NA(a)
    
    TRUE ~ haven::tagged_na("b")
  )
}

#' Smoking pack-years (enhanced validation wrapper)
#'
#' @description Enhanced version with comprehensive parameter validation.
#'  Maintained for backward compatibility.
#'
#' @export
pack_years_fun_A <- function(SMKDSTY_A, DHHGAGE_cont, time_quit_smoking, SMKG203_cont,
                            SMKG207_cont, SMK_204, SMK_05B, SMK_208, SMK_05C, 
                            SMKG01C_cont, SMK_01A,
                            min_DHHGAGE_cont = 12, max_DHHGAGE_cont = 102,
                            min_time_quit_smoking = 0.5, max_time_quit_smoking = 82,
                            min_SMKG203_cont = 8, max_SMKG203_cont = 84,
                            min_SMKG207_cont = 8, max_SMKG207_cont = 84,
                            min_SMK_204 = 1, max_SMK_204 = 99, 
                            min_SMK_05B = 1, max_SMK_05B = 99,
                            min_SMK_208 = 1, max_SMK_208 = 99, 
                            min_SMK_05C = 0, max_SMK_05C = 31,
                            min_SMKG01C_cont = 8, max_SMKG01C_cont = 84) {
  
  pack_years_fun(SMKDSTY_A, DHHGAGE_cont, time_quit_smoking, SMKG203_cont,
                 SMKG207_cont, SMK_204, SMK_05B, SMK_208, SMK_05C, 
                 SMKG01C_cont, SMK_01A,
                 validate_params = TRUE,
                 min_DHHGAGE_cont = min_DHHGAGE_cont, max_DHHGAGE_cont = max_DHHGAGE_cont,
                 min_time_quit_smoking = min_time_quit_smoking, max_time_quit_smoking = max_time_quit_smoking,
                 min_SMKG203_cont = min_SMKG203_cont, max_SMKG203_cont = max_SMKG203_cont,
                 min_SMKG207_cont = min_SMKG207_cont, max_SMKG207_cont = max_SMKG207_cont,
                 min_SMK_204 = min_SMK_204, max_SMK_204 = max_SMK_204,
                 min_SMK_05B = min_SMK_05B, max_SMK_05B = max_SMK_05B,
                 min_SMK_208 = min_SMK_208, max_SMK_208 = max_SMK_208,
                 min_SMK_05C = min_SMK_05C, max_SMK_05C = max_SMK_05C,
                 min_SMKG01C_cont = min_SMKG01C_cont, max_SMKG01C_cont = max_SMKG01C_cont)
}

#' Categorical smoking pack-years
#'
#' @description This function creates a categorical derived variable
#' (pack_years_cat) that categorizes smoking pack-years (pack_years_der).
#'
#' @param pack_years_der derived variable that calculates smoking pack-years
#'
#' @return value for pack year categories in the pack_years_cat variable.
#'
#' @export
pack_years_fun_cat <- function(pack_years_der) {
  dplyr::case_when(
    pack_years_der == 0 ~ 1L,
    pack_years_der > 0 & pack_years_der <= 0.01 ~ 2L,
    pack_years_der > 0.01 & pack_years_der <= 3.0 ~ 3L,
    pack_years_der > 3.0 & pack_years_der <= 9.0 ~ 4L,
    pack_years_der > 9.0 & pack_years_der <= 16.2 ~ 5L,
    pack_years_der > 16.2 & pack_years_der <= 25.7 ~ 6L,
    pack_years_der > 25.7 & pack_years_der <= 40.0 ~ 7L,
    pack_years_der > 40.0 ~ 8L,
    haven::is_tagged_na(pack_years_der, "a") ~ haven::tagged_na("a"),
    .default = haven::tagged_na("b")
  )
}

# ----------------------------------------------------------------------------
# Age-Related Smoking Functions (Modernized)
# ----------------------------------------------------------------------------

#' Age started smoking daily - daily/former daily smokers (unified approach)
#'
#' @description This function creates a continuous derived variable
#' (SMKG040_fun) that calculates the approximate age that a daily or former
#' daily smoker began smoking daily.
#'
#' @param SMKG203_cont age started smoking daily (daily smokers)
#' @param SMKG207_cont age started smoking daily (former daily smokers)
#' @param validate_params logical, enable parameter validation (default FALSE)
#' @param min_SMKG203_cont minimum valid age for daily smokers (default 5)
#' @param max_SMKG203_cont maximum valid age for daily smokers (default 84)
#' @param min_SMKG207_cont minimum valid age for former daily (default 5)
#' @param max_SMKG207_cont maximum valid age for former daily (default 80)
#'
#' @return value for age started smoking daily for daily/former daily smokers
#'
#' @examples
#' # Standard CCHS processing
#' SMKG040_fun(c(18, NA), c(NA, 25))  # Returns c(18, 25)
#'
#' # With validation for research data quality
#' SMKG040_fun(c(18, 4), c(NA, 25), validate_params = TRUE)  # Returns c(18, tagged_na("b"))
#'
#' # Copy-paste standalone usage
#' library(dplyr); library(haven)
#' SMKG040_fun(18, NA)  # Works independently
#'
#' @export
SMKG040_fun <- function(SMKG203_cont, SMKG207_cont,
                        validate_params = FALSE,
                        min_SMKG203_cont = 8, max_SMKG203_cont = 84,
                        min_SMKG207_cont = 8, max_SMKG207_cont = 84) {
  
  # Note: SMKG203_cont and SMKG207_cont are typically already processed continuous values
  # from SMKG203_fun() and SMKG207_fun(), but we'll add minimal preprocessing for robustness
  
  # Apply validation if requested
  if (validate_params) {
    SMKG203_cont <- dplyr::case_when(
      !is.na(SMKG203_cont) & (SMKG203_cont < min_SMKG203_cont | SMKG203_cont > max_SMKG203_cont) ~ haven::tagged_na("b"),
      .default = SMKG203_cont
    )
    
    SMKG207_cont <- dplyr::case_when(
      !is.na(SMKG207_cont) & (SMKG207_cont < min_SMKG207_cont | SMKG207_cont > max_SMKG207_cont) ~ haven::tagged_na("b"),
      .default = SMKG207_cont
    )
  }
  
  dplyr::case_when(
    (haven::is_tagged_na(SMKG203_cont, "a") & haven::is_tagged_na(SMKG207_cont, "a")) ~ haven::tagged_na("a"),
    (haven::is_tagged_na(SMKG203_cont, "b") & haven::is_tagged_na(SMKG207_cont, "b")) ~ haven::tagged_na("b"),
    !is.na(SMKG203_cont) ~ SMKG203_cont,
    !is.na(SMKG207_cont) ~ SMKG207_cont,
    .default = haven::tagged_na("b")
  )
}

#' Age started smoking daily - daily/former daily smokers (enhanced validation wrapper)
#'
#' @description Enhanced version with parameter validation.
#'  Maintained for backward compatibility.
#'
#' @export
SMKG040_fun_A <- function(SMKG203_cont, SMKG207_cont,
                          min_SMKG203_cont = 8, max_SMKG203_cont = 84,
                          min_SMKG207_cont = 8, max_SMKG207_cont = 84) {
  SMKG040_fun(SMKG203_cont, SMKG207_cont,
               validate_params = TRUE,
               min_SMKG203_cont = min_SMKG203_cont, max_SMKG203_cont = max_SMKG203_cont,
               min_SMKG207_cont = min_SMKG207_cont, max_SMKG207_cont = max_SMKG207_cont)
}

#' Age started to smoke daily - daily smoker (from combined variable)
#'
#' @description This function creates a continuous derived variable
#' (SMKG203_cont) for age started to smoke daily for daily smokers.
#'
#' @param SMK_005 type of smoker presently
#' @param SMKG040 age started to smoke daily - daily/former daily smoker
#'
#' @return value for continuous age started to smoke daily for daily smokers
#'
#' @export
SMKG203_fun <- function(SMK_005, SMKG040) {
  # Preprocess original CCHS missing codes
  SMK_005 <- preprocess_smoking_variable(SMK_005, variable_name = "SMK_005")
  SMKG040 <- preprocess_smoking_variable(SMKG040, variable_name = "SMKG203")  # SMKG040 uses categorical age pattern
  
  SMKG203 <- dplyr::case_when(
    SMK_005 == 1 ~ SMKG040,
    haven::is_tagged_na(SMK_005, "a") | haven::is_tagged_na(SMKG040, "a") ~ haven::tagged_na("a"),
    .default = haven::tagged_na("b")
  )
  
  # Convert categorical to continuous ages
  dplyr::case_when(
    SMKG203 == 1 ~ 8,
    SMKG203 == 2 ~ 13,
    SMKG203 == 3 ~ 16,
    SMKG203 == 4 ~ 18.5,
    SMKG203 == 5 ~ 22,
    SMKG203 == 6 ~ 27,
    SMKG203 == 7 ~ 32,
    SMKG203 == 8 ~ 37,
    SMKG203 == 9 ~ 42,
    SMKG203 == 10 ~ 47,
    SMKG203 == 11 ~ 55,
    haven::is_tagged_na(SMKG203, "a") ~ haven::tagged_na("a"),
    .default = haven::tagged_na("b")
  )
}

#' Age started to smoke daily - former daily smoker (from combined variable)
#'
#' @description This function creates a continuous derived variable
#' (SMKG207_cont) for age started to smoke daily for former daily smokers.
#'
#' @param SMK_030 smoked daily - lifetime (occasional/former smoker)
#' @param SMKG040 age started to smoke daily - daily/former daily smoker
#'
#' @return value for continuous age started to smoke daily for former daily smokers
#'
#' @export
SMKG207_fun <- function(SMK_030, SMKG040) {
  # Preprocess original CCHS missing codes
  SMK_030 <- preprocess_smoking_variable(SMK_030, variable_name = "SMK_030")
  SMKG040 <- preprocess_smoking_variable(SMKG040, variable_name = "SMKG207")  # SMKG040 uses categorical age pattern
  
  SMKG207 <- dplyr::case_when(
    SMK_030 == 1 ~ SMKG040,
    haven::is_tagged_na(SMK_030, "a") | haven::is_tagged_na(SMKG040, "a") ~ haven::tagged_na("a"),
    .default = haven::tagged_na("b")
  )
  
  # Convert categorical to continuous ages
  dplyr::case_when(
    SMKG207 == 1 ~ 8,
    SMKG207 == 2 ~ 13,
    SMKG207 == 3 ~ 16,
    SMKG207 == 4 ~ 18.5,
    SMKG207 == 5 ~ 22,
    SMKG207 == 6 ~ 27,
    SMKG207 == 7 ~ 32,
    SMKG207 == 8 ~ 37,
    SMKG207 == 9 ~ 42,
    SMKG207 == 10 ~ 47,
    SMKG207 == 11 ~ 55,
    haven::is_tagged_na(SMKG207, "a") ~ haven::tagged_na("a"),
    .default = haven::tagged_na("b")
  )
}

#' Age started to smoke daily - daily smoker (modern CCHS 2015+)
#'
#' @description This function creates a derived variable (SMKG203_cont)
#'  that provides the age started to smoke daily for current daily smokers.
#'
#' @param SMK_005 Type of smoker presently
#' @param SMKG040_I Age started to smoke daily - daily/former daily smoker (continuous)
#'
#' @return the CCHS derived variable SMKG203_cont is a continuous variable
#'
#' @export
SMKG203I_fun <- function(SMK_005, SMKG040_I) {
  SMKG203 <- dplyr::case_when(
    SMK_005 == 1 ~ SMKG040_I,
    haven::is_tagged_na(SMK_005, "a") | haven::is_tagged_na(SMKG040_I, "a") ~ haven::tagged_na("a"),
    TRUE ~ haven::tagged_na("b")
  )

  dplyr::case_when(
    SMKG203 %in% 8:95 ~ SMKG203,
    haven::is_tagged_na(SMKG203, "a") ~ haven::tagged_na("a"),
    TRUE ~ haven::tagged_na("b")
  )
}

#' Age started to smoke daily - former daily smoker (modern CCHS 2015+)
#'
#' @description This function creates a derived variable (SMKG207_cont)
#'  that provides the age started to smoke daily for former daily smokers.
#'
#' @param SMK_030 Smoked daily - lifetime (occasional/former smoker)
#' @param SMKG040_I Age started to smoke daily - daily/former daily smoker (continuous)
#'
#' @return the CCHS derived variable SMKG207_cont is a continuous variable
#'
#' @export
SMKG207I_fun <- function(SMK_030, SMKG040_I) {
  SMKG207 <- dplyr::case_when(
    SMK_030 == 1 ~ SMKG040_I,
    haven::is_tagged_na(SMK_030, "a") | haven::is_tagged_na(SMKG040_I, "a") ~ haven::tagged_na("a"),
    TRUE ~ haven::tagged_na("b")
  )
  
  dplyr::case_when(
    SMKG207 %in% 8:95 ~ SMKG207,
    haven::is_tagged_na(SMKG207, "a") ~ haven::tagged_na("a"),
    TRUE ~ haven::tagged_na("b")
  )
}

# ----------------------------------------------------------------------------
# Modern CCHS Functions (2022+) - Modernized
# ----------------------------------------------------------------------------

#' Time since stopped smoking for former daily smokers (cycle 2022)
#'
#' @description This function creates a derived variable (SPU_25I)
#'  that calculates the approximate time since a former daily smoker has quit smoking based
#'  on 2022 CCHS variables.
#'
#' @param SPU_25A_I stopped smoking daily - month (former daily smoker)
#' @param SPU_25B_I stopped smoking daily - year (former daily smoker)
#' @param ADM_MOI_I month of interview
#' @param ADM_YOI_I year of interview
#'
#' @return the CCHS derived variable SPU_25I is a continuous variable
#'
#' @export
SPU25_fun <- function(SPU_25A_I, SPU_25B_I, ADM_MOI_I, ADM_YOI_I) {
  # Preprocess original CCHS missing codes for modern CCHS variables  
  # Note: 2022+ variables may use different patterns, but we'll handle standard codes
  SPU_25A_I <- preprocess_smoking_variable(SPU_25A_I, pattern_type = "standard_response")
  SPU_25B_I <- preprocess_smoking_variable(SPU_25B_I, pattern_type = "continuous_standard")
  ADM_MOI_I <- preprocess_smoking_variable(ADM_MOI_I, pattern_type = "standard_response")
  ADM_YOI_I <- preprocess_smoking_variable(ADM_YOI_I, pattern_type = "continuous_standard")
  
  # The interview date calculation
  end <- dplyr::case_when(
    (!haven::is_tagged_na(ADM_YOI_I, "a") & !haven::is_tagged_na(ADM_YOI_I, "b")) & (ADM_MOI_I %in% 1:12) ~ 
      as.Date(ISOdate(ADM_YOI_I, ADM_MOI_I, 01)),
    haven::is_tagged_na(ADM_YOI_I, "a") | haven::is_tagged_na(ADM_MOI_I, "a") ~ haven::tagged_na("a"),
    .default = haven::tagged_na("b")
  )
  
  # The quit date calculation
  start <- dplyr::case_when(
    SPU_25B_I == "NA(b)" ~ haven::tagged_na("b"),
    haven::is_tagged_na(SPU_25B_I, "b") ~ haven::tagged_na("b"),
    haven::is_tagged_na(SPU_25B_I, "a") ~ haven::tagged_na("a"),
    SPU_25A_I %in% 1:12 ~ as.Date(ISOdate(SPU_25B_I, SPU_25A_I, 01)),
    .default = as.Date(ISOdate(SPU_25B_I, ADM_MOI_I, 01))
  )
  
  # Calculate years difference
  dplyr::case_when(
    end == "NA(b)" ~ haven::tagged_na("b"),
    haven::is_tagged_na(end, "b") ~ haven::tagged_na("b"),
    start != "NA(a)" & start != "NA(b)" & !haven::is_tagged_na(start, "a") & !haven::is_tagged_na(start, "b") ~ (end - start) / 365.2422,
    .default = start
  )
}

#' Years since stopped smoking for former daily smokers (categorical - version 1)
#'
#' @description This function creates a derived variable (SMK_09A_I)
#'  that calculates the approximate time since a former daily smoker has quit smoking
#'  with categories: half a year, one and a half years, two and a half years, 4 years or more
#'
#' @param SPU_25I Time since stopped smoking for former daily smokers
#'
#' @return the CCHS derived variable SMK_09A_I is a categorical variable
#'
#' @export
SMK09AI_fun <- function(SPU_25I) {
  dplyr::case_when(
    haven::is_tagged_na(SPU_25I, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(SPU_25I, "b") ~ haven::tagged_na("b"),
    SPU_25I < 1 ~ 0.5,
    SPU_25I >= 1 & SPU_25I < 2 ~ 1.5,
    SPU_25I >= 2 & SPU_25I < 3 ~ 2.5,
    TRUE ~ 4
  )
}

#' Years since stopped smoking for former daily smokers (categorical - version 2)
#'
#' @description This function creates a derived variable (SMK_09A_B)
#'  that calculates the approximate time since a former daily smoker has quit smoking
#'  with categories: less than a year, 1 to <2 years, 2 to <3 years, 3 years or more
#'
#' @param SPU_25I Time since stopped smoking for former daily smokers
#'
#' @return the CCHS derived variable SMK_09A_B is a categorical variable
#'
#' @export
SMK09AB_fun <- function(SPU_25I) {
  dplyr::case_when(
    haven::is_tagged_na(SPU_25I, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(SPU_25I, "b") ~ haven::tagged_na("b"),
    SPU_25I < 1 ~ 1,
    SPU_25I >= 1 & SPU_25I < 2 ~ 2,
    SPU_25I >= 2 & SPU_25I < 3 ~ 3,
    TRUE ~ 4
  )
}

#' Years since stopped smoking for former daily smokers (categorical - version 3)
#'
#' @description This function creates a derived variable (SMKG09C)
#'  that calculates the approximate years since a former daily smoker has quit smoking
#'  with categories: 3 to 5 years, 6 to 10 years, 11 years or more
#'
#' @param SPU_25I Time since stopped smoking for former daily smokers
#'
#' @return the CCHS derived variable SMKG09C_I is a categorical variable
#'
#' @export
SMK09C_fun <- function(SPU_25I) {
  dplyr::case_when(
    haven::is_tagged_na(SPU_25I, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(SPU_25I, "b") ~ haven::tagged_na("b"),
    SPU_25I >= 3 & SPU_25I < 6 ~ 1,
    SPU_25I >= 6 & SPU_25I < 11 ~ 2,
    SPU_25I >= 11 ~ 3,
    TRUE ~ haven::tagged_na("a")
  )
}

# ============================================================================
# PHASE 2 COMPLETE: All core functions refactored with case_when()
# Ready for Phase 3: Consolidation and standardization
# ============================================================================