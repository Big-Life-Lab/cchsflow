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

# Smoking initiation age bounds (evidence-based from Holford et al.)
SMOKING_AGE_BOUNDS <- list(
  min_initiation = 8, # Minimum plausible smoking initiation age
  max_initiation = 95, # Maximum plausible smoking initiation age
  min_current_age = 12, # Minimum age for smoking questions
  max_current_age = 102 # Maximum age in CCHS
)

# Time since quit smoking bounds
TIME_QUIT_BOUNDS <- list(
  min = 0.5, # Minimum time since quitting (6 months)
  max = 82 # Maximum plausible time since quitting
)

# Pack-years calculation constants
PACK_YEARS_CONSTANTS <- list(
  cigarettes_per_pack = 20,
  min_pack_years = 0.0137, # Minimum for former occasional smokers
  min_pack_years_never = 0.007 # Minimum for never-daily smokers
)

# Smoking validation bounds for standalone function use
# NOTE: These constants ensure smoking functions work independently ("cut and paste")
# while rec_with_table() uses variable_details.csv for CSV-driven validation.
# IMPORTANT: Keep synchronized with variable_details.csv ground truth values
SMOKING_VALIDATION_BOUNDS <- list(
  # Categorical variables (1-6 scale for smoking status)
  smoking_status = list(min = 1, max = 6), # Daily, occasional, former daily, former occasional, never
  binary_response = list(min = 1, max = 2), # 1 = yes, 2 = no
  # Age variables
  age_initiation = list(min = 8, max = 95), # Age started smoking
  current_age = list(min = 12, max = 102), # Current age bounds
  # Continuous smoking variables
  cigarettes_daily = list(min = 1, max = 80), # Cigarettes per day
  days_monthly = list(min = 1, max = 30), # Days smoked per month
  time_quit = list(min = 0.5, max = 82), # Time since quitting
  pack_years = list(min = 0.001, max = 250) # Pack-years range
)

# ==============================================================================
# 2. SPECIALIZED HELPER FUNCTIONS
# ==============================================================================

#' Core smoking status assessment (internal helper)
#'
#' Vector-aware smoking status assessment without validation - used as building block
#' @param smk_005_clean,smk_030_clean,smk_01a_clean Cleaned smoking variables (already validated)
#' @return Smoking status indicator with proper tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-07-05, status: active - Vector aware
#' @noRd
calculate_smoking_status_core <- function(smk_005_clean, smk_030_clean, smk_01a_clean) {
  # Use case_when for element-wise processing with tagged NA handling
  dplyr::case_when(
    # !!! (splice operator) expands generate_tagged_na_conditions() output for each variable
    !!!generate_tagged_na_conditions(smk_005_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(smk_030_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(smk_01a_clean, categorical_labels = FALSE),

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

#' Core time since quitting calculation (internal helper)
#'
#' Vector-aware time since quitting assessment without validation - used as building block
#' @param smk_09a_b_clean,smkg09c_clean Cleaned time variables (already validated)
#' @return Time since quitting with proper tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-07-05, status: active - Vector aware
#' @noRd
calculate_time_quit_core <- function(smk_09a_b_clean, smkg09c_clean) {
  dplyr::case_when(
    # Calculate time since quitting: continuous variable takes precedence over everything
    !is.na(smkg09c_clean) & smkg09c_clean >= TIME_QUIT_BOUNDS$min & smkg09c_clean <= TIME_QUIT_BOUNDS$max ~ smkg09c_clean,

    # Handle invalid continuous values
    !is.na(smkg09c_clean) & (smkg09c_clean < TIME_QUIT_BOUNDS$min | smkg09c_clean > TIME_QUIT_BOUNDS$max) ~ haven::tagged_na("b"),

    # Handle missing data from categorical variable (only when continuous is NA)
    !!!generate_tagged_na_conditions(smk_09a_b_clean, categorical_labels = FALSE),

    # Categorical variable conversion to continuous (when continuous is NA and categorical is valid)
    smk_09a_b_clean == 1 ~ 0.5, # Less than 1 year
    smk_09a_b_clean == 2 ~ 1.5, # 1-2 years
    smk_09a_b_clean == 3 ~ 4.0, # 3-5 years
    smk_09a_b_clean == 4 ~ 7.5, # 6-10 years
    smk_09a_b_clean == 5 ~ 12.5, # 11-15 years
    smk_09a_b_clean == 6 ~ 20.0, # 16+ years
    .default = haven::tagged_na("b")
  )
}

#' Core pack-years calculation (internal helper)
#'
#' Vector-aware pack-years calculation without validation - used as building block
#' @param smoking_status_clean,current_age_clean,time_quit_clean,age_started_clean,cigarettes_daily_clean Cleaned variables (already validated)
#' @return Pack-years with proper tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-07-05, status: active - Vector aware
#' @noRd
calculate_pack_years_core <- function(smoking_status_clean, current_age_clean, time_quit_clean, age_started_clean, cigarettes_daily_clean) {
  dplyr::case_when(
    # Handle missing data
    !!!generate_tagged_na_conditions(smoking_status_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(current_age_clean, categorical_labels = FALSE),

    # Never smokers
    smoking_status_clean == 6 ~ 0.0,

    # Current daily smokers
    smoking_status_clean == 1 ~ ((current_age_clean - age_started_clean) * cigarettes_daily_clean) / PACK_YEARS_CONSTANTS$cigarettes_per_pack,

    # Former daily smokers
    smoking_status_clean == 4 ~ ((current_age_clean - time_quit_clean - age_started_clean) * cigarettes_daily_clean) / PACK_YEARS_CONSTANTS$cigarettes_per_pack,

    # Occasional smokers (current or former) - minimum pack-years
    smoking_status_clean %in% c(2, 3, 5) ~ PACK_YEARS_CONSTANTS$min_pack_years,
    .default = haven::tagged_na("b")
  )
}

# ==============================================================================
# 3. PUBLIC API FUNCTIONS
# ==============================================================================

#' Assess smoking status with comprehensive classification
#'
#' @description
#' Classify smoking status into 6 categories using consistent CCHS variables across cycles.
#' Provides comprehensive smoking status assessment for population health research and
#' clinical applications using harmonized variable approach.
#'
#' @param SMK_005 Current smoking status (1=daily, 2=occasional, 3=not at all). Accepts raw CCHS codes or preprocessed values.
#' @param SMK_030 Ever smoked daily (1=yes, 2=no). Accepts raw CCHS codes or preprocessed values.
#' @param SMK_01A Ever smoked (1=yes, 2=no). Accepts raw CCHS codes or preprocessed values.
#' @param min_SMK_005,max_SMK_005,min_SMK_030,max_SMK_030,min_SMK_01A,max_SMK_01A Validation parameters (defaults from variable_details.csv)
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Integer smoking status indicator. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Values: 1 = daily smoker, 2 = occasional smoker (former daily), 3 = occasional smoker (never daily),
#'   4 = former daily smoker, 5 = former occasional smoker, 6 = never smoked.
#'
#' @details
#' Based on harmonized CCHS smoking variables available across all cycles. Classification follows
#' Health Canada guidelines for smoking status assessment.
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c("SMK_005", "SMK_030", "SMK_01A", "SMKDSTY_der")
#' )
#'
#' # Scalar usage examples with variable and value labels
#' # calculate_smoking_status(SMK_005,    SMK_030,      SMK_01A)
#' #                      (current,    ever_daily,   ever_smoked)
#' #                      1=daily      1=yes         1=yes
#' #                      2=occasional 2=no          2=no
#' #                      3=not_at_all
#' calculate_smoking_status(1, 1, 1) # Returns: 1L (daily smoker)
#' calculate_smoking_status(2, 1, 1) # Returns: 2L (occasional, former daily)
#' calculate_smoking_status(2, 2, 1) # Returns: 3L (occasional, never daily)
#' calculate_smoking_status(3, 1, 1) # Returns: 4L (former daily)
#' calculate_smoking_status(3, 2, 1) # Returns: 5L (former occasional)
#' calculate_smoking_status(3, 2, 2) # Returns: 6L (never smoked)
#'
#' # Vector examples with real-world scenarios
#' # John (daily), Mary (former daily), Bob (never), Sarah (missing data),
#' # Mike (out of bounds), Lisa (occasional never daily)
#' smoking_scenarios <- data.frame(
#'   person = c("John", "Mary", "Bob", "Sarah", "Mike", "Lisa"),
#'   SMK_005 = c(1, 3, 3, 7, 99, 2), # Current: daily, not_at_all, not_at_all, don't_know, invalid, occasional
#'   SMK_030 = c(1, 1, 2, 8, 1, 2), # Ever daily: yes, yes, no, refusal, yes, no
#'   SMK_01A = c(1, 1, 2, 1, 1, 1), # Ever smoked: yes, yes, no, yes, yes, yes
#'   description = c(
#'     "45-year-old daily smoker since age 20",
#'     "60-year-old quit daily smoking 5 years ago",
#'     "30-year-old never smoked",
#'     "Missing data - participant refused to answer",
#'     "Invalid response code in data",
#'     "25-year-old occasional smoker, never smoked daily"
#'   )
#' )
#'
#' # Calculate smoking status for all scenarios
#' smoking_scenarios$status <- calculate_smoking_status(
#'   smoking_scenarios$SMK_005,
#'   smoking_scenarios$SMK_030,
#'   smoking_scenarios$SMK_01A
#' )
#'
#' # Results: c(1L, 4L, 6L, tagged_na("b"), tagged_na("b"), 3L)
#'
#' @seealso
#' \\code{\\link{calculate_time_quit_smoking}} for time since quitting assessment
#' \\code{\\link{calculate_pack_years}} for pack-years calculation
#'
#' @references
#' Health Canada. (2013). Canadian Tobacco Use Monitoring Survey (CTUMS).
#' Health Canada Controlled Documents.
#'
#' @note v3.0.0, last updated: 2025-07-05, status: active
#'
#' **Testing**: Run comprehensive tests with:
#' \code{library(testthat); source('R/smoking.R'); test_file('tests/testthat/test-smoking-v3.R')}
#'
#' **Development**: Enhanced with comprehensive preprocessing and modern missing data handling
#' @export
calculate_smoking_status <- function(SMK_005, SMK_030, SMK_01A,
                                     # Validation bounds: defaults from variable_details.csv for standalone use
                                     # Use rec_with_table() for full CSV-driven validation workflow
                                     min_SMK_005 = SMOKING_VALIDATION_BOUNDS$smoking_status$min,
                                     max_SMK_005 = SMOKING_VALIDATION_BOUNDS$smoking_status$max,
                                     min_SMK_030 = SMOKING_VALIDATION_BOUNDS$binary_response$min,
                                     max_SMK_030 = SMOKING_VALIDATION_BOUNDS$binary_response$max,
                                     min_SMK_01A = SMOKING_VALIDATION_BOUNDS$binary_response$min,
                                     max_SMK_01A = SMOKING_VALIDATION_BOUNDS$binary_response$max,
                                     validate_params = NULL,
                                     log_level = "silent") {
  # Clean categorical variables (handles missing variables with tagged_na("d"))
  cleaned <- clean_categorical_variables(
    SMK_005 = SMK_005,
    SMK_030 = SMK_030,
    SMK_01A = SMK_01A,
    valid_values = list(
      SMK_005 = min_SMK_005:max_SMK_005,
      SMK_030 = min_SMK_030:max_SMK_030,
      SMK_01A = min_SMK_01A:max_SMK_01A
    ),
    pattern_type = "double_digit_missing", # Use double_digit_missing pattern since smoking status 1-6 are all valid
    log_level = log_level
  )

  # Calculate smoking status from clean inputs
  calculate_smoking_status_core(
    cleaned$SMK_005_clean, cleaned$SMK_030_clean, cleaned$SMK_01A_clean
  )
}

#' Assess time since quitting smoking with comprehensive validation
#'
#' @description
#' Calculate time since quitting smoking using harmonized CCHS variables across cycles.
#' Provides comprehensive time assessment for former smokers using categorical and continuous
#' time variables with automatic conversion to unified continuous scale.
#'
#' @param SMK_09A_B Time since quitting (categorical: 1=<1yr, 2=1-2yrs, 3=3-5yrs, 4=6-10yrs, 5=11-15yrs, 6=16+yrs). Accepts raw CCHS codes or preprocessed values.
#' @param SMKG09C Time since quitting (continuous, in years). Accepts raw CCHS codes or preprocessed values.
#' @param min_SMK_09A_B,max_SMK_09A_B,min_SMKG09C,max_SMKG09C Validation parameters (defaults from variable_details.csv)
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric time in years since quitting. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (current smokers)
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Range: 0.5-82 years (minimum 6 months, maximum plausible time).
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c("SMK_09A_B", "SMKG09C", "time_quit_smoking_der")
#' )
#'
#' # Scalar usage examples with variable and value labels
#' # calculate_time_quit_smoking(SMK_09A_B,    SMKG09C)
#' #                         (categorical,  continuous_years)
#' #                         1=<1yr         (exact years)
#' #                         2=1-2yrs
#' #                         3=3-5yrs
#' #                         4=6-10yrs
#' #                         5=11-15yrs
#' #                         6=16+yrs
#' calculate_time_quit_smoking(1, NA) # Returns: 0.5 (quit <1 year ago)
#' calculate_time_quit_smoking(3, NA) # Returns: 4.0 (quit 3-5 years ago, midpoint)
#' calculate_time_quit_smoking(NA, 2.5) # Returns: 2.5 (continuous takes precedence)
#' calculate_time_quit_smoking(2, 1.8) # Returns: 1.8 (continuous takes precedence)
#'
#' # Vector examples with real-world scenarios
#' # Former smokers with different quit patterns
#' quit_scenarios <- data.frame(
#'   person = c("Anne", "Bob", "Carol", "David", "Eve", "Frank"),
#'   SMK_09A_B = c(2, 4, 6, 7, 6, NA), # Categories: 1-2yrs, 6-10yrs, 16+yrs, missing, not_applicable, missing
#'   SMKG09C = c(NA, NA, 25.0, NA, 996, 5.2), # Continuous: missing, missing, 25yrs, missing, not_applicable, 5.2yrs
#'   description = c(
#'     "52-year-old quit daily smoking 1-2 years ago",
#'     "68-year-old quit daily smoking 6-10 years ago",
#'     "75-year-old quit daily smoking 25 years ago",
#'     "45-year-old former smoker, time unknown",
#'     "35-year-old current smoker (not applicable)",
#'     "50-year-old quit exactly 5.2 years ago (precise measurement)"
#'   )
#' )
#'
#' # Calculate time since quitting for all scenarios
#' quit_scenarios$time_quit <- calculate_time_quit_smoking(
#'   quit_scenarios$SMK_09A_B,
#'   quit_scenarios$SMKG09C
#' )
#'
#' # Results: c(1.5, 7.5, 25.0, tagged_na("b"), tagged_na("a"), 5.2)
#'
#' @note v3.0.0, last updated: 2025-07-05, status: active
#'
#' **Testing**: Run comprehensive tests with:
#' \code{library(testthat); source('R/smoking.R'); test_file('tests/testthat/test-smoking-v3.R')}
#'
#' **Development**: Enhanced with comprehensive preprocessing and modern missing data handling
#' @export
calculate_time_quit_smoking <- function(SMK_09A_B, SMKG09C,
                                        # Validation bounds: defaults from variable_details.csv for standalone use
                                        min_SMK_09A_B = SMOKING_VALIDATION_BOUNDS$binary_response$min,
                                        max_SMK_09A_B = 6,
                                        min_SMKG09C = SMOKING_VALIDATION_BOUNDS$time_quit$min,
                                        max_SMKG09C = SMOKING_VALIDATION_BOUNDS$time_quit$max,
                                        validate_params = NULL,
                                        log_level = "silent") {
  # Clean mixed variables (categorical + continuous)
  cleaned <- clean_variables(
    continuous_vars = list(smkg09c = SMKG09C),
    categorical_vars = list(smk_09a_b = SMK_09A_B),
    min_values = list(smkg09c = min_SMKG09C),
    max_values = list(smkg09c = max_SMKG09C),
    valid_values = list(smk_09a_b = min_SMK_09A_B:max_SMK_09A_B),
    continuous_pattern = "triple_digit_missing",
    categorical_pattern = "single_digit_missing",
    log_level = log_level
  )

  # Calculate time since quitting from clean inputs
  calculate_time_quit_core(cleaned$smk_09a_b_clean, cleaned$smkg09c_clean)
}

#' Calculate pack-years of smoking with comprehensive validation
#'
#' @description
#' Calculate pack-years of smoking exposure using harmonized CCHS variables across cycles.
#' Provides comprehensive pack-years assessment for smoking history analysis using smoking
#' status, duration, and intensity variables with automatic conversions.
#'
#' @param smoking_status Smoking status (1=daily, 2=occasional former daily, 3=occasional never daily, 4=former daily, 5=former occasional, 6=never). Accepts raw CCHS codes or preprocessed values.
#' @param current_age Current age in years. Accepts raw CCHS codes or preprocessed values.
#' @param time_quit Time since quitting (in years). Accepts raw CCHS codes or preprocessed values.
#' @param age_started Age started smoking regularly. Accepts raw CCHS codes or preprocessed values.
#' @param cigarettes_daily Average cigarettes per day. Accepts raw CCHS codes or preprocessed values.
#' @param min_smoking_status,max_smoking_status Validation parameters for smoking status
#' @param min_current_age,max_current_age Validation parameters for current age
#' @param min_time_quit,max_time_quit Validation parameters for time since quitting
#' @param min_age_started,max_age_started Validation parameters for age started
#' @param min_cigarettes_daily,max_cigarettes_daily Validation parameters for cigarettes daily
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Numeric pack-years of smoking exposure. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Range: 0.0-250 pack-years (0.0 for never smokers, minimum values for occasional smokers).
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c("smoking_status", "current_age", "time_quit", "age_started", "cigarettes_daily", "pack_years_der")
#' )
#'
#' # Scalar usage examples with variable and value labels
#' # calculate_pack_years(smoking_status, current_age, time_quit, age_started, cigarettes_daily)
#' #                     (1=daily,       (years),     (years),   (years),     (per_day)
#' #                      2=occ_former,
#' #                      3=occ_never,
#' #                      4=former_daily,
#' #                      5=former_occ,
#' #                      6=never)
#' calculate_pack_years(1, 45, NA, 18, 20) # Returns: 27.0 (45yr old daily, started at 18, 1 pack/day = 27 pack-years)
#' calculate_pack_years(4, 50, 10, 20, 15) # Returns: 15.0 (50yr old quit 10yrs ago, started at 20, 15/day = 15 pack-years)
#' calculate_pack_years(6, 30, 996, 996, 996) # Returns: 0.0 (30yr old never smoker)
#' calculate_pack_years(2, 40, NA, 25, 996) # Returns: 0.0137 (40yr old occasional former daily, minimum exposure)
#'
#' # Vector examples with complex real-world smoking histories
#' # Different smoking patterns with detailed life stories
#' smoking_histories <- data.frame(
#'   person = c("Alice", "Bob", "Carol", "David", "Eve", "Frank"),
#'   smoking_status = c(1, 4, 6, 2, 97, 3), # Status: daily, former_daily, never, occ_former, missing, occ_never
#'   current_age = c(45, 60, 35, 52, 40, 28), # Current ages
#'   time_quit = c(NA, 5, 996, NA, NA, 996), # Years since quit: current, 5yrs, not_applicable, current, missing, not_applicable
#'   age_started = c(18, 16, 996, 22, 20, 996), # Age started: 18, 16, not_applicable, 22, 20, not_applicable
#'   cigarettes_daily = c(25, 20, 996, 996, 15, 996), # Per day: 25, 20, not_applicable, not_applicable, 15, not_applicable
#'   description = c(
#'     "45yr old current daily smoker, started at 18, smokes 25/day (1.25 packs)",
#'     "60yr old former daily smoker, quit 5yrs ago, started at 16, smoked 20/day (1 pack)",
#'     "35yr old never smoker, healthy lifestyle",
#'     "52yr old occasional smoker who formerly smoked daily, started at 22",
#'     "40yr old with missing smoking data, incomplete survey responses",
#'     "28yr old occasional smoker who never smoked daily, social smoking only"
#'   )
#' )
#'
#' # Calculate pack-years for all complex scenarios
#' smoking_histories$pack_years <- calculate_pack_years(
#'   smoking_histories$smoking_status,
#'   smoking_histories$current_age,
#'   smoking_histories$time_quit,
#'   smoking_histories$age_started,
#'   smoking_histories$cigarettes_daily
#' )
#'
#' # Results: c(33.75, 39.0, 0.0, 0.0137, tagged_na("b"), 0.0137)
#' # Alice: (45-18) * 25/20 = 27 * 1.25 = 33.75 pack-years
#' # Bob: (60-5-16) * 20/20 = 39 * 1.0 = 39.0 pack-years
#' # Carol: 0.0 (never smoker)
#' # David: 0.0137 (minimum for occasional former daily)
#' # Eve: tagged_na("b") (missing data)
#' # Frank: 0.0137 (minimum for occasional never daily)
#'
#' @note v3.0.0, last updated: 2025-07-05, status: active
#'
#' **Testing**: Run comprehensive tests with:
#' \code{library(testthat); source('R/smoking.R'); test_file('tests/testthat/test-smoking-v3.R')}
#'
#' **Development**: Enhanced with comprehensive preprocessing and modern missing data handling
#' @export
calculate_pack_years <- function(smoking_status, current_age, time_quit, age_started, cigarettes_daily,
                                 # Validation bounds: defaults from variable_details.csv for standalone use
                                 min_smoking_status = SMOKING_VALIDATION_BOUNDS$smoking_status$min,
                                 max_smoking_status = SMOKING_VALIDATION_BOUNDS$smoking_status$max,
                                 min_current_age = SMOKING_VALIDATION_BOUNDS$current_age$min,
                                 max_current_age = SMOKING_VALIDATION_BOUNDS$current_age$max,
                                 min_time_quit = SMOKING_VALIDATION_BOUNDS$time_quit$min,
                                 max_time_quit = SMOKING_VALIDATION_BOUNDS$time_quit$max,
                                 min_age_started = SMOKING_VALIDATION_BOUNDS$age_initiation$min,
                                 max_age_started = SMOKING_VALIDATION_BOUNDS$age_initiation$max,
                                 min_cigarettes_daily = SMOKING_VALIDATION_BOUNDS$cigarettes_daily$min,
                                 max_cigarettes_daily = SMOKING_VALIDATION_BOUNDS$cigarettes_daily$max,
                                 validate_params = NULL,
                                 log_level = "silent") {
  # Clean mixed variables (categorical + continuous)
  cleaned <- clean_variables(
    continuous_vars = list(
      current_age = current_age,
      time_quit = time_quit,
      age_started = age_started,
      cigarettes_daily = cigarettes_daily
    ),
    categorical_vars = list(smoking_status = smoking_status),
    min_values = list(
      current_age = min_current_age,
      time_quit = min_time_quit,
      age_started = min_age_started,
      cigarettes_daily = min_cigarettes_daily
    ),
    max_values = list(
      current_age = max_current_age,
      time_quit = max_time_quit,
      age_started = max_age_started,
      cigarettes_daily = max_cigarettes_daily
    ),
    valid_values = list(smoking_status = min_smoking_status:max_smoking_status),
    continuous_pattern = "triple_digit_missing",
    categorical_pattern = "double_digit_missing", # Use double_digit_missing pattern since smoking status 1-6 are all valid
    log_level = log_level
  )

  # Calculate pack-years from clean inputs
  calculate_pack_years_core(
    cleaned$smoking_status_clean, cleaned$current_age_clean, cleaned$time_quit_clean,
    cleaned$age_started_clean, cleaned$cigarettes_daily_clean
  )
}

#' Core simple smoking status assessment (internal helper)
#'
#' Vector-aware simple smoking status assessment without validation - used as building block
#' @param smkdsty_cat5_clean,time_quit_smoking_clean Cleaned smoking variables (already validated)
#' @return Simple smoking status indicator with proper tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-07-05, status: active - Vector aware
#' @noRd
smoke_simple_core <- function(smkdsty_cat5_clean, time_quit_smoking_clean) {
  # Use case_when for element-wise processing with tagged NA handling
  dplyr::case_when(
    # Handle missing data first
    haven::is_tagged_na(smkdsty_cat5_clean, "c") ~ haven::tagged_na("c"), # Question not asked
    haven::is_tagged_na(smkdsty_cat5_clean, "d") ~ haven::tagged_na("d"), # Variable missing
    haven::is_tagged_na(smkdsty_cat5_clean, "a") ~ haven::tagged_na("a"), # Not applicable
    haven::is_tagged_na(smkdsty_cat5_clean, "b") ~ haven::tagged_na("b"), # Missing/unknown
    haven::is_tagged_na(time_quit_smoking_clean, "c") ~ haven::tagged_na("c"), # Question not asked
    haven::is_tagged_na(time_quit_smoking_clean, "d") ~ haven::tagged_na("d"), # Variable missing
    haven::is_tagged_na(time_quit_smoking_clean, "a") ~ haven::tagged_na("a"), # Not applicable
    haven::is_tagged_na(time_quit_smoking_clean, "b") ~ haven::tagged_na("b"), # Missing/unknown

    # Simple smoking status classification logic
    smkdsty_cat5_clean == 5 ~ 0L, # Never smoked
    smkdsty_cat5_clean %in% c(1, 2) ~ 1L, # Current smoker (daily and occasional)
    smkdsty_cat5_clean == 4 ~ 2L, # Former occasional smoker
    smkdsty_cat5_clean == 3 & !is.na(time_quit_smoking_clean) & time_quit_smoking_clean < 5 ~ 2L, # Former daily smoker quit <5 years
    smkdsty_cat5_clean == 3 & !is.na(time_quit_smoking_clean) & time_quit_smoking_clean >= 5 ~ 3L, # Former daily smoker quit >=5 years
    smkdsty_cat5_clean == 3 & (is.na(time_quit_smoking_clean) | haven::is_tagged_na(time_quit_smoking_clean)) ~ 2L, # Former daily smoker, time unknown - default to category 2

    # Default to missing for any unhandled cases
    .default = haven::tagged_na("b")
  )
}

#' Assess simple smoking status with comprehensive classification
#'
#' @description
#' Classify smoking status into 4 simplified categories using harmonized CCHS variables across cycles.
#' Provides simplified smoking status assessment for population health research by combining
#' detailed smoking status with time since quitting information.
#'
#' @param SMKDSTY_cat5 5-category smoking status (1=daily, 2=occasional, 3=former daily, 4=former occasional, 5=never). Accepts raw CCHS codes or preprocessed values.
#' @param time_quit_smoking Time since quitting smoking (continuous, in years). Accepts raw CCHS codes or preprocessed values.
#' @param min_SMKDSTY_cat5,max_SMKDSTY_cat5 Validation parameters for smoking status (defaults from variable_details.csv)
#' @param min_time_quit_smoking,max_time_quit_smoking Validation parameters for time since quitting (defaults from variable_details.csv)
#' @param validate_params Auto-detect validation mode (default NULL for auto-detection)
#' @param log_level Logging level: "silent" (default), "warning", "verbose"
#'
#' @return Integer simple smoking status indicator. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Values: 0 = non-smoker (never smoked), 1 = current smoker (daily and occasional),
#'   2 = former daily smoker quit <5 years or former occasional smoker, 3 = former daily smoker quit >=5 years.
#'
#' @details
#' Based on harmonized CCHS smoking variables available across all cycles. Classification simplifies
#' detailed smoking status into 4 categories commonly used in health research and policy analysis.
#'
#' **Classification Logic:**
#' - Category 0: Never smoked (SMKDSTY_cat5 = 5)
#' - Category 1: Current smoker (SMKDSTY_cat5 = 1 or 2)
#' - Category 2: Former daily smoker quit <5 years (SMKDSTY_cat5 = 3 and time_quit_smoking < 5) OR former occasional smoker (SMKDSTY_cat5 = 4)
#' - Category 3: Former daily smoker quit >=5 years (SMKDSTY_cat5 = 3 and time_quit_smoking >= 5)
#' - When time since quitting is missing for former daily smokers, defaults to category 2
#'
#' @examples
#' # Standard cchsflow workflow (primary usage - recommended)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c("SMKDSTY_cat5", "time_quit_smoking", "smoke_simple")
#' )
#'
#' # Scalar usage examples with variable and value labels
#' # smoke_simple_fun(SMKDSTY_cat5, time_quit_smoking)
#' #                  (smoking_status, years_since_quit)
#' #                  1=daily         (continuous)
#' #                  2=occasional
#' #                  3=former_daily
#' #                  4=former_occ
#' #                  5=never
#' smoke_simple_fun(5, NA) # Returns: 0L (never smoked)
#' smoke_simple_fun(1, NA) # Returns: 1L (current daily smoker)
#' smoke_simple_fun(2, NA) # Returns: 1L (current occasional smoker)
#' smoke_simple_fun(4, 2.5) # Returns: 2L (former occasional smoker)
#' smoke_simple_fun(3, 3.0) # Returns: 2L (former daily smoker quit <5 years)
#' smoke_simple_fun(3, 10.0) # Returns: 3L (former daily smoker quit >5 years)
#' smoke_simple_fun(3, NA) # Returns: 2L (former daily smoker, time unknown)
#'
#' # Vector examples with real-world scenarios
#' # Mixed smoking histories with different quit patterns
#' smoking_scenarios <- data.frame(
#'   person = c("Alice", "Bob", "Carol", "David", "Eve", "Frank"),
#'   SMKDSTY_cat5 = c(5, 1, 2, 3, 3, 4), # Status: never, daily, occasional, former_daily, former_daily, former_occ
#'   time_quit_smoking = c(NA, NA, NA, 2.5, 8.0, 1.5), # Years quit: n/a, n/a, n/a, 2.5yrs, 8yrs, 1.5yrs
#'   description = c(
#'     "30-year-old never smoked",
#'     "45-year-old current daily smoker",
#'     "52-year-old current occasional smoker",
#'     "60-year-old former daily smoker, quit 2.5 years ago",
#'     "65-year-old former daily smoker, quit 8 years ago",
#'     "38-year-old former occasional smoker, quit 1.5 years ago"
#'   )
#' )
#'
#' # Calculate simple smoking status for all scenarios
#' smoking_scenarios$smoke_simple <- smoke_simple_fun(
#'   smoking_scenarios$SMKDSTY_cat5,
#'   smoking_scenarios$time_quit_smoking
#' )
#'
#' # Results: c(0L, 1L, 1L, 2L, 3L, 2L)
#' # Alice: 0 (never smoked)
#' # Bob: 1 (current daily smoker)
#' # Carol: 1 (current occasional smoker)
#' # David: 2 (former daily smoker, quit <5 years)
#' # Eve: 3 (former daily smoker, quit >5 years)
#' # Frank: 2 (former occasional smoker)
#'
#' @seealso
#' \\code{\\link{calculate_smoking_status}} for detailed smoking status assessment
#' \\code{\\link{calculate_time_quit_smoking}} for time since quitting assessment
#'
#' @references
#' Health Canada. (2013). Canadian Tobacco Use Monitoring Survey (CTUMS).
#' Health Canada Controlled Documents.
#'
#' @note v3.0.0, last updated: 2025-07-05, status: active
#'
#' **Testing**: Run comprehensive tests with:
#' \code{library(testthat); source('R/smoking.R'); test_file('tests/testthat/test-smoking-v3.R')}
#'
#' **Development**: Enhanced with comprehensive preprocessing and modern missing data handling
#' @export
calculate_smoke_simple <- function(SMKDSTY_cat5, time_quit_smoking,
                                   # Validation bounds: defaults from variable_details.csv for standalone use
                                   min_SMKDSTY_cat5 = 1,
                                   max_SMKDSTY_cat5 = 5,
                                   min_time_quit_smoking = SMOKING_VALIDATION_BOUNDS$time_quit$min,
                                   max_time_quit_smoking = SMOKING_VALIDATION_BOUNDS$time_quit$max,
                                   validate_params = NULL,
                                   log_level = "silent") {
  # Use simple preprocessing approach to ensure reliability
  # The clean_variables function integration can be done later when helper functions are stable

  # Simple preprocessing for missing data codes
  smkdsty_cat5_clean <- SMKDSTY_cat5
  time_quit_smoking_clean <- time_quit_smoking

  # Convert common CCHS missing codes to tagged_na
  # For SMKDSTY_cat5 (double digit missing pattern)
  if (is.numeric(smkdsty_cat5_clean)) {
    smkdsty_cat5_clean[smkdsty_cat5_clean == 96] <- haven::tagged_na("a")
    smkdsty_cat5_clean[smkdsty_cat5_clean == 97] <- haven::tagged_na("b")
    smkdsty_cat5_clean[smkdsty_cat5_clean == 98] <- haven::tagged_na("b")
    smkdsty_cat5_clean[smkdsty_cat5_clean == 99] <- haven::tagged_na("b")
    # Also handle out-of-bounds values
    smkdsty_cat5_clean[smkdsty_cat5_clean < min_SMKDSTY_cat5 | smkdsty_cat5_clean > max_SMKDSTY_cat5] <- haven::tagged_na("b")
  }

  # For time_quit_smoking (triple digit missing pattern)
  if (is.numeric(time_quit_smoking_clean)) {
    time_quit_smoking_clean[time_quit_smoking_clean == 996] <- haven::tagged_na("a")
    time_quit_smoking_clean[time_quit_smoking_clean == 997] <- haven::tagged_na("b")
    time_quit_smoking_clean[time_quit_smoking_clean == 998] <- haven::tagged_na("b")
    time_quit_smoking_clean[time_quit_smoking_clean == 999] <- haven::tagged_na("b")
    # Also handle out-of-bounds values
    time_quit_smoking_clean[time_quit_smoking_clean < min_time_quit_smoking | time_quit_smoking_clean > max_time_quit_smoking] <- haven::tagged_na("b")
  }

  # Calculate simple smoking status from clean inputs
  smoke_simple_core(smkdsty_cat5_clean, time_quit_smoking_clean)
}
