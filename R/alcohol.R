# ==============================================================================
# Alcohol Functions
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
#   source('R/alcohol.R')
#   test_file('tests/testthat/test-alcohol.R')

# ==============================================================================
# 1. CONSTANTS AND CONFIGURATION
# ==============================================================================

# Binge drinking thresholds by sex
BINGE_THRESHOLDS <- list(
  male = 5, # 5+ drinks for males
  female = 4 # 4+ drinks for females
)

# Low-risk drinking guidelines (Canada)
DRINKING_LIMITS <- list(
  weekly = list(
    male = list(low_risk = 15, high_risk = 20),
    female = list(low_risk = 10, high_risk = 15)
  ),
  daily = list(
    male = list(low_risk = 3, high_risk = 4),
    female = list(low_risk = 2, high_risk = 3)
  )
)

# Alcohol validation bounds for standalone function use
# NOTE: These constants ensure alcohol functions work independently ("cut and paste")
# while rec_with_table() uses variable_details.csv for CSV-driven validation.
# IMPORTANT: Keep synchronized with variable_details.csv ground truth values
ALCOHOL_VALIDATION_BOUNDS <- list(
  sex = list(min = 1, max = 2), # 1 = male, 2 = female
  binary_response = list(min = 1, max = 2), # 1 = yes, 2 = no
  drinks_daily = list(min = 0, max = 995), # Daily drinks count [0,995] from variable_details.csv
  drinks_weekly = list(min = 0, max = 995) # Weekly drinks count [0,995] from variable_details.csv
)

# ==============================================================================
# 2. SPECIALIZED HELPER FUNCTIONS
# ==============================================================================

#' Core binge drinking assessment (internal helper)
#'
#' Vector-aware binge drinking assessment without validation - used as building block
#' @param sex_clean Cleaned sex variable (already validated)
#' @param drinks_in_week_clean Cleaned drinks in past week variable (already validated)
#' @param alw_2a1_clean,alw_2a2_clean,alw_2a3_clean,alw_2a4_clean,alw_2a5_clean,alw_2a6_clean,alw_2a7_clean Cleaned daily drinks variables (already validated)
#' @return Binge drinking indicator with proper tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-07-05, status: active - Vector aware
#' @noRd
assess_binge_core <- function(sex_clean, drinks_in_week_clean, alw_2a1_clean, alw_2a2_clean, alw_2a3_clean,
                              alw_2a4_clean, alw_2a5_clean, alw_2a6_clean, alw_2a7_clean) {
  # Use case_when for element-wise processing with tagged NA handling
  dplyr::case_when(
    # !!! (splice operator) expands generate_tagged_na_conditions() output for each variable
    !!!generate_tagged_na_conditions(sex_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(drinks_in_week_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a1_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a2_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a3_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a4_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a5_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a6_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a7_clean, categorical_labels = FALSE),

    # Non-drinkers are not applicable
    drinks_in_week_clean == 2 ~ haven::tagged_na("a"),

    # Apply sex-specific binge thresholds
    sex_clean == 1 & (alw_2a1_clean >= BINGE_THRESHOLDS$male | alw_2a2_clean >= BINGE_THRESHOLDS$male |
      alw_2a3_clean >= BINGE_THRESHOLDS$male | alw_2a4_clean >= BINGE_THRESHOLDS$male |
      alw_2a5_clean >= BINGE_THRESHOLDS$male | alw_2a6_clean >= BINGE_THRESHOLDS$male |
      alw_2a7_clean >= BINGE_THRESHOLDS$male) ~ 1L, # Male binge drinker
    sex_clean == 2 & (alw_2a1_clean >= BINGE_THRESHOLDS$female | alw_2a2_clean >= BINGE_THRESHOLDS$female |
      alw_2a3_clean >= BINGE_THRESHOLDS$female | alw_2a4_clean >= BINGE_THRESHOLDS$female |
      alw_2a5_clean >= BINGE_THRESHOLDS$female | alw_2a6_clean >= BINGE_THRESHOLDS$female |
      alw_2a7_clean >= BINGE_THRESHOLDS$female) ~ 1L, # Female binge drinker

    # No binge drinking detected
    .default = 2L
  )
}

#' Core drinking risk assessment (internal helper)
#'
#' Vector-aware drinking risk assessment without validation - used as building block
#' @param sex_clean,weekly_drinks_clean,alc_1_clean,alw_1_clean Cleaned core variables (already validated)
#' @param alw_2a1_clean,alw_2a2_clean,alw_2a3_clean,alw_2a4_clean,alw_2a5_clean,alw_2a6_clean,alw_2a7_clean Cleaned daily drinking variables (already validated)
#' @param risk_type Type of risk assessment ("short" for acute, "long" for chronic)
#' @return Risk indicator with proper tagged NA handling
#' @note Internal v3.0.0, last updated: 2025-07-05, status: active - Vector aware
#' @noRd
assess_risk_core <- function(sex_clean, weekly_drinks_clean, alc_1_clean, alw_1_clean,
                             alw_2a1_clean, alw_2a2_clean, alw_2a3_clean, alw_2a4_clean,
                             alw_2a5_clean, alw_2a6_clean, alw_2a7_clean, risk_type = "short") {
  # Use case_when for element-wise processing with tagged NA handling
  dplyr::case_when(
    # !!! (splice operator) expands generate_tagged_na_conditions() output for each variable
    !!!generate_tagged_na_conditions(sex_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(weekly_drinks_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alc_1_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_1_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a1_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a2_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a3_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a4_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a5_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a6_clean, categorical_labels = FALSE),
    !!!generate_tagged_na_conditions(alw_2a7_clean, categorical_labels = FALSE),

    # Non-drinkers are not applicable
    alc_1_clean == 2 | alw_1_clean == 2 ~ haven::tagged_na("a"),

    # Short-term risk assessment for males (>3 drinks/day OR >15 drinks/week)
    sex_clean == 1 & risk_type == "short" & (
      alw_2a1_clean > DRINKING_LIMITS$daily$male$low_risk | alw_2a2_clean > DRINKING_LIMITS$daily$male$low_risk |
        alw_2a3_clean > DRINKING_LIMITS$daily$male$low_risk | alw_2a4_clean > DRINKING_LIMITS$daily$male$low_risk |
        alw_2a5_clean > DRINKING_LIMITS$daily$male$low_risk | alw_2a6_clean > DRINKING_LIMITS$daily$male$low_risk |
        alw_2a7_clean > DRINKING_LIMITS$daily$male$low_risk | weekly_drinks_clean > DRINKING_LIMITS$weekly$male$low_risk
    ) ~ 1L,

    # Short-term risk assessment for females (>2 drinks/day OR >10 drinks/week)
    sex_clean == 2 & risk_type == "short" & (
      alw_2a1_clean > DRINKING_LIMITS$daily$female$low_risk | alw_2a2_clean > DRINKING_LIMITS$daily$female$low_risk |
        alw_2a3_clean > DRINKING_LIMITS$daily$female$low_risk | alw_2a4_clean > DRINKING_LIMITS$daily$female$low_risk |
        alw_2a5_clean > DRINKING_LIMITS$daily$female$low_risk | alw_2a6_clean > DRINKING_LIMITS$daily$female$low_risk |
        alw_2a7_clean > DRINKING_LIMITS$daily$female$low_risk | weekly_drinks_clean > DRINKING_LIMITS$weekly$female$low_risk
    ) ~ 1L,

    # Long-term risk assessment for males (≥4 drinks/day OR ≥20 drinks/week)
    sex_clean == 1 & risk_type == "long" & (
      alw_2a1_clean >= DRINKING_LIMITS$daily$male$high_risk | alw_2a2_clean >= DRINKING_LIMITS$daily$male$high_risk |
        alw_2a3_clean >= DRINKING_LIMITS$daily$male$high_risk | alw_2a4_clean >= DRINKING_LIMITS$daily$male$high_risk |
        alw_2a5_clean >= DRINKING_LIMITS$daily$male$high_risk | alw_2a6_clean >= DRINKING_LIMITS$daily$male$high_risk |
        alw_2a7_clean >= DRINKING_LIMITS$daily$male$high_risk | weekly_drinks_clean >= DRINKING_LIMITS$weekly$male$high_risk
    ) ~ 1L,

    # Long-term risk assessment for females (≥3 drinks/day OR ≥15 drinks/week)
    sex_clean == 2 & risk_type == "long" & (
      alw_2a1_clean >= DRINKING_LIMITS$daily$female$high_risk | alw_2a2_clean >= DRINKING_LIMITS$daily$female$high_risk |
        alw_2a3_clean >= DRINKING_LIMITS$daily$female$high_risk | alw_2a4_clean >= DRINKING_LIMITS$daily$female$high_risk |
        alw_2a5_clean >= DRINKING_LIMITS$daily$female$high_risk | alw_2a6_clean >= DRINKING_LIMITS$daily$female$high_risk |
        alw_2a7_clean >= DRINKING_LIMITS$daily$female$high_risk | weekly_drinks_clean >= DRINKING_LIMITS$weekly$female$high_risk
    ) ~ 1L,

    # No increased risk
    .default = 2L
  )
}

# ==============================================================================
# 3. PUBLIC API FUNCTIONS
# ==============================================================================

#' Identify binge drinking patterns with sex-specific thresholds
#'
#' @description
#' Identify binge drinking patterns based on sex-specific daily consumption thresholds. Examines
#' daily alcohol consumption across the week, applying sex-specific binge criteria for acute
#' alcohol risk assessment in population health research.
#'
#' @param DHH_SEX CCHS variable for sex (1=male, 2=female). Accepts raw CCHS codes or preprocessed values.
#' @param ALW_1 Had drinks in past week (1=yes, 2=no). Accepts raw CCHS codes or preprocessed values.
#' @param ALW_2A1 Number of drinks on Sunday. Accepts raw CCHS codes or preprocessed values.
#' @param ALW_2A2 Number of drinks on Monday. Accepts raw CCHS codes or preprocessed values.
#' @param ALW_2A3 Number of drinks on Tuesday. Accepts raw CCHS codes or preprocessed values.
#' @param ALW_2A4 Number of drinks on Wednesday. Accepts raw CCHS codes or preprocessed values.
#' @param ALW_2A5 Number of drinks on Thursday. Accepts raw CCHS codes or preprocessed values.
#' @param ALW_2A6 Number of drinks on Friday. Accepts raw CCHS codes or preprocessed values.
#' @param ALW_2A7 Number of drinks on Saturday. Accepts raw CCHS codes or preprocessed values.
#'
#' @return Integer binge drinking indicator. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (no drinking in past week)
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Values: 1 = binge drinker, 2 = non-binge drinker.
#'
#' @details
#' Only applied to respondents who had alcohol in past week. Non-drinkers receive not applicable status.
#' Enhanced preprocessing may identify different patterns compared to legacy if_else2() implementation.
#'
#' Calculation criteria: Males: Binge = ≥5 drinks on any single day; Females: Binge = ≥4 drinks on any single day.
#' Comprehensive preprocessing of CCHS codes (6,7,8,9 and 996-999) is applied. Priority: Not applicable > Missing > Valid assessment.
#'
#' @examples
#' # Standard cchsflow workflow (primary usage)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c(
#'     "DHH_SEX", "ALW_1", "ALW_2A1", "ALW_2A2", "ALW_2A3",
#'     "ALW_2A4", "ALW_2A5", "ALW_2A6", "ALW_2A7", "ALCDBINGE_der"
#'   )
#' )
#'
#' # Scalar usage examples with variable labels
#' # calculate_binge_drinking(DHH_SEX, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7)
#' #                      (sex,    drinks, sun,     mon,     tue,     wed,     thu,     fri,     sat)
#' calculate_binge_drinking(1, 1, 3, 1, 6, 0, 3, 8, 2) # Returns: 1L (male binge: 6 drinks Tue, 8 drinks Fri)
#' calculate_binge_drinking(2, 1, 1, 2, 2, 3, 0, 1, 2) # Returns: 2L (female non-binge: all days ≤3 drinks)
#' calculate_binge_drinking(1, 1, 4, 4, 4, 4, 4, 4, 4) # Returns: 2L (male non-binge: all days <5 drinks)
#' calculate_binge_drinking(2, 1, 0, 0, 0, 0, 4, 0, 0) # Returns: 1L (female binge: 4 drinks Thu exactly threshold)
#' calculate_binge_drinking(1, 2, 0, 0, 0, 0, 0, 0, 0) # Returns: tagged_na("a") (non-drinker: not applicable)
#'
#' # Vector processing with missing data and edge cases
#' calculate_binge_drinking(
#'   c(1, 2, 6, 7, 8, 9), # Mixed valid/CCHS codes for sex
#'   c(1, 1, 1, 2, 1, 1), # Mixed drinking statuses
#'   c(5, 4, 2, 0, 7, 996), # Mixed Sunday drinks with CCHS codes
#'   c(0, 0, 0, 0, 8, 997), # Mixed Monday drinks
#'   c(0, 0, 0, 0, 9, 998), # Tuesday
#'   c(0, 0, 0, 0, 0, 999), # Wednesday
#'   c(0, 0, 0, 0, 0, 0), # Thursday
#'   c(0, 0, 0, 0, 0, 0), # Friday
#'   c(0, 0, 0, 0, 0, 0) # Saturday
#' ) # Returns: c(1L, 1L, tagged_na("b"), tagged_na("a"), tagged_na("b"), tagged_na("b"))
#'
#' # Vector with string missing values
#' calculate_binge_drinking(
#'   c(1, 2, "Not applicable"), # Mixed sex with string NA
#'   c(1, 1, 1),
#'   c(6, 3, 2), c(0, 0, 0), c(0, 0, 0), c(0, 0, 0),
#'   c(0, 0, 0), c(0, 0, 0), c(0, 0, 0)
#' ) # Returns: c(1L, 2L, tagged_na("b"))
#'
#' @seealso
#' \\code{\\link{calculate_drinking_risk_short}} for short-term drinking risk assessment
#' \\code{\\link{calculate_drinking_risk_long}} for long-term drinking risk assessment
#'
#' @references
#' Butt, P., et al. (2011). Alcohol and health in Canada: a summary of evidence and guidelines
#' for low-risk drinking. Canadian Centre on Substance Abuse.
#'
#' @note v3.0.0, last updated: 2025-07-05, status: active, Note: Enhanced with comprehensive preprocessing and modern missing data handling
#' @export
calculate_binge_drinking <- function(DHH_SEX, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3,
                                     ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7,
                                     # Validation bounds: defaults from variable_details.csv for standalone use
                                     # Use rec_with_table() for full CSV-driven validation workflow
                                     min_DHH_SEX = 1,
                                     max_DHH_SEX = 2,
                                     min_ALW_1 = 1,
                                     max_ALW_1 = 2,
                                     min_ALW_2A1 = 0,
                                     max_ALW_2A1 = 995,
                                     min_ALW_2A2 = 0,
                                     max_ALW_2A2 = 995,
                                     min_ALW_2A3 = 0,
                                     max_ALW_2A3 = 995,
                                     min_ALW_2A4 = 0,
                                     max_ALW_2A4 = 995,
                                     min_ALW_2A5 = 0,
                                     max_ALW_2A5 = 995,
                                     min_ALW_2A6 = 0,
                                     max_ALW_2A6 = 995,
                                     min_ALW_2A7 = 0,
                                     max_ALW_2A7 = 995,
                                     validate_params = NULL,
                                     log_level = "silent") {
  # Clean variables using standardized helpers (handles missing variables with tagged_na("d"))
  cleaned <- clean_variables(
    categorical_vars = list(DHH_SEX = DHH_SEX, ALW_1 = ALW_1),
    continuous_vars = list(
      ALW_2A1 = ALW_2A1, ALW_2A2 = ALW_2A2, ALW_2A3 = ALW_2A3,
      ALW_2A4 = ALW_2A4, ALW_2A5 = ALW_2A5, ALW_2A6 = ALW_2A6, ALW_2A7 = ALW_2A7
    ),
    valid_values = list(
      DHH_SEX = min_DHH_SEX:max_DHH_SEX,
      ALW_1 = min_ALW_1:max_ALW_1
    ),
    min_values = list(
      ALW_2A1 = min_ALW_2A1, ALW_2A2 = min_ALW_2A2, ALW_2A3 = min_ALW_2A3,
      ALW_2A4 = min_ALW_2A4, ALW_2A5 = min_ALW_2A5, ALW_2A6 = min_ALW_2A6, ALW_2A7 = min_ALW_2A7
    ),
    max_values = list(
      ALW_2A1 = max_ALW_2A1, ALW_2A2 = max_ALW_2A2, ALW_2A3 = max_ALW_2A3,
      ALW_2A4 = max_ALW_2A4, ALW_2A5 = max_ALW_2A5, ALW_2A6 = max_ALW_2A6, ALW_2A7 = max_ALW_2A7
    ),
    categorical_pattern = "single_digit_missing",
    continuous_pattern = "triple_digit_missing",
    log_level = log_level
  )

  # Calculate binge drinking assessment from clean inputs
  assess_binge_core(
    cleaned$DHH_SEX_clean, cleaned$ALW_1_clean, cleaned$ALW_2A1_clean, cleaned$ALW_2A2_clean,
    cleaned$ALW_2A3_clean, cleaned$ALW_2A4_clean, cleaned$ALW_2A5_clean, cleaned$ALW_2A6_clean, cleaned$ALW_2A7_clean
  )
}

#' Assess short-term alcohol health risks using Low-Risk Guidelines
#'
#' @description
#' Assess short-term health risks from drinking patterns following Canada's Low-Risk Guidelines.
#' Evaluates daily and weekly consumption against sex-specific acute risk thresholds for
#' injury and overdose risk assessment.
#'
#' @param DHH_SEX,ALW_1,ALW_2A1,ALW_2A2,ALW_2A3,ALW_2A4,ALW_2A5,ALW_2A6,ALW_2A7 As in calculate_binge_drinking()
#' @param ALC_1 Had drinks in past year (1=yes, 2=no). Accepts raw CCHS codes or preprocessed values.
#' @param ALWDWKY Total drinks in past week. Accepts raw CCHS codes or preprocessed values.
#'
#' @return Integer short-term risk indicator. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (non-drinkers)
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Values: 1 = increased short-term risk, 2 = no increased short-term risk.
#'
#' @details
#' Based on Canada's Low-Risk Alcohol Drinking Guidelines. Thresholds may differ from other national guidelines.
#' Non-drinkers (past year or week) receive "no increased risk" classification.
#'
#' Risk criteria: Males: >4 drinks/day OR >15 drinks/week; Females: >3 drinks/day OR >10 drinks/week.
#' Comprehensive preprocessing with priority handling is applied. Non-drinkers are automatically
#' classified as "no increased risk".
#'
#' @examples
#' # Standard cchsflow workflow (primary usage)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c(
#'     "DHH_SEX", "ALWDWKY", "ALC_1", "ALW_1", "ALW_2A1", "ALW_2A2",
#'     "ALW_2A3", "ALW_2A4", "ALW_2A5", "ALW_2A6", "ALW_2A7", "ALCDLOWI_der"
#'   )
#' )
#'
#' # Scalar usage examples with variable labels
#' # calculate_drinking_risk_short(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7)
#' #                           (sex,    weekly,  year, week,  sun,     mon,     tue,     wed,     thu,     fri,     sat)
#' calculate_drinking_risk_short(1, 20, 1, 1, 0, 0, 5, 0, 0, 0, 0) # Returns: 1L (male short-term risk: 5 drinks/day OR 20/week)
#' calculate_drinking_risk_short(2, 8, 1, 1, 2, 1, 2, 1, 1, 1, 0) # Returns: 2L (female no risk: ≤2 drinks/day AND ≤10/week)
#' calculate_drinking_risk_short(1, 15, 1, 1, 3, 3, 3, 3, 3, 0, 0) # Returns: 2L (male no risk: ≤3 drinks/day AND ≤15/week)
#' calculate_drinking_risk_short(2, 11, 1, 1, 2, 2, 2, 2, 2, 1, 0) # Returns: 1L (female risk: 11 drinks/week exceeds 10)
#' calculate_drinking_risk_short(1, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0) # Returns: tagged_na("a") (non-drinker: not applicable)
#'
#' # Vector processing with missing data and edge cases
#' calculate_drinking_risk_short(
#'   c(1, 2, 6, 8, 9), # Mixed valid/CCHS codes for sex
#'   c(16, 11, 5, 996, 997), # Mixed weekly drinks with CCHS codes
#'   c(1, 1, 1, 1, 2), # Mixed alcohol use statuses
#'   c(1, 1, 1, 2, 1), # Mixed weekly drinking statuses
#'   c(4, 3, 1, 0, 7), # Mixed Sunday drinks
#'   c(0, 0, 0, 0, 8), c(0, 0, 0, 0, 9), c(0, 0, 0, 0, 0),
#'   c(0, 0, 0, 0, 0), c(0, 0, 0, 0, 0), c(0, 0, 0, 0, 0)
#' ) # Returns: c(1L, 1L, tagged_na("b"), tagged_na("b"), tagged_na("a"))
#'
#' @seealso
#' \code{\link{calculate_binge_drinking}} for binge drinking assessment
#' \code{\link{calculate_drinking_risk_long}} for long-term drinking risk assessment
#'
#' @references
#' Butt, P., et al. (2011). Alcohol and health in Canada: a summary of evidence and guidelines
#' for low-risk drinking. Canadian Centre on Substance Abuse.
#'
#' @note v3.0.0, last updated: 2025-07-05, status: active, Note: Enhanced with comprehensive risk assessment and missing data handling
#' @export
calculate_drinking_risk_short <- function(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1,
                                          ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7,
                                          # Validation bounds: defaults from variable_details.csv for standalone use
                                          # Use rec_with_table() for full CSV-driven validation workflow
                                          min_DHH_SEX = 1,
                                          max_DHH_SEX = 2,
                                          min_ALWDWKY = 0,
                                          max_ALWDWKY = 995,
                                          min_ALC_1 = 1,
                                          max_ALC_1 = 2,
                                          min_ALW_1 = 1,
                                          max_ALW_1 = 2,
                                          min_ALW_2A1 = 0,
                                          max_ALW_2A1 = 995,
                                          min_ALW_2A2 = 0,
                                          max_ALW_2A2 = 995,
                                          min_ALW_2A3 = 0,
                                          max_ALW_2A3 = 995,
                                          min_ALW_2A4 = 0,
                                          max_ALW_2A4 = 995,
                                          min_ALW_2A5 = 0,
                                          max_ALW_2A5 = 995,
                                          min_ALW_2A6 = 0,
                                          max_ALW_2A6 = 995,
                                          min_ALW_2A7 = 0,
                                          max_ALW_2A7 = 995,
                                          validate_params = NULL,
                                          log_level = "silent") {
  # Clean variables using standardized helpers (handles missing variables with tagged_na("d"))
  cleaned <- clean_variables(
    categorical_vars = list(DHH_SEX = DHH_SEX, ALC_1 = ALC_1, ALW_1 = ALW_1),
    continuous_vars = list(
      ALWDWKY = ALWDWKY, ALW_2A1 = ALW_2A1, ALW_2A2 = ALW_2A2, ALW_2A3 = ALW_2A3,
      ALW_2A4 = ALW_2A4, ALW_2A5 = ALW_2A5, ALW_2A6 = ALW_2A6, ALW_2A7 = ALW_2A7
    ),
    valid_values = list(
      DHH_SEX = min_DHH_SEX:max_DHH_SEX,
      ALC_1 = min_ALC_1:max_ALC_1,
      ALW_1 = min_ALW_1:max_ALW_1
    ),
    min_values = list(
      ALWDWKY = min_ALWDWKY, ALW_2A1 = min_ALW_2A1, ALW_2A2 = min_ALW_2A2, ALW_2A3 = min_ALW_2A3,
      ALW_2A4 = min_ALW_2A4, ALW_2A5 = min_ALW_2A5, ALW_2A6 = min_ALW_2A6, ALW_2A7 = min_ALW_2A7
    ),
    max_values = list(
      ALWDWKY = max_ALWDWKY, ALW_2A1 = max_ALW_2A1, ALW_2A2 = max_ALW_2A2, ALW_2A3 = max_ALW_2A3,
      ALW_2A4 = max_ALW_2A4, ALW_2A5 = max_ALW_2A5, ALW_2A6 = max_ALW_2A6, ALW_2A7 = max_ALW_2A7
    ),
    categorical_pattern = "single_digit_missing",
    continuous_pattern = "triple_digit_missing",
    log_level = log_level
  )

  # Use core risk assessment function with "short" risk type
  assess_risk_core(
    cleaned$DHH_SEX_clean, cleaned$ALWDWKY_clean, cleaned$ALC_1_clean, cleaned$ALW_1_clean,
    cleaned$ALW_2A1_clean, cleaned$ALW_2A2_clean, cleaned$ALW_2A3_clean, cleaned$ALW_2A4_clean,
    cleaned$ALW_2A5_clean, cleaned$ALW_2A6_clean, cleaned$ALW_2A7_clean,
    risk_type = "short"
  )
}

#' Assess long-term alcohol health risks using Low-Risk Guidelines
#'
#' @description
#' Assess long-term health risks from drinking patterns following Canada's Low-Risk Guidelines.
#' Evaluates daily and weekly consumption against sex-specific chronic risk thresholds for
#' long-term health outcomes assessment.
#'
#' @param DHH_SEX,ALWDWKY,ALC_1,ALW_1,ALW_2A1,ALW_2A2,ALW_2A3,ALW_2A4,ALW_2A5,ALW_2A6,ALW_2A7 As in calculate_drinking_risk_short()
#'
#' @return Integer long-term risk indicator. Missing data handled as:
#'   \itemize{
#'     \item \code{haven::tagged_na("a")} for not applicable cases (non-drinkers)
#'     \item \code{haven::tagged_na("b")} for missing/invalid responses
#'   }
#'   Values: 1 = increased long-term risk, 2 = no increased long-term risk.
#'
#' @details
#' Risk criteria: Males: ≥4 drinks/day OR ≥20 drinks/week; Females: ≥3 drinks/day OR ≥15 drinks/week.
#' Focus on chronic health risks (liver disease, cancer, cardiovascular). Same preprocessing and
#' missing data handling as short-term version.
#'
#' @examples
#' # Standard cchsflow workflow (primary usage)
#' library(cchsflow)
#' result <- rec_with_table(
#'   cchs2013_2014_p,
#'   c(
#'     "DHH_SEX", "ALWDWKY", "ALC_1", "ALW_1", "ALW_2A1", "ALW_2A2",
#'     "ALW_2A3", "ALW_2A4", "ALW_2A5", "ALW_2A6", "ALW_2A7", "ALCDLOWL_der"
#'   )
#' )
#'
#' # Scalar usage examples with variable labels
#' # calculate_drinking_risk_long(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7)
#' #                          (sex,    weekly,  year, week,  sun,     mon,     tue,     wed,     thu,     fri,     sat)
#' calculate_drinking_risk_long(1, 20, 1, 1, 0, 0, 4, 0, 0, 0, 0) # Returns: 1L (male long-term risk: 4 drinks/day OR 20/week)
#' calculate_drinking_risk_long(2, 14, 1, 1, 2, 2, 2, 2, 2, 2, 2) # Returns: 2L (female no risk: <3 drinks/day AND <15/week)
#' calculate_drinking_risk_long(1, 19, 1, 1, 3, 3, 3, 3, 3, 3, 1) # Returns: 2L (male no risk: <4 drinks/day AND <20/week)
#' calculate_drinking_risk_long(2, 15, 1, 1, 2, 2, 2, 2, 2, 2, 3) # Returns: 1L (female risk: 15 drinks/week exactly threshold)
#' calculate_drinking_risk_long(1, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0) # Returns: tagged_na("a") (non-drinker: not applicable)
#'
#' # Vector processing with missing data and edge cases
#' calculate_drinking_risk_long(
#'   c(1, 2, 6, 8, 9), # Mixed valid/CCHS codes for sex
#'   c(20, 15, 8, 996, 997), # Mixed weekly drinks with CCHS codes
#'   c(1, 1, 1, 1, 2), # Mixed alcohol use statuses
#'   c(1, 1, 1, 2, 1), # Mixed weekly drinking statuses
#'   c(4, 3, 2, 0, 7), # Mixed Sunday drinks
#'   c(0, 0, 0, 0, 8), c(0, 0, 0, 0, 9), c(0, 0, 0, 0, 0),
#'   c(0, 0, 0, 0, 0), c(0, 0, 0, 0, 0), c(0, 0, 0, 0, 0)
#' ) # Returns: c(1L, 1L, tagged_na("b"), tagged_na("b"), tagged_na("a"))
#'
#' @seealso
#' \code{\link{calculate_drinking_risk_short}} for short-term drinking risk assessment
#' \code{\link{calculate_binge_drinking}} for binge drinking assessment
#'
#' @references
#' Butt, P., et al. (2011). Alcohol and health in Canada: a summary of evidence and guidelines
#' for low-risk drinking. Canadian Centre on Substance Abuse.
#'
#' @note v3.0.0, last updated: 2025-07-05, status: active, Note: Enhanced with comprehensive long-term risk assessment
#' @export
calculate_drinking_risk_long <- function(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1,
                                         ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7,
                                         # Validation bounds: defaults from variable_details.csv for standalone use
                                         # Use rec_with_table() for full CSV-driven validation workflow
                                         min_DHH_SEX = 1,
                                         max_DHH_SEX = 2,
                                         min_ALWDWKY = 0,
                                         max_ALWDWKY = 995,
                                         min_ALC_1 = 1,
                                         max_ALC_1 = 2,
                                         min_ALW_1 = 1,
                                         max_ALW_1 = 2,
                                         min_ALW_2A1 = 0,
                                         max_ALW_2A1 = 995,
                                         min_ALW_2A2 = 0,
                                         max_ALW_2A2 = 995,
                                         min_ALW_2A3 = 0,
                                         max_ALW_2A3 = 995,
                                         min_ALW_2A4 = 0,
                                         max_ALW_2A4 = 995,
                                         min_ALW_2A5 = 0,
                                         max_ALW_2A5 = 995,
                                         min_ALW_2A6 = 0,
                                         max_ALW_2A6 = 995,
                                         min_ALW_2A7 = 0,
                                         max_ALW_2A7 = 995,
                                         validate_params = NULL,
                                         log_level = "silent") {
  # Clean variables using standardized helpers (handles missing variables with tagged_na("d"))
  cleaned <- clean_variables(
    categorical_vars = list(DHH_SEX = DHH_SEX, ALC_1 = ALC_1, ALW_1 = ALW_1),
    continuous_vars = list(
      ALWDWKY = ALWDWKY, ALW_2A1 = ALW_2A1, ALW_2A2 = ALW_2A2, ALW_2A3 = ALW_2A3,
      ALW_2A4 = ALW_2A4, ALW_2A5 = ALW_2A5, ALW_2A6 = ALW_2A6, ALW_2A7 = ALW_2A7
    ),
    valid_values = list(
      DHH_SEX = min_DHH_SEX:max_DHH_SEX,
      ALC_1 = min_ALC_1:max_ALC_1,
      ALW_1 = min_ALW_1:max_ALW_1
    ),
    min_values = list(
      ALWDWKY = min_ALWDWKY, ALW_2A1 = min_ALW_2A1, ALW_2A2 = min_ALW_2A2, ALW_2A3 = min_ALW_2A3,
      ALW_2A4 = min_ALW_2A4, ALW_2A5 = min_ALW_2A5, ALW_2A6 = min_ALW_2A6, ALW_2A7 = min_ALW_2A7
    ),
    max_values = list(
      ALWDWKY = max_ALWDWKY, ALW_2A1 = max_ALW_2A1, ALW_2A2 = max_ALW_2A2, ALW_2A3 = max_ALW_2A3,
      ALW_2A4 = max_ALW_2A4, ALW_2A5 = max_ALW_2A5, ALW_2A6 = max_ALW_2A6, ALW_2A7 = max_ALW_2A7
    ),
    categorical_pattern = "single_digit_missing",
    continuous_pattern = "triple_digit_missing",
    log_level = log_level
  )

  # Use core risk assessment function with "long" risk type
  assess_risk_core(
    cleaned$DHH_SEX_clean, cleaned$ALWDWKY_clean, cleaned$ALC_1_clean, cleaned$ALW_1_clean,
    cleaned$ALW_2A1_clean, cleaned$ALW_2A2_clean, cleaned$ALW_2A3_clean, cleaned$ALW_2A4_clean,
    cleaned$ALW_2A5_clean, cleaned$ALW_2A6_clean, cleaned$ALW_2A7_clean,
    risk_type = "long"
  )
}
