# ==============================================================================
# Enhanced Alcohol Functions - Following v3.0.0 Development Guide
# ==============================================================================

# REQUIRED DEPENDENCIES:
#   library(haven)   # for haven::tagged_na() and haven::is_tagged_na()
#   library(dplyr)   # for dplyr::case_when() and dplyr::if_else()
#   source("R/missing-data-helpers.R")  # for preprocessing functions

# ==============================================================================
# 1. CONSTANTS AND CONFIGURATION
# ==============================================================================

# Binge drinking thresholds by sex
BINGE_THRESHOLDS <- list(
  male = 5,     # 5+ drinks for males
  female = 4    # 4+ drinks for females
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

# Valid response ranges for alcohol variables
ALCOHOL_VALIDATION_RANGES <- list(
  sex = c(1, 2),
  binary_response = c(1, 2),
  drinks_daily = c(0, 995),
  drinks_weekly = c(0, 995)
)

# ==============================================================================
# 2. CORE UTILITY FUNCTIONS
# ==============================================================================

#' Enhanced alcohol input validation
#' @param param_name Parameter name for error messages
#' @param param_value Parameter value to validate
#' @param required Logical indicating if parameter is required
#' @return Validated parameter or error
#' @noRd
validate_alcohol_parameter <- function(param_name, param_value, required = TRUE) {
  if (required && missing(param_value)) {
    stop(paste("Required alcohol parameter", param_name, "must be provided"), call. = FALSE)
  }
  return(param_value)
}

#' Check vector length compatibility for alcohol inputs
#' @param ... Input vectors to check
#' @return Logical indicating compatibility
#' @noRd
check_alcohol_length_compatibility <- function(...) {
  lengths <- lengths(list(...))
  # All lengths must be equal OR some can be length 1 (scalar)
  return(length(unique(lengths[lengths > 1])) <= 1)
}

#' Preprocess single alcohol variable with comprehensive missing data handling
#' @param alcohol_var Single alcohol variable (binary responses or continuous drinks)
#' @return Preprocessed alcohol variable with haven::tagged_na()
#' @noRd
preprocess_alcohol_variable <- function(alcohol_var) {
  dplyr::case_when(
    # Handle original CCHS missing codes
    alcohol_var == 6 ~ haven::tagged_na("a"),  # Not applicable
    alcohol_var %in% c(7, 8, 9) ~ haven::tagged_na("b"),  # Don't know, refusal, not stated
    
    # Handle continuous missing codes (996-999)
    alcohol_var == 996 ~ haven::tagged_na("a"),  # Not applicable
    alcohol_var %in% c(997, 998, 999) ~ haven::tagged_na("b"),  # Don't know, refusal, not stated
    
    # Handle string-based missing values (legacy support)
    is.character(alcohol_var) & alcohol_var == "NA(a)" ~ haven::tagged_na("a"),
    is.character(alcohol_var) & alcohol_var == "NA(b)" ~ haven::tagged_na("b"),
    is.character(alcohol_var) & alcohol_var %in% c("Not applicable", "not applicable") ~ haven::tagged_na("a"),
    is.character(alcohol_var) & alcohol_var %in% c("Missing", "Don't know", "Refusal") ~ haven::tagged_na("b"),
    
    # Handle existing haven::tagged_na (passthrough)
    haven::is_tagged_na(alcohol_var, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(alcohol_var, "b") ~ haven::tagged_na("b"),
    
    # Handle regular NAs
    is.na(alcohol_var) ~ haven::tagged_na("b"),
    
    # Valid responses pass through as numeric
    .default = as.numeric(alcohol_var)
  )
}

# ==============================================================================
# 3. SPECIALIZED HELPER FUNCTIONS
# ==============================================================================

#' Check if any day meets binge drinking criteria
#' @param daily_drinks_list List of preprocessed daily drink variables
#' @param sex_threshold Binge threshold for sex (4 for female, 5 for male)
#' @return Logical indicating binge drinking
#' @noRd
check_binge_drinking <- function(daily_drinks_list, sex_threshold) {
  
  # Check for any not applicable values (highest priority)
  has_not_applicable <- any(sapply(daily_drinks_list, function(x) haven::is_tagged_na(x, "a")))
  
  # Check for any missing values
  has_missing <- any(sapply(daily_drinks_list, function(x) haven::is_tagged_na(x, "b") | is.na(x)))
  
  # Check for binge drinking (any day >= threshold)
  has_binge_day <- any(sapply(daily_drinks_list, function(x) 
    if(!haven::is_tagged_na(x) & !is.na(x)) x >= sex_threshold else FALSE))
  
  # Return result based on missing data priority
  dplyr::case_when(
    has_not_applicable ~ haven::tagged_na("a"),
    has_missing ~ haven::tagged_na("b"),
    has_binge_day ~ 1L,  # Binge drinker
    .default = 2L  # Non-binge drinker
  )
}

#' Check drinking risk based on daily and weekly consumption
#' @param daily_drinks_list List of preprocessed daily drink variables
#' @param weekly_drinks Preprocessed weekly drinks variable
#' @param sex Sex variable (1=male, 2=female)
#' @param risk_type Type of risk assessment ("short" or "long")
#' @return Risk level indicator
#' @noRd
assess_drinking_risk <- function(daily_drinks_list, weekly_drinks, sex, risk_type = "short") {
  
  # Get appropriate thresholds
  if (risk_type == "short") {
    daily_threshold <- if (sex == 1) 4 else 3  # Short-term daily limits
    weekly_threshold <- if (sex == 1) 15 else 10  # Short-term weekly limits
  } else {  # long-term
    daily_threshold <- if (sex == 1) 3 else 2  # Long-term daily limits  
    weekly_threshold <- if (sex == 1) 15 else 10  # Long-term weekly limits
  }
  
  # Check for any not applicable values
  has_not_applicable <- any(sapply(daily_drinks_list, function(x) haven::is_tagged_na(x, "a"))) ||
                        haven::is_tagged_na(weekly_drinks, "a")
  
  # Check for any missing values
  has_missing <- any(sapply(daily_drinks_list, function(x) haven::is_tagged_na(x, "b") | is.na(x))) ||
                 haven::is_tagged_na(weekly_drinks, "b") || is.na(weekly_drinks)
  
  # Check for high-risk drinking
  has_high_daily <- any(sapply(daily_drinks_list, function(x) 
    if(!haven::is_tagged_na(x) & !is.na(x)) x > daily_threshold else FALSE))
  
  has_high_weekly <- if(!haven::is_tagged_na(weekly_drinks) & !is.na(weekly_drinks)) {
    weekly_drinks > weekly_threshold
  } else FALSE
  
  # Return risk assessment
  dplyr::case_when(
    has_not_applicable ~ haven::tagged_na("a"),
    has_missing ~ haven::tagged_na("b"),
    has_high_daily | has_high_weekly ~ 1L,  # Increased risk
    .default = 2L  # No increased risk
  )
}

# ==============================================================================
# 4. PUBLIC API FUNCTIONS (Enhanced)
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
#' result <- rec_with_table(cchs2013_2014_p, 
#'                         c("DHH_SEX", "ALW_1", "ALW_2A1", "ALW_2A2", "ALW_2A3", 
#'                           "ALW_2A4", "ALW_2A5", "ALW_2A6", "ALW_2A7", "ALCDBINGE_der"))
#'
#' # Male binge drinker (6 drinks Tuesday, 8 drinks Friday)
#' binge_drinker_fun(1, 1, 3, 1, 6, 0, 3, 8, 2)  # Returns: 1
#'
#' # Female non-binge drinker (all days ≤3 drinks)
#' binge_drinker_fun(2, 1, 1, 2, 2, 3, 0, 1, 2)  # Returns: 2
#'
#' # Vector processing with missing data
#' binge_drinker_fun(c(1, 2, 6), c(1, 1, 7), c(5, 4, 2), c(0, 0, 2), 
#'                  c(0, 0, 2), c(0, 0, 2), c(0, 0, 2), c(0, 0, 2), c(0, 0, 2))
#'
#' @seealso
#' \\code{\\link{low_drink_short_fun}} for short-term drinking risk assessment
#' \\code{\\link{low_drink_long_fun}} for long-term drinking risk assessment
#'
#' @references
#' Butt, P., et al. (2011). Alcohol and health in Canada: a summary of evidence and guidelines 
#' for low-risk drinking. Canadian Centre on Substance Abuse.
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced with comprehensive preprocessing and modern missing data handling
#' @export
binge_drinker_fun <- function(DHH_SEX, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, 
                              ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7) {
  
  # 1. Enhanced input validation
  validate_alcohol_parameter("DHH_SEX", DHH_SEX, required = TRUE)
  validate_alcohol_parameter("ALW_1", ALW_1, required = TRUE)
  validate_alcohol_parameter("ALW_2A1", ALW_2A1, required = TRUE)
  validate_alcohol_parameter("ALW_2A2", ALW_2A2, required = TRUE)
  validate_alcohol_parameter("ALW_2A3", ALW_2A3, required = TRUE)
  validate_alcohol_parameter("ALW_2A4", ALW_2A4, required = TRUE)
  validate_alcohol_parameter("ALW_2A5", ALW_2A5, required = TRUE)
  validate_alcohol_parameter("ALW_2A6", ALW_2A6, required = TRUE)
  validate_alcohol_parameter("ALW_2A7", ALW_2A7, required = TRUE)
  
  # 2. Length compatibility check
  if (!check_alcohol_length_compatibility(DHH_SEX, ALW_1, ALW_2A1, ALW_2A2, 
                                         ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7)) {
    stop("Input vectors must have compatible lengths", call. = FALSE)
  }
  
  # 3. Preprocess all alcohol variables
  dhh_sex_processed <- preprocess_alcohol_variable(DHH_SEX)
  alw_1_processed <- preprocess_alcohol_variable(ALW_1)
  alw_2a1_processed <- preprocess_alcohol_variable(ALW_2A1)
  alw_2a2_processed <- preprocess_alcohol_variable(ALW_2A2)
  alw_2a3_processed <- preprocess_alcohol_variable(ALW_2A3)
  alw_2a4_processed <- preprocess_alcohol_variable(ALW_2A4)
  alw_2a5_processed <- preprocess_alcohol_variable(ALW_2A5)
  alw_2a6_processed <- preprocess_alcohol_variable(ALW_2A6)
  alw_2a7_processed <- preprocess_alcohol_variable(ALW_2A7)
  
  # 4. Calculate binge drinking status
  daily_drinks_list <- list(alw_2a1_processed, alw_2a2_processed, alw_2a3_processed,
                           alw_2a4_processed, alw_2a5_processed, alw_2a6_processed,
                           alw_2a7_processed)
  
  # Vectorized calculation
  result <- vector("integer", length(dhh_sex_processed))
  
  for (i in seq_along(dhh_sex_processed)) {
    # Check prerequisites first
    sex_val <- dhh_sex_processed[i]
    alw_1_val <- alw_1_processed[i]
    
    # Handle missing sex or drinking status
    if (haven::is_tagged_na(sex_val, "a") | haven::is_tagged_na(alw_1_val, "a")) {
      result[i] <- haven::tagged_na("a")
      next
    }
    
    if (haven::is_tagged_na(sex_val, "b") | haven::is_tagged_na(alw_1_val, "b") |
        is.na(sex_val) | is.na(alw_1_val)) {
      result[i] <- haven::tagged_na("b")
      next
    }
    
    # Not applicable if no drinking in past week
    if (alw_1_val == 2) {
      result[i] <- haven::tagged_na("a")
      next
    }
    
    # Invalid sex
    if (!(sex_val %in% c(1, 2))) {
      result[i] <- haven::tagged_na("b")
      next
    }
    
    # Get daily drinks for this observation
    daily_drinks_i <- lapply(daily_drinks_list, function(x) x[i])
    
    # Determine binge threshold
    binge_threshold <- if (sex_val == 1) BINGE_THRESHOLDS$male else BINGE_THRESHOLDS$female
    
    # Check binge drinking
    result[i] <- check_binge_drinking(daily_drinks_i, binge_threshold)
  }
  
  return(result)
}

#' Assess short-term alcohol health risks using Low-Risk Guidelines
#'
#' @description 
#' Assess short-term health risks from drinking patterns following Canada's Low-Risk Guidelines.
#' Evaluates daily and weekly consumption against sex-specific acute risk thresholds for 
#' injury and overdose risk assessment.
#'
#' @param DHH_SEX,ALW_1,ALW_2A1,ALW_2A2,ALW_2A3,ALW_2A4,ALW_2A5,ALW_2A6,ALW_2A7 As in binge_drinker_enhanced()
#' @param ALC_1 Had drinks in past year (1=yes, 2=no). Accepts raw CCHS codes or preprocessed values.
#' @param ALWDWKY Total drinks in past week. Accepts raw CCHS codes or preprocessed values.
#'
#' @return 
#' **Data Type**: Integer short-term risk indicator
#' **Missing Data Handling**: Same as binge_drinker_enhanced()
#' **Values**: 1 = increased short-term risk, 2 = no increased short-term risk
#'
#' @details
#' **Transformation Warnings**:
#' ⚠️ **CAUTION - Guideline Changes**: Based on Canada's Low-Risk Alcohol Drinking Guidelines.
#' Thresholds may differ from other national guidelines.
#' 
#' ⚠️ **CAUTION - Population Restrictions**: Non-drinkers (past year or week) receive 
#' "no increased risk" classification.
#'
#' **Risk Criteria**:
#' - Males: >4 drinks/day OR >15 drinks/week
#' - Females: >3 drinks/day OR >10 drinks/week
#' - Missing Data: Comprehensive preprocessing with priority handling
#' - Non-drinkers: Automatically classified as "no increased risk"
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced with comprehensive risk assessment and missing data handling
#' @export
low_drink_short_fun <- function(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, 
                                ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7) {
  
  # 1. Input validation
  validate_alcohol_parameter("DHH_SEX", DHH_SEX, required = TRUE)
  validate_alcohol_parameter("ALWDWKY", ALWDWKY, required = TRUE)
  validate_alcohol_parameter("ALC_1", ALC_1, required = TRUE)
  validate_alcohol_parameter("ALW_1", ALW_1, required = TRUE)
  validate_alcohol_parameter("ALW_2A1", ALW_2A1, required = TRUE)
  validate_alcohol_parameter("ALW_2A2", ALW_2A2, required = TRUE)
  validate_alcohol_parameter("ALW_2A3", ALW_2A3, required = TRUE)
  validate_alcohol_parameter("ALW_2A4", ALW_2A4, required = TRUE)
  validate_alcohol_parameter("ALW_2A5", ALW_2A5, required = TRUE)
  validate_alcohol_parameter("ALW_2A6", ALW_2A6, required = TRUE)
  validate_alcohol_parameter("ALW_2A7", ALW_2A7, required = TRUE)
  
  # 2. Length compatibility check
  if (!check_alcohol_length_compatibility(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, 
                                         ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7)) {
    stop("Input vectors must have compatible lengths", call. = FALSE)
  }
  
  # 3. Preprocess all variables
  dhh_sex_processed <- preprocess_alcohol_variable(DHH_SEX)
  alwdwky_processed <- preprocess_alcohol_variable(ALWDWKY)
  alc_1_processed <- preprocess_alcohol_variable(ALC_1)
  alw_1_processed <- preprocess_alcohol_variable(ALW_1)
  alw_2a1_processed <- preprocess_alcohol_variable(ALW_2A1)
  alw_2a2_processed <- preprocess_alcohol_variable(ALW_2A2)
  alw_2a3_processed <- preprocess_alcohol_variable(ALW_2A3)
  alw_2a4_processed <- preprocess_alcohol_variable(ALW_2A4)
  alw_2a5_processed <- preprocess_alcohol_variable(ALW_2A5)
  alw_2a6_processed <- preprocess_alcohol_variable(ALW_2A6)
  alw_2a7_processed <- preprocess_alcohol_variable(ALW_2A7)
  
  # 4. Risk assessment with comprehensive missing data handling
  dplyr::case_when(
    # Handle missing data priority
    haven::is_tagged_na(dhh_sex_processed, "a") | haven::is_tagged_na(alwdwky_processed, "a") |
    haven::is_tagged_na(alc_1_processed, "a") | haven::is_tagged_na(alw_1_processed, "a") ~ haven::tagged_na("a"),
    
    haven::is_tagged_na(dhh_sex_processed, "b") | haven::is_tagged_na(alwdwky_processed, "b") |
    haven::is_tagged_na(alc_1_processed, "b") | haven::is_tagged_na(alw_1_processed, "b") |
    is.na(dhh_sex_processed) | is.na(alwdwky_processed) | is.na(alc_1_processed) | is.na(alw_1_processed) ~ haven::tagged_na("b"),
    
    # Invalid values
    !(dhh_sex_processed %in% c(1, 2)) | !(alc_1_processed %in% c(1, 2)) | !(alw_1_processed %in% c(1, 2)) ~ haven::tagged_na("b"),
    
    # Non-drinkers (no increased risk)
    alc_1_processed == 2 | alw_1_processed == 2 ~ 2L,
    
    # Risk assessment for males
    dhh_sex_processed == 1 & (
      alw_2a1_processed >= 5 | alw_2a2_processed >= 5 | alw_2a3_processed >= 5 |
      alw_2a4_processed >= 5 | alw_2a5_processed >= 5 | alw_2a6_processed >= 5 |
      alw_2a7_processed >= 5 | alwdwky_processed >= 16
    ) ~ 1L,
    
    # Risk assessment for females  
    dhh_sex_processed == 2 & (
      alw_2a1_processed >= 4 | alw_2a2_processed >= 4 | alw_2a3_processed >= 4 |
      alw_2a4_processed >= 4 | alw_2a5_processed >= 4 | alw_2a6_processed >= 4 |
      alw_2a7_processed >= 4 | alwdwky_processed >= 11
    ) ~ 1L,
    
    # No increased risk (within guidelines)
    .default = 2L
  )
}

#' Enhanced long-term alcohol risk assessment
#'
#' @description 
#' **Purpose**: Assess long-term health risks from drinking patterns following Canada's Low-Risk Guidelines
#' **Method**: Evaluates daily and weekly consumption against sex-specific chronic risk thresholds
#' **Clinical Context**: Chronic alcohol risk assessment for long-term health outcomes
#'
#' @param DHH_SEX,ALWDWKY,ALC_1,ALW_1,ALW_2A1,ALW_2A2,ALW_2A3,ALW_2A4,ALW_2A5,ALW_2A6,ALW_2A7 As in alcohol_risk_short_enhanced()
#'
#' @return 
#' **Data Type**: Integer long-term risk indicator
#' **Missing Data Handling**: Same as alcohol_risk_short_enhanced()
#' **Values**: 1 = increased long-term risk, 2 = no increased long-term risk
#'
#' @details
#' **Risk Criteria**:
#' - Males: >3 drinks/day OR >15 drinks/week
#' - Females: >2 drinks/day OR >10 drinks/week
#' - Focus on chronic health risks (liver disease, cancer, cardiovascular)
#' - Same preprocessing and missing data handling as short-term version
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced with comprehensive long-term risk assessment
#' @export
low_drink_long_fun <- function(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, 
                               ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7) {
  
  # 1. Input validation (same as short-term)
  validate_alcohol_parameter("DHH_SEX", DHH_SEX, required = TRUE)
  validate_alcohol_parameter("ALWDWKY", ALWDWKY, required = TRUE)
  validate_alcohol_parameter("ALC_1", ALC_1, required = TRUE)
  validate_alcohol_parameter("ALW_1", ALW_1, required = TRUE)
  validate_alcohol_parameter("ALW_2A1", ALW_2A1, required = TRUE)
  validate_alcohol_parameter("ALW_2A2", ALW_2A2, required = TRUE)
  validate_alcohol_parameter("ALW_2A3", ALW_2A3, required = TRUE)
  validate_alcohol_parameter("ALW_2A4", ALW_2A4, required = TRUE)
  validate_alcohol_parameter("ALW_2A5", ALW_2A5, required = TRUE)
  validate_alcohol_parameter("ALW_2A6", ALW_2A6, required = TRUE)
  validate_alcohol_parameter("ALW_2A7", ALW_2A7, required = TRUE)
  
  # 2. Length compatibility check
  if (!check_alcohol_length_compatibility(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, 
                                         ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7)) {
    stop("Input vectors must have compatible lengths", call. = FALSE)
  }
  
  # 3. Preprocess all variables (same as short-term)
  dhh_sex_processed <- preprocess_alcohol_variable(DHH_SEX)
  alwdwky_processed <- preprocess_alcohol_variable(ALWDWKY)
  alc_1_processed <- preprocess_alcohol_variable(ALC_1)
  alw_1_processed <- preprocess_alcohol_variable(ALW_1)
  alw_2a1_processed <- preprocess_alcohol_variable(ALW_2A1)
  alw_2a2_processed <- preprocess_alcohol_variable(ALW_2A2)
  alw_2a3_processed <- preprocess_alcohol_variable(ALW_2A3)
  alw_2a4_processed <- preprocess_alcohol_variable(ALW_2A4)
  alw_2a5_processed <- preprocess_alcohol_variable(ALW_2A5)
  alw_2a6_processed <- preprocess_alcohol_variable(ALW_2A6)
  alw_2a7_processed <- preprocess_alcohol_variable(ALW_2A7)
  
  # 4. Long-term risk assessment with different thresholds
  dplyr::case_when(
    # Handle missing data priority (same logic as short-term)
    haven::is_tagged_na(dhh_sex_processed, "a") | haven::is_tagged_na(alwdwky_processed, "a") |
    haven::is_tagged_na(alc_1_processed, "a") | haven::is_tagged_na(alw_1_processed, "a") ~ haven::tagged_na("a"),
    
    haven::is_tagged_na(dhh_sex_processed, "b") | haven::is_tagged_na(alwdwky_processed, "b") |
    haven::is_tagged_na(alc_1_processed, "b") | haven::is_tagged_na(alw_1_processed, "b") |
    is.na(dhh_sex_processed) | is.na(alwdwky_processed) | is.na(alc_1_processed) | is.na(alw_1_processed) ~ haven::tagged_na("b"),
    
    # Invalid values
    !(dhh_sex_processed %in% c(1, 2)) | !(alc_1_processed %in% c(1, 2)) | !(alw_1_processed %in% c(1, 2)) ~ haven::tagged_na("b"),
    
    # Non-drinkers (no increased risk)
    alc_1_processed == 2 | alw_1_processed == 2 ~ 2L,
    
    # Long-term risk assessment for males (≥4 drinks/day OR ≥16 drinks/week)
    dhh_sex_processed == 1 & (
      alw_2a1_processed >= 4 | alw_2a2_processed >= 4 | alw_2a3_processed >= 4 |
      alw_2a4_processed >= 4 | alw_2a5_processed >= 4 | alw_2a6_processed >= 4 |
      alw_2a7_processed >= 4 | alwdwky_processed >= 16
    ) ~ 1L,
    
    # Long-term risk assessment for females (≥3 drinks/day OR ≥11 drinks/week)
    dhh_sex_processed == 2 & (
      alw_2a1_processed >= 3 | alw_2a2_processed >= 3 | alw_2a3_processed >= 3 |
      alw_2a4_processed >= 3 | alw_2a5_processed >= 3 | alw_2a6_processed >= 3 |
      alw_2a7_processed >= 3 | alwdwky_processed >= 11
    ) ~ 1L,
    
    # No increased long-term risk (within guidelines)
    .default = 2L
  )
}