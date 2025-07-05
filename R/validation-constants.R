# ==============================================================================
# Validation Constants for All Derived Variable Domains - v3.0.0 Architecture
# ==============================================================================

# REQUIRED DEPENDENCIES:
#   None - this file contains only constants

# PURPOSE:
# Centralized validation bounds and constants for all derived variable domains.
# These constants support both basic CCHS processing and enhanced research validation.
# Constants are designed for rec_with_table() compatibility and range interval notation.

# ==============================================================================
# 1. BMI DOMAIN VALIDATION BOUNDS
# ==============================================================================

# BMI validation bounds for standalone function use
# NOTE: These constants ensure BMI functions work independently ("cut and paste")
# while rec_with_table() uses variable_details.csv for CSV-driven validation.
# IMPORTANT: Keep synchronized with variable_details.csv - see test-validation-consistency.R

BMI_VALIDATION_BOUNDS <- list(
  height = list(min = 0.914, max = 2.134), # Height in meters [0.914,2.134] from variable_details.csv
  weight = list(min = 27.0, max = 135.0), # Weight in kilograms [27.0,135.0] from variable_details.csv
  bmi = list(min = 10, max = 100) # BMI valid range [10,100] from variable_details.csv
)

# Legacy format for backward compatibility
ANTHROPOMETRIC_BOUNDS <- list(
  HWTGHTM = BMI_VALIDATION_BOUNDS$height,
  HWTGWTK = BMI_VALIDATION_BOUNDS$weight
)

# BMI correction coefficients from Connor Gorber et al. 2008
BMI_CORRECTION_COEFFICIENTS <- list(
  male = list(intercept = -1.07575, slope = 1.07592),
  female = list(intercept = -0.12374, slope = 1.05129)
)

# ==============================================================================
# 2. SMOKING DOMAIN VALIDATION BOUNDS
# ==============================================================================

# Smoking initiation age bounds (evidence-based from Holford et al.)
SMOKING_AGE_BOUNDS <- list(
  min_initiation = 8, # Minimum plausible smoking initiation age
  max_initiation = 95, # Maximum plausible smoking initiation age
  min_current_age = 12, # Minimum age for smoking questions
  max_current_age = 102 # Maximum age in CCHS
)

# Variable-specific smoking age bounds for rec_with_table()
SMOKING_VARIABLE_BOUNDS <- list(
  SMKG203_cont = list(min = 8, max = 84), # Age started daily (daily smokers)
  SMKG207_cont = list(min = 8, max = 84), # Age started daily (former daily)
  SMKG040_cont = list(min = 8, max = 95), # Combined age started daily
  SMKG01C_cont = list(min = 8, max = 84), # Age first cigarette
  SMK_204 = list(min = 1, max = 80), # Cigarettes per day (daily)
  SMK_208 = list(min = 1, max = 80), # Cigarettes per day (former daily)
  SMK_05B = list(min = 1, max = 80), # Cigarettes per day (occasional)
  SMK_05C = list(min = 1, max = 30) # Days smoked per month
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

# ==============================================================================
# 3. ALCOHOL DOMAIN VALIDATION BOUNDS
# ==============================================================================

# Binge drinking thresholds by sex (Canadian guidelines)
BINGE_THRESHOLDS <- list(
  male = 5, # 5+ drinks for males
  female = 4 # 4+ drinks for females
)

# Low-risk drinking guidelines (Canada's Low-Risk Alcohol Drinking Guidelines)
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

# Alcohol variable validation ranges for rec_with_table()
ALCOHOL_VALIDATION_BOUNDS <- list(
  ALW_1 = list(min = 1, max = 2), # Drinks in past 12 months (1=yes, 2=no)
  ALW_2A1 = list(min = 0, max = 20), # Sunday drinks
  ALW_2A2 = list(min = 0, max = 20), # Monday drinks
  ALW_2A3 = list(min = 0, max = 20), # Tuesday drinks
  ALW_2A4 = list(min = 0, max = 20), # Wednesday drinks
  ALW_2A5 = list(min = 0, max = 20), # Thursday drinks
  ALW_2A6 = list(min = 0, max = 20), # Friday drinks
  ALW_2A7 = list(min = 0, max = 20), # Saturday drinks
  drinks_per_day = list(min = 0, max = 20), # Daily drinks range
  drinks_per_week = list(min = 0, max = 140) # Weekly drinks range (20 × 7)
)

# ==============================================================================
# 4. ADL DOMAIN VALIDATION BOUNDS
# ==============================================================================

# ADL response validation
ADL_VALIDATION_BOUNDS <- list(
  response_range = c(1, 2), # 1 = needs help, 2 = does not need help
  score_5_range = list(min = 0, max = 5), # 5-item ADL score range
  score_6_range = list(min = 0, max = 6) # 6-item ADL score range
)

# ADL variable names for different scales
ADL_ITEM_SETS <- list(
  five_item = c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05"),
  six_item = c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_06")
)

# ADL variable bounds for rec_with_table()
ADL_VARIABLE_BOUNDS <- list(
  ADL_01 = list(min = 1, max = 2), # Preparing meals
  ADL_02 = list(min = 1, max = 2), # Getting to appointments
  ADL_03 = list(min = 1, max = 2), # Household chores
  ADL_04 = list(min = 1, max = 2), # Personal care
  ADL_05 = list(min = 1, max = 2), # Moving around house
  ADL_06 = list(min = 1, max = 2) # Managing finances (6-item version)
)

# ==============================================================================
# 5. GENERAL DEMOGRAPHIC VALIDATION BOUNDS
# ==============================================================================

# Common demographic variable bounds
DEMOGRAPHIC_BOUNDS <- list(
  DHH_SEX = list(min = 1, max = 2), # Sex (1=male, 2=female)
  DHHGAGE_cont = list(min = 12, max = 102), # Continuous age
  age_categories = list(min = 1, max = 12), # Age category variables (typical)
  binary_response = list(min = 1, max = 2), # Yes/No responses
  province = list(min = 10, max = 62) # Province codes (Stats Canada)
)

# ==============================================================================
# 6. PHYSICAL ACTIVITY DOMAIN VALIDATION BOUNDS
# ==============================================================================

# Physical activity validation bounds
PHYSICAL_ACTIVITY_BOUNDS <- list(
  PAA_045 = list(min = 0, max = 16), # Hours vigorous activity per week
  PAA_050 = list(min = 0, max = 59), # Minutes vigorous activity
  PAA_075 = list(min = 0, max = 16), # Hours moderate activity per week
  PAA_080 = list(min = 0, max = 59), # Minutes moderate activity
  PAADVDYS = list(min = 0, max = 7), # Active days per week
  PAADVVIG = list(min = 0, max = 1680), # Total vigorous minutes per week
  energy_expenditure = list(min = 0, max = 20) # Energy expenditure (kcal/kg/day)
)

# ==============================================================================
# 7. VALIDATION HELPER CONSTANTS
# ==============================================================================

# Standard CCHS missing codes by pattern type
CCHS_MISSING_CODES <- list(
  standard_response = list(
    not_applicable = 6,
    missing_codes = c(7, 8, 9)
  ),
  categorical_age = list(
    not_applicable = 96,
    missing_codes = c(97, 98, 99)
  ),
  continuous_standard = list(
    not_applicable = 996,
    missing_codes = c(997, 998, 999)
  )
)

# Validation modes for different processing contexts
VALIDATION_MODES <- list(
  basic_cchs = "No validation bounds applied - accepts all CCHS values",
  enhanced_research = "Applies validation bounds with comprehensive checks",
  rec_with_table = "Uses CSV-driven validation from variable_details.csv"
)
