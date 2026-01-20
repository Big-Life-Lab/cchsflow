# ================================================================================
# Smoking Cessation Classification Functions
# ================================================================================
#
# There are five smoking cessation variables that can be harmonized between 
# CCHS 2001 and 2023:
#
# 1. SMK_06A_B - "When did you stop smoking - occasional/never daily smoker (categorical)"
#    CCHS cycles: 2001 → 2023 (4 categories)
#    Categories: 1=<1yr ago, 2=1-2yr ago, 3=2-3yr ago, 4=3+yr ago
#
# 2. SMK_06A_cont - "When did you stop smoking - occasional/never daily smoker (continuous)"
#    CCHS cycles: 2001 → 2023 (continuous years)
#    Values: Actual years since stopped smoking (numeric)
#
# 3. SMK_09A_B - "When did you stop smoking daily - former daily smoker (categorical)"
#    CCHS cycles: 2001 → 2023 (4 categories)  
#    Categories: 1=<1yr ago, 2=1-2yr ago, 3=2-3yr ago, 4=3+yr ago
#
# 4. SMK_09A_cont - "When did you stop smoking daily - former daily smoker (continuous)"
#    CCHS cycles: 2001 → 2023 (continuous years)
#    Values: Actual years since stopped smoking daily (numeric)
#
# 5. SPU_25I - "Time since stopped smoking for former daily smokers"
#    CCHS cycles: 2022 only (continuous years)
#    Values: Derived from SPU_25_A_I, SPU_25_B_I, ADM_MOI_I, ADM_YOI_I
#
# IMPLEMENTATION ORDER:
# - Variables 1-5: Direct harmonization via rec_with_table() (documentation-only initially)
# - Complex derivation may be needed for 2022 cycle using SPU_25* + ADM_*OI_I variables
#
# ================================================================================

# Package dependencies are declared in DESCRIPTION and loaded via NAMESPACE
# Functions used: haven::tagged_na(), haven::is_tagged_na(), dplyr::case_when()
# Internal functions: clean_variables(), any_missing(), get_priority_missing()

# ================================================================================

# SMK_06A_B - When stopped smoking - occasional/never daily (categorical) - DOCUMENTATION ONLY
# ================================================================================

#' @title When Stopped Smoking - Occasional/Never Daily - SMK_06A_B (categorical)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMK_06A variable across CCHS cycles 2001-2023.
#' This variable asks occasional and never-daily smokers when they stopped smoking,
#' categorized into 4 time periods.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2001-2014: SMK_06A (cycle-specific: SMKA_06A, SMKC_06A, etc.)
#'   - 2015-2021: SMK_060 (same question structure)
#'   - 2023: SPU_10 (different underlying source, same harmonized structure)
#' - **Harmonization**: Simple 1:1 mapping with source variable evolution
#' 
#' **Categories (4)**:
#' \itemize{
#'   \item 1 = Less than one year ago
#'   \item 2 = 1 year to less than 2 years ago
#'   \item 3 = 2 years to less than 3 years ago
#'   \item 4 = 3 or more years ago
#' }
#' 
#' **Variable evolution**:
#' - **2001-2014**: Direct survey question SMK_06A "When did you stop smoking?"
#' - **2015-2021**: Maps to SMK_060 variable (same question, consistent structure)
#' - **2023**: Maps to SPU_10 variable (same question, different variable name)
#'   - Asked of former occasional and experimental smokers
#'   - Provides timeframe categorization for smoking cessation patterns
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of time period classifications (1-4, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMK_06A_B")
#' time_stopped_occasional <- harmonized_data$SMK_06A_B
#' }
#' 
#' @export
calculate_SMK_06A_B <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_06A_B') for implementation")
}

# ================================================================================

# SMK_06A_cont - When stopped smoking - occasional/never daily (continuous) - DOCUMENTATION ONLY
# ================================================================================

#' @title When Stopped Smoking - Occasional/Never Daily - SMK_06A_cont (continuous)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMK_06A continuous variable across CCHS cycles 2001-2023.
#' This variable provides the continuous years since occasional and never-daily 
#' smokers stopped smoking.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2001-2014: SMK_06A continuous values (cycle-specific: SMKA_06A, etc.)
#'   - 2015-2021: SMK_060 continuous values (same structure)
#'   - 2023: SPU_10 continuous values (different source, same harmonized structure)
#' - **Harmonization**: Simple 1:1 mapping with source variable evolution
#' 
#' **Values**: Continuous years since cessation (numeric)
#' - Range typically 0-50+ years
#' - Actual years since respondent stopped smoking
#' - More precise than categorical version (SMK_06A_B)
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of continuous years values (numeric, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMK_06A_cont")
#' time_stopped_cont <- harmonized_data$SMK_06A_cont
#' }
#' 
#' @export
calculate_SMK_06A_cont <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_06A_cont') for implementation")
}

# ================================================================================

# SMK_09A_B - When stopped smoking daily - former daily (categorical) - DOCUMENTATION ONLY
# ================================================================================

#' @title When Stopped Smoking Daily - Former Daily Smoker - SMK_09A_B (categorical)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMK_09A categorical variable across CCHS cycles 2001-2023.
#' This variable asks former daily smokers when they stopped smoking daily, 
#' with categorical time periods.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2001-2014: SMK_09A (cycle-specific: SMKA_09A, SMKC_09A, etc.)
#'   - 2015-2021: SMK_080 (renamed variable, same question structure)
#'   - 2023: SPU_25 (different underlying source, same harmonized structure)
#' - **Harmonization**: Simple 1:1 mapping with source variable transition
#' 
#' **Categories (4)**:
#' \itemize{
#'   \item 1 = Less than one year ago
#'   \item 2 = 1 year to less than 2 years ago
#'   \item 3 = 2 years to less than 3 years ago
#'   \item 4 = 3 or more years ago
#' }
#' 
#' **Variable evolution**:
#' - **2001-2014**: Direct survey question SMK_09A "When did you stop smoking daily?"
#' - **2015-2021**: Maps to SMK_080 variable (renamed, same question)
#' - **2023**: Maps to SPU_25 variable (different source, same structure)
#'   - Asked of former daily smokers only
#'   - Critical for pack-years and smoking exposure calculations
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of time period classifications (1-4, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMK_09A_B")
#' time_stopped_daily <- harmonized_data$SMK_09A_B
#' }
#' 
#' @export
calculate_SMK_09A_B <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_09A_B') for implementation")
}

# ================================================================================
# SMK_09A_cont - When stopped smoking daily - former daily (continuous)
# ================================================================================

#' @title When Stopped Smoking Daily - Former Daily Smoker - SMK_09A_cont (continuous)
#' @description Creates harmonized SMK_09A_cont variable across CCHS cycles 2001-2023.
#' This variable provides continuous years since former daily smokers stopped smoking daily.
#' 
#' @details 
#' **Implementation method**: 3-step architecture
#' - **Step 1**: clean_variables() - Clean SMK_09A_B and SMKG09C inputs
#' - **Step 2**: Missing data functions + domain logic - Convert categories to continuous
#' - **Step 3**: Output cleaning using variable_details.csv
#' 
#' **Legacy logic pattern** (from variable_details.csv):
#' - Categories 1-3: Use categorical midpoint conversion
#' - Category 4 (3+ years): Use SMKG09C continuous variable for precision
#' - Missing: Inherit from input variables with priority hierarchy
#' 
#' **Architecture improvements**:
#' - Uses variable_details.csv recStart/recEnd mappings for conversion
#' - Supports both direct continuous variables (2001-2014) and derived conversion (2015+)
#' - Applies proper missing data priority handling
#' 
#' **Input requirements**:
#' - SMK_09A_B: Categorical time stopped smoking daily (1-4 categories)
#' - SMKG09C: Continuous years for "3+ years ago" category (when SMK_09A_B = 4)
#' 
#' @param SMK_09A_B Numeric vector. Categorical time stopped smoking daily
#' @param SMKG09C Numeric vector. Continuous years when SMK_09A_B = 4
#' @param output_format Character. Output format ("tagged_na" or "original")
#' 
#' @return Vector of continuous years values (0-50+ years, plus missing codes)
#' 
#' @examples
#' \dontrun{
#' # Scalar inputs - single respondent
#' result_scalar <- calculate_SMK_09A_cont(SMK_09A_B = 2, SMKG09C = NA)
#' # Returns: 1.5 (category 2 = 1-2 years, midpoint used)
#' 
#' # Vector inputs - multiple respondents  
#' smk_09a <- c(1, 2, 3, 4, 997, 999)  # <1yr, 1-2yr, 2-3yr, 3+yr, missing, missing
#' smkg09c <- c(NA, NA, NA, 5.5, NA, NA)  # continuous years for category 4
#' result_vector <- calculate_SMK_09A_cont(smk_09a, smkg09c)
#' # Returns: c(0.5, 1.5, 2.5, 5.5, NA::a, NA::b)
#' 
#' # Different output formats
#' result_tagged <- calculate_SMK_09A_cont(smk_09a, smkg09c, "tagged_na")
#' # Returns tagged_na for missing (haven format)
#' 
#' result_original <- calculate_SMK_09A_cont(smk_09a, smkg09c, "original") 
#' # Returns original CCHS missing codes (997, 999, etc.)
#' 
#' # Integration with rec_with_table() for CCHS harmonization
#' harmonized_data <- rec_with_table(cchs_data, "SMK_09A_cont", 
#'                                   custom_function = calculate_SMK_09A_cont)
#' }
#' 
#' @export
calculate_SMK_09A_cont <- function(SMK_09A_B, SMKG09C, output_format = "tagged_na") {
  
  # === STEP 1: DATA CLEANING and VALIDATION ===
  # Clean input variables (includes automatic length validation)
  cleaned <- clean_variables(vars = list(
    SMK_09A_B = SMK_09A_B,
    SMKG09C = SMKG09C
  ), output_format = output_format)
  
  # === STEP 2: DOMAIN LOGIC WITH MISSING DATA FUNCTIONS ===
  # Convert categorical to continuous using standard category midpoints
  result <- dplyr::case_when(
    # Missing data detection and priority processing
    any_missing(cleaned$SMK_09A_B) ~ 
      get_priority_missing(cleaned$SMK_09A_B, cleaned$SMKG09C, output_format = output_format),
    
    # Domain logic: Convert categories to continuous years
    cleaned$SMK_09A_B == 1 ~ 0.5,    # <1 year ago → 0.5 years
    cleaned$SMK_09A_B == 2 ~ 1.5,    # 1-2 years ago → 1.5 years  
    cleaned$SMK_09A_B == 3 ~ 2.5,    # 2-3 years ago → 2.5 years
    cleaned$SMK_09A_B == 4 & !any_missing(cleaned$SMKG09C) ~ cleaned$SMKG09C,  # 3+ years → use continuous
    cleaned$SMK_09A_B == 4 & any_missing(cleaned$SMKG09C) ~ 5.0,  # 3+ years fallback → 5.0 years
    
    # Invalid categories get missing value
    .default = assign_missing("not_applicable", "SMK_09A_cont", output_format)
  )
  
  # === STEP 3: OUTPUT CLEANING ===
  # Apply validation bounds and constraints from variable_details.csv
  output_cleaned <- clean_variables(vars = list(
    SMK_09A_cont = result
  ), output_format = output_format)
  
  return(output_cleaned$SMK_09A_cont)
}

# ================================================================================

# SPU_25I - Time since stopped smoking for former daily (2022 only) - DOCUMENTATION ONLY
# ================================================================================

#' @title Time Since Stopped Smoking - Former Daily (2022) - SPU_25I (continuous)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SPU_25I variable for CCHS 2022 cycle only.
#' This variable provides continuous years since former daily smokers stopped smoking,
#' derived from complex month/year interview date calculations.
#' 
#' @details 
#' **Implementation Method**: Complex derivation for 2022 cycle
#' - **Input variables**: SPU_25_A_I (years), SPU_25_B_I (months), ADM_MOI_I (interview month), ADM_YOI_I (interview year)
#' - **Logic**: Calculate time difference from stopping date to interview date
#' - **Architecture**: 3-step pattern with clean_variables() → domain logic → clean_variables()
#' 
#' **Values**: Continuous years since cessation (numeric)
#' - Range typically 0-50+ years  
#' - Calculated from interview date minus cessation date
#' - Higher precision than categorical versions
#' 
#' **Complex derivation logic**:
#' - Convert SPU_25_A_I (years ago) and SPU_25_B_I (additional months) to total months
#' - Calculate interview date from ADM_MOI_I and ADM_YOI_I
#' - Compute precise time difference in years
#' - Handle edge cases for month boundaries and leap years
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of continuous years values (numeric, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation (2022 only)
#' # or complex derivation function for precise calculation
#' harmonized_data <- rec_with_table(cchs_data_2022, "SPU_25I")
#' time_stopped_precise <- harmonized_data$SPU_25I
#' }
#' 
#' @export
calculate_SPU_25I <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SPU_25I') for implementation")
}

# ================================================================================

# TIME_QUIT_SMOKING - Combined cessation timeframe (harmonized across cycles)
# ================================================================================

#' @title Combined Smoking Cessation Timeframe - TIME_QUIT_SMOKING (continuous)
#' @description Implementation using 3-step architecture
#' 
#' Creates unified time since cessation variable combining all smoking cessation measures
#' across CCHS cycles 2001-2023. Prioritizes most precise available data source.
#' 
#' @details 
#' **Implementation Method**: 3-step architecture combining multiple sources
#' - **Step 1**: clean_variables() - Clean all cessation time inputs  
#' - **Step 2**: Missing data functions + domain logic - Prioritize data sources
#' - **Step 3**: Output cleaning using variable_details.csv
#' 
#' **Data source priority** (highest to lowest precision):
#' 1. SPU_25I (2022): Precise month-level calculation
#' 2. SMK_09A_cont (2001-2023): Continuous years for former daily smokers
#' 3. SMK_06A_cont (2001-2023): Continuous years for occasional/never daily smokers
#' 4. Categorical fallbacks: SMK_09A_B, SMK_06A_B with midpoint conversion
#' 
#' **Architecture Integration**:
#' - Combines all cessation timeframe variables into single harmonized measure
#' - Essential input for pack-years calculations and smoking exposure modeling
#' - Uses get_priority_missing() for proper missing data handling across sources
#' 
#' **Input Requirements**:
#' - SMK_09A_cont: Years since stopped daily smoking (former daily smokers)
#' - SMK_06A_cont: Years since stopped smoking (occasional/never daily smokers)
#' - SPU_25I: Precise cessation timing (2022 only)
#' 
#' @param SMK_09A_cont Numeric vector. Years since stopped daily smoking
#' @param SMK_06A_cont Numeric vector. Years since stopped smoking (occasional)
#' @param SPU_25I Numeric vector. Precise cessation timing (2022)
#' @param output_format Character. Output format ("tagged_na" or "original")
#' 
#' @return Vector of continuous years since cessation (0-50+ years, plus missing codes)
#' 
#' @examples
#' \dontrun{
#' # Scalar inputs - single respondent
#' result_scalar <- calculate_time_quit_smoking(
#'   SMK_09A_cont = 3.2, SMK_06A_cont = tagged_na("a"), SPU_25I = NA
#' )
#' # Returns: 3.2 (uses former daily smoker cessation time)
#' 
#' # Vector inputs - mixed smoking histories
#' smk_09a <- c(5.0, tagged_na("a"), 2.5, tagged_na("a"))  # Former daily cessation
#' smk_06a <- c(tagged_na("a"), 4.2, tagged_na("a"), 1.8)  # Occasional cessation  
#' spu_25i <- c(NA, NA, NA, NA)  # 2022 precise data (not available)
#' result_vector <- calculate_time_quit_smoking(smk_09a, smk_06a, spu_25i)
#' # Returns: c(5.0, 4.2, 2.5, 1.8) - uses best available for each respondent
#' 
#' # Priority demonstration with 2022 data
#' priority_test <- calculate_time_quit_smoking(
#'   SMK_09A_cont = c(5.0, 3.0),
#'   SMK_06A_cont = c(4.0, 2.0), 
#'   SPU_25I = c(5.1, NA)
#' )
#' # Returns: c(5.1, 3.0) - prioritizes SPU_25I precision when available
#' 
#' # Integration with rec_with_table() for CCHS harmonization
#' harmonized_data <- rec_with_table(cchs_data, "TIME_QUIT_SMOKING", 
#'                                   custom_function = calculate_time_quit_smoking)
#' }
#' 
#' @export
calculate_time_quit_smoking <- function(SMK_09A_cont, SMK_06A_cont, SPU_25I = NULL, 
                                       output_format = "tagged_na") {
  
  # Handle NULL SPU_25I (not available for most cycles)
  if (is.null(SPU_25I)) {
    SPU_25I <- rep(NA_real_, length(SMK_09A_cont))
  }
  
  # === STEP 1: DATA CLEANING and VALIDATION ===
  # Clean input variables (includes automatic length validation)
  cleaned <- clean_variables(vars = list(
    SMK_09A_cont = SMK_09A_cont,
    SMK_06A_cont = SMK_06A_cont,
    SPU_25I = SPU_25I
  ), output_format = output_format)
  
  # === STEP 2: DOMAIN LOGIC WITH MISSING DATA FUNCTIONS ===
  # Prioritize data sources by precision and availability
  result <- dplyr::case_when(
    # Highest priority: SPU_25I (2022 precise calculation)
    !any_missing(cleaned$SPU_25I) ~ cleaned$SPU_25I,
    
    # Second priority: SMK_09A_cont (former daily smokers)
    !any_missing(cleaned$SMK_09A_cont) ~ cleaned$SMK_09A_cont,
    
    # Third priority: SMK_06A_cont (occasional/never daily smokers)
    !any_missing(cleaned$SMK_06A_cont) ~ cleaned$SMK_06A_cont,
    
    # If all inputs are missing, get priority missing value
    any_missing(cleaned$SMK_09A_cont) & any_missing(cleaned$SMK_06A_cont) & any_missing(cleaned$SPU_25I) ~ 
      get_priority_missing(cleaned$SMK_09A_cont, cleaned$SMK_06A_cont, cleaned$SPU_25I, output_format = output_format),
    
    # Fallback: not applicable (shouldn't reach here with proper inputs)
    .default = assign_missing("not_applicable", "time_quit_smoking", output_format)
  )
  
  # === STEP 3: OUTPUT CLEANING ===
  # Apply validation bounds and constraints from variable_details.csv
  output_cleaned <- clean_variables(vars = list(
    time_quit_smoking = result
  ), output_format = output_format)
  
  return(output_cleaned$time_quit_smoking)
}