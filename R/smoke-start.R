# ================================================================================
# Age Started Smoking Classification Functions
# ================================================================================
#
# There are eight age started smoking variables that can be harmonized between 
# CCHS 2001 and 2023:
#
# 1. SMKG040_cat - "Age started to smoke daily - daily/former daily smoker"
#    CCHS cycles: 2015 � 2023 (6 categories)
#    Categories: 1=10 years or under, 2=11-14 years, 3=15-17 years, 4=18-19 years,
#               5=20-24 years, 6=25 years or over
#
# 2. SMKG040_cont - "Age started to smoke daily - daily/former daily smoker (continuous)"
#    CCHS cycles: 2015 � 2023 (continuous age values)
#    Values: Actual age when started smoking daily (numeric)
#
# 3. SMKG203_cat - "Age started to smoke daily - daily smoker"
#    CCHS cycles: 2015 � 2023 (6 categories)
#    Categories: 1=10 years or under, 2=11-14 years, 3=15-17 years, 4=18-19 years,
#               5=20-24 years, 6=25 years or over
#
# 4. SMKG203_cont - "Age started to smoke daily - daily smoker (continuous)"
#    CCHS cycles: 2001 � 2014 (continuous age values)
#    Values: Actual age when started smoking daily (numeric)
#
# 5. SMKG207_cat - "Age started to smoke daily - former daily smoker"
#    CCHS cycles: 2001 � 2014 (6 categories)
#    Categories: 1=10 years or under, 2=11-14 years, 3=15-17 years, 4=18-19 years,
#               5=20-24 years, 6=25 years or over
#
# 6. SMKG207_cont - "Age started to smoke daily - daily smoker (continuous)"
#    CCHS cycles: 2001 � 2014 (continuous age values)
#    Values: Actual age when started smoking daily (numeric)
#
# 7. SMK_207 - "Age started smoking daily"
#    CCHS cycles: 2001 � 2014 (continuous age values)
#    Values: Actual age when started smoking daily (numeric)
#
# 8. SMK_203 - "Age started smoking regularly"
#    CCHS cycles: 2001 � 2014 (continuous age values)
#    Values: Actual age when started smoking regularly (numeric)
#
# IMPLEMENTATION ORDER:
# - Variables 1-8: Direct harmonization via rec_with_table() (documentation-only initially)
# - Complex derivation may be needed for 2015+ cycles using SMK_040/SPU_15 + smoking status
#
# ================================================================================

# Package dependencies are declared in DESCRIPTION and loaded via NAMESPACE
# Functions used: haven::tagged_na(), haven::is_tagged_na(), dplyr::case_when()
# Internal functions: clean_variables(), any_missing(), get_priority_missing()

# ================================================================================

# SMKG040_cat - Age started smoking daily (6 categories) - DOCUMENTATION ONLY
# ================================================================================

#' @title Age Started Smoking Daily - SMKG040_cat (6 categories)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMKG040_cat variable across CCHS cycles 2015-2023.
#' This variable asks daily and former daily smokers when they started smoking daily,
#' categorized into 6 age groups.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2015-2021: SMK_040 (direct variable)
#'   - 2022-2023: SPU_15 (different underlying source, same harmonized structure)
#' - **Harmonization**: Simple 1:1 mapping with source variable transition
#' 
#' **Categories (6)**:
#' \itemize{
#'   \item 1 = 10 years or under
#'   \item 2 = 11-14 years
#'   \item 3 = 15-17 years
#'   \item 4 = 18-19 years
#'   \item 5 = 20-24 years
#'   \item 6 = 25 years or over
#' }
#' 
#' **Variable evolution**:
#' - **2015-2021**: Direct survey question SMK_040 "Age started smoking daily"
#' - **2022-2023**: Maps to SPU_15 variable (same question, different variable name)
#'   - Asked of daily and former daily smokers only
#'   - Provides age group categorization for smoking initiation patterns
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of age group classifications (1-6, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMKG040_cat")
#' age_started_daily_cat <- harmonized_data$SMKG040_cat
#' }
#' 
#' @export
calculate_SMKG040_cat <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMKG040_cat') for implementation")
}

# ================================================================================

# NOTE: calculate_SMKG040_cont() implementation is below (~line 506).
# It combines SMKG203_cont + SMKG207_cont. Doc stub removed to avoid
# name collision with the real implementation.

# ================================================================================

# SMKG203_cat - Age started smoking daily - daily smoker (categorical) - DOCUMENTATION ONLY
# ================================================================================

#' @title Age Started Smoking Daily - Daily Smoker - SMKG203_cat (categorical)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMKG203 categorical variables for CCHS cycles 2001-2014.
#' This variable asks daily smokers when they started smoking daily, with categorical age groups.
#' 
#' **Note**: Categorical versions have temporal variants:
#' - **SMKG203_pre2005** (2001-2003): 10 categories with broader age bins
#' - **SMKG203_2005plus** (2005-2014): 11 categories with finer age bins  
#' Most applications should use continuous **SMKG203_cont** instead.
#' 
#' @details 
#' **Implementation Method**: Complex derivation for 2015+ cycles
#' - **Input variables**: SMK_005 (smoking status) + SMK_040/SPU_15 (age started)
#' - **Logic**: Filter for former daily smokers (SMK_005 = 3 & SMK_030 = 1) then apply age categories
#' - **Architecture**: 3-step pattern with clean_variables() � domain logic � clean_variables()
#' 
#' **Categories (6)**:
#' \itemize{
#'   \item 1 = 10 years or under
#'   \item 2 = 11-14 years
#'   \item 3 = 15-17 years
#'   \item 4 = 18-19 years
#'   \item 5 = 20-24 years
#'   \item 6 = 25 years or over
#' }
#' 
#' **Complex derivation logic**:
#' - Apply to respondents where SMK_005 = 3 (not smoking) AND SMK_030 = 1 (formerly daily)
#' - Use SMK_040 (2015-2021) or SPU_15 (2022-2023) for age data
#' - Return missing for non-former-daily smokers
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of age group classifications (1-6, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation (2001-2014)
#' # or complex derivation function for 2015-2023
#' harmonized_data <- rec_with_table(cchs_data, "SMKG203_pre2005")  # or SMKG203_2005plus
#' age_started_daily_cat <- harmonized_data$SMKG203_pre2005
#' }
#' 
#' @export
calculate_SMKG203_cat <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMKG203_pre2005' or 'SMKG203_2005plus') for implementation")
}

# ================================================================================
# SMKG203_cont - Age started smoking daily - daily smoker (continuous)
# ================================================================================

#' @title Age Started Smoking Daily - Daily Smoker - SMKG203_cont (continuous)
#' @description Creates harmonized SMKG203_cont variable across CCHS cycles 2015-2023.
#' This variable provides continuous age when daily smokers started smoking daily.
#' 
#' @details 
#' **Implementation method**: 3-step architecture
#' - **Step 1**: Clean_variables() - Clean SMK_005 and SMKG040_cont inputs
#' - **Step 2**: Missing data functions + domain logic - Filter by SMK_005 = 1 (daily smoker)
#' - **Step 3**: Categorical-to-continuous conversion using variable_details.csv
#' 
#' **Legacy logic pattern** (from variable_details.csv):
#' - Filter: SMK_005 == 1 → use SMKG040_cont 
#' - Missing: SMK_005 == 'NA(a)' | SMKG040_cont == 'NA(a)' → return 'NA(a)'
#' - Otherwise → return 'NA(b)'
#' 
#' **Architecture improvements**:
#' - Uses variable_details.csv recStart/recEnd mappings for conversion
#' - Supports both 2001-2014 direct variables and 2015+ derivation
#' 
#' **Input requirements**:
#' - SMK_005: Current smoking status (1 = daily, 2 = occasional, 3 = not at all)
#' - SMKG040_cont: Continuous age started smoking daily (5-121 years)
#' 
#' @param SMK_005 Numeric vector. Current smoking status
#' @param SMKG040_cont Numeric vector. Continuous age started smoking daily  
#' @param output_format Character. Output format ("tagged_na" or "original")
#' 
#' @return Vector of continuous age values for daily smokers (5-121 years, plus missing codes)
#' 
#' @examples
#' \dontrun{
#' # Scalar inputs - single respondent
#' result_scalar <- calculate_SMKG203_cont(SMK_005 = 1, SMKG040_cont = 18.5)
#' # Returns: 18.5 (daily smoker gets age)
#' 
#' # Vector inputs - multiple respondents  
#' smk_005 <- c(1, 2, 3, 1, 997, 999)  # daily, occasional, non-smoker, daily, missing, missing
#' ages <- c(16.5, 20.0, 25.0, 22.0, 16.0, 30.0)
#' result_vector <- calculate_SMKG203_cont(smk_005, ages)
#' # Returns: c(16.5, NA::a, NA::a, 22.0, NA::b, NA::b)
#' 
#' # Different output formats
#' result_tagged <- calculate_SMKG203_cont(smk_005, ages, "tagged_na")
#' # Returns tagged_na for missing (haven format)
#' 
#' result_original <- calculate_SMKG203_cont(smk_005, ages, "original") 
#' # Returns original CCHS missing codes (996, 999, etc.)
#' 
#' # Integration with rec_with_table() for CCHS harmonization
#' harmonized_data <- rec_with_table(cchs_data, "SMKG203_cont", 
#'                                   custom_function = calculate_SMKG203_cont)
#' }
#' 
#' @export
calculate_SMKG203_cont <- function(SMK_005, SMKG040_cont, output_format = "tagged_na") {
  
  # === STEP 1: DATA CLEANING and VALIDATION ===
  # Clean input variables (includes automatic length validation)
  cleaned <- clean_variables(vars = list(
    SMK_005 = SMK_005,
    SMKG040_cont = SMKG040_cont
  ), output_format = "tagged_na")
  
  # === STEP 2: DOMAIN LOGIC WITH MISSING DATA FUNCTIONS ===
  # Apply SMK_005 filtering logic from variable_details.csv dependency pattern
  result <- dplyr::case_when(
    # Missing gate variable — propagate
    any_missing(cleaned$SMK_005) ~
      get_priority_missing(cleaned$SMK_005, output_format = output_format),

    # Domain logic: Filter by SMK_005 = 1 (daily smokers)
    cleaned$SMK_005 == 1 & !any_missing(cleaned$SMKG040_cont) ~ cleaned$SMKG040_cont,
    cleaned$SMK_005 == 1 & any_missing(cleaned$SMKG040_cont) ~
      get_priority_missing(cleaned$SMKG040_cont, output_format = output_format),

    # Non-daily smokers get missing value (not applicable)
    .default = assign_missing("not_applicable", "SMKG203_cont", output_format)
  )
  
  # === STEP 3: OUTPUT CLEANING ===
  # Apply validation bounds and constraints from variable_details.csv
  output_cleaned <- clean_variables(vars = list(
    SMKG203_cont = result
  ), output_format = output_format)
  
  return(output_cleaned$SMKG203_cont)
}

# ================================================================================
# SMKG207_cat - Age started smoking daily - former daily smoker (categorical) - DOCUMENTATION ONLY
# ================================================================================

#' @title Age Started Smoking Daily - Former Daily Smoker - SMKG207_cat (categorical)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMKG207 categorical variables across CCHS cycles 2001-2014.
#' This variable asks former daily smokers when they started smoking daily, with categorical age groups.
#' 
#' **Note**: Categorical versions have temporal variants:
#' - **SMKG207_pre2005** (2001-2003): 10 categories with broader age bins
#' - **SMKG207_2005plus** (2005-2014): 11 categories with finer age bins
#' Most applications should use continuous **SMKG207_cont** instead.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2001-2014: Cycle-specific variables (SMKG207_pre2005/B direct)
#' - **Harmonization**: Simple 1:1 mapping across early cycles
#' - **Availability**: 2001-2014 only (replaced by complex derivation in 2015+)
#' 
#' **Categories (6)**:
#' \itemize{
#'   \item 1 = 10 years or under
#'   \item 2 = 11-14 years
#'   \item 3 = 15-17 years
#'   \item 4 = 18-19 years
#'   \item 5 = 20-24 years
#'   \item 6 = 25 years or over
#' }
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of age group classifications (1-6, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMKG207_pre2005")  # or SMKG207_2005plus 
#' age_started_former_daily_pre2015 <- harmonized_data$SMKG207_pre2005
#' }
#' 
#' @export
calculate_SMKG207_cat <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMKG207_pre2005' or 'SMKG207_2005plus') for implementation")
}

# ================================================================================

# SMKG207_cont - Age started smoking daily - former daily smoker (continuous) - LEVEL 7 IMPLEMENTATION
# ================================================================================

#' @title Age Started Smoking Daily - Former Daily Smoker - SMKG207_cont (continuous)
#' @description Implementation using 3-step architecture
#' 
#' Creates harmonized SMKG207_cont variable across CCHS cycles 2015-2023.
#' This variable provides continuous age when former daily smokers started smoking daily.
#' Uses Level 7 categorical-to-continuous conversion from variable_details.csv.
#' 
#' @details 
#' **Implementation Method**: 3-step architecture
#' - **Step 1**: clean_variables() - Clean SMK_030 and SMKG040_cont inputs
#' - **Step 2**: Missing data functions + domain logic - Filter by SMK_030 = 1 (former daily)
#' - **Step 3**: Output cleaning using variable_details.csv
#' 
#' **Legacy Logic Pattern** (from SMK function diagrams):
#' - Filter: SMK_030 == 1 � use SMKG040_cont 
#' - Missing: SMK_030 == 'NA(a)' | SMKG040_cont == 'NA(a)' � return 'NA(a)'
#' - Otherwise � return 'NA(b)'
#' 
#' **Architecture Improvements**:
#' - Replaces hard-coded categorical-to-continuous mapping (1�8, 2�13, etc.)
#' - Uses variable_details.csv recStart/recEnd mappings for conversion
#' - Applies missing code preprocessing and output cleaning
#' 
#' **Input Requirements**:
#' SMK_030: Former daily smoking status (1 = formerly smoked daily, 
#' 2 = did not formerly smoke daily)
#' - SMKG040_cont: Continuous age started smoking daily (5-95 years)
#' 
#' @param SMK_030 Numeric vector. Former daily smoking status
#' @param SMKG040_cont Numeric vector. Continuous age started smoking daily  
#' @param output_format Character. Output format ("tagged_na" or "original")
#' 
#' @return Vector of continuous age values for former daily smokers (5-95 years, plus missing codes)
#' 
#' @examples
#' \dontrun{
#' # Scalar inputs - single respondent
#' result_scalar <- calculate_SMKG207_cont(SMK_030 = 1, SMKG040_cont = 25.0)
#' # Returns: 25.0 (former daily smoker gets age)
#' 
#' # Vector inputs - multiple respondents
#' smk_030 <- c(1, 2, 1, 997, 999)  # former daily, never daily, former daily, missing, missing
#' ages <- c(25.0, 28.0, 30.0, 20.0, 35.0)
#' result_vector <- calculate_SMKG207_cont(smk_030, ages)
#' # Returns: c(25.0, NA::a, 30.0, NA::b, NA::b)
#'
#' # Different output formats 
#' result_tagged <- calculate_SMKG207_cont(smk_030, ages, "tagged_na")
#' # Returns tagged_na for missing (haven format)
#' 
#' result_original <- calculate_SMKG207_cont(smk_030, ages, "original")
#' # Returns original CCHS missing codes (996, 999, etc.)
#' 
#' # Integration with rec_with_table() for CCHS harmonization
#' harmonized_data <- rec_with_table(cchs_data, "SMKG207_cont", 
#'                                   custom_function = calculate_SMKG207_cont)
#' }
#' 
#' @export
calculate_SMKG207_cont <- function(SMK_030, SMKG040_cont, output_format = "tagged_na") {
  
  # === STEP 1: DATA CLEANING and VALIDATION ===
  # Clean input variables (includes automatic length validation)
  cleaned <- clean_variables(vars = list(
    SMK_030 = SMK_030,
    SMKG040_cont = SMKG040_cont
  ), output_format = "tagged_na")
  
  # === STEP 2: DOMAIN LOGIC WITH MISSING DATA FUNCTIONS ===
  # Apply SMK_030 filtering logic from SMK function diagrams
  result <- dplyr::case_when(
    # Missing gate variable — propagate
    any_missing(cleaned$SMK_030) ~
      get_priority_missing(cleaned$SMK_030, output_format = output_format),

    # Domain logic: Filter by SMK_030 = 1 (former daily smokers)
    cleaned$SMK_030 == 1 & !any_missing(cleaned$SMKG040_cont) ~ cleaned$SMKG040_cont,
    cleaned$SMK_030 == 1 & any_missing(cleaned$SMKG040_cont) ~
      get_priority_missing(cleaned$SMKG040_cont, output_format = output_format),

    # Non-former daily smokers get missing value (not applicable)
    .default = assign_missing("not_applicable", "SMKG207_cont", output_format)
  )
  
  # === STEP 3: OUTPUT CLEANING ===
  # Apply validation bounds and constraints from variable_details.csv
  output_cleaned <- clean_variables(vars = list(
    SMKG207_cont = result
  ), output_format = output_format)
  
  return(output_cleaned$SMKG207_cont)
}

# ================================================================================

# SMKG040_cont - Age started smoking daily - daily/former daily smoker (continuous) - LEVEL 7 IMPLEMENTATION
# ================================================================================

#' @title Age Started Smoking Daily - Daily/Former Daily Smoker - SMKG040_cont (continuous)
#' @description Implementation using 3-step architecture
#' 
#' Creates harmonized SMKG040_cont variable across CCHS cycles 2001-2014.
#' This variable combines SMKG203_cont (daily smokers) and SMKG207_cont (former daily smokers)
#' to provide age started smoking daily for both groups.
#' 
#' @details 
#' **Implementation Method**: 3-step architecture combining derived variables
#' - **Step 1**: clean_variables() - Clean SMKG203_cont and SMKG207_cont inputs
#' - **Step 2**: Missing data functions + domain logic - Combine daily and former daily data
#' - **Step 3**: Output cleaning using variable_details.csv
#' 
#' **Logic Pattern** (from variable_details.csv dependency):
#' - Use SMKG203_cont where available (daily smokers)
#' - Use SMKG207_cont where available (former daily smokers)  
#' - Priority hierarchy: valid data > missing data (NA::a > NA::b)
#' 
#' **Architecture Integration**:
#' - For 2001-2014: Combines SMKG203_cont + SMKG207_cont (this function)
#' - For 2015+: Direct from SMK_040/SPU_15 via rec_with_table()
#' - Uses get_priority_missing() for proper missing data handling
#' 
#' **Input Requirements**:
#' - SMKG203_cont: Age started smoking daily - daily smokers only
#' - SMKG207_cont: Age started smoking daily - former daily smokers only
#' 
#' @param SMKG203_cont Numeric vector. Age started smoking daily for daily smokers
#' @param SMKG207_cont Numeric vector. Age started smoking daily for former daily smokers
#' @param output_format Character. Output format ("tagged_na" or "original")
#' 
#' @return Vector of continuous age values for daily/former daily smokers (5-121 years, plus missing codes)
#' 
#' @examples
#' \dontrun{
#' # Scalar inputs - single respondent
#' result_scalar <- calculate_SMKG040_cont(SMKG203_cont = 18.5, SMKG207_cont = tagged_na("a"))
#' # Returns: 18.5 (uses daily smoker age when available)
#' 
#' # Vector inputs - mutually exclusive data (realistic scenario)
#' smkg203 <- c(18.5, tagged_na("a"), 22.0, tagged_na("a"))  # Daily smokers' ages
#' smkg207 <- c(tagged_na("a"), 25.0, tagged_na("a"), tagged_na("b"))  # Former daily smokers' ages  
#' result_vector <- calculate_SMKG040_cont(smkg203, smkg207)
#' # Returns: c(18.5, 25.0, 22.0, NA::b)
#' 
#' # Different output formats
#' result_tagged <- calculate_SMKG040_cont(smkg203, smkg207, "tagged_na")
#' # Returns tagged_na for missing (haven format)
#' 
#' result_original <- calculate_SMKG040_cont(smkg203, smkg207, "original")
#' # Returns original CCHS missing codes (996, 999, etc.)
#' 
#' # Priority hierarchy demonstration  
#' priority_test <- calculate_SMKG040_cont(
#'   SMKG203_cont = c(tagged_na("a"), tagged_na("b")),
#'   SMKG207_cont = c(tagged_na("b"), tagged_na("a"))
#' )
#' # Returns: c(NA::b, NA::b) - prioritizes "not_stated" over "not_applicable"
#' 
#' # Integration with rec_with_table() for 2001-2014 CCHS harmonization
#' harmonized_data <- rec_with_table(cchs_data, "SMKG040_cont", 
#'                                   custom_function = calculate_SMKG040_cont)
#' }
#' 
#' @export
calculate_SMKG040_cont <- function(SMKG203_cont, SMKG207_cont, output_format = "tagged_na") {
  
  # === STEP 1: DATA CLEANING and VALIDATION ===
  # Clean input variables (includes automatic length validation)
  cleaned <- clean_variables(vars = list(
    SMKG203_cont = SMKG203_cont,
    SMKG207_cont = SMKG207_cont
  ), output_format = "tagged_na")
  
  # === STEP 2: DOMAIN LOGIC WITH MISSING DATA FUNCTIONS ===
  # Combine daily and former daily smoker ages using priority hierarchy
  result <- dplyr::case_when(
    # If both inputs are missing, get priority missing value
    any_missing(cleaned$SMKG203_cont) & any_missing(cleaned$SMKG207_cont) ~ 
      get_priority_missing(cleaned$SMKG203_cont, cleaned$SMKG207_cont, output_format = output_format),
    
    # Domain logic: Use whichever has valid data (mutually exclusive in real data)
    !any_missing(cleaned$SMKG203_cont) ~ cleaned$SMKG203_cont,  # Daily smokers
    !any_missing(cleaned$SMKG207_cont) ~ cleaned$SMKG207_cont,  # Former daily smokers
    
    # Fallback: not applicable (shouldn't reach here with proper inputs)
    .default = assign_missing("not_applicable", "SMKG040_cont", output_format)
  )
  
  # === STEP 3: OUTPUT CLEANING ===
  # Apply validation bounds and constraints from variable_details.csv
  output_cleaned <- clean_variables(vars = list(
    SMKG040_cont = result
  ), output_format = output_format)
  
  return(output_cleaned$SMKG040_cont)
}

# ================================================================================

# SMK_207 - Age started smoking daily - DOCUMENTATION ONLY
# ================================================================================

#' @title Age Started Smoking Daily - SMK_207 (continuous)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMK_207 variable across CCHS cycles 2001-2014.
#' This is the primary age started smoking daily variable for pre-2015 cycles,
#' providing continuous age values.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2001-2014: Cycle-specific variables (SMK_207 direct)
#' - **Harmonization**: Simple 1:1 mapping across early cycles
#' - **Availability**: 2001-2014 only (replaced by SMK_040/SPU_15 in 2015+)
#' 
#' **Values**: Continuous age values (numeric)
#' - Range typically 5-80+ years
#' - Primary source for age started smoking daily analysis in pre-2015 data
#' - Foundation variable for derived categorical versions
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of continuous age values (numeric, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMK_207")
#' age_started_smoking_daily <- harmonized_data$SMK_207
#' }
#' 
#' @export
calculate_SMK_207 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_207') for implementation")
}

# ================================================================================

# SMK_203 - Age started smoking regularly - DOCUMENTATION ONLY
# ================================================================================

#' @title Age Started Smoking Regularly - SMK_203 (continuous)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMK_203 variable across CCHS cycles 2001-2014.
#' This variable asks when respondents started smoking regularly (not necessarily daily),
#' providing continuous age values. Pre-2015 cycles only.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2001-2014: Cycle-specific variables (SMK_203 direct)
#' - **Harmonization**: Simple 1:1 mapping across early cycles
#' - **Availability**: 2001-2014 only (discontinued in 2015+)
#' 
#' **Values**: Continuous age values (numeric)
#' - Range typically 5-80+ years
#' - Asked about regular smoking (broader than daily smoking)
#' - Complements SMK_207 (daily smoking) for smoking initiation patterns
#' 
#' **Key difference from SMK_207**:
#' - SMK_203: Age started smoking regularly (any regular pattern)
#' - SMK_207: Age started smoking daily (specifically daily pattern)
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of continuous age values (numeric, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMK_203")
#' age_started_smoking_regularly <- harmonized_data$SMK_203
#' }
#' 
#' @export
calculate_SMK_203 <- function(data, output_format = "tagged_na") {

  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_203') for implementation")
}

# ================================================================================
# age_start_smoking - Unified Age Started Smoking Daily (Derived Variable)
# ================================================================================

#' @title Unified Age Started Smoking Daily - age_start_smoking
#' @description Creates unified age_start_smoking derived variable across
#'   CCHS cycles.
#'
#' Receives a single continuous age input — the worksheet routes the
#' appropriate source variable depending on database type:
#' - **PUMF**: SMKG040_cont (midpoint estimation from grouped categories)
#' - **Master**: SMK_040 (exact continuous age)
#'
#' @details
#' **Implementation method**: 3-step architecture (single-source pass-through)
#' - **Step 1**: clean_variables() - Clean continuous age input
#' - **Step 2**: Direct pass-through (source routing handled by worksheet)
#' - **Step 3**: Output cleaning with age_start_smoking metadata
#'
#' **Coverage**:
#' - PUMF: 2001-2021 via SMKG040_cont (midpoint imputation)
#' - Master: 2001-2023 via SMK_040 (exact continuous)
#'
#' **Universe**: Ever-daily smokers (SMKDSTY 1, 2, 4)
#' - Returns NA::a for never-daily smokers (SMKDSTY 3, 5, 6)
#' - Returns NA::b for missing input data
#'
#' @param age_start_smoking Numeric. Age started smoking daily (continuous).
#'   The worksheet routes the appropriate source variable:
#'   PUMF provides SMKG040_cont, Master provides SMK_040.
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Numeric vector of age started smoking daily (continuous, plus missing codes)
#'
#' @examples
#' \dontrun{
#' # Single value
#' calculate_age_start_smoking(18)
#' # Returns: 18
#'
#' # Vector with missing values
#' calculate_age_start_smoking(c(18, 25, NA, 16))
#'
#' # Tagged NA pass-through (never-daily smoker)
#' calculate_age_start_smoking(haven::tagged_na("a"))
#' # Returns: NA::a (not applicable)
#'
#' # NULL input returns NA::b (missing)
#' calculate_age_start_smoking(NULL)
#' }
#'
#' @export
calculate_age_start_smoking <- function(age_start_smoking = NULL,
                                        output_format = "tagged_na") {
  derive_passthrough(age_start_smoking, "age_start_smoking", output_format)
}

# ================================================================================
# age_first_cigarette - Unified Age Smoked First Cigarette (Derived Variable)
# ================================================================================

#' @title Unified Age Smoked First Whole Cigarette - age_first_cigarette
#' @description Creates unified age_first_cigarette derived variable across
#'   CCHS cycles.
#'
#' Receives a single continuous age input — the worksheet routes the
#' appropriate source variable depending on database type:
#' - **PUMF**: SMKG01C_cont (midpoint estimation from grouped categories)
#' - **Master**: SMK_01C (exact continuous age)
#'
#' @details
#' **Implementation method**: 3-step architecture (single-source pass-through)
#' - **Step 1**: clean_variables() - Clean continuous age input
#' - **Step 2**: Direct pass-through (source routing handled by worksheet)
#' - **Step 3**: Output cleaning with age_first_cigarette metadata
#'
#' **Coverage**:
#' - PUMF: 2001-2021 via SMKG01C_cont (midpoint imputation)
#' - Master: 2001-2023 via SMK_01C (exact continuous)
#'
#' **Universe**: Ever smoked 100+ cigarettes in lifetime (SMK_01A == 1).
#'
#' @param age_first_cigarette Numeric vector. Continuous age first smoked a
#'   whole cigarette. Source depends on database type: SMK_01C (Master) or
#'   SMKG01C_cont (PUMF). NULL if not available.
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Numeric vector of continuous age values (8-95), with:
#' - NA::a for never-smokers / not applicable
#' - NA::b for missing/refused
#'
#' @examples
#' \dontrun{
#' # Single value
#' calculate_age_first_cigarette(14)    # Returns: 14
#'
#' # Vector input with missing values
#' calculate_age_first_cigarette(c(14, 16, NA, 12))
#' # Returns: c(14, 16, NA::b, 12)
#'
#' # Tagged NA pass-through
#' calculate_age_first_cigarette(tagged_na("a"))  # NA::a (not applicable)
#'
#' # NULL input (variable not in dataset)
#' calculate_age_first_cigarette(NULL)  # NA::b (not stated)
#' }
#'
#' @export
calculate_age_first_cigarette <- function(age_first_cigarette = NULL,
                                          output_format = "tagged_na") {
  derive_passthrough(age_first_cigarette, "age_first_cigarette", output_format)
}

# ================================================================================
# smoked_100_lifetime - Ever Smoked 100+ Cigarettes (Derived Variable)
# ================================================================================

#' @title Ever Smoked 100 or More Cigarettes in Lifetime - smoked_100_lifetime
#' @description Creates unified smoked_100_lifetime derived variable across CCHS cycles.
#'
#' This is a pass-through wrapper for SMK_01A, providing a self-documenting
#' variable name for standalone research use. The underlying source (SMK_01A)
#' is already harmonised across cycles in the worksheets.
#'
#' @details
#' **Implementation method**: 3-step architecture (single-source pass-through)
#' - **Step 1**: clean_variables() - Clean categorical input
#' - **Step 2**: Direct pass-through (1=yes, 2=no)
#' - **Step 3**: Output cleaning with smoked_100_lifetime metadata
#'
#' **Source**: SMK_01A across all cycles (already harmonised via worksheets).
#'
#' **Coverage**:
#' - PUMF: 2001-2021
#' - Master: 2001-2023
#'
#' **Universe**: Respondents who have smoked at least one whole cigarette.
#'
#' @param smoked_100_lifetime Numeric. 1=Yes (100+), 2=No, with missing codes.
#'   The worksheet routes SMK_01A from the appropriate database.
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Numeric vector: 1=Yes, 2=No, with:
#' - NA::a for not applicable (never tried a cigarette)
#' - NA::b for missing/refused
#'
#' @examples
#' \dontrun{
#' # Single value
#' calculate_smoked_100_lifetime(1)  # Returns: 1 (yes, 100+ cigarettes)
#' calculate_smoked_100_lifetime(2)  # Returns: 2 (no)
#'
#' # Vector input with missing values
#' calculate_smoked_100_lifetime(c(1, 2, 1, NA))
#'
#' # Tagged NA pass-through
#' calculate_smoked_100_lifetime(haven::tagged_na("a"))  # NA::a (not applicable)
#'
#' # NULL input (no data available)
#' calculate_smoked_100_lifetime(NULL)  # NA::b (not stated)
#' }
#'
#' @export
calculate_smoked_100_lifetime <- function(smoked_100_lifetime = NULL,
                                          output_format = "tagged_na") {
  prep_cat_output(derive_passthrough(smoked_100_lifetime, "smoked_100_lifetime", output_format))
}
