# ================================================================================
# Smoking Status Classification Functions
# ================================================================================
#
# There are six smoking classification variables that can be harmonized between
# CCHS 2001 and 2023:
#
# 1. SMKDSTY_cat6 - "Type of smoker: daily, occasional (former daily), occasional (never daily),
#                   former daily, former occasional, never" (harmonized 6-category with pre-2015 semantics)
#    CCHS cycles: 2001-2021, 2023 PUMF; 2001-2023 Master (6 categories)
#    Categories: 1=Daily smoker, 2=Occasional (former daily), 3=Always occasional (never daily),
#               4=Former daily, 5=Former occasional, 6=Never smoked
#    NOTE: 2022 PUMF gap - SPU_05 only asked of daily smokers, cannot derive cat2 vs cat3
#
# 2. SMKDSTY_cat5 - "Type of smoker: daily, occasional, former daily, former occasional, never"
#    CCHS cycles: 2001 → 2023 (5 categories, full coverage)
#    Categories: 1=Daily, 2=Occasional, 3=Former daily, 4=Former other, 5=Never
#    NOTE: Merges cat6 categories 2+3, handles 2015 semantic break in category 5
#
# 3. SMKDSTY_cat3 - "Type of smoker: current, former, never"
#    CCHS cycles: 2001 → 2023 (3 categories, full coverage)
#    Categories: 1=Current smoker, 2=Former smoker, 3=Never smoked
#
# 4. SMK_005 - "Type of smoker presently"
#    CCHS cycles: 2015 → 2023 (3 categories)
#    Categories: 1=Daily, 2=Occasionally, 3=Not at all
#
# 5. SMK_030 - "Smoked daily - lifetime (occasional/former smoker)"
#    CCHS cycles: 2015 → 2023 (2 categories)
#    Categories: 1=Yes, 2=No
#
# 6. SMK_01A - "In lifetime, smoked 100 or more cigarettes"
#    CCHS cycles: 2001 → 2023 (2 categories)
#    Categories: 1=Yes, 2=No
#
# IMPLEMENTATION ORDER:
# - Variables 2-6: Simple harmonization via rec_with_table() (documentation-only initially)
# - Variable 1 (SMKDSTY_cat6): Complex derivation requiring SMK_005 + SMK_030 + SMK_01A for 2015+

# ================================================================================

# Package dependencies are declared in DESCRIPTION and loaded via NAMESPACE
# Functions used: haven::tagged_na(), haven::is_tagged_na(), dplyr::case_when()
# Internal functions: clean_variables(), any_missing(), get_priority_missing()


# ================================================================================

# SMKDSTY_cat5 - Smoking status (5 categories) - DOCUMENTATION ONLY
# ================================================================================

#' @title Smoking Status Classification - SMKDSTY_cat5 (5 categories)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMKDSTY_cat5 variable across CCHS cycles 2001-2023.
#' This is a 5-category smoking status variable that collapses some categories
#' from the 6-category versions for simplified analysis.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2001-2014: SMKDSTY (SMKADSTY/SMKCDSTY/SMKEDSTY)
#'   - 2015-2023: SMKDVSTY
#' - **Harmonization**: Simple mapping with category consolidation
#' 
#' **Categories (5)**:
#' \itemize{
#'   \item 1 = Current daily smoker
#'   \item 2 = Current occasional smoker
#'   \item 3 = Former daily smoker
#'   \item 4 = Former occasional
#'   \item 5 = Never smoked
#' }
#' 
#' **Category consolidation**:
#' - Pre-2015: "Occasional" and "Always occasional" combined → Category 2
#' - 2015+: "Former occasional" and "Experimental" combined → Category 4
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of smoking status classifications (1-5, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMKDSTY_cat5")
#' smoking_status_5cat <- harmonized_data$SMKDSTY_cat5
#' }
#' 
#' @export
calculate_SMKDSTY_cat5 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMKDSTY_cat5') for implementation")
}

# ================================================================================

# SMKDSTY_cat3 - Smoking status (3 categories) - DOCUMENTATION ONLY
# ================================================================================

#' @title Smoking Status Classification - SMKDSTY_cat3 (3 categories)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMKDSTY_cat3 variable across CCHS cycles 2001-2023.
#' This is a 3-category smoking status variable that provides the most simplified
#' classification for broad population analysis.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2001-2014: SMKDSTY (SMKADSTY/SMKCDSTY/SMKEDSTY)
#'   - 2015-2023: SMKDVSTY
#' - **Harmonization**: Simple mapping with major category consolidation
#' 
#' **Categories (3)**:
#' \itemize{
#'   \item 1 = Current smoker
#'   \item 2 = Former smoker
#'   \item 3 = Never smoked
#' }
#' 
#' **Category consolidation**:
#' - **Current smoker (1)**: Combines Daily + Occasional categories
#'   - Pre-2015: Daily (1) + Occasional (2) + Always occasional (3) → 1
#'   - 2015+: Daily (1) + Occasional (2) → 1
#' - **Former smoker (2)**: Combines all former smoking categories
#'   - Pre-2015: Former daily (4) + Former occasional (5) → 2
#'   - 2015+: Former daily (3) + Former occasional (4) + Experimental (5) → 2
#' - **Never smoked (3)**: Remains as category 6 → 3
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of smoking status classifications (1-3, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMKDSTY_cat3")
#' smoking_status_3cat <- harmonized_data$SMKDSTY_cat3
#' }
#' 
#' @export
calculate_SMKDSTY_cat3 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMKDSTY_cat3') for implementation")
}

# ================================================================================

# SMK_005 - Type of smoker presently (3 categories) - DOCUMENTATION ONLY
# ================================================================================

#' @title Type of Smoker Presently - SMK_005 (3 categories)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMK_005 variable across CCHS cycles 2015-2023.
#' This variable asks about current smoking behavior and serves as a key
#' component for SMKDSTY_original reconstruction in the 2015-2023 period.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2015-2021: SMK_005 (direct variable)
#'   - 2022-2023: Derived from SMKDVSTY (1→1, 2→2, [3-6]→3)
#' - **Harmonization**: Simple 1:1 mapping (2015-2021) or derivation (2022-2023)
#' 
#' **Categories (3)**:
#' \itemize{
#'   \item 1 = Daily
#'   \item 2 = Occasionally  
#'   \item 3 = Not at all
#' }
#' 
#' **Variable evolution**:
#' - **2015-2021**: Direct survey question "Type of smoker presently"
#' - **2022-2023**: Derived from SMKDVSTY to maintain harmonization
#'   - SMKDVSTY 1 (Daily) → SMK_005 1 (Daily)
#'   - SMKDVSTY 2 (Occasional) → SMK_005 2 (Occasionally)
#'   - SMKDVSTY 3-6 (All non-smokers) → SMK_005 3 (Not at all)
#' 
#' **Usage**: Primary input for SMKDSTY_original complex reconstruction function
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of current smoking status (1-3, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMK_005")
#' current_smoking <- harmonized_data$SMK_005
#' }
#' 
#' @export
calculate_SMK_005 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_005') for implementation")
}

# ================================================================================

# SMK_030 - Smoked daily - lifetime (2 categories) - DOCUMENTATION ONLY
# ================================================================================

#' @title Smoked Daily - Lifetime - SMK_030 (2 categories)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMK_030 variable across CCHS cycles 2015-2023.
#' This variable asks whether occasional/former smokers ever smoked daily
#' and serves as a key component for SMKDSTY_original reconstruction.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2015-2021: SMK_030 (direct variable)
#'   - 2022-2023: SPU_05 (different underlying source, same harmonized name)
#' - **Harmonization**: Simple 1:1 mapping with source variable transition
#' 
#' **Categories (2)**:
#' \itemize{
#'   \item 1 = Yes
#'   \item 2 = No
#' }
#' 
#' **Variable evolution**:
#' - **2015-2021**: Direct survey question SMK_030 "Smoked daily - lifetime"
#' - **2022-2023**: Maps to SPU_05 variable (same question, different variable name)
#'   - Asked of occasional/former smokers to determine daily smoking history
#'   - Critical for distinguishing "former daily" vs "never daily" categories
#' 
#' **Usage**: 
#' - Second input for SMKDSTY_original complex reconstruction function
#' - Determines occasional smoker subcategories (former daily vs never daily)
#' - Used in conjunction with SMK_005 to classify smoking patterns
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of daily smoking history (1-2, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMK_030")
#' daily_history <- harmonized_data$SMK_030
#' }
#' 
#' @export
calculate_SMK_030 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_030') for implementation")
}

# ================================================================================

# SMK_01A - In lifetime, smoked 100 or more cigarettes (2 categories) - DOCUMENTATION ONLY
# ================================================================================

#' @title Lifetime 100+ Cigarettes - SMK_01A (2 categories)
#' @description DOCUMENTATION ONLY - Use rec_with_table() for implementation
#' 
#' Creates harmonized SMK_01A variable across CCHS cycles 2001-2023.
#' This variable asks about lifetime cigarette consumption threshold (100+ cigarettes)
#' and serves as the final component for SMKDSTY_original reconstruction and smoking classification.
#' 
#' @details 
#' **Implementation Method**: Direct harmonization via rec_with_table()
#' - **Source variables**: 
#'   - 2001-2014: Cycle-specific variables (SMKA_01A, SMKC_01A, SMKE_01A, SMK_01A)
#'   - 2015-2021: SMK_020 (direct variable)
#'   - 2022-2023: CSS_15 (different underlying source, same harmonized structure)
#' - **Harmonization**: Simple 1:1 mapping across all periods with source transitions
#' 
#' **Categories (2)**:
#' \itemize{
#'   \item 1 = Yes
#'   \item 2 = No
#' }
#' 
#' **Variable evolution**:
#' - **2001-2014**: Cycle-specific variable names, consistent question
#' - **2015-2021**: Standardized as SMK_020 "In lifetime, smoked 100 or more cigarettes"
#' - **2022-2023**: Maps to CSS_15 variable (same question, different variable name)
#'   - 100+ cigarette threshold distinguishes experimental from never smokers
#'   - Critical for "former occasional" vs "never smoker" classification
#' 
#' **Usage**: 
#' - Third input for SMKDSTY_original complex reconstruction function
#' - Distinguishes experimental/former occasional smokers from never smokers
#' - Used with SMK_005 and SMK_030 to determine final smoking status categories
#' - Longest-running harmonized smoking variable (2001-2023 coverage)
#' 
#' @param data Data frame containing CCHS data
#' @param output_format Character. Output format for missing values ("tagged_na" or "standard")
#' 
#' @return Vector of 100+ cigarette lifetime status (1-2, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Use rec_with_table() for actual implementation
#' harmonized_data <- rec_with_table(cchs_data, "SMK_01A")
#' lifetime_100plus <- harmonized_data$SMK_01A
#' }
#' 
#' @export
calculate_SMK_01A <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_01A') for implementation")
}

# ================================================================================

# SMKDSTY_cat6 - Type of smoker (6 categories) - COMPLEX DERIVATION
# ================================================================================

#' @title Type of Smoker - SMKDSTY_cat6 (6 categories, harmonized)
#' @description Complex derived variable using 3-step architecture
#'
#' Creates harmonized SMKDSTY_cat6 variable for CCHS cycles 2015+ by applying
#' the pre-2015 SMKDSTY logic to harmonized variables SMK_005, SMK_030, and SMK_01A.
#' For 2001-2014, uses direct SMKDSTY variable via rec_with_table().
#'
#' @details
#' **Implementation Method**: Complex derivation for 2015+ cycles
#' - **Input variables**: SMK_005 (current status), SMK_030 (ever daily), SMK_01A (100+ cigs)
#' - **Architecture**: 3-step pattern with clean_variables() → domain logic → clean_variables()
#' - **Logic source**: CCHS 2013-2014 SMKDSTY specifications applied to harmonized variables
#'
#' **Categories (6)**:
#' \itemize{
#'   \item 1 = Daily smoker
#'   \item 2 = Occasional smoker (former daily smoker)
#'   \item 3 = Occasional smoker (never a daily smoker)
#'   \item 4 = Former daily smoker (non-smoker now)
#'   \item 5 = Former occasional smoker (at least 1 whole cigarette, non-smoker now)
#'   \item 6 = Never smoked (a whole cigarette)
#' }
#'
#' **PUMF coverage**: 2001-2021, 2023 (2022 gap due to questionnaire redesign where
#' SPU_05 was only asked of daily smokers, preventing cat2 vs cat3 derivation)
#'
#' **Master coverage**: 2001-2023 (full coverage)
#'
#' **Logic mapping adapted from legacy implementation**:
#' - **Daily smoker**: SMK_005 = 1 → 1
#' - **Occasional (former daily)**: SMK_005 = 2 AND SMK_030 = 1 → 2
#' - **Occasional (never daily)**: SMK_005 = 2 AND (SMK_030 = 2 OR SMK_030 missing) → 3
#' - **Former daily**: SMK_005 = 3 AND SMK_030 = 1 → 4
#' - **Former occasional**: SMK_005 = 3 AND SMK_030 = 2 AND SMK_01A = 1 → 5
#' - **Never smoked**: SMK_005 = 3 AND SMK_01A = 2 → 6
#'
#' **Missing data handling**: Uses Level 5-6 infrastructure with any_missing() and get_priority_missing()
#'
#' **Input compatibility**:
#' - **Scalar**: Single values for individual respondents
#' - **Vector**: Element-wise processing for multiple respondents
#' - **Database**: Works with data.frame columns (e.g., `data$SMK_005`)
#' - **Mixed types**: Handles numeric codes (999) and tagged_na inputs
#'
#' @param SMK_005 Numeric scalar/vector. Current smoking status (1=Daily, 2=Occasionally, 3=Not at all)
#' @param SMK_030 Numeric scalar/vector. Ever smoked daily (1=Yes, 2=No)
#' @param SMK_01A Numeric scalar/vector. Lifetime 100+ cigarettes (1=Yes, 2=No)
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Vector of smoking type classifications (1-6, plus missing value codes)
#'
#' @examples
#' \dontrun{
#' # Example 1: Vector inputs - All 6 smoking status categories
#' smoking_status <- calculate_SMKDSTY_cat6(
#'   SMK_005 = c(1, 2, 2, 3, 3, 3),      # Current smoking status
#'   SMK_030 = c(1, 1, 2, 1, 2, 2),      # Ever smoked daily
#'   SMK_01A = c(1, 1, 1, 1, 1, 2),      # 100+ cigarettes lifetime
#'   output_format = "tagged_na"
#' )
#' # Returns: c(1, 2, 3, 4, 5, 6)
#' # 1=Daily, 2=Occasional(former daily), 3=Occasional(never daily),
#' # 4=Former daily, 5=Former occasional, 6=Never smoked
#'
#' # Example 2: Scalar inputs - Single respondent
#' single_respondent <- calculate_SMKDSTY_cat6(
#'   SMK_005 = 2,                        # Occasionally
#'   SMK_030 = 1,                        # Yes, smoked daily before
#'   SMK_01A = 1,                        # Yes, 100+ cigarettes
#'   output_format = "original"
#' )
#' # Returns: 2 (Occasional smoker, former daily)
#'
#' # Example 3: Database-like usage with data.frame
#' cchs_data <- data.frame(
#'   respondent_id = 1001:1003,
#'   SMK_005 = c(1, 999, 2),            # Daily, Missing, Occasionally
#'   SMK_030 = c(1, 1, 999),            # Yes, Yes, Missing
#'   SMK_01A = c(1, 1, 1)               # All have 100+ cigarettes
#' )
#'
#' cchs_data$SMKDSTY_cat6 <- calculate_SMKDSTY_cat6(
#'   cchs_data$SMK_005,
#'   cchs_data$SMK_030,
#'   cchs_data$SMK_01A,
#'   output_format = "tagged_na"
#' )
#' # Creates new column with: c(1, tagged_na("b"), 3)
#' # Note: Missing SMK_030 with SMK_005=2 gives category 3 (never daily)
#'
#' # Example 4: Handling missing data - different output formats
#' missing_example_tagged <- calculate_SMKDSTY_cat6(
#'   SMK_005 = c(1, 999, 2),            # 999 = missing code
#'   SMK_030 = c(1, 1, 999),            # 999 = missing SMK_030
#'   SMK_01A = c(1, 1, 1),
#'   output_format = "tagged_na"        # Modern format
#' )
#' # Returns: c(1, tagged_na("b"), 3)
#'
#' missing_example_original <- calculate_SMKDSTY_cat6(
#'   SMK_005 = c(1, 999, 2),
#'   SMK_030 = c(1, 1, 999),
#'   SMK_01A = c(1, 1, 1),
#'   output_format = "original"         # Legacy compatibility
#' )
#' # Returns: c(1, NA, 3) - Same logic, different format
#'
#' # Example 5: Mixed input types (tagged_na + numeric)
#' mixed_inputs <- calculate_SMKDSTY_cat6(
#'   SMK_005 = c(1, haven::tagged_na("b"), 2),
#'   SMK_030 = c(1, 1, 2),
#'   SMK_01A = c(1, 1, 1),
#'   output_format = "tagged_na"
#' )
#' # Returns: c(1, tagged_na("b"), 3) - Handles pre-tagged inputs
#'
#' # For 2001-2014 cycles, use direct variable harmonization:
#' # harmonized_data <- rec_with_table(cchs_data, "SMKDSTY_cat6")
#' }
#'
#' @export
calculate_SMKDSTY_cat6 <- function(SMK_005 = NULL, SMK_030 = NULL, SMK_01A = NULL,
                                   output_format = "tagged_na") {

  # Handle all-NULL inputs (variable not collected in this cycle)
  if (is.null(SMK_005) && is.null(SMK_030) && is.null(SMK_01A)) {
    return(haven::tagged_na("c"))
  }
  # Expand remaining NULLs
  n <- max(length(SMK_005), length(SMK_030), length(SMK_01A))
  optional <- expand_null_inputs(list(
    SMK_005 = SMK_005, SMK_030 = SMK_030, SMK_01A = SMK_01A
  ), n)
  SMK_005 <- optional$SMK_005
  SMK_030 <- optional$SMK_030
  SMK_01A <- optional$SMK_01A

  # === STEP 1: Clean input variables using Level 6 infrastructure ===
  clean_vars_list <- list(
    SMK_005 = SMK_005,  # Maps to SMK_202 (current smoking status)
    SMK_030 = SMK_030,  # Maps to SMK_05D (ever smoked daily)
    SMK_01A = SMK_01A   # Maps to SMK_01A (100+ cigarettes)
  )

  cleaned <- clean_variables(vars = clean_vars_list, output_format = "tagged_na")

  # === STEP 2: Apply legacy SMKDSTY logic matching smoking-caitlin-maikol-original.R ===
  # Based on lines 913-922: SMKDSTY_fun function with corrected logic for missing data
  SMKDSTY_cat6_result <- dplyr::case_when(
    # Handle missing SMK_005 first (primary decision variable)
    any_missing(cleaned$SMK_005) ~
      get_priority_missing(cleaned$SMK_005, cleaned$SMK_030, cleaned$SMK_01A,
                           output_format = output_format),

    # Category 1: Daily smoker
    cleaned$SMK_005 == 1 ~ 1L,

    # Category 2: Occasional smoker (former daily)
    cleaned$SMK_005 == 2 & cleaned$SMK_030 == 1 ~ 2L,

    # Category 3: Occasional smoker (never daily) - includes missing SMK_030
    # Legacy logic: SMK_005 == 2 & (SMK_030 == 2|SMK_030 == "NA(a)"|SMK_030 == "NA(b)")
    cleaned$SMK_005 == 2 & (cleaned$SMK_030 == 2 | any_missing(cleaned$SMK_030)) ~ 3L,

    # Category 4: Former daily smoker (non-smoker now)
    cleaned$SMK_005 == 3 & cleaned$SMK_030 == 1 ~ 4L,

    # Category 5: Former occasional smoker (at least 1 whole cigarette, non-smoker now)
    cleaned$SMK_005 == 3 & cleaned$SMK_030 == 2 & cleaned$SMK_01A == 1 ~ 5L,

    # Category 6: Never smoked (a whole cigarette)
    cleaned$SMK_005 == 3 & cleaned$SMK_01A == 2 ~ 6L,

    # Handle remaining missing combinations
    .default = get_priority_missing(cleaned$SMK_005, cleaned$SMK_030, cleaned$SMK_01A,
                                   output_format = output_format)
  )

  # === STEP 3: Clean output using derived variable bounds ===
  # Use worksheet variable name SMKDSTY_original for metadata lookup (valid range 1-6)
  output_clean <- clean_variables(vars = list(SMKDSTY_original = SMKDSTY_cat6_result),
                                 output_format = output_format)

  return(prep_cat_output(output_clean$SMKDSTY_original))
}

#' @title Type of Smoker - SMKDSTY_original (deprecated alias)
#' @description Deprecated alias for [calculate_SMKDSTY_cat6()]. Use
#'   `calculate_SMKDSTY_cat6()` directly for new code.
#' @param ... Arguments passed to [calculate_SMKDSTY_cat6()]
#' @return Vector of smoking type classifications (1-6, plus missing value codes)
#'
#' @examples
#' \dontrun{
#' # Deprecated: use calculate_SMKDSTY_cat6() instead
#' calculate_SMKDSTY_original(SMK_005 = 1, SMK_030 = 2, SMK_01A = 1)
#' }
#'
#' @export
calculate_SMKDSTY_original <- function(...) {
  .Deprecated("calculate_SMKDSTY_cat6")
  calculate_SMKDSTY_cat6(...)
}

# ================================================================================

# smoke_simple - Simplified smoking status (4 categories) - COMPLEX DERIVATION
# ================================================================================

#' @title Simplified Smoking Status - smoke_simple (4 categories)
#' @description Complex derived variable using 3-step architecture
#' 
#' Creates harmonized smoke_simple variable across CCHS cycles 2003-2018 by combining
#' SMKDSTY_cat5 smoking status with time_quit_smoking to create simplified categories
#' for population-level smoking analysis.
#' 
#' @details 
#' **Implementation Method**: Complex derivation requiring two input variables
#' - **Input variables**: SMKDSTY_cat5 (5-category smoking status), time_quit_smoking (years since quit)
#' - **Architecture**: 3-step pattern with clean_variables() → domain logic → clean_variables()
#' - **Logic source**: Legacy smoke_simple_fun() from smoking-legacy-v2-1-0.R:138-176
#' 
#' **Categories (4)**:
#' \itemize{
#'   \item 0 = Non-smoker (never smoked)
#'   \item 1 = Current smoker (daily and occasional)  
#'   \item 2 = Former daily smoker quit ≤5 years OR former occasional smoker
#'   \item 3 = Former daily smoker quit >5 years
#' }
#' 
#' **Logic mapping from legacy implementation**:
#' - **Non-smoker (0)**: SMKDSTY_cat5 = 5 (never smoked) -> 0
#' - **Current smoker (1)**: SMKDSTY_cat5 in 1,2 (daily/occasional) -> 1
#' - **Former <=5yrs/occasional (2)**:
#'   - SMKDSTY_cat5 = 4 (former occasional) -> 2, OR
#'   - SMKDSTY_cat5 = 3 (former daily) AND time_quit_smoking <= 5 -> 2
#' - **Former >5yrs (3)**: SMKDSTY_cat5 = 3 AND time_quit_smoking > 5 -> 3
#' 
#' **Missing data handling**: Uses Level 5-6 infrastructure with any_missing() and get_priority_missing()
#' 
#' **Research applications**: 
#' - Population smoking prevalence studies
#' - Simplified exposure categories for epidemiological analysis
#' - Health outcome risk stratification
#' 
#' @param SMKDSTY_cat5 Numeric scalar/vector. 5-category smoking status (1=Daily, 2=Occasional, 3=Former daily, 4=Former occasional, 5=Never)
#' @param time_quit_smoking Numeric scalar/vector. Approximate years since quitting smoking (continuous)
#' @param output_format Character. Output format ("tagged_na" or "original")
#' 
#' @return Vector of simplified smoking classifications (0-3, plus missing value codes)
#' 
#' @examples
#' \dontrun{
#' # Example 1: Vector inputs - All 4 smoking categories
#' simple_status <- calculate_smoke_simple(
#'   SMKDSTY_cat5 = c(5, 1, 2, 3, 3, 4),
#'   time_quit_smoking = c(NA, NA, NA, 3, 8, 2),
#'   output_format = "tagged_na"
#' )
#' # Returns: c(0, 1, 1, 2, 3, 2)
#' # 0=Never, 1=Current, 2=Former <=5yrs, 3=Former >5yrs
#' 
#' # Example 2: Scalar inputs - Single respondent
#' former_daily_recent <- calculate_smoke_simple(
#'   SMKDSTY_cat5 = 3,              # Former daily smoker
#'   time_quit_smoking = 4,         # Quit 4 years ago
#'   output_format = "original"
#' )
#' # Returns: 2 (Former daily smoker quit ≤5 years)
#' 
#' # Example 3: Database-like usage
#' cchs_data <- data.frame(
#'   respondent_id = 2001:2005,
#'   SMKDSTY_cat5 = c(1, 3, 3, 4, 5),           # Daily, Former daily x2, Former occasional, Never
#'   time_quit_smoking = c(NA, 2, 10, 1, NA)     # Current/never have NA
#' )
#' 
#' cchs_data$smoke_simple <- calculate_smoke_simple(
#'   cchs_data$SMKDSTY_cat5,
#'   cchs_data$time_quit_smoking,
#'   output_format = "tagged_na"
#' )
#' # Creates: c(1, 2, 3, 2, 0) - Current daily, Former ≤5yrs, Former >5yrs, Former occasional, Never
#' 
#' # Example 4: Missing data handling
#' missing_example <- calculate_smoke_simple(
#'   SMKDSTY_cat5 = c(1, 999, 3),              # Daily, Missing, Former daily
#'   time_quit_smoking = c(NA, 5, 999),         # Current smoker NA, 5 years, Missing quit time
#'   output_format = "tagged_na"
#' )
#' # Returns: c(1, tagged_na("b"), tagged_na("b"))
#' 
#' # Use with rec_with_table() for harmonized input variables:
#' # harmonized_data <- rec_with_table(cchs_data, c("SMKDSTY_cat5", "time_quit_smoking"))
#' # simple_smoking <- calculate_smoke_simple(
#' #   harmonized_data$SMKDSTY_cat5,
#' #   harmonized_data$time_quit_smoking
#' # )
#' }
#' 
#' @export
calculate_smoke_simple <- function(SMKDSTY_cat5 = NULL, time_quit_smoking = NULL,
                                   output_format = "tagged_na") {
  
  # Handle all-NULL inputs (variable not collected in this cycle)
  if (is.null(SMKDSTY_cat5) && is.null(time_quit_smoking)) {
    return(haven::tagged_na("c"))
  }
  # Expand remaining NULLs
  n <- max(length(SMKDSTY_cat5), length(time_quit_smoking))
  optional <- expand_null_inputs(list(
    SMKDSTY_cat5 = SMKDSTY_cat5, time_quit_smoking = time_quit_smoking
  ), n)
  SMKDSTY_cat5 <- optional$SMKDSTY_cat5
  time_quit_smoking <- optional$time_quit_smoking
  
  # === STEP 1: Clean input variables using Level 6 infrastructure ===
  clean_vars_list <- list(
    SMKDSTY_cat5 = SMKDSTY_cat5,           # 5-category smoking status
    time_quit_smoking = time_quit_smoking   # Years since quitting
  )
  
  cleaned <- clean_variables(vars = clean_vars_list, output_format = "tagged_na")

  # === STEP 2: Apply legacy smoke_simple logic from smoking-legacy-v2-1-0.R:161-175 ===
  
  # Nested helper: derive current smoker status (0/1 from SMKDSTY_cat5)
  current_smoker <- dplyr::case_when(
    any_missing(cleaned$SMKDSTY_cat5) ~ 
      get_priority_missing(cleaned$SMKDSTY_cat5, output_format = output_format),
    cleaned$SMKDSTY_cat5 %in% c(1, 2) ~ 1L,     # Daily (1) + Occasional (2) = Current (1)
    cleaned$SMKDSTY_cat5 %in% c(3, 4, 5) ~ 0L,  # Former daily (3) + Former occasional (4) + Never (5) = Not current (0)
    .default = get_priority_missing(cleaned$SMKDSTY_cat5, output_format = output_format)
  )
  
  # Nested helper: derive ever smoker status (0/1 from SMKDSTY_cat5)
  ever_smoker <- dplyr::case_when(
    any_missing(cleaned$SMKDSTY_cat5) ~ 
      get_priority_missing(cleaned$SMKDSTY_cat5, output_format = output_format),
    cleaned$SMKDSTY_cat5 %in% c(1, 2, 3, 4) ~ 1L, # Daily (1) + Occasional (2) + Former daily (3) + Former occasional (4) = Ever smoked (1)
    cleaned$SMKDSTY_cat5 == 5 ~ 0L,                # Never (5) = Never smoked (0)
    .default = get_priority_missing(cleaned$SMKDSTY_cat5, output_format = output_format)
  )
  
  # Main smoke_simple logic - adapted from legacy lines 162-174
  smoke_simple_result <- dplyr::case_when(
    # Handle primary missing data first
    any_missing(current_smoker) | any_missing(ever_smoker) ~
      get_priority_missing(current_smoker, ever_smoker, output_format = output_format),
    
    # Category 0: Non-smoker (never smoked)
    current_smoker == 0 & ever_smoker == 0 ~ 0L,
    
    # Category 1: Current smoker (daily + occasional)
    current_smoker == 1 & ever_smoker == 1 ~ 1L,
    
    # Category 2: Former daily smoker quit ≤5 years OR former occasional smoker
    # Legacy logic: smoker == 0 & eversmoker == 1 & time_quit_smoking <= 5 | SMKDSTY_cat5 == 4
    (current_smoker == 0 & ever_smoker == 1 & !any_missing(cleaned$time_quit_smoking) & cleaned$time_quit_smoking <= 5) | 
    cleaned$SMKDSTY_cat5 == 4 ~ 2L,
    
    # Category 3: Former daily smoker quit >5 years
    current_smoker == 0 & ever_smoker == 1 & !any_missing(cleaned$time_quit_smoking) & cleaned$time_quit_smoking > 5 ~ 3L,
    
    # Handle remaining missing combinations (especially missing time_quit_smoking for former smokers)
    .default = get_priority_missing(cleaned$SMKDSTY_cat5, cleaned$time_quit_smoking, 
                                   output_format = output_format)
  )
  
  # === STEP 3: Clean output using derived variable bounds ===
  output_clean <- clean_variables(vars = list(smoke_simple = smoke_simple_result), 
                                 output_format = output_format)
  
  return(prep_cat_output(output_clean$smoke_simple))
}

