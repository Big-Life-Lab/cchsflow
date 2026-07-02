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

#' @title Categorize age started smoking daily (SMKG040_cat)
#'
#' @description
#' Harmonize CCHS variable SMKG040_cat (age started daily smoking in
#' 6 categories) across cycles 2015-2023. Implemented via
#' rec_with_table().
#'
#' @details
#' Asked of daily and former daily smokers. Source is SMK_040
#' (2015-2021) or SPU_15 (2022-2023). Categories: 1 = 10 or under,
#' 2 = 11-14, 3 = 15-17, 4 = 18-19, 5 = 20-24, 6 = 25 or over.
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector of age group (1-6). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMKG040_cat")
#' }
#'
#' @seealso \code{\link{calculate_SMKG040_cont}},
#'   \code{\link{calculate_age_start_smoking}}
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

#' @title Categorize age started daily smoking for daily smokers (SMKG203_cat)
#'
#' @description
#' Harmonize SMKG203 categorical age-started variables for daily smokers
#' across CCHS cycles. Implemented via rec_with_table().
#'
#' @details
#' Temporal variants exist: SMKG203_pre2005 (2001-2003, 10 categories)
#' and SMKG203_2005plus (2005-2014, 11 categories). For 2015+, the
#' function derives the value from SMK_005 and SMK_040/SPU_15. Most
#' applications should prefer continuous SMKG203_cont instead.
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector of age group (1-6). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMKG203_2005plus")
#' }
#'
#' @seealso \code{\link{calculate_SMKG203_cont}},
#'   \code{\link{calculate_SMKG040_cat}}
#'
#' @export
calculate_SMKG203_cat <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMKG203_pre2005' or 'SMKG203_2005plus') for implementation")
}

# ================================================================================
# SMKG203_cont - Age started smoking daily - daily smoker (continuous)
# ================================================================================

#' @title Derive continuous age started daily smoking for daily smokers
#'   (SMKG203_cont)
#'
#' @description
#' Filter SMKG040_cont by SMK_005 to produce a continuous age-started
#' variable restricted to current daily smokers (SMK_005 == 1).
#'
#' @details
#' Non-daily smokers receive tagged_na("a"). The function uses 3-step
#' architecture: clean inputs, apply SMK_005 gate logic, clean output.
#' Supports both 2001-2014 direct variables and 2015+ derivation.
#'
#' @param SMK_005 Numeric. Current smoking status
#'   (1 = daily, 2 = occasionally, 3 = not at all).
#' @param SMKG040_cont Numeric. Continuous age started smoking daily
#'   (5-121 years).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of age started daily smoking (5-121).
#'   Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar: daily smoker
#' calculate_SMKG203_cont(SMK_005 = 1, SMKG040_cont = 18.5)
#'
#' @seealso \code{\link{calculate_SMKG207_cont}},
#'   \code{\link{calculate_SMKG040_cont}},
#'   \code{\link{calculate_age_start_smoking}}
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

#' @title Categorize age started daily smoking for former daily smokers
#'   (SMKG207_cat)
#'
#' @description
#' Harmonize SMKG207 categorical age-started variables for former daily
#' smokers across CCHS cycles 2001-2014. Implemented via rec_with_table().
#'
#' @details
#' Temporal variants exist: SMKG207_pre2005 (2001-2003, 10 categories)
#' and SMKG207_2005plus (2005-2014, 11 categories). Available 2001-2014
#' only; replaced by complex derivation in 2015+. Most applications
#' should prefer continuous SMKG207_cont instead.
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector of age group (1-6). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMKG207_2005plus")
#' }
#'
#' @seealso \code{\link{calculate_SMKG207_cont}},
#'   \code{\link{calculate_SMKG040_cat}}
#'
#' @export
calculate_SMKG207_cat <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMKG207_pre2005' or 'SMKG207_2005plus') for implementation")
}

# ================================================================================

# SMKG207_cont - Age started smoking daily - former daily smoker (continuous) - LEVEL 7 IMPLEMENTATION
# ================================================================================

#' @title Derive continuous age started daily smoking for former daily
#'   smokers (SMKG207_cont)
#'
#' @description
#' Filter SMKG040_cont by SMK_030 to produce a continuous age-started
#' variable restricted to former daily smokers (SMK_030 == 1).
#'
#' @details
#' Non-former-daily smokers receive tagged_na("a"). The function uses
#' 3-step architecture: clean inputs, apply SMK_030 gate logic, clean
#' output. Replaces hard-coded categorical-to-continuous mapping with
#' variable_details.csv recStart/recEnd lookups.
#'
#' @param SMK_030 Numeric. Former daily smoking status
#'   (1 = formerly smoked daily, 2 = did not).
#' @param SMKG040_cont Numeric. Continuous age started smoking daily
#'   (5-95 years).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of age started daily smoking (5-95).
#'   Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar: former daily smoker
#' calculate_SMKG207_cont(SMK_030 = 1, SMKG040_cont = 25.0)
#'
#' @seealso \code{\link{calculate_SMKG203_cont}},
#'   \code{\link{calculate_SMKG040_cont}},
#'   \code{\link{calculate_age_start_smoking}}
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

#' @title Derive continuous age started daily smoking for all ever-daily
#'   smokers (SMKG040_cont)
#'
#' @description
#' Combine SMKG203_cont (daily smokers) and SMKG207_cont (former daily
#' smokers) into a single continuous age-started variable for 2001-2014
#' cycles.
#'
#' @details
#' SMKG203_cont and SMKG207_cont are mutually exclusive by design. This
#' function selects whichever has valid data. For 2015+, SMKG040_cont
#' comes directly from SMK_040/SPU_15 via rec_with_table(). When both
#' inputs are missing, get_priority_missing() determines the output.
#'
#' @param SMKG203_cont Numeric. Age started daily smoking for current
#'   daily smokers.
#' @param SMKG207_cont Numeric. Age started daily smoking for former
#'   daily smokers.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of age started daily smoking (5-121).
#'   Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar: daily smoker
#' calculate_SMKG040_cont(
#'   SMKG203_cont = 18.5,
#'   SMKG207_cont = haven::tagged_na("a")
#' )
#'
#' @seealso \code{\link{calculate_SMKG203_cont}},
#'   \code{\link{calculate_SMKG207_cont}},
#'   \code{\link{calculate_age_start_smoking}}
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

#' @title Derive age started smoking daily (SMK_207)
#'
#' @description
#' Harmonize SMK_207 (continuous age started daily smoking) across CCHS
#' cycles 2001-2014. Implemented via rec_with_table().
#'
#' @details
#' Available 2001-2014 only (replaced by SMK_040/SPU_15 in 2015+).
#' Source variables are cycle-specific (SMK_207 direct). Range
#' typically 5-80+ years.
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of age started daily smoking. Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMK_207")
#' }
#'
#' @seealso \code{\link{calculate_SMK_203}},
#'   \code{\link{calculate_age_start_smoking}}
#'
#' @export
calculate_SMK_207 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_207') for implementation")
}

# ================================================================================

# SMK_203 - Age started smoking regularly - DOCUMENTATION ONLY
# ================================================================================

#' @title Derive age started smoking regularly (SMK_203)
#'
#' @description
#' Harmonize SMK_203 (continuous age started regular smoking) across
#' CCHS cycles 2001-2014. Implemented via rec_with_table().
#'
#' @details
#' Available 2001-2014 only (discontinued in 2015+). Captures when
#' respondents started smoking regularly, which is broader than daily
#' smoking (see SMK_207). Range typically 5-80+ years.
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of age started regular smoking. Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMK_203")
#' }
#'
#' @seealso \code{\link{calculate_SMK_207}},
#'   \code{\link{calculate_age_start_smoking}}
#'
#' @export
calculate_SMK_203 <- function(data, output_format = "tagged_na") {

  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_203') for implementation")
}

# ================================================================================
# age_start_smoking - Unified Age Started Smoking Daily (Derived Variable)
# ================================================================================

#' @title Calculate unified age started smoking daily (age_start_smoking)
#'
#' @description
#' Pass-through wrapper that provides a self-documenting variable name
#' for age started daily smoking. The worksheet routes the appropriate
#' source: SMKG040_cont (PUMF) or SMK_040 (Master).
#'
#' @details
#' Universe: ever-daily smokers (SMKDSTY 1, 2, 4). Never-daily smokers
#' receive tagged_na("a"). Coverage: PUMF 2001-2021 (midpoint), Master
#' 2001-2023 (exact continuous). Uses 3-step pass-through architecture.
#'
#' @param age_start_smoking Numeric. Age started smoking daily
#'   (continuous). Worksheet routes SMKG040_cont or SMK_040.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of age started daily smoking. Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' calculate_age_start_smoking(18)
#'
#' @seealso \code{\link{calculate_SMKG040_cont}},
#'   \code{\link{calculate_cigs_per_day}},
#'   \code{\link{calculate_pack_years}}
#'
#' @export
calculate_age_start_smoking <- function(age_start_smoking = NULL,
                                        output_format = "tagged_na") {
  derive_passthrough(age_start_smoking, "age_start_smoking", output_format)
}

# ================================================================================
# age_first_cigarette - Unified Age Smoked First Cigarette (Derived Variable)
# ================================================================================

#' @title Calculate unified age smoked first whole cigarette
#'   (age_first_cigarette)
#'
#' @description
#' Pass-through wrapper that provides a self-documenting variable name
#' for age first smoked a whole cigarette. The worksheet routes the
#' appropriate source: SMKG01C_cont (PUMF) or SMK_01C (Master).
#'
#' @details
#' Universe: respondents who have smoked 100+ cigarettes in lifetime
#' (SMK_01A == 1). Coverage: PUMF 2001-2021 (midpoint), Master
#' 2001-2023 (exact continuous). Uses 3-step pass-through architecture.
#'
#' @param age_first_cigarette Numeric. Continuous age first smoked a
#'   whole cigarette. Worksheet routes SMKG01C_cont or SMK_01C.
#'   NULL if not available.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of age first smoked (8-95). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' calculate_age_first_cigarette(14)
#'
#' @seealso \code{\link{calculate_age_start_smoking}},
#'   \code{\link{calculate_smoked_100_lifetime}}
#'
#' @export
calculate_age_first_cigarette <- function(age_first_cigarette = NULL,
                                          output_format = "tagged_na") {
  derive_passthrough(age_first_cigarette, "age_first_cigarette", output_format)
}

# ================================================================================
# smoked_100_lifetime - Ever Smoked 100+ Cigarettes (Derived Variable)
# ================================================================================

#' @title Assess lifetime 100-cigarette threshold (smoked_100_lifetime)
#'
#' @description
#' Pass-through wrapper for SMK_01A providing a self-documenting
#' variable name for whether the respondent has smoked 100 or more
#' cigarettes in their lifetime.
#'
#' @details
#' Source: SMK_01A, already harmonised across all cycles in the
#' worksheets. Coverage: PUMF 2001-2021, Master 2001-2023. Universe:
#' respondents who have smoked at least one whole cigarette. Uses
#' 3-step pass-through architecture.
#'
#' @param smoked_100_lifetime Numeric. 1 = yes (100+), 2 = no.
#'   Worksheet routes SMK_01A from the appropriate database.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector (1 = yes, 2 = no). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' calculate_smoked_100_lifetime(1)
#'
#' @seealso \code{\link{calculate_age_first_cigarette}},
#'   \code{\link{calculate_SMK_01A}}
#'
#' @export
calculate_smoked_100_lifetime <- function(smoked_100_lifetime = NULL,
                                          output_format = "tagged_na") {
  prep_cat_output(derive_passthrough(smoked_100_lifetime, "smoked_100_lifetime", output_format))
}
