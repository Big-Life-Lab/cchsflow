# ==============================================================================
# Smoking Cessation Derived Variable Functions
# ==============================================================================
#
# This file contains all derived variable functions for smoking cessation analysis.
# Consolidated from smoke-stop.R (archived 2026-01-03) and new L4.1 implementations.
#
# FUNCTION HIERARCHY:
#
# Foundational (categorical → continuous conversion):
# ├── calculate_SMK_06A_cont()  → time since quit (former occasional)
# ├── calculate_SMK_09A_cont()  → time since stopped daily (former daily)
# └── calculate_SMK_10A_cont()  → time since quit completely (former daily who continued occasional)
#
# Note: rec_with_table() is the primary mechanism for passthrough midpoint
# conversion. These DV functions exist as alternative entry points for use
# by combining functions below.
#
# Combining function:
# └── calculate_time_quit_smoking()  → combines foundational outputs with priority logic
#
# Pathway-aware functions:
# ├── assess_quit_pathway()  → categorical indicator of how they quit
# └── calculate_time_quit_complete()  → unified quit time using pathway + gate
#
# Dependencies:
# - clean_variables() from clean-variables.R
# - any_missing(), get_priority_missing(), assign_missing() from missing-data-functions.R
#
# Related files:
# - smoking-status.R: SMKDSTY status classification
# - smoke-stop_ARCHIVE_2026-01-03.R: Previous version (archived)
#
# Specification: harmonization-development/smoking/03-cessation/L4_dv_specifications.md
#
# ==============================================================================


# Source required helper functions (conditional loading for package context)
tryCatch(
  {
    if (file.exists("R/missing-data-functions.R")) {
      source("R/missing-data-functions.R", local = FALSE)
      source("R/clean-variables.R", local = FALSE)
    } else if (file.exists("missing-data-functions.R")) {
      source("missing-data-functions.R", local = FALSE)
      source("clean-variables.R", local = FALSE)
    } else if (file.exists("../../R/missing-data-functions.R")) {
      source("../../R/missing-data-functions.R", local = FALSE)
      source("../../R/clean-variables.R", local = FALSE)
    }
  },
  error = function(e) {
    # Functions will be loaded via package imports during package build
  }
)

# ==============================================================================
# FOUNDATIONAL FUNCTIONS (Categorical → Continuous Conversion)
# ==============================================================================
#
# These functions convert categorical quit timing variables to continuous years
# using midpoint imputation. rec_with_table() handles this conversion via
# worksheet rows (Pattern 1). These DV functions exist as alternative entry
# points for combining functions that call them in R code (Pattern 2).
#
# Variable series:
# - SMK_06: Former occasional smokers (never smoked daily)
# - SMK_09: Former daily smokers (when stopped daily)
# - SMK_10: Former daily smokers who continued occasional (when quit completely)
#
# ==============================================================================

# ------------------------------------------------------------------------------
# calculate_SMK_06A_cont - Former occasional quit timing (continuous)
# ------------------------------------------------------------------------------

#' Calculate Years Since Quit - Former Occasional Smokers (SMK_06A_cont)
#'
#' Converts categorical SMK_06A (when stopped smoking for former occasional/never
#' daily smokers) to continuous years using midpoint imputation.
#'
#' @details
#' **Implementation method**: 3-step architecture
#' - **Step 1**: clean_variables() - Clean SMK_06A_cat4 and SMKG06C inputs
#' - **Step 2**: Domain logic with midpoint conversion
#' - **Step 3**: Output cleaning
#'
#' **Note**: rec_with_table() is the primary mechanism for passthrough midpoint
#' conversion. This DV function exists as an alternative entry point for use by
#' combining functions like calculate_time_quit_smoking().
#'
#' **Category mappings** (midpoint imputation):
#' \itemize{
#'   \item 1 = Less than 1 year ago → 0.5 years
#'   \item 2 = 1 to less than 2 years ago → 1.5 years
#'   \item 3 = 2 to less than 3 years ago → 2.5 years
#'   \item 4 = 3 or more years ago → use SMKG06C if available, else 5.0 years
#' }
#'
#' @param SMK_06A_cat4 Numeric vector. Categorical time since quit (1-4)
#' @param SMKG06C Numeric vector. Continuous years for category 4 (3+ years)
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Numeric vector of continuous years since quit (0-80+), with:
#' - NA::a for never smokers or current smokers (not applicable)
#' - NA::b for missing/refused
#'
#' @examples
#' \dontrun{
#' # Category 2 (1-2 years) → 1.5 years
#' calculate_SMK_06A_cont(SMK_06A_cat4 = 2, SMKG06C = NA)
#' # Returns: 1.5
#'
#' # Category 4 with continuous follow-up
#' calculate_SMK_06A_cont(SMK_06A_cat4 = 4, SMKG06C = 7.5)
#' # Returns: 7.5
#'
#' # Category 4 without follow-up (fallback)
#' calculate_SMK_06A_cont(SMK_06A_cat4 = 4, SMKG06C = NA)
#' # Returns: 5.0
#' }
#'
#' @export
calculate_SMK_06A_cont <- function(SMK_06A_cat4, SMKG06C = NULL, output_format = "tagged_na") {

  # Handle empty input vectors
  if (length(SMK_06A_cat4) == 0) return(numeric(0))

  # Handle NULL SMKG06C
  if (is.null(SMKG06C)) {
    SMKG06C <- rep(NA_real_, length(SMK_06A_cat4))
  }

  # === STEP 1: DATA CLEANING AND VALIDATION ===
  cleaned <- clean_variables(vars = list(
    SMK_06A_cat4 = SMK_06A_cat4,
    SMKG06C = SMKG06C
  ), output_format = output_format)

  # === STEP 2: DOMAIN LOGIC WITH MISSING DATA FUNCTIONS ===
  result <- dplyr::case_when(
    # Missing data detection and priority processing
    any_missing(cleaned$SMK_06A_cat4) ~
      get_priority_missing(cleaned$SMK_06A_cat4, cleaned$SMKG06C, output_format = output_format),

    # Domain logic: Convert categories to continuous years
    cleaned$SMK_06A_cat4 == 1 ~ 0.5,    # <1 year ago → 0.5 years
    cleaned$SMK_06A_cat4 == 2 ~ 1.5,    # 1-2 years ago → 1.5 years
    cleaned$SMK_06A_cat4 == 3 ~ 2.5,    # 2-3 years ago → 2.5 years
    cleaned$SMK_06A_cat4 == 4 & !any_missing(cleaned$SMKG06C) ~ cleaned$SMKG06C,  # 3+ years → use continuous
    cleaned$SMK_06A_cat4 == 4 & any_missing(cleaned$SMKG06C) ~ 5.0,  # 3+ years fallback → 5.0 years

    # Invalid categories get missing value
    .default = assign_missing("not_applicable", "SMK_06A_cont", output_format)
  )

  # === STEP 3: OUTPUT CLEANING ===
  output_cleaned <- clean_variables(vars = list(
    SMK_06A_cont = result
  ), output_format = output_format)

  return(output_cleaned$SMK_06A_cont)
}

# ------------------------------------------------------------------------------
# calculate_SMK_09A_cont - Former daily stopped daily timing (continuous)
# ------------------------------------------------------------------------------

#' Calculate Years Since Stopped Daily - Former Daily Smokers (SMK_09A_cont)
#'
#' Converts categorical SMK_09A (when stopped smoking daily for former daily
#' smokers) to continuous years using midpoint imputation.
#'
#' @details
#' **Implementation method**: 3-step architecture
#' - **Step 1**: clean_variables() - Clean SMK_09A_cat4 and SMKG09C inputs
#' - **Step 2**: Domain logic with midpoint conversion
#' - **Step 3**: Output cleaning
#'
#' **Note**: rec_with_table() is the primary mechanism for passthrough midpoint
#' conversion. This DV function exists as an alternative entry point for use by
#' combining functions like calculate_time_quit_smoking().
#'
#' **Category mappings** (midpoint imputation):
#' \itemize{
#'   \item 1 = Less than 1 year ago → 0.5 years
#'   \item 2 = 1 to less than 2 years ago → 1.5 years
#'   \item 3 = 2 to less than 3 years ago → 2.5 years
#'   \item 4 = 3 or more years ago → use SMKG09C if available, else 5.0 years
#' }
#'
#' **Important**: This measures when they stopped DAILY smoking, NOT when they
#' quit completely. Former daily smokers may have continued as occasional smokers.
#' Use SMK_10_gate to determine if they quit completely when stopping daily.
#'
#' @param SMK_09A_cat4 Numeric vector. Categorical time since stopped daily (1-4)
#' @param SMKG09C Numeric vector. Continuous years for category 4 (3+ years)
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Numeric vector of continuous years since stopped daily (0-80+), with:
#' - NA::a for never-daily smokers (not applicable)
#' - NA::b for missing/refused
#'
#' @examples
#' \dontrun{
#' # Scalar inputs - single respondent
#' result_scalar <- calculate_SMK_09A_cont(SMK_09A_cat4 = 2, SMKG09C = NA)
#' # Returns: 1.5 (category 2 = 1-2 years, midpoint used)
#'
#' # Vector inputs - multiple respondents
#' smk_09a <- c(1, 2, 3, 4, 4)
#' smkg09c <- c(NA, NA, NA, 5.5, NA)
#' result_vector <- calculate_SMK_09A_cont(smk_09a, smkg09c)
#' # Returns: c(0.5, 1.5, 2.5, 5.5, 5.0)
#' }
#'
#' @export
calculate_SMK_09A_cont <- function(SMK_09A_cat4, SMKG09C = NULL, output_format = "tagged_na") {

  # Handle empty input vectors
  if (length(SMK_09A_cat4) == 0) return(numeric(0))

  # Handle NULL SMKG09C
  if (is.null(SMKG09C)) {
    SMKG09C <- rep(NA_real_, length(SMK_09A_cat4))
  }

  # === STEP 1: DATA CLEANING AND VALIDATION ===
  cleaned <- clean_variables(vars = list(
    SMK_09A_cat4 = SMK_09A_cat4,
    SMKG09C = SMKG09C
  ), output_format = output_format)

  # === STEP 2: DOMAIN LOGIC WITH MISSING DATA FUNCTIONS ===
  result <- dplyr::case_when(
    # Missing data detection and priority processing
    any_missing(cleaned$SMK_09A_cat4) ~
      get_priority_missing(cleaned$SMK_09A_cat4, cleaned$SMKG09C, output_format = output_format),

    # Domain logic: Convert categories to continuous years
    cleaned$SMK_09A_cat4 == 1 ~ 0.5,    # <1 year ago → 0.5 years
    cleaned$SMK_09A_cat4 == 2 ~ 1.5,    # 1-2 years ago → 1.5 years
    cleaned$SMK_09A_cat4 == 3 ~ 2.5,    # 2-3 years ago → 2.5 years
    cleaned$SMK_09A_cat4 == 4 & !any_missing(cleaned$SMKG09C) ~ cleaned$SMKG09C,  # 3+ years → use continuous
    cleaned$SMK_09A_cat4 == 4 & any_missing(cleaned$SMKG09C) ~ 5.0,  # 3+ years fallback → 5.0 years

    # Invalid categories get missing value
    .default = assign_missing("not_applicable", "SMK_09A_cont", output_format)
  )

  # === STEP 3: OUTPUT CLEANING ===
  output_cleaned <- clean_variables(vars = list(
    SMK_09A_cont = result
  ), output_format = output_format)

  return(output_cleaned$SMK_09A_cont)
}

# ------------------------------------------------------------------------------
# calculate_SMK_10A_cont - Former daily quit completely timing (continuous)
# ------------------------------------------------------------------------------

#' Calculate Years Since Quit Completely - Former Daily Who Continued Occasional (SMK_10A_cont)
#'
#' Converts categorical SMK_10A (when quit completely for former daily smokers
#' who continued as occasional after stopping daily) to continuous years.
#'
#' @details
#' **Implementation method**: 3-step architecture
#' - **Step 1**: clean_variables() - Clean SMK_10A and SMKG10C inputs
#' - **Step 2**: Domain logic with midpoint conversion
#' - **Step 3**: Output cleaning
#'
#' **Note**: rec_with_table() is the primary mechanism for passthrough midpoint
#' conversion. This DV function exists as an alternative entry point for use by
#' combining functions like calculate_time_quit_complete().
#'
#' **Category mappings** (midpoint imputation):
#' \itemize{
#'   \item 1 = Less than 1 year ago → 0.5 years
#'   \item 2 = 1 to less than 2 years ago → 1.5 years
#'   \item 3 = 2 to less than 3 years ago → 2.5 years
#'   \item 4 = 3 or more years ago → use SMKG10C if available, else 5.0 years
#' }
#'
#' **Important**: This variable is ONLY asked when SMK_10_gate = 2 (did not quit
#' completely when stopped daily smoking). If SMK_10_gate = 1, use SMK_09A_cont instead.
#'
#' @param SMK_10A Numeric vector. Categorical time since quit completely (1-4)
#' @param SMKG10C Numeric vector. Continuous years for category 4 (3+ years)
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Numeric vector of continuous years since quit completely (0-80+), with:
#' - NA::a for those who quit when stopped daily (SMK_10_gate = 1) or never-daily
#' - NA::b for missing/refused or 2001 cycle
#'
#' @examples
#' \dontrun{
#' # Category 2 (1-2 years) → 1.5 years
#' calculate_SMK_10A_cont(SMK_10A = 2, SMKG10C = NA)
#' # Returns: 1.5
#'
#' # Category 4 with continuous follow-up
#' calculate_SMK_10A_cont(SMK_10A = 4, SMKG10C = 8.0)
#' # Returns: 8.0
#' }
#'
#' @export
calculate_SMK_10A_cont <- function(SMK_10A, SMKG10C = NULL, output_format = "tagged_na") {

  # Handle empty input vectors
  if (length(SMK_10A) == 0) return(numeric(0))

  # Handle NULL SMKG10C
  if (is.null(SMKG10C)) {
    SMKG10C <- rep(NA_real_, length(SMK_10A))
  }

  # === STEP 1: DATA CLEANING AND VALIDATION ===
  cleaned <- clean_variables(vars = list(
    SMK_10A = SMK_10A,
    SMKG10C = SMKG10C
  ), output_format = output_format)

  # === STEP 2: DOMAIN LOGIC WITH MISSING DATA FUNCTIONS ===
  result <- dplyr::case_when(
    # Missing data detection and priority processing
    any_missing(cleaned$SMK_10A) ~
      get_priority_missing(cleaned$SMK_10A, cleaned$SMKG10C, output_format = output_format),

    # Domain logic: Convert categories to continuous years
    cleaned$SMK_10A == 1 ~ 0.5,    # <1 year ago → 0.5 years
    cleaned$SMK_10A == 2 ~ 1.5,    # 1-2 years ago → 1.5 years
    cleaned$SMK_10A == 3 ~ 2.5,    # 2-3 years ago → 2.5 years
    cleaned$SMK_10A == 4 & !any_missing(cleaned$SMKG10C) ~ cleaned$SMKG10C,  # 3+ years → use continuous
    cleaned$SMK_10A == 4 & any_missing(cleaned$SMKG10C) ~ 5.0,  # 3+ years fallback → 5.0 years

    # Invalid categories get missing value
    .default = assign_missing("not_applicable", "SMK_10A_cont", output_format)
  )

  # === STEP 3: OUTPUT CLEANING ===
  output_cleaned <- clean_variables(vars = list(
    SMK_10A_cont = result
  ), output_format = output_format)

  return(output_cleaned$SMK_10A_cont)
}

# ==============================================================================
# COMBINING FUNCTION
# ==============================================================================
#
# This function combines the foundational continuous outputs with priority logic
# to provide a single "time since quit smoking" value.
#
# ==============================================================================

# ------------------------------------------------------------------------------
# calculate_time_quit_smoking - Combined cessation timeframe
# ------------------------------------------------------------------------------

#' Calculate Time Since Quit Smoking (Combined)
#'
#' Combines cessation timing from multiple sources with priority logic.
#' Provides a single continuous "years since quit" value regardless of
#' smoking history pathway.
#'
#' @details
#' **Implementation method**: 3-step architecture
#' - **Step 1**: clean_variables() - Clean all continuous time inputs
#' - **Step 2**: Priority logic to select best available source
#' - **Step 3**: Output cleaning
#'
#' **Priority order** (highest to lowest):
#' 1. SMK_09A_cont - Former daily smokers (stopped daily timing)
#' 2. SMK_06A_cont - Former occasional smokers (quit timing)
#'
#' **Note**: This is a simpler combining function that doesn't use pathway
#' logic. For pathway-aware quit timing, use calculate_time_quit_complete().
#'
#' @param SMK_09A_cont Numeric vector. Years since stopped daily (from calculate_SMK_09A_cont)
#' @param SMK_06A_cont Numeric vector. Years since quit occasional (from calculate_SMK_06A_cont)
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Numeric vector of continuous years since quit (0-80+), with:
#' - NA::a for current smokers and never smokers
#' - NA::b for missing/refused
#'
#' @examples
#' \dontrun{
#' # Former daily smoker - uses SMK_09A_cont
#' calculate_time_quit_smoking(SMK_09A_cont = 3.5, SMK_06A_cont = NA)
#' # Returns: 3.5
#'
#' # Former occasional - uses SMK_06A_cont
#' calculate_time_quit_smoking(SMK_09A_cont = NA, SMK_06A_cont = 5.0)
#' # Returns: 5.0
#' }
#'
#' @export
calculate_time_quit_smoking <- function(SMK_09A_cont, SMK_06A_cont,
                                        output_format = "tagged_na") {

  # Handle empty input vectors
  if (length(SMK_09A_cont) == 0) return(numeric(0))

  # === STEP 1: DATA CLEANING AND VALIDATION ===
  cleaned <- clean_variables(vars = list(
    SMK_09A_cont = SMK_09A_cont,
    SMK_06A_cont = SMK_06A_cont
  ), output_format = output_format)

  # === STEP 2: DOMAIN LOGIC WITH PRIORITY SELECTION ===
  result <- dplyr::case_when(
    # First priority: SMK_09A_cont (former daily smokers)
    !any_missing(cleaned$SMK_09A_cont) ~ cleaned$SMK_09A_cont,

    # Second priority: SMK_06A_cont (former occasional smokers)
    !any_missing(cleaned$SMK_06A_cont) ~ cleaned$SMK_06A_cont,

    # If all inputs are missing, get priority missing value
    any_missing(cleaned$SMK_09A_cont) & any_missing(cleaned$SMK_06A_cont) ~
      get_priority_missing(cleaned$SMK_09A_cont, cleaned$SMK_06A_cont, output_format = output_format),

    # Fallback: not applicable
    .default = assign_missing("not_applicable", "time_quit_smoking", output_format)
  )

  # === STEP 3: OUTPUT CLEANING ===
  output_cleaned <- clean_variables(vars = list(
    time_quit_smoking = result
  ), output_format = output_format)

  return(output_cleaned$time_quit_smoking)
}

# ==============================================================================
# PATHWAY-AWARE FUNCTIONS
# ==============================================================================
#
# These functions use SMK_10_gate and SMKDSTY_cat5 to apply pathway-specific
# logic for more accurate cessation analysis.
#
# ==============================================================================

# ------------------------------------------------------------------------------
# assess_quit_pathway - Categorical quit pathway indicator
# ------------------------------------------------------------------------------

#' Assess Smoking Cessation Pathway
#'
#' Classifies former smokers by their cessation pathway based on smoking history.
#' Uses SMKDSTY_cat5 (5-category smoking status) and SMK_10_gate (quit timing gate).
#'
#' @details
#' **Implementation method**: 3-step architecture
#' - **Step 1**: clean_variables() - Clean SMKDSTY_cat5 and SMK_10_gate inputs
#' - **Step 2**: Missing data functions + domain logic - Classify quit pathway
#' - **Step 3**: Output cleaning
#'
#' **Pathway categories**:
#' \itemize{
#'   \item 1 = Direct quit: Quit completely when stopped daily smoking
#'   \item 2 = Gradual reducer: Stopped daily, continued occasional, then quit
#'   \item 3 = Former occasional: Never smoked daily, quit occasional smoking
#' }
#'
#' **Input requirements**:
#' - SMKDSTY_cat5: 5-category smoking status (1=daily, 2=occasional, 3=former daily,
#'   4=former occasional, 5=never smoked)
#' - SMK_10_gate: Gate variable indicating quit timing for former daily smokers
#'   (1=quit when stopped daily, 2=quit later)
#'
#' **Era handling**:
#' - 2001: SMK_10_gate not available, returns NA::b for former daily smokers
#' - 2003+: Full pathway classification available
#'
#' @param SMKDSTY_cat5 Numeric vector. 5-category smoking status
#' @param SMK_10_gate Numeric vector. Quit timing gate (1 or 2)
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Integer vector of pathway codes (1-3), with:
#' - NA::a for current smokers and never smokers (not applicable)
#' - NA::b for missing/unknown or 2001 (pathway unknown)
#'
#' @examples
#' \dontrun{
#' # Direct quit (former daily who quit when stopped daily)
#' assess_quit_pathway(SMKDSTY_cat5 = 3, SMK_10_gate = 1)
#' # Returns: 1L
#'
#' # Gradual reducer (former daily who continued occasional)
#' assess_quit_pathway(SMKDSTY_cat5 = 3, SMK_10_gate = 2)
#' # Returns: 2L
#'
#' # Former occasional (never smoked daily)
#' assess_quit_pathway(SMKDSTY_cat5 = 4, SMK_10_gate = NA)
#' # Returns: 3L
#'
#' # Current smoker (not applicable)
#' assess_quit_pathway(SMKDSTY_cat5 = 1, SMK_10_gate = NA)
#' # Returns: NA::a
#'
#' # 2001 cycle (no gate variable)
#' assess_quit_pathway(SMKDSTY_cat5 = 3, SMK_10_gate = NA)
#' # Returns: NA::b (pathway unknown)
#' }
#'
#' @export
assess_quit_pathway <- function(SMKDSTY_cat5, SMK_10_gate, output_format = "tagged_na") {

  # Handle empty input vectors
  if (length(SMKDSTY_cat5) == 0) return(numeric(0))

  # === STEP 1: DATA CLEANING AND VALIDATION ===
  cleaned <- clean_variables(vars = list(
    SMKDSTY_cat5 = SMKDSTY_cat5,
    SMK_10_gate = SMK_10_gate
  ), output_format = output_format)

  # === STEP 2: DOMAIN LOGIC WITH MISSING DATA FUNCTIONS ===
  result <- dplyr::case_when(
    # Missing data detection first
    any_missing(cleaned$SMKDSTY_cat5) ~
      get_priority_missing(cleaned$SMKDSTY_cat5, output_format = output_format),

    # Current or never smokers -> not applicable
    cleaned$SMKDSTY_cat5 %in% c(1L, 2L, 5L) ~
      assign_missing("not_applicable", "quit_pathway", output_format),

    # Former occasional (never smoked daily) -> pathway 3
    cleaned$SMKDSTY_cat5 == 4L ~ 3L,

    # Former daily pathways (SMKDSTY_cat5 == 3)
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_10_gate) & cleaned$SMK_10_gate == 1L ~ 1L,  # Direct quit
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_10_gate) & cleaned$SMK_10_gate == 2L ~ 2L,  # Gradual reducer
    cleaned$SMKDSTY_cat5 == 3L & any_missing(cleaned$SMK_10_gate) ~
      assign_missing("not_stated", "quit_pathway", output_format),  # 2001 or missing gate

    # Default: unknown
    .default = assign_missing("not_stated", "quit_pathway", output_format)
  )

  # === STEP 3: OUTPUT CLEANING ===
  output_cleaned <- clean_variables(vars = list(
    quit_pathway = result
  ), output_format = output_format)

  return(output_cleaned$quit_pathway)
}

# ------------------------------------------------------------------------------
# calculate_time_quit_complete - Unified years since completely quit
# ------------------------------------------------------------------------------

#' Calculate Time Since Completely Quit Smoking
#'
#' Unified years since completely quit smoking, combining all quit pathways:
#' - Former occasional smokers (SMK_06 series)
#' - Former daily smokers who quit when stopped daily (SMK_09 + gate=1)
#' - Former daily smokers who quit later (SMK_10 series + gate=2)
#'
#' @details
#' **Implementation method**: 3-step architecture
#' - **Step 1**: clean_variables() - Clean all inputs
#' - **Step 2**: Missing data functions + domain logic - Select appropriate time source
#' - **Step 3**: Output cleaning
#'
#' **Pathway selection logic**:
#' - Former occasional (SMKDSTY_cat5 == 4): Use time_quit_occ (SMK_06A_cont)
#' - Former daily, direct quit (SMKDSTY_cat5 == 3, gate == 1): Use time_quit_daily (SMK_09A_cont)
#' - Former daily, gradual (SMKDSTY_cat5 == 3, gate == 2): Use time_quit_complete_daily (SMK_10A_cont)
#' - Former daily, 2001 fallback (gate == NA): Use time_quit_daily as proxy
#'
#' **Era-specific handling**:
#' - 2001: No SMK_10 gate, uses time_quit_daily as proxy
#' - 2003-2014: Full pathway logic with gate variable
#' - 2015-2021: Same logic, different source variable names
#' - 2022-2023: Same logic, uses SPU_25 categorical with midpoint imputation
#'
#' @param SMKDSTY_cat5 Numeric vector. 5-category smoking status
#' @param SMK_10_gate Numeric vector. Quit timing gate (1 or 2)
#' @param time_quit_occ Numeric vector. Years since quit (former occasional, from SMK_06A_cont)
#' @param time_quit_daily Numeric vector. Years since stopped daily (from SMK_09A_cont)
#' @param time_quit_complete_daily Numeric vector. Years since completely quit (from SMK_10A_cont)
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Numeric vector of continuous years since completely quit (0-80+), with:
#' - NA::a for current smokers and never smokers
#' - NA::b for missing/refused
#'
#' @examples
#' \dontrun{
#' # Former occasional, quit 5 years ago
#' calculate_time_quit_complete(
#'   SMKDSTY_cat5 = 4, SMK_10_gate = NA,
#'   time_quit_occ = 5.0, time_quit_daily = NA, time_quit_complete_daily = NA
#' )
#' # Returns: 5.0
#'
#' # Former daily, quit when stopped daily (direct quit)
#' calculate_time_quit_complete(
#'   SMKDSTY_cat5 = 3, SMK_10_gate = 1,
#'   time_quit_occ = NA, time_quit_daily = 3.0, time_quit_complete_daily = NA
#' )
#' # Returns: 3.0
#'
#' # Former daily, continued occasional, quit later (gradual)
#' calculate_time_quit_complete(
#'   SMKDSTY_cat5 = 3, SMK_10_gate = 2,
#'   time_quit_occ = NA, time_quit_daily = 5.0, time_quit_complete_daily = 2.0
#' )
#' # Returns: 2.0
#'
#' # 2001 fallback (no gate available)
#' calculate_time_quit_complete(
#'   SMKDSTY_cat5 = 3, SMK_10_gate = NA,
#'   time_quit_occ = NA, time_quit_daily = 4.0, time_quit_complete_daily = NA
#' )
#' # Returns: 4.0
#' }
#'
#' @export
calculate_time_quit_complete <- function(SMKDSTY_cat5, SMK_10_gate,
                                         time_quit_occ, time_quit_daily,
                                         time_quit_complete_daily,
                                         output_format = "tagged_na") {

  # Handle empty input vectors
  if (length(SMKDSTY_cat5) == 0) return(numeric(0))

  # === STEP 1: DATA CLEANING AND VALIDATION ===
  cleaned <- clean_variables(vars = list(
    SMKDSTY_cat5 = SMKDSTY_cat5,
    SMK_10_gate = SMK_10_gate,
    time_quit_occ = time_quit_occ,
    time_quit_daily = time_quit_daily,
    time_quit_complete_daily = time_quit_complete_daily
  ), output_format = output_format)

  # === STEP 2: DOMAIN LOGIC WITH MISSING DATA FUNCTIONS ===
  result <- dplyr::case_when(
    # Missing data detection first
    any_missing(cleaned$SMKDSTY_cat5) ~
      get_priority_missing(cleaned$SMKDSTY_cat5, output_format = output_format),

    # Never smoked -> not applicable
    cleaned$SMKDSTY_cat5 == 5L ~
      assign_missing("not_applicable", "time_quit_complete", output_format),

    # Current smokers (daily or occasional) -> not applicable
    cleaned$SMKDSTY_cat5 %in% c(1L, 2L) ~
      assign_missing("not_applicable", "time_quit_complete", output_format),

    # Former occasional (SMKDSTY_cat5 == 4) -> use time_quit_occ
    cleaned$SMKDSTY_cat5 == 4L & !any_missing(cleaned$time_quit_occ) ~
      cleaned$time_quit_occ,
    cleaned$SMKDSTY_cat5 == 4L & any_missing(cleaned$time_quit_occ) ~
      get_priority_missing(cleaned$time_quit_occ, output_format = output_format),

    # Former daily (SMKDSTY_cat5 == 3), direct quit (gate == 1) -> use time_quit_daily
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_10_gate) & cleaned$SMK_10_gate == 1L &
      !any_missing(cleaned$time_quit_daily) ~ cleaned$time_quit_daily,
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_10_gate) & cleaned$SMK_10_gate == 1L &
      any_missing(cleaned$time_quit_daily) ~
      get_priority_missing(cleaned$time_quit_daily, output_format = output_format),

    # Former daily (SMKDSTY_cat5 == 3), gradual (gate == 2) -> use time_quit_complete_daily
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_10_gate) & cleaned$SMK_10_gate == 2L &
      !any_missing(cleaned$time_quit_complete_daily) ~ cleaned$time_quit_complete_daily,
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_10_gate) & cleaned$SMK_10_gate == 2L &
      any_missing(cleaned$time_quit_complete_daily) ~
      get_priority_missing(cleaned$time_quit_complete_daily, output_format = output_format),

    # Former daily (SMKDSTY_cat5 == 3), 2001 fallback (no gate) -> use time_quit_daily as proxy
    cleaned$SMKDSTY_cat5 == 3L & any_missing(cleaned$SMK_10_gate) &
      !any_missing(cleaned$time_quit_daily) ~ cleaned$time_quit_daily,
    cleaned$SMKDSTY_cat5 == 3L & any_missing(cleaned$SMK_10_gate) &
      any_missing(cleaned$time_quit_daily) ~
      get_priority_missing(cleaned$time_quit_daily, output_format = output_format),

    # Default: not stated
    .default = assign_missing("not_stated", "time_quit_complete", output_format)
  )

  # === STEP 3: OUTPUT CLEANING ===
  output_cleaned <- clean_variables(vars = list(
    time_quit_complete = result
  ), output_format = output_format)

  return(output_cleaned$time_quit_complete)
}
