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
#   All _cont variables (SMK_06A_cont, SMK_09A_cont, SMK_10A_cont) use
#   worksheet-only direct recode (DHHGAGE_cont pattern) — no R functions.
#   Midpoint values live in variable_details.csv recEnd, the single source
#   of truth. See worksheet-reference.md § "Worksheet-first principle".
#
# Combining functions (pathway-aware):
# ├── calculate_time_quit_smoking_complete()  → years since completely quit (pathway logic)
# └── calculate_time_quit_smoking_daily()     → years since stopped daily (SMK_09C priority + SMK_09A_cont fallback)
#
# Supporting function:
# └── assess_quit_pathway()  → categorical indicator of how they quit
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


# Package dependencies are declared in DESCRIPTION and loaded via NAMESPACE
# Functions used: haven::tagged_na(), haven::is_tagged_na(), dplyr::case_when()
# Internal functions: clean_variables(), any_missing(), get_priority_missing(), assign_missing()

# ------------------------------------------------------------------------------
# Foundational _cont variables — worksheet-only (no R functions)
# ------------------------------------------------------------------------------
# REMOVED: calculate_SMK_06A_cont(), calculate_SMK_09A_cont(), and
# calculate_SMK_10A_cont() were deleted. All three used hard-coded midpoints
# that duplicated (and in some cases contradicted) variable_details.csv recEnd
# values. The worksheet handles midpoint conversion directly via recStart →
# recEnd rows — the DHHGAGE_cont pattern. No R function is needed.
#
# Use rec_with_table(data, "SMK_06A_cont") (or SMK_09A_cont, SMK_10A_cont)
# for implementation. The combining functions below take pre-computed _cont
# values as parameters.

# ==============================================================================
# COMBINING FUNCTIONS
# ==============================================================================
#
# These functions combine foundational continuous outputs to provide unified
# cessation timing variables. They handle both PUMF (midpoint-imputed) and
# Master (true continuous) pathways.
#
# ==============================================================================

# ------------------------------------------------------------------------------
# calculate_time_quit_smoking_complete - Years since completely quit smoking
# ------------------------------------------------------------------------------

#' Calculate Years Since Completely Quit Smoking
#'
#' Pathway-aware years since the respondent completely quit smoking. Uses
#' cat5 smoking status and the quit-timing gate to route to the appropriate
#' continuous input (SMK_06A_cont / SMK_09A_cont / SMK_10A_cont). Works
#' uniformly across all cycles 2001+ on both PUMF and Master.
#'
#' @details
#' **Implementation method**: 3-step architecture
#' - **Step 1**: clean_variables() - Clean all inputs
#' - **Step 2**: Pathway-aware logic (cat5 + gate routing)
#' - **Step 3**: Output cleaning
#'
#' **Routing logic**:
#' 1. Former occasional (SMKDSTY_cat5 == 4): use SMK_06A_cont
#' 2. Former daily, direct quit (cat5 == 3, gate == 1): use SMK_09A_cont
#' 3. Former daily, gradual reducer (cat5 == 3, gate == 2): use SMK_10A_cont
#' 4. Former daily, 2001 fallback (no gate): use SMK_09A_cont as proxy
#'
#' @param SMKDSTY_cat5 Numeric vector. 5-category smoking status
#' @param SMK_10_gate Numeric vector. Quit timing gate (1 or 2)
#' @param SMK_06A_cont Numeric vector. Years since quit (former occasional)
#' @param SMK_09A_cont Numeric vector. Years since stopped daily
#' @param SMK_10A_cont Numeric vector. Years since quit completely (gradual)
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Numeric vector of continuous years since completely quit (0-80+), with:
#' - NA::a for current smokers and never smokers
#' - NA::b for missing/refused
#'
#' @examples
#' \dontrun{
#' # Former occasional
#' calculate_time_quit_smoking_complete(
#'   SMKDSTY_cat5 = 4, SMK_10_gate = NA,
#'   SMK_06A_cont = 5.0, SMK_09A_cont = NA, SMK_10A_cont = NA
#' )
#' # Returns: 5.0
#'
#' # Former daily, direct quit
#' calculate_time_quit_smoking_complete(
#'   SMKDSTY_cat5 = 3, SMK_10_gate = 1,
#'   SMK_06A_cont = NA, SMK_09A_cont = 3.5, SMK_10A_cont = NA
#' )
#' # Returns: 3.5
#'
#' # Former daily, gradual reducer
#' calculate_time_quit_smoking_complete(
#'   SMKDSTY_cat5 = 3, SMK_10_gate = 2,
#'   SMK_06A_cont = NA, SMK_09A_cont = 5.0, SMK_10A_cont = 2.0
#' )
#' # Returns: 2.0 (when they quit completely, not when they stopped daily)
#' }
#'
#' @export
calculate_time_quit_smoking_complete <- function(SMKDSTY_cat5, SMK_10_gate,
                                                  SMK_06A_cont, SMK_09A_cont,
                                                  SMK_10A_cont,
                                                  output_format = "tagged_na") {

  # Handle empty input vectors
  if (length(SMKDSTY_cat5) == 0) return(numeric(0))

  # === STEP 1: DATA CLEANING AND VALIDATION ===
  cleaned <- clean_variables(vars = list(
    SMKDSTY_cat5 = SMKDSTY_cat5,
    SMK_10_gate = SMK_10_gate,
    SMK_06A_cont = SMK_06A_cont,
    SMK_09A_cont = SMK_09A_cont,
    SMK_10A_cont = SMK_10A_cont
  ), output_format = "tagged_na")

  # === STEP 2: DOMAIN LOGIC ===
  result <- dplyr::case_when(
    # Missing smoking status -> propagate
    any_missing(cleaned$SMKDSTY_cat5) ~
      get_priority_missing(cleaned$SMKDSTY_cat5, output_format = output_format),

    # Never smoked -> not applicable
    cleaned$SMKDSTY_cat5 == 5L ~
      assign_missing("not_applicable", "time_quit_smoking_complete", output_format),

    # Current smokers (daily or occasional) -> not applicable
    cleaned$SMKDSTY_cat5 %in% c(1L, 2L) ~
      assign_missing("not_applicable", "time_quit_smoking_complete", output_format),

    # --- Pathway-aware logic ---

    # Former occasional (cat5 == 4) -> use SMK_06A_cont
    cleaned$SMKDSTY_cat5 == 4L & !any_missing(cleaned$SMK_06A_cont) ~
      cleaned$SMK_06A_cont,
    cleaned$SMKDSTY_cat5 == 4L & any_missing(cleaned$SMK_06A_cont) ~
      get_priority_missing(cleaned$SMK_06A_cont, output_format = output_format),

    # Former daily (cat5 == 3), direct quit (gate == 1) -> use SMK_09A_cont
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_10_gate) &
      cleaned$SMK_10_gate == 1L & !any_missing(cleaned$SMK_09A_cont) ~
      cleaned$SMK_09A_cont,
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_10_gate) &
      cleaned$SMK_10_gate == 1L & any_missing(cleaned$SMK_09A_cont) ~
      get_priority_missing(cleaned$SMK_09A_cont, output_format = output_format),

    # Former daily (cat5 == 3), gradual reducer (gate == 2) -> use SMK_10A_cont
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_10_gate) &
      cleaned$SMK_10_gate == 2L & !any_missing(cleaned$SMK_10A_cont) ~
      cleaned$SMK_10A_cont,
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_10_gate) &
      cleaned$SMK_10_gate == 2L & any_missing(cleaned$SMK_10A_cont) ~
      get_priority_missing(cleaned$SMK_10A_cont, output_format = output_format),

    # Former daily (cat5 == 3), 2001 fallback (no gate) -> use SMK_09A_cont as proxy
    cleaned$SMKDSTY_cat5 == 3L & any_missing(cleaned$SMK_10_gate) &
      !any_missing(cleaned$SMK_09A_cont) ~ cleaned$SMK_09A_cont,
    cleaned$SMKDSTY_cat5 == 3L & any_missing(cleaned$SMK_10_gate) &
      any_missing(cleaned$SMK_09A_cont) ~
      get_priority_missing(cleaned$SMK_09A_cont, output_format = output_format),

    # Default: not stated
    .default = assign_missing("not_stated", "time_quit_smoking_complete", output_format)
  )

  # === STEP 3: OUTPUT CLEANING ===
  output_cleaned <- clean_variables(vars = list(
    time_quit_smoking_complete = result
  ), output_format = output_format)

  return(output_cleaned$time_quit_smoking_complete)
}

# ------------------------------------------------------------------------------
# calculate_time_quit_smoking_daily - Years since stopped smoking daily
# ------------------------------------------------------------------------------

#' Calculate Years Since Stopped Smoking Daily
#'
#' Continuous years since the respondent stopped smoking daily. Uses
#' SMK_09C (Master exact years) when available, falling back to
#' SMK_09A_cont (PUMF midpoint imputation).
#'
#' @details
#' **Implementation method**: 3-step architecture
#' - **Step 1**: clean_variables() - Clean all inputs
#' - **Step 2**: Master priority + PUMF fallback
#' - **Step 3**: Output cleaning
#'
#' **Routing logic**:
#' 1. SMK_09C available (Master 2001-2021): use directly (exact years)
#' 2. SMK_09A_cont available (PUMF, or Master fallback): use midpoint value
#' 3. Current/never/occasional-only smokers: NA::a (not applicable)
#'
#' **Universe**: Former daily smokers only. Former occasional smokers who
#' never smoked daily receive NA::a.
#'
#' @param SMKDSTY_cat5 Numeric vector. 5-category smoking status
#' @param SMK_09A_cont Numeric vector. PUMF midpoint-imputed years since stopped daily
#' @param SMK_09C Numeric vector. Master exact years since stopped daily
#' @param output_format Character. Output format ("tagged_na" or "original")
#'
#' @return Numeric vector of continuous years since stopped smoking daily (0-80+), with:
#' - NA::a for current smokers, never smokers, and former occasional-only smokers
#' - NA::b for missing/refused
#'
#' @examples
#' \dontrun{
#' # Master - exact years available
#' calculate_time_quit_smoking_daily(
#'   SMKDSTY_cat5 = 3, SMK_09A_cont = NA, SMK_09C = 7.0
#' )
#' # Returns: 7.0
#'
#' # PUMF - midpoint imputation
#' calculate_time_quit_smoking_daily(
#'   SMKDSTY_cat5 = 3, SMK_09A_cont = 2.5, SMK_09C = NA
#' )
#' # Returns: 2.5
#'
#' # Former occasional (never daily) - not applicable
#' calculate_time_quit_smoking_daily(
#'   SMKDSTY_cat5 = 4, SMK_09A_cont = NA, SMK_09C = NA
#' )
#' # Returns: NA::a
#' }
#'
#' @export
calculate_time_quit_smoking_daily <- function(SMKDSTY_cat5, SMK_09A_cont,
                                               SMK_09C = NULL,
                                               output_format = "tagged_na") {

  # Handle empty input vectors
  if (length(SMKDSTY_cat5) == 0) return(numeric(0))

  # Handle NULL SMK_09C
  if (is.null(SMK_09C)) {
    SMK_09C <- rep(NA_real_, length(SMKDSTY_cat5))
  }

  # === STEP 1: DATA CLEANING AND VALIDATION ===
  cleaned <- clean_variables(vars = list(
    SMKDSTY_cat5 = SMKDSTY_cat5,
    SMK_09A_cont = SMK_09A_cont,
    SMK_09C = SMK_09C
  ), output_format = "tagged_na")

  # === STEP 2: DOMAIN LOGIC ===
  result <- dplyr::case_when(
    # Missing smoking status -> propagate
    any_missing(cleaned$SMKDSTY_cat5) ~
      get_priority_missing(cleaned$SMKDSTY_cat5, output_format = output_format),

    # Never smoked -> not applicable
    cleaned$SMKDSTY_cat5 == 5L ~
      assign_missing("not_applicable", "time_quit_smoking_daily", output_format),

    # Current smokers (daily or occasional) -> not applicable
    cleaned$SMKDSTY_cat5 %in% c(1L, 2L) ~
      assign_missing("not_applicable", "time_quit_smoking_daily", output_format),

    # Former occasional only (never daily) -> not applicable
    cleaned$SMKDSTY_cat5 == 4L ~
      assign_missing("not_applicable", "time_quit_smoking_daily", output_format),

    # --- Former daily smokers (cat5 == 3) ---

    # Master priority: SMK_09C available -> use exact years
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_09C) ~
      cleaned$SMK_09C,

    # PUMF fallback: SMK_09A_cont available -> use midpoint
    cleaned$SMKDSTY_cat5 == 3L & !any_missing(cleaned$SMK_09A_cont) ~
      cleaned$SMK_09A_cont,

    # Both missing -> propagate missing
    cleaned$SMKDSTY_cat5 == 3L ~
      get_priority_missing(cleaned$SMK_09C, cleaned$SMK_09A_cont, output_format = output_format),

    # Default: not stated
    .default = assign_missing("not_stated", "time_quit_smoking_daily", output_format)
  )

  # === STEP 3: OUTPUT CLEANING ===
  output_cleaned <- clean_variables(vars = list(
    time_quit_smoking_daily = result
  ), output_format = output_format)

  return(output_cleaned$time_quit_smoking_daily)
}

# ==============================================================================
# SUPPORTING FUNCTIONS
# ==============================================================================
#
# assess_quit_pathway() classifies former smokers by cessation pathway.
# Used by the combining functions above and available for direct analysis.
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
  ), output_format = "tagged_na")

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
# calculate_time_quit_complete - REMOVED
# ------------------------------------------------------------------------------
# Merged into calculate_time_quit_smoking_complete() which now includes
# pathway-aware logic plus SMKDVSTP Master priority. The old prototype
# function with positional time_quit_occ/time_quit_daily parameters has been
# replaced by the canonical function using cchsflow variable names directly.
