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

#' @title Calculate years since completely quit smoking
#'
#' @description
#' Derive pathway-aware years since the respondent completely stopped
#' smoking, using 5-category status and the quit-timing gate to route
#' to the correct continuous feeder variable.
#'
#' @details
#' The function routes former smokers to the appropriate cessation
#' timing variable: SMK_06A_cont for former occasional smokers,
#' SMK_09A_cont for former daily direct quitters (gate = 1),
#' SMK_10A_cont for gradual reducers (gate = 2), and SMK_09A_cont as
#' a proxy when the gate is missing. Current smokers and never smokers
#' receive tagged_na("a"). Not supported: 2001 (SMK_10A missing),
#' 2022 (feeders skip 2022), cchs2023_p (SMK_10A_cont Master-only).
#'
#' @param SMKDSTY_cat5 Numeric. 5-category smoking status
#'   (1 = daily, 2 = occasional, 3 = former daily, 4 = former
#'   occasional, 5 = never).
#' @param SMK_10_gate Numeric. Quit timing gate (1 = direct quit,
#'   2 = gradual reducer).
#' @param SMK_06A_cont Numeric. Years since quit, former occasional.
#' @param SMK_09A_cont Numeric. Years since stopped daily smoking.
#' @param SMK_10A_cont Numeric. Years since quit completely (gradual).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of years since completely quit (0-80+).
#'   Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar: former occasional smoker
#' calculate_time_quit_smoking_complete(
#'   SMKDSTY_cat5 = 4, SMK_10_gate = NA,
#'   SMK_06A_cont = 5.0, SMK_09A_cont = NA,
#'   SMK_10A_cont = NA
#' )
#'
#' @seealso \code{\link{calculate_time_quit_smoking_daily}},
#'   \code{\link{assess_quit_pathway}}
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

    # Former daily (cat5 == 3), no gate available -> use SMK_09A_cont as proxy
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

#' @title Calculate years since stopped smoking daily
#'
#' @description
#' Derive continuous years since the respondent stopped daily smoking,
#' preferring Master exact values (SMK_09C) with PUMF midpoint fallback
#' (SMK_09A_cont).
#'
#' @details
#' Routing: SMK_09C (Master 2001-2021 exact years) takes priority;
#' SMK_09A_cont (PUMF midpoint imputation) is used as fallback. Only
#' former daily smokers (SMKDSTY_cat5 == 3) receive valid values.
#' Current smokers, never smokers, and former occasional-only smokers
#' receive tagged_na("a").
#'
#' @param SMKDSTY_cat5 Numeric. 5-category smoking status
#'   (1 = daily, 2 = occasional, 3 = former daily, 4 = former
#'   occasional, 5 = never).
#' @param SMK_09A_cont Numeric. PUMF midpoint-imputed years since
#'   stopped daily.
#' @param SMK_09C Numeric. Master exact years since stopped daily
#'   (NULL if not available).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Numeric vector of years since stopped daily (0-80+).
#'   Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar: former daily smoker with PUMF data
#' calculate_time_quit_smoking_daily(
#'   SMKDSTY_cat5 = 3, SMK_09A_cont = 2.5, SMK_09C = NA
#' )
#'
#' @seealso \code{\link{calculate_time_quit_smoking_complete}},
#'   \code{\link{assess_quit_pathway}}
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

#' @title Assess smoking cessation pathway
#'
#' @description
#' Classify former smokers by their cessation pathway using 5-category
#' smoking status and the quit-timing gate variable.
#'
#' @details
#' Pathway categories: 1 = direct quit (stopped daily and quit
#' completely), 2 = gradual reducer (stopped daily, continued
#' occasional, then quit), 3 = former occasional (never smoked
#' daily). Current and never smokers receive tagged_na("a"). In 2001,
#' SMK_10_gate is unavailable, so former daily smokers receive
#' tagged_na("b").
#'
#' @param SMKDSTY_cat5 Numeric. 5-category smoking status
#'   (1 = daily, 2 = occasional, 3 = former daily, 4 = former
#'   occasional, 5 = never).
#' @param SMK_10_gate Numeric. Quit timing gate
#'   (1 = quit when stopped daily, 2 = quit later).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector of pathway codes (1 = direct quit,
#'   2 = gradual reducer, 3 = former occasional). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar: direct quit
#' assess_quit_pathway(SMKDSTY_cat5 = 3, SMK_10_gate = 1)
#'
#' # Scalar: former occasional
#' assess_quit_pathway(SMKDSTY_cat5 = 4, SMK_10_gate = NA)
#'
#' @seealso \code{\link{calculate_time_quit_smoking_complete}},
#'   \code{\link{calculate_time_quit_smoking_daily}}
#'
#' @export
assess_quit_pathway <- function(SMKDSTY_cat5 = NULL, SMK_10_gate = NULL,
                                output_format = "tagged_na") {

  # Handle all-NULL inputs (variable not collected in this cycle)
  if (is.null(SMKDSTY_cat5) && is.null(SMK_10_gate)) {
    return(haven::tagged_na("c"))
  }
  n <- max(length(SMKDSTY_cat5), length(SMK_10_gate))
  optional <- expand_null_inputs(list(
    SMKDSTY_cat5 = SMKDSTY_cat5,
    SMK_10_gate = SMK_10_gate
  ), n)
  SMKDSTY_cat5 <- optional$SMKDSTY_cat5
  SMK_10_gate <- optional$SMK_10_gate

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

  return(prep_cat_output(output_cleaned$quit_pathway))
}

# ------------------------------------------------------------------------------
# calculate_time_quit_complete - REMOVED
# ------------------------------------------------------------------------------
# Merged into calculate_time_quit_smoking_complete() which now includes
# pathway-aware logic plus SMKDVSTP Master priority. The old prototype
# function with positional time_quit_occ/time_quit_daily parameters has been
# replaced by the canonical function using cchsflow variable names directly.
