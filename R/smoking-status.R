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

#' @title Classify smoking status into 5 categories (SMKDSTY_cat5)
#'
#' @description
#' Harmonize CCHS smoking status into a 5-category classification across
#' cycles 2001-2023. Implemented via rec_with_table(), not direct R logic.
#'
#' @details
#' This variable collapses the 6-category SMKDSTY into 5 categories by
#' merging occasional-smoker subcategories. Source variables differ by era:
#' SMKDSTY (2001-2014) and SMKDVSTY (2015-2023). Pre-2015 "occasional"
#' and "always occasional" merge into category 2; post-2015 "former
#' occasional" and "experimental" merge into category 4.
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector of smoking status (1 = daily, 2 = occasional,
#'   3 = former daily, 4 = former occasional, 5 = never). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMKDSTY_cat5")
#' }
#'
#' @seealso \code{\link{calculate_SMKDSTY_cat3}},
#'   \code{\link{calculate_SMKDSTY_cat6}}
#'
#' @export
calculate_SMKDSTY_cat5 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMKDSTY_cat5') for implementation")
}

# ================================================================================

# SMKDSTY_cat3 - Smoking status (3 categories) - DOCUMENTATION ONLY
# ================================================================================

#' @title Classify smoking status into 3 categories (SMKDSTY_cat3)
#'
#' @description
#' Harmonize CCHS smoking status into a 3-category classification across
#' cycles 2001-2023. Implemented via rec_with_table(), not direct R logic.
#'
#' @details
#' This is the most simplified smoking classification, collapsing all
#' current smoker subtypes into one category and all former smoker
#' subtypes into another. Source variables: SMKDSTY (2001-2014) and
#' SMKDVSTY (2015-2023). Current smoker merges daily + occasional;
#' former smoker merges all former categories including experimental.
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector of smoking status (1 = current smoker,
#'   2 = former smoker, 3 = never smoked). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMKDSTY_cat3")
#' }
#'
#' @seealso \code{\link{calculate_SMKDSTY_cat5}},
#'   \code{\link{calculate_SMKDSTY_cat6}}
#'
#' @export
calculate_SMKDSTY_cat3 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMKDSTY_cat3') for implementation")
}

# ================================================================================

# SMK_005 - Type of smoker presently (3 categories) - DOCUMENTATION ONLY
# ================================================================================

#' @title Categorize current smoking behaviour (SMK_005)
#'
#' @description
#' Harmonize CCHS variable SMK_005 (type of smoker presently) across
#' cycles 2015-2023. Implemented via rec_with_table(), not direct R logic.
#'
#' @details
#' SMK_005 captures whether the respondent currently smokes daily,
#' occasionally, or not at all. It serves as the primary gate variable
#' for SMKDSTY_cat6 reconstruction in the 2015-2023 period. Source is
#' SMK_005 direct (2015-2021) or derived from SMKDVSTY (2022-2023).
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector of current smoking status (1 = daily,
#'   2 = occasionally, 3 = not at all). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMK_005")
#' }
#'
#' @seealso \code{\link{calculate_SMKDSTY_cat6}},
#'   \code{\link{calculate_SMK_030}}, \code{\link{calculate_SMK_01A}}
#'
#' @export
calculate_SMK_005 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_005') for implementation")
}

# ================================================================================

# SMK_030 - Smoked daily - lifetime (2 categories) - DOCUMENTATION ONLY
# ================================================================================

#' @title Assess lifetime daily smoking history (SMK_030)
#'
#' @description
#' Harmonize CCHS variable SMK_030 (ever smoked daily in lifetime) across
#' cycles 2015-2023. Implemented via rec_with_table(), not direct R logic.
#'
#' @details
#' SMK_030 asks occasional and former smokers whether they ever smoked
#' daily. It distinguishes "former daily" from "never daily" smokers and
#' is the second gate variable for SMKDSTY_cat6 reconstruction. Source
#' is SMK_030 (2015-2021) or SPU_05 (2022-2023).
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector (1 = yes, 2 = no). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMK_030")
#' }
#'
#' @seealso \code{\link{calculate_SMKDSTY_cat6}},
#'   \code{\link{calculate_SMK_005}}, \code{\link{calculate_SMK_01A}}
#'
#' @export
calculate_SMK_030 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_030') for implementation")
}

# ================================================================================

# SMK_01A - In lifetime, smoked 100 or more cigarettes (2 categories) - DOCUMENTATION ONLY
# ================================================================================

#' @title Assess lifetime cigarette consumption threshold (SMK_01A)
#'
#' @description
#' Harmonize CCHS variable SMK_01A (smoked 100+ cigarettes in lifetime)
#' across cycles 2001-2023. Implemented via rec_with_table(), not direct
#' R logic.
#'
#' @details
#' SMK_01A distinguishes experimental/former occasional smokers from
#' never smokers using the 100-cigarette threshold. It is the third
#' gate variable for SMKDSTY_cat6 reconstruction and the longest-running
#' harmonized smoking variable (2001-2023). Sources: cycle-specific
#' variables (2001-2014), SMK_020 (2015-2021), CSS_15 (2022-2023).
#'
#' @param data Data frame containing CCHS data.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector (1 = yes, 2 = no). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' \dontrun{
#' harmonized <- rec_with_table(cchs_data, "SMK_01A")
#' }
#'
#' @seealso \code{\link{calculate_SMKDSTY_cat6}},
#'   \code{\link{calculate_SMK_005}}, \code{\link{calculate_SMK_030}}
#'
#' @export
calculate_SMK_01A <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_01A') for implementation")
}

# ================================================================================

# SMKDSTY_cat6 - Type of smoker (6 categories) - COMPLEX DERIVATION
# ================================================================================

#' @title Derive 6-category smoking status (SMKDSTY_cat6)
#'
#' @description
#' Reconstruct the pre-2015 SMKDSTY 6-category smoking classification
#' for CCHS 2015+ cycles using three harmonized gate variables.
#'
#' @details
#' For 2015+ cycles, CCHS replaced the single SMKDSTY variable with
#' component questions. This function applies the 2013-2014 SMKDSTY
#' specification to harmonized inputs: SMK_005 (current status),
#' SMK_030 (ever smoked daily), and SMK_01A (100+ lifetime cigarettes).
#' For 2001-2014, use rec_with_table() directly.
#'
#' PUMF coverage: 2001-2021, 2023 (2022 gap because SPU_05 was only
#' asked of daily smokers). Master coverage: 2001-2023. Missing data
#' is handled via any_missing() and get_priority_missing().
#'
#' @param SMK_005 Numeric. Current smoking status
#'   (1 = daily, 2 = occasionally, 3 = not at all).
#' @param SMK_030 Numeric. Ever smoked daily (1 = yes, 2 = no).
#' @param SMK_01A Numeric. Lifetime 100+ cigarettes (1 = yes, 2 = no).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector of smoking type (1 = daily, 2 = occasional
#'   former daily, 3 = occasional never daily, 4 = former daily,
#'   5 = former occasional, 6 = never smoked). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar input
#' calculate_SMKDSTY_cat6(SMK_005 = 1, SMK_030 = 1, SMK_01A = 1)
#'
#' # Vector input
#' calculate_SMKDSTY_cat6(
#'   SMK_005 = c(1, 2, 3),
#'   SMK_030 = c(1, 1, 1),
#'   SMK_01A = c(1, 1, 1)
#' )
#'
#' @seealso \code{\link{calculate_SMKDSTY_cat5}},
#'   \code{\link{calculate_SMKDSTY_cat3}},
#'   \code{\link{calculate_SMK_005}},
#'   \code{\link{calculate_SMK_030}},
#'   \code{\link{calculate_SMK_01A}}
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

#' @title Derive 6-category smoking status (deprecated alias)
#'
#' @description
#' Deprecated. Use \code{\link{calculate_SMKDSTY_cat6}} instead.
#'
#' @param ... Arguments passed to \code{\link{calculate_SMKDSTY_cat6}}.
#'
#' @export
calculate_SMKDSTY_original <- function(...) {
  .Deprecated("calculate_SMKDSTY_cat6")
  calculate_SMKDSTY_cat6(...)
}

# ================================================================================

# smoke_simple - Simplified smoking status (4 categories) - COMPLEX DERIVATION
# ================================================================================

#' @title Derive simplified 4-category smoking status (smoke_simple)
#'
#' @description
#' Combine 5-category smoking status with cessation timing to produce a
#' simplified 4-category variable for population-level analysis.
#'
#' @details
#' This derived variable merges SMKDSTY_cat5 with time_quit_smoking to
#' separate recent quitters (5 years or less) from long-term quitters.
#' Former occasional smokers are grouped with recent quitters. The
#' function uses the 3-step architecture (clean, derive, clean) and
#' handles missing data via get_priority_missing().
#'
#' @param SMKDSTY_cat5 Numeric. 5-category smoking status
#'   (1 = daily, 2 = occasional, 3 = former daily, 4 = former
#'   occasional, 5 = never).
#' @param time_quit_smoking Numeric. Years since quitting (continuous).
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector (0 = never smoker, 1 = current smoker,
#'   2 = former quit 5 years or less / former occasional,
#'   3 = former quit more than 5 years). Missing data:
#'   \code{haven::tagged_na("a")} (not applicable),
#'   \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar input
#' calculate_smoke_simple(SMKDSTY_cat5 = 3, time_quit_smoking = 4)
#'
#' # Vector input
#' calculate_smoke_simple(
#'   SMKDSTY_cat5 = c(5, 1, 3, 4),
#'   time_quit_smoking = c(NA, NA, 8, 2)
#' )
#'
#' @seealso \code{\link{calculate_SMKDSTY_cat5}},
#'   \code{\link{calculate_time_quit_smoking_complete}}
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

