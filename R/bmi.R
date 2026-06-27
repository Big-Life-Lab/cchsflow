# ==============================================================================
# BMI (Body Mass Index) Functions
# ==============================================================================
#
# Source-agnostic derived variable functions using the v3 3-step architecture.
# These functions use semantic parameter names (height_m, weight_kg) and work
# with both PUMF and Master data. The worksheet routes the appropriate source
# variables (HWTGHTM/HWTGWTK for PUMF, HWTDHTM/HWTDWTK for Master) to the
# same function parameters.
#
# This is the same pattern used by calculate_pack_years() in smoke-pack-years.R.

# Connor Gorber et al. (2008) bias-correction coefficients
BMI_CORRECTION_MALE <- list(intercept = -1.07575, slope = 1.07592)
BMI_CORRECTION_FEMALE <- list(intercept = -0.12374, slope = 1.05129)

#' @title Calculate Body Mass Index (BMI)
#'
#' @description
#' Calculates BMI from height and weight using the standard formula:
#' weight (kg) / height (m)^2. Source-agnostic — works with both PUMF
#' and Master data via worksheet routing.
#'
#' @param height_m Height in metres.
#' @param weight_kg Weight in kilograms.
#' @param output_format Output missing data format: "tagged_na" (default) or
#'   "original".
#'
#' @return Numeric vector of BMI values (kg/m^2). Missing inputs produce
#'   tagged NAs with priority: NA::b (not stated) over NA::a (not applicable).
#'
#' @details
#' ## PUMF vs Master
#'
#' This function is source-agnostic. The worksheet routes different source
#' variables to the same parameters depending on database type:
#'
#' | Parameter | PUMF source | Master source |
#' |-----------|-------------|---------------|
#' | height_m  | HWTGHTM (grouped) | HWTDHTM (continuous) |
#' | weight_kg | HWTGWTK (grouped) | HWTDWTK (continuous) |
#'
#' PUMF values are midpoint-imputed from grouped categories; Master values
#' are true continuous measurements. The BMI formula is identical — the
#' precision difference comes from the input variables.
#'
#' @examples
#' # Scalar
#' calculate_bmi(height_m = 1.75, weight_kg = 70)
#'
#' # Vector
#' calculate_bmi(
#'   height_m = c(1.75, 1.60, NA),
#'   weight_kg = c(70, 55, 70)
#' )
#'
#' # Dataframe
#' library(dplyr)
#' data.frame(ht = c(1.75, 1.60), wt = c(70, 55)) %>%
#'   mutate(bmi = calculate_bmi(ht, wt))
#'
#' # Standalone with rec_with_table
#' \dontrun{
#' result <- rec_with_table(
#'   cchs2015_2016_p,
#'   variables = variables,
#'   variable_details = variable_details,
#'   log = TRUE
#' )
#' result$HWTGBMI_der
#' }
#'
#' @references
#' World Health Organization. (2000). Obesity: preventing and managing the
#' global epidemic. WHO Technical Report Series, 894.
#'
#' @param ... Arguments passed from deprecated aliases.
#'
#' @seealso [adjust_bmi()] for bias-corrected BMI, [categorize_bmi()] for
#'   WHO categories.
#' @export
calculate_bmi <- function(height_m, weight_kg, output_format = "tagged_na") {
  # Step 1: Clean inputs — use PUMF variable names for pattern lookup.
  # When called via rec_with_table(), inputs are already pre-cleaned by
  # the feeder variable rows; Step 1 is a safety net for direct callers.
  cleaned <- clean_variables(
    vars = list(HWTGHTM = height_m, HWTGWTK = weight_kg),
    output_format = "tagged_na"
  )

  ht <- cleaned$HWTGHTM
  wt <- cleaned$HWTGWTK

  # Step 2: Domain logic
  result <- dplyr::case_when(
    any_missing(ht, wt) ~
      get_priority_missing(ht, wt, output_format = output_format),
    ht <= 0 ~
      assign_missing("not_stated", "HWTGBMI_der", output_format),
    .default = wt / (ht^2)
  )

  # Step 3: Clean output — validate range and convert to requested format
  output_cleaned <- clean_variables(
    vars = list(HWTGBMI_der = result),
    output_format = output_format
  )
  output_cleaned$HWTGBMI_der
}

#' @title Calculate bias-corrected BMI
#'
#' @description
#' Applies sex-specific correction from Connor Gorber et al. (2008) to
#' account for self-reporting bias in height and weight. Source-agnostic —
#' works with both PUMF and Master data via worksheet routing.
#'
#' @param sex Sex (1 = male, 2 = female). CCHS missing codes (6-9) are
#'   handled automatically.
#' @param height_m Height in metres.
#' @param weight_kg Weight in kilograms.
#' @param output_format Output missing data format: "tagged_na" (default) or
#'   "original".
#'
#' @return Numeric vector of bias-corrected BMI values.
#'
#' @details
#' ## Correction formulas
#'
#' Connor Gorber et al. (2008) derived sex-specific linear corrections
#' from measured vs self-reported anthropometrics:
#'
#' - Male: -1.07575 + 1.07592 * BMI
#' - Female: -0.12374 + 1.05129 * BMI
#'
#' ## PUMF vs Master
#'
#' This function is source-agnostic. The worksheet routes different source
#' variables to the same parameters depending on database type:
#'
#' | Parameter | PUMF source | Master source |
#' |-----------|-------------|---------------|
#' | sex       | DHH_SEX     | DHH_SEX       |
#' | height_m  | HWTGHTM (grouped) | HWTDHTM (continuous) |
#' | weight_kg | HWTGWTK (grouped) | HWTDWTK (continuous) |
#'
#' @examples
#' # Scalar — male
#' adjust_bmi(sex = 1, height_m = 1.75, weight_kg = 70)
#'
#' # Scalar — female
#' adjust_bmi(sex = 2, height_m = 1.65, weight_kg = 60)
#'
#' # Vector
#' adjust_bmi(
#'   sex = c(1, 2, 1),
#'   height_m = c(1.75, 1.65, 1.80),
#'   weight_kg = c(70, 60, 80)
#' )
#'
#' # Dataframe
#' library(dplyr)
#' data.frame(sex = c(1, 2), ht = c(1.75, 1.65), wt = c(70, 60)) %>%
#'   mutate(adj_bmi = adjust_bmi(sex, ht, wt))
#'
#' # Standalone with rec_with_table
#' \dontrun{
#' result <- rec_with_table(
#'   cchs2015_2016_p,
#'   variables = variables,
#'   variable_details = variable_details,
#'   log = TRUE
#' )
#' result$HWTGCOR_der
#' }
#'
#' @references
#' Connor Gorber, S., et al. (2008). The accuracy of self-reported height
#' and weight in a nationally representative sample of Canadian adults.
#' *Obesity*, 16(10), 2326-2332.
#'
#' @param ... Arguments passed from deprecated aliases.
#'
#' @seealso [calculate_bmi()] for standard BMI, [categorize_bmi()] for
#'   WHO categories.
#' @export
adjust_bmi <- function(sex, height_m, weight_kg,
                       output_format = "tagged_na") {
  # Step 1: Clean inputs
  cleaned <- clean_variables(
    vars = list(DHH_SEX = sex, HWTGHTM = height_m, HWTGWTK = weight_kg),
    output_format = "tagged_na"
  )

  s <- cleaned$DHH_SEX
  ht <- cleaned$HWTGHTM
  wt <- cleaned$HWTGWTK

  # Calculate raw BMI for valid height/weight (internal, no output cleaning)
  raw_bmi <- dplyr::case_when(
    any_missing(ht, wt) ~
      get_priority_missing(ht, wt, output_format = "tagged_na"),
    ht <= 0 ~ haven::tagged_na("b"),
    .default = wt / (ht^2)
  )

  # Step 2: Apply sex-specific correction
  result <- dplyr::case_when(
    any_missing(raw_bmi) ~
      get_priority_missing(raw_bmi, s, output_format = output_format),
    any_missing(s) ~
      get_priority_missing(s, output_format = output_format),
    s == 1 ~
      BMI_CORRECTION_MALE$intercept + BMI_CORRECTION_MALE$slope * raw_bmi,
    s == 2 ~
      BMI_CORRECTION_FEMALE$intercept + BMI_CORRECTION_FEMALE$slope * raw_bmi,
    .default = assign_missing("not_stated", "HWTGCOR_der", output_format)
  )

  # Step 3: Clean output
  output_cleaned <- clean_variables(
    vars = list(HWTGCOR_der = result),
    output_format = output_format
  )
  output_cleaned$HWTGCOR_der
}

#' @title Categorize BMI into WHO categories
#'
#' @description
#' Maps continuous BMI to the standard 4-category WHO classification.
#' Source-agnostic — works with BMI from any source (PUMF, Master, or
#' external data).
#'
#' @param bmi Continuous BMI value (from [calculate_bmi()] or any source).
#' @param output_format Output missing data format: "tagged_na" (default) or
#'   "original".
#'
#' @return Integer vector: 1 = underweight (< 18.5), 2 = normal (18.5-24.9),
#'   3 = overweight (25.0-29.9), 4 = obese (>= 30.0).
#'
#' @details
#' ## WHO BMI classification
#'
#' Uses the standard WHO adult BMI classification (WHO, 2000):
#'
#' | Category | BMI range | Code |
#' |----------|-----------|------|
#' | Underweight | < 18.5 | 1 |
#' | Normal weight | 18.5-24.9 | 2 |
#' | Overweight | 25.0-29.9 | 3 |
#' | Obese | >= 30.0 | 4 |
#'
#' This function accepts BMI from any source — the `bmi` parameter is
#' fully semantic. The worksheet routes HWTGBMI_der (PUMF) or
#' HWTDBMI_der (Master) to the same function.
#'
#' @examples
#' # Scalar
#' categorize_bmi(bmi = 27.3)
#'
#' # Vector
#' categorize_bmi(bmi = c(16, 22, 27, 35))
#'
#' # Dataframe
#' library(dplyr)
#' data.frame(bmi = c(16.5, 22.0, 27.3, 35.0)) %>%
#'   mutate(bmi_cat = categorize_bmi(bmi))
#'
#' # Standalone with rec_with_table
#' \dontrun{
#' result <- rec_with_table(
#'   cchs2015_2016_p,
#'   variables = variables,
#'   variable_details = variable_details,
#'   log = TRUE
#' )
#' result$HWTGBMI_der_cat4
#' }
#'
#' @references
#' World Health Organization. (2000). Obesity: preventing and managing the
#' global epidemic. WHO Technical Report Series, 894.
#'
#' @param ... Arguments passed from deprecated aliases.
#'
#' @seealso [calculate_bmi()], [adjust_bmi()]
#' @export
categorize_bmi <- function(bmi, output_format = "tagged_na") {
  # Step 1: Clean input
  cleaned <- clean_variables(
    vars = list(HWTGBMI_der = bmi),
    output_format = "tagged_na"
  )

  b <- cleaned$HWTGBMI_der

  # Step 2: WHO category boundaries
  result <- dplyr::case_when(
    any_missing(b) ~
      get_priority_missing(b, output_format = output_format),
    b < 18.5 ~ 1L,
    b < 25.0 ~ 2L,
    b < 30.0 ~ 3L,
    b >= 30.0 ~ 4L,
    .default = assign_missing("not_stated", "HWTGBMI_der_cat4", output_format)
  )

  # Step 3: Clean output
  output_cleaned <- clean_variables(
    vars = list(HWTGBMI_der_cat4 = result),
    output_format = output_format
  )
  prep_cat_output(output_cleaned$HWTGBMI_der_cat4)
}
