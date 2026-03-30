# Pattern: Formula calculation

## What it is

A function that computes a derived value from multiple inputs using
arithmetic formulas. The logic is mathematical rather than categorical
mapping. Functions use **semantic parameter names** and are
**source-agnostic** — the same function serves both PUMF and Master
data via worksheet routing.

## When to use

- Output is computed from multiple inputs via arithmetic
- Examples: pack-years (age, start age, quit time, cigarettes/day), BMI
  (height, weight), alcohol grams/day

## Source-agnostic design

Functions take semantic parameters (`height_m`, `weight_kg`, `age`)
not CCHS variable names (`HWTGHTM`, `HWTDHTM`). The worksheet routes
different source variables to the same function depending on database
type:

```
# Both rows call the SAME function with different feeder variables
variable,       databaseStart,  variableStart,                    recEnd
HWTGBMI_der,    cchs2001_p,     DerivedVar::[HWTGHTM, HWTGWTK],  Func::calculate_bmi
HWTDBMI_der,    cchs2001_m,     DerivedVar::[HWTDHTM, HWTDWTK],  Func::calculate_bmi
```

The function is portable — it can be copy-pasted into other systems
that have height in metres and weight in kilograms.

## How to recognise from worksheet

```
variable,          variableStart,                                    recEnd
pack_years_der,    DerivedVar::[SMKDSTY_A,DHHGAGE_cont,...],        Func::calculate_pack_years
```

DerivedVar with many feeders and a `Func::` pointing to a calculation
function. The feeder list is typically longer than for other patterns.

## Bronze template

```r
calculate_bmi <- function(height_m, weight_kg) {
  result <- weight_kg / (height_m ^ 2)
  ifelse(result < 10 | result > 60, NA, result)
}
```

## Silver template

```r
#' @title Calculate BMI
#'
#' @description Calculates body mass index from height (metres) and
#' weight (kilograms).
#'
#' @param height_m Height in metres.
#' @param weight_kg Weight in kilograms.
#'
#' @return Numeric vector of BMI values (kg/m^2). Values outside
#'   [10, 60] are set to NA.
#'
#' @examples
#' # Scalar
#' calculate_bmi(height_m = 1.75, weight_kg = 70)
#'
#' # Vector
#' calculate_bmi(height_m = c(1.60, 1.75, 1.80),
#'               weight_kg = c(55, 70, 90))
#'
#' # Dataframe
#' library(dplyr)
#' df <- data.frame(ht = c(1.60, 1.75), wt = c(55, 70))
#' df %>% mutate(bmi = calculate_bmi(ht, wt))
#'
#' @export
calculate_bmi <- function(height_m, weight_kg) {
  dplyr::case_when(
    is.na(height_m) | is.na(weight_kg) ~ NA_real_,
    height_m <= 0 ~ NA_real_,
    TRUE ~ weight_kg / (height_m ^ 2)
  )
}
```

## Gold template

```r
calculate_bmi <- function(height_m, weight_kg,
                          output_format = "tagged_na") {
  # Step 1: Clean inputs — use a representative CCHS variable name for
  # pattern lookup. When called via rec_with_table(), inputs are already
  # pre-cleaned; Step 1 is a safety net for direct callers.
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
    .default = wt / (ht ^ 2)
  )

  # Step 3: Clean outputs — user's requested format
  output_clean <- clean_variables(
    vars = list(HWTGBMI_der = result),
    output_format = output_format
  )
  output_clean$HWTGBMI_der
}
```

**Step 1 mapping note:** The `vars` list keys are CCHS variable names
used for missing code pattern lookup in `variable_details.csv`. The
function's semantic parameter names (`height_m`, `weight_kg`) are the
external API; internally, `clean_variables()` needs a known variable
name to find the right missing codes. Pick a representative CCHS
variable that covers both PUMF and Master patterns — like
`calculate_pack_years()` uses `DHHGAGE_cont` for age regardless of
whether the input comes from PUMF or Master.

## Reference implementations

- `calculate_bmi()` — R/bmi.R (source-agnostic, semantic params,
  worksheet routes PUMF/Master to same function)
- `calculate_pack_years()` — R/smoke-pack-years.R (complex, 6 status
  pathways with different formulas per pathway)
- `.calculate_pack_years_core()` — R/smoke-pack-years.R (internal, pure
  arithmetic with `pmax()` floor values)
- `calculate_pack_years_categorical()` — R/smoke-pack-years.R (formula
  output → categorical binning)

## Common mistakes

- Not handling division by zero (height == 0 for BMI, duration == 0 for rates)
- Forgetting that formula inputs may themselves be derived variables that
  carry missing data — check `any_missing()` on all inputs
- Hardcoding floor/ceiling values instead of using named constants
  (see `PACK_YEARS_CONSTANTS` for the right approach)
- For complex formulas with status-based branching (like pack-years),
  consider extracting the core arithmetic into an internal helper function
