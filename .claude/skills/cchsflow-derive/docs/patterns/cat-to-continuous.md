# Pattern: Categorical to continuous

## What it is

A function that converts categorical ranges into continuous values using
midpoint imputation. The input is a categorical variable (e.g., "1-2 years"),
the output is a continuous value (e.g., 1.5).

## When to use

- Input is categorical with ordered ranges
- Output is continuous (midpoint of each range)
- Examples: quit timing categories → years, age-started categories → age

## How to recognise from worksheet

```
variable,         variableStart,                  recEnd
SMK_06A_cont,     DerivedVar::[SMK_06A_2003plus,SMKG06C],  Func::calculate_SMK_06A_cont
```

The `_cont` suffix is a strong signal. DerivedVar with a categorical source
and optional continuous companion for the open-ended top category.

## Bronze template

```r
calculate_my_var_cont <- function(cat_var) {
  ifelse(cat_var == 1, 0.5,
  ifelse(cat_var == 2, 1.5,
  ifelse(cat_var == 3, 2.5,
  ifelse(cat_var == 4, 5.0, NA))))
}
```

## Silver template

```r
#' @title Calculate [continuous version of categorical variable]
#'
#' @description Converts categorical [variable] to continuous values using
#' midpoint imputation.
#'
#' @param cat_var Categorical input values.
#' @param continuous_companion Optional continuous value for the open-ended
#'   top category (e.g., exact years from Master file).
#'
#' @return Numeric vector of midpoint-imputed values.
#'
#' @examples
#' # Scalar
#' calculate_my_var_cont(cat_var = 2)
#'
#' # Vector
#' calculate_my_var_cont(cat_var = c(1, 2, 3, 4))
#'
#' # With continuous companion for top category
#' calculate_my_var_cont(cat_var = 4, continuous_companion = 7.3)
#'
#' @export
calculate_my_var_cont <- function(cat_var, continuous_companion = NULL) {
  # Midpoints derived from variable_details.csv recEnd ranges
  # Category 1: [0, 1) → 0.5
  # Category 2: [1, 2) → 1.5
  # Category 3: [2, 3) → 2.5
  # Category 4: [3, inf) → use companion if available, else 5.0
  dplyr::case_when(
    cat_var == 1 ~ 0.5,
    cat_var == 2 ~ 1.5,
    cat_var == 3 ~ 2.5,
    cat_var == 4 & !is.na(continuous_companion) ~ continuous_companion,
    cat_var == 4 ~ 5.0,
    .default = NA_real_
  )
}
```

## Gold template

```r
calculate_my_var_cont <- function(cat_var, continuous_companion = NULL,
                                  output_format = "tagged_na") {
  # Step 1: Clean inputs — always tagged_na for Step 2
  cleaned <- clean_variables(
    vars = list(cat_var = cat_var, companion = continuous_companion),
    output_format = "tagged_na"
  )

  # Step 2: Domain logic — midpoints from recEnd ranges
  result <- dplyr::case_when(
    any_missing(cleaned$cat_var) ~
      get_priority_missing(cleaned$cat_var),
    cleaned$cat_var == 1 ~ 0.5,
    cleaned$cat_var == 2 ~ 1.5,
    cleaned$cat_var == 3 ~ 2.5,
    cleaned$cat_var == 4 & !any_missing(cleaned$companion) ~
      cleaned$companion,
    cleaned$cat_var == 4 ~ 5.0,
    .default = assign_missing("not_applicable", "my_var_cont",
                              output_format)
  )

  # Step 3: Clean outputs — user's requested format
  output_clean <- clean_variables(
    vars = list(my_var_cont = result),
    output_format = output_format
  )
  output_clean$my_var_cont
}
```

## Reference implementations

- `calculate_SMK_06A_cont()` — R/smoking-cessation.R (quit timing midpoints)
- `smkg_age_midpoint()` — R/smoking.R (L2 helper for age-started categories)
- `calculate_SMKG203_continuous()` — R/smoking.R (PUMF age-started)

## Common mistakes

- Hardcoding midpoint values that should come from recEnd ranges in
  `variable_details.csv` (acceptable at bronze, not at silver/gold)
- Forgetting the open-ended top category needs special handling (often has
  a continuous companion from Master files)
- Not documenting where midpoint values come from
