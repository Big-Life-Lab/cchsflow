# Pattern: Multi-source routing

## What it is

A function that selects the best available value from multiple sources,
typically prioritising Master (exact continuous) over PUMF (midpoint
imputed). May also filter by smoking status to determine applicability.

**Note:** Do not confuse multi-source routing with PUMF/Master
variants. When PUMF and Master use the same formula with different
input variables, use ONE source-agnostic function with semantic params
(see formula-calculation pattern). Multi-source routing is for when a
single function needs to choose between multiple available sources at
runtime (e.g., prefer continuous over midpoint when both are present).

## When to use

- Multiple sources provide the same information at different precision levels
- A priority chain determines which source to use
- May include status-based filtering (L5 variant)
- Examples: quit timing (Master exact years vs PUMF midpoint), age started
  daily (daily smokers only vs former daily smokers only)

## How to recognise from worksheet

```
variable,                        variableStart,                    recEnd
time_quit_smoking_daily,         DerivedVar::[SMKDSTY_cat5,SMK_09A_cont,SMK_09C],  Func::calculate_time_quit_smoking_daily
```

DerivedVar with multiple sources that represent the same concept at
different precision or from different file types. The function implements
the priority logic.

## Bronze template

```r
calculate_my_routed_var <- function(source_pumf, source_master = NULL) {
  ifelse(!is.na(source_master), source_master,
  ifelse(!is.na(source_pumf), source_pumf, NA))
}
```

## Silver template

```r
#' @title Calculate [routed variable]
#'
#' @description Selects the best available value from Master (exact) or
#' PUMF (midpoint) sources.
#'
#' @param source_pumf PUMF midpoint-imputed value.
#' @param source_master Master exact continuous value (may be NULL/NA if
#'   working with PUMF data).
#'
#' @return Numeric vector using Master when available, PUMF otherwise.
#'
#' @examples
#' # Scalar — PUMF only
#' calculate_my_routed_var(source_pumf = 1.5)
#'
#' # Scalar — Master available
#' calculate_my_routed_var(source_pumf = 1.5, source_master = 1.3)
#'
#' # Vector — mixed availability
#' calculate_my_routed_var(
#'   source_pumf = c(1.5, 2.5, 0.5),
#'   source_master = c(1.3, NA, 0.4)
#' )
#'
#' @export
calculate_my_routed_var <- function(source_pumf, source_master = NULL) {
  dplyr::case_when(
    !is.na(source_master) ~ source_master,
    !is.na(source_pumf)   ~ source_pumf,
    .default = NA_real_
  )
}
```

## Gold template

```r
calculate_my_routed_var <- function(status, source_pumf,
                                    source_master = NULL,
                                    output_format = "tagged_na") {
  # Step 1: Clean inputs — always tagged_na for Step 2
  cleaned <- clean_variables(
    vars = list(
      status = status,
      source_pumf = source_pumf,
      source_master = source_master
    ),
    output_format = "tagged_na"
  )

  # Step 2: Domain logic — priority chain with universe check
  result <- dplyr::case_when(
    # Universe check: only applicable to certain statuses
    any_missing(cleaned$status) ~
      get_priority_missing(cleaned$status),
    cleaned$status %in% c(1, 2, 3, 6) ~
      assign_missing("not_applicable", "my_routed_var", output_format),

    # Priority: Master exact > PUMF midpoint
    !any_missing(cleaned$source_master) ~ cleaned$source_master,
    !any_missing(cleaned$source_pumf)   ~ cleaned$source_pumf,

    # All sources missing
    .default = get_priority_missing(cleaned$source_pumf,
                                     cleaned$source_master)
  )

  # Step 3: Clean outputs — user's requested format
  output_clean <- clean_variables(
    vars = list(my_routed_var = result),
    output_format = output_format
  )
  output_clean$my_routed_var
}
```

## L5 variant: Status-based filtering

When the function extracts a subset based on status (not priority routing):

```r
# Only daily smokers get age-started-daily; others get NA::a
result <- dplyr::case_when(
  any_missing(cleaned$status) ~ get_priority_missing(cleaned$status),
  cleaned$status == 1 ~ cleaned$age_started,
  .default = assign_missing("not_applicable", "var_name", output_format)
)
```

## Reference implementations

- `calculate_time_quit_smoking_daily()` — R/smoking-cessation.R (Master
  exact years > PUMF midpoint, with universe check)
- `calculate_SMKG040_cont()` — R/smoke-start.R (combines daily + former
  daily age-started with priority)
- `calculate_SMKG203_cont()` — R/smoke-start.R (L5 variant: filters for
  current daily smokers only)
- `calculate_SMKG207_cont()` — R/smoke-start.R (L5 variant: filters for
  former daily smokers only)

## Common mistakes

- Forgetting the universe check — not every respondent should get a value.
  Former-only variables should return NA::a for current/never smokers.
- Using `is.na()` instead of `any_missing()` — `is.na()` doesn't detect
  tagged NAs properly in some contexts
- Not documenting which source takes priority and why
- Mixing up `!any_missing()` (value IS available) with `any_missing()`
  (value IS missing) — easy to invert the logic
