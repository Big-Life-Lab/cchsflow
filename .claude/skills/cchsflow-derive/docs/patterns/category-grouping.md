# Pattern: Category grouping

## What it is

A function that collapses or remaps multiple input categories into fewer
output categories. The core logic is a mapping table implemented as
`case_when()` arms.

## When to use

- Input is categorical, output is categorical with fewer levels
- The function maps N input categories to M output categories (M < N)
- Examples: 6-category smoking status, ADL difficulty grouping, alcohol
  frequency banding

## How to recognise from worksheet

```
variable,        variableStart,                    recEnd
SMKDSTY_cat6,    DerivedVar::[SMK_005,SMK_030,SMK_01A],  Func::calculate_SMKDSTY_cat6
```

DerivedVar with multiple categorical feeders, output is also categorical.

## Bronze template

```r
calculate_my_grouped_var <- function(input_var) {
  ifelse(input_var %in% c(1, 2), 1L,
  ifelse(input_var %in% c(3, 4), 2L,
  ifelse(input_var %in% c(5, 6), 3L, NA)))
}
```

## Silver template

```r
#' @title Calculate [grouped variable]
#'
#' @description Collapses [source] categories into [N] groups.
#'
#' @param input_var [Source variable] values.
#'
#' @return Integer vector: 1 = [group1], 2 = [group2], 3 = [group3].
#'
#' @examples
#' # Scalar
#' calculate_my_grouped_var(input_var = 3)
#'
#' # Vector
#' calculate_my_grouped_var(input_var = c(1, 3, 5, NA))
#'
#' # Dataframe
#' library(dplyr)
#' df <- data.frame(source = c(1, 2, 3, 4, 5, 6))
#' df %>% mutate(grouped = calculate_my_grouped_var(source))
#'
#' @export
calculate_my_grouped_var <- function(input_var) {
  dplyr::case_when(
    input_var %in% c(1, 2) ~ 1L,
    input_var %in% c(3, 4) ~ 2L,
    input_var %in% c(5, 6) ~ 3L,
    .default = NA_integer_
  )
}
```

## Gold template

```r
calculate_my_grouped_var <- function(input_var,
                                     output_format = "tagged_na") {
  # Step 1: Clean inputs — always tagged_na for Step 2
  cleaned <- clean_variables(
    vars = list(input_var = input_var),
    output_format = "tagged_na"
  )

  # Step 2: Domain logic
  result <- dplyr::case_when(
    any_missing(cleaned$input_var) ~
      get_priority_missing(cleaned$input_var),
    cleaned$input_var %in% c(1, 2) ~ 1L,
    cleaned$input_var %in% c(3, 4) ~ 2L,
    cleaned$input_var %in% c(5, 6) ~ 3L,
    .default = assign_missing("not_applicable", "my_grouped_var",
                              output_format)
  )

  # Step 3: Clean outputs — user's requested format
  output_clean <- clean_variables(
    vars = list(my_grouped_var = result),
    output_format = output_format
  )
  output_clean$my_grouped_var
}
```

## Reference implementations

- `calculate_SMKDSTY_cat6()` — R/smoking-status.R (3 inputs → 6 categories)
- `calculate_smoke_simple()` — R/smoking-status.R (2 inputs → 4 categories,
  uses nested helper variables)

## Common mistakes

- Forgetting the `.default` arm in `case_when()` — leaves unmatched values as NA
  without tracking whether it's NA::a or NA::b
- Not checking `any_missing()` first — missing inputs should propagate, not
  fall through to a category
- Hardcoding category labels instead of using integer codes that match
  `output_format.csv`
