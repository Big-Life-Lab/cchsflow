# Pattern: Pass-through

## What it is

A function that passes a single source variable through with cleaning and
validation. No domain logic — the worksheet handles routing the correct
source variable for each database/cycle.

## When to use

- `variableStart` is a single source name (no `DerivedVar::`)
- `recEnd` is `copy` or a simple value remap
- The function just needs to clean and validate the input

## How to recognise from worksheet

```
variable,         variableStart,    recEnd
age_start_smoking, SMKG040_cont,    copy
age_start_smoking, SMK_040,         copy
```

Multiple rows for the same variable with different `databaseStart` ranges —
the worksheet routes the right source, the function just passes through.

## Bronze template

```r
calculate_my_var <- function(input_var) {
  input_var
}
```

## Silver template

```r
#' @title Calculate [variable description]
#'
#' @description Pass-through variable. The worksheet routes the appropriate
#' source variable; this function cleans and validates the input.
#'
#' @param input_var Source variable value(s).
#'
#' @return Cleaned value(s).
#'
#' @examples
#' # Scalar
#' calculate_my_var(input_var = 25)
#'
#' # Vector
#' calculate_my_var(input_var = c(15, 20, 25, NA))
#'
#' # Dataframe
#' library(dplyr)
#' df <- data.frame(source = c(15, 20, 25))
#' df %>% mutate(result = calculate_my_var(source))
#'
#' @export
calculate_my_var <- function(input_var,
                             output_format = "tagged_na") {
  derive_passthrough(input_var, "my_var", output_format)
}
```

## Gold template

Same as silver — pass-through functions are inherently simple. Gold adds
namespace-qualified calls and explicit dependency documentation.

```r
calculate_my_var <- function(input_var,
                             output_format = "tagged_na") {
  derive_passthrough(input_var, "my_var", output_format)
}
```

## Reference implementations

- `calculate_age_start_smoking()` — R/smoke-start.R
- `calculate_age_first_cigarette()` — R/smoke-start.R
- `calculate_smoked_100_lifetime()` — R/smoke-start.R

## Common mistakes

- Writing domain logic that belongs in the worksheet `recEnd` column
- Forgetting the `output_format` parameter
- Not using `derive_passthrough()` (reimplementing the cleaning logic)
