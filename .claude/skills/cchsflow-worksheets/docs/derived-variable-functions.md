# Derived variable functions (Func:: pattern)

This document describes how to write R functions referenced by `Func::` rows in `variable_details.csv`. These functions implement the calculation logic for derived variables that cannot be expressed as simple recoding rules.

## The 3-step architecture

Every `Func::` function follows three steps:

```r
calculate_example <- function(input_a, input_b, output_format = "tagged_na") {

  # === STEP 1: DATA CLEANING (input metadata) ===
  cleaned <- clean_variables(vars = list(
    INPUT_VAR_A = input_a,    # list names = worksheet variable names
    INPUT_VAR_B = input_b
  ), output_format = output_format)

  # === STEP 2: DOMAIN LOGIC ===
  result <- dplyr::case_when(
    # ... calculation using cleaned$INPUT_VAR_A, cleaned$INPUT_VAR_B
  )

  # === STEP 3: OUTPUT CLEANING (output metadata) ===
  output_cleaned <- clean_variables(vars = list(
    example_der = result       # list name = output variable name
  ), output_format = output_format)

  return(output_cleaned$example_der)
}
```

### Why both Step 1 and Step 3 call `clean_variables()`

This is not redundant. The two calls use **different variable names** and therefore look up **different metadata**:

- **Step 1** looks up *input* variable patterns — e.g., `SMKDSTY_A` has valid range 1-6 with codes 7/8/9 as missing
- **Step 3** looks up *output* variable patterns — e.g., `pack_years_der` has valid range 0-165

A function that skips Step 3 is a bug, not a simplification. The only exception is `derive_passthrough()`, where input and output are the same variable.

## Function categories

### Pass-through functions

When the worksheet handles PUMF/Master source routing and the function simply cleans and returns, use `derive_passthrough()`:

```r
#' @export
calculate_age_start_smoking <- function(age_start_smoking = NULL,
                                        output_format = "tagged_na") {
  derive_passthrough(age_start_smoking, "age_start_smoking", output_format)
}
```

The helper (`R/utility-functions.R`) handles NULL, empty input, and `clean_variables()` in one call. Use this when:

- The function has a **single input parameter** (plus `output_format`)
- Step 2 is a pure pass-through — no transformation logic
- The worksheet splits handle all PUMF/Master differences

Current examples: `age_start_smoking`, `age_first_cigarette`, `smoked_100_lifetime`.

### Domain logic functions

When the function combines multiple inputs using business rules:

```r
calculate_cigs_per_day <- function(SMKDSTY_A, SMK_204, SMK_208,
                                   output_format = "tagged_na") {
  # Step 1: clean inputs
  # Step 2: case_when routing by smoking status
  # Step 3: clean output
}
```

These keep **all three steps explicit**. Use this when:

- Multiple inputs are combined via `case_when()` or arithmetic
- The function contains genuinely different logic per input status
- Examples: `cigs_per_day`, `time_quit_smoking`, `pack_years`, `SMKDSTY_cat6`

### Documentation-only stubs

For variables harmonised entirely via worksheet recoding rules (no R logic needed), provide a stub so the function is discoverable:

```r
#' @export
calculate_SMK_204 <- function(data, output_format = "tagged_na") {
  stop("DOCUMENTATION ONLY: Use rec_with_table(data, 'SMK_204') for implementation")
}
```

**Important**: Do not create a doc stub if a real implementation exists — this causes a name collision where R silently uses whichever definition is sourced last.

## Parameter naming

### Semantic names for derived variables

Function parameters should describe **what the data means**, not **where it comes from**:

| Preferred (semantic) | Avoid (source-specific) |
|---------------------|------------------------|
| `smoking_status` | `SMKDSTY_A` |
| `age` | `DHHGAGE_cont` |
| `age_start_smoking` | `SMK_040`, `SMKG040_cont` |

The worksheet handles routing the correct source variable to each parameter. The function is source-agnostic.

### When source-specific names are acceptable

Source-level functions that harmonise raw StatCan variables across eras can keep source-specific parameter names:

```r
# Source-level: combines two era-specific variables
calculate_SMKG040_cont <- function(SMKG203_cont, SMKG207_cont, ...)

# Domain logic with well-known source names
calculate_cigs_per_day <- function(SMKDSTY_A, SMK_204, SMK_208, ...)
```

The test: if the parameter name IS a StatCan variable that exists in both PUMF and Master, it's fine to keep it. If the same concept has different names on PUMF vs Master, use a semantic name and let the worksheet route.

### The `clean_variables()` + worksheet name mapping

When parameters are semantic but `clean_variables()` needs worksheet variable names for pattern lookup, pass worksheet names in the list and map afterwards:

```r
# Step 1: use worksheet names for clean_variables() lookup
cleaned_raw <- clean_variables(vars = list(
  SMKDSTY_A = smoking_status,      # worksheet name = variable name in CSV
  DHHGAGE_cont = age
), output_format = output_format)

# Map to semantic names for Step 2
cleaned <- cleaned_raw
cleaned$smoking_status <- cleaned_raw$SMKDSTY_A
cleaned$age <- cleaned_raw$DHHGAGE_cont
```

This is necessary because `clean_variables()` uses the list names to look up valid ranges and missing code patterns from `variable_details.csv`. An unknown name falls back to auto-detection, which can misclassify valid values as missing codes (e.g., smoking status 6 interpreted as NA::a).

## Worksheet feeder alignment

### Positional matching

`rec_with_table()` passes `DerivedVar::[a, b, c]` feeders to the `Func::` function **by position**. The feeder count and order must match the function signature:

```
# Worksheet
variableStart: DerivedVar::[SMKDSTY_A, DHHGAGE_cont, age_start_smoking, cigs_per_day, ...]
recEnd: Func::calculate_pack_years

# Function signature (must match positionally)
calculate_pack_years <- function(smoking_status, age, age_start_smoking, cigs_per_day, ...)
#                                 ^pos 1          ^pos 2  ^pos 3            ^pos 4
```

### Feeder names are resolved, not matched

The feeder names in the worksheet (e.g., `SMKDSTY_A`) are resolved by `rec_with_table()` to actual data columns before being passed to the function. The function parameter names don't need to match the feeder names — only the **position** matters.

### Nested DerivedVar chains

When a feeder is itself a `DerivedVar` (e.g., `age_start_smoking`), `rec_with_table()` resolves it first. This means the function receives the already-computed derived value, not the raw source variable.

## PUMF/Master row splitting for Func:: rows

### When to split

Split `Func::` rows when PUMF and Master route **different source variables** to the same function parameter. The function itself stays source-agnostic — only the worksheet feeder list changes:

```
# PUMF row
databaseStart: cchs2001_p, cchs2003_p, ...
variableStart: DerivedVar::[SMKDSTY_A, DHHGAGE_cont, age_start_smoking, ...]
recEnd: Func::calculate_pack_years

# Master row
databaseStart: cchs2001_m, cchs2003_m, ...
variableStart: DerivedVar::[SMKDSTY_A, DHH_AGE, age_start_smoking, ...]
recEnd: Func::calculate_pack_years
```

The only difference is position 2: `DHHGAGE_cont` (PUMF grouped midpoint) vs `DHH_AGE` (Master true continuous). The function receives both as `age`.

### When NOT to split

Don't split when the feeder variable is the **same on both PUMF and Master**:

- `SMK_204` exists identically on both → no split needed
- `age_start_smoking` is itself a DerivedVar that handles PUMF/Master internally → no split needed for downstream consumers

### Domain routing is not a PUMF/Master split

Functions like `cigs_per_day` take `SMK_204` (current daily) and `SMK_208` (former daily) and route by smoking status. Both variables exist on both PUMF and Master. This is **domain logic**, not a data-file split. The worksheet rows stay combined.

## NULL handling convention

v3 functions use `= NULL` defaults for optional parameters to support standalone use outside `rec_with_table()`:

```r
calculate_pack_years <- function(smoking_status, age, ...,
                                 cigs_occasional = NULL,     # optional
                                 days_per_month = NULL,      # optional
                                 output_format = "tagged_na")
```

NULL inputs are converted to NA vectors at function entry:
```r
if (is.null(cigs_occasional)) cigs_occasional <- rep(NA_real_, n)
```

This is tracked for standardisation in issue #173.

## Reference implementations

| Pattern | Example function | File |
|---------|-----------------|------|
| Pass-through | `calculate_age_start_smoking()` | `R/smoke-start.R` |
| Domain routing | `calculate_cigs_per_day()` | `R/smoke-intensity.R` |
| Multi-input calculation | `calculate_pack_years()` | `R/smoke-pack-years.R` |
| Categorical binning | `calculate_pack_years_categorical()` | `R/smoke-pack-years.R` |
| Source combining | `calculate_time_quit_smoking()` | `R/smoking-cessation.R` |
| Doc stub | `calculate_SMK_204()` | `R/smoke-intensity.R` |
