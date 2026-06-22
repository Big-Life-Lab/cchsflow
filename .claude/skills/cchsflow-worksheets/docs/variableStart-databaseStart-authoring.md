# variableStart and databaseStart authoring

This document covers the complex coordination between `variableStart` and `databaseStart` fields in cchsflow worksheets, including era-specific mappings and derived variable patterns.

## Core principle

**databaseStart should be derived from variableStart**, not authored independently. The variableStart field is the source of truth for which databases a harmonized variable supports.

## How rec_with_table() processes variableStart

The `get_data_variable_name()` function in `R/recode-with-table.R` (lines 380-424) determines which source variable to use:

```r
# Priority 1: Explicit mapping for this database
if (grepl(data_name, var_start_names)) {
  for (var_name in var_start_names_list) {
    if (grepl(data_name, var_name)) {
      data_variable_being_checked <- strsplit(var_name, "::")[[1]][[2]]
    }
  }
# Priority 2: Default [VAR] notation
} else if (grepl("\\[", var_start_names)) {
  data_variable_being_checked <- str_match(var_start_names, "\\[(.*?)\\]")[, 2]
}
```

**Key insight**: The `[VAR]` default applies to ALL databases in that row's databaseStart that don't have explicit `db::VAR` mappings.

## variableStart notation patterns

| Pattern | Meaning | Example |
|---------|---------|---------|
| `db::VAR` | Explicit mapping for one database | `cchs2001_m::SMKA_203` |
| `[VAR]` | Default for unmapped databases | `[SMK_203]` |
| `db::[VAR1, VAR2]` | Multi-variable input for one database | `cchs2015_p::[SMKG005, SMKG040]` |
| `DerivedVar::[VAR1, VAR2]` | Inputs for derived function | Triggers `Func::` processing |

### Invalid patterns

| Pattern | Problem |
|---------|---------|
| `[[VAR1, VAR2]]` | Double brackets not supported - typo |
| `[VAR1, VAR2]` without `db::` | Ambiguous - use explicit mappings |

## CCHS variable naming eras

Variables have different names across eras. **You must use explicit mappings when spanning eras.**

| Era | Years | Pattern | Example |
|-----|-------|---------|---------|
| Pre-2007 | 2001-2005 | Cycle letter in 4th position | `SMKA_203` (2001), `SMKC_203` (2003), `SMKE_203` (2005) |
| 2007-2014 | 2007-2014 | Standard naming | `SMK_203`, `SMKDSTY` |
| Post-2014 | 2015-2021 | 3-digit increments of 5 | `SMK_040`, `SMKDVSTY`, `ADL_005` |
| 2023+ | 2023+ | 2-digit (some domains) | `ADL_05` (was `ADL_005`), `ADL_10` (was `ADL_010`) |

**Note on 2023 renames:** Some domains (e.g., ADL) were renamed again in 2023 from 3-digit to 2-digit numbering (`ADL_005` → `ADL_05`). When a variable's databaseStart spans 2015-2021 and 2023+, the `[VAR]` default cannot be used — explicit mappings are required for both eras. Check cchsflow-docs data dictionaries for each domain.

### Era mapping reference for common variables

| Concept | Pre-2007 | 2007-2014 | Post-2014 | 2022+ |
|---------|----------|-----------|-----------|-------|
| Age started daily (current) | SMKA/C/E_203 | SMK_203 | SMK_040 (filtered) | CSS_25 |
| Age started daily (former) | SMKA/C/E_207 | SMK_207 | SMK_040 (filtered) | CSS_25 |
| Type of smoker (derived) | SMKA/C/EDSTY | SMKDSTY | SMKDVSTY | SMKDVSTY |
| When stopped daily (cat) | SMKA/C/E_09A | SMK_09A | SMK_080 | SPU_25 |
| Years since quit (derived) | SMKA/C/EDSTP | SMKDSTP | SMKDVSTP | SMKDVSTP |

### Cessation timing variables - 2015 redesign

The cessation timing variables underwent a complete renumbering in 2015. **These are commonly missed:**

| Series | Component | Pre-2007 | 2007-2014 | 2015-2021 | 2022+ |
|--------|-----------|----------|-----------|-----------|-------|
| **SMK_06** (Former occasional) | Categorical (A) | SMKA/C/E_06A | SMK_06A | SMK_060 | SPU_10 |
| | Month (B) | SMKA/C/E_06B | SMK_06B | SMK_065 | SPU_10A |
| | Years (C) | SMKA/C/E_06C | SMK_06C | **SMK_070** | SPU_10B |
| **SMK_09** (Stopped daily) | Categorical (A) | SMKA/C/E_09A | SMK_09A | SMK_080 | SPU_25 |
| | Month (B) | SMKA/C/E_09B | SMK_09B | SMK_085 | SPU_25A |
| | Years (C) | SMKA/C/E_09C | SMK_09C | **SMK_090** | SPU_25B |
| **SMK_10** (Quit completely) | Gate | SMKA/C/E_10 | SMK_10 | SMK_095 | SPU_30 |
| | Categorical (A) | SMKA/C/E_10A | SMK_10A | SMK_100 | SPU_35 |
| | Month (B) | SMKA/C/E_10B | SMK_10B | SMK_105 | SPU_35A |
| | Years (C) | SMKA/C/E_10C | SMK_10C | **SMK_110** | SPU_35B |

**Common error**: Using `[SMK_09C]` for 2015+ cycles. The variable is named `SMK_090` in 2015+.

### PUMF grouped variables - 2015 redesign

PUMF files use grouped versions (SMKG prefix). These also changed in 2015:

| Variable | 2007-2014 | 2015+ |
|----------|-----------|-------|
| Years since stopped (occasional) | SMKG06C | SMKG070 |
| Years since stopped daily | SMKG09C | SMKG090 |
| Years since quit completely | SMKG10C | SMKG110 |

## The dangerous default pattern

**WRONG** - Using `[VAR]` across naming eras:

```
databaseStart: cchs2007_2008_m, cchs2015_2016_m, cchs2022_m
variableStart: [SMK_09A]
```

This applies `SMK_09A` to ALL three databases, but `SMK_09A` doesn't exist in 2015+.

**CORRECT** - Explicit era mappings:

```
databaseStart: cchs2007_2008_m, cchs2015_2016_m, cchs2022_m
variableStart: cchs2015_2016_m::SMK_080, cchs2022_m::SMK_080, [SMK_09A]
```

Now `[SMK_09A]` only applies to `cchs2007_2008_m`.

## Mixed direct-recoding and derived-function variables

A harmonized variable can have **multiple blocks of rows** in variable_details.csv with different processing:

### Block types

1. **Direct recoding rows** - Use `recStart`/`recEnd` with midpoints or category mappings
2. **Derived function rows** - Use `Func::function_name` in `recEnd`

### Example: SMKG203_cont

This variable has direct recoding for 2001-2014 and derived function for 2015+:

**Block 1: Direct recoding (2001-2014)**
```csv
variable,databaseStart,variableStart,recStart,recEnd
SMKG203_cont,"cchs2001_p, cchs2003_p","cchs2001_p::SMKAG203, cchs2003_p::SMKCG203",2,13
SMKG203_cont,"cchs2001_p, cchs2003_p","cchs2001_p::SMKAG203, cchs2003_p::SMKCG203",3,17
...
SMKG203_cont,"cchs2005_p, cchs2009_2010_p, ...","cchs2005_p::SMKEG203, [SMKG203]",2,13
SMKG203_cont,"cchs2005_p, cchs2009_2010_p, ...","cchs2005_p::SMKEG203, [SMKG203]",3,16
...
```

**Block 2: Derived function (2015+)**
```csv
variable,databaseStart,variableStart,recStart,recEnd
SMKG203_cont,"cchs2015_2016_p, cchs2017_2018_p, cchs2021_p","DerivedVar::[SMKG005, SMKG040]","[1,55]","Func::calculate_SMKG203_continuous"
SMKG203_cont,"cchs2015_2016_p, cchs2017_2018_p, cchs2021_p","DerivedVar::[SMKG005, SMKG040]",else,NA::b
```

### Key rules for mixed variables

1. **Each block has its own databaseStart** - Direct recoding rows list 2001-2014 databases; derived rows list 2015+ databases

2. **The `[VAR]` default is scoped to the row's databaseStart** - It doesn't apply across blocks

3. **Derived function inputs come from DerivedVar::[], not from databaseStart** - The function receives already-recoded variables

4. **variables.csv must list ALL databases** - Union of all blocks' databaseStart values

5. **variables.csv variableStart is a summary** - Lists all explicit mappings and multi-variable inputs

## Coordination between variables.csv and variable_details.csv

### variables.csv entry (summary)

```csv
variable,databaseStart,variableStart
SMKG203_cont,"cchs2001_p, cchs2003_p, cchs2005_p, ..., cchs2015_2016_p, cchs2017_2018_p, cchs2021_p","cchs2001_p::SMKAG203, cchs2003_p::SMKCG203, cchs2005_p::SMKEG203, cchs2007_2008_p::SMKG203, ..., cchs2015_2016_p::[SMKG005, SMKG040], cchs2017_2018_p::[SMKG005, SMKG040], cchs2021_p::[SMKG005, SMKG040]"
```

### Consistency requirements

See also [csv-conventions.md §4 Union rule](csv-conventions.md) for the canonical statement of this rule.

| Check | Requirement |
|-------|-------------|
| Database coverage | variables.csv databaseStart = union of all variable_details.csv databaseStart for that variable |
| Explicit mappings | All `db::VAR` in variable_details must appear in variables.csv variableStart |
| Multi-variable inputs | All `db::[VAR1, VAR2]` patterns must be consistent |

## Validation infrastructure

### Validate source references against DDI

```r
source("R/validate-all-source-references.R")
result <- validate_all_source_references("path/to/variable_details.csv")
print_all_validation_result(result)
```

This parses variableStart (including applying `[VAR]` defaults to all unmapped databases) and checks each `db::VAR` pair against DDI.

### Check variable existence

```r
source("R/source-lookups.R")

# Single check
variable_exists_in_database("SMK_09A", "cchs2015_2016_m")  # FALSE

# Find correct name
vars <- get_variables_for_database("cchs2015_2016_m")
grep("SMK_08", vars, value = TRUE)  # "SMK_080"
```

### Build validated variableStart

```r
source("R/constrained-authoring.R")

# This will ERROR if any mapping is invalid
build_variableStart(list(
  cchs2007_2008_m = "SMK_09A",
  cchs2015_2016_m = "SMK_080"
))
# Returns: "cchs2007_2008_m::SMK_09A, cchs2015_2016_m::SMK_080"
```

## Common error patterns and fixes

### Error: Variable not found in DDI

**Cause**: Wrong era variable name applied via `[VAR]` default

**Fix**: Add explicit mappings for each era:
```
# Before (wrong)
variableStart: [SMKDVSTY]
databaseStart: cchs2009_2010_m, cchs2015_2016_m

# After (correct)
variableStart: cchs2009_2010_m::SMKDSTY, [SMKDVSTY]
databaseStart: cchs2009_2010_m, cchs2015_2016_m
```

### Error: PUMF variable used for Master database

**Cause**: Grouped variable (SMKG...) referenced for Master file

**Fix**: Use ungrouped variable for Master:
```
# Before (wrong)
variableStart: [SMKG203]
databaseStart: cchs2007_2008_m, cchs2007_2008_p

# After (correct)
variableStart: cchs2007_2008_m::SMK_203, [SMKG203]
databaseStart: cchs2007_2008_m, cchs2007_2008_p
```

### Error: Double brackets in variableStart

**Cause**: Typo - `[[VAR1, VAR2]]` instead of `DerivedVar::[VAR1, VAR2]`

**Fix**: Use correct derived variable notation:
```
# Before (wrong)
variableStart: [[SMKG005, SMKG040]]

# After (correct)
variableStart: DerivedVar::[SMKG005, SMKG040]
```

## Authoring workflow

### Step 1: Identify all databases and eras

List target databases and group by naming era:
- Pre-2007: cchs2001, cchs2003, cchs2005
- 2007-2014: cchs2007_2008 through cchs2013_2014
- Post-2014: cchs2015_2016 through cchs2023

### Step 2: Look up correct variable names per era

Use DDI or source-lookups.R:
```r
source("R/source-lookups.R")
get_variables_for_database("cchs2015_2016_m") |> grep("SMK", x = _, value = TRUE)
```

### Step 3: Build explicit mappings

For each era, create explicit `db::VAR` mappings:
```r
mappings <- list(
  cchs2001_m = "SMKA_203",
  cchs2003_m = "SMKC_203",
  cchs2005_m = "SMKE_203",
  cchs2007_2008_m = "SMK_203",
  # ... 2007-2014 all use SMK_203
  cchs2015_2016_m = "SMK_040",
  # ... 2015+ all use SMK_040
)
```

### Step 4: Determine if `[VAR]` default is safe

A `[VAR]` default is safe ONLY when:
- All remaining unmapped databases use the same variable name
- The variable exists in all those databases
- **CRITICAL**: The variable name didn't change across the 2015 redesign boundary

**Rule of thumb**: If your databaseStart spans both 2007-2014 AND 2015+ cycles, you almost certainly need explicit mappings for the 2015+ cycles. The `[VAR]` fallback will fail silently at runtime.

### Step 5: Create variable_details rows

Group rows by:
1. Common databaseStart + variableStart combinations
2. Processing type (direct recoding vs derived function)

### Step 6: Create variables.csv entry

Aggregate:
- databaseStart: Union of all variable_details databaseStart
- variableStart: All unique explicit mappings + multi-variable inputs

### Step 7: Validate (MANDATORY)

**This step is not optional.** Before merging worksheets to inst/extdata, you MUST validate:

```r
source("R/validate-all-source-references.R")
result <- validate_all_source_references("path/to/variable_details.csv")
if (length(result$invalid_refs) > 0) {
  print(result$invalid_refs)
  stop("Cannot proceed with invalid source references")
}
```

**What this catches:**
- `[VAR]` defaults that don't exist in 2015+ cycles (e.g., `[SMK_09C]` when 2015+ uses `SMK_090`)
- Typos in variable names
- PUMF variables used for Master databases (or vice versa)
- Missing explicit mappings for renamed variables

### Step 8: Cross-check variables.csv against variable_details.csv

Ensure the summary in variables.csv matches the detail rows:

```r
# Check that all explicit mappings in variable_details appear in variables.csv
# Check that databaseStart in variables.csv covers all databases in variable_details
source("R/csv-workflow.R")
csv_validate("path/to/variables.csv", "path/to/variable_details.csv")
```

## Preventing the 2015 rename error

The most common error is using `[VAR]` for a variable that was renamed in 2015. To prevent this:

1. **Check the era mapping tables above** before using `[VAR]` notation
2. **Run validation** against DDI before merging to inst/extdata
3. **If databaseStart includes both 2007-2014 AND 2015+ cycles**, add explicit mappings for 2015+

Example fix for SMK_09C:
```
# WRONG - [SMK_09C] doesn't exist in 2015+
variableStart: cchs2003_m::SMKC_09C, cchs2005_m::SMKE_09C, [SMK_09C]

# CORRECT - explicit mappings for 2015+ where it's called SMK_090
variableStart: cchs2003_m::SMKC_09C, cchs2005_m::SMKE_09C, cchs2015_2016_m::SMK_090, cchs2017_2018_m::SMK_090, cchs2019_2020_m::SMK_090, cchs2021_m::SMK_090, [SMK_09C]
```

## Related documentation

- [csv-conventions.md](csv-conventions.md) — structural conventions: cycle ordering, era block collapsing, row sort order, dummyVariable naming, union rule
- [harmonization-workflow.md](harmonization-workflow.md) — L0-L6 staged workflow
- [derived-variable-functions.md](derived-variable-functions.md) — Func:: row authoring and PUMF/Master splitting for derived variables
