---
name: cchsflow-validation
description: Validate cchsflow worksheets for CSV formatting, source references, and cross-file consistency. Use before merging PRs that modify variables.csv or variable_details.csv, after authoring worksheet rows (L5 stage), or when GHA checks fail and you need local diagnostics.
allowed-tools: Bash(Rscript:*), Bash(R:*), Bash(git:*), Read, Glob, Grep
---

# cchsflow worksheet validation

Run programmatic validation checks on cchsflow worksheets. This skill runs the same checks as GHA but locally, with additional cross-file consistency checks.

## Usage

```
/cchsflow-validation
/cchsflow-validation path/to/variables.csv path/to/variable_details.csv
```

When invoked without arguments, validates the production worksheets at `inst/extdata/`.

### Scoped validation

For development workflow, scope validation to in-scope variables instead of the full file:

```bash
# By subject (matches the subject column in variables.csv)
Rscript exec/check-worksheets.R --subject "Ethnicity,Language,Migration"
Rscript exec/fix-worksheets.R --subject "Smoking"

# By variable name
Rscript exec/check-worksheets.R --variables "SDCGCGT,SDCFIMM,SDCGLNG"

# Combined (union of both filters)
Rscript exec/fix-worksheets.R --subject "Ethnicity" --variables "COPD_Emph_der"
```

Scoped mode extracts matching rows to temp files, runs checks/fixes on those, then (for fix) merges corrected rows back into the full worksheets. This reduces check time from ~2s (full file) to ~0.2s (scoped).

**When to use scoped vs full:**
- **Scoped**: During development, PR review, iterative worksheet authoring
- **Full**: CI/GHA, pre-merge final check, after bulk edits

The R functions `scope_worksheets()` and `parse_scope_args()` in `R/scope-worksheets.R` can also be called programmatically.

## Which skill to use

| Task | Skill |
|------|-------|
| **Authoring** new variables or editing worksheets | `cchsflow-worksheets` |
| **Validating** worksheets for formatting and consistency | `cchsflow-validation` (this skill) |
| **Reviewing** a PR or self-reviewing harmonisation work | `cchsflow-review` |
| **Writing** derived variable R functions | `cchsflow-derive` |

Typical flow: worksheets → validation → review (for PRs) or worksheets → validation (for self-review).

## L-stage mapping

| Check | L-stage | When to run |
|-------|---------|-------------|
| 1: CSV formatting | L5 | After authoring, before committing |
| 2: Source references | L3 | After variableStart authoring |
| 3: Cross-file consistency | L5 | After adding variables to either file |
| 4: databaseStart coverage | L5 | After modifying databaseStart fields |
| 5: R CMD check | L6 | Before merge, after R/ file changes |
| 6: Pre-2007 explicit mappings | L3 | After adding pre-2007 databases |
| 7: DerivedVar mixed _p/_m | L5 | After writing DerivedVar rows |
| 8: Trailing empty columns | L5 | After any Excel-based editing |

## Validation checks

### Check 1: CSV formatting

Run the fix-worksheets script to check (and optionally fix) formatting:

```r
Rscript exec/fix-worksheets.R
```

This checks for:
- Excessive quoting (all fields quoted when not needed)
- Wrong column order (compared against YAML schemas)
- Empty trailing columns
- CRLF line endings (should be LF only)
- Unsorted rows (variables.csv sorted by `variable` column)

**Schema files:**
- `inst/metadata/schemas/core/variables.yaml`
- `inst/metadata/schemas/core/variable_details.yaml`

If `fix-worksheets.R` fails due to package load errors, use the fallback:

```r
Rscript -e "
library(readr)
vars <- read.csv('inst/extdata/variables.csv', stringsAsFactors = FALSE, check.names = FALSE)
write_csv(vars, 'inst/extdata/variables.csv', na = '', quote = 'needed', escape = 'double', eol = '\n')
details <- read.csv('inst/extdata/variable_details.csv', stringsAsFactors = FALSE, check.names = FALSE)
write_csv(details, 'inst/extdata/variable_details.csv', na = '', quote = 'needed', escape = 'double', eol = '\n')
"
```

### Check 2: Source reference validation

If `R/validate-all-source-references.R` exists, validate that all variableStart references point to real variables in the DDI:

```r
Rscript -e "
source('R/validate-all-source-references.R')
result <- validate_all_source_references('inst/extdata/variable_details.csv')
print_all_validation_result(result)
"
```

This catches:
- `[VAR]` defaults that don't exist in 2015+ cycles
- Typos in variable names
- PUMF variables used for master databases (or vice versa)
- Missing explicit mappings for renamed variables

### Check 3: Cross-file consistency

Use R to check that variables.csv and variable_details.csv are internally consistent:

```r
Rscript -e "
vars <- read.csv('inst/extdata/variables.csv', stringsAsFactors = FALSE, check.names = FALSE)
details <- read.csv('inst/extdata/variable_details.csv', stringsAsFactors = FALSE, check.names = FALSE)

# Variables in details but not in vars
detail_vars <- unique(details\$variable)
var_vars <- unique(vars\$variable)
missing_in_vars <- setdiff(detail_vars, var_vars)
missing_in_details <- setdiff(var_vars, detail_vars)

if (length(missing_in_vars) > 0) {
  cat('ERROR: Variables in variable_details.csv but not in variables.csv:\n')
  cat(paste(' -', missing_in_vars), sep = '\n')
}
if (length(missing_in_details) > 0) {
  cat('WARNING: Variables in variables.csv but not in variable_details.csv:\n')
  cat(paste(' -', missing_in_details), sep = '\n')
}
if (length(missing_in_vars) == 0 && length(missing_in_details) == 0) {
  cat('OK: All variables present in both files.\n')
}
"
```

### Check 4: databaseStart coverage

For each variable, verify that the `databaseStart` in variables.csv matches the union of all `databaseStart` entries in variable_details.csv:

```r
Rscript -e "
vars <- read.csv('inst/extdata/variables.csv', stringsAsFactors = FALSE, check.names = FALSE)
details <- read.csv('inst/extdata/variable_details.csv', stringsAsFactors = FALSE, check.names = FALSE)

parse_dbs <- function(x) {
  trimws(unlist(strsplit(x, ',')))
}

errors <- character()
for (v in unique(vars\$variable)) {
  vars_dbs <- sort(parse_dbs(vars\$databaseStart[vars\$variable == v][1]))
  details_rows <- details[details\$variable == v, ]
  details_dbs <- sort(unique(unlist(lapply(details_rows\$databaseStart, parse_dbs))))

  in_vars_not_details <- setdiff(vars_dbs, details_dbs)
  in_details_not_vars <- setdiff(details_dbs, vars_dbs)

  if (length(in_vars_not_details) > 0) {
    errors <- c(errors, paste0(v, ': in variables.csv but not variable_details.csv: ',
                               paste(in_vars_not_details, collapse = ', ')))
  }
  if (length(in_details_not_vars) > 0) {
    errors <- c(errors, paste0(v, ': in variable_details.csv but not variables.csv: ',
                               paste(in_details_not_vars, collapse = ', ')))
  }
}

if (length(errors) > 0) {
  cat('databaseStart mismatches:\n')
  cat(paste(' -', errors), sep = '\n')
} else {
  cat('OK: All databaseStart fields are consistent.\n')
}
"
```

### Check 5: R CMD check (package integrity)

Run a lightweight R CMD check to catch package-level issues such as undeclared dependencies, invalid `library()` calls in R/ files, missing NAMESPACE exports, and broken function references:

```bash
Rscript -e "devtools::check(document = FALSE, args = '--no-tests --no-examples --no-vignettes --no-manual')" 2>&1 | tail -30
```

This catches:
- `library()` calls in R/ files (must use DESCRIPTION Depends/Imports instead)
- Missing package dependencies (e.g., `here` used but not in DESCRIPTION)
- Undefined exports in NAMESPACE
- `source()` calls that fail in package context
- Syntax errors in R files

**Quick alternative** — if full R CMD check is too slow, test that the package loads:

```r
Rscript -e "devtools::load_all('.'); cat('Package loads OK\n')"
```

If `devtools::load_all()` fails, the GHA will also fail when it tries to install the package.

### Check 6: Pre-2007 explicit mapping coverage

For any variable where `databaseStart` includes pre-2007 databases (`cchs2001_m`, `cchs2001_p`, `cchs2003_m`, `cchs2003_p`, `cchs2005_m`, `cchs2005_p`), verify that `variableStart` contains explicit `db::VAR` entries for those cycles rather than relying on `[VAR]` defaults.

The `[VAR]` default applies the base variable name to all unlisted databases. For pre-2007 cycles, the correct name requires a cycle letter in position 4 (A=2001, C=2003, E=2005). A `[VAR]` default for these cycles will silently look up the wrong variable name.

```r
Rscript -e "
vd <- read.csv('inst/extdata/variable_details.csv', stringsAsFactors = FALSE)
pre2007 <- c('cchs2001_m', 'cchs2001_p', 'cchs2003_m', 'cchs2003_p',
             'cchs2005_m', 'cchs2005_p')

issues <- character()
for (v in unique(vd\$variable)) {
  rows <- vd[vd\$variable == v, ]
  for (i in seq_len(nrow(rows))) {
    dbs <- trimws(strsplit(rows\$databaseStart[i], ',')[[1]])
    vs  <- rows\$variableStart[i]
    pre <- dbs[dbs %in% pre2007]
    if (length(pre) == 0) next
    # Check each pre-2007 db has an explicit db::VAR mapping
    for (db in pre) {
      if (!grepl(paste0(db, '::'), vs, fixed = TRUE)) {
        issues <- c(issues, paste0(v, ': ', db, ' has no explicit mapping in variableStart'))
      }
    }
  }
}
if (length(issues) > 0) {
  cat('Pre-2007 mapping gaps (will use [VAR] default — likely WRONG name):\n')
  cat(paste(' -', issues), sep = '\n')
} else {
  cat('OK: All pre-2007 databases have explicit variableStart mappings.\n')
}
"
```

Pre-2007 mapping gaps are **P1** errors — the variable exists in those cycles but the wrong source variable is read at runtime.

### Check 7: DerivedVar mixed _p/_m row detection

DerivedVar rows must not mix `_p` (PUMF) and `_m` (Master) databases in a single row when those database types use different feeder variables. If a single DerivedVar row's `databaseStart` contains both `_p` and `_m` entries, `rec_with_table()` will apply the same feeder variable set to all databases in that row — silently producing wrong results when PUMF and Master use different age, sex, or other input variables.

```r
Rscript -e "
vd <- read.csv('inst/extdata/variable_details.csv', stringsAsFactors = FALSE)

mixed <- data.frame(variable = character(), row = integer(),
                    n_p = integer(), n_m = integer(), stringsAsFactors = FALSE)
derived_rows <- vd[grepl('DerivedVar::', vd\$variableStart), ]
for (i in seq_len(nrow(derived_rows))) {
  dbs <- trimws(strsplit(derived_rows\$databaseStart[i], ',')[[1]])
  has_p <- any(grepl('_p$', dbs))
  has_m <- any(grepl('_m$', dbs))
  if (has_p && has_m) {
    mixed <- rbind(mixed, data.frame(
      variable = derived_rows\$variable[i],
      row = which(vd\$variable == derived_rows\$variable[i] &
                    vd\$variableStart == derived_rows\$variableStart[i])[1],
      n_p = sum(grepl('_p$', dbs)),
      n_m = sum(grepl('_m$', dbs)),
      stringsAsFactors = FALSE
    ))
  }
}
if (nrow(mixed) > 0) {
  cat('DerivedVar rows mixing _p and _m databases:\n')
  for (i in seq_len(nrow(mixed))) {
    cat(sprintf('  %-30s (row ~%d): %d _p, %d _m — inspect feeder sets\n',
                mixed\$variable[i], mixed\$row[i], mixed\$n_p[i], mixed\$n_m[i]))
  }
  cat('\nFor each flagged variable: compare resolve_dependencies(variable, databases=\"cchs2015_2016_p\")\n')
  cat('vs resolve_dependencies(variable, databases=\"cchs2015_2016_m\") — if feeders differ, split the rows.\n')
} else {
  cat('OK: No DerivedVar rows mix _p and _m databases.\n')
}
"
```

A mixed row is **always suspect**. It is a **P1** error if the `_p` and `_m` feeder sets differ (use `resolve_dependencies()` with a `databases` filter to confirm). It may be acceptable if feeders are identical across both database types, but this should be verified explicitly.

### Check 8: Trailing empty columns

Check for trailing empty columns added by Excel editing (a recurring issue across v3 PRs):

```r
Rscript -e "
vd <- read.csv('inst/extdata/variable_details.csv', stringsAsFactors = FALSE, check.names = FALSE)
cat('variable_details.csv columns:', ncol(vd), '\n')
empty <- which(names(vd) == '' | is.na(names(vd)))
if (length(empty) > 0) cat('WARNING: Empty column names at positions:', empty, '\n')
else cat('OK: No trailing empty columns\n')

vars <- read.csv('inst/extdata/variables.csv', stringsAsFactors = FALSE, check.names = FALSE)
cat('variables.csv columns:', ncol(vars), '\n')
empty2 <- which(names(vars) == '' | is.na(names(vars)))
if (length(empty2) > 0) cat('WARNING: Empty column names at positions:', empty2, '\n')
else cat('OK: No trailing empty columns\n')
"
```

Expected column counts: variables.csv = 10, variable_details.csv = 16. (Defined in YAML schemas at `inst/metadata/schemas/core/`.)

## Interpreting results

| Check | Pass | Severity | Fail action |
|-------|------|----------|------------|
| 1: CSV formatting | No output / clean exit | P2 | Run `Rscript exec/fix-worksheets.R` to auto-fix, then commit |
| 2: Source references | No invalid refs | P0 | Fix variableStart mappings per era rules |
| 3: Cross-file consistency | All variables in both files | P1 | Add missing entries to the appropriate file |
| 4: databaseStart coverage | No mismatches | P1 | Align databaseStart between files |
| 5: R CMD check | 0 errors, 0 warnings | P0 | Fix R/ files: remove `library()` calls, declare deps in DESCRIPTION |
| 6: Pre-2007 explicit mappings | No gaps | P1 | Add explicit `db::VAR` entries for pre-2007 cycles |
| 7: DerivedVar mixed _p/_m | No mixed rows | P1 | Split rows by database type; verify feeders with `resolve_dependencies()` |
| 8: Trailing empty columns | Expected column counts | P2 | Trim to real columns using R `write.csv()` |

## When to run

- **Before committing** worksheet changes (L5 stage)
- **Before merging** PRs that modify worksheets
- **When GHA "Check CSV Formatting" fails** — run locally for detailed diagnostics
- **After bulk edits** (adding master cycles to many variables)
- **When R/ files are modified** — run R CMD check to catch package-level issues
