# L6 implementation validation

**This is the highest-priority check.** Run `rec_with_table()` against actual PUMF data. This is not just a pass/fail test — the output is an analytical tool. By examining prevalence and distributions across cycles and categories, reviewers can identify harmonization problems that worksheet checks alone cannot catch, such as a sudden step change in prevalence at an era boundary (e.g., 2014 → 2015) that signals a naming mismatch or category recode error.

## Multi-era recode validation

For variables with multiple recode blocks (identified in Check 2b), standard L6 prevalence checks are insufficient — `rec_with_table()` may silently apply the wrong block or blend blocks without error. For these variables, perform era-specific output validation:

1. **Identify one representative PUMF cycle per block** — e.g., for SMK_09A_cont: `cchs2001_p` (Block 1 era), `cchs2007_2008_p` (Block 3 era)
2. **Run `rec_with_table()` for each representative cycle**
3. **Verify the recEnd values match the expected midpoints for that era** — not just that they are non-missing

For continuous variables, check a known respondent's output value against the expected midpoint for their source category. If the era boundary is at 2003 (different category boundaries in 2001 vs 2003+), a respondent with source code 3 should produce recEnd=4 in 2001 but recEnd=2.5 in 2003+. If both cycles produce the same value, the wrong block is being applied to one of them.

Flag any era boundary where observed output values do not match expected midpoints as **P0**.

## Scope and limitations

**PUMF data only.** L6 can currently test only `_p` databases. The `data/` directory contains PUMF RData files (`cchs2001_p.RData` through `cchs2017_2018_p.RData`). Master (`_m`) data is in a secure environment where LLMs cannot run.

For master-only changes (e.g., a PR that only adds `_m` cycles), L6 cannot validate at runtime. In this case:
- Rely on L3-L5 worksheet checks (especially era boundary and naming checks)
- Generate the integration test R script anyway and save it to the CEP — the user or a colleague can run it in the secure environment
- Note the limitation explicitly in the review output

**Future:** Mock data from the `mockdata` repo will enable L6 testing for all database types.

## Data locations

PUMF RData files are in `data/`:
- `cchs2001_p.RData` through `cchs2017_2018_p.RData`

Each file loads a data frame named after the cycle (e.g., `cchs2001_p`).

## Integration test script

Generate and run a fully executable R script for the in-scope variables — no placeholders. Extract the actual variable names and cycle list from the worksheets. Save the script to the CEP directory so reviewers can re-run it.

The script should:
1. Read `variable_details.csv` to extract the `_p` databases from `databaseStart` for each in-scope variable
2. Load cchsflow from the PR branch (use `devtools::load_all()` if R functions were modified, otherwise `library(cchsflow)`)
3. For each cycle, run `rec_with_table()` and collect results
4. Print cross-cycle prevalence summary
5. Save results CSV

Pattern based on CEP-006:

```r
# devtools::load_all()  # Use if PR modifies R/ functions
library(cchsflow)
library(dplyr)

# Load worksheet from the branch under review
variable_details <- read.csv("inst/extdata/variable_details.csv",
                             stringsAsFactors = FALSE)

# Extract PUMF cycles from databaseStart for the in-scope variables
# (agent: replace with actual variable names and cycles from the worksheet)
variables_to_test <- c("FVCDFRU", "FVCDSAL", "FVCDPOT")
cycles <- c("cchs2001_p", "cchs2003_p", "cchs2005_p",
            "cchs2007_2008_p", "cchs2009_2010_p", "cchs2011_2012_p",
            "cchs2013_2014_p", "cchs2015_2016_p", "cchs2017_2018_p")

results <- data.frame()

for (cycle in cycles) {
  rdata_file <- file.path("data", paste0(cycle, ".RData"))
  if (!file.exists(rdata_file)) {
    cat("SKIP", cycle, "- file not found\n")
    next
  }

  load(rdata_file)
  df <- get(cycle)

  result <- tryCatch({
    rec_with_table(
      data = df,
      variables = variables_to_test,
      database_name = cycle,
      variable_details = variable_details,
      log = FALSE
    )
  }, error = function(e) {
    cat("ERROR in", cycle, ":", e$message, "\n")
    NULL
  })

  if (!is.null(result)) {
    n <- nrow(result)
    for (v in setdiff(names(result), "ADM_RNO")) {
      valid <- sum(!is.na(result[[v]]))
      cat(cycle, v, ": valid =", valid, "/", n,
          "(", round(100 * valid / n, 1), "%)\n")

      # Category distribution (for categorical variables)
      freq <- table(result[[v]], useNA = "ifany")
      print(freq)

      results <- rbind(results, data.frame(
        cycle = cycle, variable = v,
        n = n, valid = valid,
        valid_pct = round(100 * valid / n, 1),
        stringsAsFactors = FALSE
      ))
    }
  }

  rm(list = cycle)  # free memory
}

# Cross-cycle prevalence summary
cat("\n=== CROSS-CYCLE SUMMARY ===\n")
for (v in unique(results$variable)) {
  cat("\n", v, ":\n")
  sub <- results[results$variable == v, ]
  print(sub[, c("cycle", "n", "valid", "valid_pct")], row.names = FALSE)
}

# Save results
write.csv(results, "ceps/cep-NNN-domain/vars-pumf-integration-test.csv",
          row.names = FALSE)
```

## Cross-cycle prevalence QMD

After generating the integration test CSV, create a Quarto document (`.qmd`) that visualises the cross-cycle results. This is a standard CEP artifact — visual inspection of prevalence trends is the most effective way to detect era boundary problems.

The QMD should include:
1. **Cross-cycle valid % line plot** for each key variable (or a representative subset), with cycles on the x-axis and valid % on the y-axis. Add vertical reference lines at era boundaries (2007, 2015).
2. **Category distribution plot** for categorical derived variables (e.g., stacked bar chart of diet_score_cat3 across cycles).
3. **Annotations** for known data patterns — e.g., optional content cycles where low prevalence is expected, documented in the R function's roxygen or CCHS documentation.
4. **Brief narrative** interpreting the plots: are transitions clean? Any unexpected step changes?

Use base R graphics (`plot()`, `barplot()`) to avoid extra dependencies. The QMD should be self-contained — load the results CSV, not rerun the integration test.

Pattern:

```yaml
---
title: "CEP-NNN: Cross-cycle prevalence"
format:
  html:
    toc: true
    code-fold: true
---
```

```r
results <- read.csv("domain-pumf-integration-test.csv")

# Extract year from cycle name for x-axis
results$year <- as.numeric(gsub("cchs(\\d{4}).*", "\\1", results$cycle))

# Plot valid % by cycle for a key variable
var_data <- results[results$variable == "KEY_VAR", ]
plot(var_data$year, var_data$valid_pct, type = "b", pch = 19,
     xlab = "CCHS cycle", ylab = "Valid %",
     main = "KEY_VAR: cross-cycle prevalence")
abline(v = c(2007, 2015), lty = 2, col = "grey50")
```

Save the QMD to the CEP directory alongside the other artifacts:

```
ceps/cep-NNN-<domain>/
  cep-NNN-<domain>.qmd              # Cross-cycle prevalence plots
  PR-<number>-review-summary.md
  integration-test-<vars>.R
  <vars>-pumf-integration-test.csv
```

## Cross-cycle prevalence analysis

The cross-cycle summary is the most important output. Review the `valid_pct` column for each variable across cycles and look for:

1. **Step changes at era boundaries** — a sudden jump or drop in prevalence between 2005 → 2007 (pre-2007 to standard era) or 2014 → 2015 (standard to post-2014 era) suggests a naming mismatch or incorrect `[VAR]` default
2. **Unexpected zeros** — a cycle showing 0% valid when the variable should be available indicates a wrong source variable name or missing `db::VAR` mapping
3. **Exposure distribution shifts** — the key harmonization question is whether typical exposures remain stable across cycles. For continuous variables (e.g., daily fruit/veg consumption), check whether the proportion at clinically meaningful thresholds (e.g., 0 servings, >5 servings/day) shifts at era boundaries. For categorical variables, compare `table()` output across cycles. A sudden distribution change at 2015 that doesn't track the gradual secular trend suggests a mapping or recoding error, not a real population change.
4. **Derived variable completeness** — if a derived variable has lower valid % than its inputs, the DV function may be dropping valid cases

**Optional content cycles:** Some CCHS modules are optional content in certain cycles — provinces opt in, so prevalence drops sharply. Before flagging low prevalence as an issue, check the R function's roxygen documentation and CCHS documentation for known optional content cycles. For example, FVC (fruit and vegetable consumption) was optional in 2005 and 2017-2018, producing ~56% and ~1% valid respectively — these are expected, not errors.

Cross-cycle trends require human judgement. The skill should produce a clear summary table and flag any obvious discontinuities, but the reviewer interprets the results using their domain knowledge. In future, threshold-based alerts may be added.

Example of a step change indicating a problem:
```
  cycle           valid_pct
  cchs2009_2010_p    34.1     <- normal
  cchs2011_2012_p    14.7     <- lower (optional content)
  cchs2013_2014_p    28.9     <- normal
  cchs2015_2016_p     0.0     <- PROBLEM: variable renamed but mapping missing
  cchs2017_2018_p     0.0     <- same problem
```

## Derived variable testing

If the in-scope variables include derived variables (functions in `R/`):

1. Identify the DV function (e.g., `diet_score_fun()` in `R/diet.R`)
2. Check that all input variables are available in the test cycles
3. Run `rec_with_table()` with the derived variable to verify the full pipeline
4. Compare the derived variable's valid % against its input variables — the DV should not have materially higher valid % than its least-available input
5. For categorical derived variables and key continuous inputs, examine the **exposure distribution** across cycles — not just valid counts. The central harmonization question is whether typical exposures (e.g., proportion with 0 fruit/veg, or >5 servings/day) remain stable across cycles. A sudden shift in the distribution at an era boundary signals a recoding or mapping error even when valid % is unchanged. Include these distributions in both the integration test output and the QMD visualisation

## What to report from L6

For each cycle tested:
- **N**: Total respondents
- **Valid count and %**: Non-NA values for each variable
- **Category distribution**: `table()` output for categorical variables
- **Errors**: Any `rec_with_table()` failures with error messages

Flag:
- **Step changes at era boundaries** (most important — signals naming/mapping errors)
- Cycles where valid % is 0 (variable may not exist despite being listed)
- Cycles where category distributions shift unexpectedly
- Derived variable failures or unexplained completeness gaps
