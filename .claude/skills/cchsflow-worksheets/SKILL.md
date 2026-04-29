---
name: cchsflow-worksheets
description: Author and edit CCHS harmonization worksheets (variables.csv, variable_details.csv). Use when adding variables, mapping source variables across cycles, following the L0-L6 harmonization workflow, or consulting era-specific naming conventions.
---

# cchsflow worksheet authoring

This skill provides guidance for authoring and editing cchsflow harmonization worksheets. The two primary worksheets are:

- `inst/extdata/variables.csv` — variable registry (metadata, database coverage)
- `inst/extdata/variable_details.csv` — recoding/transformation rules

## Variable lookup: cchs-metadata MCP

**Always use the cchs-metadata MCP server as the primary tool for looking up CCHS variable metadata** during worksheet authoring. It provides the most complete, cross-referenced metadata (16,000+ variables, 251 datasets) and is faster and more reliable than searching raw files.

Key tools for authoring:
- `mcp__cchs-metadata__get_variable_history(variable_name)` — check which cycles/datasets contain a variable (essential for `databaseStart` authoring)
- `mcp__cchs-metadata__search_variables(query)` — find variables by name or label (essential for identifying era renames)
- `mcp__cchs-metadata__compare_master_pumf(variable_name, cycle)` — check whether PUMF and Master differ (essential for deciding row-splitting)
- `mcp__cchs-metadata__get_value_codes(variable_name)` — get response categories (essential for `recStart`/`recEnd` authoring)
- `mcp__cchs-metadata__suggest_cchsflow_row(variable_name)` — draft a harmonisation row
- `mcp__cchs-metadata__get_source_conflicts(variable_name, dataset_id)` — find cross-source label disagreements (useful for catching metadata inconsistencies before authoring)

If the MCP is not available:
1. Check that `../cchsflow-docs/mcp-server/server.py` exists (may need to restore from that repo's main branch)
2. Verify MCP configuration in `~/.claude.json` includes the `cchs-metadata` server
3. **Standalone CLI fallback**: `python3 ../cchsflow-docs/mcp-server/server.py --cli search "SMK_005"` (runs without Claude Code)
4. **File-based fallback**: Read DDI YAML files directly from `../cchsflow-docs/ddi/` or CSVs from `../cchsflow-docs/data/`

The MCP server (v0.3.0+) lives in `../cchsflow-docs/mcp-server/` and is also available as a [GitHub release](https://github.com/Big-Life-Lab/cchsflow-docs/releases).

## Key references

Detailed documentation is in the `docs/` subdirectory:

- [harmonization-workflow.md](docs/harmonization-workflow.md) — the L0-L6 staged workflow for harmonizing CCHS variables, from documentation assessment through integration testing
- [variableStart-databaseStart-authoring.md](docs/variableStart-databaseStart-authoring.md) — technical rules for coordinating `variableStart` and `databaseStart` fields, including era-specific mappings and the dangerous `[VAR]` default pattern
- [pumf-master-harmonization.md](docs/pumf-master-harmonization.md) — patterns for splitting worksheet rows when PUMF and Master databases require different recoding logic (midpoint imputation vs continuous pass-through)
- [derived-variable-functions.md](docs/derived-variable-functions.md) — how to write R functions for `Func::` rows: 3-step architecture, semantic parameter naming, `derive_passthrough()`, feeder alignment, and `clean_variables()` worksheet-name mapping
- [csv-conventions.md](docs/csv-conventions.md) — structural conventions for both CSVs: canonical cycle ordering, single-year parent-child rules, era block collapsing, row sort order within blocks, dummyVariable naming, alphabetical variable ordering, label alignment, and the union rule

## Quick reference

### CCHS variable naming eras

| Era | Years | Pattern | Example |
|-----|-------|---------|---------|
| Pre-2007 | 2001-2005 | Cycle letter in 4th position | `SMKA_203` (2001), `SMKC_203` (2003), `SMKE_203` (2005) |
| 2007-2014 | 2007-2014 | Standard naming | `SMK_203` |
| Post-2014 | 2015+ | 3-digit increments | `SMK_040` |

### Database suffixes

| Suffix | Meaning | Notes |
|--------|---------|-------|
| `_p` | PUMF (Public Use Microdata File) | Grouped/derived variables |
| `_m` | Master survey file | Ungrouped source variables |
| `_s` | Share file (deprecated) | Replace with `_m` |
| `_i` | ICES-linked (deprecated) | Replace with `_m` |

### PUMF vs Master row splitting

When PUMF has grouped categorical and Master has true continuous source variables, rows must be split by database type. See [pumf-master-harmonization.md](docs/pumf-master-harmonization.md) for the full pattern.

**Quick test**: If `variableStart` references both a categorical variable (e.g., SMK_06A) and a continuous companion (e.g., SMK_06C) for the same harmonized variable, you likely need the split pattern.

### The dangerous default pattern

If `databaseStart` spans both 2007-2014 and 2015+ cycles, a `[VAR]` default will apply the 2007-2014 name to 2015+ databases where the variable may have been renamed. Always add explicit `db::VAR` mappings for 2015+ cycles.

### Structural conventions (quick reference)

See [csv-conventions.md](docs/csv-conventions.md) for full detail. Key rules:

**Canonical cycle order** — all `_p` chronologically, then all `_m` chronologically; single-year children immediately follow their two-year parent:
```
cchs2001_p … cchs2023_p | cchs2001_m … cchs2009_2010_m, cchs2009_m, cchs2010_m … cchs2023_m
```

**Single-year child rules** — whenever a parent cycle is present, its children must also be present:

| Parent | Children |
|---|---|
| `cchs2009_2010_p` | `cchs2010_p` |
| `cchs2011_2012_p` | `cchs2012_p` |
| `cchs2013_2014_p` | `cchs2014_p` |
| `cchs2009_2010_m` | `cchs2009_m`, `cchs2010_m` |
| `cchs2011_2012_m` | `cchs2012_m` |
| `cchs2013_2014_m` | `cchs2014_m` |

Children inherit the same source variable as their parent. Child without parent → remove the child.

**Era block collapsing** — merge two blocks into one when they share identical recoding and differ only in which cycles they list (use `[VAR]` pass-through). Do NOT collapse when source variable name changed across eras or recoding differs.

**Row sort order within each era block:**

Categorical / continuous blocks:
1. Numerical category / copy rows — ascending `recStart`
2. `NA::a` rows
3. `NA::b` rows (non-else)
4. `NA::b` else row

Derived variable (`Func::`) blocks — **no `NA::b else` row ever**:
- Continuous output: exactly 3 rows — Func:: + NA::a + NA::b
- Categorical output: Func:: row, then N category rows (ascending recEnd), then NA::a + NA::b

Func:: row always has `typeStart=N/A`, `recStart=N/A`, `catLabel=N/A`. Collapse two Func:: blocks into one when they call the same function with the same feeder list across consecutive eras.

**dummyVariable naming** — `{variable}_cat{N}_{x}` where N = `numValidCat`, x = `1`…`N`, then `NAa`, `NAb`. Continuous variables use `N/A`.

**Alphabetical ordering** — both CSVs must be sorted by `variable` column. Insert new variables at the correct alphabetical position.

**Label alignment:**
- `variables.csv label` ↔ `variable_details.csv variableStartShortLabel`
- `variables.csv labelLong` ↔ `variable_details.csv variableStartLabel`

**Union rule** — `variables.csv databaseStart` = union of all era block `databaseStart` values; `variables.csv variableStart` = union of all explicit tokens and `[VAR]`/`DerivedVar::` patterns across all era blocks.

**NA row label conventions** (fixed values — no variation):

| Row type | catLabel | catLabelLong | catStartLabel |
|---|---|---|---|
| `NA::a` | `not applicable` | `not applicable` | `not applicable` |
| `NA::b` else | `missing` | `missing` | `else` |
| `NA::b` non-else, codes | `missing` | `missing` | `don't know (X7); refusal (X8); not stated (X9)` |
| `NA::b` non-else, `N/A` recStart | `missing` | `missing` | `missing` |

For code-range rows, derive X7/X8/X9 from `recStart` lower bound (e.g., `[97,99]` → `don't know (97); refusal (98); not stated (99)`). Category value rows: `Yes`/`No` always capitalised.

---

### Writing CSVs — quoting rules

**CRITICAL**: Never use `write.csv()` for worksheets — it quotes all fields, which fails the worksheet checker. Use one of:

```r
# Option 1: readr (preferred for scripts)
readr::write_csv(df, path, na = "", quote = "needed", escape = "double", eol = "\n")

# Option 2: fix_worksheet() after any write
devtools::load_all(quiet = TRUE)
fix_worksheet(path, "variable_details")  # strips unnecessary quotes
```

### Rebuilding RData after worksheet changes

Whenever CSVs change, rebuild the RData files that `rec_with_table()` uses at runtime:

```r
vd <- read.csv("inst/extdata/variable_details.csv", stringsAsFactors = FALSE)
variable_details <- vd[, c("variable", "dummyVariable", "typeEnd", "databaseStart",
                            "variableStart", "typeStart", "recEnd", "numValidCat",
                            "catLabel", "catLabelLong", "units", "recStart",
                            "catStartLabel", "variableStartShortLabel",
                            "variableStartLabel", "notes")]
save(variable_details, file = "data/variable_details.RData")

v <- read.csv("inst/extdata/variables.csv", stringsAsFactors = FALSE)
variables <- v[, c("variable", "label", "labelLong", "section", "subject",
                    "variableType", "units", "databaseStart", "variableStart",
                    "description")]
save(variables, file = "data/variables.RData")
```

The RData files contain the same columns as the CSVs: 16 columns for variable_details, 10 for variables. Match the column lists above when rebuilding.

### CSV validation before committing

```bash
# Full file (CI/pre-merge)
Rscript exec/fix-worksheets.R

# Scoped to your working variables (faster, recommended during development)
Rscript exec/fix-worksheets.R --subject "Ethnicity,Language,Migration"
Rscript exec/check-worksheets.R --variables "SDCGCGT,SDCFIMM"
```

This checks and fixes: excessive quoting, column order, empty trailing columns, CRLF line endings, unsorted rows. Scoped mode runs only on matching rows (~0.2s vs ~2s for the full file).
