# Harmonization workflow (L0-L6)

This document describes the staged workflow for harmonizing CCHS variables in cchsflow.

## Overview

The L0-L6 workflow ensures systematic, validated harmonization of CCHS variables:

| Stage | Name | Purpose | Output |
|-------|------|---------|--------|
| L0 | Documentation assessment | Review all data sources | L0_documentation_assessment.md |
| L1 | Variable concordance | Map source variables across cycles | L1_variable_concordance.md |
| L2 | Semantic mapping | Define harmonization rules | L2_semantic_mapping.md |
| L3 | Worksheet authoring | Create CSV worksheets | variables.csv, variable_details.csv |
| L4 | DV specifications | Specify derived variable functions | L4_dv_specifications.md |
| L5 | Testing | Unit tests and validation | test-*.R |
| L6 | Integration | Merge to production, integration testing | QMD reports |

## L0: Documentation assessment

### Purpose

Identify and review all available documentation sources before writing worksheets. This prevents missed variables and ensures accurate era mappings.

### Required data sources

**Primary source (always use first):**

1. **cchs-metadata MCP server** — the unified metadata database with 16,000+ variables across 251 datasets
   - `get_variable_history(variable_name)` — confirm which cycles/datasets contain a variable
   - `search_variables(query)` — find variables by name or label, identify era renames
   - `get_value_codes(variable_name)` — get category codes and labels per cycle
   - `compare_master_pumf(variable_name, cycle)` — check PUMF vs Master differences
   - Cross-references PUMF RData, DDI XML, and ICES sources with full provenance
   - If not available, see troubleshooting in `.claude/skills/cchsflow-review/SKILL.md`

**Supplementary sources (use to fill gaps or cross-check):**

2. **DDI YAML files** (`cchsflow-docs/cchs-extracted/data-dictionary/`)
   - Raw extracted data dictionaries — coverage 2000-2001 through 2023
   - Use when MCP lacks coverage for a specific cycle (e.g., 2022-2023 if not yet ingested)

3. **cchs_available_variables_list.csv** (`development/`)
   - Quick reference for variable availability across cycles
   - Shows source variable names by era

4. **Existing PR worksheets** (if applicable)
   - Check branch-specific variables.csv and variable_details.csv
   - Note any existing errors or gaps

5. **cchsflow-docs variable listings** (when available)
   - Cross-reference for comprehensive coverage
   - Includes Ontario Linked file availability

### Multi-source reconciliation process

**CRITICAL**: Before authoring worksheets, reconcile all sources:

```
Step 1: List all variables from existing PR (if any)
Step 2: Cross-check against cchs_available_variables_list.csv
Step 3: Verify each variable exists in DDI for claimed cycles
Step 4: Check cchsflow-docs for Ontario Linked file availability
Step 5: Document any discrepancies in L0 assessment
```

### Ontario Linked file tracking

For Ontario-specific research (e.g., dementia studies), document:

- Which variables are available in Ontario Linked files
- Which cycles have Ontario-specific restrictions (e.g., 2003 HUI Ontario exclusion)
- Age restrictions for target populations (50+, 55+, 60+)

### L0 document template

```markdown
# L0: Documentation assessment - [Domain]

## Topic overview

**Domain**: [e.g., Hearing/Vision]
**Sub-topic**: [e.g., HUI hearing items]
**Scope**: [Brief description of variables in scope]

## Documentation sources reviewed

| Source | Location | Status |
|--------|----------|--------|
| DDI YAMLs | cchsflow-docs/cchs-extracted/data-dictionary/ | [Reviewed/Pending] |
| cchs_available_variables_list.csv | development/ | [Reviewed/Pending] |
| Existing PR worksheets | [branch]/inst/extdata/ | [Reviewed/N/A] |
| cchsflow-docs listings | [URL] | [Reviewed/Pending/N/A] |

## Multi-source reconciliation

### Variables from existing PR
[List variables found in PR worksheets]

### Variables from cchs_available_variables_list.csv
[List variables for this domain]

### Discrepancies identified
[Document any gaps or conflicts between sources]

## Provincial availability

### Ontario-specific restrictions

| Variable | Cycle | Issue |
|----------|-------|-------|
| [e.g., HUICGHER] | 2003 | [No Ontario data in PUMF] |

### Ontario Linked file availability

| Variable | Cycles available | Notes |
|----------|-----------------|-------|
| [var] | [cycles] | [any restrictions] |

## Variables in scope

[Comprehensive list with cycle coverage]

## Key decisions

[Document any decisions made during assessment]
```

## L1: Variable concordance

### Purpose

Map source variable names across all eras and identify naming patterns.

### How to build concordance

**Use the cchs-metadata MCP as the primary tool:**
- `get_variable_history(variable_name)` shows all datasets containing a variable — this directly reveals era renames (e.g., SMK_09C appearing in 2007-2014 datasets, SMK_090 in 2015+ datasets)
- `search_variables(query)` with partial name patterns finds related variables across naming eras
- `compare_master_pumf(variable_name, cycle)` reveals PUMF vs Master naming differences for each cycle

### Era patterns

| Era | Years | Naming pattern | Example |
|-----|-------|----------------|---------|
| Pre-2007 | 2001-2005 | Cycle letter in 4th position | HUIA_06 (2001), HUIC_06 (2003) |
| 2007-2014 | 2007-2014 | Standard naming | HUI_06 |
| Post-2014 | 2015+ | 3-digit increments | HUI_060 or module redesign |

### Concordance table template

| Harmonized | 2001 | 2003 | 2005 | 2007-2008 | 2009-2010 | 2011-2012 | 2013-2014 | 2015-2016 | 2017-2018 |
|------------|------|------|------|-----------|-----------|-----------|-----------|-----------|-----------|
| [target] | [src] | [src] | [src] | [src] | [src] | [src] | [src] | [src] | [src] |

### PUMF vs Master

Document differences between PUMF (grouped) and Master (derived) variables:

| Concept | PUMF variable | Master variable | Difference |
|---------|---------------|-----------------|------------|
| [e.g., Hearing] | HUICGHER | HUICDHER | Grouped vs derived |

## L2: Semantic mapping

### Purpose

Define category mappings and identify semantic breaks across cycles.

### Semantic break documentation

For each identified break:

1. **Year of change**: When the break occurred
2. **Nature of change**: What changed (categories, question wording, etc.)
3. **Impact**: How it affects harmonization
4. **Resolution**: How cchsflow addresses it

### PUMF vs Master source type differences

During semantic mapping, identify whether PUMF and Master databases provide the same variable type:
- **Same type**: categorical on both → standard harmonization
- **Different type**: PUMF categorical, Master continuous → requires row splitting (see [pumf-master-harmonization.md](pumf-master-harmonization.md))

This determination affects L3 worksheet authoring — rows must be split by database type when recoding logic differs.

### Category mapping table

| Harmonized value | Meaning | 2001-2014 source | 2015+ source |
|------------------|---------|------------------|--------------|
| 1 | [meaning] | [code] | [code] |
| 2 | [meaning] | [code] | [code] |

## L3: Worksheet authoring

### Purpose

Create the actual CSV worksheets following cchsflow schema.

### Pre-authoring checklist

- [ ] L0-L2 documents complete
- [ ] All source variables verified against DDI
- [ ] Era mappings documented
- [ ] Semantic breaks identified

### Validation requirements

**Before merging to inst/extdata:**

```r
source("R/validate-all-source-references.R")
result <- validate_all_source_references("path/to/variable_details.csv")
print_all_validation_result(result)
```

### Common errors to avoid

1. **Wrong era variable name via `[VAR]` default** - See variableStart-databaseStart-authoring.md
2. **Database name typos** - `cchs_2009_2010_m` vs `cchs2009_2010_m`
3. **Wrong source variable mapping** - Double-check each db::VAR pair

## L4: DV specifications

### Purpose

Specify derived variable functions when variables cannot be passed through.

### When needed

- Collapsing categories across semantic breaks
- Deriving continuous from categorical
- Complex multi-variable derivations

## L5: Testing

### Purpose

Validate harmonization logic with unit tests and package checks.

### Required tests

1. **Category coverage** - All output categories have test cases
2. **Edge cases** - Missing data, boundary values
3. **Cross-cycle consistency** - Same inputs produce same outputs

### CSV worksheet validation

Before committing worksheet changes, validate CSV formatting:

```r
# Fix excessive quoting and formatting issues
Rscript exec/fix-worksheets.R
```

This runs `check_worksheet()` and `fix_worksheet()` from the cchsflow package against both `inst/extdata/variables.csv` and `inst/extdata/variable_details.csv`. The GHA "Check CSV Formatting" workflow will fail if CSVs have:

- Excessive quoting (all fields quoted when not needed)
- Wrong column order
- Empty trailing columns
- CRLF line endings
- Unsorted rows

If `exec/fix-worksheets.R` fails due to package load errors (untracked R files or missing dependencies), use this workaround:

```r
Rscript -e "
library(readr)
vars <- read.csv('inst/extdata/variables.csv', stringsAsFactors = FALSE, check.names = FALSE)
write_csv(vars, 'inst/extdata/variables.csv', na = '', quote = 'needed', escape = 'double', eol = '\n')
details <- read.csv('inst/extdata/variable_details.csv', stringsAsFactors = FALSE, check.names = FALSE)
write_csv(details, 'inst/extdata/variable_details.csv', na = '', quote = 'needed', escape = 'double', eol = '\n')
"
```

### R CMD check

Run R CMD check to catch package-level issues before pushing:

```r
Rscript -e "devtools::check()"
```

Common failures to watch for:

- **Undefined exports in NAMESPACE** - Functions listed in NAMESPACE that don't exist in any R file. This happens when functions are renamed or removed but NAMESPACE isn't updated. Fix by removing the stale `export()` lines from NAMESPACE or regenerating with `roxygen2::roxygenise()`.
- **Missing documentation** - New exported functions need roxygen docs.
- **Unresolved `source()` calls** - Untracked R files in `R/` that `source()` files not in the repo will break `devtools::load_all()` and roxygen.

## L6: Integration

### Purpose

Merge to production and validate with real PUMF/Master data.

### Pre-merge checklist

Before merging a PR:

1. **CSV formatting passes** - `Rscript exec/fix-worksheets.R` exits cleanly
2. **R CMD check passes** - No errors or warnings
3. **NAMESPACE is correct** - All exported functions exist; no stale exports
4. **GHA checks are green** - Both "R-CMD-check" and "Check CSV Formatting" workflows pass

### Integration test QMD template

Each CEP should include:

1. **availability-matrix.qmd** - Respondent counts by cycle, age group, province
2. **integration-test.qmd** - `rec_with_table()` validation

### Age cutoffs for dementia research

Standard cutoffs: 50+, 55+, 60+

### Two-tier testing

1. **Canada-wide** - Full sample availability
2. **Ontario-specific** - Filtered to Ontario (province == 35)

## Workflow state tracking

Each CEP subgroup should have a `_workflow_state.yaml`:

```yaml
domain: [domain]
subgroup: [subgroup]
status: [L0_pending | L1_complete | ... | L6_complete]

stages:
  L0_documentation:
    status: [pending | in_progress | complete]
    date_completed: "YYYY-MM-DD"
    output: L0_documentation_assessment.md

  L1_concordance:
    status: [pending | in_progress | complete]
    # ...

# etc.
```

## Related documentation

- [variableStart-databaseStart-authoring.md](variableStart-databaseStart-authoring.md) - Technical authoring rules
- [field-reference.md](field-reference.md) - Field definitions
