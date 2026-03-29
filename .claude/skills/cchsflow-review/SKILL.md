---
name: cchsflow-review
description: Review cchsflow worksheet changes for correctness using the CEP/L0-L6 process. Use when reviewing PRs that modify variables.csv or variable_details.csv, or when a user wants to validate their own harmonization work. Generates or updates a CEP as a review artifact, runs worksheet checks, and performs L6 implementation validation with rec_with_table(). Invoke with a PR number or a list of variables.
allowed-tools: Bash(gh:*), Bash(git:*), Bash(Rscript:*), Bash(R:*), Read, Glob, Grep
---

# cchsflow worksheet review

CEP-driven review for cchsflow worksheet changes. Reviews follow the L0-L6 harmonization workflow, generating a CEP as a review artifact that documents findings and links to the PR.

## Usage

```
/cchsflow-review <PR-number>
/cchsflow-review              # review unstaged changes
```

## Workflow

### Step 1: Scope and triage

Before any checks, establish what is being reviewed and assess the shape of the diff.

#### Confirm scope with the user

cchsflow PRs typically cover one domain at a time (smoking, alcohol, physical activity, etc.). If the scope is not explicit in the invocation, **ask before proceeding**:

> "Which variables or domain should I focus on? (e.g., smoking variables, SMK_*/SMKG_*, or a specific list)"

This prevents accidentally reviewing or modifying variables from other domains that happen to share the same worksheets. Do not infer scope from the branch name alone — confirm with the user.

#### Review contexts

- **PR review**: Reviewing another contributor's PR
- **Self-review**: User is checking their own in-progress harmonization work

#### Triage the diff

For PR reviews, run triage first:

1. **Get the diff** and identify which variables were modified in `variable_details.csv` and `variables.csv`
2. **Check `variables.csv` diff size** — if the entire file was rewritten (line count matches total rows), flag as potential formatting/schema change vs targeted edits
3. **Check GHA status** — have CI checks run? Are they passing?
4. **Count modified variables** and group by domain

**Important:** `gh pr diff --stat` does not exist and `gh pr diff` does not support path filtering. Instead, check out the PR branch and use git directly:

```bash
gh pr checkout <PR> --repo Big-Life-Lab/cchsflow
git fetch origin <target-branch>
git diff origin/<target>...HEAD --numstat          # file-level change stats
git diff origin/<target>...HEAD --name-only        # file list
gh pr checks <PR> --repo Big-Life-Lab/cchsflow 2>&1 || echo "No checks configured"
```

Note: `gh pr checks` returns exit code 1 when no checks exist — this is not an error.

**Full-file formatting changes:** If `variables.csv` shows a line count close to its total row count (e.g., 379+/379-), the diff may be dominated by formatting changes (quoting, whitespace) rather than content changes. Use Python's csv module to compare content between branches, ignoring formatting:

```python
python3 -c "
import csv
old = list(csv.DictReader(open('/tmp/variables_target.csv')))
new = list(csv.DictReader(open('inst/extdata/variables.csv')))
# Compare by variable name, find content differences
"
```

Never use bash text tools (sed, awk, grep) to parse CSV files — use Python csv or R `read.csv()` for reliable structured data parsing.

#### Propose a scope

Extract the list of modified variables from the diff, then propose:

1. **Variables**: List all variables found in the diff, grouped by domain if possible. Flag any variables that appear in the diff but are not mentioned in the PR title/description.
   - Example: "Proposing to review 8 variables: FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG, FVCDJUI, diet_score, diet_score_cat3. The PR also modifies ADL_01 and 293 other variables in variables.csv — these are outside the stated scope."

2. **Database types**: Default to **both PUMF (`_p`) and Master (`_m`)**. cchsflow currently supports `_p` and `_m` suffixes. The `_s` (share file) suffix is deprecated and must be converted to `_m` whenever encountered in reviewed variables — this is a required fix, not just a note. The `_i` (ICES) suffix is similarly deprecated — replace with `_m`. Before converting `_s` → `_m`, verify that a corresponding `_m` entry does not already exist for that database (if it does, delete the `_s` row instead of renaming it).

3. **Cycles**: Default to **all cycles present in the diff** (typically 2001 through 2017-2018, expanding as new cycles are added).

#### Print and proceed

Print the proposed scope and triage summary clearly to the console, then proceed. The user can interrupt at any time to narrow or expand the scope.

```
Triage:
  Files changed: variables.csv (379+/379-), variable_details.csv (186+/186-)
  Variables modified: 302 total (8 in-scope, 294 out-of-scope)
  GHA checks: not run
  Full-file rewrite detected in variables.csv (likely formatting change)

Proposed review scope:
  Variables: FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG, FVCDJUI, diet_score, diet_score_cat3
  Database types: PUMF (_p) and Master (_m)
  Cycles: 2001 through 2017-2018
  Out-of-scope: 294 other variables, column reordering

Proceeding with review. Interrupt to adjust scope.
```

If the user has already specified a scope (e.g., "just review the FVC variables"), skip the proposal and use their scope directly.

#### Scope boundaries

If the diff contains changes beyond the agreed variables (e.g., column reordering, unrelated variable modifications), note this in the triage output but do not review those changes unless the user requests it.

### Step 2: Eligibility check (PR reviews only)

For PR reviews, check the PR is reviewable:

- State is OPEN and not a draft
- Not an automated PR
- If a prior approval exists, check whether new commits were pushed after the approval date — if so, the PR still needs review

```bash
gh pr view <PR> --repo Big-Life-Lab/cchsflow --json state,isDraft,author,reviews,commits
```

### Step 3: Set up working tree and locate/create CEP

#### Ensure worksheets are from the PR branch

For PR reviews, the working tree must have the PR's worksheets so that `rec_with_table()` tests the PR's changes, not `main`. Check out the PR branch:

```bash
gh pr checkout <PR> --repo Big-Life-Lab/cchsflow
```

If the PR modifies R functions (e.g., new derived variable functions in `R/`), use `devtools::load_all()` instead of `library(cchsflow)` in integration tests so the PR's code is loaded rather than the installed package version.

**Expected warnings:** `devtools::load_all()` on feature branches commonly produces NAMESPACE conflict warnings (e.g., `has_cchs_missing_codes`, `if_else2`) and "no such file" warnings for files that exist on other branches. These are expected and do not prevent tests from running. Do not flag these as issues.

#### Regression baseline

To distinguish PR-introduced issues from pre-existing ones, fetch the target branch and use it as a baseline:

```bash
git fetch origin <target-branch>
```

For every issue found in steps 5-6, check whether it also exists on the target branch:
- **Worksheet typos**: Compare the specific variable's rows between branches using Python csv module
- **L6 failures**: If `rec_with_table()` fails for a cycle, check whether the same cycle works on the target branch
- **Low prevalence**: Check whether the same pattern exists on the target branch — if so, it's pre-existing

An issue that exists on the target branch is pre-existing (score 0) unless the PR makes it worse. An issue that exists on the target branch for *other* variables but was copied into the PR's variables is PR-introduced (score normally).

#### Locate or create CEP

Check if a CEP already exists for this domain/variable group. CEPs live in `ceps/cep-NNN-<domain>/`.

**If a CEP exists:**
- Read its current state (`_workflow_state.yaml` if present)
- Note which L-stages are complete
- Focus the review on stages that are incomplete or need re-validation

**If no CEP exists**, default to creating a **minimal review CEP** for PR reviews:

```
ceps/cep-NNN-<domain>/
  PR-<number>-review-summary.md    # Findings and recommendations
  integration-test-<vars>.R        # rec_with_table() test script
  <vars>-pumf-integration-test.csv # Test results
  variable-availability.csv        # Variable availability matrix
```

The user can interrupt to request a full CEP (with L0-L6 documents, QMDs, subgroup specs — see CEP-002 for the pattern) or to skip CEP generation entirely.

**CEP numbering:** To avoid collisions with CEPs on other branches, scan for existing CEP numbers across all branches:

```bash
git log --all --oneline -- 'ceps/' | head -20
ls ceps/ 2>/dev/null
```

Use the next available number. Include the domain name (e.g., `cep-007-diet`) to disambiguate.

### Step 4: L0-L2 documentation review

For each in-scope variable, verify the documentation foundations. Read `.claude/skills/cchsflow-worksheets/docs/harmonization-workflow.md` for detailed L0-L2 templates.

#### L0: Documentation assessment

Verify source variables against CCHS documentation using the **cchsflow-docs** repository (`Big-Life-Lab/cchsflow-docs` on GitHub, cloned alongside cchsflow). This step confirms that variables claimed in `variableStart` and `databaseStart` actually exist in the CCHS data for those cycles.

##### Primary source: cchs-metadata MCP server

**Always use the cchs-metadata MCP as the primary tool for L0-L1 verification.** It provides the most complete and queryable metadata — 16,000+ variables across 251 datasets, enriched from PUMF RData, DDI XML, and ICES sources with full provenance tracking.

**Key tools:**
- `mcp__cchs-metadata__search_variables(query)` — find variables by name or label substring
- `mcp__cchs-metadata__get_variable_detail(variable_name)` — full metadata including labels, question text, value codes, dataset history
- `mcp__cchs-metadata__get_variable_history(variable_name)` — which cycles/datasets contain the variable (essential for era boundary verification)
- `mcp__cchs-metadata__get_value_codes(variable_name)` — response categories with frequencies
- `mcp__cchs-metadata__compare_master_pumf(variable_name, cycle)` — compare PUMF vs Master metadata for a specific cycle (essential for PUMF/Master split decisions)
- `mcp__cchs-metadata__suggest_cchsflow_row(variable_name)` — draft a cchsflow harmonisation row
- `mcp__cchs-metadata__get_dataset_variables(dataset_id)` — list all variables in a specific dataset
- `mcp__cchs-metadata__get_source_conflicts(variable_name, dataset_id)` — find cross-source label disagreements (useful for catching metadata inconsistencies)
- `mcp__cchs-metadata__get_database_summary()` — database overview and statistics

**Using MCP results:**
- The `cchsflow_name` field maps StatCan source variables to their cchsflow harmonized names — use this to verify that `variableStart` entries point to the correct source variable for each cycle
- Use `get_variable_history` to confirm a variable exists across claimed cycles and to identify era renames (e.g., SMK_09C → SMK_090 at the 2015 boundary)
- Use `compare_master_pumf` to verify whether PUMF and Master share the same source variable or need split rows

**Caution:** The MCP `label_short`/`label_long` fields may be contaminated by cchsflow labels (see MCP error report from alcohol review). Always cross-check against `label_statcan` which comes from DDI primary sources.

##### If the MCP is not available

Check whether the MCP is loaded:
```bash
claude mcp list
```

If `cchs-metadata` is missing or shows "Failed to connect", the server needs to be configured. The MCP server (v0.3.0+) lives in the **cchsflow-docs** repository and is also available as a [GitHub release](https://github.com/Big-Life-Lab/cchsflow-docs/releases).

**Quick setup** (if cchsflow-docs is cloned at `../cchsflow-docs/`):
```bash
cd ../cchsflow-docs/mcp-server && bash ../scripts/setup.sh
claude mcp add cchs-metadata -- python3 /Users/dmanuel/github/cchsflow-docs/mcp-server/server.py
```

**Manual setup:**
1. Ensure `cchsflow-docs` is cloned alongside cchsflow (typically `../cchsflow-docs/`)
2. Ensure `mcp-server/server.py` exists in cchsflow-docs
3. Ensure the database exists: `../cchsflow-docs/database/cchs_metadata.duckdb` (download from the [v0.3.0 release](https://github.com/Big-Life-Lab/cchsflow-docs/releases) or rebuild: `Rscript --vanilla ../cchsflow-docs/database/build_db.R`)
4. Add the MCP to Claude Code:
   ```bash
   claude mcp add cchs-metadata -- python3 /Users/dmanuel/github/cchsflow-docs/mcp-server/server.py
   ```
   Or add to `~/.claude.json` (see `.mcp.json.example` in cchsflow-docs for a template):
   ```json
   "cchs-metadata": {
     "type": "stdio",
     "command": "python3",
     "args": ["/Users/dmanuel/github/cchsflow-docs/mcp-server/server.py"],
     "env": {"CCHS_DB_PATH": "/Users/dmanuel/github/cchsflow-docs/database/cchs_metadata.duckdb"}
   }
   ```
5. Restart Claude Code for the MCP tools to appear in the tool list

##### CLI fallback

If the MCP server cannot be started but the database exists, use the standalone CLI (no FastMCP dependency — only `duckdb` required):
```bash
python3 ../cchsflow-docs/mcp-server/cli.py search smoking
python3 ../cchsflow-docs/mcp-server/cli.py detail SMKDSTY
python3 ../cchsflow-docs/mcp-server/cli.py history SMK_204
python3 ../cchsflow-docs/mcp-server/cli.py conflicts --variable SMKDSTY
python3 ../cchsflow-docs/mcp-server/cli.py codes SMK_204
```

All commands support `--json` for machine-readable output and `--db PATH` for custom database path.

See the cchsflow-docs `CLAUDE.md` and `.claude/skills/cchs-database/SKILL.md` for database build workflow and schema details.

##### Fallback: file-based lookups

If the MCP is unavailable and cannot be restored, use these file-based sources in the cchsflow-docs repo (typically `../cchsflow-docs/`):

1. **Extracted YAML data dictionaries** — structured variable definitions by cycle:
   ```
   ../cchsflow-docs/cchs-extracted/data-dictionary/{year}/
   ```
   Coverage: 2000-2001 through 2023.

2. **DDI XML files** — authoritative StatsCan PUMF documentation:
   ```
   ../cchsflow-docs/cchs-pumf-docs/CCHS_DDI/
   ```

3. **CCHS variable dictionary CSV** — flat file for quick lookups:
   ```
   ../cchsflow-docs/data/cchs_variable_dictionary.csv
   ```

These are the raw sources that feed the MCP database. The MCP is strongly preferred because it cross-references all sources, deduplicates, and provides structured query tools rather than requiring manual grep/search across hundreds of files.

##### What to verify

For each in-scope variable:
1. **Existence**: Does the source variable name appear in the documentation for each claimed cycle?
2. **Category codes**: Do `recStart` values match the documented category definitions?
3. **Era renames**: For 2015+ cycles, confirm the renamed variable exists
4. **Cycle coverage up to latest available**: Check whether the variable exists in cycles beyond the PR's `databaseStart` (documentation covers up to 2023) — these may be candidates for expansion

##### What to flag

- Variable listed in `variableStart` but not found in documentation for that cycle → **P0** (wrong variable name)
- Variable not checked (no documentation available for that cycle) → note as untested
- Variable exists in additional cycles not included in `databaseStart` → informational (expansion opportunity)

#### L1: Variable concordance

Use the cchsflow-docs extracted data dictionaries to verify source variable names across eras:

- Pre-2007: cycle letter in 4th position (A=2001, C=2003, E=2005)
- 2007-2014: standard naming
- Post-2014: check for 3-digit renames — search the 2015+ YAML files to confirm actual names
- 2022+: check for modular renames (e.g., CSS/SPU prefixes for smoking)

For each era boundary, compare the variable name in `variableStart` against the corresponding cycle's YAML data dictionary in cchsflow-docs. PUMF and Master data dictionaries may differ — check both `_p` and `_m` YAML files where available.

#### L2: Semantic mapping

- Are category codes consistent across cycles?
- Are semantic breaks identified and documented?
- Do recoding rules handle all source categories?

### Step 5: L3-L5 worksheet and testing checks

Run these checks in parallel for the in-scope variables. Read `.claude/skills/cchsflow-worksheets/docs/variableStart-databaseStart-authoring.md` for detailed reference.

#### Check 1: Era boundary defaults

The most dangerous class of bug. For each variable:

1. Parse the `databaseStart` field — does it span both 2007-2014 and 2015+ cycles?
2. Parse the `variableStart` field — do 2015+ databases have explicit `db::VAR` mappings?
3. If a `[VAR]` default exists and 2015+ databases lack explicit mappings, the default will apply the wrong variable name at runtime

**Key 2015 renames to check:**
- Smoking categorical: SMK_06A → SMK_060, SMK_09A → SMK_080, SMK_10A → SMK_100
- Smoking continuous: SMK_06C → SMK_070, SMK_09C → SMK_090, SMK_10C → SMK_110
- Smoking derived: SMKDSTY → SMKDVSTY, SMKDSTP → SMKDVSTP
- PUMF grouped: SMKG06C → SMKG070, SMKG09C → SMKG090, SMKG10C → SMKG110
- FVC: FVCDFRU → FVCDVFRU, FVCDSAL → FVCDVGRN, FVCDCAR → FVCDVORA, FVCDPOT → FVCDVPOT, FVCDVEG → FVCDVVEG, FVCDJUI → FVCDVJUI
- ADL: ADL_01-06 → ADL_005-030 (3-digit, 2015-2021), then → ADL_05-30 (2-digit, 2023+)

**Key 2023 renames to check:**
- ADL: ADL_005 → ADL_05, ADL_010 → ADL_10, ADL_015 → ADL_15, ADL_020 → ADL_20, ADL_025 → ADL_25, ADL_030 → ADL_30. This is a new era boundary — `[ADL_005]` defaults will not work for 2023 databases.

#### Check 2: databaseStart consistency

For each variable:
1. Extract `databaseStart` from variables.csv
2. Extract all `databaseStart` entries from variable_details.csv for that variable
3. The variables.csv list must equal the union of all variable_details.csv lists
4. Flag any databases present in one file but not the other

For each mismatch found, classify it:
- **PR-introduced**: The mismatch is new (not on target branch) — report as P1
- **Pre-existing**: The mismatch exists on the target branch — document in pre-existing issues
- **`_p` in vd only**: PUMF databases in variable_details but not variables.csv is a known pattern for variables that span both pre-2015 and 2015+ eras (the pre-2015 block includes `_p` databases that the 2015+ block in variables.csv doesn't list). Note but do not flag as a bug.

All mismatches must be explicitly listed in the review summary, even pre-existing ones. Do not silently omit consistency results.

#### Check 2b: Multi-block recStart collisions

**Terminology:** A **recode block** is a set of rows in variable_details.csv sharing the same `variableStart` value. A recode block defines how one source variable maps to the harmonized output. Variables that changed source variable names or response category definitions across CCHS cycles require multiple blocks — one per distinct source structure. A single block can span multiple eras when the source variable name and category boundaries were stable across them.

Variables with multiple recode blocks must not have the same `recStart` value appearing in more than one block for the same database. If a `(database, recStart)` pair matches rows from two blocks, `rec_with_table()` will find duplicate rows and produce incorrect output.

Note: `databaseStart` overlap alone (a database appearing in two blocks' lists) is not sufficient to flag an error — cchsflow legitimately uses parallel PUMF and Master blocks that share databases but have non-overlapping `recStart` ranges. The collision must be at the `(database, recStart)` level.

**Automated check:** `exec/check-worksheets.R` runs `check_recode_blocks()` automatically. For manual inspection of a specific variable:

```r
vd_var <- variable_details[variable_details$variable == "VAR", ]
blocks <- split(vd_var, vd_var$variableStart)
db_sets <- lapply(blocks, function(b) {
  trimws(unlist(strsplit(b$databaseStart[1], ",")))
})
# Check all pairwise intersections (overlap is a necessary but not sufficient condition)
pairs <- combn(length(db_sets), 2)
for (i in seq_len(ncol(pairs))) {
  overlap <- intersect(db_sets[[pairs[1,i]]], db_sets[[pairs[2,i]]])
  if (length(overlap) > 0)
    cat("OVERLAP (check recStart too):", paste(overlap, collapse=", "), "\n")
}
```

Flag any confirmed `(database, recStart)` collision as **P0**.

This check is especially important for continuous variables with era-specific midpoint recodes (e.g., SMK_09A_cont, SMK_06A_cont) where different cycles have different category boundaries and require separate recode blocks.

#### Check 3: PUMF vs Master naming

For `_m` (master) databases:
- Pre-2007: cycle letter in source variable name (A=2001, C=2003, E=2005)
- 2007-2014: standard naming (no prefix letter)
- 2015+: check for renamed variables

For `_p` (PUMF) databases:
- May use grouped/derived variable names (e.g., SMKG prefix, FVCD prefix)

Verify that `_m` databases don't reference PUMF-only grouped variables, and vice versa.

For variables where PUMF and Master use fundamentally different source types (categorical vs continuous), see `cchsflow-worksheets/docs/pumf-master-harmonization.md` for the required row-splitting pattern and common errors.

#### Check 4: Pre-2007 cycle letters

For variables with pre-2007 master cycles, verify the cycle letter:
- 2001 (`_m` or `_p`): letter A in the variable name (e.g., SMKA_203, FVCADFRU)
- 2003: letter C (e.g., SMKC_203, FVCCDFRU)
- 2005: letter E (e.g., SMKE_203, FVCEDFRU)

The letter position varies by variable domain but follows a consistent pattern within each domain.

#### Check 5: Known error patterns

**Automated check:** `exec/check-worksheets.R` runs `check_invalid_databases()` on both worksheets. Review its output before manual scanning — it catches the first four patterns below automatically.

Scan for:
- `cchs20013_` — extra zero typo (should be `cchs2013_`)
- `chs20` without leading `c` — missing `c` typo (should be `cchs20`). This pattern has been found in ADL and FVC variables (e.g., `chs2011_2012_m` instead of `cchs2011_2012_m`). Check all database names match the `cchs` prefix.
- `_i` suffix databases — deprecated, should be `_m`
- `_s` suffix databases — deprecated, **always convert to `_m`** when found in reviewed variables. Check that a corresponding `_m` entry doesn't already exist (if it does, delete the `_s` row; if not, rename `_s` → `_m`). This applies even if the `_s` is pre-existing on the target branch — if the PR touches these rows, fix the suffix. **Naming convention**: `_s` share files are single-year extracts, so map to the single-year master form: `cchs2009_s` → `cchs2009_m` (not `cchs2009_2010_m`), `cchs2010_s` → `cchs2010_m`, `cchs2012_s` → `cchs2012_m`. Check `variables.csv` to confirm which `_m` form is expected.
- `cchs2021_p`, `cchs2022_p`, `cchs2023_p` — **invalid PUMF databases**. The 2021 CCHS was not released as a standalone PUMF — it was combined with 2022 data into a 2021-2022 PUMF (not yet in cchsflow). The 2022+ smoking variables were restructured into CSS/SPU modules; no standalone PUMF equivalent exists for variables like SMK_09A in those cycles. Remove these from `databaseStart` for PUMF-only or mixed blocks when encountered in reviewed variables.
- `[[VAR]]` — double brackets (invalid notation)
- `[VAR1, VAR2]` without `DerivedVar::` prefix — ambiguous multi-variable input

**Pre-existing typo propagation:** Typo patterns often exist in the target branch for other variables and get copied into new variables through copy-paste. For each typo found, check whether the same pattern exists on the target branch for the same variables — if not, it was introduced by this PR even if the pattern exists elsewhere.

#### Check 5b: dummyVariable naming conventions

Verify that `dummyVariable` values follow the naming convention defined in `metadata_registry.yaml`.

**Categorical variables** — regex: `^[a-zA-Z0-9_]+_cat[0-9]+(_[0-9]+|_NA[a-z])$`

| Row type | Pattern | Example |
|----------|---------|---------|
| Valid category | `{variable}_cat{N}_{recEnd}` | `SMK_204_cat4_1`, `FVC_1A_cat5_3` |
| Missing (not applicable) | `{variable}_cat{N}_NAa` | `SMK_204_cat4_NAa` |
| Missing (don't know/refusal) | `{variable}_cat{N}_NAb` | `SMK_204_cat4_NAb` |

**Continuous variables and Func rows** use `N/A` (no naming convention).

**Key rules:**
1. **No colons in dummy names** — use `_NAa` and `_NAb`, not `_NA::a` or `_NA::b`. Colons are invalid in identifiers.
2. **Suffix must match recEnd** — the number after the last underscore should equal the `recEnd` value for that row. A mismatch (e.g., `_cat5_2` with `recEnd=1`) indicates a copy-paste error.
3. **N must match numValidCat** — the number after `_cat` should equal the `numValidCat` value for valid categories of that variable.
4. **Func rows use `N/A`** — derived variable rows (where `recEnd` starts with `Func::`) use `dummyVariable=N/A`.

**What to flag:**
- `_NA::a` or `_NA::b` patterns (should be `_NAa` / `_NAb`)
- Suffix-recEnd mismatches (e.g., `_cat5_2` on a row with `recEnd=1`)
- Func rows with constructed dummy names instead of `N/A`
- Continuous rows with anything other than `N/A`

#### Check 5c: Swapped recEnd values

Check for rows where `recEnd` values appear to be swapped between adjacent rows. This is a **P0 data bug** — it produces incorrect values at runtime with no warning.

**Detection pattern:**
1. For each variable, examine rows where `recStart` is a valid data range (e.g., `[1,120]`) and adjacent rows where `recStart` is a not-applicable code (e.g., `996`)
2. The valid data range should map to `recEnd=copy` (or the appropriate output value), not to `NA::a`
3. A not-applicable code should map to `NA::a` or `NA::b`, not to `copy`

**Example (FVC_6D bug found in PR #148):**
```
# WRONG — recEnd values swapped
recStart=[1,120]  recEnd=NA::a   ← valid data being set to missing!
recStart=996      recEnd=copy    ← not-applicable code being copied as data!

# CORRECT
recStart=[1,120]  recEnd=copy    ← valid data copied through
recStart=996      recEnd=NA::a   ← not-applicable code set to missing
```

**When to check:** Always check continuous variables with `copy` and `NA::a`/`NA::b` recEnd values. Swapped values are especially likely for variables added via copy-paste from similar variables.

#### Check 5d: Label and metadata consistency

Scan for common metadata quality issues in modified variables:

1. **Double spaces** — check `label`, `labelLong`, `catLabel`, `catLabelLong`, `variableStartShortLabel`, and `variableStartLabel` for consecutive spaces
2. **Spelling errors in labels** — common typos: "consumptoin" (consumption), "freqeuncy" (frequency), "repondent" (respondent)
3. **Trailing punctuation in labelLong** — trailing dashes or incomplete labels (e.g., `"Daily consumption - fruit - (D)"` should be `"Daily consumption - fruit (D)"`)
4. **Missing descriptions** — derived daily frequency variables (FVCD*) and other derived variables should have `description` fields
5. **catLabel propagation** — when a label is fixed in `catLabel`, check that the same fix applies to `catLabelLong`, `variableStartShortLabel`, and `variableStartLabel` where those fields share the same text

These are P2 issues (metadata quality) but are cheap to fix during review and prevent accumulation of inconsistencies.

#### DV function naming convention (v3)

New or refactored DV functions should use tidyverse-style verb-first names. The `_fun` suffix is legacy and being phased out as functions are refactored.

| Verb | Purpose | Example |
|------|---------|---------|
| `calculate_*()` | Mathematical computation | `calculate_pct_time()`, `calculate_bmi()` |
| `categorize_*()` | Classification into groups | `categorize_pct_time()`, `categorize_bmi()` |
| `assess_*()` | Health risk evaluation | `assess_drinking_risk()` |
| `score_*()` | Scoring systems | `score_adl()` |
| `adjust_*()` | Data correction | `adjust_bmi()` |

Legacy functions (e.g., `bmi_fun()`, `pack_years_fun()`) retain old names until refactored. Worksheets reference functions via `Func::` prefix (e.g., `Func::calculate_pct_time`).

#### Check 6: L4 — derived variable specification review

If the in-scope variables include derived variables (functions in `R/`):

1. **Input consistency**: Read the DV function (e.g., `calculate_pct_time()` in `R/percent-time-canada.R`) and verify that the input variable names it expects match those listed in `variable_details.csv` for the derived variable
2. **Category coverage**: Verify the function handles all category values that the worksheet's `recFrom` maps to — no unhandled cases that would silently produce NA
3. **Output consistency**: Verify the function's return values match the `recTo` values in the worksheet
4. **Output bounds validation**: For continuous DVs, check whether the function validates output range. Values outside the valid domain (e.g., percentage >100 or <0) indicate inconsistent inputs and should return `tagged_na("b")`. The valid range should be documented in the `notes` field of the Func row in variable_details (documentation only for now, ready for future validation framework). If the DV lacks bounds checking, flag as P1.
5. **Documentation**: Check roxygen docs match the actual function signature

#### Check 7: Unit tests (L5)

If the PR includes or modifies test files in `tests/testthat/`:
- Verify category coverage (all output categories have test cases)
- Check edge cases (missing data, boundary values)
- Verify cross-cycle consistency

If the PR lacks tests for new derived variables, flag this.

### Step 6: L6 implementation validation

**This is the highest-priority check.** Run `rec_with_table()` against actual PUMF data. This is not just a pass/fail test — the output is an analytical tool. By examining prevalence and distributions across cycles and categories, reviewers can identify harmonization problems that worksheet checks alone cannot catch, such as a sudden step change in prevalence at an era boundary (e.g., 2014 → 2015) that signals a naming mismatch or category recode error.

#### Multi-era recode validation

For variables with multiple recode blocks (identified in Check 2b), standard L6 prevalence checks are insufficient — `rec_with_table()` may silently apply the wrong block or blend blocks without error. For these variables, perform era-specific output validation:

1. **Identify one representative PUMF cycle per block** — e.g., for SMK_09A_cont: `cchs2001_p` (Block 1 era), `cchs2007_2008_p` (Block 3 era)
2. **Run `rec_with_table()` for each representative cycle**
3. **Verify the recEnd values match the expected midpoints for that era** — not just that they are non-missing

For continuous variables, check a known respondent's output value against the expected midpoint for their source category. If the era boundary is at 2003 (different category boundaries in 2001 vs 2003+), a respondent with source code 3 should produce recEnd=4 in 2001 but recEnd=2.5 in 2003+. If both cycles produce the same value, the wrong block is being applied to one of them.

Flag any era boundary where observed output values do not match expected midpoints as **P0**.

#### Scope and limitations

**PUMF data only.** L6 can currently test only `_p` databases. The `data/` directory contains PUMF RData files (`cchs2001_p.RData` through `cchs2017_2018_p.RData`). Master (`_m`) data is in a secure environment where LLMs cannot run.

For master-only changes (e.g., a PR that only adds `_m` cycles), L6 cannot validate at runtime. In this case:
- Rely on L3-L5 worksheet checks (especially era boundary and naming checks)
- Generate the integration test R script anyway and save it to the CEP — the user or a colleague can run it in the secure environment
- Note the limitation explicitly in the review output

**Future:** Mock data from the `mockdata` repo will enable L6 testing for all database types.

#### Data locations

PUMF RData files are in `data/`:
- `cchs2001_p.RData` through `cchs2017_2018_p.RData`

Each file loads a data frame named after the cycle (e.g., `cchs2001_p`).

#### Integration test script

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

#### Cross-cycle prevalence QMD

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

#### Cross-cycle prevalence analysis

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

#### Derived variable testing

If the in-scope variables include derived variables (functions in `R/`):

1. Identify the DV function (e.g., `diet_score_fun()` in `R/diet.R`)
2. Check that all input variables are available in the test cycles
3. Run `rec_with_table()` with the derived variable to verify the full pipeline
4. Compare the derived variable's valid % against its input variables — the DV should not have materially higher valid % than its least-available input
5. For categorical derived variables and key continuous inputs, examine the **exposure distribution** across cycles — not just valid counts. The central harmonization question is whether typical exposures (e.g., proportion with 0 fruit/veg, or >5 servings/day) remain stable across cycles. A sudden shift in the distribution at an era boundary signals a recoding or mapping error even when valid % is unchanged. Include these distributions in both the integration test output and the QMD visualisation

#### What to report from L6

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

### Step 7: Confidence scoring

#### Re-confirm findings before scoring

Before finalising the review summary, **re-confirm each P0/P1 finding** by reading the specific cell directly from the current branch's `inst/extdata/` file using Python csv. Do not rely on earlier script output or cached copies (e.g., `/tmp/vd_pr.csv`). A finding that cannot be reproduced on a fresh read of the branch should be downgraded to 0. This step catches false positives caused by stale data in intermediate files.

#### Scoring scale

For each issue found, score confidence 0-100:

- **0**: False positive — doesn't stand up to scrutiny, or pre-existing issue (also present on target branch)
- **25**: Might be real but could be false positive; stylistic issue not in project docs
- **50**: Verified real issue but minor/nitpick; not very important relative to the PR
- **75**: Verified real issue that will impact functionality or is called out in project docs
- **100**: Definitely a real issue confirmed by evidence

**L6-specific scoring guidance:**
- `rec_with_table()` error (function fails) → **100** (confirmed breakage)
- 0% valid for a cycle that should have data → **100** (confirmed by PUMF data)
- Step change at era boundary → **90-100** depending on magnitude (confirmed by cross-cycle trend)
- Category distribution shift → **75** (requires domain interpretation, but flagged by data)
- L6 limitation (master-only, no runtime test available) → do not score; note as untestable

Filter out issues scoring below 80.

### Step 8: Report results

#### Save CEP artifacts

Save the integration test script, results, and QMD to the CEP directory:

```
ceps/cep-NNN-<domain>/
  cep-NNN-<domain>.qmd              # Cross-cycle prevalence plots and narrative
  PR-<number>-review-summary.md
  integration-test-<vars>.R
  <vars>-pumf-integration-test.csv
```

#### Commit and push CEP artifacts

After saving artifacts, **commit and push them to the PR branch** so other reviewers can access them. CEP artifacts referenced in PR comments must exist on the branch — local-only files create dead references.

```bash
git add ceps/cep-NNN-<domain>/
# Exclude rendered output (.html, *_files/, .quarto/) — only commit source files
git commit -m "Add CEP-NNN review artifacts for PR #XXX"
git push origin <branch>
```

If working on a different branch than the PR, push to the PR branch or note in the PR comment where the artifacts live.

#### Post PR comment (PR reviews)

Post a comment on the PR using `gh pr comment`:

```markdown
### Code review

Reviewed [N variables] for [PUMF/Master/both] across [cycle range].

#### L6 integration test: cross-cycle prevalence

Ran `rec_with_table()` against PUMF data for each cycle:

| Cycle | N | VAR1 valid % | VAR2 valid % | ... |
|-------|---|-------------|-------------|-----|
| cchs2001_p | 130,880 | 35.7% | ... | ... |
| cchs2003_p | 134,072 | 58.6% | ... | ... |
| ... | ... | ... | ... | ... |

[Note any step changes, zeros, or unexpected patterns here]

[If master-only changes were not testable, note: "Master (_m) mappings validated by worksheet checks only — no runtime data available for L6 testing."]

#### Issues found

[N issues or "No issues found"]

1. <description> (<L-stage>, <source of rule>)
   <link to file and line with full SHA>

CEP: `ceps/cep-NNN-<domain>/`

Generated with [Claude Code](https://claude.ai/code)
```

If no issues survive filtering:

```markdown
### Code review

Reviewed [N variables] for [PUMF/Master/both] across [cycle range]. No issues found.

L6 integration test: `rec_with_table()` ran successfully for all PUMF cycles.

Checked: era boundary defaults, databaseStart consistency, naming conventions, DV specifications, known error patterns, and PUMF integration.

CEP: `ceps/cep-NNN-<domain>/`

Generated with [Claude Code](https://claude.ai/code)
```

#### Self-review reporting

For self-review, report findings directly to the user without posting a PR comment. Still save CEP artifacts if CEP generation was not skipped.

### Step 9: Run CSV validation tools

Before proposing fixes, run the automated CSV validation tools to catch formatting and schema issues that the manual checks may have missed.

#### Available tools

**`check_worksheet()` / `fix_worksheet()`** (on `v3-smoking` and later branches):

```bash
# Check for formatting violations (column order, line endings, row sorting, quoting)
Rscript exec/check-worksheets.R

# Auto-fix formatting violations
Rscript exec/fix-worksheets.R
```

These are enforced by the `check-csv.yml` GitHub Action on PRs that modify `inst/extdata/variables.csv` or `variable_details.csv`. The GHA runs `check-worksheets.R` and fails if violations are found.

**`standardise_csv()`** (on `feature/csv-standardisation-updates` branch):

```r
# Basic mode — fix git conflicts (BOM, line endings, column order)
standardise_csv("inst/extdata/variables.csv")

# Collaboration mode — enhanced schema validation
standardise_csv("inst/extdata/variable_details.csv", collaboration = TRUE, validate_only = TRUE)
```

Collaboration mode validates fields against `metadata_registry.yaml` regex patterns including `dummyVariable`, `variableStart`, `recStart`, and `recEnd`. It also checks for missing categorical dummy variables and cross-field rules.

#### When to run

- **Always** run `check-worksheets.R` (or `standardise_csv()` if available) before proposing fixes, to ensure proposed changes don't introduce new formatting violations
- **After applying fixes**, run validation again to confirm the fix didn't break formatting
- If the PR's branch has `check-csv.yml` GHA, check whether CI passed — if not, the formatting issues may need to be fixed before the review's substantive issues

#### Branch availability

| Tool | Branches |
|------|----------|
| `check_worksheet()` / `fix_worksheet()` | `v3-smoking`, `feature/v3.0.0-validation-infrastructure`, and later |
| `standardise_csv()` with collaboration mode | `feature/csv-standardisation-updates` and later |
| `check-csv.yml` GHA | `v3-smoking` and later |

If the PR's branch doesn't have these tools, run validation from a branch that does by checking out only the worksheet files:

```bash
# Validate worksheets from a branch that has the tools
git stash
git checkout v3-smoking -- exec/check-worksheets.R R/check-worksheet.R R/fix-worksheet.R
Rscript exec/check-worksheets.R
git checkout -- exec/ R/check-worksheet.R R/fix-worksheet.R
git stash pop
```

### Step 10: Propose worksheet fixes (if issues found)

If the review identified worksheet errors (typos, missing mappings, incorrect database names), propose fixes to the user rather than silently modifying the worksheets.

#### Workflow

1. **Summarize the proposed changes** — list each fix with the affected variable(s), the current (incorrect) value, and the corrected value. For example:

   ```
   Proposed worksheet fixes:

   1. FVC_1A through FVC_6E (30 variables): Replace `chs2011_2012_m` with
      `cchs2011_2012_m` and `chs2013_2014_m` with `cchs2013_2014_m` in both
      variables.csv and variable_details.csv

   2. FVCDPOT: Replace `cchs20013_2014_m` with `cchs2013_2014_m` in
      variable_details.csv (extra zero)
   ```

2. **Wait for user approval** — the user decides whether to apply the fixes now, defer them, or handle them differently (e.g., as a follow-up PR, or let the PR author fix them).

3. **Apply fixes using R or Python** — never use bash text tools on CSV files. Use R's `read.csv()`/`write.csv()` or Python's csv module to make targeted edits while preserving the file's existing formatting and quoting conventions.

   **CRITICAL: Scope fixes to in-scope variables only.** When applying replacements (e.g., `_s` → `_m`, typo corrections), filter to only the rows belonging to the PR's in-scope variables. Never apply global `gsub()` or `str_replace_all()` across the entire dataframe — this will modify hundreds of unrelated variables. Always subset first:
   ```r
   alc_idx <- which(vd$variable %in% in_scope_vars)
   for (i in alc_idx) {
     vd$databaseStart[i] <- gsub("cchs2009_s", "cchs2009_m", vd$databaseStart[i])
   }
   ```

   **Multi-block databaseStart fix rule:** When `check_recode_blocks()` flags a recStart collision, the fix is to narrow each block's `databaseStart` to only the databases where that block's source variable actually exists. The key mental model:

   > Each block's `databaseStart` should contain only the databases where that block's `variableStart` is the correct source variable.

   **Critical anti-pattern — do not replace the entire databaseStart.** A block's `databaseStart` may include databases covered by a `[SHORTHAND]` entry in `variableStart` (e.g., `[SMK_09A]` covers cchs2007_2008_p through cchs2013_2014_p implicitly). If you replace the full `databaseStart` with only the databases visible in the explicit `db::VAR` prefixes, you will drop the shorthand-covered databases and create new gaps. Instead:

   1. Identify which database(s) are causing the collision (appear in two blocks)
   2. Determine which block those databases actually belong to (based on which era's source variable they use)
   3. Remove those databases from the block they do *not* belong to — leave everything else intact

   **Example:** If `cchs2001_p` appears in both Block 1 (2001 source variable) and Block 2 (2003+ source variable), remove `cchs2001_p` from Block 2's `databaseStart` only. Do not rewrite Block 2's full `databaseStart`.

   Always open Beyond Compare to verify the proposed fix before applying it to `inst/extdata/`.

4. **Save fixes to a temporary file** — per project conventions (CLAUDE.local.md), write proposed changes to `/tmp/` for user review before editing the main worksheet files directly. The user or PR author integrates the changes.

5. **Verify idempotency** — always read from `inst/extdata/` (the clean source), never from previously modified `/tmp/` files. After running a modification script, re-run it to confirm the output is identical. If the script detects its own changes on the second run (e.g., skips "already has 2021"), the idempotency check passed.

6. **Offer visual diff review** — before applying changes to `inst/extdata/`, pause and ask the user whether they want to review the diff in a visual diff tool (e.g., Beyond Compare, Kaleidoscope, VS Code diff). This is especially valuable for large worksheet changes where the programmatic summary may miss formatting issues (e.g., Python csv re-quoting all fields, creating a noisy diff that obscures the real changes).

   **For PR reviews**: Use the **merge base** as the comparison baseline, not the target branch tip. This ensures the diff shows only what the PR branch changed, excluding divergence on the target branch since the PR was created. This is especially important for full-file rewrites where comparing against the target tip shows noise from unrelated target-side changes.

   ```bash
   # Find the merge base between the PR branch and target
   MERGE_BASE=$(git merge-base origin/<target-branch> <pr-branch>)

   # Extract the file at the merge base
   git show ${MERGE_BASE}:inst/extdata/variable_details.csv > /tmp/vd_mergebase.csv
   git show ${MERGE_BASE}:inst/extdata/variables.csv > /tmp/vars_mergebase.csv

   # Compare merge base vs current PR branch (shows only PR changes)
   bcompare /tmp/vd_mergebase.csv inst/extdata/variable_details.csv
   bcompare /tmp/vars_mergebase.csv inst/extdata/variables.csv
   ```

   **For self-review / proposed fixes**: Compare the current working copy against the proposed modifications in `/tmp/`:

   ```bash
   bcompare inst/extdata/variable_details.csv /tmp/variable_details_updated.csv
   bcompare inst/extdata/variables.csv /tmp/variables_updated.csv
   ```

   If the user doesn't have a visual diff tool configured, offer to help set one up. Common options:
   - **Beyond Compare**: `brew install --cask beyond-compare` — configure as git difftool with `git config --global diff.tool bc` and `git config --global difftool.bc.path /usr/local/bin/bcompare`
   - **VS Code**: `code --diff <left> <right>`
   - **Kaleidoscope**: `ksdiff <left> <right>`
   - **FileMerge** (macOS built-in): `opendiff <left> <right>`

   **Why merge-base matters:** In the GEN_10 PR (#169) review, comparing against the target tip showed 23 extra DHHGAGE_E rows and SDCDCGT changes that were on the target branch, not the PR. This noise obscured the actual PR changes. Using merge-base revealed only the GEN_07 and GEN_10 rows — the true scope. Similarly, in the diet PR (#148) review, Python's csv writer re-quoted every field, producing a noisy git diff. A visual diff tool with merge-base comparison would have caught both issues immediately.

#### When not to fix

- Pre-existing issues on the target branch that are outside the PR's scope — note them in the review but do not propose fixes as part of this PR
- **Exception: `_s` suffix databases** — always fix `_s` → `_m` when encountered in reviewed variables, even if pre-existing. Deprecated suffixes should not persist in the worksheets.
- Issues that require domain judgement (e.g., whether a variable should use a different source name) — flag for human review
- Changes to R functions — these require separate code review and testing

### Scope expansion during review

If the review identifies expansion opportunities (e.g., additional cycles available in cchsflow-docs that are not yet in the worksheets) and the user requests adding them, the review transitions into authoring:

1. **Enter plan mode** to design the worksheet changes. The plan should cover which variables, databases, and variableStart mappings need updating.
2. **Write a modification script** (Python csv module) that reads from `inst/extdata/`, applies all changes, and writes to `/tmp/` for user review. The script should handle both the expansion and any typo fixes from the review.
3. **Run verification** — check databaseStart consistency, era boundary correctness, and variableStart mappings in the `/tmp/` output files.
4. **Present changes to the user** with a clear summary of what was modified before applying to `inst/extdata/`.
5. **Update the CEP** to document the expansion (new cycles, era boundaries, naming changes).
6. **Re-run CSV validation** (Step 9) on the expanded worksheets.

The key constraint: all changes go through `/tmp/` for review before touching `inst/extdata/`. The review skill delegates to the worksheets skill for authoring decisions (era naming conventions, variableStart patterns).

### Step 11: Retrospective — review the skill

After the PR comment is posted (or findings reported for self-review), take a moment to reflect on the review process while the work is still in context. This step is easy to skip but valuable for continuous improvement.

1. **What worked well?** Which checks caught real issues? Which were most efficient?
2. **What was slow or failed?** R script execution problems, false positives that wasted time, checks that didn't apply?
3. **What patterns emerged?** New typo patterns, domain-specific naming conventions, recurring copy-paste errors?
4. **Should the skill be updated?** New known error patterns, improved check logic, better operational practices (e.g., "always write R scripts to files, not inline")?
5. **What carries forward?** Pre-existing issues noted but not fixed, refactoring opportunities flagged, expansion opportunities identified?

Summarise the retrospective to the user. If skill updates are warranted, propose specific edits. If operational lessons were learned, consider updating project memory.

## Reference

- L0-L6 workflow: `.claude/skills/cchsflow-worksheets/docs/harmonization-workflow.md`
- Era mapping tables: `.claude/skills/cchsflow-worksheets/docs/variableStart-databaseStart-authoring.md`
- Schema definitions: `inst/metadata/schemas/core/variables.yaml`, `inst/metadata/schemas/core/variable_details.yaml`
- Regex patterns and naming conventions: `inst/metadata/documentation/metadata_registry.yaml`
- CSV formatting check/fix: `exec/check-worksheets.R`, `exec/fix-worksheets.R` (uses `R/check-worksheet.R`, `R/fix-worksheet.R`)
- CSV standardisation with schema validation: `R/csv-utils.R` (`standardise_csv()`), `R/schema-validation.R` (`validate_csv_against_schema()`)
- Validation constants: `R/validation-constants.R`
- GHA workflow for CSV checks: `.github/workflows/check-csv.yml`
- Example CEP (full): `ceps/cep-002-smoking/` (smoking harmonization)
- Example CEP (review): `ceps/cep-006-oral-health/` (DEN_132 PR review with integration tests)
- PUMF data: `data/cchs*_p.RData`
