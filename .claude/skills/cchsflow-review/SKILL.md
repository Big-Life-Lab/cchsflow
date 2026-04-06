---
name: cchsflow-review
description: Review cchsflow worksheet changes for correctness using the CEP/L0-L6 process. Use when reviewing PRs that modify variables.csv or variable_details.csv, or when a user wants to validate their own harmonization work. Generates or updates a CEP as a review artifact, runs worksheet checks, and performs L6 implementation validation with rec_with_table(). Invoke with a PR number or a list of variables.
allowed-tools: Bash(gh:*), Bash(git:*), Bash(Rscript:*), Bash(R:*), Read, Glob, Grep
---

# cchsflow worksheet review

CEP-driven review for cchsflow worksheet changes. Reviews follow the L0-L6 harmonization workflow, generating a CEP as a review artifact that documents findings and links to the PR.

## Usage

```
/cchsflow-review <PR-number>              # PR review mode
/cchsflow-review                          # self-review (unstaged changes)
/cchsflow-review --dev <variable-list>    # development/authoring mode
```

**Review mode** (default): Validates existing worksheet entries. Checks 1-8 focus on correctness of what's present. Check 8 (completeness) runs but flags omissions as informational rather than blocking.

**Development mode** (`--dev`): Runs all review checks plus full completeness audit with MCP verification. Omissions are flagged as P1. Useful when authoring new variables or expanding existing ones to additional cycles. The completeness audit actively searches for missing cycle coverage, missing variable family members (`_cont` bridges, categorical companions), and missing-code row gaps.

## Workflow

### Prerequisite: Read the worksheet reference

**Before performing any review**, read `docs/worksheet-reference.md` (located in this skill's `docs/` folder). This is the canonical reference for how cchsflow worksheets work — variable types, database naming, recStart/recEnd semantics, DerivedVar/Func:: mechanism, PUMF-Master bridging patterns, era splits, midpoint imputation, and v3 naming conventions. Without understanding these conventions, review findings will be unreliable.

Also available for cross-checking worksheet accuracy:
- **Gem verification workflow**: `docs/review/` (in this skill's folder) contains the Google NotebookLM Gem system prompt, notebook manifest, and coverage summary. The Gem cross-checks worksheet entries against ~239 StatCan PDFs. See the memory file `reference_gem_verification_workflow.md` for the full three-way triangulation process (Gem + MCP + Claude Code).
- **MCP cchs-metadata server**: Primary tool for L0-L1 verification (described in Step 4).

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
3. **Check GHA status** — have CI checks run? Are they passing? If GHA ran and **failed**, treat this as blocking — diagnose the failure before proceeding with worksheet review. Common GHA failures (CSV formatting, R CMD check) indicate package-level issues that should be resolved first.
4. **Count modified variables** and group by domain
5. **Check for R/ and tests/ changes** — if `git diff origin/<target>...HEAD --name-only` shows files under `R/` or `tests/testthat/`, the PR touches code, not just worksheets. Flag this in the triage output and note that **Step 7b (package health check)** will run. If R functions are new or substantially modified, the **cchsflow-derive** skill's done criteria (unit tests, R CMD check, roxygen, test coverage) also apply — see `.claude/skills/cchsflow-derive/SKILL.md` § "Done criteria".

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
  R/ files changed: R/immigration.R (whitespace only)
  Tests changed: tests/testthat/test-immigration.R (whitespace only)
  Variables modified: 302 total (8 in-scope, 294 out-of-scope)
  GHA checks: not run
  Full-file rewrite detected in variables.csv (likely formatting change)

Proposed review scope:
  Variables: FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG, FVCDJUI, diet_score, diet_score_cat3
  Database types: PUMF (_p) and Master (_m)
  Cycles: 2001 through 2017-2018
  Out-of-scope: 294 other variables, column reordering
  Package health: Step 7b will run (R/ files in diff)

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

Read and follow `docs/l0-l2-documentation-review.md` for the full procedure. This covers:
- L0: Documentation assessment (MCP cchs-metadata as primary tool, CLI fallback, file-based fallback)
- L1: Variable concordance (era rename chains, pre-2007 cycle letters)
- L2: Semantic mapping (category consistency, recoding rule coverage)

### Step 5: L3-L5 worksheet and testing checks

Read and follow `docs/l3-l5-worksheet-checks.md` for the full procedure. This covers:
- Check 1: Era boundary defaults (most dangerous bug class)
- Check 2: databaseStart consistency
- Check 2b: Multi-block recStart collisions
- Check 3: PUMF vs Master naming
- Check 4: Pre-2007 cycle letters
- Check 5: Known error patterns (typos, deprecated suffixes, invalid databases)
- Check 5b-5e: dummyVariable naming, swapped recEnd, label consistency, opaque suffixes
- DV function naming convention (v3)
- Worksheet-first principle
- Check 6: L4 derived variable specification review
- Check 7: Unit tests (L5)

### Step 6: L6 implementation validation

Read and follow `docs/l6-implementation-validation.md` for the full procedure. This covers:
- Multi-era recode validation
- Scope and limitations (PUMF data only)
- Integration test script template
- Cross-cycle prevalence QMD
- Cross-cycle prevalence analysis (step changes, unexpected zeros, distribution shifts)
- Derived variable testing
- What to report from L6

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
```

If no issues survive filtering:

```markdown
### Code review

Reviewed [N variables] for [PUMF/Master/both] across [cycle range]. No issues found.

L6 integration test: `rec_with_table()` ran successfully for all PUMF cycles.

Checked: era boundary defaults, databaseStart consistency, naming conventions, DV specifications, known error patterns, and PUMF integration.

CEP: `ceps/cep-NNN-<domain>/`
```

#### Self-review reporting

For self-review, report findings directly to the user without posting a PR comment. Still save CEP artifacts if CEP generation was not skipped.

### Step 9: Run CSV validation and propose fixes

Read and follow `docs/csv-validation-and-fixes.md` for the full procedure. This covers:
- Running `check-worksheets.R` and `standardise_csv()`
- Branch availability for validation tools
- Proposing worksheet fixes (scoped to in-scope variables only)
- Multi-block databaseStart fix rules
- Visual diff review with Beyond Compare
- Scope expansion during review

**Scoped validation (recommended):** Use `--subject` or `--variables` to limit checks to in-scope rows:
```bash
Rscript exec/check-worksheets.R --subject "Ethnicity,Language,Migration"
Rscript exec/fix-worksheets.R --variables "SDCGCGT,SDCFIMM"
```
Scoped mode is faster (~0.2s vs ~2s) and filters out pre-existing issues in unrelated variables. Use full-file mode for final pre-merge checks.

### Step 10: Scope expansion during review

If the review identifies expansion opportunities and the user requests adding them, follow the scope expansion procedure in `docs/csv-validation-and-fixes.md`.

### Step 11: Retrospective — review the skill

After the PR comment is posted (or findings reported for self-review), take a moment to reflect on the review process while the work is still in context. This step is easy to skip but valuable for continuous improvement.

1. **What worked well?** Which checks caught real issues? Which were most efficient?
2. **What was slow or failed?** R script execution problems, false positives that wasted time, checks that didn't apply?
3. **What patterns emerged?** New typo patterns, domain-specific naming conventions, recurring copy-paste errors?
4. **Should the skill be updated?** New known error patterns, improved check logic, better operational practices (e.g., "always write R scripts to files, not inline")?
5. **What carries forward?** Pre-existing issues noted but not fixed, refactoring opportunities flagged, expansion opportunities identified?

Summarise the retrospective to the user. If skill updates are warranted, propose specific edits. If operational lessons were learned, consider updating project memory.

## Reference

### Skill docs (in this folder)

- **Worksheet reference (MUST READ)**: `docs/worksheet-reference.md` — canonical guide to cchsflow worksheet conventions
- **L0-L2 documentation review**: `docs/l0-l2-documentation-review.md` — MCP setup, variable verification, concordance
- **L3-L5 worksheet checks**: `docs/l3-l5-worksheet-checks.md` — era boundaries, naming, error patterns
- **L6 implementation validation**: `docs/l6-implementation-validation.md` — rec_with_table() testing, prevalence analysis
- **CSV validation and fixes**: `docs/csv-validation-and-fixes.md` — check/fix tools, fix workflow, visual diff
- **Variable naming conventions**: `docs/variable-naming-conventions.md` — harmonized variable naming rules
- **Gem verification workflow**: `docs/review/` — NotebookLM Gem system prompt, notebook manifest, coverage summary

### External references

- L0-L6 workflow: `.claude/skills/cchsflow-worksheets/docs/harmonization-workflow.md`
- Era mapping tables: `.claude/skills/cchsflow-worksheets/docs/variableStart-databaseStart-authoring.md`
- Schema definitions: `inst/metadata/schemas/core/variables.yaml`, `inst/metadata/schemas/core/variable_details.yaml`
- Regex patterns and naming conventions: `inst/metadata/documentation/metadata_registry.yaml`
- CSV formatting check/fix: `exec/check-worksheets.R`, `exec/fix-worksheets.R` (uses `R/check-worksheet.R`, `R/fix-worksheet.R`). Supports `--subject` and `--variables` for scoped validation.
- Scope filtering: `R/scope-worksheets.R` (`scope_worksheets()`, `parse_scope_args()`)
- CSV standardisation with schema validation: `R/csv-utils.R` (`standardise_csv()`), `R/schema-validation.R` (`validate_csv_against_schema()`)
- Validation constants: `R/validation-constants.R`
- GHA workflow for CSV checks: `.github/workflows/check-csv.yml`
- Example CEP (full): `ceps/cep-002-smoking/` (smoking harmonization)
- Example CEP (review): `ceps/cep-006-oral-health/` (DEN_132 PR review with integration tests)
- PUMF data: `data/cchs*_p.RData`
