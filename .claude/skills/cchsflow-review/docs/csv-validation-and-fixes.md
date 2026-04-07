# CSV validation and worksheet fixes

## CSV validation tools

Before proposing fixes, run the automated CSV validation tools to catch formatting and schema issues that the manual checks may have missed.

### Available tools

**`check_worksheet()` / `fix_worksheet()`** (on `v3-smoking` and later branches):

```bash
# Check for formatting violations (column order, line endings, row sorting, quoting)
Rscript exec/check-worksheets.R

# Auto-fix formatting violations
Rscript exec/fix-worksheets.R
```

On branches that have it (see branch availability table below), these are enforced by the `check-csv.yml` GitHub Action on PRs that modify `inst/extdata/variables.csv` or `variable_details.csv`. The GHA runs `check-worksheets.R` and fails if violations are found.

**`standardise_csv()`** (on `feature/csv-standardisation-updates` branch):

```r
# Basic mode — fix git conflicts (BOM, line endings, column order)
standardise_csv("inst/extdata/variables.csv")

# Collaboration mode — enhanced schema validation
standardise_csv("inst/extdata/variable_details.csv", collaboration = TRUE, validate_only = TRUE)
```

Collaboration mode validates fields against naming convention regex patterns (see `docs/variable-naming-conventions.md`) including `dummyVariable`, `variableStart`, `recStart`, and `recEnd`. It also checks for missing categorical dummy variables and cross-field rules.

### When to run

- **Always** run `check-worksheets.R` (or `standardise_csv()` if available) before proposing fixes, to ensure proposed changes don't introduce new formatting violations
- **After applying fixes**, run validation again to confirm the fix didn't break formatting
- If the PR's branch has `check-csv.yml` GHA, check whether CI passed — if not, the formatting issues may need to be fixed before the review's substantive issues

### Branch availability

| Tool | Branches |
|------|----------|
| `check_worksheet()` / `fix_worksheet()` | `skills/review-validation`, `v3-smoking`, `feature/v3.0.0-validation-infrastructure`, and later |
| `standardise_csv()` with collaboration mode | `feature/csv-standardisation-updates` and later |
| `check-csv.yml` GHA | `v3-smoking` and later |
| `diff-worksheets.R` | `skills/review-validation` and later |
| `rebuild-rows.R` | `skills/review-validation` and later |
| `query-metadata.R` | `skills/review-validation` and later |

**Known gap:** PR branches forked before validation tools were added will not have `exec/check-worksheets.R` or `exec/fix-worksheets.R`. If the PR branch lacks these tools, cherry-pick them from `skills/review-validation` or check out only the needed files:

```bash
# Validate worksheets from a branch that has the tools
git stash
git checkout skills/review-validation -- exec/check-worksheets.R exec/fix-worksheets.R R/check-worksheet.R R/fix-worksheet.R
Rscript exec/check-worksheets.R
git checkout -- exec/ R/check-worksheet.R R/fix-worksheet.R
git stash pop
```

### Content-based diff

For PRs with large diffs dominated by formatting changes (quoting, whitespace), line-based diffs are unreliable. Use the content-based diff tool:

```bash
Rscript exec/diff-worksheets.R --ref origin/main
Rscript exec/diff-worksheets.R --ref origin/main --variables "HUI06,HUI07,HUI08"
```

This groups rows by variable and compares only key fields, ignoring formatting differences.

### Programmatic row rebuild

For bulk coverage expansion (adding many cycles to a variable group), use the template-based row builder rather than manual CSV editing:

```r
source("exec/rebuild-rows.R")
vd <- read_vd()
template <- vd[vd$variable == "HUI06", ][1, ]

# Generate rows
rows <- rbind(
  binary_block(template, "HUI06", "cchs2001_m, cchs2003_m", "[HUI_06]"),
  wdm_block(template, "HUI07A", "cchs2017_2018_m", "[WDM_010]")
)
preview_rows(rows)
rebuild_variable(rows, "HUI06", dry_run = FALSE)
```

Available block generators: `binary_block()`, `wdm_block()`, `likert4_block()`. See `exec/rebuild-rows.R --help` for details.

## Proposing worksheet fixes

If the review identified worksheet errors (typos, missing mappings, incorrect database names), propose fixes to the user rather than silently modifying the worksheets.

### Workflow

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

### When not to fix

- Pre-existing issues on the target branch that are outside the PR's scope — note them in the review but do not propose fixes as part of this PR
- **Exception: `_s` suffix databases** — always fix `_s` → `_m` when encountered in reviewed variables, even if pre-existing. Deprecated suffixes should not persist in the worksheets.
- Issues that require domain judgement (e.g., whether a variable should use a different source name) — flag for human review
- Changes to R functions — these require separate code review and testing

## Scope expansion during review

If the review identifies expansion opportunities (e.g., additional cycles available in cchsflow-docs that are not yet in the worksheets) and the user requests adding them, the review transitions into authoring:

1. **Enter plan mode** to design the worksheet changes. The plan should cover which variables, databases, and variableStart mappings need updating.
2. **Write a modification script** (Python csv module) that reads from `inst/extdata/`, applies all changes, and writes to `/tmp/` for user review. The script should handle both the expansion and any typo fixes from the review.
3. **Run verification** — check databaseStart consistency, era boundary correctness, and variableStart mappings in the `/tmp/` output files.
4. **Present changes to the user** with a clear summary of what was modified before applying to `inst/extdata/`.
5. **Update the CEP** to document the expansion (new cycles, era boundaries, naming changes).
6. **Re-run CSV validation** on the expanded worksheets.

The key constraint: all changes go through `/tmp/` for review before touching `inst/extdata/`. The review skill delegates to the worksheets skill for authoring decisions (era naming conventions, variableStart patterns).
