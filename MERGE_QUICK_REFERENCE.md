# CSV Merge Quick Reference Guide

This is a condensed quick-reference version of the full merge strategy. For
detailed information, see `CSV_MERGE_STRATEGY.md`.

## Overview

Merge `feature/v3.0.0-validation-infrastructure` → `dev` in phases to avoid
100% CSV diffs.

**Key Problem**: CSV files show 100% diffs due to:
- Formatting changes (line endings, quoting)
- Structural changes (6 new columns, reordering)
- Content changes (new variables, metadata updates)

**Solution**: Separate formatting, structure, and content into distinct PRs.

## Quick Start Checklist

### Prerequisites
- [ ] Review full strategy document: `CSV_MERGE_STRATEGY.md`
- [ ] Ensure you have Python 3 with pandas installed
- [ ] Clean working directory on dev branch
- [ ] Scripts available: `quote-empty-cells.py`, `restructure_csvs.py`,
      `verify_csv_integrity.py`

---

## Phase 1: Format Standardization (1 day)

**Goal**: Standardize CSV formatting on dev branch

```bash
# 1. Create branch
git checkout dev && git pull origin dev
git checkout -b dev-csv-formatting-prep

# 2. Apply standardization
python3 quote-empty-cells.py

# 3. Verify
python3 verify_csv_integrity.py formatted
git diff --stat inst/extdata/

# 4. Commit
git add inst/extdata/variables.csv inst/extdata/variable_details.csv
git commit -m "chore: standardize CSV formatting

Apply consistent formatting:
- variables.csv: QUOTE_ALL, LF line endings
- variable_details.csv: QUOTE_MINIMAL, CRLF line endings

No content changes. Using quote-empty-cells.py."

# 5. Create PR
gh pr create --base dev --head dev-csv-formatting-prep \
  --title "Standardize CSV formatting" \
  --body "Standardizes CSV formatting using quote-empty-cells.py script. \
No content changes - formatting only."

# 6. After merge
git checkout dev && git pull origin dev
```

---

## Phase 2: Structure Update (2-3 days)

**Goal**: Add 6 new columns to each file and reorder columns

```bash
# 1. Create branch
git checkout dev
git checkout -b dev-csv-structure-updates

# 2. Apply restructuring
python3 restructure_csvs.py

# 3. Verify (should be 360/3464 rows, 16/22 columns)
python3 verify_csv_integrity.py restructured
wc -l inst/extdata/*.csv

# 4. Review changes
git diff inst/extdata/variables.csv | head -50
head -1 inst/extdata/variables.csv

# 5. Commit
git add inst/extdata/variables.csv inst/extdata/variable_details.csv
git commit -m "feat: add new columns and restructure CSV files for v3.0.0

Added new metadata columns and reordered for logical grouping.

variables.csv (6 new columns):
- version, lastUpdated, reviewNotes
- ICES.confirmation, Observation..MD., status

variable_details.csv (6 new columns):
- ICES.confirmation, version, lastUpdated
- status, reviewNotes, review

No content changes - structure only.
Row counts: 360 and 3464 (unchanged)"

# 6. Create PR
gh pr create --base dev --head dev-csv-structure-updates \
  --title "Add new columns and restructure CSV files for v3.0.0" \
  --body "Adds 6 new metadata columns to each CSV file. All new columns \
empty. Structure-only change."

# 7. After merge
git checkout dev && git pull origin dev
```

---

## Phase 3: Content Changes (1-2 weeks)

**Goal**: Cherry-pick content changes in logical groups

### Group 1: Infrastructure/Schema (2-3 days)

```bash
git checkout dev
git checkout -b merge-v3-infrastructure-schema

# Cherry-pick commits
git cherry-pick 4690c86 73223df 786d8a8 15fc75d 26e719f 9fdd927

# Resolve conflicts, then:
python3 quote-empty-cells.py
git add inst/extdata/
git commit --amend --no-edit

# Create PR
gh pr create --base dev --head merge-v3-infrastructure-schema \
  --title "Infrastructure: v3.0.0 validation infrastructure and schema" \
  --body "Cherry-picked 6 commits implementing v3.0.0 validation \
infrastructure."
```

### Group 2: BMI Variables (1-2 days)

```bash
git checkout dev
git checkout -b merge-v3-bmi-variables

git cherry-pick 227888e 9dc3958 ab206c1

python3 quote-empty-cells.py
git add inst/extdata/
git commit --amend --no-edit

gh pr create --base dev --head merge-v3-bmi-variables \
  --title "BMI: Modernize BMI variables and validation" \
  --body "Cherry-picked 3 commits modernizing BMI architecture."
```

### Group 3: Smoking Variables (2-3 days)

```bash
git checkout dev
git checkout -b merge-v3-smoking-variables

git cherry-pick f8f0277 6220e84 82b9c63 57aba9c \
  801eb97 e961c6e 352fe16 4a25f8a

python3 quote-empty-cells.py
git add inst/extdata/
git commit --amend --no-edit

gh pr create --base dev --head merge-v3-smoking-variables \
  --title "Smoking: Add continuous smoking variables" \
  --body "Cherry-picked 8 commits adding smoking variables."
```

### Group 4: ADL Variables (1 day)

```bash
git checkout dev
git checkout -b merge-v3-adl-variables

git cherry-pick aeae50e 35007e4 65b417e

python3 quote-empty-cells.py
git add inst/extdata/
git commit --amend --no-edit

gh pr create --base dev --head merge-v3-adl-variables \
  --title "ADL: Bug fixes and updates" \
  --body "Cherry-picked 3 commits updating ADL variables."
```

### Group 5: Oral Health Variables (1-2 days)

```bash
git checkout dev
git checkout -b merge-v3-oral-health-variables

git cherry-pick a562a2a b3ca31e 0220528 9be9b8f ae18185

python3 quote-empty-cells.py
git add inst/extdata/
git commit --amend --no-edit

gh pr create --base dev --head merge-v3-oral-health-variables \
  --title "Oral Health: Add five new variables" \
  --body "Cherry-picked 5 commits adding oral health variables."
```

### Group 6: Other Updates (1 day)

```bash
git checkout dev
git checkout -b merge-v3-health-condition-updates

git cherry-pick e724e82

python3 quote-empty-cells.py
git add inst/extdata/
git commit --amend --no-edit

gh pr create --base dev --head merge-v3-health-condition-updates \
  --title "Health Conditions: Update CCC_181" \
  --body "Cherry-picked 1 commit updating CCC_181 variable."
```

---

## Phase 4: Final Verification (1-2 days)

```bash
# After all PRs merged
git checkout dev && git pull origin dev

# Verify final state (should be 379/3721 rows, 16/22 columns)
python3 verify_csv_integrity.py final

# Check for any missing commits
git log feature/v3.0.0-validation-infrastructure --not dev \
  -- inst/extdata/variables.csv inst/extdata/variable_details.csv

# Should show no output if all commits merged
```

---

## Common Commands

### Verification Commands
```bash
# Count rows
wc -l inst/extdata/*.csv

# Check headers
head -1 inst/extdata/variables.csv
head -1 inst/extdata/variable_details.csv

# Verify integrity
python3 verify_csv_integrity.py <stage>
# Stages: dev, formatted, restructured, final

# View formatting
cat -A inst/extdata/variables.csv | head -5

# Check diff stats
git diff --stat dev..HEAD -- inst/extdata/
```

### Git Commands
```bash
# View commits needing merge
git log --oneline feature/v3.0.0-validation-infrastructure --not dev \
  -- inst/extdata/variables.csv inst/extdata/variable_details.csv

# Show specific commit
git show <commit-hash> --stat

# Cherry-pick with author preservation
git cherry-pick <commit-hash>

# If conflicts in cherry-pick
git status
# ... resolve conflicts ...
git cherry-pick --continue
```

### Troubleshooting
```bash
# Abort cherry-pick
git cherry-pick --abort

# Reset branch to origin
git reset --hard origin/dev

# View recent actions
git reflog

# Check file encoding
file inst/extdata/variables.csv
```

---

## Commit Inventory

Quick reference of commits by group:

**Infrastructure (6):**
4690c86, 73223df, 786d8a8, 15fc75d, 26e719f, 9fdd927

**BMI (3):**
227888e, 9dc3958, ab206c1

**Smoking (8):**
f8f0277, 6220e84, 82b9c63, 57aba9c, 801eb97, e961c6e, 352fe16, 4a25f8a

**ADL (3):**
aeae50e, 35007e4, 65b417e

**Oral Health (5):**
a562a2a, b3ca31e, 0220528, 9be9b8f, ae18185

**Other (1):**
e724e82

**Total: 26 commits**

---

## Expected Row/Column Counts

| Stage             | variables.csv      | variable_details.csv |
|-------------------|--------------------|----------------------|
| Dev original      | 360 rows, 10 cols  | 3464 rows, 16 cols   |
| After formatting  | 360 rows, 10 cols  | 3464 rows, 16 cols   |
| After restructure | 360 rows, 16 cols  | 3464 rows, 22 cols   |
| Final             | 379 rows, 16 cols  | 3721 rows, 22 cols   |

---

## Success Criteria

- [x] All 26+ commits accounted for
- [x] Correct structure: 16 and 22 columns
- [x] Correct row counts: 379 and 3721
- [x] All tests pass
- [x] Proper formatting (quoting, line endings)
- [x] All PRs reviewed and approved
- [x] Clean git history
- [x] Original authorship preserved
- [x] No data loss or corruption

---

## Emergency Rollback

If something goes wrong:

```bash
# For a single PR (before other changes)
gh pr close <PR-number>
git checkout dev
git revert <merge-commit-hash>
git push origin dev

# For multiple PRs
# Create new branch and restart from last known good state
git checkout -b dev-rollback <last-good-commit>
# Then force push if necessary (with team approval)
```

---

## Resources

- Full strategy: `CSV_MERGE_STRATEGY.md`
- Scripts:
  - `quote-empty-cells.py` - Format standardization
  - `restructure_csvs.py` - Add columns and reorder
  - `verify_csv_integrity.py` - Verify structure
- Branch: `feature/v3.0.0-validation-infrastructure`
- Target: `dev`

---

**Quick Reference Version**: 1.0
**Last Updated**: 2025-11-14
