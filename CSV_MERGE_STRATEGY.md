# CSV Merge Strategy: feature/v3.0.0-validation-infrastructure → dev

## Executive Summary

This document outlines the strategy to merge changes from
`feature/v3.0.0-validation-infrastructure` into `dev` branch. The primary
challenge is that CSV files show 100% diffs on GitHub due to structural
changes (new columns, column reordering) and formatting changes (line endings,
quoting style), making it impossible to review the actual content changes.

## Problem Analysis

### Root Causes of 100% Diffs

1. **Structural Changes**
   - `variables.csv`: 6 new columns added (version, lastUpdated, reviewNotes,
     ICES.confirmation, Observation..MD., status)
   - `variable_details.csv`: 6 new columns added (ICES.confirmation, version,
     lastUpdated, status, reviewNotes, review)
   - Column reordering in both files

2. **Formatting Changes**
   - **Line endings**: dev uses CRLF (`\r\n`), feature branch uses LF (`\n`)
     for variables.csv
   - **Quoting style**: dev uses unquoted fields, feature branch uses quoted
     fields

3. **Content Changes**
   - 19 new variables added to variables.csv (360→379 rows)
   - 257 new rows added to variable_details.csv (3464→3721 rows)
   - Multiple updates to existing variable metadata
   - 26 commits affecting the CSV files

### Current State

- **Branch**: `feature/v3.0.0-validation-infrastructure`
- **Base**: `dev`
- **Commits**: 26 commits affecting CSV files in `inst/extdata/`
- **Standardization script**: `quote-empty-cells.py` available in working
  directory. This script reads in the variables and variable details sheet
  from the repo and outputs a standardized version.

## Merge Strategy

### Overview

The strategy uses a **multi-phase, multi-branch approach** with the following
key principles:

1. Standardize formatting on dev first
2. Apply structural changes separately from content changes
3. Group content changes into logical, reviewable commits
4. Preserve original commit metadata (authors, dates)
5. Create intermediate branches for review

### Phase 1: Prepare dev Branch (Formatting Standardization)

**Objective**: Standardize CSV formatting on dev to match the feature branch
conventions.

**Steps**:

1. Create a preparation branch from dev:
   ```bash
   git checkout dev
   git pull origin dev
   git checkout -b dev-csv-formatting-prep
   ```

2. Apply the standardization script to dev:
   ```bash
   # The script is already configured correctly
   python3 quote-empty-cells.py
   ```

3. Verify the changes:
   ```bash
   git diff inst/extdata/variables.csv | head -50
   git diff inst/extdata/variable_details.csv | head -50
   ```

4. Commit the formatting changes:
   ```bash
   git add inst/extdata/variables.csv inst/extdata/variable_details.csv
   git commit -m "Refactor: Standardize CSV formatting for variables and variable_details

Apply consistent formatting to CSV files:
- variables.csv: Use QUOTE_ALL quoting and LF line endings
- variable_details.csv: Use QUOTE_MINIMAL quoting and CRLF line endings

This standardization is done using quote-empty-cells.py to ensure
consistent formatting across contributors using different editors
and operating systems.

No content changes in this commit."
   ```

5. Create a PR and merge to dev:
   ```bash
   gh pr create --base dev --head dev-csv-formatting-prep \
     --title "Standardize CSV formatting" \
     --body "$(cat <<'EOF'
## Summary

Standardizes CSV file formatting using the quote-empty-cells.py script.

## Changes

- Apply QUOTE_ALL quoting to variables.csv with LF line endings
- Apply QUOTE_MINIMAL quoting to variable_details.csv with CRLF line endings
- Remove any empty header columns

## Why This Change

This standardization eliminates formatting-related diffs and ensures
consistent file formatting regardless of the editor or OS used by
contributors. This is a prerequisite for cleanly merging the v3.0.0
validation infrastructure changes.

## Verification

No content changes - only formatting. File row counts remain the same:
- variables.csv: 360 rows
- variable_details.csv: 3464 rows
EOF
)"
   ```

6. After PR approval and merge, update local dev:
   ```bash
   git checkout dev
   git pull origin dev
   ```

### Phase 2: Apply Structural Changes

**Objective**: Add new columns and reorder existing columns to match the
feature branch structure.

**Steps**:

1. Create a structural changes branch from updated dev:
   ```bash
   git checkout dev
   git checkout -b dev-csv-structure-updates
   ```

2. Extract the structural commit from the feature branch:

   Identify the earliest commit that added the new columns:
   ```bash
   git log --reverse feature/v3.0.0-validation-infrastructure --not dev \
     -- inst/extdata/variables.csv inst/extdata/variable_details.csv | head -20
   ```

   The structural changes appear to originate from:
   - `4690c86` - feat: add v2.2.0 variable enhancements
   - `73223df` - feat: enhance variable metadata for v2.2.0

3. Cherry-pick the structural commits:
   ```bash
   # First, analyze what these commits do
   git show 4690c86 --stat
   git show 73223df --stat

   # Cherry-pick with --no-commit to allow modifications
   git cherry-pick --no-commit 4690c86
   ```

4. **Manual intervention required**: At this point, you'll need to carefully:
   - Keep ONLY the column additions (new headers)
   - Keep ONLY the column reordering
   - REMOVE any content changes (new rows, modified data)
   - Preserve the dev data but in the new structure

5. A Python script approach for structural changes:

   Create `restructure_csvs.py`:
   ```python
   import csv
   import pandas as pd

   # For variables.csv
   # Read dev version
   dev_vars = pd.read_csv('inst/extdata/variables.csv')

   # Define new structure with column order from feature branch
   new_columns_order = [
       'variable', 'label', 'labelLong', 'variableType',
       'databaseStart', 'variableStart', 'subject', 'section',
       'units', 'description', 'version', 'lastUpdated',
       'reviewNotes', 'ICES.confirmation', 'Observation..MD.', 'status'
   ]

   # Add new columns with empty values
   for col in new_columns_order:
       if col not in dev_vars.columns:
           dev_vars[col] = ''

   # Reorder columns
   dev_vars_reordered = dev_vars[new_columns_order]

   # Write with proper formatting
   dev_vars_reordered.to_csv('inst/extdata/variables.csv',
                              index=False, quoting=csv.QUOTE_ALL,
                              lineterminator='\n')

   # Similar process for variable_details.csv
   dev_details = pd.read_csv('inst/extdata/variable_details.csv')

   new_details_columns_order = [
       'variable', 'dummyVariable', 'typeEnd', 'databaseStart',
       'variableStart', 'ICES.confirmation', 'typeStart', 'recEnd',
       'numValidCat', 'catLabel', 'catLabelLong', 'units',
       'recStart', 'catStartLabel', 'variableStartShortLabel',
       'variableStartLabel', 'notes', 'version', 'lastUpdated',
       'status', 'reviewNotes', 'review'
   ]

   for col in new_details_columns_order:
       if col not in dev_details.columns:
           dev_details[col] = ''

   dev_details_reordered = dev_details[new_details_columns_order]

   dev_details_reordered.to_csv('inst/extdata/variable_details.csv',
                                 index=False, quoting=csv.QUOTE_MINIMAL,
                                 lineterminator='\r\n')
   ```

6. Run the restructuring script:
   ```bash
   python3 restructure_csvs.py
   ```

7. Verify row counts haven't changed:
   ```bash
   wc -l inst/extdata/variables.csv inst/extdata/variable_details.csv
   # Should show 360 and 3464 rows respectively (plus headers)
   ```

8. Commit the structural changes:
   ```bash
   git add inst/extdata/variables.csv inst/extdata/variable_details.csv
   git commit -m "feat: add new columns and restructure CSV files for v3.0.0

Add new metadata columns to CSV worksheets:

variables.csv:
- version: Track variable version numbers
- lastUpdated: Record last modification date
- reviewNotes: Store review comments
- ICES.confirmation: ICES confirmation status
- Observation..MD.: MD observation notes
- status: Variable status (active/deprecated)

variable_details.csv:
- ICES.confirmation: ICES confirmation status
- version: Track version numbers
- lastUpdated: Record last modification date
- status: Variable status
- reviewNotes: Store review comments
- review: Review status

Also reordered columns for improved logical grouping.

No content changes - all new columns are empty at this stage.
Row counts remain unchanged:
- variables.csv: 360 rows
- variable_details.csv: 3464 rows"
   ```

9. Create PR for structural changes:
   ```bash
   gh pr create --base dev --head dev-csv-structure-updates \
     --title "Add new columns and restructure CSV files for v3.0.0" \
     --body "$(cat <<'EOF'
## Summary

Adds new metadata columns to CSV worksheets and reorders columns for better
logical grouping. This is in preparation for the v3.0.0 validation
infrastructure.

## Changes

### variables.csv (6 new columns)
- version: Track variable version numbers
- lastUpdated: Record last modification date
- reviewNotes: Store review comments
- ICES.confirmation: ICES confirmation status
- Observation..MD.: MD observation notes
- status: Variable status (active/deprecated)

### variable_details.csv (6 new columns)
- ICES.confirmation: ICES confirmation status
- version, lastUpdated, status, reviewNotes, review

## Verification

- No content changes - all new columns are empty
- Row counts unchanged: 360 and 3464 rows
- Column ordering improved for logical grouping

## Next Steps

After this PR is merged, content changes from
feature/v3.0.0-validation-infrastructure will be applied in logical,
reviewable groups.
EOF
)"
   ```

10. After approval and merge:
    ```bash
    git checkout dev
    git pull origin dev
    ```

### Phase 3: Apply Content Changes in Logical Groups

**Objective**: Cherry-pick content changes from the feature branch, grouping
them into logical, reviewable commits.

#### Commit Groupings

Based on analysis of the 33 commits, organize changes into these logical
groups:

1. **Infrastructure and Schema (Foundation)**
2. **BMI Variables**
3. **Smoking Variables**
4. **ADL (Activities of Daily Living) Variables**
5. **Oral Health Variables**
6. **Other Health Condition Updates**

#### Steps for Each Group

For each logical group, follow this process:

##### Group 1: Infrastructure and Schema

1. Create a feature branch:
   ```bash
   git checkout dev
   git checkout -b merge-v3-infrastructure-schema
   ```

2. Cherry-pick relevant commits:
   ```bash
   # List of commits to cherry-pick (oldest to newest):
   git cherry-pick 4690c86  # feat: add v2.2.0 variable enhancements
   git cherry-pick 73223df  # feat: enhance variable metadata for v2.2.0
   git cherry-pick 786d8a8  # feat: update metadata and test files
   git cherry-pick 15fc75d  # feat: add comprehensive version validation
   git cherry-pick 26e719f  # feat: implement v3.0.0 validation
   git cherry-pick 9fdd927  # feat: enhance schema validation
   ```

3. Handle conflicts if any (focusing on CSV files):
   - The structural changes are already in place, so conflicts should be
     minimal
   - Accept content changes from the feature branch
   - Preserve the proper formatting (quoting, line endings)

4. Verify changes:
   ```bash
   git diff dev..HEAD -- inst/extdata/variables.csv | head -100
   git diff dev..HEAD -- inst/extdata/variable_details.csv | head -100
   ```

5. Run standardization to ensure formatting consistency:
   ```bash
   python3 quote-empty-cells.py
   git add inst/extdata/variables.csv inst/extdata/variable_details.csv
   git commit --amend --no-edit
   ```

6. Create PR:
   ```bash
   gh pr create --base dev --head merge-v3-infrastructure-schema \
     --title "Infrastructure: v3.0.0 validation infrastructure and schema" \
     --body "$(cat <<'EOF'
## Summary

Implements v3.0.0 validation infrastructure and schema enhancements.

## Changes

This PR includes foundational changes for v3.0.0:
- Enhanced schema validation and data consistency rules
- Version validation and metadata framework
- Metadata enhancements for v2.2.0 variables
- Function renaming updates

## Content Changes

- Variables added/updated: [list specific variables]
- Variable details rows added/updated: [count]

## Cherry-picked Commits

- 4690c86: feat: add v2.2.0 variable enhancements
- 73223df: feat: enhance variable metadata for v2.2.0
- 786d8a8: feat: update metadata and test files for function renaming
- 15fc75d: feat: add comprehensive version validation
- 26e719f: feat: implement v3.0.0 validation infrastructure
- 9fdd927: feat: enhance schema validation

## Test Plan

- [ ] CSV files pass validation
- [ ] All tests pass
- [ ] Documentation builds successfully
EOF
)"
   ```

##### Group 2: BMI Variables

1. Create branch:
   ```bash
   git checkout dev
   git checkout -b merge-v3-bmi-variables
   ```

2. Cherry-pick BMI commits:
   ```bash
   git cherry-pick 227888e  # refactor: modernize BMI function architecture
   git cherry-pick 9dc3958  # feat: restore BMI metadata
   git cherry-pick ab206c1  # feat: Corrected smoking variable mappings
   ```

3. Standardize and commit:
   ```bash
   python3 quote-empty-cells.py
   git add inst/extdata/variables.csv inst/extdata/variable_details.csv
   git commit --amend --no-edit
   ```

4. Create PR:
   ```bash
   gh pr create --base dev --head merge-v3-bmi-variables \
     --title "BMI: Modernize BMI variables and validation" \
     --body "$(cat <<'EOF'
## Summary

Modernizes BMI function architecture and adds range-based BMI categories.

## Changes

- Modernized BMI function architecture and validation approach
- Restored BMI metadata
- Added range-based HWTGBMI_cat4 variable
- Refactored bmi.R constants

## Variables Affected

- HWTGBMI_cat4 (new)
- [List other BMI-related variables updated]

## Cherry-picked Commits

- 227888e: refactor: modernize BMI function architecture
- 9dc3958: feat: restore BMI metadata and add range-based HWTGBMI_cat4
- ab206c1: feat: Corrected smoking variable mappings, refactored bmi.R
EOF
)"
   ```

##### Group 3: Smoking Variables

1. Create branch:
   ```bash
   git checkout dev
   git checkout -b merge-v3-smoking-variables
   ```

2. Cherry-pick smoking commits (oldest to newest):
   ```bash
   git cherry-pick f8f0277  # feat: variable_details.csv added: 4 X SMK
   git cherry-pick 6220e84  # feat: All five smoking status variables
   git cherry-pick 82b9c63  # feat: added smoking initiation functions
   git cherry-pick 57aba9c  # update: variable_details.csv for smoking
   git cherry-pick 801eb97  # updates to the SMK_01B variable details
   git cherry-pick e961c6e  # updates to SMKG01C_A variable
   git cherry-pick 352fe16  # updates to the SMKG01C_B variable
   git cherry-pick 4a25f8a  # updates to the SMKG01C_cont variable
   ```

3. Standardize:
   ```bash
   python3 quote-empty-cells.py
   git add inst/extdata/variables.csv inst/extdata/variable_details.csv
   git commit --amend --no-edit
   ```

4. Create PR:
   ```bash
   gh pr create --base dev --head merge-v3-smoking-variables \
     --title "Smoking: Add continuous smoking variables and update status" \
     --body "$(cat <<'EOF'
## Summary

Adds continuous smoking variables and completes all five smoking status
variables.

## Changes

- Added 4 continuous smoking variables (SMK_*_cont)
- Completed all five smoking status variables
- Added smoking initiation functions
- Updated variable mappings and details

## Variables Added/Updated

- SMK_01B (new row in variables)
- SMKG01C_A (updated)
- SMKG01C_B (updated)
- SMKG01C_cont (updated)
- [List other smoking variables]

## Cherry-picked Commits

- f8f0277: feat: variable_details.csv added: 4 X SMK ... _cont
- 6220e84: feat: All five smoking status variables completed
- 82b9c63: feat: added smoking initiation functions
- 57aba9c: update: variable_details.csv for smoking continuous variable
- 801eb97: updates to the SMK_01B variable details
- e961c6e: updates to SMKG01C_A variable
- 352fe16: updates to the SMKG01C_B variable
- 4a25f8a: updates to the SMKG01C_cont variable
EOF
)"
   ```

##### Group 4: ADL Variables

1. Create branch:
   ```bash
   git checkout dev
   git checkout -b merge-v3-adl-variables
   ```

2. Cherry-pick ADL commits:
   ```bash
   git cherry-pick aeae50e  # fixed bug with ADL_01 variable
   git cherry-pick 35007e4  # updated ADL_der variable
   git cherry-pick 65b417e  # updated ADL_score_5 variable
   ```

3. Standardize:
   ```bash
   python3 quote-empty-cells.py
   git add inst/extdata/variables.csv inst/extdata/variable_details.csv
   git commit --amend --no-edit
   ```

4. Create PR:
   ```bash
   gh pr create --base dev --head merge-v3-adl-variables \
     --title "ADL: Bug fixes and updates to ADL variables" \
     --body "$(cat <<'EOF'
## Summary

Bug fixes and updates to Activities of Daily Living (ADL) variables.

## Changes

- Fixed bug with ADL_01 variable
- Updated ADL_der derived variable
- Updated ADL_score_5 variable

## Variables Updated

- ADL_01 (bug fix)
- ADL_der (updated)
- ADL_score_5 (updated)

## Cherry-picked Commits

- aeae50e: fixed bug with ADL_01 variable
- 35007e4: updated ADL_der variable
- 65b417e: updated ADL_score_5 variable
EOF
)"
   ```

##### Group 5: Oral Health Variables

1. Create branch:
   ```bash
   git checkout dev
   git checkout -b merge-v3-oral-health-variables
   ```

2. Cherry-pick oral health commits (oldest to newest):
   ```bash
   git cherry-pick a562a2a  # new: last time visited dental professional
   git cherry-pick b3ca31e  # new: self-perceived oral health
   git cherry-pick 0220528  # new: frequency teeth/gum pain
   git cherry-pick 9be9b8f  # new: has one or more of own teeth
   git cherry-pick ae18185  # new: frequency of brushing teeth
   ```

3. Standardize:
   ```bash
   python3 quote-empty-cells.py
   git add inst/extdata/variables.csv inst/extdata/variable_details.csv
   git commit --amend --no-edit
   ```

4. Create PR:
   ```bash
   gh pr create --base dev --head merge-v3-oral-health-variables \
     --title "Oral Health: Add five new harmonized oral health variables" \
     --body "$(cat <<'EOF'
## Summary

Adds five new harmonized oral health variables to the repository.

## Changes

Added five new oral health variables:
1. Last time visited dental professional
2. Self-perceived oral health
3. Frequency teeth/gum pain
4. Has one or more of own teeth
5. Frequency of brushing teeth

## Variables Added

- [Variable name 1]
- [Variable name 2]
- [Variable name 3]
- [Variable name 4]
- [Variable name 5]

## Cherry-picked Commits

- a562a2a: new harmonized variable, last time visited dental professional
- b3ca31e: harmonzied new variable, self-perceived oral health
- 0220528: new harmonized variable, frequency teeth/gum pain
- 9be9b8f: new harmonized variable, has one or more of own teeth
- ae18185: new harmonized variable, frequency of brushing teeth
EOF
)"
   ```

##### Group 6: Other Health Conditions

1. Create branch:
   ```bash
   git checkout dev
   git checkout -b merge-v3-health-condition-updates
   ```

2. Cherry-pick remaining commits:
   ```bash
   git cherry-pick e724e82  # updated CCC_181 variable
   ```

3. Standardize:
   ```bash
   python3 quote-empty-cells.py
   git add inst/extdata/variables.csv inst/extdata/variable_details.csv
   git commit --amend --no-edit
   ```

4. Create PR:
   ```bash
   gh pr create --base dev --head merge-v3-health-condition-updates \
     --title "Health Conditions: Update CCC_181 variable" \
     --body "$(cat <<'EOF'
## Summary

Updates to the CCC_181 chronic condition variable.

## Changes

- Updated CCC_181 variable metadata and details

## Variables Updated

- CCC_181

## Cherry-picked Commits

- e724e82: updated CCC_181 variable
EOF
)"
   ```

## Success Criteria

The merge is considered successful when:

1. ✅ All 26 commits from feature branch are accounted for
2. ✅ CSV files in dev have correct structure (16 and 22 columns)
3. ✅ CSV files in dev have correct row counts (379 and 3721)
4. ✅ All tests pass
5. ✅ CSV files are properly formatted (consistent quoting and line endings)
6. ✅ Each PR has been reviewed and approved
7. ✅ Git history is clean with meaningful commit messages
8. ✅ Original commit authorship is preserved
9. ✅ Documentation is updated to reflect new columns
10. ✅ No data loss or corruption

## Rollback Plan

If issues are discovered:

1. **During a PR**: Close the PR, delete the branch, restart from the last
   good state
2. **After a PR merge**: Create a revert PR and restart the affected phase
3. **After multiple PRs**: Consider creating a new integration branch and
   starting over with lessons learned

## Documentation Updates

After successful merge, update these documents:

1. **Data Dictionary**: Document new columns and their purposes
2. **Contributing Guide**: Add CSV formatting requirements
3. **CHANGELOG.md**: Document all changes by group
4. **README.md**: Update version information if applicable

## Appendix A: Commit Inventory

### Infrastructure and Schema (6 commits)
- 4690c86: feat: add v2.2.0 variable enhancements
- 73223df: feat: enhance variable metadata for v2.2.0
- 786d8a8: feat: update metadata and test files for function renaming
- 15fc75d: feat: add comprehensive version validation and metadata updates
- 26e719f: feat: implement v3.0.0 validation infrastructure and modernize
  derived variables
- 9fdd927: feat: enhance schema validation and data consistency rules

### BMI Variables (3 commits)
- 227888e: refactor: modernize BMI function architecture and validation
  approach
- 9dc3958: feat: restore BMI metadata and add range-based HWTGBMI_cat4
- ab206c1: feat: Corrected smoking variable mappings, refactored bmi.R
  constants

### Smoking Variables (8 commits)
- f8f0277: feat: variable_details.csv added: 4 X SMK ... _cont
- 6220e84: feat: All five smoking status variables completed
- 82b9c63: feat: added smoking initiation functions and update
  variable_details
- 57aba9c: update: variable_details.csv for smoking continuous variable
- 801eb97: updates to the SMK_01B variable details. added row for it to
  variables sheet
- e961c6e: updates to SMKG01C_A variable
- 352fe16: updates to the SMKG01C_B variable
- 4a25f8a: updates to the SMKG01C_cont variable

### ADL Variables (3 commits)
- aeae50e: fixed bug with ADL_01 variable
- 35007e4: updated ADL_der variable
- 65b417e: updated ADL_score_5 variable

### Oral Health Variables (5 commits)
- a562a2a: new harmonized variable, last time visited dental professional
- b3ca31e: harmonzied new variable, self-perceived oral health
- 0220528: new harmonized variable, frequency teeth/gum pain
- 9be9b8f: new harmonized variable, has one or more of own teeth
- ae18185: harmonized new variable, frequency of brushing teeth

### Other Health Conditions (1 commit)
- e724e82: updated CCC_181 variable

**Total: 26 commits identified**

Note: 7 commits from the original 33 may affect other files in
`inst/extdata/` or may be merge commits. Verify with:
```bash
git log --oneline feature/v3.0.0-validation-infrastructure --not dev \
  -- inst/extdata/ | wc -l
```

## Appendix B: Column Mapping

### variables.csv

**Dev columns (10):**
```
variable, label, labelLong, section, subject, variableType,
units, databaseStart, variableStart, description
```

**Feature branch columns (16):**
```
variable, label, labelLong, variableType, databaseStart, variableStart,
subject, section, units, description, version, lastUpdated,
reviewNotes, ICES.confirmation, Observation..MD., status
```

**Changes:**
- Column order: `section` and `subject` swapped positions
- 6 new columns added at the end

### variable_details.csv

**Dev columns (16):**
```
variable, dummyVariable, typeEnd, databaseStart, variableStart, typeStart,
recEnd, numValidCat, catLabel, catLabelLong, units, recStart,
catStartLabel, variableStartShortLabel, variableStartLabel, notes
```

**Feature branch columns (22):**
```
variable, dummyVariable, typeEnd, databaseStart, variableStart,
ICES.confirmation, typeStart, recEnd, numValidCat, catLabel,
catLabelLong, units, recStart, catStartLabel, variableStartShortLabel,
variableStartLabel, notes, version, lastUpdated, status, reviewNotes, review
```

**Changes:**
- `ICES.confirmation` inserted at position 6
- 5 additional columns added at the end

## Appendix C: Pre-commit Hook (Optional)

To enforce CSV formatting automatically:

Create `.git/hooks/pre-commit`:
```bash
#!/bin/bash

# Check if CSV files are being committed
csv_files=$(git diff --cached --name-only --diff-filter=ACM | \
  grep "inst/extdata/.*\.csv$")

if [ -n "$csv_files" ]; then
    echo "CSV files detected in commit. Running standardization..."
    python3 quote-empty-cells.py

    # Re-add the files
    for file in $csv_files; do
        git add "$file"
    done

    echo "CSV files standardized and re-staged."
fi

exit 0
```

Make it executable:
```bash
chmod +x .git/hooks/pre-commit
```

---

**Document Version**: 1.0
**Last Updated**: 2025-11-14
**Author**: Strategy developed with Claude Code
**Status**: Ready for implementation
