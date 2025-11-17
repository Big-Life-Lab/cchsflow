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

1. Convert Python tooling to R for consistency with project language
2. Standardize formatting on dev first
3. Apply structural changes separately from content changes
4. Group content changes into logical, reviewable commits
5. Preserve original commit metadata (authors, dates)
6. Create intermediate branches for review

### Phase 0: Convert Python Script to R

**Objective**: Convert the `quote-empty-cells.py` standardization script from
Python to R to maintain consistency with the project's primary language.

**Steps**:

1. Create an R version of the standardization script (`standardize_csvs.R`):
   ```r
   # standardize_csvs.R
   library(readr)
   library(dplyr)

   standardize_variables <- function(
     input_path = "inst/extdata/variables.csv",
     output_path = "inst/extdata/variables.csv") {
     # Read CSV
     df <- read_csv(input_path, show_col_types = FALSE)

     # Remove any empty columns
     df <- df %>% select(where(~ !all(is.na(.) | . == "")))

     # Write with all fields quoted and LF line endings
     write_csv(df, output_path, quote = "all", eol = "\n")
     message("Standardized variables.csv")
   }

   standardize_variable_details <- function(
     input_path = "inst/extdata/variable_details.csv",
     output_path = "inst/extdata/variable_details.csv") {
     # Read CSV
     df <- read_csv(input_path, show_col_types = FALSE)

     # Remove any empty columns
     df <- df %>% select(where(~ !all(is.na(.) | . == "")))

     # Write with minimal quoting and CRLF line endings
     write_csv(df, output_path, quote = "needed", eol = "\r\n")
     message("Standardized variable_details.csv")
   }

   # Main execution
   standardize_variables()
   standardize_variable_details()
   ```

2. Test the R script:
   ```bash
   # Create a test branch
   git checkout -b test-r-standardization

   # Run the R script
   Rscript standardize_csvs.R

   # Verify the output is correct
   git diff inst/extdata/

   # Clean up test branch
   git checkout dev
   git branch -D test-r-standardization
   ```

3. Add the R script to the repository:
   ```bash
   git add standardize_csvs.R
   git commit -m "Add R version of CSV standardization script

   Provides native R implementation of CSV standardization to maintain
   consistency with project language. Replaces quote-empty-cells.py.

   Features:
   - Standardizes variables.csv with all fields quoted and LF line endings
   - Standardizes variable_details.csv with minimal quoting and CRLF
   - Removes empty columns
   - Uses readr for consistent CSV handling"
   ```

4. Update documentation to reference `standardize_csvs.R` instead of
   `quote-empty-cells.py` in all subsequent phases

**Note**: The original Python script (`quote-empty-cells.py`) can be kept for
reference but all subsequent phases will use `standardize_csvs.R`.

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

2. Apply the R standardization script to dev:
   ```bash
   # The script is already configured correctly
   Rscript standardize_csvs.R
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

This standardization is done using standardize_csvs.R to ensure
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

Standardizes CSV file formatting using the standardize_csvs.R script.

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

### Phase 3: Apply Structural Changes

**Objective**: Add new columns in multiple commits, filling values from the
feature branch for existing dev rows. This approach makes changes reviewable
and preserves data integrity.

**Overview**: Structural changes are broken into 7 separate commits to make
each change reviewable. Each commit adds specific columns and fills values
for existing rows from the latest commits in the feature branch.

#### Step 1: Identify Source Commits

First, identify the latest commits in the feature branch that contain the
column definitions and values:

```bash
# View recent commits affecting CSV files
git log feature/v3.0.0-validation-infrastructure --oneline -20 \
  -- inst/extdata/variables.csv inst/extdata/variable_details.csv

# The latest commits contain the most accurate column values
```

**Note**: Use the **latest** commits from the feature branch (not the
earliest) as they contain the most up-to-date and accurate values.

#### Step 2: Create Branch and Prepare

```bash
git checkout dev
git pull origin dev
git checkout -b dev-csv-structure-updates
```

#### Step 3: Add Columns with Values (7 Commits)

**Commit 1: Add version, lastUpdated, status to variables.csv**

```bash
# Extract these columns from feature branch for matching rows
git show feature/v3.0.0-validation-infrastructure:inst/extdata/variables.csv \
  > /tmp/feature_vars.csv

# Use R to merge the columns with values:
Rscript -e "
library(readr)
library(dplyr)

# Read both versions
dev <- read_csv('inst/extdata/variables.csv', show_col_types = FALSE)
feature <- read_csv('/tmp/feature_vars.csv', show_col_types = FALSE)

# Select only the new columns from feature for matching variables
feature_subset <- feature %>%
  select(variable, version, lastUpdated, status)

# Left join to add columns with values for existing rows
result <- dev %>%
  left_join(feature_subset, by = 'variable') %>%
  mutate(across(c(version, lastUpdated, status),
                ~if_else(is.na(.), '', as.character(.))))

# Reorder columns (add new ones after description)
col_order <- c('variable', 'label', 'labelLong', 'variableType',
               'databaseStart', 'variableStart', 'subject', 'section',
               'units', 'description', 'version', 'lastUpdated', 'status')
result <- result %>% select(all_of(col_order))

# Write with standardization
write_csv(result, 'inst/extdata/variables.csv', quote = 'all', eol = '\n')
"

# Run standardization
Rscript standardize_csvs.R

# Commit
git add inst/extdata/variables.csv
git commit -m "feat: add version, lastUpdated, and status columns to variables.csv

Add three metadata columns with values from feature branch:
- version: Variable version numbers
- lastUpdated: Last modification dates
- status: Variable status (active/deprecated)

Values filled for all existing rows based on latest feature branch state.
Row count unchanged: 360 rows."
```

**Commit 2: Add reviewNotes to variables.csv**

```bash
# Similar approach - extract and merge reviewNotes
Rscript -e "
library(readr)
library(dplyr)

dev <- read_csv('inst/extdata/variables.csv', show_col_types = FALSE)
feature <- read_csv('/tmp/feature_vars.csv', show_col_types = FALSE)

feature_subset <- feature %>% select(variable, reviewNotes)

result <- dev %>%
  left_join(feature_subset, by = 'variable') %>%
  mutate(reviewNotes = if_else(is.na(reviewNotes), '', as.character(reviewNotes)))

# Reorder: insert reviewNotes after status
col_order <- c('variable', 'label', 'labelLong', 'variableType',
               'databaseStart', 'variableStart', 'subject', 'section',
               'units', 'description', 'version', 'lastUpdated', 'status',
               'reviewNotes')
result <- result %>% select(all_of(col_order))

write_csv(result, 'inst/extdata/variables.csv', quote = 'all', eol = '\n')
"

Rscript standardize_csvs.R

git add inst/extdata/variables.csv
git commit -m "feat: add reviewNotes column to variables.csv

Add reviewNotes column with values from feature branch for storing
review comments and notes.

Values filled for all existing rows.
Row count unchanged: 360 rows."
```

**Commit 3: Add ICES.confirmation to variables.csv**

```bash
Rscript -e "
library(readr)
library(dplyr)

dev <- read_csv('inst/extdata/variables.csv', show_col_types = FALSE)
feature <- read_csv('/tmp/feature_vars.csv', show_col_types = FALSE)

feature_subset <- feature %>% select(variable, ICES.confirmation)

result <- dev %>%
  left_join(feature_subset, by = 'variable') %>%
  mutate(ICES.confirmation = if_else(is.na(ICES.confirmation), '',
                                     as.character(ICES.confirmation)))

col_order <- c('variable', 'label', 'labelLong', 'variableType',
               'databaseStart', 'variableStart', 'subject', 'section',
               'units', 'description', 'version', 'lastUpdated', 'status',
               'reviewNotes', 'ICES.confirmation')
result <- result %>% select(all_of(col_order))

write_csv(result, 'inst/extdata/variables.csv', quote = 'all', eol = '\n')
"

Rscript standardize_csvs.R

git add inst/extdata/variables.csv
git commit -m "feat: add ICES.confirmation column to variables.csv

Add ICES.confirmation column with values from feature branch for
tracking ICES confirmation status.

Values filled for all existing rows.
Row count unchanged: 360 rows."
```

**Commit 4: Add Observation..MD. to variables.csv**

```bash
Rscript -e "
library(readr)
library(dplyr)

dev <- read_csv('inst/extdata/variables.csv', show_col_types = FALSE)
feature <- read_csv('/tmp/feature_vars.csv', show_col_types = FALSE)

feature_subset <- feature %>% select(variable, Observation..MD.)

result <- dev %>%
  left_join(feature_subset, by = 'variable') %>%
  mutate(Observation..MD. = if_else(is.na(Observation..MD.), '',
                                    as.character(Observation..MD.)))

col_order <- c('variable', 'label', 'labelLong', 'variableType',
               'databaseStart', 'variableStart', 'subject', 'section',
               'units', 'description', 'version', 'lastUpdated', 'status',
               'reviewNotes', 'ICES.confirmation', 'Observation..MD.')
result <- result %>% select(all_of(col_order))

write_csv(result, 'inst/extdata/variables.csv', quote = 'all', eol = '\n')
"

Rscript standardize_csvs.R

git add inst/extdata/variables.csv
git commit -m "feat: add Observation..MD. column to variables.csv

Add Observation..MD. column with values from feature branch for
MD observation notes.

Values filled for all existing rows.
Row count unchanged: 360 rows."
```

**Commit 5: Add version, lastUpdated, status to variable_details.csv**

```bash
# Extract from feature branch
git show feature/v3.0.0-validation-infrastructure:inst/extdata/variable_details.csv \
  > /tmp/feature_details.csv

Rscript -e "
library(readr)
library(dplyr)

dev <- read_csv('inst/extdata/variable_details.csv', show_col_types = FALSE)
feature <- read_csv('/tmp/feature_details.csv', show_col_types = FALSE)

feature_subset <- feature %>%
  select(variable, databaseStart, variableStart, version, lastUpdated, status)

result <- dev %>%
  left_join(feature_subset, by = c('variable', 'databaseStart', 'variableStart')) %>%
  mutate(across(c(version, lastUpdated, status),
                ~if_else(is.na(.), '', as.character(.))))

# Reorder columns (add after notes)
col_order <- c('variable', 'dummyVariable', 'typeEnd', 'databaseStart',
               'variableStart', 'typeStart', 'recEnd', 'numValidCat',
               'catLabel', 'catLabelLong', 'units', 'recStart',
               'catStartLabel', 'variableStartShortLabel',
               'variableStartLabel', 'notes', 'version', 'lastUpdated',
               'status')
result <- result %>% select(all_of(col_order))

write_csv(result, 'inst/extdata/variable_details.csv', quote = 'needed',
          eol = '\r\n')
"

Rscript standardize_csvs.R

git add inst/extdata/variable_details.csv
git commit -m "feat: add version, lastUpdated, status to variable_details.csv

Add three metadata columns with values from feature branch.

Values filled for all existing rows.
Row count unchanged: 3464 rows."
```

**Commit 6: Add reviewNotes and review to variable_details.csv**

```bash
Rscript -e "
library(readr)
library(dplyr)

dev <- read_csv('inst/extdata/variable_details.csv', show_col_types = FALSE)
feature <- read_csv('/tmp/feature_details.csv', show_col_types = FALSE)

feature_subset <- feature %>%
  select(variable, databaseStart, variableStart, reviewNotes, review)

result <- dev %>%
  left_join(feature_subset, by = c('variable', 'databaseStart', 'variableStart')) %>%
  mutate(across(c(reviewNotes, review),
                ~if_else(is.na(.), '', as.character(.))))

col_order <- c('variable', 'dummyVariable', 'typeEnd', 'databaseStart',
               'variableStart', 'typeStart', 'recEnd', 'numValidCat',
               'catLabel', 'catLabelLong', 'units', 'recStart',
               'catStartLabel', 'variableStartShortLabel',
               'variableStartLabel', 'notes', 'version', 'lastUpdated',
               'status', 'reviewNotes', 'review')
result <- result %>% select(all_of(col_order))

write_csv(result, 'inst/extdata/variable_details.csv', quote = 'needed',
          eol = '\r\n')
"

Rscript standardize_csvs.R

git add inst/extdata/variable_details.csv
git commit -m "feat: add reviewNotes and review to variable_details.csv

Add two review-related columns with values from feature branch.

Values filled for all existing rows.
Row count unchanged: 3464 rows."
```

**Commit 7: Add ICES.confirmation to variable_details.csv**

```bash
Rscript -e "
library(readr)
library(dplyr)

dev <- read_csv('inst/extdata/variable_details.csv', show_col_types = FALSE)
feature <- read_csv('/tmp/feature_details.csv', show_col_types = FALSE)

feature_subset <- feature %>%
  select(variable, databaseStart, variableStart, ICES.confirmation)

result <- dev %>%
  left_join(feature_subset, by = c('variable', 'databaseStart', 'variableStart')) %>%
  mutate(ICES.confirmation = if_else(is.na(ICES.confirmation), '',
                                     as.character(ICES.confirmation)))

# Insert ICES.confirmation after variableStart (before typeStart)
col_order <- c('variable', 'dummyVariable', 'typeEnd', 'databaseStart',
               'variableStart', 'ICES.confirmation', 'typeStart', 'recEnd',
               'numValidCat', 'catLabel', 'catLabelLong', 'units',
               'recStart', 'catStartLabel', 'variableStartShortLabel',
               'variableStartLabel', 'notes', 'version', 'lastUpdated',
               'status', 'reviewNotes', 'review')
result <- result %>% select(all_of(col_order))

write_csv(result, 'inst/extdata/variable_details.csv', quote = 'needed',
          eol = '\r\n')
"

Rscript standardize_csvs.R

git add inst/extdata/variable_details.csv
git commit -m "feat: add ICES.confirmation to variable_details.csv

Add ICES.confirmation column with values from feature branch, inserted
after variableStart for logical grouping.

Values filled for all existing rows.
Row count unchanged: 3464 rows."
```

#### Step 4: Create PR

```bash
gh pr create --base dev --head dev-csv-structure-updates \
  --title "Add new metadata columns to CSV files (7 commits)" \
  --body "$(cat <<'EOF'
## Summary

Adds new metadata columns to CSV worksheets in 7 separate commits for
reviewability. Each commit adds specific columns with values filled from
the latest feature branch state.

## Changes

### variables.csv (6 new columns across 4 commits)
1. version, lastUpdated, status
2. reviewNotes
3. ICES.confirmation
4. Observation..MD.

### variable_details.csv (6 new columns across 3 commits)
1. version, lastUpdated, status
2. reviewNotes, review
3. ICES.confirmation

## Key Points

- All values filled from latest feature branch commits
- Row counts unchanged: 360 and 3464 rows
- Each commit is independently reviewable
- Columns reordered for logical grouping

## Verification

```bash
wc -l inst/extdata/*.csv  # Should show 361 and 3465 (with headers)
```
EOF
)"
```

#### Step 5: After Merge

```bash
git checkout dev
git pull origin dev
```

### Phase 4: Apply Content Changes in Logical Groups

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
   Rscript standardize_csvs.R
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
   Rscript standardize_csvs.R
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
   Rscript standardize_csvs.R
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
   Rscript standardize_csvs.R
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
   Rscript standardize_csvs.R
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
   Rscript standardize_csvs.R
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
    Rscript standardize_csvs.R

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
