# Evidence-Grounding Review: v4 Planning Documents

**Reviewer lens:** Evidence grounding  
**Date:** 2026-06-12  
**Documents reviewed:**
- `/tmp/v4-research/review-targets/reunification.md` (engine comparison + recommendation)
- `/tmp/v4-research/review-targets/requirements.md` (consolidated v4 requirements, 7 tracks)
- `/tmp/v4-research/review-targets/inventory.md` (design-issues inventory)  
**Evidence base consulted:** all `inventory-*.json`, `engines-*.md`, `bench-*.md`, `skeptic-*.json`, `verify-*.json`, `engines-schema-diff.md`, plus direct code inspection of cchsflow `fix/v3-smoking-worksheet-sync` and recodeflow `dev` branch.

---

## Executive summary

The three planning documents are grounded in real evidence and the large majority of
factual claims check out against their sources. Ten or more spot-checks on load-bearing
claims (bug reproductions, line counts, parameter counts, worksheet data, package
metadata) confirm that the evidence program was thorough and mostly accurately
represented. However, six specific categories of evidence-grounding problems were found:
four are count / numeric discrepancies, one is an overstated similarity claim between the
two codebases, and one is a significant currency problem where a critical finding is
described as unfixed when it was already fixed in the same branch.

---

## Finding 1 (important): List-mode bug described as unfixed, but already fixed in this branch

**Claim:** inventory.md headline finding #1 and the "Fast fixes" table describe the
list-mode `rec_with_table()` bug as a current defect requiring a fix. The fast-fixes
table entry reads: "List-mode loop bug: pass `data_name`, not `database_name`".

**Evidence contradiction:** Commit `63450ba3` (June 11, 2026, same day as the inventory
date) fixes exactly this bug. The current branch (`fix/v3-smoking-worksheet-sync`) code
at `R/recode-with-table.R:195` already passes `database_name = data_name` (the loop
variable). The commit message: "Fix list-mode rec_with_table applying first database's
rules to all databases." Running `git log --oneline --follow R/recode-with-table.R`
confirms this.

**Impact:** The inventory and requirements present this as an outstanding fix, but it is
already done. The fast-fixes table entry is stale. Requirements Track 1 says "fix
shared defects once in recodeflow" -- but the list-mode bug is already fixed in cchsflow
and must be carried forward intentionally, not treated as unfixed.

**Suggestion:** Update the inventory fast-fixes table to mark list-mode as "fixed in
63450ba3". Track 1 should reference the fix and require it be ported to recodeflow's
shared engine rather than implying it has not yet been addressed.

---

## Finding 2 (important): "byte-identical" claim overstated

**Claim:** reunification.md states: "the NA-formatting function is byte-identical in
both repos."

**Evidence contradiction:** The engines-core-diff.md (the cited source) says only
"Functionally identical" at section 2I. Direct comparison of the functions shows they
differ in: (a) function name (`recode_variable_NA_formating` vs `format_recoded_value`),
(b) namespace references (`pkg.globals$argument.CatType` vs `pkg.env$columns.value.catType`),
and (c) qualified vs unqualified call (`haven::tagged_na()` vs `tagged_na()`). The logic
is equivalent but the two functions are not byte-identical.

**Impact:** Minor for the design argument (functional identity is what matters for the
shared-defect claim), but the word "byte-identical" will look wrong to anyone who reads
both files.

**Suggestion:** Replace "byte-identical" with "functionally equivalent" to match the
source evidence.

---

## Finding 3 (important): Severity count discrepancies in per-dimension summaries

**Claim:** inventory.md per-dimension summaries state specific counts. Four of six
original dimensions have counts that do not match their source JSON files.

| Dimension | Claimed | Actual (from JSON) | Discrepancy |
|---|---|---|---|
| Engine internals | 4h / 11m / 6l | 4h / 10m / 7l | 1 medium → low |
| Labels/metadata | 4h / 6m / 1l | 4h / 5m / 2l | 1 medium → low |
| API surface/UX | 15 total, 7h / 7m / 1l | **12 total**, 6h / 5m / 1l | 3 items and multiple sev missing |
| Data artifacts | 10 total, 4h / 4m / 2l | **11 total**, 3h / 7m / 1l | total off; high→medium shift |

(Missing data architecture: 9 / 5h / 4m -- confirmed correct. Dependencies: 12 / 3h / 6m / 3l -- confirmed correct.)

**Evidence:** `inventory-api-surface-ux.json` has 12 issues (not 15); the JSON summary
itself says "4 high, 9 medium, 8 low" for engine internals while the issues list
yields 4h/10m/7l.

**Impact:** The api-surface discrepancy is the most consequential: the claimed 15 issues
with 7 high is the basis for prioritizing that dimension, but the JSON holds 12. The
totals affect the "78 confirmed-style findings" header claim (see Finding 4).

**Suggestion:** Re-run the count script against the final JSON files and update the per-
dimension tables in inventory.md before distribution.

---

## Finding 4 (important): "78 confirmed-style findings" count is stale

**Claim:** inventory.md header states: "78 confirmed-style findings across six dimensions."

**Evidence contradiction:** The six original inventory JSON files sum to 76 issues (21 + 9 + 11 + 12 + 12 + 11 = 76), not 78. Additionally, the coverage section notes that four more dimensions were added on June 11 (worksheet-schema: 19, testing-ci: 9, docs-vignettes: 13, issues-triage: 14 = 55 additional), bringing the full program to 131 issues across ten dimensions. The "78 across six" figure is stale relative to the final evidence program in two ways: wrong total for the six, and the six is no longer the full count.

**Impact:** Minor for design decisions (the findings themselves are sound) but the header
claim is numerically wrong and will be confusing to readers who count the files.

**Suggestion:** Update the header to "76 confirmed findings across six original
dimensions; the complete ten-dimension program covers 131 findings" (or similar that
reflects the June 11 additions accurately).

---

## Finding 5 (minor): "verification-verdicts.json" cited but does not exist

**Claim:** inventory.md "Coverage and verification status" states: "41 verdicts
(`evidence/verification-verdicts.json`): 38 findings confirmed real, 3 refuted and
removed."

**Evidence contradiction:** `verification-verdicts.json` does not exist in
`/tmp/v4-research/`. The verdicts exist as 41 separate `verify-*.json` files (35
`verify-salvaged-*.json` + 3 `verify-worksheet-schema-*.json` + 3
`verify-issues-triage-*.json`). All 35 salvaged-finding verifications have `isReal:
true`; none have `isReal: false`. The three "refuted" items are the three that have
`isReal: null` (the YAML-nesting crash claim, the DerivedVar-silently-ignored claim, and
the haven_labelled framing of #159) -- they were found to be incorrect claims but the
JSON field was left null rather than set to false, making programmatic counting ambiguous.
The 38-confirmed/3-refuted tally is approximately correct but the cited file is absent.

**Impact:** Traceability gap. Readers following the citation `evidence/verification-verdicts.json` will find nothing.

**Suggestion:** Either create a `verification-verdicts.json` aggregating the 41 individual
files, or change the citation to "see individual `verify-*.json` files."

---

## Finding 6 (minor): recodeflow parameter count off by one in engines-core-diff.md

**Claim:** engines-core-diff.md section 2A table states recodeflow has "14 params
including `tables`, `id_role_name`, `name_of_environment_to_load`, `append_non_db_columns`"
and cchsflow has "12 params."

**Evidence contradiction:** Counting the actual function signatures:
- recodeflow `rec_with_table()` has 15 parameters: `data, variables, database_name,
  variable_details, else_value, append_to_data, log, notes, var_labels,
  custom_function_path, attach_data_name, id_role_name, name_of_environment_to_load,
  append_non_db_columns, tables`.
- cchsflow `rec_with_table()` has 11 parameters (not 12): `data, variables,
  database_name, variable_details, else_value, append_to_data, log, notes, var_labels,
  custom_function_path, attach_data_name`.

The inventory.md (API section) correctly states "11 parameters." The engines-core-diff.md
is off by 1 in both directions.

**Impact:** Minor -- the structural comparison is correct; only the counts need correction.

**Suggestion:** Correct the table in engines-core-diff.md to 15 / 11.

---

## Finding 7 (minor): "419-variable CCHS content" is 418 variables

**Claim:** reunification.md and features.md both state "419-variable CCHS content."

**Evidence contradiction:** `inst/extdata/variables.csv` has 419 lines including the
header row, meaning 418 data rows (418 variables).

**Impact:** Off-by-one. The inventory and schema-diff say "~419 rows" (qualified with
~) or "418"; only the two planning documents say "419-variable."

**Suggestion:** Use "418 variables" for precision, or "~419" to match the approximate
phrasing in the evidence files.

---

## Finding 8 (minor): "GPL-3 caution" misapplied to pointblank in requirements.md Track 7

**Claim:** requirements.md Track 7 item 3 says: "imitate pointblank's informant concept...
and validate's rules-as-objects pattern for step-3 output specs -- imitate, not adopt
(GPL-3 caution; both verdicts in evidence/bench-dictionaries-validation.md)."

**Evidence contradiction:** pointblank is MIT-licensed (confirmed in both
bench-dictionaries-validation.md and skeptic-dictionaries-validation.json). The GPL-3
caution applies only to validate. The bench document's recommendation for pointblank is
"Wrap (informant only)" based on MIT license and active maintenance, not "imitate, not
adopt." The requirements conflate both under the same caution.

**Impact:** Minor in practice (both are recommended to imitate rather than import as
dependencies). However, a reader considering wrapping pointblank's informant directly
(which MIT permits) would be incorrectly deterred by the GPL-3 framing.

**Suggestion:** Separate the two verdicts: "imitate validate's rules-as-objects pattern
(GPL-3 caution); pointblank's informant concept is MIT and could be wrapped directly if
interactive HTML QA reports become a v4 deliverable."

---

## Confirmed claims (spot-checked; no discrepancy found)

The following load-bearing claims were verified directly against source code or evidence
files and found correct:

1. **DEN_132 databaseStart tokens missing underscore** (`cchs2007_2008p` /
   `cchs2007_2008m`): confirmed in `inst/extdata/variable_details.csv` (14 rows with
   this token).

2. **14 production rows use `[7,8,9]` set syntax** (all HUI_0x variables): confirmed by
   Python scan of variable_details.csv.

3. **Pack years status-3 formula has no pmax guard** (issue #138): confirmed at
   `R/smoke-pack-years.R:301-302`; `(age - age_first_cig)` has no `pmax(..., 0)`.

4. **Immigration bug** (issue #139): non-immigrants born outside Canada
   (`immigrant_status==2, born_canada==2`) fall through to `TRUE ~ tagged_na("b")`;
   confirmed in `R/immigration.R`.

5. **91 `stop()` calls** in the R/ directory: confirmed by `grep` count.

6. **18 functions with `stop("DOCUMENTATION ONLY...")` body**: confirmed (22 lines
   total, 4 are comments).

7. **All 9 runtime packages in Depends, no Imports field**: confirmed in DESCRIPTION.

8. **~2,400 lines for missing-data architecture**: confirmed (830 + 492 + 1077 = 2,399
   lines across the three files).

9. **recodeflow exports 6 symbols**: confirmed from recodeflow NAMESPACE.

10. **cchsflow exports 125 symbols**: confirmed from cchsflow NAMESPACE.

11. **missing_priority_rules.yaml does not exist in `inst/`**: confirmed; the file is on
    the `3-step-tidyverse` branch (commit `370b673b`) only, never merged.

12. **Built-in priority fallback `na_a=1`** (Not Applicable wins): confirmed at
    `R/missing-data-functions.R:198`.

13. **`get_missing_pattern_auto()` calls `load_database_config()`** which is never
    defined: confirmed; no `load_database_config` function definition exists anywhere in
    `R/`.

14. **`verification-verdicts.json` citation**: file does not exist (confirmed by
    directory listing). Individual verify files total 41, matching the claimed count.

15. **recodeflow PR #85 (`parse_variables_sheet`) merged 2025-12-30**: confirmed from
    git log on recodeflow dev branch.

16. **`pbc_metadata.yaml` follows Dublin Core format**: confirmed in
    `recodeflow/inst/extdata/pbc_metadata.yaml`.

17. **`data-raw/` directory does not exist**: confirmed.

18. **The 84 string constants / ~47-53 dead bllflow leftovers in strings.R**: confirmed
    (grep finds 49 bllFlow/tableOne/LongTable/WorkingData/FunctionList references in
    strings.R; 82 total constant assignments).

---

## Note on an error in the concurrent completeness-critic review

The `review-completeness-critic.md` file contains an internal inconsistency worth
flagging: its Gap 1 (testing) cites the claim that "`check_worksheet()` crashes on every
`variable_details` sheet (purrr::keep error on `logical(0)`)" as a high-severity
confirmed finding. This claim was explicitly refuted in `verify-worksheet-schema-0.json`:
"Claim that check_worksheet crashes is false: it runs without error, returning 4
unrelated formatting errors." The completeness-critic review repeats the refuted claim
as though it were confirmed evidence. This does not affect the documents under review here
but should be corrected in that companion review.

---

## Summary table

| # | Severity | Location | Claim | Contradiction |
|---|---|---|---|---|
| 1 | important | inventory.md headline #1 + fast-fixes; requirements.md staging | List-mode bug "requires fix" | Fixed in commit 63450ba3 on this branch |
| 2 | important | reunification.md | NA-formatting function "byte-identical" | Evidence source says "functionally identical"; differs in name, namespace, qualified call |
| 3 | important | inventory.md per-dimension summaries | Severity counts for engine/labels/api/data-artifacts | JSON files yield different counts; api-surface total 12 not 15 |
| 4 | important | inventory.md header | "78 confirmed-style findings across six dimensions" | Six dimensions sum to 76; ten-dimension program totals 131 |
| 5 | minor | inventory.md coverage section | `verification-verdicts.json` cited | File does not exist; verdicts in 41 separate files |
| 6 | minor | engines-core-diff.md §2A | recodeflow 14 params / cchsflow 12 params | Actually 15 / 11 |
| 7 | minor | reunification.md; features.md | "419-variable CCHS content" | 418 variables (419 lines including header) |
| 8 | minor | requirements.md Track 7 | GPL-3 caution applied to both pointblank and validate | pointblank is MIT-licensed; GPL-3 applies only to validate |

