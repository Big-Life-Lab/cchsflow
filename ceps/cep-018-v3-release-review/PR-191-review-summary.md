# PR #191 Review Summary -- cchsflow v3 Release (v3 -> dev)

**Date:** July 15, 2026
**Scope:** Release-wide. Automated L3-L5 sweeps over all 481 changed
variables (120 added, 41 removed, 320 modified vs dev); semantic depth and
L6 integration sampled across the changed domains; package health checks.
Review method: cchsflow-review skill (CEP/L0-L6 process).

## Triage

- 100 commits, 443 files, +52,383/-9,858; worksheets churn 4,143+/3,464-
  (variable_details) and 441+/360- (variables).
- Removed variables are the deprecated era-split names (`_A`/`_B`:
  ADL_0X_A, DHHGAGE_A-D, CCC_102_A/B, SDCG*_A) per the rename policy.
- Cross-file integrity: zero variable_details entries without a
  variables.csv row.
- R-CMD-check: PASS. CSV-format check: FAIL -- diagnosed below as a
  workflow bug, not worksheet violations.

## Findings (confidence-scored; threshold 80 for blocking)

### 1. CI "CSV formatting" failure is a workflow bug, not worksheet violations [100]

`exec/check-worksheets.R` calls `devtools::load_all()` when run from a
repo checkout, but the check-csv workflow never installs devtools. The
job dies on `there is no package called 'devtools'` and a catch-all step
then prints the misleading "CSV formatting violations detected" message.
The worksheets themselves pass cleanly (local run on the same commit
91654679: exit 0, zero violations, all three checks green).

Fix (S): install devtools in the workflow (or make the script fall back
to `pkgload`/installed package), and change the catch-all message to
surface the actual failure. The red check must be green before merge.

### 2. Issue #139 (immigration misclassification) remains unfixed [90]

`categorize_immigration()`'s case_when has arms for
`immigrant_status == 1 & born_canada == 2` and
`immigrant_status == 2 & born_canada == 1` but none for non-immigrants
born outside Canada (`immigrant_status == 2 & born_canada == 2`), who
fall through to missing. Confirmed by code read (R/immigration.R
case_when block); flagged and verified in the June CEP-017 evidence
sweep. Fix recipe: two additional arms before the NA-propagation block.

### 3. Missing-data priority YAML still unshipped [75 -- release-notes item]

`missing_priority_rules.yaml` is absent from `inst/metadata/schemas/`;
the built-in fallback gives "Not Applicable wins" while the never-merged
config on 3-step-tidyverse specified "Not Stated wins". Live behaviour
verified this review: NA::a wins. Not a code defect, but the
methodological decision is undocumented and CEP-017's requirements place
the decision + YAML in the v3.x window. Recommend deciding before or at
release and characterizing it in NEWS as methodological, per CEP-017.

### 4. DEN_132 claims cchs2001_p coverage but shows 0% valid in the 2001 sample [50 -- verify, informational]

The L6 matrix shows DEN_132 0.0% valid in cchs2001_p while databaseStart
claims it. May be optional-content/sample limitation (200-row samples);
cross-check against CEP-006 (oral health) / source documentation before
merge. DEN_132 also lists single-year tokens (cchs2010_p, cchs2012_p,
cchs2014_p) -- confirm intentional.

### Informational (score 0 or explained)

- **ADL step change at 2011 is a survey redesign, correctly handled.**
  ADL_der drops from ~100% valid (2001-2008, RAC-module source asked
  broadly) to 8-40% (2011+, gated ADL module: most respondents receive
  code 6 -> NA(a)). Diagnosis confirmed source-level: raw 2011+ data is
  dominated by code 6; the recode produces NA(a), no NA(b) floods, no
  errors. The 2015+ four-category format recodes correctly ([1,2]->2,
  [3,4]->1). Function docs already caution about applicability gating;
  consider quantifying the pre/post-2011 comparability break in the
  ADL documentation.
- **Six pre-existing `_A`/`_B` names remain** (INCGHH_A/B, INCGPER_A/B,
  INJ_05_A, RACG5_A) -- present on dev, not PR-introduced; follow-up
  under the rename-when-touched policy.
- **Era-boundary source-mapping check: clean** (0 variables with claimed
  databases lacking a source mapping). **databaseStart token sweep:
  clean** (the DEN_132-class malformed tokens from CEP-017 are fixed).
  **No deprecated `_s`/`_i` tokens, no dummyVariable old-style names, no
  duplicate (variable, database, recStart) blocks.**

### Regression checks on previously reported issues

- #138 (negative pack years): no longer reproduces at the API with the
  June recipe (guards present in `.calculate_pack_years_core`).
- #159 (tibble input): fixed -- `rec_with_table()` on a tibble runs
  end-to-end.
- #184/#185 (NAMESPACE, smoking sync): fixed since June.
- June ADL/alcohol repair and list-mode fix: present and covered by
  their regression tests.

## L6 integration summary

`rec_with_table()` cross-cycle runs on PUMF samples: DHH_OWN
(95.5-100%), ALCDTTM_former (98.5-100%), CCCG102_2005plus (4-10%,
plausible prevalence), INCGHH_cont (81.5-100%; the 100% from 2011+
matches StatCan income imputation), SMKDGSTP_cont (22.5-30.5%,
consistent), SBE_005 (2017_2018 only, 59% -- new sedentary domain;
2019+ cycles untestable, see below), ADL family (see step-change note).
Full matrix: `l6-pumf-prevalence-matrix.txt`.

**L6 limitation:** no sample data exists for 2019-2020, 2022, or 2023
PUMFs, so the release's headline coverage extensions to those cycles are
validated by worksheet checks only (pre-existing gap; CEP-017 Track 0
item 4).

## Package health

- R CMD check: PASS on CI.
- Full test suite: 828 passing, 0 failures, 0 errors -- after this
  review restored the three `@note` version-metadata lines dropped from
  R/adl.R by a July refactor (the only suite failures found; the
  metadata-convention test covers them). Fix applied in this review's
  commit.
- check-worksheets: exit 0 locally (all checks pass); the
  variable_details checker crash from June is fixed.

### Observation: prep_cat_output() design choice (no action required)

Commit b6270d20 added `prep_cat_output()` to all 18 categorical derived-
variable functions, returning `"NA(a)"/"NA(b)"/"NA(c)"` strings when any
missing is present. This deliberately preserves the a/b/c distinction
through the engine's `as.factor()` dispatch -- fixing the tag-destruction
defect documented in CEP-017 -- at the cost of type instability at the
function API (numeric when complete, character when any missing).
Coherent within v3's string-based categorical regime; CEP-017 Track 3
remains the longer-term resolution (haven_labelled end to end).

### Fixes applied by this review

1. check-csv workflow: install devtools (root cause of the red check)
   and reword the misleading failure message.
2. R/adl.R: restored the three `@note` metadata lines (+ regenerated Rd).

## Recommendation

Approve after: (1) the CI check re-runs green with the workflow fix in
this review's commit; (2) #139 arms are added (S-effort, recipe above); (3) the priority-order
decision is recorded (or explicitly deferred with a NEWS note). Items
(2) and (3) could alternatively be accepted as fast-follow issues if the
team prefers not to grow the PR further.
