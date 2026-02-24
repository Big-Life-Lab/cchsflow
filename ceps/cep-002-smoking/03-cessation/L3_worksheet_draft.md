# L3: Worksheet draft - Smoking cessation timing

**Topic**: Smoking cessation timing (SMK_06, SMK_09, SMK_10, SMKDSTP)
**Date started**: 2026-01-02
**Status**: Phase 1 complete, Phase 2 pending

## Overview

This document tracks L3 worksheet authoring for smoking cessation timing variables.
L3 produces two CSV files that contain the harmonization rules.

## Output files

| File | Status | Description |
|------|--------|-------------|
| `variables_draft.csv` | In progress | Variable summary (one row per harmonized variable) |
| `variable_details_draft.csv` | In progress | Recoding rules (multiple rows per variable) |

## Phase status

L3 uses a two-phase approach: Phase 1 for pass-through variables that don't require derived functions (DVs), and Phase 2 for variables that require DV implementation first.

### Phase 1: Variables not requiring DVs (complete)

| Variable | Type | Source pattern | Status | Notes |
|----------|------|----------------|--------|-------|
| SMKDSTP | Pass-through | StatCan derived | Complete | Master=cont, PUMF=cat. Uses SMKDSTP (2007-2014), SMKDVSTP (2015+), SMKCDSTP (2003), SMKEDSTP (2005) |
| SMK_10_gate | Pass-through | Categorical | Complete | Quit gate. Uses SMK_10 (2007-2014), SMK_095 (2015-2021), SPU_30 (2022+). Not in 2001 |
| SMK_06A_cat | Pass-through | Categorical | Complete | Former occasional quit timing. Uses SMKA_06A (2001), SMKC_06A (2003), SMKE_06A (2005), SMK_06A (2007-2014), SMK_060 (2015-2021), SPU_10 (2023). 2022 excluded (Phase 2) |
| SMK_10A_cat | Pass-through | Categorical | Complete | Former daily quit timing (who continued occasional). Uses SMKC_10A (2003), SMKE_10A (2005), SMK_10A (2007-2014), SMK_100 (2015-2021), SPU_35 (2023). Not in 2001, 2022 excluded (Phase 2) |

**Phase 1 completion date**: 2026-01-02 (SMKDSTP), 2026-01-03 (SMK_10_gate, SMK_06A_cat, SMK_10A_cat)

### Phase 2: Variables requiring DVs (complete)

| Variable | Type | DV function | Status | Notes |
|----------|------|-------------|--------|-------|
| quit_pathway | Derived categorical | `assess_quit_pathway` | Complete | 3-category: direct/gradual/occasional |
| SMK_06A_cont | Derived continuous | `calculate_SMK_06A_cont` | Complete | Tier 1: categorical → continuous |
| SMK_10A_cont | Derived continuous | `calculate_SMK_10A_cont` | Complete | Tier 1: categorical → continuous |
| time_quit_smoking | Derived continuous | `calculate_time_quit_smoking` | Complete | Tier 2: combines Tier 1 outputs |

**Phase 2 completion date**: 2026-01-03

**Phase 2 output files**:
- `phase2_variables_draft.csv` - variables.csv entries for DV variables
- `phase2_variable_details_draft.csv` - variable_details.csv entries with `Func::` recEnd

## Naming decisions

### SMKDSTP (Phase 1)

| Decision | Rationale |
|----------|-----------|
| Name: `SMKDSTP` | StatCan derived variable (D in position 4) - keep original name per decision tree |
| No `_cont` suffix | Variable is already continuous in Master. `_cont` is reserved for pseudo-continuous from grouped categories |
| Single variable for both file types | Master uses `typeStart: cont`, PUMF uses `typeStart: cat`. Same harmonized variable, different transformation paths |
| variableType: Categorical | Lowest common denominator - PUMF only has categorical version |

### SMK_10_gate (Phase 1)

| Decision | Rationale |
|----------|-----------|
| Name: `SMK_10_gate` | Descriptive name indicating gate function for quit pathway selection |
| Categorical (2 valid categories) | 1=Yes (quit when stopped daily), 2=No (continued occasional) |
| Not available 2001 | Use SMK_09 as proxy with documentation for 2001 analysis |

### SMK_06A_cat (Phase 1)

| Decision | Rationale |
|----------|-----------|
| Name: `SMK_06A_cat` | Follows source variable naming (SMK_06A = categorical "when stopped"). `_cat` suffix distinguishes from continuous/years versions |
| Universe | Former occasional smokers (SMKDSTY_cat5 == 4, never daily) |
| Categories (4 valid) | 1=<1yr, 2=1-2yr, 3=2-3yr, 4=3+yr |
| 2022 excluded | Month/year only (SPU_10A/B) - needs DV derivation in Phase 2 |
| Input for | `time_quit_occ` derivation in Phase 2 |

### SMK_10A_cat (Phase 1)

| Decision | Rationale |
|----------|-----------|
| Name: `SMK_10A_cat` | Follows source variable naming (SMK_10A = categorical "when quit completely"). `_cat` suffix distinguishes from continuous/years versions |
| Universe | Former daily who continued occasional smoking (SMKDSTY_cat5 == 3 AND SMK_10_gate == 2) |
| Categories (4 valid) | 1=<1yr, 2=1-2yr, 3=2-3yr, 4=3+yr |
| Not available 2001 | SMK_10 series not collected in 2001 |
| 2022 excluded | Month/year only (SPU_35A/B) - needs DV derivation in Phase 2 |
| Input for | `time_quit_complete_daily` derivation in Phase 2 |

## Row format validation

Per SKILL.md, L3 must use **condensed row format**:

- One row per `recEnd` category, not per database
- Group databases with identical `recStart` -> `recEnd` mappings
- Use `[VARNAME]` reference for pass-through cycles

### Row count check

| Variable | Expected rows | Actual rows | Status |
|----------|---------------|-------------|--------|
| SMKDSTP | ~40 | 40 | ✅ Condensed |
| SMK_10_gate | ~25 | 25 | ✅ Condensed |
| SMK_06A_cat | ~42 | 42 | ✅ Condensed |
| SMK_10A_cat | ~35 | 35 | ✅ Condensed |

**Note**: All worksheets condensed on 2026-01-03. Condensed files have `_condensed.csv` suffix. Original exploded versions retained as `_draft.csv` for reference.

### Condensed worksheet files

| Original file | Condensed file | Rows (orig → condensed) |
|---------------|----------------|------------------------|
| `variable_details_draft.csv` | `variable_details_draft_condensed.csv` | 196 → 65 |
| `SMK_06_series_details_draft.csv` | `SMK_06_series_details_condensed.csv` | 77 → 42 |
| `SMK_10_series_details_draft.csv` | `SMK_10_series_details_condensed.csv` | 63 → 35 |

### New worksheet files (SMK_06/SMK_10 series)

L3 worksheets for the SMK_06 and SMK_10 categorical timing variables are stored in separate draft files:

| File | Variable | Description |
|------|----------|-------------|
| `SMK_06_series_draft.csv` | SMK_06A_cat | variables.csv entry |
| `SMK_06_series_details_condensed.csv` | SMK_06A_cat | variable_details.csv entries (42 rows) |
| `SMK_10_series_draft.csv` | SMK_10A_cat | variables.csv entry |
| `SMK_10_series_details_condensed.csv` | SMK_10A_cat | variable_details.csv entries (35 rows) |

These will be merged into the main `variables_draft.csv` and `variable_details_draft.csv` files after review.

## Dependencies from L2

From [L2-semantic-mapping.md](L2-semantic-mapping.md):

| Dependency | Status | Notes |
|------------|--------|-------|
| SMKDSTY (smoking status) | Pending | Required for pathway selection. Waiting for 01-status update |
| SMK_06 series | **Complete** | SMK_06A_cat (categorical quit timing for former occasional) |
| SMK_09 series | Pending | Stopped daily timing. Need L3 worksheet (also exists in cchsflow as SMK_09A_B) |
| SMK_10 series | **Complete** | SMK_10A_cat (categorical quit timing for former daily who continued occasional) |

## Validation checklist

Before marking L3 complete:

- [x] Phase 1 variables added to variables_draft.csv
- [x] Phase 1 recoding rules added to variable_details_draft.csv
- [x] Row format condensed (2026-01-03)
- [x] Validation against DDI sources passed (2026-01-03)
- [x] Phase 2 variables added (2026-01-03)
- [x] All databases verified against DDI (2026-01-03)

## Related documents

- [L0-assessment-smoking-cessation.md](L0-assessment-smoking-cessation.md) - Documentation assessment
- [L1-variable-concordance.md](L1-variable-concordance.md) - Variable discovery
- [L2-semantic-mapping.md](L2-semantic-mapping.md) - Semantic groupings
- [L4_dv_specifications.md](L4_dv_specifications.md) - DV specifications
- [_workflow_state.yaml](_workflow_state.yaml) - Workflow tracking

## Change log

| Date | Change | Author |
|------|--------|--------|
| 2026-01-02 | Created L3 document, added SMKDSTP (Phase 1) | claude-code |
| 2026-01-03 | Added SMK_10_gate (Phase 1), Phase 1 marked complete | claude-code |
| 2026-01-03 | Created this tracking document (retroactive) | claude-code |
| 2026-01-03 | Added SMK_06A_cat and SMK_10A_cat (Phase 1). Created separate draft files for these variables pending merge | claude-code |
| 2026-01-03 | Condensed all L3 worksheets (196→65, 77→42, 63→35 rows). DDI verification complete | claude-code |
| 2026-01-03 | Added Phase 2 DV entries: quit_pathway, SMK_06A_cont, SMK_10A_cont, time_quit_smoking | claude-code |
| 2026-01-03 | Merged all draft files into variables_merged.csv (8 vars) and variable_details_merged.csv (157 rows) | claude-code |
| 2026-01-26 | PR163 fix: Added explicit SMK_090/SMK_070 mappings for 2015+ cycles in SMK_09C and SMK_06C. The `[SMK_09C]` and `[SMK_06C]` bracket fallback doesn't work for 2015+ where variables were renamed. | claude-code |
| 2026-02-22 | v3.0 naming rationalisation: SMK_06A_A/SMK_06A_B → SMK_06A_cat4, SMK_09A_A/SMK_09A_B → SMK_09A_cat4, SMK_10A_B → SMK_10A. See `smoking-dv-refactoring-plan.md` for rationale. | claude-code |
