# L0: Documentation assessment - Hearing and vision

## Topic overview

**Domain**: Sensory health
**Sub-topic**: Hearing and vision (HUI items and Washington Group measures)
**Scope**: Variables measuring hearing and vision ability from the Health Utility Index (HUI) module and Washington Group disability measures (2017-2018)

## Documentation sources reviewed

| Source | Location | Status |
|--------|----------|--------|
| DDI YAMLs | cchsflow-docs/cchs-extracted/data-dictionary/ | Partial |
| cchs_available_variables_list.csv | development/ | Reviewed |
| Existing PR #160 worksheets | hearing branch inst/extdata/ | Reviewed |
| cchsflow-docs listings | N/A | Pending |

## Multi-source reconciliation

### Variables from existing PR #160

From `inst/extdata/variables.csv` on hearing branch:

| Variable | databaseStart in PR | Issues |
|----------|---------------------|--------|
| HUI06 | cchs2001_m, cchs2003_m, cchs_2009_2010_m, cchs2013_2014_m | Typo: `cchs_2009_2010_m` |
| HUI07 | cchs2001_m, cchs2003_m, cchs_2009_2010_m, cchs2013_2014_m | Typo: `cchs_2009_2010_m` |
| HUI07A | cchs2001_m, cchs2003_m, cchs_2009_2010_m, cchs2013_2014_m, cchs_2017_2018_m | Typos: both database names |
| HUI08 | cchs2001_m, cchs2003_m, cchs_2009_2010_m, cchs2013_2014_m | Typo: `cchs_2009_2010_m` |
| HUI09 | cchs2001_m, cchs2003_m, cchs_2009_2010_m, cchs2013_2014_m | Typo + wrong source var |
| HUIGHER | Multiple PUMF and Master cycles | Correct format |
| HUIGVIS | Multiple PUMF and Master cycles | Correct format |

### Variables from cchs_available_variables_list.csv

| Variable Name | Label | 2001 | 2003 | 2005 | 2007-2008 | 2009-2010 | 2011-2012 | 2013-2014 | 2015-2016 | 2017-2018 |
|---------------|-------|------|------|------|-----------|-----------|-----------|-----------|-----------|-----------|
| HUI_01 | Read newsprint without aid | HUIA_01 | HUIC_01 | N/A | N/A | HUI_01 | N/A | HUI_01 | N/A | N/A |
| HUI_02 | Read newsprint with aid | HUIA_02 | HUIC_02 | N/A | N/A | HUI_02 | N/A | HUI_02 | N/A | N/A |
| HUI_03 | Able to see at all | HUIA_03 | HUIC_03 | N/A | N/A | HUI_03 | N/A | HUI_03 | N/A | N/A |
| HUI_04 | Recognize friend without aid | HUIA_04 | HUIC_04 | N/A | N/A | HUI_04 | N/A | HUI_04 | N/A | N/A |
| HUI_05 | Recognize friend with aid | HUIA_05 | HUIC_05 | N/A | N/A | HUI_05 | N/A | HUI_05 | N/A | N/A |
| HUI_06 | Group convo without aid | HUIA_06 | HUIC_06 | N/A | N/A | HUI_06 | N/A | HUI_06 | N/A | N/A |
| HUI_07 | Group convo with aid | HUIA_07 | HUIC_07 | N/A | N/A | HUI_07 | N/A | HUI_07 | N/A | N/A |
| HUI_07A | Able to hear at all | HUIA_07A | HUIC_07A | HUIE_07A | HUI_07A | HUI_07A | N/A | HUI_07A | N/A | N/A |
| HUI_08 | Single convo without aid | HUIA_08 | HUIC_08 | N/A | N/A | HUI_08 | N/A | HUI_08 | N/A | N/A |
| HUI_09 | Single convo with aid | HUIA_09 | HUIC_09 | N/A | N/A | HUI_09 | HUI_09 | HUI_09 | N/A | N/A |
| HUIDHER | Hearing problems (derived) | HUIADHER | HUICDHER | N/A | N/A | HUIDHER | N/A | HUIDHER | N/A | N/A |
| HUIDVIS | Vision trouble (derived) | HUIADVIS | HUICDVIS | N/A | N/A | HUIDVIS | N/A | HUIDVIS | N/A | N/A |
| HUIGHER | Hearing problems (grouped) | N/A | HUICGHER | N/A | N/A | N/A | N/A | N/A | N/A | N/A |
| HUIGVIS | Vision trouble (grouped) | N/A | HUICGVIS | N/A | N/A | N/A | N/A | N/A | N/A | N/A |
| WDM_005 | Washington Group - Difficulty seeing | N/A | N/A | N/A | N/A | N/A | N/A | N/A | N/A | WDM_005 |
| WDM_010 | Washington Group - Difficulty hearing | N/A | N/A | N/A | N/A | N/A | N/A | N/A | N/A | WDM_010 |

### Discrepancies identified

#### 1. Database name typos in PR #160

| Line | Variable | Error | Correction |
|------|----------|-------|------------|
| 176 | HUI06 | `cchs_2009_2010_m` | `cchs2009_2010_m` |
| 177 | HUI07 | `cchs_2009_2010_m` | `cchs2009_2010_m` |
| 178 | HUI07A | `cchs_2009_2010_m`, `cchs_2017_2018_m` | `cchs2009_2010_m`, `cchs2017_2018_m` |
| 179 | HUI08 | `cchs_2009_2010_m` | `cchs2009_2010_m` |
| 180 | HUI09 | `cchs_2009_2010_m` | `cchs2009_2010_m` |

#### 2. Source variable errors in PR #160

| Line | Variable | Error | Correction |
|------|----------|-------|------------|
| 176 | HUI06 | `cchs2003_m::HUAC_06` | `cchs2003_m::HUIC_06` |
| 180 | HUI09 | `[HUI_08]` | `[HUI_09]` |

#### 3. Missing cycles in PR vs available variables list

| Variable | In PR | In cchs_available_variables_list | Gap |
|----------|-------|----------------------------------|-----|
| HUI_07A | 2001, 2003, 2009-2010, 2013-2014, 2017-2018 | 2001, 2003, **2005**, **2007-2008**, 2009-2010, 2013-2014 | Missing 2005, 2007-2008 in PR; 2017-2018 WDM mapping questionable |
| HUI_09 | 2001, 2003, 2009-2010, 2013-2014 | 2001, 2003, 2009-2010, **2011-2012**, 2013-2014 | Missing 2011-2012 in PR |

#### 4. HUIGHER/HUIGVIS availability mismatch

The `cchs_available_variables_list.csv` shows HUIGHER and HUIGVIS only available in 2003 (HUICGHER, HUICGVIS). However, the PR worksheets claim broader PUMF coverage. Need DDI verification.

## Provincial availability

### Ontario-specific restrictions

| Variable | Cycle | Issue | Source |
|----------|-------|-------|--------|
| HUICGHER | 2003 | **No Ontario data in PUMF** | ontario_sensory_oral_analysis.R finding |
| HUICGVIS | 2003 | **No Ontario data in PUMF** | ontario_sensory_oral_analysis.R finding |

The 2003 PUMF HUI grouped variables have geographic restrictions - only Atlantic provinces and Quebec have data. This is a critical finding for Ontario dementia research.

### Ontario Linked file availability

**Status**: Pending verification from cchsflow-docs

| Variable | Cycles available | Notes |
|----------|------------------|-------|
| TBD | TBD | Need to check cchsflow-docs Linked file listings |

## Variables in scope

### Final variable list for CEP-004

Based on multi-source reconciliation, the following variables are in scope:

#### HUI vision items (Master file)

| Harmonized | Type | Cycles | Universe |
|------------|------|--------|----------|
| HUI01 | Categorical | 2001, 2003, 2009-2010, 2013-2014 | Age 12+ |
| HUI02 | Categorical | 2001, 2003, 2009-2010, 2013-2014 | Age 12+ |
| HUI03 | Categorical | 2001, 2003, 2009-2010, 2013-2014 | Age 12+ |
| HUI04 | Categorical | 2001, 2003, 2009-2010, 2013-2014 | Age 12+ |
| HUI05 | Categorical | 2001, 2003, 2009-2010, 2013-2014 | Age 12+ |

#### HUI hearing items (Master file)

| Harmonized | Type | Cycles | Universe |
|------------|------|--------|----------|
| HUI06 | Categorical | 2001, 2003, 2009-2010, 2013-2014 | Age 12+ |
| HUI07 | Categorical | 2001, 2003, 2009-2010, 2013-2014 | Age 12+ |
| HUI07A | Categorical | 2001, 2003, 2005, 2007-2008, 2009-2010, 2013-2014 | Age 12+ |
| HUI08 | Categorical | 2001, 2003, 2009-2010, 2013-2014 | Age 12+ |
| HUI09 | Categorical | 2001, 2003, 2009-2010, 2011-2012, 2013-2014 | Age 12+ |

#### HUI derived hearing/vision (PUMF and Master)

| Harmonized | Type | PUMF cycles | Master cycles | Notes |
|------------|------|-------------|---------------|-------|
| HUIGHER | Categorical | Verify DDI | Verify DDI | Grouped version |
| HUIGVIS | Categorical | Verify DDI | Verify DDI | Grouped version |
| HUIDHER | Categorical | N/A | 2001, 2003, 2009-2010, 2013-2014 | Derived (Master only) |
| HUIDVIS | Categorical | N/A | 2001, 2003, 2009-2010, 2013-2014 | Derived (Master only) |

#### Washington Group measures (2017-2018)

| Harmonized | Type | Cycles | Scale | Notes |
|------------|------|--------|-------|-------|
| WDM_005 | Categorical | 2017-2018 | 1-4 | Vision difficulty |
| WDM_010 | Categorical | 2017-2018 | 1-4 | Hearing difficulty |

## Key decisions

### D1: Database name standardization

**Decision**: Use standard cchsflow database naming (e.g., `cchs2009_2010_m` not `cchs_2009_2010_m`).

**Rationale**: Consistency with existing worksheets and validation infrastructure.

### D2: Fix existing PR errors before L3

**Decision**: Document all errors in this L0 assessment and create a fix proposal CSV rather than direct edits.

**Rationale**: Per CLAUDE.local.md guidelines for data handling.

### D3: HUI07A WDM mapping

**Decision**: The 2017-2018 mapping of WDM_101 to HUI07A requires semantic mapping documentation in L2.

**Rationale**: HUI uses 6-point scale while Washington Group uses 4-point scale. Harmonization requires documented crosswalk.

### D4: Scope includes vision items (HUI01-05)

**Decision**: Include HUI vision items (01-05) in this CEP alongside hearing items.

**Rationale**: They share the same module, era patterns, and data availability restrictions.

## Phase classification

### Phase 1: Fix existing PR errors

Create fix proposal CSV for:
1. Database name typos (5 rows)
2. Source variable errors (2 rows)
3. Missing cycle coverage (2 rows)

### Phase 2: Extend coverage

Add variables from cchs_available_variables_list.csv not in current PR:
1. HUI01-05 (vision items)
2. Verify HUIGHER/HUIGVIS actual availability

### Phase 3: Integration testing

Create QMD for:
1. Canada-wide availability matrix
2. Ontario-specific analysis (50+, 55+, 60+ age cutoffs)

## Next steps

1. **L1 Variable concordance**: Map all source variable names across eras
2. **DDI verification**: Confirm variable existence for each claimed database
3. **Fix proposal CSV**: Document corrections needed for PR #160 worksheets
