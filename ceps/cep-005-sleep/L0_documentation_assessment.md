# L0 Documentation assessment: Sleep variables

## Overview

This document assesses the documentation and source data for sleep harmonization variables in the CCHS, supporting PR #161.

## Variables in scope

| Variable | Label | Type | Description |
|----------|-------|------|-------------|
| SLP_02 | Trouble sleeping | Categorical | Frequency of trouble going to sleep or staying asleep |
| SLP_02_A | Trouble sleeping | Categorical | 2001 version (different scale) |
| SLP_03 | Sleep refreshing | Categorical | Frequency of finding sleep refreshing |
| SLP_03_A | Sleep refreshing | Categorical | 2001 version (different scale) |
| SLP_04 | Difficulty awake | Categorical | Frequency of difficulty staying awake |
| SLP_04_A | Difficulty awake | Categorical | 2001 version (different scale) |
| SLPG01 | Hours sleep | Categorical | Hours spent sleeping each night (grouped) |
| SLPG01_A | Hours sleep | Categorical | 2001/2007-2008 version |
| SLPG01_B | Hours sleep | Categorical | Master file 2011-2014 |
| SLPG01_C | Hours sleep | Categorical | Master file 2001/2015-2016 |
| SLPG01_cont | Hours sleep | Continuous | Hours sleeping (continuous) |

## PUMF availability by cycle

| Cycle | Sleep hours | Trouble sleeping | Sleep refreshing | Difficulty awake |
|-------|-------------|------------------|------------------|------------------|
| 2001 | GENA_03 | GENA_04 | GENA_05 | GENA_06 |
| 2003 | **NOT AVAILABLE** | **NOT AVAILABLE** | **NOT AVAILABLE** | **NOT AVAILABLE** |
| 2005 | **NOT AVAILABLE** | **NOT AVAILABLE** | **NOT AVAILABLE** | **NOT AVAILABLE** |
| 2007-2008 | SLP_01 | SLP_02 | SLP_03 | SLP_04 |
| 2009-2010 | **NOT AVAILABLE** | **NOT AVAILABLE** | **NOT AVAILABLE** | **NOT AVAILABLE** |
| 2011-2012 | SLPG01 | SLP_02 | SLP_03 | SLP_04 |
| 2013-2014 | SLPG01 | SLP_02 | SLP_03 | SLP_04 |
| 2015-2016 | SLPG005 | SLP_010 | SLP_015 | SLP_020 |
| 2017-2018 | SLPG005 | SLP_010 | SLP_015 | SLP_020 |

## Critical finding: Missing cycles

**Sleep variables are NOT available in 2003, 2005, and 2009-2010 PUMF files.**

This was verified by searching the DDI XML files for all sleep-related variable names. The sleep module was an optional content module that was not included in these cycles.

## Scale differences

### 2001 scale (3-point)
For trouble sleeping (GENA_04), sleep refreshing (GENA_05), difficulty awake (GENA_06):
- 1 = Most of the time
- 2 = Sometimes
- 3 = Never
- 6 = Not applicable
- 7 = Don't know
- 8 = Refusal
- 9 = Not stated

### 2007+ scale (5-point)
For SLP_02, SLP_03, SLP_04 (and renamed versions):
- 1 = None of the time / Never
- 2 = A little of the time / Rarely
- 3 = Some of the time / Sometimes
- 4 = Most of the time
- 5 = All of the time
- 6 = Valid skip / Not applicable
- 7 = Don't know
- 8 = Refusal
- 9 = Not stated

**Important:** The 2001 scale cannot be directly mapped to the 2007+ scale because:
- 2001 has 3 substantive categories (most/sometimes/never)
- 2007+ has 5 substantive categories (none/little/some/most/all)

This explains why there are separate _A variables for 2001 - they cannot be harmonized to the same scale.

## Hours sleep variable encoding

### 2001 (GENA_03) - Continuous-ish
- 1 = < 2 hours
- 2 = 2 - < 3 hours
- ...
- 12 = >= 12 hours
- 96 = Not applicable
- 97 = Don't know
- 98 = Refusal
- 99 = Not stated

### 2007-2008 (SLP_01) - Grouped
Uses similar categorical encoding.

### 2011-2018 (SLPG01, SLPG005) - Grouped
Similar categorical encoding with hour ranges.

## PR #161 review

### Scope
PR adds master file cycles to existing sleep variables:
- SLP_02, SLP_03, SLP_04: Adding cchs2011_2012_m, cchs2013_2014_m, cchs2015_2016_m
- SLP_02_A, SLP_03_A, SLP_04_A: Adding cchs2001_m
- SLPG01_A: Adding cchs2001_p (already has cchs2007_2008_p), cchs2015_2016_m
- SLPG01_B: New variable for cchs2011_2012_m, cchs2013_2014_m
- SLPG01_cont: Adding master cycles

### Issues identified

1. **SLPG01_C variableStart reference**: Uses `cchs2015_2016_m::SLP_009` but need to verify this is the correct Master file variable name (PUMF uses SLPG005).

2. **Scale consistency**: The PR correctly maintains separate _A variables for 2001 due to the scale difference.

3. **Database name format**: Need to verify all database names follow the correct format (e.g., `cchs2011_2012_m` not `cchs_2011_2012_m`).

## Recommendations

1. Verify SLPG01_C source variable for 2015-2016 Master file
2. Document the scale incompatibility between 2001 and later cycles in variable notes
3. Add coverage note about 2003, 2005, 2009-2010 being unavailable

## Sources reviewed

- [x] cchs_available_variables_list.csv
- [x] CCHS 2001 DDI
- [x] CCHS 2015-2016 DDI
- [x] Existing variables.csv
- [x] PR #161 diff
