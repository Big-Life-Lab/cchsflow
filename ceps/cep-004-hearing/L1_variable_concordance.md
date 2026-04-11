# L1: Variable concordance - Hearing and vision

## Overview

This document maps source variable names across all CCHS cycles for hearing and vision variables.

## Era naming patterns

| Era | Years | Pattern | Example |
|-----|-------|---------|---------|
| Pre-2007 | 2001 | Cycle A prefix | HUIA_06 |
| Pre-2007 | 2003 | Cycle C prefix | HUIC_06 |
| Pre-2007 | 2005 | Cycle E prefix | HUIE_07A |
| 2007-2014 | 2007-2014 | Standard | HUI_06 |
| Post-2014 | 2017-2018 | Washington Group | WDM_005, WDM_010 |

## HUI vision items (Master file)

### Source variable concordance

| Harmonized | 2001 | 2003 | 2005 | 2007-2008 | 2009-2010 | 2011-2012 | 2013-2014 | 2015-2016 | 2017-2018 |
|------------|------|------|------|-----------|-----------|-----------|-----------|-----------|-----------|
| HUI01 | HUIA_01 | HUIC_01 | - | - | HUI_01 | - | HUI_01 | - | - |
| HUI02 | HUIA_02 | HUIC_02 | - | - | HUI_02 | - | HUI_02 | - | - |
| HUI03 | HUIA_03 | HUIC_03 | - | - | HUI_03 | - | HUI_03 | - | - |
| HUI04 | HUIA_04 | HUIC_04 | - | - | HUI_04 | - | HUI_04 | - | - |
| HUI05 | HUIA_05 | HUIC_05 | - | - | HUI_05 | - | HUI_05 | - | - |

### Vision item descriptions

| Variable | Question | Response scale |
|----------|----------|----------------|
| HUI01 | Are you able to read ordinary newsprint WITHOUT glasses or contact lenses? | 1=Yes, 2=No |
| HUI02 | Are you able to read ordinary newsprint WITH glasses or contact lenses? | 1=Yes, 2=No |
| HUI03 | Are you able to see at all? | 1=Yes, 2=No |
| HUI04 | Are you able to recognize a friend on the other side of the street WITHOUT glasses or contact lenses? | 1=Yes, 2=No |
| HUI05 | Are you able to recognize a friend on the other side of the street WITH glasses or contact lenses? | 1=Yes, 2=No |

## HUI hearing items (Master file)

### Source variable concordance

| Harmonized | 2001 | 2003 | 2005 | 2007-2008 | 2009-2010 | 2011-2012 | 2013-2014 | 2015-2016 | 2017-2018 |
|------------|------|------|------|-----------|-----------|-----------|-----------|-----------|-----------|
| HUI06 | HUIA_06 | HUIC_06 | - | - | HUI_06 | - | HUI_06 | - | - |
| HUI07 | HUIA_07 | HUIC_07 | - | - | HUI_07 | - | HUI_07 | - | - |
| HUI07A | HUIA_07A | HUIC_07A | HUIE_07A | HUI_07A | HUI_07A | - | HUI_07A | - | - |
| HUI08 | HUIA_08 | HUIC_08 | - | - | HUI_08 | - | HUI_08 | - | - |
| HUI09 | HUIA_09 | HUIC_09 | - | - | HUI_09 | HUI_09 | HUI_09 | - | - |

### Hearing item descriptions

| Variable | Question | Response scale |
|----------|----------|----------------|
| HUI06 | Are you able to hear what is said in a group conversation with at least three other people WITHOUT a hearing aid? | 1=Yes, 2=No |
| HUI07 | Are you able to hear what is said in a group conversation with at least three other people WITH a hearing aid? | 1=Yes, 2=No |
| HUI07A | Are you able to hear at all? | 1=Yes, 2=No |
| HUI08 | Are you able to hear what is said in a conversation with one other person in a quiet room WITHOUT a hearing aid? | 1=Yes, 2=No |
| HUI09 | Are you able to hear what is said in a conversation with one other person in a quiet room WITH a hearing aid? | 1=Yes, 2=No |

### Notes on HUI07A

HUI07A is unique among hearing items - it has broader cycle coverage including 2005 and 2007-2008. This may be because it serves as a gate question for the hearing aid items.

### Notes on HUI09

HUI09 appears in 2011-2012 according to cchs_available_variables_list.csv, but this is not reflected in the current PR #160 worksheets.

## HUI derived variables (PUMF and Master)

### PUMF grouped variables

| Harmonized | 2001 | 2003 | 2005 | 2007-2008 | 2009-2010 | 2011-2012 | 2013-2014 | 2015-2016 | 2017-2018 |
|------------|------|------|------|-----------|-----------|-----------|-----------|-----------|-----------|
| HUIGHER | HUIAGHER | HUICGHER* | HUIEGHER | HUIGHER | HUIGHER | - | HUIGHER | HUIDGHER | - |
| HUIGVIS | HUIAGVIS | HUICGVIS* | HUIEGVIS | HUIGVIS | HUIGVIS | - | HUIGVIS | HUIDGVIS | - |

*2003 PUMF HUICGHER/HUICGVIS has NO Ontario data - only Atlantic provinces and Quebec.

### Master derived variables

| Harmonized | 2001 | 2003 | 2005 | 2007-2008 | 2009-2010 | 2011-2012 | 2013-2014 | 2015-2016 | 2017-2018 |
|------------|------|------|------|-----------|-----------|-----------|-----------|-----------|-----------|
| HUIDHER | HUIADHER | HUICDHER | - | - | HUIDHER | - | HUIDHER | - | - |
| HUIDVIS | HUIADVIS | HUICDVIS | - | - | HUIDVIS | - | HUIDVIS | - | - |

### Derived variable scale

The HUI derived variables (HUIDHER, HUIDVIS, HUIGHER, HUIGVIS) use a 6-point scale:

| Code | Meaning |
|------|---------|
| 1 | Able to hear/see normally |
| 2 | Able to hear/see with difficulty |
| 3 | Able to hear/see with a lot of difficulty |
| 4 | Able to hear/see, but with extreme difficulty |
| 5 | Unable to hear/see, despite aids |
| 6 | Unable to hear/see at all |

## Washington Group measures (2017-2018)

### Source variables

| Harmonized | 2017-2018 | Module |
|------------|-----------|--------|
| WDM_005 | WDM_005 | Washington Disability Module |
| WDM_010 | WDM_010 | Washington Disability Module |

### Washington Group scale

| Code | Meaning |
|------|---------|
| 1 | No difficulty |
| 2 | Some difficulty |
| 3 | A lot of difficulty |
| 4 | Cannot do at all |

### Scale comparison: HUI vs Washington Group

| HUI code | HUI meaning | WDM code | WDM meaning |
|----------|-------------|----------|-------------|
| 1 | Normal | 1 | No difficulty |
| 2 | Some difficulty | 2 | Some difficulty |
| 3 | A lot of difficulty | 3 | A lot of difficulty |
| 4 | Extreme difficulty | 3 | A lot of difficulty |
| 5 | Unable despite aids | 4 | Cannot do at all |
| 6 | Unable at all | 4 | Cannot do at all |

**Note**: The HUI 6-point scale to WDM 4-point scale crosswalk requires semantic mapping documentation in L2.

## variableStart patterns for worksheets

### HUI items (Master file)

```
# HUI06 example
variableStart: cchs2001_m::HUIA_06, cchs2003_m::HUIC_06, [HUI_06]
databaseStart: cchs2001_m, cchs2003_m, cchs2009_2010_m, cchs2013_2014_m
```

### HUI07A (broader coverage)

```
# HUI07A - includes 2005 and 2007-2008
variableStart: cchs2001_m::HUIA_07A, cchs2003_m::HUIC_07A, cchs2005_m::HUIE_07A, [HUI_07A]
databaseStart: cchs2001_m, cchs2003_m, cchs2005_m, cchs2007_2008_m, cchs2009_2010_m, cchs2013_2014_m
```

### HUI09 (includes 2011-2012)

```
# HUI09 - includes 2011-2012 per cchs_available_variables_list.csv
variableStart: cchs2001_m::HUIA_09, cchs2003_m::HUIC_09, [HUI_09]
databaseStart: cchs2001_m, cchs2003_m, cchs2009_2010_m, cchs2011_2012_m, cchs2013_2014_m
```

## Errors in current PR #160 worksheets

### Database name errors

| Variable | Current | Correct |
|----------|---------|---------|
| HUI06-09 | `cchs_2009_2010_m` | `cchs2009_2010_m` |
| HUI07A | `cchs_2017_2018_m` | `cchs2017_2018_m` |

### Source variable errors

| Variable | Current | Correct | Issue |
|----------|---------|---------|-------|
| HUI06 | `cchs2003_m::HUAC_06` | `cchs2003_m::HUIC_06` | Typo in variable name |
| HUI09 | `[HUI_08]` | `[HUI_09]` | Wrong default variable |

### Missing cycle coverage

| Variable | Missing from PR | Source |
|----------|-----------------|--------|
| HUI07A | 2005, 2007-2008 | cchs_available_variables_list.csv shows HUIE_07A in 2005, HUI_07A in 2007-2008 |
| HUI09 | 2011-2012 | cchs_available_variables_list.csv shows HUI_09 in 2011-2012 |

## Next steps

1. **L2 Semantic mapping**: Document HUI to Washington Group crosswalk
2. **Fix proposal CSV**: Create corrections for PR #160 errors
3. **DDI verification**: Confirm all claimed cycles against DDI YAMLs
