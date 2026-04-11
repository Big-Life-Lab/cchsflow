# L2: Semantic mapping - Hearing and vision

## Overview

This document defines harmonization rules and identifies semantic breaks for hearing and vision variables.

## Scale systems

### HUI scale (2001-2014)

The Health Utility Index uses a 6-point scale for derived hearing (HUIDHER/HUIGHER) and vision (HUIDVIS/HUIGVIS) variables:

| Code | Label | Description |
|------|-------|-------------|
| 1 | Normal | Able to see/hear normally |
| 2 | Some difficulty | Able to see/hear with some difficulty |
| 3 | A lot of difficulty | Able to see/hear with a lot of difficulty |
| 4 | Extreme difficulty | Able to see/hear, but with extreme difficulty |
| 5 | Unable despite aids | Unable to see/hear, despite aids |
| 6 | Unable at all | Unable to see/hear at all |

### Washington Group scale (2017-2018)

The Washington Group uses a 4-point scale:

| Code | Label |
|------|-------|
| 1 | No difficulty |
| 2 | Some difficulty |
| 3 | A lot of difficulty |
| 4 | Cannot do at all |

## Semantic break: HUI to Washington Group

### Nature of change

In 2017-2018, CCHS replaced the HUI hearing/vision module with Washington Group disability measures. This represents:

1. **Scale change**: 6-point to 4-point
2. **Conceptual shift**: HUI measures ability with/without aids; WDM measures functional difficulty
3. **Universe change**: WDM was administered differently

### HUI to WDM crosswalk

| HUI code | HUI meaning | Recommended WDM mapping | Rationale |
|----------|-------------|-------------------------|-----------|
| 1 | Normal | 1 (No difficulty) | Direct semantic match |
| 2 | Some difficulty | 2 (Some difficulty) | Direct semantic match |
| 3 | A lot of difficulty | 3 (A lot of difficulty) | Direct semantic match |
| 4 | Extreme difficulty | 3 (A lot of difficulty) | Conservative mapping - extreme maps to "a lot" |
| 5 | Unable despite aids | 4 (Cannot do at all) | Functional inability despite intervention |
| 6 | Unable at all | 4 (Cannot do at all) | Complete inability |

### WDM to HUI crosswalk (reverse)

| WDM code | WDM meaning | Recommended HUI mapping | Rationale |
|----------|-------------|-------------------------|-----------|
| 1 | No difficulty | 1 (Normal) | Direct semantic match |
| 2 | Some difficulty | 2 (Some difficulty) | Direct semantic match |
| 3 | A lot of difficulty | 3 (A lot of difficulty) | Conservative - could be 3 or 4 |
| 4 | Cannot do at all | 6 (Unable at all) | Conservative - could be 5 or 6 |

### Harmonization strategy options

#### Option A: 4-category harmonized scale (recommended for cross-era analysis)

Collapse to common denominator:

| Harmonized code | Meaning | HUI source | WDM source |
|-----------------|---------|------------|------------|
| 1 | No difficulty | 1 | 1 |
| 2 | Some difficulty | 2 | 2 |
| 3 | A lot of difficulty | 3, 4 | 3 |
| 4 | Unable/Cannot do | 5, 6 | 4 |

#### Option B: Preserve 6-category with imputation

Keep full HUI scale and impute WDM responses:
- WDM 3 → HUI 3 (conservative)
- WDM 4 → HUI 6 (conservative)

This preserves granularity for 2001-2014 but introduces uncertainty for 2017-2018.

### Decision: D1 - Use 4-category harmonized scale

**Rationale**: For dementia research requiring cross-era comparison, the 4-category scale provides:
1. Direct conceptual mapping without imputation
2. Consistent interpretation across eras
3. Sufficient granularity for clinical significance

## Category mappings

### Binary hearing/vision variables (HUI01-09)

These are straightforward binary items:

| Original code | Meaning | Harmonized code |
|---------------|---------|-----------------|
| 1 | Yes | 1 |
| 2 | No | 2 |
| 6 | Not applicable | NA::a |
| 7 | Don't know | NA::b |
| 8 | Refusal | NA::b |
| 9 | Not stated | NA::b |

### Derived variables (HUIGHER, HUIGVIS)

| Original code | Meaning | 4-cat harmonized |
|---------------|---------|------------------|
| 1 | Normal | 1 |
| 2 | Some difficulty | 2 |
| 3 | A lot of difficulty | 3 |
| 4 | Extreme difficulty | 3 |
| 5 | Unable despite aids | 4 |
| 6 | Unable at all | 4 |
| 96 | Not applicable | NA::a |
| 97 | Don't know | NA::b |
| 98 | Refusal | NA::b |
| 99 | Not stated | NA::b |

### Washington Group (WDM_005, WDM_010)

| Original code | Meaning | 4-cat harmonized |
|---------------|---------|------------------|
| 1 | No difficulty | 1 |
| 2 | Some difficulty | 2 |
| 3 | A lot of difficulty | 3 |
| 4 | Cannot do at all | 4 |
| 6 | Not applicable | NA::a |
| 7 | Don't know | NA::b |
| 8 | Refusal | NA::b |
| 9 | Not stated | NA::b |

## Missing data handling

### Missing code patterns

| Pattern | Codes | Meaning |
|---------|-------|---------|
| NA::a | 6, 96, 996 | Not applicable (valid skip) |
| NA::b | 7, 8, 9, 97, 98, 99, 997, 998, 999 | Don't know / Refusal / Not stated |

### Universe restrictions

| Variable | Universe | Skip pattern |
|----------|----------|--------------|
| HUI01-05 | Age 12+ | Skip if age < 12 |
| HUI06-09 | Age 12+ | Skip if age < 12 |
| HUI07A | Age 12+ | Gate question for hearing aid items |
| HUIGHER/HUIGVIS | Age 12+ | Derived from component items |
| WDM_005/WDM_010 | Age 15+ | 2017-2018 Washington Group universe |

## Provincial restrictions

### 2003 Ontario exclusion

**CRITICAL**: The 2003 PUMF HUI grouped variables have NO Ontario data:

| Variable | 2003 PUMF | 2003 Master |
|----------|-----------|-------------|
| HUICGHER | No Ontario | Verify |
| HUICGVIS | No Ontario | Verify |

Available provinces in 2003 PUMF:
- Atlantic provinces
- Quebec

**Impact**: For Ontario dementia research, the 2003 cycle cannot be used for PUMF-based hearing/vision analysis.

## Harmonization rules summary

### HUI binary items (HUI01-09)

```
recStart → recEnd
1 → 1 (Yes)
2 → 2 (No)
6 → NA::a
7,8,9 → NA::b
else → NA::b
```

### HUI derived to 4-category (HUIGHER_cat4, HUIGVIS_cat4)

```
recStart → recEnd
1 → 1 (No difficulty)
2 → 2 (Some difficulty)
3,4 → 3 (A lot of difficulty)
5,6 → 4 (Unable/Cannot do)
96 → NA::a
97,98,99 → NA::b
else → NA::b
```

### WDM to 4-category (direct pass-through)

```
recStart → recEnd
1 → 1 (No difficulty)
2 → 2 (Some difficulty)
3 → 3 (A lot of difficulty)
4 → 4 (Cannot do at all)
6 → NA::a
7,8,9 → NA::b
else → NA::b
```

## Recommendations for dementia research

### Preferred variables by cycle

| Cycle | Hearing variable | Vision variable | Notes |
|-------|------------------|-----------------|-------|
| 2001 | HUIGHER_cat4 | HUIGVIS_cat4 | Full coverage |
| 2003 | HUIGHER_cat4 | HUIGVIS_cat4 | **No Ontario in PUMF** |
| 2005 | HUIGHER_cat4 | HUIGVIS_cat4 | Verify availability |
| 2007-2008 | HUIGHER_cat4 | HUIGVIS_cat4 | Verify availability |
| 2009-2010 | HUIGHER_cat4 | HUIGVIS_cat4 | Full coverage |
| 2011-2012 | N/A | N/A | Not available |
| 2013-2014 | HUIGHER_cat4 | HUIGVIS_cat4 | Full coverage |
| 2015-2016 | HUIGHER_cat4 | HUIGVIS_cat4 | Verify availability |
| 2017-2018 | WDM_010_cat4 | WDM_005_cat4 | Washington Group |

### Analysis considerations

1. **Exclude 2003 from Ontario PUMF analysis** - No Ontario data available
2. **Document scale crosswalk** - When combining pre-2017 and 2017-2018 data
3. **Use 4-category scale** - For consistent cross-era comparison
4. **Report cycles separately** - Allow readers to assess comparability

## Next steps

1. **L3 Worksheet authoring**: Create variable_details.csv with recoding rules
2. **Integration test QMD**: Validate category distributions across cycles
3. **Ontario availability matrix**: Document respondent counts by age group
