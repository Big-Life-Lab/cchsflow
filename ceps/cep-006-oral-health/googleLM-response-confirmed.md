# GoogleLM response: Oral health variable availability confirmed

**Date**: 2026-01-26
**Source**: GoogleLM NotebookLM cross-check against cchs_variable_dictionary.csv

## Executive summary

| Issue | Resolution |
|-------|------------|
| 2017-2018 DEN_132 | **Renamed to DEN_035** - concept exists, variable name changed |
| 2015-2016 gap | **Confirmed** - Only CHP_080 (binary 12-month) available |
| 2011-2012 gap | **Confirmed** - 2011 Annual only; oral health dropped in 2012 |
| OHT_015 vs DEN_132 | **Different concepts** - OHT_015 = health status, DEN_132 = utilisation |

## Corrected availability matrix (Ontario Share files)

| Era | Last dental visit | Teeth condition | Pain | Dentures | Brushing |
|-----|-------------------|-----------------|------|----------|----------|
| 2001 | DENA_132 | - | - | - | - |
| 2003 | DENC_132 | OH1C_20 | OH1C_22 | OH2C_20 | OH2C_30 |
| 2005 | DENE_132 | - | - | OH2E_20 | OH2E_30 |
| 2007-08 | DEN_132 | OH1_20 | OH1_22 | - | - |
| 2009-10 | DEN_132 | - | - | OH2_20 | OH2_30 |
| 2011 | DEN_132 | - | - | - | - |
| 2012 | **Missing** | **Missing** | **Missing** | **Missing** | **Missing** |
| 2013-14 | DEN_132 | OH1_20 | OH1_22 | OH2_20 | OH2_30 |
| 2015-16 | CHP_080* | - | - | - | - |
| 2017-18 | **DEN_035** | OHT_015 | - | DEN_020 | DEN_010 |

*CHP_080 = "Consulted dental professional in past 12 months" (Yes/No) - NOT equivalent to DEN_132 categories.

## Key findings

### 1. DEN_132 renamed to DEN_035 in 2017-2018

- **DEN_132** (2001-2014): "Last time visited dental professional" with categories (<1 year, 1-2 years, etc.)
- **DEN_035** (2017-2018): Same concept, renamed variable
- Available in CCHS201718_ONT_SHARE

### 2. OHT_015 is NOT equivalent to DEN_132

| Variable | Concept | Question type |
|----------|---------|---------------|
| DEN_132 / DEN_035 | Service utilisation | "When was the last time you visited...?" |
| OHT_015 | Health status | "In general, would you say the health of your teeth and mouth is...?" |

OHT_015 is the 2017+ equivalent of **OH1_20** (self-perceived oral health), not DEN_132.

### 3. 2015-2016 harmonisation limitation

CHP_080 cannot be mapped to DEN_132 categories:
- CHP_080 = Yes → DEN_132 ≈ "Less than 1 year"
- CHP_080 = No → DEN_132 ≈ "1 year or more" (pooled)

**Recommendation**: Collapse DEN_132 to binary (visited <1yr vs ≥1yr) to harmonise with 2015-2016.

### 4. 2011-2012 gap explained

- **2011**: DEN_132 exists in Annual Share file (likely Core content that year)
- **2012**: Oral health modules dropped
- **2011-2012 Combined**: ~50% of sample has missing oral health data

**Recommendation**: Use 2011 Annual weights only, or mark as missing for combined cycle.

### 5. OH2_30 split in 2017-2018 (brushing frequency)

| Variable | Content |
|----------|---------|
| DEN_010 | Frequency (numeric: 2, 3, 7, etc.) |
| DEN_010A | Reporting period (1=Day, 2=Week, 3=Month, 4=Year) |

**Harmonisation formula**:
- If DEN_010A = 1 (Day): Frequency = DEN_010
- If DEN_010A = 2 (Week): Frequency = DEN_010 / 7
- If DEN_010A = 3 (Month): Frequency = DEN_010 / 30
- If DEN_010A = 4 (Year): Frequency = DEN_010 / 365

### 6. Ontario module selection pattern

| Era | Condition module (OH1) | Care module (OH2/DEN) |
|-----|------------------------|----------------------|
| 2003 | Selected | Selected |
| 2005 | Not selected | Selected |
| 2007-2008 | Selected | Not selected |
| 2009-2010 | Not selected | Selected |
| 2011-2012 | Not selected | Not selected |
| 2013-2014 | Selected | Selected |

## Implications for PR #158

### Current PR configuration
```
DEN_132: cchs2001_m, cchs2003_m, cchs2005_m, cchs2009_2010_m, cchs2011_2012_m, cchs2013_2014_m
```

### Issues identified

1. **2011-2012**: Should be `cchs2011_m` only (not combined) since 2012 has no data
2. **2017-2018 missing**: Should add `cchs2017_2018_m::DEN_035` as source

### Recommended changes

```r
# Updated variableStart for DEN_132
cchs2001_m::DENA_132, cchs2003_m::DENC_132, cchs2005_m::DENE_132,
cchs2009_2010_m::DEN_132, cchs2011_m::DEN_132, cchs2013_2014_m::DEN_132,
cchs2017_2018_m::DEN_035
```

Or create separate entry for 2017-2018 if categories differ.

## Action items

- [ ] Verify DEN_035 category values match DEN_132 categories
- [ ] Update PR #158 to handle 2011-2012 correctly
- [ ] Consider adding 2017-2018 with DEN_035 mapping
- [ ] Add note about 2015-2016 gap (binary only via CHP_080)
- [ ] Document OH2_30 → DEN_010/DEN_010A derivation for future harmonisation
