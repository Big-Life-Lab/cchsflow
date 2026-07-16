# Ontario oral health variable availability

**Date**: 2026-01-26
**Source**: ICES CCHS Dictionary v1.0.0 (14,005 variables)

## Summary

Oral health variables in Ontario Linked/Share files have **era-specific naming** that must be accounted for in harmonization.

## Dental visit variable (DEN_132 equivalents)

| Cycle | Variable | Ontario Linked | Ontario Share |
|-------|----------|----------------|---------------|
| 2001 | DENA_132 | Yes | Yes |
| 2003 | DENC_132 | Yes | Yes |
| 2005 | DENE_132 | Yes | Yes |
| 2007-2008 | - | **No** | **No** |
| 2009-2010 | DEN_132 | Yes | Yes |
| 2011 | DEN_132 | **No** | Yes (annual only) |
| 2011-2012 | DEN_132 | **No** | **No** |
| 2013-2014 | DEN_132 | Yes | Yes |
| 2015-2016 | CHP_080* | **No** | Yes |
| 2017-2018 | **?** | **No** | **No** |

*CHP_080 asks about dental visit in past 12 months only - not equivalent to DEN_132's categorical time ranges.

## Teeth condition (OH1_20/OH1_22 equivalents)

| Cycle | Variables | Ontario Linked | Ontario Share |
|-------|-----------|----------------|---------------|
| 2001 | - | **No** | **No** |
| 2003 | OH1C_20, OH1C_22 | Yes | Yes |
| 2005 | - | **No** | **No** |
| 2007-2008 | OH1_20, OH1_22 | Yes | Yes |
| 2009-2010 | - | **No** | **No** |
| 2011-2012 | - | **No** | **No** |
| 2013-2014 | OH1_20, OH1_22 | Yes | Yes |
| 2015-2016 | - | **No** | **No** |
| 2017-2018 | OHT_015 | **No** | Yes |

## Dentures/brushing (OH2_20/OH2_21/OH2_30 equivalents)

| Cycle | Variables | Ontario Linked | Ontario Share |
|-------|-----------|----------------|---------------|
| 2001 | - | **No** | **No** |
| 2003 | OH2C_20, OH2C_21, OH2C_30 | Yes | Yes |
| 2005 | OH2E_20, OH2E_21, OH2E_30 | Yes | Yes |
| 2007-2008 | - | **No** | **No** |
| 2009-2010 | OH2_20, OH2_21, OH2_30 | Yes | Yes |
| 2011-2012 | - | **No** | **No** |
| 2013-2014 | OH2_20, OH2_21, OH2_30 | Yes | Yes |
| 2015-2016 | - | **No** | **No** |
| 2017-2018 | DEN_010, DEN_010A | **No** | Yes |

## Key findings

### 1. Era-specific variable naming
- 2001: Suffix A (DENA_132)
- 2003: Suffix C (DENC_132, OH1C_20, OH2C_30)
- 2005: Suffix E (DENE_132, OH2E_20, OH2E_30)
- 2007+: Standard naming (DEN_132, OH1_20, OH2_20)

### 2. Module availability pattern
- **OH1 (teeth condition)**: 2003, 2007-2008, 2013-2014 only
- **OH2 (dentures/brushing)**: 2003, 2005, 2009-2010, 2013-2014 only
- **DEN_132**: Most cycles except 2007-2008 and 2015+

### 3. 2011-2012 gap
- DEN_132 exists in 2011 annual Share file but NOT in combined 2011-2012 files
- No OH1 or OH2 variables in any 2011-2012 files

### 4. 2015-2016 replacement
- CHP_080 replaces DEN_132 but measures different concept (12-month window vs categorical time ranges)
- No OH1 or OH2 equivalents

### 5. 2017-2018 changes
- OH2_30 (brushing frequency) split into DEN_010 (frequency) + DEN_010A (period)
- OHT_015 appears to replace OH1 module
- **DEN_132 not found in Ontario Share** - needs verification

## GoogleLM cross-check results

**Discrepancy resolved**: Rafidul was correct that 2017-2018 has the dental visit concept, but the variable was **renamed from DEN_132 to DEN_035**.

### Corrected findings

| Issue | Resolution |
|-------|------------|
| 2017-2018 DEN_132 | **Renamed to DEN_035** - same concept, new name |
| 2015-2016 gap | **Confirmed** - Only CHP_080 (binary Yes/No for 12-month visit) |
| 2011-2012 gap | **Confirmed** - 2011 Annual only; oral health dropped in 2012 |
| OHT_015 | **Different concept** - measures oral health status, not utilisation |

### Ontario module selection pattern (confirmed)

| Era | Condition (OH1) | Care (OH2/DEN) |
|-----|-----------------|----------------|
| 2003 | Selected | Selected |
| 2005 | Not selected | Selected |
| 2007-2008 | Selected | Not selected |
| 2009-2010 | Not selected | Selected |
| 2011-2012 | Not selected | Not selected |
| 2013-2014 | Selected | Selected |

## Implications for PR #158

PR #158 adds DEN_132 for Master files: 2001, 2003, 2005, 2009-2010, 2011-2012, 2013-2014

### Issues identified

1. **2011-2012**: Should be `cchs2011_m` only - 2012 has no oral health data
2. **2017-2018 missing**: Should add `cchs2017_2018_m::DEN_035` as source

### Recommendations

1. ⚠️ Change `cchs2011_2012_m` to `cchs2011_m`
2. ✓ Add `cchs2017_2018_m::DEN_035` mapping
3. 📝 Add note about 2015-2016 binary-only availability (CHP_080)
