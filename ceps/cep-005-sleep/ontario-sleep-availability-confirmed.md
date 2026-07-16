# Ontario sleep variable availability: confirmed findings

**Date**: 2026-01-26
**Sources**: GoogleLM NotebookLM cross-check, ICES CCHS Dictionary v1.0.0, PUMF direct analysis

## Executive summary

Sleep variable availability for Ontario has a **13-year gap (2003-2014)** in both PUMF and Ontario Linked/Share files. This is confirmed by three independent sources.

### Data availability epochs

| Epoch | Years | File types | Variables | Scale |
|-------|-------|------------|-----------|-------|
| Early era | 2001-2002 | PUMF, ONT_LINK | GENA_03-06, GENB_03-05 | 3-point |
| **Gap** | **2003-2014** | **None for Ontario** | **Missing** | **N/A** |
| Modern era | 2015-2018 | PUMF (2015-16 only), ONT_SHARE | SLP_005, SLP_010-020 | 5-point |

## Detailed findings

### PUMF availability (confirmed via direct .RData analysis)

| Cycle | Ontario 50+ N | Valid sleep | DOSLP flag | Status |
|-------|---------------|-------------|------------|--------|
| 2001 | 9,930 | 9,393 (94.6%) | No flag | **Available** |
| 2003 | - | - | - | Not in PUMF |
| 2005 | - | - | - | Not in PUMF |
| 2007-2008 | 17,861 | 0 (0%) | No flag | Ontario excluded |
| 2009-2010 | - | - | - | Not in PUMF |
| 2011-2012 | 19,730 | 0 (0%) | NA for Ontario | Ontario excluded |
| 2013-2014 | 21,031 | 0 (0%) | 2 (excluded) | Ontario excluded |
| 2015-2016 | 15,139 | 14,366 (94.9%) | 1 (included) | **Available** |
| 2017-2018 | 15,905 | 0 (0%) | 2 (excluded) | Ontario excluded |

### Ontario Linked/Share file availability (confirmed via ICES Dictionary)

| Period | Hours | Trouble | Refreshing | Difficulty awake | Dataset |
|--------|-------|---------|------------|------------------|---------|
| 2001 | GENA_03 | GENA_04 | GENA_05 | GENA_06 | CCHS2001_ONT_LINK_11 |
| 2002 | GENB_03 | GENB_04 | GENB_05 | Not found | CCHS2002_ONT_LINK_12 |
| 2003 | Missing | DPSC_09* | Missing | Missing | CCHS2003_ONT_LINK_21 |
| 2005 | Missing | Missing | Missing | Missing | N/A |
| 2007-2014 | Missing | Missing | Missing | Missing | N/A |
| 2015-2016 | SLP_005 | SLP_010 | SLP_015 | SLP_020 | CCHS201516_ONT_SHARE |
| 2017-2018 | SLP_005 | SLP_010 | SLP_015 | SLP_020 | CCHS201718_ONT_SHARE |

*DPSC_09 is from Depression module ("Did you have more trouble falling asleep than you usually do?") - measures **change**, not status.

### Exception: 2012 Mental Health survey

The CCHS 2012 Mental Health focused survey (CCHS2012_MH_ONT_LINK) contains DEP_26G ("Symptom - trouble falling/staying asleep"), but this is:
- Only available in the Mental Health supplement, not Annual
- A depression symptom question, not the standard sleep module

## GoogleLM confirmation

GoogleLM confirmed:

1. **Sleep was Optional Content** from 2007-2018
2. **Ontario did NOT select** the sleep module for 2007-2014 and 2017-2018
3. **Ontario DID select** the sleep module for 2015-2016 only
4. **Scale inversion**: 2001 (lower = worse), 2007+ (higher = worse)
5. **Harmonization**: Cannot directly pool; recommend binary "Any trouble" vs "No trouble"

## Scale incompatibility

### 2001 (3-point scale)
- 1 = Most of the time (worst)
- 2 = Sometimes
- 3 = Never (best)
- **Direction**: Lower = more trouble

### 2007+ (5-point scale)
- 1 = None of the time (best)
- 2 = A little of the time
- 3 = Some of the time
- 4 = Most of the time
- 5 = All of the time (worst)
- **Direction**: Higher = more trouble

### Harmonization recommendation

Create binary variable:
- **2001**: 1 & 2 ("Most" + "Sometimes") = Yes; 3 ("Never") = No
- **2007+**: 2-5 ("A little" to "All") = Yes; 1 ("None") = No

## Implications for research

### For Ontario dementia research (PUMF)

**Usable cycles**: Only 2001 and 2015-2016

**Gap**: 14 years (2002-2014) of missing sleep data

**Cannot**:
- Pool 2001 with 2015-2016 for quality variables (incompatible scales)
- Create continuous time series for sleep trends

**Can**:
- Compare two time points (2001 vs 2015-2016) for binary sleep trouble
- Pool hours of sleep (continuous) with caution

### For Ontario research using ICES (Linked files)

**Usable cycles**: 2001, 2002, 2015-2016, 2017-2018

**Advantage over PUMF**: 2002 and 2017-2018 available in ONT_SHARE

**Gap**: Still 13 years (2003-2014) missing

## Verification checklist

- [x] 2003, 2005, 2009-2010 PUMF do NOT have sleep variables
- [x] 2001 sleep variables are GENA_03, GENA_04, GENA_05, GENA_06
- [x] 2001 uses 3-point scale (incompatible with 2007+)
- [x] 2007-2008 introduces SLP_01 through SLP_04 (but NOT for Ontario)
- [x] 2011-2014 uses SLPG01 for hours and SLP_02-04 for quality (but NOT for Ontario)
- [x] 2015-2018 uses SLPG005, SLP_010, SLP_015, SLP_020
- [x] Ontario data is **NOT available** for 2007-2014 in PUMF or Linked files
- [x] Ontario data is **NOT available** for 2017-2018 in PUMF (but IS in ONT_SHARE)
- [x] Scale inversion confirmed between 2001 and 2007+
- [x] ICES Dictionary confirms same gap as PUMF analysis

## Source documentation

1. **PUMF direct analysis**: ceps/cep-005-sleep/availability-matrix.qmd
2. **GoogleLM cross-check**: ceps/cep-005-sleep/google-lm-crosscheck-prompt.md
3. **ICES Dictionary**: cchsflow-docs/data/catalog/ices_cchs_availability_matrix.csv (v1.0.0)
