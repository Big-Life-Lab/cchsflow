# Physical activity harmonization summary report

**CEP-003: Physical Activity Variable Harmonization**
**Date**: 2026-01-18
**Purpose**: Cross-validation with CCHS DDI documentation via Google Notebook LLM
**Status**: Cross-validated with DDI documentation (see Part 7)

---

## Executive summary

This report summarizes the L0-L4 harmonization analysis for physical activity (PA) variables in cchsflow. The analysis identified coverage gaps, proposed extensions to 2019-2020, added a new WHO classification variable, and corrected worksheet errors.

### Key findings

1. **PA measurement evolved through three distinct eras** with different variable naming and conceptual approaches
2. **`energy_exp` and `active_transport`** are derived variables that bridge across all eras (2001-2020)
3. **2019-2020 worksheet gaps** existed for active transport inputs and derived energy expenditure (note: Active Transportation was Optional Content in 2019-2020, selected by some but not all provinces/territories)
4. **PAADVWHO** (WHO physical activity classification) was not previously harmonized despite being available 2015-2022
5. **2021 re-integration**: The 2021 cycle saw return of core PA modules including active transport data

---

## Part 1: Documentation assessment (L0)

### CCHS physical activity variable eras

| Era | Years | Prefix pattern | Conceptual approach |
|-----|-------|----------------|---------------------|
| Era 1 | 2001-2005 | `PAC{A/C/E}_*` | Activity-type based (leisure activities with METs) |
| Era 2 | 2007-2014 | `PAC_*`, `PACD*` | Activity-type based (continued) |
| Era 3 | 2015-2022 | `PAA_*`, `PAADV*`, `PAYDV*` | Time-based (minutes at intensity levels) |
| Era 4 | 2023+ | `MPA_*`, `MPAD*` | Redesigned module (not yet harmonized) |

### Critical 2015 redesign

The 2015 CCHS redesign fundamentally changed PA measurement:

**Pre-2015 approach:**
- Asked about specific leisure activities (PAC_1A through PAC_1X)
- Calculated METs based on activity type, frequency, duration
- StatCan derived variable: `PACDEE` (daily energy expenditure)

**Post-2015 approach:**
- Asks about total time at intensity levels (vigorous, moderate)
- Separate modules for adults (18+) using `PAA_*`/`PAADV*` and youth (12-17) using `PAYDV*`
- No direct equivalent to PACDEE; requires custom derivation

### Age-specific variables (2015+)

| Age group | Variable prefix | Example variables |
|-----------|-----------------|-------------------|
| Adults 18+ | `PAA_*`, `PAADV*` | PAA_045, PAA_050, PAADVVIG, PAADVDYS, PAADVTRV |
| Youth 12-17 | `PAYDV*` | PAYDVVIG, PAYDVDYS, PAYDVTTR, PAYDVTOA, PAYDVADL |

---

## Part 2: Variable concordance (L1)

### Currently harmonized PA variables in cchsflow

| Variable | Type | Cycles | Description |
|----------|------|--------|-------------|
| **Derived variables** | | | |
| `active_transport` | Continuous | 2001-2018 | Daily active transportation (minutes) |
| `energy_exp` | Continuous | 2001-2018 | Daily energy expenditure (METs) |
| **Pre-2015 source variables** | | | |
| `PACDEE` | Continuous | 2001-2014 | StatCan daily energy expenditure |
| `PACDEE_cat3` | Categorical | 2001-2014 | 3-level activity classification |
| `PACFLEI` | Categorical | 2001-2014 | Leisure physical activity flag |
| `PAC_4A` | Categorical | 2001-2005 | Time walking to work/school |
| `PAC_4A_cont` | Continuous | 2001-2005 | Midpoint imputed walking time |
| `PAC_4B` | Categorical | 2001-2005 | Time biking to work/school |
| `PAC_4B_cont` | Continuous | 2001-2005 | Midpoint imputed biking time |
| `PAC_7` | Categorical | 2007-2014 | Walked to work/school (Y/N) |
| `PAC_7A` | Continuous | 2007-2014 | Number of times walked |
| `PAC_7B` | Categorical | 2007-2014 | Time per walk session |
| `PAC_7B_cont` | Continuous | 2007-2014 | Midpoint imputed walk time |
| `PAC_8` | Categorical | 2007-2014 | Biked to work/school (Y/N) |
| `PAC_8A` | Continuous | 2007-2014 | Number of times biked |
| `PAC_8B` | Categorical | 2007-2014 | Time per bike session |
| `PAC_8B_cont` | Continuous | 2007-2014 | Midpoint imputed bike time |
| **Post-2015 source variables** | | | |
| `PAA_045` | Continuous | 2015-2020 | Sweat/breathe hard exercises (hours) - adults |
| `PAA_050` | Continuous | 2015-2020 | Sweat/breathe hard exercises (minutes) - adults |
| `PAA_075` | Continuous | 2015-2020 | Other physical activities (hours) - adults |
| `PAA_080` | Continuous | 2015-2020 | Other physical activities (minutes) - adults |
| `PAADVDYS` | Continuous | 2015-2020 | Active days - adults |
| `PAADVVIG` | Continuous | 2015-2022 | Vigorous activity minutes - adults |
| `PAADVTRV` | Continuous | 2015-2018 | Active transportation - adults |
| `PAYDVADL` | Continuous | 2015-2020 | Leisure activities - youth |
| `PAYDVDYS` | Continuous | 2015-2020 | Active days - youth |
| `PAYDVTOA` | Continuous | 2015-2020 | Other activities - youth |
| `PAYDVTTR` | Continuous | 2015-2018 | Active transportation - youth |
| `PAYDVVIG` | Continuous | 2015-2020 | Vigorous activities - youth |

### Coverage gaps identified

| Gap | Details | Impact |
|-----|---------|--------|
| **PAADVTRV 2019-2020** | Adult active transport not in cchsflow worksheets | `active_transport` incomplete |
| **PAYDVTTR 2019-2020** | Youth active transport not in cchsflow worksheets | `active_transport` incomplete |
| **energy_exp 2019-2020** | Derived variable not extended (inputs exist) | Cross-cycle analysis incomplete |
| **PAADVWHO not harmonized** | WHO classification available 2015-2022 | Missing key categorical outcome |
| **2021-2022 coverage** | Limited variable extension | Most PA variables stop at 2020 |
| **2023 MPA_* module** | Entirely new question structure | Requires separate harmonization effort |

> **Important clarification (from DDI validation)**: In 2019-2020, Active Transportation questions were **Optional Content** selected by only certain provinces/territories, rather than being part of the core survey administered to all respondents. This is distinct from variables being "missing" from the survey design entirely. The 2021 cycle saw a return of many core PA modules, including active transport data.

---

## Part 3: Semantic mapping (L2)

### Derived variable: `active_transport`

Bridges three measurement eras with era-specific functions:

| Era | Function | Input variables | Calculation |
|-----|----------|-----------------|-------------|
| 2001-2005 | `active_transport1_fun()` | PAC_4A_cont, PAC_4B_cont | (walk + bike hours) / 7 days |
| 2007-2014 | `active_transport2_fun()` | PAC_7, PAC_7A, PAC_7B_cont, PAC_8, PAC_8A, PAC_8B_cont | (walk sessions × time + bike sessions × time) / 90 days |
| 2015-2020 | `active_transport3_fun()` | PAYDVTTR, PAADVTRV | (youth + adult minutes) / 7 days |

**Units**: Minutes per day (standardized across eras)

### Derived variable: `energy_exp`

Bridges pre-2015 and post-2015 measurement approaches:

| Era | Method | Source |
|-----|--------|--------|
| 2001-2014 | Direct copy | PACDEE (StatCan derived variable) |
| 2015-2020 | Custom calculation | `calculate_energy_expenditure()` function |

**Post-2015 calculation** (from `R/physical-activity.R`):

```
EE = ((leisure_minutes - vigorous_minutes) × 3 METs + vigorous_minutes × 6 METs) / 7 / 60

Where:
- Adults: leisure = PAA_045×60 + PAA_050 + PAA_075×60 + PAA_080
- Youth: leisure = PAYDVTOA + PAYDVADL
- Vigorous from PAADVVIG (adults) or PAYDVVIG (youth)
- Active days from PAADVDYS (adults) or PAYDVDYS (youth)
```

### PAADVWHO (WHO physical activity classification)

StatCan-derived categorical variable based on WHO guidelines:

| Value | Label | Description |
|-------|-------|-------------|
| 1 | Level 1 - Most active | Meets WHO guidelines with margin |
| 2 | Level 2 | Meets WHO guidelines |
| 3 | Level 3 | Partially meets guidelines |
| 4 | Level 4 - Least active | Does not meet guidelines |
| 6 | Valid skip | Not applicable |
| 7-9 | Missing | Don't know/Refusal/Not stated |

**Availability**: PUMF 2015-2016, 2017-2018, 2019-2020, 2022

---

## Part 4: Worksheet review (L3)

### Issues identified in current worksheets

| Issue | Variable | Problem | Fix |
|-------|----------|---------|-----|
| Label error | PAC_4B | Says "walking" but variable is for biking | Changed to "biking" |
| Label error | PAC_4B_cont | Same as PAC_4B | Changed to "biking" |
| dummyVariable naming | PACFLEI | Uses `PACFLEI_cat_cat6_*` (double "cat") | Changed to `PACFLEI_cat2_*` |
| Missing coverage | PAADVTRV | Stops at 2017-2018 | Extended to 2019-2020 |
| Missing coverage | PAYDVTTR | Stops at 2017-2018 | Extended to 2019-2020 |
| Missing coverage | active_transport | Stops at 2017-2018 | Extended to 2019-2020 |
| Missing coverage | energy_exp | Stops at 2017-2018 | Extended to 2019-2020 |
| Not harmonized | PAADVWHO | Not in worksheets | Added for 2015-2022 |

### Staged worksheet updates

**File: pa_variables_update.csv** (7 rows)

| Variable | Change |
|----------|--------|
| PAADVTRV | databaseStart extended to include `cchs2019_2020_p` |
| PAYDVTTR | databaseStart extended to include `cchs2019_2020_p` |
| active_transport | databaseStart extended to include `cchs2019_2020_p` |
| energy_exp | databaseStart extended to include `cchs2019_2020_p` |
| PAADVWHO | New variable added (2015-2022) |
| PAC_4B | Label corrected: "walking" → "biking" |
| PACFLEI | Label typo corrected |

**File: pa_variable_details_update.csv** (41 rows)

| Variable | Rows | Change |
|----------|------|--------|
| PAADVTRV | 4 | Added cchs2019_2020_p mappings |
| PAYDVTTR | 4 | Added cchs2019_2020_p mappings |
| active_transport | 1 | Added cchs2019_2020_p with Func::active_transport3_fun |
| energy_exp | 2 | Added cchs2019_2020_p with Func::calculate_energy_expenditure |
| PAADVWHO | 7 | New categorical variable (4 categories + missing) |
| PACFLEI | 5 | Fixed dummyVariable naming |
| PAC_4B | 9 | Fixed labels |
| PAC_4B_cont | 9 | Fixed labels |

---

## Part 5: Derived variable specifications (L4)

### Existing functions (no changes needed)

| Function | File | Status |
|----------|------|--------|
| `active_transport1_fun()` | R/active-transportation.R | Works for 2001-2005 |
| `active_transport2_fun()` | R/active-transportation.R | Works for 2007-2014 |
| `active_transport3_fun()` | R/active-transportation.R | Works for 2015-2020 (after worksheet extension) |
| `calculate_energy_expenditure()` | R/physical-activity.R | Works for 2015-2020 (after worksheet extension) |

### Key insight: No function changes required

The existing derived variable functions work correctly for 2019-2020 data. The gap was in the **worksheet coverage**, not the function logic. Once worksheets are extended to include `cchs2019_2020_p`, the existing functions will process the data correctly.

---

## Part 6: Validation questions for DDI cross-check

### Questions for Google Notebook LLM to validate

1. **PAADVTRV availability**: Is PAADVTRV (adult active transportation minutes) available in CCHS 2019-2020 PUMF? What are the valid value ranges and missing codes?

2. **PAYDVTTR availability**: Is PAYDVTTR (youth active transportation minutes) available in CCHS 2019-2020 PUMF? What are the valid value ranges and missing codes?

3. **PAADVWHO categories**: Confirm the category labels for PAADVWHO in 2015-2022 cycles:
   - Is Level 1 "Most active" and Level 4 "Least active"?
   - What is the valid skip code (we assume 6)?
   - What are the missing codes (we assume 7-9)?

4. **PAC_4B question wording**: Confirm that PAC_4B (and PACA_4B, PACC_4B, PACE_4B) asks about **biking** to work/school, not walking. The harmonized label incorrectly said "walking".

5. **PACFLEI binary coding**: Confirm PACFLEI is a binary variable (1=Yes participated, 2=No) and not a 6-category variable as the dummyVariable naming suggested.

6. **energy_exp inputs for 2019-2020**: Confirm these variables are available in 2019-2020 PUMF:
   - PAA_045, PAA_050, PAA_075, PAA_080
   - PAADVDYS, PAADVVIG
   - PAYDVTOA, PAYDVADL, PAYDVVIG, PAYDVDYS

7. **2022 PA variable availability**: Which PA variables are available in CCHS 2022 PUMF? We have:
   - PAADVVIG (confirmed)
   - PAADVWHO (to be added)
   - Are there others?

8. **Missing data codes for 2015+ variables**: Confirm the standard missing codes for PAADV*/PAYDV* variables:
   - 99996 = Valid skip/Not applicable
   - 99997 = Don't know
   - 99998 = Refusal
   - 99999 = Not stated

---

## Part 7: Cross-validation results

### DDI validation summary (2026-01-18)

The report was cross-validated against CCHS DDI documentation using Google Notebook LLM with a RAG of all CCHS documentation. Overall assessment: **Mostly accurate** with specific clarifications incorporated.

### Validated as accurate

| Item | Validation status |
|------|-------------------|
| Era 1 (2001-2005) definitions | ✓ Confirmed - PAC/PACD variables, leisure activity lists with METs |
| Era 2 (2007-2014) definitions | ✓ Confirmed - Continued PAC module structure |
| Era 3 (2015-2022) definitions | ✓ Confirmed - PAA/PAADV redesign, minute-based reporting |
| PAADVWHO availability | ✓ Confirmed - Available in DV specifications for 2017, 2018 and later |
| Coverage gaps in 2019-2020 | ✓ Confirmed with clarification (see below) |

### Clarifications incorporated

1. **Active Transport in 2019-2020**: The original report stated these were "coverage gaps." The DDI validation clarified that Active Transportation was **Optional Content** in 2019-2020, selected by only certain provinces/territories (e.g., Yukon, Northwest Territories, Nunavut often select different modules). This is distinct from variables being entirely absent from the survey design.

2. **2021 re-integration**: The 2021 User Guide confirms that "Physical activities" returned as a module, often capturing active transport data again. This closes the gap identified for 2019-2020.

3. **Variable suffix variations**: In some years, vigorous activity minutes may use `PAADVVIGM` (with `_M` suffix) rather than `PAADVVIG`. The harmonization should account for these naming variations.

### Recommendations from validation

| Recommendation | Status | Action |
|----------------|--------|--------|
| Clarify Active Transport as Optional Content in 2019-2020 | ✓ Incorporated | Updated coverage gaps section |
| Note 2021 re-integration of PA modules | ✓ Incorporated | Added to key findings |
| Check for `_M` suffix variations on PAADVVIG | ⚠ To verify | Review variableStart mappings |

### Outstanding questions for further verification

1. **PAADVVIGM vs PAADVVIG**: Need to confirm which cycles use which naming convention and ensure variableStart mappings handle both
2. **2022 PUMF variable list**: Confirm complete list of available PA variables beyond PAADVVIG and PAADVWHO
3. **Provincial coverage for 2019-2020 Active Transport**: Which specific provinces/territories selected this Optional Content?

---

## Part 8: Broader harmonization opportunities

### Additional validation feedback (2026-01-18)

The Notebook LLM provided strategic recommendations for maximizing harmonization opportunities, both within PA and across other CCHS domains.

### PA-specific harmonization strategy

| Variable | Opportunity | Strategic value |
|----------|-------------|-----------------|
| **PAADVWHO** | Standardize WHO classification 2015-2022 | Allows "Meeting Guidelines" analysis (not compatible with Era 1-2 "Active/Inactive" groupings) |
| **energy_exp** | Bridge variable across all eras (2001-2022) | Enables continuous trend analysis despite questionnaire format changes |
| **active_transport** | Long-term commuting/travel analysis | Valid across Era 2-3, with 2019-2020 provincial flagging |

### Key strategic recommendations

1. **Prioritize derived variables over index variables**
   - Focus on calculating total Energy Expenditure (KKD) rather than relying on "Index" variables (PACDPAI vs PAADV)
   - Index thresholds changed between Era 2 and Era 3, making direct comparison problematic
   - Derived calculations using raw inputs provide more consistent cross-era comparability

2. **Create era flags for PAADVWHO**
   - Add explicit flag that WHO classification applies only to Era 3 (2015+)
   - Era 1 & 2 did not collect minute-level data required for strict WHO guideline calculation
   - Users should be warned against comparing PAADVWHO with pre-2015 "Active/Inactive" classifications

3. **Handle 2019-2020 Active Transport gap correctly**
   - Map available provincial data where collected
   - Code missing provinces as `NA::a` (Not Asked/Valid Skip) rather than `0`
   - This prevents skewing national estimates by incorrectly treating "not asked" as "no active transport"

### Broader CCHS harmonized content opportunities

The CCHS User Guides (2017-2023) identify several "Harmonized Content" modules standardized across Statistics Canada surveys:

| Domain | Module | Harmonization potential |
|--------|--------|------------------------|
| **Socio-demographics** | Age (ANC), Sex (SEX/GDR), Marital Status (DHH) | High - core identifiers |
| **Social determinants** | Education (EDU), Income (INC), Labour Force (LMA) | High - aligned with Census |
| **Chronic conditions** | CCC module (diabetes, asthma, hypertension) | Medium - structure consistent, specific conditions vary |
| **Mental health** | Self-rated mental health, stress | Medium - question wording stable |
| **Substance use** | Smoking, alcohol | Variable - major redesigns in some cycles |

### Implementation priorities for cchsflow

Based on this analysis, recommended prioritization:

1. **Immediate (this PR)**: Complete PA extensions (energy_exp, active_transport, PAADVWHO)
2. **Short-term**: Review existing socio-demographic harmonizations for Era 3 compatibility
3. **Medium-term**: Extend chronic conditions module coverage to 2019-2022
4. **Long-term**: Develop Era 4 (2023+ MPA_*) harmonization strategy

---

## Appendix A: Variable naming conventions

### Cycle-specific prefixes (pre-2015)

| Cycle | Prefix | Example |
|-------|--------|---------|
| 2001 | PACA | PACA_4B, PACADEE, PACAFLEI |
| 2003 | PACC | PACC_4B, PACCDEE, PACCFLEI |
| 2005 | PACE | PACE_4B, PACEDEE, PACEFLEI |
| 2007+ | PAC | PAC_7, PACDEE, PACFLEI |

### Standard variable patterns (2015+)

| Pattern | Meaning | Example |
|---------|---------|---------|
| PAA_### | Adult PA question | PAA_045, PAA_050 |
| PAADV### | Adult derived variable | PAADVVIG, PAADVDYS |
| PAYDV### | Youth derived variable | PAYDVVIG, PAYDVDYS |
| *VIG | Vigorous activity | PAADVVIG, PAYDVVIG |
| *DYS | Active days | PAADVDYS, PAYDVDYS |
| *TRV/*TTR | Active transportation | PAADVTRV, PAYDVTTR |
| *WHO | WHO classification | PAADVWHO |

> **Note on suffix variations**: In some years (e.g., 2015), the precise DV name for vigorous activity minutes may be `PAADVVIGM` (with `_M` suffix for minutes) vs `PAADVVIG` (categorical). The harmonization scripts should account for these suffix variations when mapping source variables.

---

## Appendix B: Database naming conventions

| Pattern | Meaning | Example |
|---------|---------|---------|
| cchs####_p | PUMF file | cchs2019_2020_p |
| cchs####_m | Master file | cchs2009_m |
| cchs####_s | Shared file (deprecated) | cchs2009_s |

---

## Appendix C: File locations

| File | Path | Description |
|------|------|-------------|
| L0 Assessment | ceps/cep-003-physical-activity/L0_documentation_assessment.md | DDI review |
| L1 Concordance | ceps/cep-003-physical-activity/L1_variable_concordance.md | Variable mapping |
| L2 Mapping | ceps/cep-003-physical-activity/L2_semantic_mapping.md | Semantic relationships |
| L3 Review | ceps/cep-003-physical-activity/L3_worksheet_review.md | Worksheet audit |
| L4 Specs | ceps/cep-003-physical-activity/L4_dv_specifications.md | DV requirements |
| Staged variables | ceps/cep-003-physical-activity/pa_variables_update.csv | variables.csv updates |
| Staged details | ceps/cep-003-physical-activity/pa_variable_details_update.csv | variable_details.csv updates |
| Integration test | ceps/cep-003-physical-activity/appendix-integration-test.qmd | Validation code |

---

*Report generated for cross-validation with CCHS DDI documentation*
