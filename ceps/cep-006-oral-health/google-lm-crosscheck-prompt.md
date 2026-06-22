# GoogleLM cross-check prompt: CCHS oral health variables

## Context

We are reviewing PR #158 which adds `DEN_132` (Last time visited dental professional) for Master files. A team member (Rafidul) has provided detailed notes on oral health variable availability that we need to verify against Statistics Canada documentation.

---

## Prompt

I need to verify the availability of CCHS oral health variables across survey cycles, with particular focus on **Ontario Linked/Share file** availability.

### Variables to verify

| Variable | Description |
|----------|-------------|
| DEN_132 | Last time visited dental professional |
| OH1_20 | Natural teeth condition - self-assessed |
| OH1_22 | Dental/oral problems - pain |
| OH2_20 | Wears dentures |
| OH2_21 | Number of natural teeth |
| OH2_30 | Brushing teeth frequency |
| CHP_080 | Consulted dental professional - 12 months (2015-2016 replacement for DEN_132) |
| OHT_015 | Oral health variable (2017-2018) |
| DEN_010 / DEN_010A | Brushing frequency split variables (2017-2018) |

### ICES Dictionary findings (Ontario Linked/Share files)

We checked the ICES CCHS Dictionary (v1.0.0, 14,005 variables) and found **era-specific variable naming**:

| Cycle | Dental visit | OH1 (teeth condition) | OH2 (dentures/brushing) | Source prefix |
|-------|-------------|----------------------|------------------------|---------------|
| 2001 | DENA_132 | Not found | Not found | A suffix |
| 2003 | DENC_132 | OH1C_20, OH1C_22 | OH2C_20, OH2C_21, OH2C_30 | C suffix |
| 2005 | DENE_132 | **Not found** | OH2E_20, OH2E_21, OH2E_30 | E suffix |
| 2007-2008 | **Not found** | OH1_20, OH1_22 | **Not found** | Standard |
| 2009-2010 | DEN_132 | **Not found** | OH2_20, OH2_21, OH2_30 | Standard |
| 2011-2012 | DEN_132* | **Not found** | **Not found** | *Annual only |
| 2013-2014 | DEN_132 | OH1_20, OH1_22 | OH2_20, OH2_21, OH2_30 | Standard |
| 2015-2016 | CHP_080 | **Not found** | **Not found** | Replacement |
| 2017-2018 | **Not found** | OHT_015 | DEN_010, DEN_010A | New module |

*DEN_132 in 2011 annual Share file only, not in 2011-2012 combined Linked file.

### Confirmed Ontario Linked/Share availability

Based on ICES Dictionary:

**DEN_132 / DENA_132 / DENC_132 / DENE_132:**
- 2001: CCHS2001_ONT_LINK_11, CCHS2001_ONT_SHARE_11
- 2003: CCHS2003_ONT_LINK_21, CCHS2003_ONT_SHARE_21
- 2005: CCHS2005_ONT_LINK_31, CCHS2005_ONT_SHARE_31
- 2009-2010: CCHS200910_ONT_LINK, CCHS200910_ONT_SHARE
- 2011: CCHS2011_ONT_SHARE (annual only, NOT in combined Linked)
- 2013-2014: CCHS201314_ONT_LINK, CCHS201314_ONT_SHARE

**CHP_080 (2015-2016 replacement):**
- 2015-2016: CCHS201516_ONT_SHARE, CCHS2015_ONT_SHARE, CCHS2016_ONT_SHARE

**OHT_015, DEN_010, DEN_010A (2017-2018):**
- 2017-2018: CCHS201718_ONT_SHARE, CCHS2017_ONT_SHARE, CCHS2018_ONT_SHARE

### Key discrepancy with team notes

Rafidul's notes say 2017-2018 has DEN_132, but ICES Dictionary does NOT show DEN_132 in 2017-2018 Ontario files.

**Please verify:**
1. Is DEN_132 available in 2017-2018 PUMF/Master files but not Ontario Share?
2. Does OHT_015 measure the same concept as DEN_132?
3. What is the content status (Common vs Optional) for oral health modules in each cycle?

### Source variable naming evolution

Please verify the source variable names for DEN_132:

| Cycle | Expected source variable |
|-------|-------------------------|
| 2001 | DENA_132 |
| 2003 | DENC_132 |
| 2005 | DENE_132 |
| 2007-2008 | DEN_132 (if available) |
| 2009-2010 | DEN_132 |
| 2011-2012 | DEN_132 (if available) |
| 2013-2014 | DEN_132 |
| 2017-2018 | DEN_132 |

### Key questions

1. **2011-2012 gap**: Is it correct that NO oral health variables were available in 2011-2012 for Ontario?

2. **2015-2016 replacement**: CHP_080 asks about dental visits in the past 12 months. DEN_132 asks about "last time" visited (with categories: <1 year, 1-2 years, 2-3 years, etc.). These are NOT equivalent. Is CHP_080 the only dental visit variable available in 2015-2016?

3. **2017-2018 OH2_30 split**: We understand that OH2_30 (brushing frequency) was split into:
   - DEN_010: Continuous frequency (numerator)
   - DEN_010A: Categorical reporting period (denominator - by day, by week, etc.)

   Is this correct? What are the response categories for DEN_010A?

4. **PUMF vs Master/Linked**: Which of these variables are available in PUMF vs only in Master/Linked files?

### Ontario-specific questions

1. Were oral health modules **Common Content** or **Optional Content** in each cycle?
2. Did Ontario select the oral health module for all cycles where it was optional?
3. Are there any cycles where Ontario Master/Linked files have oral health data but PUMF does not (or vice versa)?

---

## Expected sources to upload to NotebookLM

1. CCHS User Guides for cycles 2001, 2003, 2005, 2007-2008, 2009-2010, 2011-2012, 2013-2014, 2015-2016, 2017-2018
2. CCHS PUMF Data Dictionaries (DDI or codebook files)
3. CCHS questionnaires showing actual oral health questions
4. Statistics Canada metadata on content modules

## Verification checklist

After running this prompt, confirm:

- [ ] DEN_132 availability by cycle matches our matrix
- [ ] OH1_20/OH1_22 availability by cycle verified
- [ ] OH2_20/OH2_21/OH2_30 availability by cycle verified
- [ ] 2011-2012 gap confirmed (or corrected)
- [ ] 2015-2016 CHP_080 as replacement confirmed
- [ ] 2017-2018 DEN_010/DEN_010A split explained
- [ ] Ontario Linked file availability confirmed
- [ ] Optional vs Common content status per cycle documented

---

## Background: Rafidul's notes

For reference, here are the team notes we are verifying:

> **Oral health variables availability:**
> - 2001: DEN_132 only
> - 2003: DEN_132, OH1_20, OH1_22, OH2_20, OH2_21, and OH2_30 (full coverage)
> - 2005: DEN_132, OH2_20, OH2_21, and OH2_30 (OH1 excluded)
> - 2007-2008: OH1_20 and OH1_22 only (DEN and OH2 excluded)
> - 2009-2010: DEN_132, OH2_20, OH2_21, and OH2_30 (OH1 excluded)
> - 2011-2012: None
> - 2013-2014: DEN_132, OH1_20, OH1_22, OH2_20, OH2_21, and OH2_30 (full coverage)
> - 2015-2016: None; CHP_080 could replace DEN_132 but it asks dental visit within only past year
> - 2017-2018: DEN_132, OH1_20, OH2_20, OH2_21, and OHT_015
>
> For OH2_30 (categorical brushing teeth frequency) in 2017-2018, the variable is split into DEN_010 (continuous frequency variable - aka OH2_30 numerator) and DEN_010A (categorical reporting period (e.g., by day, by week, etc.) - aka OH2_30 denominator).
