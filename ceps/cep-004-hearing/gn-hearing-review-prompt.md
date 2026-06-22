# Hearing variables — Gem review prompt

## Context

Review the hearing-related variables in the attached `gn-hearing-variables.csv` and `gn-hearing-variable_details.csv` files. These map Health Utility Index (HUI) hearing questions and the Washington Group Disability Statistics hearing question to harmonised binary variables in cchsflow.

## Variable inventory

| Variable | Description | Source questions | Databases |
|----------|-------------|-----------------|-----------|
| HUI06 | Hearing ability | HUIA_06, HUIC_06, HUI_06 | 2001m, 2003m, 2009-2010m, 2013-2014m |
| HUI07 | Hearing with aid | HUIA_07, HUIC_07, HUI_07 | 2001m, 2003m, 2009-2010m, 2013-2014m |
| HUI07A | Hearing ability (extended) | HUIA_07A, HUIC_07A, HUIE_07A, HUI_07A, WDM_101 | 2001m-2017/2018m |
| HUI08 | Hearing in group conversation | HUIA_08, HUIC_08, HUI_08 | 2001m, 2003m, 2009-2010m, 2013-2014m |
| HUI09 | Hearing on the telephone | HUIA_09, HUIC_09, HUI_09 | 2001m-2013/2014m |

All variables are **Master-only** (no PUMF coverage).

## Review tiers

### Tier 1: Database coverage

For each variable, verify that the databases listed in `databaseStart` match the cycles where the source question actually appeared in the Master file.

Specific questions:
1. Does **HUI_06** (or its era equivalents HUIA_06, HUIC_06) appear in cchs2007_2008_m, cchs2011_2012_m, cchs2015_2016_m, cchs2017_2018_m? If so, these databases are missing from HUI06.
2. Same question for **HUI_07**, **HUI_08** — do they exist in cycles beyond what's listed?
3. Does **HUI_07A** appear in cchs2011_2012_m and cchs2015_2016_m? The current worksheet has a gap (jumps from 2009-2010 to 2013-2014 to 2017-2018).
4. Does **HUI_09** exist in cchs2015_2016_m and cchs2017_2018_m? Currently stops at 2013-2014.
5. Does **WDM_101** (Washington Group hearing) exist in any Master files beyond cchs2017_2018_m (e.g., 2019, 2020, 2021, 2022, 2023)?

### Tier 2: Source variable name accuracy

For each `cchs{year}_m::{VAR}` mapping in variableStart, verify:
1. **Pre-2007 era names**: Do the 2001 variables use the `HUIA_` prefix? Do the 2003 variables use `HUIC_`? Do the 2005 variables use `HUIE_`?
2. **2007+ era names**: Is `HUI_06`, `HUI_07`, `HUI_07A`, `HUI_08`, `HUI_09` the correct variable name in the 2007+ Master files?
3. **WDM_101**: Is this the correct variable name for the Washington Group hearing question in the 2017-2018 Master file?

### Tier 3: Response category completeness

For each variable, verify that recStart values cover all response categories from the data dictionary:

1. **HUI06-HUI09** (HUI questions): What are the valid response categories? The worksheet maps 1→1, 2→2, 6→NA::a, [7,9]→NA::b, else→NA::b. Does "6" correctly represent "not applicable"? Are there categories 3, 4, 5 that are being lost?
2. **HUI07A with WDM_101**: WDM_101 uses the Washington Group 4-point scale (1=no difficulty, 2=some difficulty, 3=a lot of difficulty, 4=cannot do at all). The worksheet now maps 1→1, 2→2, 3→2, 4→2, 6→NA::a, [7,9]→NA::b, else→NA::b. Is this correct? Does WDM_101 actually use categories 1-4?
3. For the HUI questions specifically: Are there categories beyond 1 and 2 that represent different levels of hearing ability (e.g., a scale from 1 to 5 or 1 to 6)?

### Tier 4: Semantic crosswalk accuracy

1. **HUI to binary mapping**: The HUI hearing questions may have more than 2 substantive categories. If HUI_06 has categories 1-5 (e.g., "able to hear" to "unable to hear"), what is the appropriate binary cut point?
2. **WDM_101 to HUI07A**: The 4→2 mapping (cannot do at all → yes hearing difficulty) seems semantically correct. Confirm that the WDM scale direction matches the HUI direction (i.e., higher values = more difficulty in both scales).

## Output format

For each variable, report:
- **OK** if verified with a one-line summary
- **Issue** with specific row reference, what is wrong, and what the source document says

Provide a summary table at the top:

| Variable | Tier 1 | Tier 2 | Tier 3 | Tier 4 | Status |
|----------|--------|--------|--------|--------|--------|
| HUI06    | ...    | ...    | ...    | ...    | OK / N issues |

If you do not have documentation for a specific cycle, say so explicitly.
