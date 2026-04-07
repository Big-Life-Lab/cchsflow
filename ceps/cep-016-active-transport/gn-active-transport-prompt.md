# Active transportation variable cross-check

## Context

I'm reviewing cchsflow PR #176 which adds active transportation variables to the harmonization worksheets. cchsflow harmonizes CCHS variables across cycles using `variable_details.csv` (recoding rules) and `variables.csv` (variable registry).

Two CSV files are attached:
- `gn-active-transport-variables.csv` — 15 variables from variables.csv
- `gn-active-transport-variable_details.csv` — 97 rows from variable_details.csv

## Variable inventory

The variables fall into 3 eras:

**Era 1 (2001-2005): Hours per week**
- PAC_4A / PAC_4A_cont — Hours walking to work/school (6-category → midpoint)
- PAC_4B / PAC_4B_cont — Hours biking to work/school (6-category → midpoint)
- Source names: PACA_4A (2001), PACC_4A (2003), PACE_4A (2005) — same for 4B

**Era 2 (2007-2014): Gate + frequency + duration**
- PAC_7 — Walked to work/school? (Yes/No/No work)
- PAC_7A — Number of times walked (continuous, [1,270])
- PAC_7B / PAC_7B_cont — Time per walk (4-category → midpoint in minutes)
- PAC_8 — Biked to work/school? (Yes/No/No work)
- PAC_8A — Number of times biked (continuous, [1,270])
- PAC_8B / PAC_8B_cont — Time per bike (4-category → midpoint in minutes)

**Era 3 (2015-2020): StatCan derived minutes per week**
- PAYDVTTR — Total minutes active transportation, 7 days, youth 12-17 (continuous)
- PAADVTRV — Active transportation minutes per week, adult 18+ (continuous)

**Derived variable:**
- active_transport — Daily active transportation (minutes), computed by 3 era-specific functions

## Review scope — please verify

### Tier 1: Database coverage
For each variable, check whether the CCHS cycles listed in `databaseStart` are correct:

1. **PAC_4A/4B**: Listed for cchs2001_p/m, cchs2003_p/m, cchs2005_p/m only. Confirm these variables (PACA_4A, PACC_4A, PACE_4A and equivalents for 4B) exist in those cycles and do NOT exist in 2007+.

2. **PAC_7/7A/7B and PAC_8/8A/8B**: Listed for cchs2007_2008 through cchs2013_2014 (both _p and _m), plus single-year databases cchs2010_p, cchs2012_p, cchs2014_p, cchs2009_m, cchs2010_m, cchs2012_m. Confirm:
   - PAC_7/8 exist in 2007-2014 cycles
   - PAC_7/8 do NOT exist in 2015+ (replaced by PAYDVTTR/PAADVTRV)
   - The single-year databases (cchs2009_m, cchs2010_m, cchs2012_m, cchs2010_p, cchs2012_p, cchs2014_p) are valid

3. **PAYDVTTR/PAADVTRV**: Listed for cchs2015_2016 through cchs2019_2020 (both _p and _m). Confirm:
   - These derived variables exist in 2015-2020 cycles
   - Whether they also exist in 2021+ cycles (expansion opportunity)
   - Whether 2019-2020 PUMF has these variables

4. **active_transport**: DerivedVar spanning all 3 eras plus Master databases. The PR newly adds: cchs2001_m through cchs2019_2020_m and cchs2019_2020_p. Confirm feeder variables exist in those databases.

### Tier 2: Source variable name mappings
1. Pre-2007 cycle letters: Is PACA_4A correct for 2001? PACC_4A for 2003? PACE_4A for 2005? Same pattern for 4B?
2. For 2007-2014: Is the source variable name simply PAC_7, PAC_7A, PAC_7B, PAC_8, PAC_8A, PAC_8B (no rename)?
3. For 2015+: Were PAYDVTTR and PAADVTRV introduced in 2015? Any renames in later cycles?

### Tier 3: Response categories
1. **PACA_4A/4B** (2001-2005): Confirm 6 categories: 1=None, 2=<1hr, 3=1-5hrs, 4=6-10hrs, 5=11-20hrs, 6=>20hrs. Missing: 96=NA, 97-99=DK/Ref/NS.
2. **PAC_7/8** (2007-2014): Confirm 3 categories: 1=Yes, 2=No, 3=No work/school. Missing: 6=NA, 7-9=DK/Ref/NS.
3. **PAC_7B/8B** (2007-2014): Confirm 4 categories: 1=1-15min, 2=16-30min, 3=31-60min, 4=>1hr. Missing: 6=NA, 7-9=DK/Ref/NS.
4. **PAC_7A/8A** (2007-2014): Continuous [1,270]. Missing: 996=NA, 997-999=DK/Ref/NS.
5. **PAYDVTTR**: Continuous. What is the valid range? Missing codes?
6. **PAADVTRV**: Continuous. What is the valid range? Missing codes?

### Tier 4: Derived variable logic
1. The `active_transport` variable uses three era-specific functions. For Era 3 (2015-2020), the function adds PAYDVTTR and PAADVTRV then divides by 7. Given that PAYDVTTR is youth-only and PAADVTRV is adult-only (age-routed), is it correct that a respondent would have only one non-missing value?

## Output format

For each tier, please report:
- **Confirmed**: Items verified against your sources
- **Issues**: Discrepancies found (with source citation)
- **Unable to verify**: Items not in your source documents
