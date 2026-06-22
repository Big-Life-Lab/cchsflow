# Hearing variables — Gem verification prompt (v2)

## Context

We are reviewing 5 hearing-related HUI variables in cchsflow. After cross-checking with the cchs-metadata database (DuckDB built from DDI XML and PUMF RData), we have specific findings that need verification against the StatCan source PDFs in this notebook.

The attached CSVs (`gn-hearing-variables.csv` and `gn-hearing-variable_details.csv`) show the **current** worksheet state.

## Specific verifications needed

### V1: HUI raw hearing questions in cchs2005 and cchs2007-2008 Master

The metadata DB confirms that `HUIE_06` through `HUIE_09` exist in the 2005 Master file, and `HUI_06` through `HUI_09` exist in the 2007-2008 Master file. The current worksheet is missing these cycles for HUI06, HUI07, HUI08, and HUI09.

**Please verify**: Do the 2005 and 2007-2008 Master data dictionaries contain HUIE_06/HUIE_07/HUIE_07A/HUIE_08/HUIE_09 and HUI_06/HUI_07/HUI_07A/HUI_08/HUI_09 respectively? Are they binary (1=Yes, 2=No) with 6=N/A, 7=DK, 8=Refusal, 9=Not stated?

### V2: HUI_09 in cchs2011-2012 Master

The current worksheet includes `cchs2011_2012_m` for HUI09, but the metadata DB shows **no HUI variables at all** in the 2011-2012 Master dataset.

**Please verify**: Does the 2011-2012 Master data dictionary contain HUI_09? Or was HUI optional content that was not collected in 2011-2012?

### V3: WDM_101 vs WDM_010

The current worksheet references `WDM_101` as the Washington Group hearing variable in cchs2017_2018_m. However, the metadata DB shows:
- `WDM_010` = "Difficulty - hearing" in cchs2017_2018_m (4-point scale: 1=No difficulty, 2=Some, 3=A lot, 4=Cannot do)
- `WDM_101` does **not exist** anywhere in the metadata DB

**Please verify**: In the 2017-2018 Master data dictionary, is the Washington Group hearing question `WDM_010` or `WDM_101`? What are its response categories?

### V4: 2015-2016 HUI question renumbering

The metadata DB shows that in 2015-2016 and 2019-2020, the HUI hearing questions were renumbered:
- HUI_030 = "Hearing - hear in a group without hearing aid" (formerly HUI_06)
- HUI_035 = "Hearing - hear in a group with hearing aid" (formerly HUI_07)
- HUI_040 = "Hearing - able to hear" (formerly HUI_07A)
- HUI_045 = "Hearing - hear in a quiet room without hearing aid" (formerly HUI_08)
- HUI_050 = "Hearing - hear in a quiet room with hearing aid" (formerly HUI_09)

**Please verify**: Do the 2015-2016 and/or 2019-2020 Master data dictionaries confirm these variable names and labels? Are they binary (1=Yes, 2=No) with the same response categories as the pre-2015 versions?

### V5: WDM in 2022 Master

The metadata DB shows `WDM_10` = "Difficulty - hearing" in cchs2022_m (note: renamed from `WDM_010` to `WDM_10`).

**Please verify**: Does the 2022 Master data dictionary contain `WDM_10` with the same 4-point Washington Group scale?

### V6: 2023 HUIHEAR variables

The metadata DB shows two new variables in cchs2023_m:
- `HUIHEAR1` = "Hearing - hear in a quiet room without hearing aid"
- `HUIHEAR2` = "Hearing - hear in a quiet room with hearing aid"

**Please verify**: Do these exist in the 2023 Master data dictionary? Are they binary (1=Yes, 2=No)? Are they equivalent to the old HUI_08/HUI_09 (or HUI_045/HUI_050)?

### V7: Response code 8 (Refusal)

The metadata DB shows that HUI hearing questions have codes 7=Don't know, 8=Refusal, and 9=Not stated. The current worksheet maps `[7,9]` to NA::b but does not explicitly include code 8.

**Please verify**: Do the data dictionaries consistently show code 8=Refusal for the HUI hearing questions? Should the recode pattern be `[7,8,9]` instead of `[7,9]`?

### V8: HUI_07A question wording

The label for HUI_07A / HUI_040 is "Hearing - able to hear". The Gem previously suggested that WDM respondents with "some difficulty" or "a lot of difficulty" are still "able to hear" and should map to 1 (Yes), with only "cannot do at all" mapping to 2 (No).

**Please verify**: What is the exact question wording for HUI_07A in any available data dictionary? Is it "Are you able to hear at all?" or something different?

## Output format

For each verification (V1-V8), report:
- **Confirmed** / **Contradicted** / **Cannot verify** (with reason)
- Source document reference (cycle, file type, page if available)
- Exact wording from the data dictionary if relevant

Summary table:

| # | Finding | Status | Source |
|---|---------|--------|--------|
| V1 | 2005/2007-2008 coverage | ... | ... |
| V2 | HUI_09 in 2011-2012 | ... | ... |
| V3 | WDM_101 vs WDM_010 | ... | ... |
| V4 | 2015+ renumbering | ... | ... |
| V5 | WDM in 2022 | ... | ... |
| V6 | 2023 HUIHEAR | ... | ... |
| V7 | Code 8 (Refusal) | ... | ... |
| V8 | HUI_07A wording | ... | ... |
