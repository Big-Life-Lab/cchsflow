# NotebookLM coverage summary

**Notebook:** CCHS cchsflow review notebook
**Manifest:** `notebook-manifest.csv` (in this directory)
**Last updated:** 2026-03-28

## Coverage

| Collection | Files | Cycles |
|---|---|---|
| Master | 113 | 2001-2023 (complete) |
| PUMF | 126 | 2001-2022 (2023 missing) |

### Master files (all cycles 2001-2023)

Complete coverage: data dictionaries, derived variable specs, questionnaires, and user guides for every cycle.

### PUMF files by cycle

| Cycle | DD | DV | QU | UG | Files |
|---|---|---|---|---|---|
| 2001 | yes | - | - | - | 8 |
| 2003 | yes | yes | yes | - | 8 |
| 2005 | yes | yes | yes | yes | 10 |
| 2007-2008 | - | yes | yes | yes | 8 |
| 2009-2010 | yes | yes | - | yes | 9 |
| 2010 | yes | yes | yes | yes | 9 |
| 2011-2012 | yes | yes | yes | yes | 11 |
| 2012 | yes | yes | yes | yes | 11 |
| 2013-2014 | yes | yes | yes | yes | 11 |
| 2014 | yes | yes | yes | yes | 11 |
| 2015-2016 | yes | yes | yes | yes | 10 |
| 2017-2018 | yes | yes | yes | yes | 8 |
| 2019-2020 | yes | yes | yes | yes | 6 |
| 2022 | yes | yes | - | yes | 6 |
| 2023 | - | - | - | - | 0 |

**DD** = data dictionary, **DV** = derived/grouped variable specs, **QU** = questionnaire, **UG** = user guide

### Known gaps

- **2001 PUMF**: Only data dictionary; no derived variables, questionnaire, or user guide
- **2003 PUMF**: No user guide
- **2007-2008 PUMF**: No data dictionary
- **2009-2010 PUMF**: No questionnaire
- **2022 PUMF**: No questionnaire
- **2023 PUMF**: Not in notebook (may not be released yet)

### Impact on Gem reviews

- **PUMF response category verification** is strong for 2005-2020 (data dictionaries present)
- **PUMF grouped/derived variable verification** is strong for 2003-2022
- **Master verification** is comprehensive across all cycles
- **2023 PUMF** cannot be verified at all — flag as "cannot verify" in reviews

## Adding documents

1. Obtain PDFs from the Statistics Canada CCHS documentation releases
2. Upload to the NotebookLM notebook
3. Update `manifest.csv` with filename, collection, cycle, file_size_bytes, and sha256
4. Re-run coverage analysis to confirm gap is filled
