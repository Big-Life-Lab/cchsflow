# NotebookLM prompt — ethnicity, language, and migration variable verification

**Date:** 2026-03-30
**Context:** PR #171 (`ethnicity` branch); verifying worksheet mappings for 13 sociodemographic variables spanning ethnicity (4), language (4), and migration (5) domains.

---

## Background

The cchsflow R package harmonises CCHS variables across survey cycles (2001-2023). This review covers ethnicity, language, and migration variables added in PR #171. These variables have complex era-specific naming:

- **2001**: `SDCAxxx` prefix (e.g., SDCAGRAC, SDCAGLNG, SDCAFIMM)
- **2003**: `SDCCxxx` prefix (e.g., SDCCGRAC, SDCCGLNG, SDCCFIMM)
- **2005**: `SDCExxx` prefix (e.g., SDCEGCGT, SDCEGLNG, SDCEFIMM)
- **2007-2014**: `SDCDxxx` or `SDCGxxx` prefix (e.g., SDCDCGT, SDCGLNG, SDCFIMM)
- **2015+**: `SDCDVxxx` or `SDCDGxxx` prefix (e.g., SDCDVIMM, SDCDGCGT, SDCDVCGT)
- **2022-2023**: Some variables renamed to `LAN_01` (language module restructured)

The two attached CSVs (`gn-ethnicity-variables.csv` and `gn-ethnicity-variable_details.csv`) contain the worksheet rows for all 13 in-scope variables.

---

## Variable inventory

| # | Variable | Domain | Type | Databases | Key question |
|---|----------|--------|------|-----------|--------------|
| 1 | SDCGCGT | Ethnicity | PUMF categorical | 2001p-2018p | Cultural/racial origin (grouped) |
| 2 | SDCDCGT_cat13 | Ethnicity | Master categorical | 2003m-2018m | Cultural origin (13-category detailed) |
| 3 | SDCDCGT_cat7 | Ethnicity | Master categorical | 2003m-2018m | Cultural origin (7-category collapsed) |
| 4 | SDCDVABT | Ethnicity | Master+PUMF | 2005m-2023m, 2015p-2022p | Aboriginal/Indigenous identity |
| 5 | SDCGLNG | Language | PUMF+Master | 2001p-2010p, 2001m-2023m | Languages can converse in |
| 6 | SDC_5A_1 | Language | PUMF+Master | 2011p-2018p, 2011m-2023m | Knowledge of official languages |
| 7 | SDCGLHM | Language | PUMF | 2007p-2018p | Language(s) spoken at home |
| 8 | SDCGLHM_A | Language | Master only | 2009m, 2010m, 2012m | Language(s) spoken at home (Master variant) |
| 9 | SDCDFOLS | Language | PUMF+Master | 2011p-2018p, 2012m | First official language spoken |
| 10 | SDCFIMM | Migration | PUMF+Master | 2001p-2022p, 2001m-2023m | Immigrant status |
| 11 | SDCGCBG | Migration | PUMF+Master | 2001p-2018p, 2009m-2012m | Country of birth (grouped) |
| 12 | SDCGCBG_A | Migration | Master only | 2009m, 2010m, 2012m | Country of birth (Master variant) |
| 13 | SDCGRES | Migration | PUMF+Master | 2001p-2018p, 2009m-2012m | Time in Canada since immigration |

---

## Tier 1: Database coverage verification

For each variable, verify that the databases listed in `databaseStart` match the cycles where the source variable actually exists in StatCan documentation.

**Questions:**

1. **SDCGCGT (PUMF ethnicity):** The worksheet maps 2001p through 2018p with era-specific source names: `SDCAGRAC` (2001), `SDCCGRAC` (2003), `SDCEGCGT` (2005), `SDCDGCGT` (2015-2018), and `[SDCGCGT]` as the default (2007-2014). Does each source variable exist in its respective PUMF data dictionary? Is SDCGCGT available on any PUMF after 2018 (e.g., 2019, 2022)?

2. **SDCDCGT_cat13 and SDCDCGT_cat7 (Master ethnicity):** These cover 2003m through 2018m with `SDCCDRAC` (2003), `SDCEDCGT` (2005), `SDCDVCGT` (2015-2018), and `[SDCDCGT]` default (2007-2014). Does `SDCDCGT` exist on 2007-2014 Master files? Does `SDCDVCGT` exist on 2015-2018 Master files? Is there a Master ethnicity variable on 2019+ (e.g., SDCDVFLA or SDCDVVM)?

3. **SDCDVABT (Aboriginal/Indigenous identity):** Covers 2005m-2023m and 2015p-2022p. Source names include `SDCEFABT` (2005m), `SDCDABT` (2007-2014m), `SDC_015` (2015+ PUMF). Does SDC_015 exist on 2019-2020, 2022, and 2023 PUMF? What is the Master source variable for 2019+ — is it still SDCDVABT or was it renamed (e.g., to SDCDVABT with SDC_015 as source)?

4. **SDCFIMM (immigrant status):** Covers 2001p-2022p and 2001m-2023m. The 2022 PUMF maps to `SDCDGIMM`. Does SDCDGIMM exist on the 2022 PUMF? Is SDCFIMM available on 2023 PUMF? For Master 2019+, the worksheet uses `SDCDVIMM` — does this exist on 2019-2020, 2021, 2022, and 2023 Master files?

5. **SDCGLNG and SDC_5A_1 (language knowledge):** SDCGLNG covers 2001-2010 PUMF and spans to 2023 on Master. SDC_5A_1 covers 2011-2018 PUMF and 2011-2023 Master. The 2022-2023 Master maps to `LAN_01`. Does LAN_01 exist on 2022 and 2023 Master? Is there a PUMF language variable on 2019+ (e.g., SDC_025, LAN_01)?

6. **SDCGCBG and SDCGRES (country of birth, time in Canada):** Both cover 2001p-2018p PUMF and 2009m-2012m Master. Source names include era-specific variants (SDCAGCBG/SDCAGRES for 2001, SDCCGCBG/SDCCGRES for 2003, etc.) and `SDCDGCB`/`SDCDGRES` for 2015+. Are these available on any PUMF or Master after 2018?

---

## Tier 2: Source variable name accuracy

For each era-specific `db::VAR` mapping in `variableStart`, verify the source variable name matches the StatCan data dictionary for that cycle.

**Questions:**

7. **2001 PUMF names:** Do these exist — `SDCAGRAC` (ethnicity), `SDCAGLNG` (language), `SDCAFIMM` (immigration), `SDCAGCBG` (country of birth), `SDCAGRES` (time in Canada)?

8. **2003 names:** Do these exist — `SDCCGRAC` (ethnicity PUMF), `SDCCGLNG` (language PUMF), `SDCCFIMM` (immigration), `SDCCGCBG` (country of birth), `SDCCGRES` (time in Canada)? And Master: `SDCCDRAC` (ethnicity), `SDCCFIMM`, `SDCCDLNG` (language)?

9. **2005 names:** `SDCEGCGT`, `SDCEGLNG`, `SDCEFIMM`, `SDCEGCBG`, `SDCEGRES`, `SDCEFABT`, `SDCEDCGT` — do all exist?

10. **2007-2014 default names:** `[SDCGCGT]`, `[SDCDCGT]`, `[SDCGLNG]`, `[SDCFIMM]`, `[SDCGCBG]`, `[SDCGRES]`, `[SDCGLHM]`, `[SDCDFOLS]`, `[SDCGCB]` — do the bracketed defaults match the actual variable names on 2007-2014 files?

11. **2015+ renames:** `SDCDGCGT` (PUMF ethnicity), `SDCDVCGT` (Master ethnicity), `SDCDVIMM` (immigration), `SDC_025` (language), `SDCDVFLS` (first official language), `SDCDGCB` (country of birth), `SDCDGRES` (time in Canada), `SDC_015` (Aboriginal/Indigenous) — do all exist on the expected 2015+ files?

12. **2022-2023 renames:** `SDCDGIMM` (immigration 2022 PUMF), `LAN_01` (language 2022-2023 Master) — confirmed in data dictionaries?

---

## Tier 3: Response category verification

For key variables, check that `recStart` values in variable_details.csv match the StatCan response categories.

**Questions:**

13. **SDCGCGT (PUMF ethnicity):** The worksheet maps categories 1 (White), 2 (Black), 3-8 (other groups), 9 (multiple origins). Does this match the data dictionary for 2007-2014 PUMF? Did categories change at the 2015 rename to SDCDGCGT?

14. **SDCDCGT_cat13 (Master ethnicity):** 13 categories covering specific ethnic/cultural groups. Do these match the Master data dictionary for SDCDCGT (2007-2014) and SDCDVCGT (2015+)?

15. **SDCFIMM (immigrant status):** Simple binary (1=immigrant, 2=Canadian-born) plus NA codes. Consistent across all cycles?

16. **SDCGLNG (language knowledge):** Categories 1 (English only), 2 (French only), 3 (both), 4 (neither). Did the meaning or coding change between the multi-language checklist era (2001-2010: SDCGLNG) and the direct question era (2011+: SDC_5A_1/SDC_025)?

17. **SDCGCBG (country of birth):** Categories include 1 (Canada), 2-12 (specific regions/countries). Did the groupings change across eras? The worksheet has era-specific variants: `SDCGCB12` (2011-2012), `SDCGCB13` (2013-2014). Do these use the same category scheme?

---

## Tier 4: Cross-variable consistency

18. **SDCGLNG vs SDC_5A_1 overlap:** These two variables are meant to cover different eras of the same construct (language knowledge). The worksheet notes describe construct equivalence despite label differences. Do their databaseStart entries overlap or is there a clean handoff? Both should not cover the same cycles.

19. **SDCGCBG vs SDCGCBG_A:** The `_A` variant covers only 3 Master databases (2009m, 2010m, 2012m). What distinguishes SDCGCBG_A from SDCGCBG on those same databases? Do both exist simultaneously or is `_A` a different variable?

20. **SDCDFOLS coverage gap:** This variable covers 2011-2018 PUMF and only cchs2012_m for Master. SDCDFOLS should logically exist on 2011-2012 Master and 2013-2014 Master. Is the Master coverage incomplete, or was SDCDFOLS truly only available as a single-year Master variable?

---

## Output format

For each question, report:
- **Confirmed**: finding matches worksheet
- **Issue**: specific discrepancy with evidence from the data dictionary
- **Unable to verify**: document not available in notebook

Please provide a summary table at the top:

| # | Variable | Tier 1 | Tier 2 | Tier 3 | Tier 4 | Notes |
|---|----------|--------|--------|--------|--------|-------|
| 1 | SDCGCGT  | ?      | ?      | ?      | —      |       |
| ... | ...   | ...    | ...    | ...    | ...    |       |

Cite specific StatCan documents (e.g., "Per the 2015-2016 PUMF data dictionary...") for each finding.
