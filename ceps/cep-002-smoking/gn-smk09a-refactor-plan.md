# SMK_09A family refactor plan

**Date:** 2026-03-17 **Branch:** v3-smoking **Status:** Awaiting review

------------------------------------------------------------------------

## Goal

Three harmonized continuous output variables covering all cycles and both file types:

| Output variable | Concept | Coverage |
|---|---|---|
| `SMK_09A_cont` | Years since stopped smoking **daily** (former daily smokers) | PUMF 2001–2023 |
| `time_quit_smoking_daily` | Years since stopped smoking daily (former daily smokers) | PUMF+Master 2001–2023 |
| `time_quit_smoking_complete` | Years since stopped smoking **completely** (all former smokers) | PUMF+Master 2001–2023 |

`SMK_09A_cont` is the PUMF-only midpoint-imputed building block. `time_quit_smoking_daily`
and `time_quit_smoking_complete` are the fully harmonized outputs across both file types.

The categorical variables (`SMK_09A_2001`, `SMK_09A_2003plus`, `SMK_06A_2001`,
`SMK_06A_2003plus`) are inputs — not the end goal, but they must be correctly formed.

------------------------------------------------------------------------

## Variable architecture

### Categorical inputs

| Variable | Type | Coverage | Source variables |
|---|---|---|---|
| `SMK_09A_2001` | Cat | PUMF+Master 2001 | `SMKA_09A` — Cat 3: 3–5 yrs, Cat 4: >5 yrs |
| `SMK_09A_2003plus` | Cat | PUMF+Master 2003–2023 | `SMKC_09A`, `SMKE_09A`, `SMK_09A`, `SMK_080`, `SPU_25` — identical 1–4 scale |
| `SMK_06A_2001` | Cat | PUMF+Master 2001 | `SMKA_06A` — 2001-era occasional smoker quit time |
| `SMK_06A_2003plus` | Cat | PUMF+Master 2003–2023 | `SMKC_06A`, `SMKE_06A`, `SMK_06A`, `SMK_060`, `SPU_10` |

**2001 structural break:** Categories 3 and 4 differ between 2001 and 2003+. The 2001
variants must not be merged with the 2003+ block.

**Note on `SMK_06A` naming:** `variable_details_fixed.csv` already has `SMK_06A_2001` and
`SMK_06A_2003plus` (correctly named). Only `variables_fixed.csv` still uses the old names
`SMK_06A_cat4_2001` / `SMK_06A_cat4` and needs updating.

### Continuous outputs

**`SMK_09A_cont` — years since stopped smoking daily (PUMF building block)**

- Coverage: PUMF only, 2001–2023
- Midpoint-imputed from categorical inputs; used as input to `time_quit_smoking_daily`
- Logic:
  - 2001 (from `SMK_09A_2001`): codes 1–2 → 0.5, 1.5; code 3 ("3–5 yrs") → 4.0; code 4
    (">5 yrs") → empirical value (flagged in `reviewNotes` pending analysis)
  - 2003–2023 (from `SMK_09A_2003plus`): codes 1–3 → 0.5, 1.5, 2.5; code 4 ("3+ yrs") →
    use `SMK_09C`/`SMK_090` where available, fallback 5.0
  - 2022–2023 PUMF (SPU_25): handled via `calculate_SMK_09A_cont()` DerivedVar block
- Note: 2001 PUMF block already exists in worksheet (rows 2885–2891); the plan previously
  stated no `_cont` for 2001 — that was wrong.

**`time_quit_smoking_daily` — years since stopped smoking daily (PUMF+Master)**

- Coverage: PUMF+Master 2001–2023
- Precision varies by era (see notes)
- Logic by file type and era:

| Era | Master | PUMF | Precision |
|---|---|---|---|
| 2001 | `SMK_09C` (exact years) | `SMK_09A_cont` (midpoints) | Master: high; PUMF: midpoint |
| 2003–2021 | `SMK_09C` (2003–2014) / `SMK_090` (2015–2021) (exact years) | `SMK_09A_cont` (midpoints) | Master: high; PUMF: midpoint |
| 2022 | DerivedVar: `ADM_YOI - SPU_25B` (exact year; no categorical `SPU_25` in 2022 Master) | `SMK_09A_cont` (midpoints) | Master: high; PUMF: midpoint |
| 2023 | Midpoint-imputed from `SMK_09A_2003plus` (no exact-year follow-up in 2023) | `SMK_09A_cont` (midpoints) | Both: midpoint only |

**`time_quit_smoking_complete` — years since stopped smoking completely (PUMF+Master)**

- Coverage: PUMF+Master 2003–2023; PUMF 2001 only (2001 Master has no StatCan DV)
- Rename from current `time_quit_smoking`
- Precision varies by era (see notes)
- Logic by file type and era:

| Era | Master | PUMF | Precision |
|---|---|---|---|
| 2001 | DV needed: combine `SMK_09C` (former daily) + 2001 occasional smoker variable — requires routing logic work | `SMK_09A_cont` + `SMK_06A_cont` (midpoints) | Master: TBD; PUMF: midpoint |
| 2003–2022 | `SMKDSTP` (2003–2014) / `SMKDVSTP` (2015–2022) — exact StatCan DV, pass-through | `SMK_09A_cont` + `SMK_06A_cont` (midpoints) | Master: high; PUMF: midpoint |
| 2023 | Midpoint-imputed from `SMK_09A_2003plus` + `SMK_06A_2003plus` (`SMKDVSTP` discontinued) | `SMK_09A_cont` + `SMK_06A_cont` (midpoints) | Both: midpoint only |

------------------------------------------------------------------------

## Problems to fix

1. **`SMK_09A_cat4` misnaming:** 2003+ categories identical to source — `_catN` unwarranted.
   Rename to `SMK_09A_2003plus`.

2. **`SMK_09A_cat4` wrong `recEnd`:** Direct recode block has midpoints (0.5, 1.5, 2.5, 4.0)
   instead of integer pass-through (1, 2, 3, 4). Categorical variable — midpoints belong only
   in `SMK_09A_cont`.

3. **`SMK_09A_cat4` wrong `typeEnd`:** Set to `cont` during this review session in error. Revert
   to `cat`.

4. **`SMK_09A` bare — wrong and redundant:** `variables.csv` declares it Categorical but
   `variable_details.csv` has `typeEnd=cont` with midpoints. Delete all 21 rows; move SPU_25
   DerivedVar block to `SMK_09A_cont`.

5. **`SMK_09A_cont` missing 2022–2023 coverage:** SPU_25 DerivedVar block is under `SMK_09A`
   bare. Moving it closes the gap (`databaseStart` already lists `cchs2022_p`, `cchs2023_p`).

6. **`SMK_06A_cat4` / `SMK_06A_cat4_2001` in `variables_fixed.csv`:** `variable_details_fixed.csv`
   already correct (`SMK_06A_2001`, `SMK_06A_2003plus`). `variables_fixed.csv` must be updated
   to match.

7. **`time_quit_smoking` rename:** Rename to `time_quit_smoking_complete` throughout: `variables_fixed.csv`,
   R function name, R documentation, tests.

10. **`time_quit_smoking_daily` missing:** New variable needed. Add to `variables_fixed.csv`
    and implement `calculate_time_quit_smoking_daily()` combining `SMK_09A_cont` (PUMF),
    `SMK_09C`/`SMK_090` (Master 2001–2021), `ADM_YOI - SPU_25B` (Master 2022), and midpoint
    imputation from `SMK_09A_2003plus` (Master 2023 — reduced precision, flag in `reviewNotes`).

11. **`SMKG09C_cont` incorrectly mapped to `cchs2022_m`:** `SMKG09C` is a legacy PUMF
    variable name and does not exist on 2022 Master. Remove `cchs2022_m` from `SMKG09C`
    and `SMKG09C_cont` `databaseStart` in `variables_fixed.csv`.

12. **`time_quit_smoking_complete` 2023 Master precision:** `SMKDVSTP` discontinued after
    2022. For 2023 Master, impute from `SMK_09A_2003plus` + `SMK_06A_2003plus` midpoints —
    same precision as PUMF. Flag reduced precision in `reviewNotes`.

8. **`SMK_09A_cont` open-ended `recEnd` bias:** Code 4 ("3+ yrs", 2003–2023 block) uses
   `recEnd=4` (lower bound). Correct value requires empirical analysis using `SMK_09C`/`SMK_090`
   follow-up data from Master files. Flag in `reviewNotes` until analysis is done.

9. **2001 Master for `time_quit_smoking_complete` needs DV work:** No StatCan composite DV
   exists for 2001 Master. Possible to construct by combining `SMK_09C` (former daily) with
   the 2001 occasional smoker routing — requires investigation of 2001 Master questionnaire
   structure. Keep `cchs2001_m` in `databaseStart` but flag as needing implementation.

------------------------------------------------------------------------

## Changes by file

### 1. `/private/tmp/variable_details_fixed.csv`

**Delete — `SMK_09A` bare direct recode rows (2843–2856):**
- 2001 block (rows 2843–2849): duplicates `SMK_09A_cont` 2001 block
- 2003+ block (rows 2850–2856): duplicates `SMK_09A_cont` 2003+ block

**Move — `SMK_09A` bare DerivedVar block (rows 2857–2863) → `SMK_09A_cont`:**
- Change `variable` from `SMK_09A` → `SMK_09A_cont` on these 7 rows

**Rename + fix — `SMK_09A_cat4` (rows 2871–2884) → `SMK_09A_2003plus`:**

Direct recode block (rows 2871–2877):
- `variable`: `SMK_09A_cat4` → `SMK_09A_2003plus`
- `typeEnd`: `cont` → `cat`
- `recEnd`: (0.5, 1.5, 2.5, 4.0) → (1, 2, 3, 4); NA rows unchanged
- `dummyVariable`: restore from `N/A` → `SMK_09A_2003plus_1`, `_2`, `_3`, `_4`, `_NAa`, `_NAb`, `_NAb`
- `variableStart`: extend to include SPU_25 databases (merge DerivedVar block into this block)

DerivedVar block (rows 2878–2884):
- SPU_25 uses same 1–4 scale — no custom function needed
- Delete Func row (2878) and output rows (2879–2884)
- SPU_25 databases added to direct recode block `variableStart` above (one block for all 2003–2023)

**Flag — `SMK_09A_cont` code 4 `recEnd` bias (rows ~2853, 2895):**
- Add `reviewNotes` entry: "recEnd=4 for code 4 ('3+ yrs') is the category lower bound, not
  an empirical midpoint. Update after analysis using SMK_09C/SMK_090."

### 2. `/private/tmp/variables_fixed.csv`

- `SMK_09A_cat4` → `SMK_09A_2003plus`: `variable`, `label`, `labelLong`
- Delete `SMK_09A` bare row
- `SMK_06A_cat4_2001` → `SMK_06A_2001`: `variable`, `label`, `labelLong`
- `SMK_06A_cat4` → `SMK_06A_2003plus`: `variable`, `label`, `labelLong`
- `time_quit_smoking` → `time_quit_smoking_complete`: `variable`, `label`, `labelLong`
- `time_quit_smoking_complete` `databaseStart`: keep `cchs2001_m`; add `reviewNotes` flagging
  that 2001 Master implementation needs DV work (routing logic for former occasional smokers)

### 3. `R/smoking-cessation.R`

- `calculate_SMK_09A_cont()`: rename parameter `SMK_09A_cat4` → `SMK_09A_2003plus`, update
  all internal references and `@param` tag (~12 lines)
- `calculate_time_quit_smoking()` → `calculate_time_quit_smoking_complete()`: rename function,
  update all call sites in same file
- Add `calculate_time_quit_smoking_daily()`: new function combining `SMK_09A_cont` (PUMF),
  `SMK_09C`/`SMK_090` (Master 2001–2021), and `ADM_YOI - SPU_25B` derivation (Master 2022)

### 4. `R/smoke-stop.R`

- Update documentation stubs: `SMK_09A_cat4` → `SMK_09A_2003plus`
- Update `time_quit_smoking` stub → `time_quit_smoking_complete`
- Add stub for `time_quit_smoking_daily`

### 5. `tests/testthat/test-time_quit_smoking.R`

- `calculate_SMK_09A_cont(SMK_09A_cat4 = ...)` → `(SMK_09A_2003plus = ...)`
- `calculate_time_quit_smoking(...)` → `calculate_time_quit_smoking_complete(...)`
- Add tests for `calculate_time_quit_smoking_daily()`

### 6. CEP documents (non-blocking, update after validation)

- `cep-002-smoking.qmd`: update rename history and variable table
- `00-variable-summary.qmd`: update variable table
- `smoking-dv-refactoring-plan.md`: note superseded by this rename pass
- `gn-smk06a-09a-cat4-query.md`, `gn-smk09a-cont-review.md`: update variable names

### 7. Draw.io diagram (new)

Create a diagram illustrating the variable architecture:
- Smoker type routing (former daily vs former occasional)
- Source variable → categorical input → continuous output flow by era
- PUMF vs Master paths
- `time_quit_smoking_complete` as the final harmonized output

------------------------------------------------------------------------

## Execution order

1. Worksheet fixes (`variable_details_fixed.csv`, `variables_fixed.csv`)
2. R code renames (`smoking-cessation.R`, `smoke-stop.R`)
3. Test updates (`test-time_quit_smoking.R`)
4. Run `Rscript exec/check-worksheets.R` and `devtools::test()` to validate
5. CEP document updates and draw.io diagram — after validation passes
