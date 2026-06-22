# NotebookLM prompt — SMK_09A family investigation

**Date:** 2026-03-17
**Context:** v3-smoking branch; investigating identity and overlap across the SMK_09A variable family

---

I'm reviewing the `SMK_09A` family of harmonized variables in a CCHS data harmonization package
(cchsflow). I need to understand the intended structure of these variables. Please look for any
documentation, worksheets, or notes that address the following:

1. **What is `SMK_09A`?** Is it a continuous (midpoint-imputed) variable or a categorical
   variable? What CCHS cycles does it cover, and does it cover both PUMF and Master files?

2. **What is `SMK_09A_cont`?** How does it differ from `SMK_09A`? Is it PUMF-only? What cycles?

3. **What is `SMK_09A_cat4`?** Is it intended to be categorical or continuous? The worksheet
   currently has two recode blocks for it:
   - A **direct recode block** with midpoint `recEnd` values (0.5, 1.5, 2.5, 4.0) for PUMF and
     Master databases 2003–2021
   - A **DerivedVar block** using `Func::calculate_SMK_09A_cont` for cycles where the source
     variable is `SPU_25` (2022–2023)
   The direct recode block produces continuous output despite the `_cat4` name — is this
   intentional or an error?

4. **Is there overlap or duplication** between `SMK_09A`, `SMK_09A_cont`, and `SMK_09A_cat4`?
   Specifically, do the PUMF and Master 2003–2021 direct recode rows in `SMK_09A_cat4` duplicate
   rows already present in `SMK_09A` or `SMK_09A_cont`?

5. **Is `SMK_09A_cat4` supposed to be a purely categorical variable** (i.e., passing through
   integer category codes 1–4 without midpoint imputation), with the DerivedVar block providing a
   categorical recoding of the SPU_25 source for 2022–2023?

6. **What is the role of `SMK_09A` (bare name)?** The `variables.csv` worksheet declares
   `variableType=Categorical` and `databaseStart` covering Master files only (2001–2023), but the
   `variable_details.csv` worksheet has `typeEnd=cont` with midpoint `recEnd` values and
   `variableStart` entries spanning both PUMF and Master. Is `SMK_09A` intended to be categorical
   (pass-through of StatCan codes) or continuous (midpoint-imputed)?

Please summarize what you find and flag any inconsistencies between the declared intent and the
worksheet implementation.
