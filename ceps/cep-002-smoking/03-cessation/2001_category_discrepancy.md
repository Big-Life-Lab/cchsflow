# CCHS 2001 smoking cessation category discrepancy

## Summary

The CCHS 2001 (Cycle 1.1) variables SMKA_06A and SMKA_09A use **different category intervals** than 2003+ equivalents SMKC_06A/SMKC_09A. Our harmonization worksheets currently apply 2003+ midpoints to 2001 data, which may produce systematic bias for categories 3 and 4.

## Category comparison

**2001** (questionnaire p. 74; data dictionary pp. 238, 240):

| Code | Label | Interval |
|------|-------|----------|
| 1 | Less than one year ago | 0 to <1 year |
| 2 | 1 to 2 years ago | 1 to 2 years |
| 3 | **3 to 5 years ago** | **3 to 5 years** |
| 4 | **More than 5 years ago** | **>5 years** |

**2003+** (questionnaire SMK_Q206A; data dictionary pp. 361, 363):

| Code | Label | Interval |
|------|-------|----------|
| 1 | Less than one year ago | 0 to <1 year |
| 2 | 1 year to less than 2 years ago | 1 to <2 years |
| 3 | **2 years to less than 3 years ago** | **2 to <3 years** |
| 4 | **3 or more years ago** | **3+ years** |

Categories 1-2 are essentially identical. Categories 3-4 have **no overlap**: 2001 cat 3 (3-5 years) vs 2003+ cat 3 (2-3 years); 2001 cat 4 (>5 years) vs 2003+ cat 4 (3+ years).

## The 2-3 year gap

The 2001 questionnaire jumps from "1 to 2 years" to "3 to 5 years", leaving 2-3 years unassigned. The data dictionary note on SMKA_06A states: *"Responses between 2 and 3 years rounded up or down by interviewer."* Respondents who quit 2-3 years ago were discretionally assigned to category 2 or 3.

## Midpoint error

Current worksheets use 2003+ midpoints for 2001:

| Code | Current midpoint | Correct 2001 midpoint | Error |
|------|-----------------|----------------------|-------|
| 1 | 0.5 | 0.5 | None |
| 2 | 1.5 | 1.5 | None |
| 3 | 2.5 | 4.0 | **-1.5 years** |
| 4 | 4.0 | ~7-8 | **-3 to -4 years** |

Most respondents fall in category 4 (SMKA_06A: n=2,726; SMKA_09A: n=24,315), where the error is largest.

## Questions for review

1. **Are the 2001 categories genuinely different intervals?** The questionnaire explicitly reads "3 to 5 years ago" and "More than 5 years ago" to respondents. These appear to be genuinely different questions, not just different labels for the same intervals.

2. **How should the 2-3 year gap be handled?** The interviewer rounding contaminates categories 2 and 3, making pure midpoint imputation less accurate for both.

3. **What midpoint for 2001 category 4 (>5 years)?** Is there a principled approach, or should we use a conservative estimate (e.g. 7-8 years)?

4. **Should 2001 be excluded from continuous harmonization?** Given the incompatible categories, would it be more defensible to provide 2001 only as categorical rather than including it alongside cycles with different interval definitions?

## Sources

- 2001 Data Dictionary: `2001cchsdictionary.pdf` (pp. 238, 240)
- 2001 Questionnaire: `CCHS 1.1 Questionnaire.pdf` (pp. 73-75)
- 2003 Data Dictionary: `2003cchsdictionary.pdf` (pp. 361, 363)
