# cchsflow 1.8.2
2021-06-23

## Bug Fixes
- Fixes to `age_cat_fun()` to support decimal values from `DHHGAGE_cont`.
- Documentation corrections to `pack_years_fun()` and `time_quit_smoking_fun()`.

## New variable
- **SMKDSTY_cat3**: 3 category smoking status derived variable.

# cchsflow 1.8.1
2021-01-25

## Bug Fixes
- Correct breaking specification errors for [diet_score](https://big-life-lab.github.io/cchsflow/reference/diet_score_fun.html).

# cchsflow 1.8.0
2021-01-22

## Features
- New function `merge_rec_data()` that merges and labels transformed CCHS
datasets.

## Minor improvements
- Improved support for `tagged_na` in derived variables. Additional
documentation added for creating derived variables in the [tagged_na](https://big-life-lab.github.io/cchsflow/articles/tagged_na_usage.html)
article.
- Updates to labels and variable classification on `variables.csv` and
`variable_details.csv`.

## New variables
- [**COPD_Emph_der:**](https://big-life-lab.github.io/cchsflow/reference/COPD_Emph_der_fun1.html)
derived variable that determines if a respondent has either COPD or Emphysema.
- [**ADL_score_5:**](https://big-life-lab.github.io/cchsflow/reference/adl_score_5_fun.html)
derived variable that scores the number of daily tasks that a respondent needs
help with.
- [**diet_score:**](https://big-life-lab.github.io/cchsflow/reference/diet_score_fun.html)
derived variable that scores respondents based on daily consumption of fruit,
vegetables, and fruit juice.
- [**smoke_simple:**](https://big-life-lab.github.io/cchsflow/reference/smoke_simple_fun.html)
derived variable that identifies respondents smoking status.
- [**time_quit_smoking:**](https://big-life-lab.github.io/cchsflow/reference/time_quit_smoking_fun.html)
derived variable that estimates approximate time since respondent has quit
smoking if they are a former smoker.
- [**LBFA_31A/LBFA_31A_a/LBFA_31A_b:**](https://big-life-lab.github.io/cchsflow/reference/LBFA_31A.html)
Occupation group. 3 different variables with differing categories were created
to harmonize between CCHS cycles.
- **CCC_041:** Fibromyalgia.
- **CCC_061:** Back problems.
- **CCC_081:** Migraine headaches.
- **CCC_290:** Anxiety disorder.

# cchsflow 1.7.1
2020-09-16

## Minor improvements
- Corrections to typos in [tagged_na](https://big-life-lab.github.io/cchsflow/articles/tagged_na_usage.html)
article.
- Refactor `recTo` values for GEOGPRV to values specified in CCHS.

## New variables
- GEODPMF: Health region. Specified for the 2013-14 & 2014 CCHS cycles.
- id_year: ID variable that appends data_name to ADM_RNO, creating a unique
identifier for all respondents.

# cchsflow 1.7.0 
2020-09-11

## Breaking changes
- Notation in **recFrom** column of `variable_details.csv` is now changed to
interval notation (e.g. 7:9 is now denoted as [7,9]).

## Features
- New `description` column added to `variables.csv` to allow a description of
variables.
- New `role` column added to `variables.csv` to allow users to set roles for
variable manipulation.

## New variables
Age:
1. DHHGAGE_5: age variable categorized into 5 20-year age groups. Used in all
CCHS cycles.
2. DHHGAGE_D: age variable categorized into 8 10-year age groups. Used in all
CCHS cycles.

Depression:
1. DPSDPP: Depression Scale - Predicted Probability. Used in all CCHS cycles.
2. DPSDSF: Depression Scale - Short Form Score. Used in all CCHS cycles
3. DIS_10G: Frequency - distress: felt sad / depressed - past month. Used in
CCHS cycles 2005-2014.
4. DIS_10H: Frequency - distress: depressed/nothing cheers - past month. Used in
CCHS cycles 2005-2014.
5. DPSDMT: Specific month when felt depressed. Used in all CCHS cycles.
6. DPSDWK: Number of weeks felt depressed - (D). Used in all CCHS cycles.
7. DPS_02: Felt sad/blue/depressed - 2 weeks or more - 12 mo. Used in CCHS
cycles 2007-2012.
8. DPS_03: Sad/depressed - length feelings lasted - 2 wk. Used in CCHS cycles
2007-2012.
9. DPS_04: Sad/depressed - frequency - 2 wk. Used in CCHS cycles 2007-2012.
10. DPS_05: Sad/depressed - lose interest in things - 2 wk. Used in CCHS cycles
2007-2012.
11. DPS_06: Sad/depressed - felt tired out / low on energy - 2 wk. Used in CCHS
cycles 2007-2012.
12. DPS_07: Sad/depressed - weight change - 2 wk. Used in CCHS cycles 2007-2012.
13. DPS_08A: Sad/depressed - weight change (amount) - 2 wk. Used in CCHS cycles
2007-2012.
14. DPS_08B: Sad/depressed - weight change (lb/kg) - 2 wk. Used in CCHS cycles
2007-2012.
15. DPS_09: Sad/depressed - trouble falling asleep - 2 wk. Used in CCHS cycles
2007-2012.
16. DPS_10: Sad/depressed trouble falling asleep - frequency - 2 wk. Used in
CCHS cycles 2007-2012.
17. DPS_11: Sad/depressed - trouble concentrating - 2 wk. Used in CCHS cycles
2007-2012.
18. DPS_12: Sad/depressed - felt down on self - 2 wk. Used in CCHS cycles
2007-2012.
19. DPS_13: Sad/depressed - thought a lot about death - 2 wk. Used in CCHS
cycles 2007-2012.
20. DPS_14: Sad/depressed - number of weeks - 12 mo. Used in CCHS cycles
2007-2012.

Influenza:
1. FLU_160: Ever had a flu shot. Used in all CCHS cycles.
2. FLU_162: Last time had flu shot. Used in all CCHS cycles.

# cchsflow 1.6.0 
2020-03-30

## Breaking changes
- Refactored derived variable functions to handle tagged_na
- Added new parameter ALW_1 to `binge_drinker_fun()`

## Bug Fixes
- Bug fix to `resp_condition_fun1()` that was not correctly handling respondents
under 35 with respiratory conditions

## Variables
- <font color = "green">**New**</font> SDC_5A_1 - Knowledge of official
languages. Used in CCHS cycles 2011-2014
- <font color = "green">**New**</font> SDCDFOLS - First official language
spoken. Used in CCHS cycles 2011-2014
- <font color = "green">**New**</font> SDCGLHM - Languages spoken at home. Used
in CCHS cycles 2007-2014
- <font color = "green">**New**</font> SDCGLNG - Languages - can converse in.
Used in CCHS cycles 2001-2010

# cchsflow 1.5.2
2020-03-24

## Bug Fixes
- Fixes to typos in `variables.csv` & `variable_details.csv`

# cchsflow 1.5.1
2020-03-20

## Minor improvements
- Minor hotfix in description of package

# cchsflow 1.5.0 
2020-03-20

## Breaking changes
- Modified internal list index access to be compliant with R 4.0.
- Added `attach_data_name`: new argument for adding data_name to
`rec_with_table()` return

## Variables
- <font color = "green">**New**</font> [ADL_der](https://big-life-lab.github.io/cchsflow/reference/adl_fun.html) -
derived "needs help with task" variable.
- <font color = "green">**New**</font> [RACDPAL_fun](https://big-life-lab.github.io/cchsflow/reference/RACDPAL_fun.html) - custom function to derive RACDPAL for the 2001 CCHS survey cycle.
- <font color = "green">**New**</font> ADL_07 - Needs help with heavy household
chores. Recoded variable used in CCHS cycles 2001-2005. 
- <font color = "green">**New**</font> ADM_RNO - Sequential record number used
in all CCHS cycles.
- <font color = "green">**New**</font> RAC_2B - Reduction in activities at
school or work due to disability variable used only in the 2001 CCHS survey
cycle
- <font color = "green">**New**</font> INCGPER_cont - continuous personal income
variable harmonized across all CCHS cycles

## Minor improvements
- New [article](https://big-life-lab.github.io/cchsflow/articles/duplicate_datasets.html)
that describes overlaps in certain CCHS datasets
- Documentation on [GEN_02A2](https://big-life-lab.github.io/cchsflow/reference/GEN_02A2.html)
variable
- Expanded derived variables section in [How to add variables](https://big-life-lab.github.io/cchsflow/articles/how_to_add_variables.html) article
- Added example in [get started](https://big-life-lab.github.io/cchsflow/articles/get_started.html)
that uses new attach_data_name argument in `rec_with_table()`

# cchsflow 1.4.4
2020-03-03

## Features
- Docsearch added to website
- New article on getting started with _cchsflow_

## Bug fixes
- Refactor to `pct_time_fun` function to output value from 0-100% as opposed
to 0-1
- Refactor to respiratory functions to better capture respondents under 35
with respiratory conditions

## Minor improvements
- Improvements to warning outputs in `rec_with_table()`
- Improvements to labels in `variables.csv` & `variable_details.csv`

# cchsflow 1.4.3 
2020-02-21

## Minor improvements
- Minor documentation improvements in vignettes and home page

# cchsflow 1.4.2 
2020-02-19

## Bug fixes and minor improvements
- New badges and documentation updates to reflect package being added to CRAN
- Fixed bug in `rec_with_table()` to include all values in recFrom column
- Improvements in labelling of variables in `variables.csv` &
`variable_details.csv`, as well as labelling of derived categorical variables

# cchsflow 1.4.1
2020-02-11

## Minor improvements
- Suppressed warning messages in function examples
- Minor edits & corrections to [tagged_na](https://big-life-lab.github.io/cchsflow/articles/tagged_na_usage.html), [variable_details](https://big-life-lab.github.io/cchsflow/articles/variable_details.html), and
[how to add variables](https://big-life-lab.github.io/cchsflow/articles/how_to_add_variables.html) articles.
- Minor changes to titles of sample datasets
- Minor refactor in `rec_with_table()` to match output type with specified toType for derived variables
- Bug fixes to Reference page

# cchsflow 1.4.0 
2020-02-10

## Breaking changes
- Added to _p suffix to the names of datasets & databaseStarts to denote CCHS
PUMF data

## Features
- Badges added to `README.md` with `usethis`.

## Variables
- <font color = "green">**New**</font> [multiple conditions](https://big-life-lab.github.io/cchsflow/reference/multiple_conditions_fun1.html) -
derived multiple chronic conditions variable
- Modifications to derived respiratory condition variable to include respondents
with asthma.

## Minor improvements
- Expansion to [tagged_na](https://big-life-lab.github.io/cchsflow/articles/tagged_na_usage.html)
article

# cchsflow 1.3.2
2020-02-04

## Variables
- <font color = "green">**New**</font> [binge_drinker](https://big-life-lab.github.io/cchsflow/reference/binge_drinker_fun.html) -
derived binge drinking variable

## Minor improvements
- Issue and variable request templates added on Github
- Minor edits to the labels and sections in `variables.csv`
- Minor correction to `DESCRIPTION`
- Minor corrections to the documentation of [`resp_condition_fun1()`](https://big-life-lab.github.io/cchsflow/reference/resp_condition_fun1.html),
[`resp_condition_fun2()`](https://big-life-lab.github.io/cchsflow/reference/resp_condition_fun2.html),
[`resp_condition_fun3()`](https://big-life-lab.github.io/cchsflow/reference/resp_condition_fun3.html),

# cchsflow 1.3.1
2020-01-28

## Variables
- <font color = "green">**New**</font> CCC_31A. Ever diagnosed with cancer
- <font color = "green">**New**</font> CCC_072. Ever diagnosed with hypertension
- <font color = "green">**New**</font> CCC_073. Taken blood pressure medication
in the last month
- <font color = "green">**New**</font> HCU_1AA. Has regular medical doctor

# cchsflow 1.3.0 
2020-01-23

## Breaking changes
- Changed `print_note()` parameter in `rec_with_table()` to `notes`.

## Variables
- <font color = "green">**New**</font> food_insecurity_der. Derived food
insecurity variable.

## Major improvements
- `message()` is now used to print messages onto console. This enables the
ability to easily suppress console output for testing purposes.

## Minor improvements and bug fixes
- Minor edits to `DESCRIPTION`

# cchsflow 1.2.0 
2020-01-13

## Breaking changes
- Changed dataset_name to database_name
- Added a simple `rec_with_table()` function call that uses `data` variable name 
as `database_name` as well as default `variables` and `variable_details` that
come  with the package.

## Minor improvements and bug fixes
- Autolinked DOI in `DESCRIPTION`
- Fixes to R chunks and minor edits in `README.md` 
- Modified tests and examples to use the new simplified `rec_with_table()` call
- Modified vignettes to reflect use of new `rec_with_table()` call

# cchsflow 1.1.0
2020-01-08

## Breaking changes

* Changed `data_source` parameter in `rec_with_table()` to `data`.

* Changed order of parameters to have `variables` after `dataset_name`.

## New features

- Added warnings to `set_data_labels()` when a variable is present in details 
    but missing in variables and vice versa.
- Added automated testing for functions

## Minor improvements and bug fixes

- Updated `DESCRIPTION` to expand description of package and added The Ottawa
   Hospital as author.
- Refactored `age_cat_fun()` to account for age below 0 and include warning
   messages for respiratory functions.
- Added trailing white space removal to `set_data_labels()`.
- Added trailing white space removal to startVariable during `rec_with_table()`.
- Added missing contributing code of conduct page.
- Minor formatting changes on vignettes and `README.md` for better clarity.
- Fixed broken links in articles.

# cchsflow 1.0.1 
2019-12-19

## Features
- Updated `LICENSE`
- Bug fixes to hyperlinks in README.md and NEWS.md

# cchsflow 1.0.0
2019-12-18

## News

- Submitted package to CRAN

## Features

- Switched formatting to snake_case
- Corrections to "using cchsflow" article
- Removed DDI files 
  
## Dependency Changes

- Dependency lowered to R 3.2
- Dependency set for haven (>= 1.1.2)
- Dependency set for dplyr (>= 0.8.2)
- Dependency set for sjlabelled (>= 1.0.17)
- Dependency set for stringr (>= 1.2.0)
- Added [bllflow](https://bllflow.projectbiglife.ca/) functions to package to avoid dependency to 
  package 


# cchsflow 0.2.3
2019-11-29

## Features

- Updated example in the [introduction](https://big-life-lab.github.io/cchsflow/index.html) to reflect data that is now included in the cchsflow package.
- Updates to vignettes to reflect latest version of package
- Updates to variables.csv & variableDetails.csv

### Variables

-  <font color="green">**New**</font> [DHHGAGE_C](https://big-life-lab.github.io/cchsflow/reference/age_cat_fun.html) - categorical age variable that groups various age categories across all CCHS cycles. Based on the continuous age variable (`DHHGAGE_cont`) that is also harmonious across all CCHS cycles.
-  <font color = "green">**New**</font> WTS_M - sampling weight variable
-  <font color = "green">**New**</font> DHHGHSZ - household size
-  <font color = "green">**New**</font> INCGHH_A, INCGHH_B, INCGHH_C, INCGHH_cont - Total household income from all sources. The categories for household income vary across CCHS cycles so three categorical variables were added along with a continuous variable that harmonizes household income across all CCHS cycles by taking the midpoint of each category.

# cchsflow 0.2.2 

2019-11-26

## Features

- Updated ref branch of bllflow to match latest version of `RecWTable()`
- Minor formatting changes in data, function, and derived variable documentation to improve 
  readability
- Minor corrections to derived variables article

# cchsflow 0.2.1 

2019-11-19

## Features

- Documentation for derived variable functions now available
- Documentation for derived alcohol variables now available

# cchsflow 0.2.0 

## Features 

- Added Support for derived variables alongside vignettes explaining how to add new variables
- Added R file with custom functions for derived variables
- Changed naming to support short database names ex. cchs2001
- Added sample data for example purposes

# cchsflow 0.1.0 (First Version)

## Features

- Added variables.csv that contains list of CCHS variables in cchsflow.
- Added variableDetails.csv that maps variables across CCHS cycles from 2001-2014 
- Added Vignettes to describe the process of using RecWTable(part of bllflow) to combine the data

