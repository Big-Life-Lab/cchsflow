# cchsflow 1.4.0 (Latest build)
2020-02-07

## Breaking changes
- Added to _p suffix to the names of datasets & databaseStarts to denote CCHS
PUMF data

## Features
- Badges added to `README.md` with `usethis`.

## Variables
- <font color = "green">**New**</font> [multiple conditions](https://big-life-lab.github.io/cchsflow/reference/multiple_conditions_fun.html) -
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

