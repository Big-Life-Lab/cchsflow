# cchsflow <img src="man/figures/logo.svg" align="right" alt="" width="180"/>

<!-- badges: start -->
[![Lifecycle:
development](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://img.shields.io/cran/v/cchsflow?color=green)](https://CRAN.R-project.org/package=cchsflow)
![](https://img.shields.io/github/v/release/big-life-lab/cchsflow?color=green&label=GitHub)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![](https://img.shields.io/badge/doi-10.17605/OSF.IO/HKUY3-yellowgreen.svg)](https://OSF.IO/HKUY3)
<!-- badges: end -->

*cchsflow* supports the use of the Canadian Community Health Survey (CCHS) by 
transforming variables from each cycle into harmonized, consistent versions that 
span survey cycles (currently, 2001 to 2014). 

The CCHS is a population-based cross-sectional survey of Canadians that has been 
administered every two years since 2001. There are approximately 130,000 
respondents per cycle. Studies use multiple CCHS cycles to examine trends over 
time and increase sample size to examine sub-groups that are too small to examine 
in a single cycle. 

The CCHS is one of the largest and most robust ongoing population health surveys 
worldwide. The CCHS, administered by Statistics Canada, is Canada's main general 
population health survey. Information about the survey is found [here](https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=3226). 
The CCHS has a [Statistic Canada Open Licence](https://www.statcan.gc.ca/eng/reference/licence).

## Concept

Each cycle of the CCHS contains over 1000 variables that cover the four main 
topics: sociodemographic measures, health behaviours, health status and health 
care use. The _seemingly_ consistent questions across CCHS cycles entice you to 
combine them together to increase sample size; however, you soon realize a 
challenge... 

Imagine you want to use BMI (body mass index) for a study that spans CCHS 2001 
to 2014. BMI _seems_ like a straightforward measure that is routinely-collected 
worldwide. Indeed, BMI is included in all CCHS cycles. You examine the 
documentation and find the variable `HWTAGBMI` in the CCHS 2001 corresponds to 
body mass index, but that in other cycles, the variable name changes to 
`HWTCGBMI`, `HWTDGBMI`, `HWTEGBMI`, etc. On reading the documentation, you 
notice that some cycles round the value to one decimal, whereas other cycles 
round to two digits. Furthermore, some cycles don't calculate BMI for 
respondents < age 20 or > 64 years. Also, some cycles calculate BMI only if 
height and weight are within specific ranges. These types of changes occur for 
almost all CCHS variables. Sometimes the changes are subtle and difficult to 
find in the documentation, even for seemingly straightforward variables such as 
BMI. `cchsflow` harmonizes the BMI variable across different cycles. 

## Usage

`cchsflow` creates harmonized variables (where possible) between CCHS cycles. 
Searching BMI in `variables` (described in the Introduction section of 
variableDetails.csv 
[vignette](https://big-life-lab.github.io/cchsflow/articles/variable_details.html)) 
shows `HWTGBMI` calculates BMI with two decimal places for all cycles for all 
respondents using the respondents' untruncated height and weight. 

*Calculate a harmonized BMI variable for CCHS 2001 cycle*

```
    # load test cchs data - included in cchsflow

    cchs2001_BMI <- rec_with_table(cchs2001_p, "HWTGBMI")
    
```

Notes printed to console indicate issues that may affect BMI classification for 
your study.
```
Loading cchsflow variable_details
Using the passed data variable name as database_name
NOTE for HWTGBMI : CCHS 2001 restricts BMI to ages 20-64
NOTE for HWTGBMI : CCHS 2001 and 2003 codes not applicable and missing 
variables as 999.6 and 999.7-999.9 respectively, while CCHS 2005 onwards codes 
not applicable and missing variables as 999.96 and 999.7-999.99 respectively
NOTE for HWTGBMI : Don't know (999.7) and refusal (999.8) not included
in 2001 CCHS"
```

## Important notes

*Combining CCHS across survey cycles will result in misclassification error and 
other forms of bias that affects studies in different ways.* The transformations 
that are described in this repository have been used in several research 
projects, but there are no guarantees regarding the accuracy or appropriate 
uses.

Care must be taken to understand how specific variable transformation and 
harmonization with `cchsflow` affect your study or use of CCHS data. Across 
survey cycles, almost all CCHS variables have had at least some change in 
wording and category responses. Furthermore, there have been changes in survey 
sampling, response rates, weighting methods and other survey design changes that 
affect responses. 

## Installation

```
    # Install release version from CRAN
    install.packages("cchsflow")

    # Install the most recent version from GitHub
    devtools::install_github("Big-Life-Lab/cchsflow")
```

Do you just want new variables not yet added to the CRAN version?

You can download and use the latest version of 
[`variables.csv`](https://github.com/Big-Life-Lab/cchsflow/blob/master/inst/extdata/variables.csv)
and [`variable_details.csv`](https://github.com/Big-Life-Lab/cchsflow/blob/master/inst/extdata/variable_details.csv) 
from GitHub.
    
## What is in the `cchsflow` package?

*cchsflow* package includes:

1. `variables.csv` - a list of variables that can be transformed across CCHS 
surveys.  
2. `variable_details.csv` - information that describes how the variables are 
recoded.
3. Vignettes - that describe how to use R to transform or generate new derived 
variables that are listed in `variables.csv`. Transformations are performed 
using `rec_with_table()`. `variables.csv` and `variable_details.csv` can be 
used with other statistics programs (see [issue](https://github.com/Big-Life-Lab/cchsflow/issues)).
4. Demonstration CCHS data -  `cchsflow` includes a random sample of 200 
respondents from each CCHS PUMF file from 2001 to 2014. These data are used for 
the vignettes. 
The CCHS test data is stored in /data as .RData files. They can be read as a 
package database.

```
# read the CCHS 2014 PUMF test data

test_data <- cchs2014_p
```

This repository does not include the full CCHS data. Information on how to 
access the CCHS data can is 
[here](https://www150.statcan.gc.ca/n1/pub/82-620-m/2005001/4144189-eng.htm). 
The Canadian university community can also access the CCHS through 
[ODESI](http://odesi2.scholarsportal.info/webview/)
(see health/Canada/Canadian Community Health Survey).

### Roadmap

Project on the roadmap can be found on [here](https://github.com/Big-Life-Lab/cchsflow/projects).

## Contributing

Please follow [this guide](https://big-life-lab.github.io/cchsflow/CONTRIBUTING.html) 
if you would like to contribute to the *cchsflow* package.

We encourage PRs for additional variable transformations and derived variables 
that you believe may be helpful to the broad CCHS community. 

Currently, *cchsflow* supports R through the `rec_with_table()` function. The 
CCHS community commonly uses SAS, Stata and other statistical packages. Please 
feel free to contribute to `cchsflow` by making a PR that creates versions of 
`rec_with_table()` for other statistical and programming languages.

## Statistics Canada Attribution

CCHS data used in this library is accessed and adapted in accordance to the 
Statistics Canada Open Licence Agreement.

Source from Statistics Canada, Canadian Community Health Survey 2001 to 2013 
PUMF, accessed Jan 2020. Reproduced and distributed on an "as is" basis with the 
permission of Statistics Canada.

Adapted from Statistics Canada, Canadian Community Health Surveys 2013 to 2013 
PUMF, accessed Jan 2020. This does not constitute an endorsement by Statistics 
Canada of this product.
