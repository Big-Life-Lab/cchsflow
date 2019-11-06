# cchsflow <img src="man/figures/logo.svg" align="right" alt="" width="180"/>

[![Lifecycle:
development](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

*cchsflow* supports the use of the Canadian Community Health Survey (CCHS) through the common transformation of harmonized variables across survey cycles from 2001 to 2014. 

The CCHS is Canada's primary population health survey. Statistics Canada administers the survey. Information about the survey is found [here](http://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=3226). The CCHS has a [Statistic Canada Open Licence](https://www.statcan.gc.ca/eng/reference/licence).

*cchsflow* package includes:

1. `variables.csv` - a list of variables that can be transformed across CCHS surveys.  
2. `variableDetails.csv` - information that describes how the variables are recoded.
3. Vignettes describe how to use R to transform or generate new derived variables that are listed in `variables.csv`. Transformations are performed using `RecWTable()` from the `bllflow` R package. `variables.csv` and `variableDetails.csv` can be used with other stats programs. see [issue](https://github.com/Big-Life-Lab/cchsflow/issues).
3. Codebooks (metadata documents) for the original CCHS surveys -- see [CCHS_DDI](https://github.com/Big-Life-Lab/cchsflow/tree/master/inst/extdata/CCHS_DDI) folder. The PDF and DDI documents are a resource to examine how variables change across survey cycles. 

This repository does not include the CCHS data. Information on how to access the CCHS data can is [here](https://www150.statcan.gc.ca/n1/pub/82-620-m/2005001/4144189-eng.htm). The Canadian university community can also access the CCHS through [Odesi](http://odesi2.scholarsportal.info/webview/) -- See health/Canada/Canadian Community Health Survey.

## Usage

You want to use BMI (body mass index) for a study that spans CCHS 2001 to 2014. You find the variable `HWTAGBMI` in the CCHS 2001 corresponds to `Body mass index`, but that in other cycles, the variable name changes to `HWTCGBMI`, `HWTDGBMI`, `HWTEGBMI`, etc. On reading the documentation, you notice that some cycles round the value to one decimal, whereas other cycles round to two digits. Furthermore, some cycles don't calculate BMI for respondents < age 20 or > 64. Also, calculate BMI only if height and weight are within specific ranges. These types of changes occur for almost all CCHS variables. Sometimes the changes are subtle and difficult to find in the documentation, even for seemingly straightforward variables such as BMI.

Searching `BMI` in variables.csv shows `HWTGBMI` calculates BMI with two decimal places for all cycles for all respondents using the respondents' untruncated height and weight. 

    # calculate BMI for each CCHS cycle
    cchs2001_BMI <- RecWTable(dataSource = cchs2001, 
                variableDetails = varDetails, 
                datasetName = "cchs-82M0013-E-2001-c1-1-general-file", 
                appendToData = TRUE,  
                variables = "HWTGBMI")

## Important notes

*Combining CCHS across survey cycles will result in misclassification error and other forms of bias that affects studies in different ways.* The transformations that are described in this repository have been used in several research projects (see reference list), but there are no guarantees regarding the accuracy or appropriate uses.

Care must be taken by you to understand how specific variable transformation and harmonization with `cchsflow` affect your study or use of CCHS data. Across survey cycles, CCHS variables have had at least some change in wording and category responses. Furthermore, there have been changes in survey sampling, response rates, weighting methods and other survey design changes that affect responses. 

## Installation

    # If not installed, install the devtools
    install.packages("devtools")
    
    # then, install the package
    devtools::install_github("Big-Life-Lab/cchsflow")

## Contributing

Please follow [this guide](CONTRIBUTING.md) if you would like to contribute to
the *cchsflow* package.

We encourage PRs for additional variable transformations and derived variables that you believe may be helpful to the broad CCHS community. 

Currently, *cchsflow* supports R through the `RecWTable()` function. The CCHS community commonly uses SAS, Stata and other statistical packages. Please feel free to contribute to `cchsflow` by making a PR that creates versions of `RecWTable()` for other statistical and programming languages.
