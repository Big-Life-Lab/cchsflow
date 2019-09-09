# cchsflow

This repository contains supports the use of the Canadian Community Health Survey (CCHS). The current focus is transformation of harmonized variables across surveys from 2001 to 2014. 

The CCHS is the Canada's main population health survey. The survey is adminstered by Statistics Canada. Information about the survey can be found [here](http://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=3226).

Documents include:

1. `variables.csv` - a list of variables that can be transformed across CCHS surveys. The default variable name corresponds to 2007 CCHS.  
2. `variableDetails.csv` - information that describes how the variables are recoded. 
3. DDI documents for the original CCHS surveys -- see CCHS_DDI folder.

This repository does not include the CCHS data. Information for how to access the CCHS data can be found [here](https://www150.statcan.gc.ca/n1/pub/82-620-m/2005001/4144189-eng.htm). Canadian university community can also access the CCCHS through [Odesi](http://odesi2.scholarsportal.info/webview/) -- See health/Canada/Canadian Community Health Survey.

## Important notes

Care must be taken to understand how your specific use of variable transformation and harmonization in `cchsflow` affects your study or use of CCHS data. Combining CCHS will result in misclassfication error and other forms of bias that affects studies in different ways. Most CCHS variables have had at least some change in wording and category responses. Furthermore, there have been changes in survey sampling, response rates, weighting methods and other survey design changes that affect responses. 

The transformations that are described in this repository have been used in several research projects (see reference list) but no guarantees are made regarding the accuracy or appropriate uses.

## Contributing

Feel free to contribute to this repository. We particularly welcome additional variable transformations and derived variables that you believe may be helpul to the broad CCHS community. 
