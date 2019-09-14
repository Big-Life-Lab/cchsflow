# cchsflow

This repository contains supports the use of the Canadian Community Health Survey (CCHS). The current focus is transformation of harmonized variables across surveys from 2001 to 2014. 

Documents include:

1. `variables.csv` - a list of variables that can be transformed across CCHS surveys. The default variable name corresponds to 2007 CCHS.  
2. `variableDetails.csv` - information that describes how the variables are recoded. 
3. DDI documents for the original CCHS surveys -- see CCHS_DDI folder.

## Important notes

Care must be taken to understand how your specific use of variable transformation and harmonization may result in misclassfication error and other forms of bias. Most variables have had some change in wording and category responses across the lifetime of the CCHS from 2001 to 2013. Furthermore, there have been changes in survey sampling, response rates, weighting methods and other survey design changes that affect responses.  

The transformations that are described in this repository have been used in several research projects (see reference list) but no guarantees are made regarding the accuracy or appropriate uses.

## Contributing

Please follow [this guide](CONTRIBUTING.md) if you like to contribute to
the *cchsflow* package.
