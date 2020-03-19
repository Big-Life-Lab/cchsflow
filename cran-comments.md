## Resubmission

This is a resubmission. In this version we have:
* Fixed errors caused by R 4.0 devel 
* Fixed our tests
* Added new argument for rec_with_table()
* Added badges to `README.md`
* Added to _p suffix to the names of datasets & databaseStarts to denote CCHS
PUMF data
* Suppressed warning messages in function examples
* Refactored `rec_with_table()` to match output type with specified toType for
derived variables
* Fixed bug in `rec_with_table()` to include all values in recFrom column
* Improved warning outputs in `rec_with_table()`
* Refactored `pct_time_fun()` to output value from 0-100% as opposed
to 0-1
* Refactored respiratory functions to better capture respondents under 35
with respiratory conditions
* Added support for more CCHS variables

## Test environments
* local OS X install, R 4.0.0
* ubuntu 14.04 (on travis-ci), R 4.0.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

