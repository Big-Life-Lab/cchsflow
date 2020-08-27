## Resubmission

This is a resubmission. In this version we have:
* Fixed a breaking bug in `variable_details()` that halted variable
transformation with the 2007-2008 CCHS cycle
* Fixed bug in `resp_condition_fun1()`
* Refactored derived variable functions to handle tagged_na
* Added new parameter ALW_1 to `binge_drinker_fun()`
* Added language variables to library

## Test environments
* local OS X install, R 4.0.0
* ubuntu 14.04 (on travis-ci), R 4.0.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

