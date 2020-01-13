## Resubmission

This is a resubmission. In this version I have:
* Autolinked DOI in `DESCRIPTION`
* Made fixes to R chunk in `README.md` 
* Changed dataset_name to database_name
* Added a simple `rec_with_table()` function call that uses `data` variable name 
as `database_name` as well as default `variables` and `variable_details` that
come  with the package.
* Modified tests and examples to use the new simplified rec_with_table call

## Test environments
* local OS X install, R 4.0.0
* ubuntu 14.04 (on travis-ci), R 4.0.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

