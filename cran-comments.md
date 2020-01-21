## Resubmission

This is a resubmission. In this version I have:
* Enabled messages to be suppressed on console and provided message() to enable
message output to console
* Added quotation marks to 'sjmisc' in `DESCRIPTION`
* Added food_insecurity_der as a new derived variable using
`food_insecurity_fun1()`, `food_insecurity_fun2()`, and `food_insecurity_fun3()`

Note:
* In `rec_with_table()`, we have decided to set the `print_note` parameter to be
set to `TRUE`. The reason being is that `cchsflow` is intended to be used by
researchers examining data from the Canadian Community Health Survey (CCHS)
between 2001 and 2014. During this time period, there have been major and minor
modification to variables between survey cycles. The purpose of the `print_note`
parameter is to inform users of any potential changes in variables that have
been transformed and harmonized between survey cycles. Given that these
changes may impact their research, we believe it is imperative to default
`print_note` to `TRUE` so they do not miss these notes. Other packages, such as
`devtools` have functions with messages to console that defaulted to `TRUE`.

## Test environments
* local OS X install, R 4.0.0
* ubuntu 14.04 (on travis-ci), R 4.0.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

