## Resubmission

This is a resubmission. In this version I have:
* Enabled the use of message() to suppress messages on console
* Added quotation marks to 'sjmisc' in `DESCRIPTION`
* Added food_insecurity_der as a new derived variable

Note:
* In `rec_with_table()`, we prefer to set the `print_note` parameter to be
set to `TRUE` as a default. We believe the note is important information for the 
approapriate use of the CCHS survey data. Defaulting the message() to console is 
analagous to messages for other common R functions and packages. e.g. 
install.package() notes or devtools check notes. 

`cchsflow` is intended to be used byresearchers examining data from the Canadian 
Community Health Survey (CCHS)between 2001 and 2014. During this time period, 
there have been major and minormodification to variables between survey cycles. 
The purpose of the `print_note`parameter is to inform users of any potential 
changes in variables that have been transformed and harmonized between survey 
cycles. Given that these changes may impact their research, we believe it is 
imperative to default `print_note` to `TRUE` so they do not miss these notes.

The previous CRAN reviewer suggested creating an object for the notes. We have 
considered this approach but our users have indicated that this object would be 
infrequently used. Therefore, we have not created an object at this time.

## Test environments
* local OS X install, R 4.0.0
* ubuntu 14.04 (on travis-ci), R 4.0.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

