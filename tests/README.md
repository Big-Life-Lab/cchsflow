# Testing a derived function

The `test_derived_function` should be used when testing a derived function. This
helper function is stored within the `tests/testthat/helper-utils.R` file. The
recommended workflow with this function is to:

1. Create a CSV file that has all your test cases for the derived function
   within the `tests/testdata` folder
2. Create a testthat test case that reads in that CSV file using the `read.csv`
   function
3. Call the `test_derived_function` function within the test case with the 
   read in CSV file and the function to test

Take a look at the `pack_years_fun` test within the
`tests/testthat/test-smoking.R` file for an example.
