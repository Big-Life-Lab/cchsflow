#' Test a derived function created to run within recodeflow
#'
#' @param test_data A data.frame containing the data to test the function
#' against. The columns of the data.frame should contain all the arguments to 
#' the derived function. It should also contain a column called `expected`
#' which contains the expected value and a column called `notes` which is diplayed
#' when the test fails. The notes columns should describe briefly what the 
#' row is testing.
#' @param derived_function the derived function to test
#' @examples
#' BMI <- function(height, weight) {
#'   return(height/(weight*weight))
#' }
#' 
#' test_data <- data.frame(
#'    height = c(185, 160),
#'    weight = c(85, 70),
#'    expected = c(24.8, 27.3),
#'    notes = c("Normal weight", "Overweight")
#' )
#'
#' test_derived_function(test_data, BMI)
test_derived_function <- function(test_data, derived_function) {
  for(i in seq_len(nrow(test_data))) {
    test_datum <- test_data[i, ]
    arguments <- list()
    for(column in colnames(test_datum)) {
      if(column != "expected" & column != "notes") {
        arguments[[column]] <- test_datum[[column]]
      }
    }
    actual <- do.call(derived_function, arguments)

    expect_equal(
      actual,
      test_datum$expected,
      info = paste("Error in row", i, "when testing", test_datum$notes)
    )
  }
}
