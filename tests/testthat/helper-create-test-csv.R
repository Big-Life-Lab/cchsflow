#' Create a CSV file to use in a test
#'
#' @param content a vector containing the content of the file. Each item in the
#' vector should be a row in the CSV file. For example, the first item is the
#' first row, the second item is the second row etc.
#' @param envir DO NOT SET THIS. This is used to properly clean up the created
#' file.
#'
#' @return the path to the created file
#' @examples
#' \dontrun {
#' csv_file_content <- c(
#'   "col_1,col2\n",
#'   "1,2\n"
#' )
#' csv_file_path <- create_test_csv(csv_file_content)
#' }
create_test_csv <- function(content, envir = parent.frame()) {
  test_csv_path <- withr::local_tempfile(fileext = ".csv", .local_envir = envir)
  test_csv_content <- paste(content, collapse = "")
  cat(test_csv_content, file = test_csv_path)
  return(test_csv_path)
}

