#' @title Merge recoded data
#' 
#' @description This function allows users to merge CCHS data transformed by the
#'  \code{\link{rec_with_table}} function. This function generates a labelled
#'  merged data frame with multiple transformed CCHS cycles.
#'
#' @details When merging recoded CCHS data, there are variables that are missing
#'  in certain CCHS cycles. This function tags missing variable observations as
#'  NA(c), indicating that the variable was not asked or included in the CCHS
#'  cycle of the respondent. 
#'  
#'  Click \href{https://big-life-lab.github.io/cchsflow/articles/tagged_na_usage.html}(here)
#'  for more details on how NA's are treated in cchsflow.
#' 
#' @param data starting data frame.
#' 
#' @param ... additional data frames to be merged.
#' 
#' @return a merged data frame consisting of multiple recoded CCHS cycles with
#'  labels for variable names and tags for variables not included in particular
#'  CCHS cycles.
#' 
#' @example 
#' # Merging two CCHS cycles with variables missing in each cycle.
#' 
#' # INCGHH_A is a cchsflow variable available for the 2001 CCHS cycle, while
#' # INCGHH_B is a cchsflow variable available for the 2003 CCHS cycle. 
#' # Using merge_rec_data(), datasets containing INCGHH_A & INCGHH_B can be
#' # merged and tagged.
#' 
#' library(cchsflow)
#' income2001 <- rec_with_table(cchs2001_p, "INCGHH_A")
#' income2003 <- rec_with_table(cchs2001_p, "INCGHH_B")
#' 
#' income_merged <- merge_rec_data(income2001, income2003)
#' head(income_merged)
#' tail(income_merged)
#' 
#' @export
merge_rec_data <- function(data, ...) {
  # Step 1: bind datasets
  new_data <- bind_rows(data,...)
  
  # Step 2: apply NA(c) to untagged NA's
  for (i in names(new_data)) {
    NA_index <- is.na(new_data[[i]])
    tagged_NA_index <- haven::is_tagged_na(new_data[[i]])
    true_NA_index <- !(NA_index == tagged_NA_index)
    if (is.numeric(new_data[[i]])) {
      new_data[true_NA_index, i] <-
        haven::tagged_na("c")
    } else {
      if (!"NA(c)" %in% levels(new_data[[i]])) {
        levels(new_data[[i]]) <-
          c(levels(new_data[[i]]),
            "NA(c)")
      }
      new_data[true_NA_index, i] <-
        "NA(c)"
    }
  }
  # Step 3: apply labels to combined dataset
  labelled_data <- set_data_labels(new_data, variable_details,
                                   variables)
  return(labelled_data)
}