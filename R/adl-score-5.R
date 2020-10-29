#' @title Derived variable. Number of tasks the individual needs help with
#'
#' @description A 6 category variables representing the number of tasks
#' an individual needs help with. It also has two categories to represent
#' "missing" and "not applicable". It uses the following five ADL variables
#' which are common accros CCHS cycles from 2001 to 2014:
#' \enumerate{
#'   \item ADL_01 - Needs help preparing meals
#'   \item ADL_02 - Needs help getting to appointments/errands
#'   \item ADL_03 - Needs help doing housework
#'   \item ADL_04 - Needs help doing personal care
#'   \item ADL_05 - Needs help moving inside house
#' }
#'
#' @param ADL_01 Needs help preparing meals
#' @param ADL_02 Needs help getting to appointments/errands
#' @param ADL_03 Needs help doing housework
#' @param ADL_04 Needs help doing personal care
#' @param ADL_05 Needs help moving inside house
#'
#' @return A derived variable (ADL_score_5) with 6 categories:
#' \enumerate{
#'   \item 0 - Needs help with 0 tasks
#'   \item 1 - Needs help with at least 1 task
#'   \item 2 - Needs help with at least 2 tasks
#'   \item 3 - Needs help with at least 3 tasks
#'   \item 4 - Needs help with at least 4 tasks
#'   \item 5 - Needs help with at least 5 tasks
#' }
#' A value of NA::b means at least one of the component variables had missing data
#' A value of NA::a means that none of the component variables had missing data
#' but at least one of them had a value of "not applicable"
#'
#' @export
#'
#' @examples
#' # Using adl_score_5_fun() to create ADL_score_5 values across CCHS cycles
#' # adl_score_5_fun() is specified in variable_details.csv along with the
#' # CCHS variables and cycles included.
#'
#' # To transform ADL_score_5, use rec_with_table() for each CCHS cycle
#' # and specify ADL_score_5, along with the various ADL variables.
#' # Then by using bind_rows() you can combine ADL_der across cycles.
#'
#' library(cchsflow)
#' adl2001 <- rec_with_table(
#'   cchs2001_p, c(
#'     "ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_score_5"
#'   )
#' )
#'
#' head(adl2001)
#'
#' adl2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_score_5"
#'   )
#' )
#'
#' tail(adl2009_2010)
#'
#' combined_adl <- bind_rows(adl2001, adl2009_2010)
#'
#' head(combined_adl)
#'
#' tail(combined_adl)
#'
#' # Using adl_score_5_fun() to generate to ADL_score_5 based on user inputted values.
#' #
#' # Let's say you do not need help preparing meals, you need help getting to
#' # appointments or errands, you need help doing housework, do not need help
#' # doing personal care, and do not need help moving inside the house. Using
#' # adl_score_5_fun() we can check the number of tasks you need help with
#'
#' ADL_score_5 <- adl_score_5_fun(2, 1, 1, 2, 2)
#'
#' print(ADL_score_5)
#'
adl_score_5_fun <-
  function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05) {
    # For each row in the ADL variables, count the total number of "not applicable"
    # values in each row
    count_missing_adl <- rep(0, length(ADL_01))
    # For each row of each of the above ADL variables, if they have a value of 
    # "missing" then increment the count for that row in the above
    # vector by one
    count_missing_adl <-
      ifelse(ADL_01 == "NA::b", count_missing_adl + 1, count_missing_adl)
    count_missing_adl <-
      ifelse(ADL_02 == "NA::b", count_missing_adl + 1, count_missing_adl)
    count_missing_adl <-
      ifelse(ADL_03 == "NA::b", count_missing_adl + 1, count_missing_adl)
    count_missing_adl <-
      ifelse(ADL_04 == "NA::b", count_missing_adl + 1, count_missing_adl)
    count_missing_adl <-
      ifelse(ADL_05 == "NA::b", count_missing_adl + 1, count_missing_adl)
    
    # For each row in the ADL variables, count the total number of missing
    # values in each row
    count_not_applicable_adl <- rep(0, length(ADL_01))
    # For each row of each of the above ADL variables, if they have a value of 
    # "not applicable" then increment the count for that row in the above
    # vector by one
    count_not_applicable_adl <-
      ifelse(ADL_01 == "NA::a",
             count_not_applicable_adl + 1,
             count_not_applicable_adl)
    count_not_applicable_adl <-
      ifelse(ADL_02 == "NA::a",
             count_not_applicable_adl + 1,
             count_not_applicable_adl)
    count_not_applicable_adl <-
      ifelse(ADL_03 == "NA::a",
             count_not_applicable_adl + 1,
             count_not_applicable_adl)
    count_not_applicable_adl <-
      ifelse(ADL_04 == "NA::a",
             count_not_applicable_adl + 1,
             count_not_applicable_adl)
    count_not_applicable_adl <-
      ifelse(ADL_05 == "NA::a",
             count_not_applicable_adl + 1,
             count_not_applicable_adl)
    
    # For each row in the ADL variables, count the total number of values
    # where the individual needed help with the task
    count_adl <- rep(0, length(ADL_01))
    count_adl <- ifelse(ADL_01 == "2", count_adl + 1, count_adl)
    count_adl <- ifelse(ADL_02 == "2", count_adl + 1, count_adl)
    count_adl <- ifelse(ADL_03 == "2", count_adl + 1, count_adl)
    count_adl <- ifelse(ADL_04 == "2", count_adl + 1, count_adl)
    count_adl <- ifelse(ADL_05 == "2", count_adl + 1, count_adl)
    
    # If the individual had missing data for any of the variables then set
    # the score to missing
    # If the inividual has "not applicable" for any of the variables, then
    # set the score to "not applicable"
    # Otherwise set the score for each individual to the count of the number
    # of tasks they needed help with
    total_num_adls <- 5
    ADL_score_5 <-
      ifelse(
        count_missing_adl >= 1,
        "NA::b",
        ifelse(
          count_not_applicable_adl >= 1,
          "NA::a",
          count_adl
        )
      )
    
    return(ADL_score_5)
  }
