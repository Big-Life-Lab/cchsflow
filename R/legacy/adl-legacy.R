#' @title Activities of Daily Living (ADL) help indicator
#' 
#' @description Creates a binary indicator variable for whether a person needs
#'   help with any activities of daily living. This harmonized variable is based
#'   on the CCHS derived variable ADLF6R but uses ADL variables that are
#'   consistent across all CCHS survey cycles.
#' 
#' @details The CCHS derived variable ADLF6R uses different ADL variables across
#'  the various CCHS survey cycles. This newly derived variable (ADL_der) uses
#'  ADL variables that are consistent across CCHS cycles.
#'  
#'  In the 2001 CCHS survey cycle, the ADLF6R variable examines the following
#'  ADL variables:
#'  \enumerate{
#'   \item ADL_01 - Needs help preparing meals
#'   \item ADL_02 - Needs help getting to appointments/errands
#'   \item ADL_03 - Needs help doing housework
#'   \item ADL_04 - Needs help doing personal care
#'   \item ADL_05 - Needs help moving inside house
#'   \item ADL_07 - Needs help doing heavy household chores
#'  }  
#' 
#'  In the 2003-2005 CCHS survey cycles, the ADLF6R variable examines the
#'  following ADL variables:
#'  \enumerate{
#'   \item ADL_01 - Needs help preparing meals
#'   \item ADL_02 - Needs help getting to appointments/errands
#'   \item ADL_03 - Needs help doing housework
#'   \item ADL_04 - Needs help doing personal care
#'   \item ADL_05 - Needs help moving inside house
#'   \item ADL_06 - Needs help doing finances
#'   \item ADL_07 - Needs help doing heavy household chores
#'  }
#'  
#'  In the 2007-2014 CCHS survey cycles, the ADLF6R variable examines the
#'  following ADL variables:
#'  \enumerate{
#'   \item ADL_01 - Needs help preparing meals
#'   \item ADL_02 - Needs help getting to appointments/errands
#'   \item ADL_03 - Needs help doing housework
#'   \item ADL_04 - Needs help doing personal care
#'   \item ADL_05 - Needs help moving inside house
#'   \item ADL_06 - Needs help doing finances
#'  }
#'  
#'  This newly derived variable (ADL_der) uses ADL_01 to ADL_05 which are
#'  consistent across all survey cycles. For any single CCHS survey year,
#'  it is appropriate to use ADLF6R. ADL_der is recommended when using multiple
#'  survey cycles.     
#' 
#' @param ADL_01 A numeric value indicating if help is needed preparing meals.
#'   Valid values: 1 = needs help, 2 = does not need help.
#' @param ADL_02 A numeric value indicating if help is needed getting to
#'   appointments/errands. Valid values: 1 = needs help, 2 = does not need help.
#' @param ADL_03 A numeric value indicating if help is needed doing housework.
#'   Valid values: 1 = needs help, 2 = does not need help.
#' @param ADL_04 A numeric value indicating if help is needed doing personal care.
#'   Valid values: 1 = needs help, 2 = does not need help.
#' @param ADL_05 A numeric value indicating if help is needed moving inside house.
#'   Valid values: 1 = needs help, 2 = does not need help.
#' 
#' @return A numeric value with the following categories:
#'   \itemize{
#'     \item \code{1}: Needs help with at least one task
#'     \item \code{2}: Does not need help with any task
#'     \item \code{"NA(b)"}: Missing or invalid input values
#'   }
#' 
#' @examples
#' # Person who needs help with at least one task
#' adl_fun(ADL_01 = 1, ADL_02 = 2, ADL_03 = 2, ADL_04 = 2, ADL_05 = 2)
#' # Returns: 1 (needs help with preparing meals)
#' 
#' # Person who needs no help with any task
#' adl_fun(ADL_01 = 2, ADL_02 = 2, ADL_03 = 2, ADL_04 = 2, ADL_05 = 2)
#' # Returns: 2 (does not need help)
#' 
#' # Person who needs help with multiple tasks
#' adl_fun(ADL_01 = 1, ADL_02 = 1, ADL_03 = 2, ADL_04 = 2, ADL_05 = 2)
#' # Returns: 1 (needs help with meals and appointments)
#' 
#' # Invalid input
#' adl_fun(ADL_01 = 0, ADL_02 = 2, ADL_03 = 2, ADL_04 = 2, ADL_05 = 2)
#' # Returns: "NA(b)" (invalid input value)
#'
#' @seealso \code{\link{adl_score_5_fun}} for counting the number of ADL tasks
#'   requiring help.
#' 
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced for new ADL_score_6 variable support
#' @export
adl_fun <- function (ADL_01, ADL_02, ADL_03, ADL_04, ADL_05) {
  # Check to see if all values are in range
  if_else2((ADL_01 %in% 1:2) & (ADL_02 %in% 1:2) & (ADL_03 %in% 1:2) &
             (ADL_04 %in% 1:2) & (ADL_05 %in% 1:2),
           # Examining if respondents needed help with any task
           if_else2(ADL_01 == 1 | ADL_02 == 1 | ADL_03 == 1 | ADL_04 == 1 |
                      ADL_05 == 1, 1,
                    # Examining if respondents do not need help with any task
                    if_else2(ADL_01 == 2 & ADL_02 == 2 & ADL_03 == 2 &
                               ADL_04 == 2 & ADL_05 == 2, 2, "NA(b)")),
           # Return NA if any ADL variable is out of range
           "NA(b)"
           )

}

#' @title ADL help score (count of tasks requiring help)
#'
#' @description Counts the number of activities of daily living (ADL) tasks
#'   that require help, creating a 6-category variable (0-5). This provides
#'   a more granular measure of functional limitation compared to the binary
#'   ADL indicator. Uses ADL variables that are consistent across all CCHS
#'   cycles from 2001 to 2014.
#'
#' @param ADL_01 A numeric value indicating if help is needed preparing meals.
#'   Valid values: 1 = needs help, 2 = does not need help, "NA(a)" = not applicable,
#'   "NA(b)" = missing.
#' @param ADL_02 A numeric value indicating if help is needed getting to
#'   appointments/errands. Valid values: 1 = needs help, 2 = does not need help,
#'   "NA(a)" = not applicable, "NA(b)" = missing.
#' @param ADL_03 A numeric value indicating if help is needed doing housework.
#'   Valid values: 1 = needs help, 2 = does not need help, "NA(a)" = not applicable,
#'   "NA(b)" = missing.
#' @param ADL_04 A numeric value indicating if help is needed doing personal care.
#'   Valid values: 1 = needs help, 2 = does not need help, "NA(a)" = not applicable,
#'   "NA(b)" = missing.
#' @param ADL_05 A numeric value indicating if help is needed moving inside house.
#'   Valid values: 1 = needs help, 2 = does not need help, "NA(a)" = not applicable,
#'   "NA(b)" = missing.
#'
#' @return A numeric value representing the count of ADL tasks requiring help:
#'   \itemize{
#'     \item \code{0}: No tasks require help
#'     \item \code{1}: One task requires help
#'     \item \code{2}: Two tasks require help
#'     \item \code{3}: Three tasks require help
#'     \item \code{4}: Four tasks require help
#'     \item \code{5}: All five tasks require help
#'     \item \code{"NA(a)"}: Not applicable (if any input is "NA(a)")
#'     \item \code{"NA(b)"}: Missing (if any input is "NA(b)")
#'   }
#'
#'
#' @examples
#' # Person needing no help with any tasks
#' adl_score_5_fun(ADL_01 = 2, ADL_02 = 2, ADL_03 = 2, ADL_04 = 2, ADL_05 = 2)
#' # Returns: 0
#' 
#' # Person needing help with two tasks
#' adl_score_5_fun(ADL_01 = 1, ADL_02 = 2, ADL_03 = 1, ADL_04 = 2, ADL_05 = 2)
#' # Returns: 2 (help needed with meals and housework)
#' 
#' # Person needing help with all tasks
#' adl_score_5_fun(ADL_01 = 1, ADL_02 = 1, ADL_03 = 1, ADL_04 = 1, ADL_05 = 1)
#' # Returns: 5
#' 
#' # Missing data example
#' adl_score_5_fun(ADL_01 = "NA(b)", ADL_02 = 1, ADL_03 = 1, ADL_04 = 2, ADL_05 = 2)
#' # Returns: "NA(b)" (any missing value results in missing score)
#'
#' @seealso \code{\link{adl_fun}} for a binary ADL help indicator.
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced validation and calculation improvements
#' @export
adl_score_5_fun <-
  function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05) {
    
    # Create vector of ADL input variables
    all_adl_vector <- c(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05)
    # Count the number of missing values in vector
    count_missing_adl <- sum(all_adl_vector == "NA(b)")
    # Count the number of not applicable values in vector
    count_not_applicable_adl <- sum(all_adl_vector == "NA(a)")
    # Count the number of ADLs that require help (value of 1)
    count_adl <- sum(all_adl_vector == 1)
    
    
    # If the individual had missing data for any of the variables then set
    # the score to missing
    # If the individual has "not applicable" for any of the variables, then
    # set the score to "not applicable"
    # Otherwise set the score for each individual to the count of the number
    # of tasks they needed help with
    total_num_adls <- 5
    ADL_score_5 <-
      ifelse(
        count_not_applicable_adl >= 1,
        "NA(a)",
        ifelse(
          count_missing_adl >= 1,
          "NA(b)",
          count_adl
        )
      )
    
    return(ADL_score_5)
  }
#'
#'  This newly derived variable (ADL_der_B) uses ADL_01 to ADL_06 which are
#'  consistent across survey cycles from 2003-2014. ADL_der_B is recommended when using multiple
#'  survey cycles.     
#' 
#' @param ADL_01 Needs help preparing meals
#' 
#' @param ADL_02 Needs help getting to appointments/errands
#' 
#' @param ADL_03 Needs help doing housework
#' 
#' @param ADL_04 Needs help doing personal care
#' 
#' @param ADL_05 Needs help moving inside house
#' 
#' @param ADL_06 Needs help doing finances
#' 
#' #' # Using adl_fun_C() to generate to ADL_der_C based on user inputted values.
#' # 
#' # Let's say you do not need help preparing meals, you need help getting to
#' # appointments or errands, you need help doing housework, do not need help
#' # doing personal care, do not need help moving inside the house, and do not 
#' # need help doing fiances. Using adl_fun_B() we can check if you need help 
#' # doing tasks
#' 
#' ADL_der_C <- adl_fun_B(2, 1, 1, 2, 2, 2)
#' 
#' print(ADL_der_C)
#' 
#' @export
adl_fun_B <- function (ADL_01, ADL_02, ADL_03, ADL_04, ADL_05, ADL_06) {
  # Check to see if all values are in range
  if_else2((ADL_01 %in% 1:2) & (ADL_02 %in% 1:2) & (ADL_03 %in% 1:2) &
             (ADL_04 %in% 1:2) & (ADL_05 %in% 1:2) & (ADL_06 %in% 1:2),
           # Examining if respondents needed help with any task
           if_else2(ADL_01 == 1 | ADL_02 == 1 | ADL_03 == 1 | ADL_04 == 1 |
                      ADL_05 == 1 | ADL_06 == 1, 1,
                    # Examining if respondents do not need help with any task
                    if_else2(ADL_01 == 2 & ADL_02 == 2 & ADL_03 == 2 &
                               ADL_04 == 2 & ADL_05 == 2 & ADL_06 == 2, 2, "NA(b)")),
           # Return NA if any ADL variable is out of range
           "NA(b)"
  )
  
}
#' @title The number of activities of daily living tasks that require help.
#'
#' @description A 7 category variable (ADL_score_6) representing the number of
#'  activities of daily living tasks that require help. This variable tallies
#'  the number of daily living tasks that a respondent requires help with based
#'  on various ADL variables that a respondent answered yes or no to. The ADL
#'  variables used are common across all CCHS cycles from 2003 to 2014.
#'
#' @param ADL_01 Needs help preparing meals.
#' @param ADL_02 Needs help getting to appointments/errands.
#' @param ADL_03 Needs help doing housework.
#' @param ADL_04 Needs help doing personal care.
#' @param ADL_05 Needs help moving inside house.
#' @param ADL_06 Needs help doing finances
#'
#' @return A derived variable (ADL_score_6) with 7 categories:
#' \enumerate{
#'   \item 0 - Needs help with 0 tasks
#'   \item 1 - Needs help with at least 1 task
#'   \item 2 - Needs help with at least 2 tasks
#'   \item 3 - Needs help with at least 3 tasks
#'   \item 4 - Needs help with at least 4 tasks
#'   \item 5 - Needs help with at least 5 tasks
#'   \item 6 - Needs help with at least 6 tasks
#' }
#'
#'
#' @examples
#' # Use adl_score_6_fun() to create the variable ADL_score_6 across CCHS 
#' # cycles adl_score_6_fun() is specified in variable_details.csv along with 
#' # the CCHS variables and cycles included.
#'
#' # To transform ADL_score_6, use rec_with_table() for each CCHS cycle
#' # and specify ADL_score_6, along with the various ADL variables.
#' # Then by using merge_rec_data() you can combine ADL_der across cycles.
#'
#' library(cchsflow)
#' adl2003 <- rec_with_table(
#'   cchs2003_p, c(
#'     "ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_score_6"
#'   )
#' )
#'
#' head(adl2003)
#'
#' adl2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_06", "ADL_score_6"
#'   )
#' )
#'
#' tail(adl2009_2010)
#'
#' combined_adl <- merge_rec_data(adl2003, adl2009_2010)
#'
#' head(combined_adl)
#'
#' tail(combined_adl)
#'
#' # Using adl_score_6_fun() to generate to ADL_score_6 based on user inputted
#' # values.
#' # Let's say you do not need help preparing meals, you need help getting to
#' # appointments or errands, you need help doing housework, do not need help
#' # doing personal care, do not need help moving inside the house, and do not 
#' # need help with finances. Using adl_score_6_fun() we can check the number of 
#' # tasks you need help with
#'
#' ADL_score_6 <- adl_score_6_fun(2, 1, 1, 2, 2, 2)
#'
#' print(ADL_score_6)
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: New function for 6-item ADL scoring with comprehensive validation
#' @export
adl_score_6_fun <-
  function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05, ADL_06) {
    
    # Create vector of ADL input variables
    all_adl_vector <- c(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05, ADL_06)
    # Count the number of missing values in vector
    count_missing_adl <- sum(all_adl_vector == "NA(b)")
    # Count the number of not applicable values in vector
    count_not_applicable_adl <- sum(all_adl_vector == "NA(a)")
    # Count the number of ADLs that require help (value of 1)
    count_adl <- sum(all_adl_vector == 1)
    
    
    # If the individual had missing data for any of the variables then set
    # the score to missing
    # If the individual has "not applicable" for any of the variables, then
    # set the score to "not applicable"
    # Otherwise set the score for each individual to the count of the number
    # of tasks they needed help with
    total_num_adls <- 6
    ADL_score_6 <-
      ifelse(
        count_not_applicable_adl >= 1,
        "NA(a)",
        ifelse(
          count_missing_adl >= 1,
          "NA(b)",
          count_adl
        )
      )
    
    return(ADL_score_6)
  }