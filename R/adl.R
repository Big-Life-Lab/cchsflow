#' @title Derived needs help with tasks
#' 
#' @description This derived variable (ADL_der) is based on the CCHS derived
#'  variable ADLF6R which flags respondents who need help with tasks based on
#'  their response to the various activities of daily living (ADL) variables.
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
#' @return A derived variable (ADL_der) with 2 categories:
#'  \enumerate{
#'   \item - Needs help with tasks
#'   \item - Does not need help with tasks
#'  }
#' 
#' @examples 
#' # Using adl_fun() to create ADL_der values across CCHS cycles
#' # adl_fun() is specified in variable_details.csv along with the
#' # CCHS variables and cycles included.
#' 
#' # To transform ADL_der, use rec_with_table() for each CCHS cycle
#' # and specify ADL_der, along with the various ADL variables.
#' # Then by using merge_rec_data() you can combine ADL_der across cycles.
#' 
#' library(cchsflow)
#' adl2001 <- rec_with_table(
#'   cchs2001_p, c(
#'     "ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_der"
#'   )
#' )
#' 
#' head(adl2001)
#' 
#' adl2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05", "ADL_der"
#'   )
#' )
#' 
#' tail(adl2009_2010)
#' 
#' combined_adl <- merge_rec_data(adl2001, adl2009_2010)
#' 
#' head(combined_adl)
#' 
#' tail(combined_adl)
#' 
#' # Using adl_fun() to generate to ADL_der based on user inputted values.
#' # 
#' # Let's say you do not need help preparing meals, you need help getting to
#' # appointments or errands, you need help doing housework, do not need help
#' # doing personal care, and do not need help moving inside the house. Using
#' # adl_fun() we can check if you need help doing tasks
#' 
#' ADL_der <- adl_fun(2, 1, 1, 2, 2)
#' 
#' print(ADL_der)
#' 
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

#' @title The number of activities of daily living tasks that require help.
#'
#' @description A 6 category variable (ADL_score_5) representing the number of
#'  activities of daily living tasks that require help. This variable tallies
#'  the number of daily living tasks that a respondent requires help with based
#'  on various ADL variables that a respondent answered yes or no to. The ADL
#'  variables used are common across all CCHS cycles from 2001 to 2014.
#'
#' @param ADL_01 Needs help preparing meals.
#' @param ADL_02 Needs help getting to appointments/errands.
#' @param ADL_03 Needs help doing housework.
#' @param ADL_04 Needs help doing personal care.
#' @param ADL_05 Needs help moving inside house.
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
#'
#'
#' @examples
#' # Use adl_score_5_fun() to create the variable ADL_score_5 across CCHS 
#' # cycles adl_score_5_fun() is specified in variable_details.csv along with 
#' # the CCHS variables and cycles included.
#'
#' # To transform ADL_score_5, use rec_with_table() for each CCHS cycle
#' # and specify ADL_score_5, along with the various ADL variables.
#' # Then by using merge_rec_data() you can combine ADL_der across cycles.
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
#' combined_adl <- merge_rec_data(adl2001, adl2009_2010)
#'
#' head(combined_adl)
#'
#' tail(combined_adl)
#'
#' # Using adl_score_5_fun() to generate to ADL_score_5 based on user inputted
#' # values.
#' # Let's say you do not need help preparing meals, you need help getting to
#' # appointments or errands, you need help doing housework, do not need help
#' # doing personal care, and do not need help moving inside the house. Using
#' # adl_score_5_fun() we can check the number of tasks you need help with
#'
#' ADL_score_5 <- adl_score_5_fun(2, 1, 1, 2, 2)
#'
#' print(ADL_score_5)
#'
#' @export
adl_score_5_fun <-
  function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05) {
    
    # Create vector of ADL input variables
    all_adl_vector <- c(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05)
    # Count the number of missing values in vector
    count_missing_adl <- sum(all_adl_vector == "NA(b)")
    # Count the number of not applicable values in vector
    count_not_applicable_adl <- sum(all_adl_vector == "NA(a)")
    # Count the number of ADLs that require help (value of 2)
    count_adl <- sum(all_adl_vector == 2)
    
    
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