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
#' # and specify ADL_dr, along with the various ADL variables.
#' # Then by using bind_rows() you can combine ADL_der across cycles.
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
#' combined_adl <- bind_rows(adl2001, adl2009_2010)
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
  if_else2(ADL_01 == 1 | ADL_02 == 1 | ADL_03 == 1 | ADL_04 == 1 |
             ADL_05 == 1, 1,
           if_else2(ADL_01 == 2 & ADL_02 == 2 & ADL_03 == 2 & ADL_04 == 2 &
                      ADL_05 == 2, 2,
                    if_else2(is.na(ADL_01)| is.na(ADL_02)| is.na(ADL_03)|
                               is.na(ADL_04)| is.na(ADL_05), return(NA),
                             return(NA))))
}