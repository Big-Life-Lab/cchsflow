#' @title food_insecurity_der
#' 
#' @description \strong{NOTE:} this is not a function.
#'  
#'  This is a derivied variable that uses the different food insecurity
#'  variables from all CCHS cycles to generate food_insecurity_der that is
#'  harmonized across all cycles. food_insecurity_der is a categorical variable
#'  with two categories:
#'  
#'  \enumerate{
#'   \item 1 - no food insecurity in the last 12 months 
#'   \item 2 - food insecurity in the last 12 months
#'  }
#'  
#' @details Food insecurity is measured differently across CCHS cycles. In 2001
#'  and 2003, FINF1 is used; in 2005, FSCDHFS is used; and in 2007
#'  to 2014, FSCDHFS2 is used. Each variable examines food insecurity in the
#'  household over the past 12 months, but use different base variables to
#'  derive food insecurity.  
#'  
#'  If you are using cchsflow for CCHS survey years that use consistent food
#'  insecurity variables, it is appropriate to use FINF1, FSCDHFS, or FSCDHFS2
#'  that are available on cchsflow. For multiple CCHS survey years that do not
#'  use the same food insecurity variables, food_insecurity_der is recommended.
#' 
#' @param FINF1 CCHS variable used in 2001 and 2003 indicating food insecurity
#'  in the past 12 months
#' 
#' @param FSCDHFS CCHS variable used in 2005 measuring food insecurity & hunger
#'  in the last 12 months
#' 
#' @param FSCDHFS2 CCHS variable used in 2007-2014 measuring household food
#'  insecurity in the last 12 months
#' 
#'
#' @examples 
#' 
#' library(cchsflow)
#' ?food_insecurity_der
#' 
#' @export
food_insecurity_der <- 
  function(FINF1, FSCDHFS, FSCDHFS2) {
    #this is for documentation purposes only
  }
