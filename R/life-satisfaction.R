#' @title Satisfaction with life (GEN_02A/GEN_02A2)
#' 
#' @description \strong{NOTE:} this is not a function.
#' 
#'  These are two variables asked in the CCHS that asks respondents to rate
#'  their satisfaction with their lives. The variable GEN_02A is a categorical
#'  variable with 5 categories:
#'  \enumerate{
#'   \item Very satisfied
#'   \item Satisfied
#'   \item Neither satisfied nor unsatisfied
#'   \item Dissatisfied
#'   \item Very dissatisfied
#'  }
#'  
#'  The GEN_02A2 is a continuous variable from 0 to 10, where 0 represents very
#'  dissatisfied and 10 represents very satisfied.
#'  
#' @details GEN_02A was asked to respondents in the 2003, 2005, and 2007-2008
#'  CCHS survey cycles; while GEN_02A2 was asked to respondents in CCHS survey
#'  cycles from 2009 to 2014. To harmonize GEN_02A2 across more cycles,
#'  GEN_02A2 was derived for earlier cycles by converting GEN_02A values to
#'  match the scale used in GEN_02A2. The very satisfied category was converted
#'  to a score of 10; the satisfied category was converted to a score of 7;
#'  the neither satisfied nor unsatisfied category was converted to a score
#'  of 5; the dissatisfied category was converted to a score of 2; and the very
#'  dissatisfied category was converted to a score of 0.
#'  
#'  When using earlier CCHS cycles (2003-2007), it is appropriate to use 
#'  GEN_02A. When using multiple CCHS cycles that include cycles from 2009-2014,
#'  GEN_02A2 is recommended.
#'  
#' @param GEN_02A - categorical life satisfaction variable asked from 2003-2007
#' 
#' @param GEN_02A2 - continuous life satisfaction variable asked from 2009-2014,
#'  and derived for 2003-2007
#' 
#' @examples
#' 
#' library(cchsflow)
#' ?GEN_02A2
#' 
#' @export
GEN_02A2 <- function(GEN_02A, GEN_02A2) {
  #this is for documentation purposes only
}