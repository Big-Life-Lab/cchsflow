#' @title Derived Depression Scale - Short Form Score
#' 
#' @description \strong{NOTE:} this is not a function.
#' 
#'  This is a continuous variable derived by Statistics Canada that assesses the
#'  level of depression of respondents who have identified that they have felt
#'  depressed or loss of interest within the last two weeks. This variable is
#'  scaled from 0 to 8, with 0 indicating a respondent has not felt depressed or
#'  loss of interest, and 8 representing the highest level of depression.
#' 
#' @details The derivation of this variable is based on the work of Kessler &
#'  Mroczek from the University of Michigan. For more details on the items used
#'  and how the variable was derived click \href{https://osf.io/auqwh/}{here}.
#'  
#'  This variable is present in every CCHS cycle used in cchsflow, and
#'  how it was derived remains consistent.
#'
#' @param DPSDSF cchsflow variable name for derived depression scale.
#' 
#' @seealso \code{\link{DPSDPP}}
#' 
#' @examples
#' library(cchsflow)
#'  ?DPSDSF
#'
#' @export
DPSDSF <- function(DPSDSF) {
  # this is for documentation purposes only
}

#' @title Depression Scale - Predicted Probability
#' 
#' @description \strong{NOTE:} this is not a function.
#' 
#'  This is categorical variable derived by Statistics Canada that predicts
#'  the probability that a respondent would be diagnosed as having a major
#'  depressive episode if a diagnostic interview was completed. This variable
#'  is derived from \code{\link{DPSDSF}} in which probabilities are assigned
#'  to respondents based on their depression scale score. For more details on
#'  how the variable was derived click \href{https://osf.io/auqwh/}{here}.
#' 
#' @details While this variable was considered to be categorical in CCHS
#'  documentation, the values range from 0 to 0.90 with no distinct names or
#'  metadata for each category. As such, this variable was specified as a
#'  continuous variable in cchsflow. This has no bearing on the final output 
#'  of the variable as there are no recode changes. This means that a respondent
#'  who was coded with a probability of 0.50 will still have a probability value
#'  of 0.50 when the variable goes through harmonization.
#' 
#'  This variable is present in every CCHS cycle used in cchsflow, and
#'  how it was derived remains consistent.
#'
#' @param DPSDPP cchsflow variable name for derived depression scale predicted
#'  probability.
#' 
#' @seealso \code{\link{DPSDSF}}
#' 
#' @examples
#' library(cchsflow)
#'  ?DPSDPP
#'
#' @export
DPSDPP <- function(DPSDPP) {
  # this is for documentation purposes only
}