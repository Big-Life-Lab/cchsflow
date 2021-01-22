#' @title Occupation Group (9 categories)
#' 
#' @description \strong{NOTE:} this is not a function.
#' 
#'  This is a 9 category variable (LBFA_31A) that is in the CCHS that asks
#'  which occupation group best describes a respondent. Occupation group is
#'  asked in the 2001 CCHS cycle and in CCHS cycles from 2007-2014.
#'  
#' @details While occupation group is asked in many survey cycles, the 2001
#'  CCHS survey cycle is the only survey that has 9 categories. The categories
#'  are as follows:
#'  \enumerate{
#'  \item Management
#'  \item Professional (including accountants)
#'  \item Technologist, Technician or Tech Occupation
#'  \item Administrative, Financial or Clerical
#'  \item Sales or Service
#'  \item Trades, Transport or Equipment Operator
#'  \item Farming, Forestry, Fishing, Mining
#'  \item Processing, Manufacturing, Utilities
#'  \item Other
#'  }
#'  To harmonize the 2001 CCHS cycle with other survey cycles,
#'  \code{\link{LBFA_31A_a}} and \code{\link{LBFA_31A_b}} were created in which
#'  categories in the 2001 survey cycle were collapsed.
#' 
#' @param LBFA_31A cchsflow variable name for Occupation Group (9 categories)
#' 
#' @seealso \code{\link{LBFA_31A_a}}, \code{\link{LBFA_31A_b}}
#' 
#' @examples 
#' library(cchsflow)
#'  ?LBFA_31A
#' 
#' @export
LBFA_31A <- function(LBFA_31A){
  # this is for documentation purposes only
}

#' @title Occupation Group (5 categories)
#' 
#' @description \strong{NOTE:} this is not a function.
#' 
#'  This is a 5 category variable (LBFA_31A_a) that is in the CCHS that asks
#'  which occupation group best describes a respondent. Occupation group is
#'  asked in the 2001 CCHS cycle and in CCHS cycles from 2007-2014.
#'  
#' @details In the 2007-2014 CCHS survey cycles, occupation group has 5
#'  categories. The categories are as follows:
#'  \enumerate{
#'  \item Management, Health, Education, Art, Culture
#'  \item Business, Finance, Admin
#'  \item Sales or Service
#'  \item Trades, Transport or Equipment Operator
#'  \item Unique to Primary Industry/Processing/Manufacturing
#'  }
#'  
#'  In this variable, categories from the 2001 CCHS survey cycle were collapsed
#'  to harmonize with the other survey cycles. "Management, Professional
#'  (including accountants), Technologist, Technician or Tech Occupation" were
#'  combined into one category "Management, Health, Education, Art, Culture".
#'  "Farming, Forestry, Fishing, Mining" and "Processing, Manufacturing,
#'  Utilities", were combined into one category "Farming, Forestry, Fishing,
#'  Mining, Processing, Manufacturing, Utilities".  
#'  
#'  The "other" category in the 2001 CCHS survey cycle was assigned to missing
#'  (NA(b)). This is consistent with 
#'  \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6423929/}{other studies}
#'  that group the "other" category as "missing". \code{\link{LBFA_31A_b}}
#'  is a 6 category variable that keeps the "other" category in the 2001 survey
#'  cycle as "other".
#'  
#' @param LBFA_31A_a cchsflow variable name for Occupation Group (5 categories)
#'   
#' @seealso \code{\link{LBFA_31A}}, \code{\link{LBFA_31A_b}}
#' 
#' @examples 
#' library(cchsflow)
#' ?LBFA_31A_a  
#'
#' @export
LBFA_31A_a <- function(LBFA_31A_a){
  # this is for documentation purposes only
}

#' @title Occupation Group (6 categories)
#' 
#' @description \strong{NOTE:} this is not a function.
#' 
#'  This is a 6 category variable (LBFA_31A_b) that is in the CCHS that asks
#'  which occupation group best describes a respondent. Occupation group is
#'  asked in the 2001 CCHS cycle and in CCHS cycles from 2007-2014.
#'  
#' @details In the 2007-2014 CCHS survey cycles, occupation group has 5
#'  categories. This variable, however, includes a sixth category to account for
#'  the "other" category asked in the 2001 CCHS survey cycle. The categories are
#'  as follows:
#'  \enumerate{
#'  \item Management, Health, Education, Art, Culture
#'  \item Business, Finance, Admin
#'  \item Sales or Service
#'  \item Trades, Transport or Equipment Operator
#'  \item Unique to Primary Industry/Processing/Manufacturing
#'  \item Other
#'  }
#'  
#'  In this variable, categories from the 2001 CCHS survey cycle were collapsed
#'  to harmonize with the other survey cycles. "Management, Professional
#'  (including accountants), Technologist, Technician or Tech Occupation" were
#'  combined into one category "Management, Health, Education, Art, Culture".
#'  "Farming, Forestry, Fishing, Mining" and "Processing, Manufacturing,
#'  Utilities", were combined into one category "Farming, Forestry, Fishing,
#'  Mining, Processing, Manufacturing, Utilities".  
#'  
#' @param LBFA_31A_b cchsflow variable name for Occupation Group (6 categories)
#'   
#' @seealso \code{\link{LBFA_31A}}, \code{\link{LBFA_31A_a}}
#' 
#' @examples 
#' library(cchsflow)
#' ?LBFA_31A_b  
#'
#' @export
LBFA_31A_b <- function(LBFA_31A_b){
  # this is for documentation purposes only
}