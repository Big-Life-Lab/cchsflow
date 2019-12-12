#' @title Number of drinks consumed in the past week
#'
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a continuous variable derived by Statistics Canada that quantifies 
#'  the amount of alcohol that is consumed in a week. This is calculated by 
#'  adding the number of drinks consumed during each day in the past week. 
#'  Respondents of each CCHS cycle are asked how much alcohol they have
#'  consumed each day in the past week (ie. how much alcohol did you consume on 
#'  Sunday, how much did you consume on Monday etc.). Each day is considered an 
#'  individual variable and ALWDWKY takes the sum of all daily variables.
#'
#' @details This variable is used in all CCHS cycles in cchsflow and how it was
#'  derived remains consistent from 2001 to 2014.
#'
#' @param ALWDWKY cchsflow variable name for number of drinks consumed in the 
#'  past week
#'
#' @export
ALWDWKY <- function(ALWDWKY) {
  # this is for documentation purposes only
}

#' @title Average daily alcohol consumption
#'
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a continuous variable derived by Statistics Canada that quantifies 
#'  the mean daily consumption of alcohol. This takes the value of ALWDWKY and 
#'  divides it by 7.
#'
#' @details This variable is used in all CCHS cycles in cchsflow and how it was 
#'  derived remains consistent from 2001 to 2014.
#'
#' @param ALWDDLY cchsflow variable name for average daily alcohol consumption
#'
#' @export
ALWDDLY <- function(ALWDDLY) {
  # this is for documentation purposes only
}

#' @title Type of drinker
#'
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a categorical variable derived by Statistics Canada that uses 
#'  various intermediate alcohol variables to categorize individuals into 4 
#'  distinct groups:
#'
#'  \enumerate {
#'   \item Regular Drinker
#'   \item Occasional Drinker
#'   \item Former Drinker
#'   \item Never Drinker
#'  }
#'
#' @details This variable is used in CCHS cycles from 2001 to 2007. How it was 
#'  derived remained consistent during these years.
#'
#'  Starting in 2007, Statistics Canada created a derived variable that looked 
#'  at drinking type in the last 12 months. This new derived variable did not 
#'  distinguish between former and never drinkers. If your research requires you
#'  to differentiate between former and never drinkers, we recommend using 
#'  earlier cycles of the CCHS.
#'
#' @param ALCDTYP cchsflow variable name for type of drinker
#'
#' @export
ALCDTYP <- function(ALCDTYP) {
  # this is for documentation purposes only
}

#' @title Type of drinker (12 months)
#'
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a categorical variable derived by Statistics Canada that uses 
#'  various intermediate alcohol variables to categorize individuals into 3 
#'  distinct groups:
#'
#'  \enumerate{
#'   \item Regular Drinker
#'   \item Occasional Drinker
#'   \item No drink in the last 12 months.
#'   }
#'
#' @details This variable was introuced in the 2007-2008 cycle of the CCHS, and 
#'  became the sole derived variable that categorized people into various 
#'  drinker types from 2009 onwards. Unlike ALCDTYP, this variable does not 
#'  distinguish between former and never drinkers.
#'
#' @param ALCDTTM cchsflow variable name for type of drinker (12 months)
#'
#' @export
ALCDTTM <- function(ALCDTTM) {
  # this is for documentation purposes only
}
