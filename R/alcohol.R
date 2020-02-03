#' @title Number of drinks consumed in the past week
#'
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a continuous variable derived by Statistics Canada that quantifies
#'  the amount of alcohol that is consumed in a week. This is calculated by
#'  adding the number of drinks consumed each day in the past week.
#'  Respondents of each CCHS cycle are asked how much alcohol they have
#'  consumed each day in the past week (ie. how much alcohol did you consume on
#'  Sunday, how much did you consume on Monday etc.). Each day is considered an
#'  individual variable and ALWDWKY takes the sum of all daily variables.
#'
#' @details This variable is present in every CCHS cycle used in cchsflow, and
#'  how it was derived remains consistent.
#'
#' @param ALWDWKY cchsflow variable name for number of drinks consumed in the
#'  past week
#'
#' @examples
#' library(cchsflow)
#'  ?ALWDWKY
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
#' @details This variable is present in every CCHS cycle used in cchsflow, and
#'  how it was derived remains consistent.
#'
#' @param ALWDDLY cchsflow variable name for average daily alcohol consumption
#'
#' @examples
#' library(cchsflow)
#'  ?ALWDDLY
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
#'  \enumerate{
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
#' @examples
#' library(cchsflow)
#'  ?ALCDTYP
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
#' @details This variable was introduced in the 2007-2008 cycle of the CCHS, and
#'  became the sole derived variable that categorized people into various
#'  drinker types from 2009 onwards. Unlike ALCDTYP, this variable does not
#'  distinguish between former and never drinkers.
#'
#' @param ALCDTTM cchsflow variable name for type of drinker (12 months)
#'
#' @examples
#' library(cchsflow)
#'  ?ALCDTTM
#'
#' @export
ALCDTTM <- function(ALCDTTM) {
  # this is for documentation purposes only
}

#' @title Binge drinking
#' 
#' @description This function creates a derived categorical variable that
#'  flags for binge drinking based on the number drinks consumed on a single
#'  day.
#'
#' @details In health research, binge drinking is defined as having an excess
#'  amount of alcohol in a single day. For males, this is defined as having five
#'  or more drinks; and for females it is four or more drinks. In the CCHS,
#'  respondents are asked to count the number of drinks they had during each
#'  day of the last week.
#' 
#' @param DHH_SEX sex of respondent (1 - male, 2 - female)
#' 
#' @param ALW_2A1 Number of drinks on Sunday
#' 
#' @param ALW_2A2 Number of drinks on Monday
#' 
#' @param ALW_2A3 Number of drinks on Tuesday
#' 
#' @param ALW_2A4 Number of drinks on Wednesday
#' 
#' @param ALW_2A5 Number of drinks on Thursday
#' 
#' @param ALW_2A6 Number of drinks on Friday
#' 
#' @param ALW_2A7 Number of drinks on Saturday
#' 
#' @return Categorical variable (binge_drinker) with two categories:
#' 
#'  \enumerate{
#'   \item 1 - binge drinker
#'   \item 2 - non-binge drinker
#'  }
#' 
#' @example 
#' # Using binge_drinker_fun() to create binge_drinker values across CCHS cycles
#' # binge_drinker_fun() is specified in variable_details.csv along with the
#' # CCHS variables and cycles included.
#' 
#' # To transform binge_drinker, use rec_with_table() for each CCHS cycle
#' # and specify binge_drinker, along with the various alcohol and sex
#' # variables. Then by using bind_rows() you can combine binge_drinker
#' # across cycles.
#' 
#' library(cchsflow)
#' binge2001 <- rec_with_table(
#'   cchs2001, c(
#'     "DHH_SEX", "ALW_2A1", "ALW_2A2", "ALW_2A3", "ALW_2A4", "ALW_2A5",
#'     "ALW_2A6", "ALW_2A7", "binge_drinker"
#'   )
#' )
#' 
#' head(binge2001)
#' 
#' binge2010 <- rec_with_table(
#'   cchs2010, c(
#'     "DHH_SEX", "ALW_2A1", "ALW_2A2", "ALW_2A3", "ALW_2A4", "ALW_2A5",
#'     "ALW_2A6", "ALW_2A7", "binge_drinker"
#'   )
#' )
#' 
#' tail(binge2010)
#' 
#' combined_binge <- bind_rows(binge2001, binge2010)
#' 
#' head(combined_binge)
#' 
#' tail(combined_binge)
#' 
#' # Using binge_drinker_fun() to generate binge_drinker with user-inputted
#' # values.
#' #
#' # Let's say you are a male, and you had 3 drinks on Sunday, 1 drink on
#' # Monday, 6 drinks on Tuesday, 0 drinks on Wednesday, 3 drinks on Thurday,
#' # 8 drinks on Friday, and 2 drinks on Saturday. Using binge_drinker_fun(),
#' # we can check if you would be classified as a drinker.
#' 
#' binge <- binge_drinker_fun(DHH_SEX = 1, ALW_2A1 = 3, ALW_2A2 = 1,
#'                           ALW_2A3 = 6, ALW_2A4 = 0, ALW_2A5 = 3,
#'                           ALW_2A6 = 8, ALW_2A7 = 2)
#' 
#' print(binge)
#' @export

binge_drinker_fun <-
  function(DHH_SEX, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6,
           ALW_2A7) {
    # Males with at least one day with 5 or more drinks
    if_else2((DHH_SEX == 1 & (ALW_2A1 >= 5 | ALW_2A2 >= 5 | ALW_2A3 >=5 |
                              ALW_2A4 >= 5 | ALW_2A5 >= 5 | ALW_2A6 >= 5 |
                              ALW_2A7 >= 5)), 1,
    # Males with no days with 5 or more drinks
    if_else2((DHH_SEX == 1 & (ALW_2A1 %in% (0:4) & ALW_2A2 %in% (0:4) &
                              ALW_2A3 %in% (0:4) & ALW_2A4 %in% (0:4) &
                              ALW_2A5 %in% (0:4) & ALW_2A6 %in% (0:4) &
                              ALW_2A7 %in% (0:4))), 2,
    # Females with at least one day with 4 or more drinks
    if_else2((DHH_SEX == 2 & (ALW_2A1 >= 4 | ALW_2A2 >= 4 | ALW_2A3 >= 4 |
                               ALW_2A4 >= 4 | ALW_2A5 >= 4 | ALW_2A6 >= 4 |
                               ALW_2A7 >= 4)), 1,
    # Females with no days with 4 or more drinks
    if_else2((DHH_SEX == 2 & (ALW_2A1 %in% (0:3) & ALW_2A2 %in% (0:3) &
                               ALW_2A3 %in% (0:3) & ALW_2A4 %in% (0:3) &
                               ALW_2A5 %in% (0:3) & ALW_2A6 %in% (0:3) &
                               ALW_2A7 %in% (0:3))), 2, NA))))
  }