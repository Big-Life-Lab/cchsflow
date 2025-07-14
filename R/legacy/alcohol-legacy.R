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
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced for expanded database coverage
#' @export
ALCDTTM <- function(ALCDTTM) {
  # this is for documentation purposes only
}

#' @title Any alcohol past week
#' 
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a categorical variable derived by Statistics Canada that determines 
#'  if alcohol was consumed in the past week. The variable is optional in 
#'  selected provinces and territories.
#' 
#' @details This variable is present in every CCHS cycle used in cchsflow. In 
#' 2007 and 2008, the variable is optional for Newfoundland and Labrador, Nova 
#' Scotia, Ontario, British Columbia and Nunavut.In 2009 and 2010, the variable 
#' is optional for Newfoundland and Labrador, Ontario, and Saskatchewan. In 
#' 2011, the variable is optional for Newfoundland and Labrador, Quebec, 
#' Ontario, Manitoba, and Saskatchewan. In 2012, the variable is optional for 
#' Newfoundland and Labrador, Quebec, Ontario, Manitoba, Nunavut, and 
#' Saskatchewan.In 2013, the variable is optional for Quebec, Ontario, Prince 
#' Edward Island, Manitoba, Yukon, and Saskatchewan. In 2014, the variable is 
#' optional for Nunavut, Quebec, Ontario, Prince Edward Island, Manitoba, 
#' Newfoundland and Labrador, Saskatchewan, and British Columbia.
#'
#' @param ALW_1 cchsflow variable name for any alcohol past week
#'
#' @examples
#' library(cchsflow)
#'  ?ALW_1
#'
#' @export
ALW_1 <- function(ALW_1) {
  # this is for documentation purposes only
} 

#' @title Number of drinks - Sunday
#' 
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a continuous variable derived by Statistics Canada that quantifies 
#'  the number of alcoholic drinks consumed on Sunday. The variable is 
#'  optional in selected provinces and territories.
#' 
#' @details This variable is present in every CCHS cycle used in cchsflow. In 
#' 2007 and 2008, the variable is optional for Newfoundland and Labrador, Nova 
#' Scotia, Ontario, British Columbia and Nunavut.In 2009 and 2010, the variable 
#' is optional for Newfoundland and Labrador, Ontario, and Saskatchewan. In 
#' 2011, the variable is optional for Newfoundland and Labrador, Quebec, 
#' Ontario, Manitoba, and Saskatchewan. In 2012, the variable is optional for 
#' Newfoundland and Labrador, Quebec, Ontario, Manitoba, Nunavut, and 
#' Saskatchewan.In 2013, the variable is optional for Quebec, Ontario, Prince 
#' Edward Island, Manitoba, Yukon, and Saskatchewan. In 2014, the variable is 
#' optional for Nunavut, Quebec, Ontario, Prince Edward Island, Manitoba, 
#' Newfoundland and Labrador, Saskatchewan, and British Columbia.
#'
#' @param ALW_2A1 cchsflow variable name for number of drinks on Sunday
#'
#' @examples
#' library(cchsflow)
#'  ?ALW_2A1
#'
#' @export
ALW_2A1 <- function(ALW_2A1) {
  # this is for documentation purposes only
}  

#' @title Number of drinks - Monday
#' 
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a continuous variable derived by Statistics Canada that quantifies 
#'  the number of alcoholic drinks consumed on Monday. The variable is 
#'  optional in selected provinces and territories.
#' 
#' @details This variable is present in every CCHS cycle used in cchsflow. In 
#' 2007 and 2008, the variable is optional for Newfoundland and Labrador, Nova 
#' Scotia, Ontario, British Columbia and Nunavut.In 2009 and 2010, the variable 
#' is optional for Newfoundland and Labrador, Ontario, and Saskatchewan. In 
#' 2011, the variable is optional for Newfoundland and Labrador, Quebec, 
#' Ontario, Manitoba, and Saskatchewan. In 2012, the variable is optional for 
#' Newfoundland and Labrador, Quebec, Ontario, Manitoba, Nunavut, and 
#' Saskatchewan.In 2013, the variable is optional for Quebec, Ontario, Prince 
#' Edward Island, Manitoba, Yukon, and Saskatchewan. In 2014, the variable is 
#' optional for Nunavut, Quebec, Ontario, Prince Edward Island, Manitoba, 
#' Newfoundland and Labrador, Saskatchewan, and British Columbia.
#'
#' @param ALW_2A2 cchsflow variable name for number of drinks on Monday
#'
#' @examples
#' library(cchsflow)
#'  ?ALW_2A2
#'
#' @export
ALW_2A2 <- function(ALW_2A2) {
  # this is for documentation purposes only
} 

#' @title Number of drinks - Tuesday
#' 
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a continuous variable derived by Statistics Canada that quantifies 
#'  the number of alcoholic drinks consumed on Tuesday. The variable is 
#'  optional in selected provinces and territories.
#' 
#' @details This variable is present in every CCHS cycle used in cchsflow. In 
#' 2007 and 2008, the variable is optional for Newfoundland and Labrador, Nova 
#' Scotia, Ontario, British Columbia and Nunavut.In 2009 and 2010, the variable 
#' is optional for Newfoundland and Labrador, Ontario, and Saskatchewan. In 
#' 2011, the variable is optional for Newfoundland and Labrador, Quebec, 
#' Ontario, Manitoba, and Saskatchewan. In 2012, the variable is optional for 
#' Newfoundland and Labrador, Quebec, Ontario, Manitoba, Nunavut, and 
#' Saskatchewan.In 2013, the variable is optional for Quebec, Ontario, Prince 
#' Edward Island, Manitoba, Yukon, and Saskatchewan. In 2014, the variable is 
#' optional for Nunavut, Quebec, Ontario, Prince Edward Island, Manitoba, 
#' Newfoundland and Labrador, Saskatchewan, and British Columbia.
#'
#' @param ALW_2A3 cchsflow variable name for number of drinks on Tuesday
#'
#' @examples
#' library(cchsflow)
#'  ?ALW_2A3
#'
#' @export
ALW_2A3 <- function(ALW_2A3) {
  # this is for documentation purposes only
} 

#' @title Number of drinks - Wednesday
#' 
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a continuous variable derived by Statistics Canada that quantifies 
#'  the number of alcoholic drinks consumed on Wednesday. The variable is 
#'  optional in selected provinces and territories.
#' 
#' @details This variable is present in every CCHS cycle used in cchsflow. In 
#' 2007 and 2008, the variable is optional for Newfoundland and Labrador, Nova 
#' Scotia, Ontario, British Columbia and Nunavut.In 2009 and 2010, the variable 
#' is optional for Newfoundland and Labrador, Ontario, and Saskatchewan. In 
#' 2011, the variable is optional for Newfoundland and Labrador, Quebec, 
#' Ontario, Manitoba, and Saskatchewan. In 2012, the variable is optional for 
#' Newfoundland and Labrador, Quebec, Ontario, Manitoba, Nunavut, and 
#' Saskatchewan.In 2013, the variable is optional for Quebec, Ontario, Prince 
#' Edward Island, Manitoba, Yukon, and Saskatchewan. In 2014, the variable is 
#' optional for Nunavut, Quebec, Ontario, Prince Edward Island, Manitoba, 
#' Newfoundland and Labrador, Saskatchewan, and British Columbia.
#'
#' @param ALW_2A4 cchsflow variable name for number of drinks on Wednesday
#'
#' @examples
#' library(cchsflow)
#'  ?ALW_2A4
#'
#' @export
ALW_2A4 <- function(ALW_2A4) {
  # this is for documentation purposes only
} 

#' @title Number of drinks - Thursday
#' 
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a continuous variable derived by Statistics Canada that quantifies 
#'  the number of alcoholic drinks consumed on Thursday. The variable is 
#'  optional in selected provinces and territories.
#' 
#' @details This variable is present in every CCHS cycle used in cchsflow. In 
#' 2007 and 2008, the variable is optional for Newfoundland and Labrador, Nova 
#' Scotia, Ontario, British Columbia and Nunavut.In 2009 and 2010, the variable 
#' is optional for Newfoundland and Labrador, Ontario, and Saskatchewan. In 
#' 2011, the variable is optional for Newfoundland and Labrador, Quebec, 
#' Ontario, Manitoba, and Saskatchewan. In 2012, the variable is optional for 
#' Newfoundland and Labrador, Quebec, Ontario, Manitoba, Nunavut, and 
#' Saskatchewan.In 2013, the variable is optional for Quebec, Ontario, Prince 
#' Edward Island, Manitoba, Yukon, and Saskatchewan. In 2014, the variable is 
#' optional for Nunavut, Quebec, Ontario, Prince Edward Island, Manitoba, 
#' Newfoundland and Labrador, Saskatchewan, and British Columbia.
#'
#' @param ALW_2A5 cchsflow variable name for number of drinks on Thursday
#'
#' @examples
#' library(cchsflow)
#'  ?ALW_2A5
#'
#' @export
ALW_2A5 <- function(ALW_2A5) {
  # this is for documentation purposes only
} 

#' @title Number of drinks - Friday
#' 
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a continuous variable derived by Statistics Canada that quantifies 
#'  the number of alcoholic drinks consumed on Friday. The variable is 
#'  optional in selected provinces and territories.
#' 
#' @details This variable is present in every CCHS cycle used in cchsflow. In 
#' 2007 and 2008, the variable is optional for Newfoundland and Labrador, Nova 
#' Scotia, Ontario, British Columbia and Nunavut.In 2009 and 2010, the variable 
#' is optional for Newfoundland and Labrador, Ontario, and Saskatchewan. In 
#' 2011, the variable is optional for Newfoundland and Labrador, Quebec, 
#' Ontario, Manitoba, and Saskatchewan. In 2012, the variable is optional for 
#' Newfoundland and Labrador, Quebec, Ontario, Manitoba, Nunavut, and 
#' Saskatchewan.In 2013, the variable is optional for Quebec, Ontario, Prince 
#' Edward Island, Manitoba, Yukon, and Saskatchewan. In 2014, the variable is 
#' optional for Nunavut, Quebec, Ontario, Prince Edward Island, Manitoba, 
#' Newfoundland and Labrador, Saskatchewan, and British Columbia.
#'
#' @param ALW_2A6 cchsflow variable name for number of drinks on Friday
#'
#' @examples
#' library(cchsflow)
#'  ?ALW_2A6
#'
#' @export
ALW_2A6 <- function(ALW_2A6) {
  # this is for documentation purposes only
}

#' @title Number of drinks - Saturday
#' 
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a continuous variable derived by Statistics Canada that quantifies 
#'  the number of alcoholic drinks consumed on Saturday. The variable is 
#'  optional in selected provinces and territories.
#' 
#' @details This variable is present in every CCHS cycle used in cchsflow. In 
#' 2007 and 2008, the variable is optional for Newfoundland and Labrador, Nova 
#' Scotia, Ontario, British Columbia and Nunavut.In 2009 and 2010, the variable 
#' is optional for Newfoundland and Labrador, Ontario, and Saskatchewan. In 
#' 2011, the variable is optional for Newfoundland and Labrador, Quebec, 
#' Ontario, Manitoba, and Saskatchewan. In 2012, the variable is optional for 
#' Newfoundland and Labrador, Quebec, Ontario, Manitoba, Nunavut, and 
#' Saskatchewan.In 2013, the variable is optional for Quebec, Ontario, Prince 
#' Edward Island, Manitoba, Yukon, and Saskatchewan. In 2014, the variable is 
#' optional for Nunavut, Quebec, Ontario, Prince Edward Island, Manitoba, 
#' Newfoundland and Labrador, Saskatchewan, and British Columbia.
#'
#' @param ALW_2A7 cchsflow variable name for number of drinks on Saturday
#'
#' @examples
#' library(cchsflow)
#'  ?ALW_2A7
#'
#' @export
ALW_2A7 <- function(ALW_2A7) {
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
#' @param ALW_1 Drinks in the last week (1 - yes, 2 - no)
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
#' @examples
#'  
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
#'   cchs2001_p, c(
#'     "ALW_1", "DHH_SEX", "ALW_2A1", "ALW_2A2", "ALW_2A3", "ALW_2A4",
#'     "ALW_2A5", "ALW_2A6", "ALW_2A7", "binge_drinker"
#'   )
#' )
#' 
#' head(binge2001)
#' 
#' binge2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "ALW_1", "DHH_SEX", "ALW_2A1", "ALW_2A2", "ALW_2A3", "ALW_2A4",
#'     "ALW_2A5", "ALW_2A6", "ALW_2A7", "binge_drinker"
#'   )
#' )
#' 
#' tail(binge2009_2010)
#' 
#' combined_binge <- bind_rows(binge2001, binge2009_2010)
#' 
#' head(combined_binge)
#' 
#' tail(combined_binge)
#' 
#' # Using binge_drinker_fun() to generate binge_drinker with user-inputted
#' # values.
#' #
#' # Let's say you are a male, and you had drinks in the last week. Let's say
#' # you had 3 drinks on Sunday, 1 drink on
#' # Monday, 6 drinks on Tuesday, 0 drinks on Wednesday, 3 drinks on Thurday,
#' # 8 drinks on Friday, and 2 drinks on Saturday. Using binge_drinker_fun(),
#' # we can check if you would be classified as a drinker.
#' 
#' binge <- binge_drinker_fun(DHH_SEX = 1, ALW_1 = 1, ALW_2A1 = 3, ALW_2A2 = 1,
#'                           ALW_2A3 = 6, ALW_2A4 = 0, ALW_2A5 = 3,
#'                           ALW_2A6 = 8, ALW_2A7 = 2)
#' 
#' print(binge)
#' 
#' library(cchsflow)
#' DHH_SEX <- c(1, 2) 
#' ALW_1 <- c(1, 1)
#' ALW_2A1 <- c(3, 1)
#' ALW_2A2 <- c(1, 5)
#' ALW_2A3 <- c(6, 2)
#' ALW_2A4 <- c(0, 4)
#' ALW_2A5 <- c(3, 0)
#' ALW_2A6 <- c(8, 1)
#' ALW_2A7 <- c(2, 2)
#'
#' binge_drink_data <- data.frame(DHH_SEX, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4,
#'                                ALW_2A5, ALW_2A6, ALW_2A7)
#' binge_drink_data[,"Binge_Drinker"] <- NA
#'print(binge_drink_data)
#'
#' binge_drink_data$Binge_Drinker<-binge_drinker_fun(binge_drink_data[[1]], binge_drink_data[[2]], 
#'                                                  binge_drink_data[[3]], binge_drink_data[[4]], 
#'                                                  binge_drink_data[[5]], binge_drink_data[[6]],
#'                                                  binge_drink_data[[7]], binge_drink_data[[8]], 
#'                                                  binge_drink_data[[9]])
#'print(binge_drink_data)
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Enhanced validation and improved logic
#' @export
binge_drinker_fun <-
  function(DHH_SEX, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6,
           ALW_2A7) {
    # If respondents had alcohol in the last week
    if_else2(ALW_1 == 1,
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
                                ALW_2A7 %in% (0:3))), 2, "NA(b)")))),
      # Respondents who didn't indicate they had alcohol in the last week
      "NA(a)")
  }

#' @title Short term risks due to drinking
#' 
#' @description This function creates a categorical variable that
#'  flags for increased short term health risks due to their drinking habits,
#'  according to Canada's Low-Risk Alcohol Drinking Guideline.
#'
#' @details The classification of drinkers according to their short term health 
#' risks comes from guidelines in Alcohol and Health in Canada: A Summary of 
#' Evidence and Guidelines for Low-risk Drinking, and is based on the alcohol 
#' consumption reported over the past week. Short-term or acute risks include 
#' injury and overdose.
#' 
#' Categories are based on CCHS 2015-2016's variable (ALWDVSTR) where short 
#' term health risk are increased when drinking more than 3 drinks (for women) 
#' or 4 drinks (for men) on any single occasion.
#' 
#' See \url{https://osf.io/ykau5/} for more details on the guideline. 
#' See \url{https://osf.io/ycxaq/} for more details on derivation of the 
#' function on page 9.
#' 
#' @param DHH_SEX Sex of respondent (1 - male, 2 - female)
#' 
#' @param ALW_1 Drinks in the last week (1 - yes, 2 - no)
#' 
#' @param ALC_1 Drinks in the past year (1 - yes, 2 - no)
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
#' @param ALWDWKY Number of drinks consumed in the past week
#' 
#' @return Categorical variable (ALWDVSTR_der) with two categories:
#' 
#'  \itemize{
#'   \item 1 - Increased short term health risk
#'   \item 2 - No increased short term health risk
#'  }
#' 
#' @examples
#'  
#' # Using low_drink_short_fun() to create ALWDVSTR_der values across CCHS cycles
#' # low_drink_short_fun() is specified in variable_details.csv along with the
#' # CCHS variables and cycles included.
#' 
#' # To transform ALWDVSTR_der, use rec_with_table() for each CCHS cycle
#' # and specify ALWDVSTR_der, along with the various alcohol and sex
#' # variables. 
#' # Using merge_rec_data(), you can combine ALWDVSTR_der across cycles.
#' 
#' library(cchsflow)
#' short_low_drink2001 <- rec_with_table(
#'   cchs2001_p, c(
#'     "ALW_1", "DHH_SEX", "ALW_2A1", "ALW_2A2", "ALW_2A3", "ALW_2A4",
#'     "ALW_2A5", "ALW_2A6", "ALW_2A7", "ALWDWKY", "ALC_1","ALWDVSTR_der"
#'   )
#' )
#' 
#' head(short_low_drink2001)
#' 
#' short_low_drink2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "ALW_1", "DHH_SEX", "ALW_2A1", "ALW_2A2", "ALW_2A3", "ALW_2A4",
#'     "ALW_2A5", "ALW_2A6", "ALW_2A7", "ALWDWKY", "ALC_1","ALWDVSTR_der"
#'   )
#' )
#' 
#' tail(short_low_drink2009_2010)
#' 
#' combined_short_low_drink <- bind_rows(short_low_drink2001, 
#' short_low_drink2009_2010)
#' 
#' head(combined_short_low_drink)
#' 
#' tail(combined_short_low_drink)
#' 
#' # Using low_drink_short_fun() to generate ALWDVSTR_der with user-inputted
#' # values.
#' #
#' # Let's say you are a male, you had drinks in the last week and in the last 
#' # year. Let's say you had 5 drinks on Sunday, 1 drink on Monday, 6 drinks on 
#' # Tuesday, 4 drinks on Wednesday, 4 drinks on Thursday, 8 drinks on Friday, 
#' # and 2 drinks on Saturday with a total of 30 drinks in a week. 
#' # Using low_drink_short_fun(), we can check if you would be classified as
#' # having an increased short term health risk due to drinking.
#' 
#' short_term_drink <- low_drink_short_fun(DHH_SEX = 1, ALWDWKY = 30, ALC_1 = 1,
#'  ALW_1 = 1, ALW_2A1 = 5, ALW_2A2 = 1, ALW_2A3 = 6, ALW_2A4 = 4, ALW_2A5 = 4, 
#'  ALW_2A6 = 8, ALW_2A7 = 2)
#'
#' print(short_term_drink)
#' 
#' library(cchsflow)
#' DHH_SEX <- c(1, 2) 
#' ALWDWKY <- c(30, 15)
#' ALC_1 <- c(1, 1)
#' ALW_1 <- c(1, 1)
#' ALW_2A1 <- c(3, 1)
#' ALW_2A2 <- c(1, 5)
#' ALW_2A3 <- c(6, 2)
#' ALW_2A4 <- c(0, 4)
#' ALW_2A5 <- c(3, 0)
#' ALW_2A6 <- c(8, 1)
#' ALW_2A7 <- c(2, 2)
#'
#' short_term_risk_data <- data.frame(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, ALW_2A2, 
#'                                ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7)
#' short_term_risk_data[,"Short_Term_Risk"] <- NA
#' print(short_term_risk_data)
#'
#' short_term_risk_data$Short_Term_Risk<-low_drink_short_fun(short_term_risk_data[[1]], short_term_risk_data[[2]], 
#'                                                  short_term_risk_data[[3]], short_term_risk_data[[4]], 
#'                                                  short_term_risk_data[[5]], short_term_risk_data[[6]],
#'                                                  short_term_risk_data[7]], short_term_risk_data[[8]], 
#'                                                  short_term_risk_data[[9]], short_term_risk_data[[10]],
#'                                                  short_term_risk_data[[11]])
#'print(short_term_risk_data)
#' @export
#' 
low_drink_short_fun <-
  function(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4, 
           ALW_2A5, ALW_2A6, ALW_2A7){
    # Test if inputs are in valid range
    if_else2(DHH_SEX %in% (1:2) & ALWDWKY %in% (0:995) & ALC_1 %in% (1:2) &
               ALW_1 %in% (1:2) & ALW_2A1 %in% (0:995) & ALW_2A2 %in% (0:995) &
               ALW_2A3 %in% (0:995) & ALW_2A4 %in% (0:995) &
               ALW_2A5 %in% (0:995) & ALW_2A6 %in% (0:995) &
               ALW_2A7 %in% (0:995),
    # Increased short term risk from due to drinking (1)
    if_else2(DHH_SEX == 1 & (ALW_2A1 %in%(5:995) | ALW_2A2 %in%(5:995) | 
                             ALW_2A3 %in%(5:995) | ALW_2A4 %in%(5:995) | 
                             ALW_2A5 %in%(5:995) | ALW_2A6 %in%(5:995) |
                             ALW_2A7 %in%(5:995) | ALWDWKY %in%(16:995)), 1,
    if_else2(DHH_SEX == 2 & (ALW_2A1 %in%(4:995) | ALW_2A2 %in%(4:995) | 
                             ALW_2A3 %in%(4:995) | ALW_2A4 %in%(4:995) |
                             ALW_2A5 %in%(4:995) | ALW_2A6 %in%(4:995) |
                             ALW_2A7 %in%(4:995) | ALWDWKY %in%(11:995)), 1,
    # No increased short term health risks due to drinking (2)
    # Includes those who did not drink in past 7 days or past 12 months
    if_else2(ALC_1 == 2 |ALW_1 ==2, 2,
    if_else2(DHH_SEX == 1 & (ALW_2A1 %in% (0:4) & ALW_2A2 %in% (0:4) &
                             ALW_2A3 %in% (0:4) & ALW_2A4 %in% (0:4) &
                             ALW_2A5 %in% (0:4) & ALW_2A6 %in% (0:4) &
                             ALW_2A7 %in% (0:4)) & ALWDWKY %in% (0:15), 2,
    if_else2(DHH_SEX == 2 & (ALW_2A1 %in% (0:3) & ALW_2A2 %in% (0:3) &
                             ALW_2A3 %in% (0:3) & ALW_2A4 %in% (0:3) &
                             ALW_2A5 %in% (0:3) & ALW_2A6 %in% (0:3) &
                             ALW_2A7 %in% (0:3)) & ALWDWKY %in% (0:10), 2,
                                                          "NA(b)"))))),
    "NA(b)")
  }

#' @title Long term risks due to drinking
#' 
#' @description This function creates a categorical variable that
#'  flags for increased long term health risks due to their drinking habits,
#'  according to Canada's Low-Risk Alcohol Drinking Guideline.
#'
#' @details The classification of drinkers according to their long term health 
#' risks comes from guidelines in Alcohol and Health in Canada: A Summary of 
#' Evidence and Guidelines for Low-risk Drinking, and is based on the alcohol 
#' consumption reported over the past week. Short-term or acute risks include 
#' injury and overdose.
#' 
#' Categories are based on CCHS 2015-2016's variable (ALWDVLTR) where long 
#' term health risk are increased when drinking more than 10 drinks a week for 
#' women, with no more than 2 drinks a day most days, and more than 15 drinks a 
#' week for men, with no more than 3 drinks a day most days.
#' 
#' See \url{https://osf.io/ykau5/} for more details on the guideline. 
#' See \url{https://osf.io/ycxaq/} for more details on the derivation of the 
#' function on page 8.
#' 
#' @param DHH_SEX Sex of respondent (1 - male, 2 - female)
#' 
#' @param ALW_1 Drinks in the last week (1 - yes, 2 - no)
#' 
#' @param ALC_1 Drinks in the past year (1 - yes, 2 - no)
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
#' @param ALWDWKY Number of drinks consumed in the past week
#' 
#' @return Categorical variable (ALWDVLTR_der) with two categories:
#' 
#'  \itemize{
#'   \item 1 - Increased long term health risk
#'   \item 2 - No increased long term health risk
#'  }
#' 
#' @examples
#'  
#' # Using low_drink_long_fun() to create ALWDVLTR_der values across CCHS cycles
#' # low_drink_long_fun() is specified in variable_details.csv along with the
#' # CCHS variables and cycles included.
#' 
#' # To transform ALWDVLTR_der, use rec_with_table() for each CCHS cycle
#' # and specify ALWDVLTR_der, along with the various alcohol and sex
#' # variables. 
#' # Using merge_rec_data(), you can combine ALWDVLTR_der across cycles.
#' 
#' library(cchsflow)
#' long_low_drink2001 <- rec_with_table(
#'   cchs2001_p, c(
#'     "ALW_1", "DHH_SEX", "ALW_2A1", "ALW_2A2", "ALW_2A3", "ALW_2A4",
#'     "ALW_2A5", "ALW_2A6", "ALW_2A7", "ALWDWKY", "ALC_1","ALWDVLTR_der"
#'   )
#' )
#' 
#' head(long_low_drink2001)
#' 
#' long_low_drink2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "ALW_1", "DHH_SEX", "ALW_2A1", "ALW_2A2", "ALW_2A3", "ALW_2A4",
#'     "ALW_2A5", "ALW_2A6", "ALW_2A7", "ALWDWKY", "ALC_1","ALWDVLTR_der"
#'   )
#' )
#' 
#' tail(long_low_drink2009_2010)
#' 
#' combined_long_low_drink <- bind_rows(long_low_drink2001, 
#' long_low_drink2009_2010)
#' 
#' head(combined_long_low_drink)
#' 
#' tail(combined_long_low_drink)
#' 
#' # Using low_drink_long_fun() to generate ALWDVLTR_der with user-inputted
#' # values.
#' #
#' # Let's say you are a male, you had drinks in the last week and in the last 
#' # year. Let's say you had 5 drinks on Sunday, 1 drink on Monday, 6 drinks on 
#' # Tuesday, 4 drinks on Wednesday, 4 drinks on Thursday, 8 drinks on Friday, 
#' # and 2 drinks on Saturday with a total of 30 drinks in a week. 
#' # Using low_drink_long_fun(), we can check if you would be classified as
#' # having an increased long term health risk due to drinking.
#' 
#' long_term_drink <- low_drink_long_fun(DHH_SEX = 1, ALWDWKY = 30, ALC_1 = 1,
#'  ALW_1 = 1, ALW_2A1 = 5, ALW_2A2 = 1, ALW_2A3 = 6, ALW_2A4 = 4, ALW_2A5 = 4, 
#'  ALW_2A6 = 8, ALW_2A7 = 2)
#'
#' print(long_term_drink)
#' 
#' library(cchsflow)
#' DHH_SEX <- c(1, 2) 
#' ALWDWKY <- c(30, 15)
#' ALC_1 <- c(1, 1)
#' ALW_1 <- c(1, 1)
#' ALW_2A1 <- c(3, 1)
#' ALW_2A2 <- c(1, 5)
#' ALW_2A3 <- c(6, 2)
#' ALW_2A4 <- c(0, 4)
#' ALW_2A5 <- c(3, 0)
#' ALW_2A6 <- c(8, 1)
#' ALW_2A7 <- c(2, 2)
#'
#' long_term_risk_data <- data.frame(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, ALW_2A2, 
#'                                ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7)
#' long_term_risk_data[,"Long_Term_Risk"] <- NA
#' print(long_term_risk_data)
#'
#' long_term_risk_data$Long_Term_Risk<-low_drink_long_fun(long_term_risk_data[[1]], 
#'                                                        long_term_risk_data[[2]], 
#'                                                        long_term_risk_data[[3]], 
#'                                                        long_term_risk_data[[4]], 
#'                                                        long_term_risk_data[[5]], 
#'                                                        long_term_risk_data[[6]],
#'                                                        long_term_risk_data[7]], 
#'                                                        long_term_risk_data[[8]], 
#'                                                        long_term_risk_data[[9]],
#'                                                        long_term_risk_data[[10]],
#'                                                        long_term_risk_data[[11]])
#'print(long_term_risk_data)
#' @export
#' 
low_drink_long_fun <-
  function(DHH_SEX, ALWDWKY, ALC_1, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4, 
           ALW_2A5, ALW_2A6, ALW_2A7){
    # Test if inputs are in valid range
    if_else2(DHH_SEX %in% (1:2) & ALWDWKY %in% (0:995) & ALC_1 %in% (1:2) &
               ALW_1 %in% (1:2) & ALW_2A1 %in% (0:995) & ALW_2A2 %in% (0:995) &
               ALW_2A3 %in% (0:995) & ALW_2A4 %in% (0:995) &
               ALW_2A5 %in% (0:995) & ALW_2A6 %in% (0:995) &
               ALW_2A7 %in% (0:995),
    # Increased long term risk from due to drinking (1)
    if_else2(DHH_SEX == 1 & (ALW_2A1 %in%(4:995) | ALW_2A2 %in%(4:995) | 
                             ALW_2A3 %in%(4:995) | ALW_2A4 %in%(4:995) |
                             ALW_2A5 %in%(4:995) | ALW_2A6 %in%(4:995) |
                             ALW_2A7 %in%(4:995) | ALWDWKY %in%(16:995)), 1,
    if_else2(DHH_SEX == 2 & (ALW_2A1 %in%(3:995) | ALW_2A2 %in%(3:995) |
                             ALW_2A3 %in%(3:995) | ALW_2A4 %in%(3:995) |
                             ALW_2A5 %in%(3:995) | ALW_2A6 %in%(3:995) |
                             ALW_2A7 %in%(3:995) | ALWDWKY %in%(11:995)), 1,
    # No increased long term health risks due to drinking (2)
    # Includes those who did not drink in past 7 days or past 12 months
    if_else2(ALC_1 == 2 |ALW_1 ==2, 2,
    if_else2(DHH_SEX == 1 & (ALW_2A1 %in% (0:3) & ALW_2A2 %in% (0:3) &
                             ALW_2A3 %in% (0:3) & ALW_2A4 %in% (0:3) &
                             ALW_2A5 %in% (0:3) & ALW_2A6 %in% (0:3) &
                             ALW_2A7 %in% (0:3)) & ALWDWKY %in% (0:15), 2,
    if_else2(DHH_SEX == 2 & (ALW_2A1 %in% (0:2) & ALW_2A2 %in% (0:2) &
                             ALW_2A3 %in% (0:2) & ALW_2A4 %in% (0:2) &
                             ALW_2A5 %in% (0:2) & ALW_2A6 %in% (0:2) &
                             ALW_2A7 %in% (0:2)) & ALWDWKY %in% (0:10), 2,
             "NA(b)"))))),
    "NA(b)")
    
    
  }

#' @title Low drinking score (all cycles)
#' 
#' @description This function creates a derived variable based on their drinking 
#' habits and flags for health and social problems from their pattern of 
#' alcohol use according to Canada's Low-Risk Alcohol Drinking Guideline.
#'
#' @details The low risk drinking score is based on the scoring system in 
#' Canada's Low-Risk Alcohol Drinking Guideline. The score is divided into two 
#' steps. Step 1 allocates points based on sex and the number of drinks 
#' that you usually have each week. In step 2, one point will be awarded for 
#' each item that is true related to drinking habits. The total score is 
#' obtained from adding the points in step 1 and step 2.
#' 
#' @note Step 2 is not included in this function because the questions in 
#' step 2 are not asked in any of the CCHS cycles. The score is only based on 
#' step 1.
#' 
#' See \url{https://osf.io/eprg7/} for more details on the guideline and score. 
#' 
#' @param DHH_SEX Sex of respondent (1 - male, 2 - female)
#' 
#' @param ALWDWKY Number of drinks consumed in the past week 
#' 
#' @return Low risk drinking score (low_drink_score) with four categories:
#' \itemize{
#'   \item 1 - Low risk (0 points)
#'   \item 2 - Marginal risk (1-2 points)
#'   \item 3 - Medium risk (3-4 points)
#'   \item 4 - High risk (5-9 points)
#'  }
#' 
#' @examples
#'  
#' # Using low_drink_score_fun() to create low_drink_score values across 
#' # CCHS cycles low_drink_score_fun() is specified in variable_details.csv 
#' # along with the CCHS variables and cycles included.
#' 
#' # To transform low_drink_score, use rec_with_table() for each CCHS cycle
#' # and specify low_drink_score, along with the various alcohol and sex
#' # variables. 
#' # Using merge_rec_data(), you can combine low_drink_score across cycles.
#' 
#' library(cchsflow)
#' low_drink2001 <- rec_with_table(
#'   cchs2001_p, c(
#'     "DHH_SEX", "ALWDWKY", "low_risk_score"
#'   )
#' )
#' 
#' head(low_drink2001)
#' 
#' low_drink2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "DHH_SEX", "ALWDWKY", "low_risk_score"
#'   )
#' )
#' 
#' tail(low_drink2009_2010)
#' 
#' combined_low_drink <- bind_rows(low_drink2001, 
#' low_drink2009_2010)
#' 
#' head(combined_low_drink)
#' 
#' tail(combined_low_drink)
#' 
#' library(cchsflow)
#' DHH_SEX <- c(1, 2) 
#' ALWDWKY <- c(30, 15)
#' 
#' low_drink_score_data <- data.frame(DHH_SEX, ALWDWKY)
#' low_drink_score_data[,"Low_Drink_Score"] <- NA
#' print(low_drink_score_data)
#'
#' low_drink_score_data$Low_Drink_Score <-low_drink_score_fun(low_drink_score_data[[1]], 
#'                                                        low_drink_score_data[[2]])
#'print(low_drink_score_data)
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: Improved scoring algorithm with enhanced validation
#' @export
#' 
low_drink_score_fun <-
  function(DHH_SEX, ALWDWKY){
    ## Step 1
    # How many standard drinks did you have in a week?
    step1<- 
      if_else2(DHH_SEX %in% (1:2) & ALWDWKY %in% (0:995),
      if_else2(ALWDWKY %in% (0:10), 0,
      if_else2(DHH_SEX == 1 & ALWDWKY > 10 & ALWDWKY <= 15, 0,
      if_else2(DHH_SEX == 2 & ALWDWKY > 10 & ALWDWKY <= 15, 1,
      if_else2(DHH_SEX == 1 & ALWDWKY > 15 & ALWDWKY <= 20, 1,
      if_else2(DHH_SEX == 2 & ALWDWKY > 15 & ALWDWKY <= 20, 3,
      if_else2(ALWDWKY >20, 3, NA)))))),
      NA)
    
    ## Categorical score
    low_drink_score <-
      # Low risk
      if_else2(step1 == 0, 1,
      # Marginal risk
      if_else2(step1 %in% (1:2), 2,
      # Medium risk
      if_else2(step1 %in% (3:4), 3,
      # High risk
      if_else2(step1 %in% (5:9), 4, "NA(b)"))))
    
    return(low_drink_score)
  }

#' @title Low drinking score (select cycles)
#' 
#' @description This function creates a derived variable based on their drinking 
#' habits and flags for health and social problems from their pattern of 
#' alcohol use according to Canada's Low-Risk Alcohol Drinking Guideline.
#'
#' @details The low risk drinking score is based on the scoring system in 
#' Canada's Low-Risk Alcohol Drinking Guideline. The score is divided into two 
#' steps. Step 1 allocates points based on sex and the number of drinks 
#' that you usually have each week. In step 2, one point will be awarded for 
#' each item that is true related to drinking habits. The total score is 
#' obtained from adding the points in step 1 and step 2.
#' 
#' This score has two 0 point categories: low risk (never drank) and low risk 
#' (former drinker). The two drinking groups are derived from 'ever had a drink 
#' in lifetime'. 'Ever had a drink in lifetime' is only available in CCHS 
#' 2001-2008 and 2015-2018.
#' 
#' 
#' @note Step 2 is not included in this function because the questions in 
#' step 2 are not asked in any of the CCHS cycles. The score is only based on 
#' step 1.
#' 
#' See \url{https://osf.io/eprg7/} for more details on the guideline and score. 
#' 
#' @param DHH_SEX Sex of respondent (1 - male, 2 - female)
#' 
#' @param ALWDWKY Number of drinks consumed in the past week 
#' 
#' @param ALC_005 In lifetime, ever had a drink? (1 - yes, 2 - no)
#' 
#' @param ALC_1 Past year, have you drank alcohol? (1 - yes, 2 - no)
#' 
#' @return Low risk drinking score (low_drink_score1) with four categories:
#' \itemize{
#'   \item 1 - Low risk - never drank (0 points)
#'   \item 2 - Low risk - former drinker (0 points)
#'   \item 3 - Marginal risk (1-2 points)
#'   \item 4 - Medium risk (3-4 points)
#'   \item 5 - High risk (5-9 points)
#'  }
#' 
#' @examples
#'  
#' # Using low_drink_score_fun1() to create low_drink_score values across 
#' # CCHS cycles low_drink_score_fun1() is specified in variable_details.csv 
#' # along with the CCHS variables and cycles included.
#' 
#' # To transform low_drink_score1, use rec_with_table() for each CCHS cycle
#' # and specify low_drink_score1, along with the various alcohol and sex
#' # variables. 
#' # Using merge_rec_data(), you can combine low_drink_score1 across cycles.
#' 
#' library(cchsflow)
#' low_drink2001 <- rec_with_table(
#'   cchs2001_p, c(
#'     "DHH_SEX", "ALWDWKY", "ALC_005", "ALC_1", "low_risk_score"
#'   )
#' )
#' 
#' head(low_drink2001)
#' 
#' low_drink2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "DHH_SEX", "ALWDWKY", "ALC_005", "ALC_1", "low_risk_score"
#'   )
#' )
#' 
#' tail(low_drink2009_2010)
#' 
#' combined_low_drink1 <- bind_rows(low_drink2001, 
#' low_drink2009_2010)
#' 
#' head(combined_low_drink1)
#' 
#' tail(combined_low_drink1)
#' 
#' library(cchsflow)
#' DHH_SEX <- c(1, 2) 
#' ALWDWKY <- c(30, 15)
#' ALC_005 <- c(1, 1)
#' ALC_1 <- c(1, 1)
#' 
#' low_drink_score1_data <- data.frame(DHH_SEX, ALWDWKY, ALC_005, ALC_1)
#' low_drink_score1_data[,"Low_Drink_Score"] <- NA
#' print(low_drink_score1_data)
#'
#' low_drink_score1_data$Low_Drink_Score <-low_drink_score1_fun(low_drink_score_data[[1]], 
#'                                                        low_drink_score_data[[2]],
#'                                                        low_drink_score_data[[3]],
#'                                                        low_drink_score_data[[4]])
#'print(low_drink_score1_data)
#' @export
#' 
low_drink_score_fun1 <-
  function(DHH_SEX, ALWDWKY, ALC_005, ALC_1){
    ## Step 1
    # How many standard drinks did you have in a week?
    step1<- 
     if_else2(DHH_SEX %in% (1:2) & ALWDWKY %in% (0:995) & ALC_005 %in% (1:2) &
               ALC_1 %in% (1:2),
     if_else2(ALWDWKY %in% (0:10), 0,
     if_else2(DHH_SEX == 1 & ALWDWKY > 10 & ALWDWKY <= 15, 0,
     if_else2(DHH_SEX == 2 & ALWDWKY > 10 & ALWDWKY <= 15, 1,
     if_else2(DHH_SEX == 1 & ALWDWKY > 15 & ALWDWKY <= 20, 1,
     if_else2(DHH_SEX == 2 & ALWDWKY > 15 & ALWDWKY <= 20, 3,
     if_else2(ALWDWKY >20, 3, NA)))))),
     NA)
    
    ## Categorical score
    low_drink_score1 <-
      # Low risk - never drank
      if_else2(step1 == 0 & ALC_005 == 2 & ALC_1 ==2, 1,
      # Low risk - former drinker
      if_else2(step1 == 0 & ALC_005 == 1 & ALC_1 ==2, 2,
      # Marginal risk
      if_else2(step1 %in% (1:2), 3,
      # Medium risk
      if_else2(step1 %in% (3:4), 4,
      # High risk
      if_else2(step1 %in% (5:9), 5, "NA(b)")))))
    
    return(low_drink_score1)
  }

#' @title ICES Type of drinker
#' 
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a categorical variable derived from ALCDTYP and ALCDTTM that uses
#'  various intermediate alcohol variables to categorize individuals into 2
#'  distinct groups:
#'
#'  \enumerate{
#'   \item Former Drinker
#'   \item Other Drinker
#'  }
#'
#'  For CCHS cycles from 2001 to 2007, categories regular drinker, 
#'  occasional drinker and never drinker are combined into an 
#'  "Other Drinker" category. Starting in 2007, Statistics Canada created 
#'  a derived variable that looked at drinking type in the last 12 months. 
#'  This new derived variable did not distinguish between former and never 
#'  drinkers. Therefore, for ICES CCHS cycles from 2007 onward regular drinker
#'  and occasional drinker are combined into the "Other Drinker" category
#'  and "Did not drink in the last 12 months" was categorized as "Former 
#'  Drinker". If your research requires you to differentiate between former 
#'  and never drinkers, we recommend using earlier cycles of the CCHS.
#'
#' @param ALCDTYP_A cchsflow variable name for type of drinker
#'
#' @examples
#' library(cchsflow)
#'  ?ALCDTYP_A
#'
#' @note v3.0.0, last updated: 2025-06-30, status: active, Note: New function for alcohol type processing in master data files
#' @export
ALCDTYP_A <- function(ALCDTYP_A) {
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
#'  distinguish between former and never drinkers. Please note that ALCDTMM for
#'  cycles from 2001 to 2005 utilize ALCDTYP where 'former' and 'never' drinkers
#'  are re-coded into 'no drink in the last 12 months' category. 
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

#' @title Number of drinks - Weekly
#' 
#' @description \strong{NOTE:} this is not a function.
#'
#'  This is a continuous variable derived by Statistics Canada that quantifies 
#'  the number of alcoholic drinks consumed weekly.
#' 
#' @details This variable is present in every CCHS cycle used in cchsflow.
#'
#' @param ALWDWKY cchsflow variable name for number of drinks per week
#'
#' @examples
#' library(cchsflow)
#'  ?ALWDWKY
#'
#' @export
ALWDWKY <- function(ALWDWKY) {
  # this is for documentation purposes only
}
