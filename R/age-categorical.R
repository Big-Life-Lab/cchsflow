#' @title Derived categorical age
#' 
#' @description This is a derived categorical age variable that groups various age categories 
#'  across all CCHS cycles. This is based on the continuous age variable (DHHGAGE_cont) that is
#'  harmonious across all CCHS cycles.
#'  
#'  The categories of this variable are based on the age groupings seen in CCHS cycles from 2007 to
#'  2014. The age breakdown is as follows:
#'  
#'  1 - 12 to 14 years
#'  
#'  2 - 15 to 17 years
#'  
#'  3 - 18 to 19 years
#'  
#'  4 - 20 to 24 years
#'  
#'  5 - 25 to 29 years
#'  
#'  6 - 30 to 34 years
#'  
#'  7 - 35 to 39 years
#'  
#'  8 - 40 to 44 years
#'  
#'  9 - 45 to 49 years
#'  
#'  10 - 50 to 54 years
#'  
#'  11 - 55 to 59 years
#'  
#'  12 - 60 to 64 years
#'  
#'  13 - 65 to 69 years
#'  
#'  14 - 70 to 74 years
#'  
#'  15 - 75 to 79 years
#'  
#'  16 - 80 years or more
#'  
#' @details The categories in the grouped age variable (DHHGAGE) vary between CCHS cycles. As such,
#'  a continous age variable (DHHGAGE_cont) was created that harmonized age across all CCHS cycle
#'  by taking the midpoint of each age category.
#'  
#' @param DHHGAGE_cont continuous age variable
#' 
#' @export
DHHGAGE_cat_fun <- function(DHHGAGE_cont) {
  ifelse2((DHHGAGE_cont %in% 12:14), 1,
  ifelse2((DHHGAGE_cont %in% 15:17), 2,
  ifelse2((DHHGAGE_cont %in% 18:19), 3,
  ifelse2((DHHGAGE_cont %in% 20:24), 4,
  ifelse2((DHHGAGE_cont %in% 25:29), 5,
  ifelse2((DHHGAGE_cont %in% 30:34), 6,
  ifelse2((DHHGAGE_cont %in% 35:39), 7,
  ifelse2((DHHGAGE_cont %in% 40:44), 8,
  ifelse2((DHHGAGE_cont %in% 45:49), 9,
  ifelse2((DHHGAGE_cont %in% 50:54), 10,
  ifelse2((DHHGAGE_cont %in% 55:59), 11,
  ifelse2((DHHGAGE_cont %in% 60:64), 12,
  ifelse2((DHHGAGE_cont %in% 65:69), 13,
  ifelse2((DHHGAGE_cont %in% 70:74), 14,
  ifelse2((DHHGAGE_cont %in% 75:79), 15,
  ifelse2((DHHGAGE_cont >= 80), 16, NA))))))))))))))))
}