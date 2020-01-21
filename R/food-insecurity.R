#' @title food_insecurity_fun1
#' 
#' @description This function uses the food insecurity variable from the 2001
#'  and 2003 CCHS cycles to generate a derived food insecurity variable
#'  (food_insecurity_der) that can be harmonized across all cycles. 
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
#' @param FINF1 CCHS variable indicating food insecurity in the past 12 months
#'   
#' @return a categorical variable (food_insecurity_der) with 2 levels:
#' 
#'  \enumerate{
#'   \item 1 - no food insecurity in the last 12 months 
#'   \item 2 - food insecurity in the last 12 months
#'  }
#'
#' @examples 
#' # Using food_insecurity_fun1() to generate food_insecurity_der across CCHS
#' # cycles.
#' 
#' # food_insecurity_fun1() is specified in variables.csv and
#' # variableDetails.csv. While food_insecurity_fun1 is used only for the 2001
#' # and 2003 CCHS cycles, food_insecurity_fun2 and food_insecurity_fun3
#' # are specified in variables.csv & variableDetails.csv. This means you can
#' # transform and combine other cycles provided you specify the original
#' # food insecurity variables for each respective CCHS cycle.
#' 
#' # To transform food_insecurity_der, use rec_with_table()
#' # for each CCHS cycle along. Then using bind_rows(), you can
#' # combine multiple CCHS cycles. 
#' 
#' library(cchsflow)
#' food_insecurity2001 <- rec_with_table(cchs2001, 
#'    c("FINF1", "food_insecurity_der"))
#' head(food_insecurity2001)
#' 
#' food_insecurity2005 <- rec_with_table(cchs2005, 
#'    c("FSCDHFS", "food_insecurity_der"))
#' tail(food_insecurity2005)
#' 
#' comb_food_insecurity <- bind_rows(food_insecurity2001, food_insecurity2005)
#' head(comb_food_insecurity)
#' tail(comb_food_insecurity)
#' 
#' @seealso \code{\link{food_insecurity_fun2}},
#' \code{\link{food_insecurity_fun3}}
#' 
#' @export
food_insecurity_fun1 <- 
  function(FINF1) {
    if_else2(FINF1 == 1, 2,
             if_else2(FINF1 == 2, 1, NA))
  }

#' @title food_insecurity_fun2
#' 
#' @description This function uses the food insecurity variable from the 2005
#'  CCHS cycle to generate a derived food insecurity variable
#'  (food_insecurity_der) that can be harmonized across all cycles. 
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
#' @param FSCDHFS CCHS variable measuring hunger in the last 12 months
#' 
#' @return a categorical variable (food_insecurity_der) with 2 levels:
#' 
#'  \enumerate{
#'   \item 1 - no food insecurity in the last 12 months 
#'   \item 2 - food insecurity in the last 12 months
#'  }
#'
#' @examples 
#' # Using food_insecurity_fun2() to generate food_insecurity_der across CCHS
#' # cycles.
#' 
#' # food_insecurity_fun2() is specified in variables.csv and
#' # variableDetails.csv. While food_insecurity_fun2 is used only for the 2005
#' # CCHS cycle, food_insecurity_fun1 and food_insecurity_fun3 are specified in
#' # variables.csv & variableDetails.csv. This means you can
#' # transform and combine other cycles provided you specify the original
#' # food insecurity variables for each respective CCHS cycle.
#'  
#' # To transform food_insecurity_der, use rec_with_table()
#' # for each CCHS cycle. Then using bind_rows(), you can
#' # combine multiple CCHS cycles. 
#' 
#' library(cchsflow)
#' food_insecurity2005 <- rec_with_table(cchs2005, 
#'    c("FSCDHFS", "food_insecurity_der"))
#' head(food_insecurity2005)
#' 
#' food_insecurity2007 <- rec_with_table(cchs2007_2008, 
#'    c("FSCDHFS2", "food_insecurity_der"))
#' tail(food_insecurity2007)
#' 
#' comb_food_insecurity <- bind_rows(food_insecurity2005, food_insecurity2007)
#' head(comb_food_insecurity)
#' tail(comb_food_insecurity)
#' 
#' @seealso \code{\link{food_insecurity_fun1}},
#' \code{\link{food_insecurity_fun3}}
#' @export
food_insecurity_fun2 <-
  function(FSCDHFS) {
    if_else2(FSCDHFS == 0, 1,
             if_else2(FSCDHFS %in% 1:3, 2, NA))
  }

#' @title food_insecurity_fun3
#' 
#' @description This function uses the food insecurity variable from the 2007
#'  to 2014 CCHS cycles to generate a derived food insecurity variable
#'  (food_insecurity_der) that can be harmonized across all cycles. 
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
#' @return a categorical variable (food_insecurity_der) with 2 levels:
#' 
#' @param FSCDHFS2 CCHS variable measuring household food insecurity in the last
#'  12 months
#' 
#'  \enumerate{
#'   \item 1 - no food insecurity in the last 12 months 
#'   \item 2 - food insecurity in the last 12 months
#'  }
#'
#' @examples 
#' # Using food_insecurity_fun3() to generate food_insecurity_der across CCHS
#' # cycles.
#' 
#' # food_insecurity_fun3() is specified in variables.csv and
#' # variableDetails.csv. While food_insecurity_fun3 is used only for the 2007
#' # to 2014 CCHS cycles, food_insecurity_fun1 and food_insecurity_fun2
#' # are specified in variables.csv & variableDetails.csv. This means you can
#' # transform and combine other cycles provided you specify the original
#' # food insecurity variables for each respective CCHS cycle.
#'  
#' # To transform food_insecurity_der, use rec_with_table()
#' # for each CCHS cycle. Then using bind_rows(), you can
#' # combine multiple CCHS cycles. 
#' 
#' library(cchsflow)
#' food_insecurity2001 <- rec_with_table(cchs2001, 
#'    c("FSCDHFS", "food_insecurity_der"))
#' head(food_insecurity2001)
#' 
#' food_insecurity2014 <- rec_with_table(cchs2014, 
#'    c("FSCDHFS2", "food_insecurity_der"))
#' tail(food_insecurity2014)
#' 
#' comb_food_insecurity <- bind_rows(food_insecurity2001, food_insecurity2014)
#' head(comb_food_insecurity)
#' tail(comb_food_insecurity)
#' 
#' 
#' @seealso \code{\link{food_insecurity_fun1}},
#' \code{\link{food_insecurity_fun2}}
#' @export
food_insecurity_fun3 <-
  function(FSCDHFS2) {
    if_else2(FSCDHFS2 == 0, 1,
             if_else2(FSCDHFS2 %in% 1:2, 2, NA))
  }