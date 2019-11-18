#' @title ifelse2
#' 
#' @description Custom ifelse function that evaluates missing (NA) values
#' 
#' @details If the logical argument compares to a value that is NA, it will set `x` to `FALSE`  
#' 
#' @param x A logical argument
#' 
#' @param a value if `x` is `TRUE`
#' 
#' @param b value if `x` is `FALSE`
#' 
#' @return None
#' 
#' @examples ifelse2((age<18),"child", "adult")
#' 
#' @export
ifelse2 <- function(x, a, b) {
  falseifNA <- function(x) {
    ifelse(is.na(x), FALSE, x)
  }
  ifelse(falseifNA(x), a, b)
}

#' @title Body Mass Index (BMI) derived variable
#' 
#' @description This function creates a harmonized BMI variable. The BMI variable provided by the CCHS calculates BMI using methods
#' that varies across cycles, leading to measurement error when using multiple CCHS cycles. In certain CCHS cycles (2001-2003, 2007+),
#' there are age restrictions in which respondents under the age of 20 and over the age of 64 were not included. Across all CCHS cycles
#' female respondents who identifed as being pregnant were excluded; and in certain CCHS cycles (2003-2007, 2013-2014), females who did not
#' answer the pregnancy question were coded as NS (not stated) for HWTGBMI. As well, in certain CCHS cycles (2001-2003, 2009-2014),
#' respondents outside certain height and weight ranges (0.914-2.108m for height, 0-260kg for weight) were excluded from HWTGBMI. 
#' 
#' @details For HWTGBMI_derived, there are no restrictions to age, height, weight, or pregnancy status. While pregnancy was consistent across 
#' all CCHS cycles, its variable (MAM_037) was not available in the PUMF CCHS datasets so it could not be harmonized and included into 
#' the function.
#' 
#' For any single CCHS survey year, it is appropriate to use the CCHS BMI variable (HWTGBMI) that is also available on cchsflow.
#' HWTGBMI_derived is recommended when using multiple survey cycles.  
#' 
#' HWTGBMI_derived uses the CCHS variables for height and weight that have been transformed by cchsflow. In order to 
#' generate a value for BMI across CCHS cycles, height and weight must be transformed and harmonized.
#' 
#' @note In earlier CCHS cycles (2001 and 2003), height was collected in inches; while in later CCHS cycles (2005+) 
#' it was collected in meters. To harmonize values across cycles, height was converted to meters (to 3 decimal points). 
#' Weight was collected in kilograms across all CCHS cycles, so no transformations were required in the harmonization process.
#' 
#' @param HWTGHTM CCHS variable for height (in meters)
#' 
#' @param HWTGWTK CCHS variable for weight (in kilograms)
#' 
#' @return numeric value for BMI in the HWTGBMI_derived variable
#' 
#' @examples 
#' # Using BMI_der to create BMI values across cycles
#' # BMI_der is specified in variableDetails.csv along with the CCHS variables and cycles included. 
#' 
#' # To transform the derived BMI variable, use RecWTable() for each CCHS cycle and specify HWTGBMI_derived, 
#' # along with height (HWTGHTM) and weight (HWTGWTK). Then by using bind_rows(), you can combined derived BMI variables
#' # across cycles
#' 
#' bmi2010 <- RecWTable(dataSource = cchs2010, variableDetails = variableDetails, datasetName = "cchs2010", 
#' variables = c("HWTGHTM", "HWTGWTK", "BMI_der"))
#' 
#' bmi2012 <- RecWTable(dataSource = cchs2012, variableDetails = variableDetails, datasetName = "cchs2012", 
#' variables = c("HWTGHTM", "HWTGWTK", "BMI_der"))
#' 
#' combined_bmi <- bind_rows(bmi2010, bmi2012)
#' 
#' # Using BMI_der to generate a BMI value with user inputted height and weight values
#' # BMI_der can also generate a value for BMI if you input a value for height and weight. Let's your height is
#' # 170cm (1.7m) and your weight is 50kg, your BMI can be calculated as followed:
#' 
#' library(cchsflow)
#' BMI <- BMI_der(1.7, 50)
#' 
#' @export
BMI_der <- 
  function(HWTGHTM, 
           HWTGWTK) {
    ifelse2((!is.na(HWTGHTM)) & (!is.na(HWTGWTK)), 
            (HWTGWTK/(HWTGHTM*HWTGHTM)), NA)
  }