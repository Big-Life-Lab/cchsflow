#' @title Body Mass Index (BMI) derived variable
#'
#' @description This function creates a harmonized BMI variable. The BMI
#'  variable provided by the CCHS calculates BMI using methods that vary across
#'  cycles, leading to measurement error when using multiple CCHS cycles. In
#'  certain CCHS cycles (2001-2003, 2007+), there are age restrictions in which
#'  respondents under the age of 20 and over the age of 64 were not included.
#'  Across all CCHS cycles, female respondents who identified as being pregnant
#'  were excluded; and in certain CCHS cycles (2003-2007, 2013-2014), females
#'  who did not answer the pregnancy question were coded as NS (not stated) for
#'  HWTGBMI. As well, in certain CCHS cycles (2001-2003, 2009-2014), respondents
#'  outside certain height and weight ranges (0.914-2.108m for height,
#'  0-260kg for weight) were excluded from HWTGBMI.
#'
#'  bmi_fun() creates a derived variable (HWTGBMI_der) that is harmonized across
#'  all CCHS cycles. This function divides weight by the square of height.
#'
#' @details For HWTGBMI_der, there are no restrictions to age, height, weight,
#'  or pregnancy status. While pregnancy was consistent across all CCHS cycles,
#'  its variable (MAM_037) was not available in the PUMF CCHS datasets so it
#'  could not be harmonized and included into the function.
#'
#'  For any single CCHS survey year, it is appropriate to use the CCHS BMI
#'  variable (HWTGBMI) that is also available on cchsflow. HWTGBMI_der is
#'  recommended when using multiple survey cycles.
#'
#'  HWTGBMI_der uses the CCHS variables for height and weight that have been
#'  transformed by cchsflow. In order to generate a value for BMI across CCHS
#'  cycles, height and weight must be transformed and harmonized.
#'
#' @note In earlier CCHS cycles (2001 and 2003), height was collected in inches;
#'  while in later CCHS cycles (2005+) it was collected in meters. To harmonize
#'  values across cycles, height was converted to meters (to 3 decimal points).
#'  Weight was collected in kilograms across all CCHS cycles, so no
#'  transformations were required in the harmonization process.
#'
#' @param HWTGHTM CCHS variable for height (in meters)
#'
#' @param HWTGWTK CCHS variable for weight (in kilograms)
#'
#' @return numeric value for BMI in the HWTGBMI_der variable
#'
#' @examples
#' # Using bmi_fun() to create BMI values between cycles
#' # bmi_fun() is specified in variable_details.csv along with the
#' # CCHS variables and cycles included.
#'
#' # To transform the derived BMI variable, use rec_with_table() for each cycle
#' # and specify HWTGBMI_der, along with height (HWTGHTM) and weight (HWTGWTK).
#' # Then by using merge_rec_data(), you can combined HWTGBMI_der across
#' # cycles.
#'
#' library(cchsflow)
#' bmi2001 <- rec_with_table(
#'   cchs2001_p, c(
#'     "HWTGHTM",
#'     "HWTGWTK", "HWTGBMI_der"
#'   )
#' )
#'
#' head(bmi2001)
#'
#' bmi2011_2012 <- rec_with_table(
#'   cchs2011_2012_p, c(
#'     "HWTGHTM",
#'     "HWTGWTK", "HWTGBMI_der"
#'   )
#' )
#'
#' tail(bmi2011_2012)
#'
#' combined_bmi <- merge_rec_data(bmi2001, bmi2011_2012)
#' head(combined_bmi)
#' tail(combined_bmi)
#'
#' # Using bmi_fun() to generate a BMI value with user inputted height and
#' # weight values. bmi_fun() can also generate a value for BMI if you input a
#' # value for height and weight. Let's say your height is 170cm (1.7m) and
#' # your weight is 50kg, your BMI can be calculated as follows:
#'
#' library(cchsflow)
#' BMI <- bmi_fun(HWTGHTM = 1.7, HWTGWTK = 50)
#' print(BMI)
#' @export
bmi_fun <-
  function(HWTGHTM, HWTGWTK) {
    if_else2(
      (!is.na(HWTGHTM)) & (!is.na(HWTGWTK)), (HWTGWTK / (HWTGHTM * HWTGHTM)),
      tagged_na("b")
    )
  }

#' @title Adjusted Body Mass Index (BMI) derived variable
#'
#' @description This function creates a harmonized adjusted BMI variable. 
#' A systematic review of the literature concluded that the use of 
#' self-reported data among adults underestimates weight and overestimates 
#' height, resulting in lower estimates of obesity than those obtained from 
#' measured data. Using data from the 2005 Canadian Community Health Survey 
#' (CCHS) subsample, where both measured and self-reported values were 
#' collected, correction equations have been developed 
#' (Connor Gorber et al. 2008). Differences between corrected estimates of 
#' obesity from the CCHS and measured estimates from the Canadian Health 
#' Measures Survey is monitored over time to determine if the bias in 
#' self-reported values is changing and if new correction equations need to be 
#' developed. Adjusted BMI variable is first introduced in the CCHS 2015 cycle.
#'
#'  adjusted_bmi_fun() creates a derived variable (HWTGCOR_der) that is 
#'  harmonized across all CCHS cycles. This function takes the BMI by dividing 
#'  weight by the square of height, and adds a correction value based on sex.
#'
#' @details For HWTGCOR_der, there are no restrictions to age, height, weight,
#'  or pregnancy status. While pregnancy was consistent across all CCHS cycles,
#'  its variable (MAM_037) was not available in the PUMF CCHS datasets so it
#'  could not be harmonized and included into the function.
#'
#'  HWTGCOR_der uses the CCHS variables for sex, height and weight that have been
#'  transformed by cchsflow. In order to generate a value for adjusted BMI 
#'  across CCHS cycles, sex, height and weight must be transformed and 
#'  harmonized.
#'
#' @note In earlier CCHS cycles (2001 and 2003), height was collected in inches;
#'  while in later CCHS cycles (2005+) it was collected in meters. To harmonize
#'  values across cycles, height was converted to meters (to 3 decimal points).
#'  Weight was collected in kilograms across all CCHS cycles, so no
#'  transformations were required in the harmonization process.
#'
#' @param HWTGHTM CCHS variable for height (in meters)
#'
#' @param HWTGWTK CCHS variable for weight (in kilograms)
#'
#' @param DHH_SEX CCHS variable for sex; 1 = male, 2 = female
#' 
#' @return numeric value for adjusted BMI in the HWTGCOR_der variable
#'
#' @examples
#' # Using adjusted_bmi_fun() to create adjusted BMI values between cycles
#' # adjusted_bmi_fun() is specified in variable_details.csv along with the
#' # CCHS variables and cycles included.
#'
#' # To transform the derived BMI variable, use rec_with_table() for each cycle
#' # and specify HWTGCOR_der, along with sex (DHH_SEX), height (HWTGHTM) and 
#' # weight (HWTGWTK).Then by using merge_rec_data(), you can combined 
#' # HWTGBMI_der across cycles.
#'
#' library(cchsflow)
#' adjustedbmi2001 <- rec_with_table(
#'   cchs2001_p, c(
#'     "HWTGHTM",
#'     "HWTGWTK", 
#'     "DHH_SEX",
#'     "HWTGCOR_der"
#'   )
#' )
#'
#' head(adjustedbmi2001)
#'
#' adjustedbmi2011_2012 <- rec_with_table(
#'   cchs2011_2012_p, c(
#'     "HWTGHTM",
#'     "HWTGWTK", 
#'     "DHH_SEX",
#'     "HWTGCOR_der"
#'   )
#' )
#'
#' tail(adjustedbmi2011_2012)
#'
#' combined_bmi <- merge_rec_data(adjustedbmi2001, adjustedbmi2011_2012)
#' head(combined_adjustedbmi)
#' tail(combined_adjustedbmi)
#'
#' # adjusted_bmi_fun() can also generate a value for BMI if you input your sex, 
#' # and a value for height and weight. Let's say your sex is male, height is 
#' 170cm (1.7m) and your weight is 50kg, your BMI can be calculated as follows:
#'
#' library(cchsflow)
#' adjusted_BMI <- adjusted_bmi_fun(DHH_SEX = 1, HWTGHTM = 1.7, HWTGWTK = 50)
#' print(adjusted_BMI)
#' @export

adjusted_bmi_fun <-
  function(DHH_SEX, HWTGHTM, HWTGWTK) {
    # BMI adjusted for male
    if_else2(
      (!is.na(HWTGHTM)) & (!is.na(HWTGWTK)) & DHH_SEX==1, 
      -1.07575 + 1.07592*(HWTGWTK / (HWTGHTM * HWTGHTM)),
      # BMI adjusted for female
      if_else2(
        (!is.na(HWTGHTM)) & (!is.na(HWTGWTK)) & DHH_SEX==2, 
        -0.12374 + 1.05129*(HWTGWTK / (HWTGHTM * HWTGHTM)),
        tagged_na("b")
      )
    )
  }

#' @title Categorical BMI (international standard)
#' 
#' @description This function creates a categorical derived variable
#' (HWTGBMI_der_cat4) that categorizes derived BMI (HWTGBMI_der).
#' 
#' @details The categories were based on international standards and are divided
#' into four categories: underweight for BMI < 18.5 (1), normal weight for BMI 
#' between 18.5 to 25 (2), overweight for BMI between 25 to 30 (3), and obese 
#' for BMI over 30 (4). 
#' 
#' HWTGBMI_der_cat4 uses the derived variable HWTGBMI_der. HWTGBMI_der uses
#' height and weight that have been transformed by cchsflow. In order to 
#' categorize BMI across CCHS cycles, height and weight variables must be 
#' transformed and harmonized.
#' 
#' @param HWTGBMI_der derived variable that calculates numeric value for BMI.
#'  See \code{\link{bmi_fun}} for documentation on how variable
#'  was derived.
#'  
#' @return value for BMI categories in the HWTGBMI_der_cat4 variable.
#'  
#' @examples  
#' # Using bmi_fun_cat() to categorize BMI across CCHS cycles
#' # bmi_fun_cat() is specified in variable_details.csv along with the 
#' # CCHS variables and cycles included.
#' 
#' # To transform HWTGBMI_der_cat4 across all cycles, use rec_with_table() for 
#' # each CCHS cycle.
#' # Since HWTGBMI_der is also a derived variable, you will have to specify 
#' # the variables that are derived from it.
#' 
#' library(cchsflow)
#'
#' bmi_cat_2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "HWTGHTM",
#'     "HWTGWTK",
#'     "HWTGBMI_der",
#'     "HWTGBMI_der_cat4"
#'   )
#' )
#'
#' head(bmi_cat_2009_2010)
#'
#' bmi_cat_2011_2012 <- rec_with_table(
#'   cchs2011_2012_p,c(
#'     "HWTGHTM",
#'     "HWTGWTK",
#'     "HWTGBMI_der",
#'     "HWTGBMI_der_cat4"
#'   )
#' )
#'
#' tail(bmi_cat_2011_2012)
#'
#' combined_bmi_cat <- suppressWarnings(merge_rec_data
#' (bmi_cat_2009_2010,bmi_cat_2011_2012))
#'
#' head(combined_bmi_cat)
#' tail(combined_bmi_cat)
#' @export

bmi_fun_cat <-
  function(HWTGBMI_der){
    # Underweight
    if_else2(HWTGBMI_der < 18.5, 1,
    # Normal weight
    if_else2(HWTGBMI_der >= 18.5 & HWTGBMI_der < 25, 2,
    # Overweight
    if_else2(HWTGBMI_der >= 25 & HWTGBMI_der < 30, 3,
    # Obese
    if_else2(HWTGBMI_der >= 30, 4,
    # No response
    if_else2(HWTGBMI_der == tagged_na("a"), "NA(a)", "NA(b)")))))
  }
