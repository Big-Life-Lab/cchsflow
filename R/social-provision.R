#' @title Five-item social provision scale (SPS-5)
#'
#' @description This function creates a derived variable for the five-item social 
#' provision scale (SPS_5_fun). The range is 0-20, where a higher score
#' reflects a higher level of perceived social support.
#'  
#' @details The Social Provisions Scale (SPS) is commonly used to measure 
#' social support. The ten-item social provisions scale (SPS-10) has been 
#' reduced to a five-item scale (SPS-5).Reducing the SPS-10 items by half 
#' decreases the respondent burden on surveys. SPS-5 is a valid measure of 
#' social support while maintaining adequate measurement properties 
#' (Orpana et al., 2019). Validation of SDS-5 using Canadian national survey 
#' data can be found  
#' \href{https://www.canada.ca/en/public-health/services/reports-publications/health-promotion-chronic-disease-prevention-canada-research-policy-practice/vol-39-no-12-2019/original-quantitative-research-social-provisions-scale.html}{here}. 
#' 
#' SPS-10 and their items were available in CCHS from 2011-2018. 
#' 
#'
#' @param SPS_03 close relationships that provide sense of emotional 
#' security and well-being 
#'
#' @param SPS_04 talk to about important decisions with someone 
#' 
#' @param SPS_05 relationships where competence and skill are recognized 
#'
#' @param SPS_07 part of a group who share attitudes and beliefs 
#' 
#' @param SPS_10 strong emotional bond with a least one person 
#'
#' @examples 
#' # Using the SPS_5_fun function to create the derived SPS5_der variable  
#' # across CCHS cycles.
#' # SPS_5_fun() is specified in the variable_details.csv.
#'
#' # To create a harmonized SPS5_der variable across CCHS cycles, use 
#' # rec_with_table() for each CCHS cycle and specify SPS_5_fun and the
#' # required base variables.
#' # Using merge_rec_data(), you can combine SPS5_der across cycles.
#'
#' library(cchsflow)
#'
#' SPS5_2011_2012 <- rec_with_table(
#'   cchs2011_2012_p, c(
#'     "SPS_03", "SPS_04", "SPS_05", "SPS_07", "SPS_10", "SPS5_der"
#'   )
#' )
#'
#' head(SPS5_2011_2012)
#'
#' SPS5_2017_2018 <- rec_with_table(
#'   cchs2017_2018_p,c(
#'     "SPS_03", "SPS_04", "SPS_05", "SPS_07", "SPS_10", "SPS5_der"
#'   )
#' )
#'
#' tail(SPS5_2017_2018)
#'
#' combined_SPS5 <- suppressWarnings(merge_rec_data(SPS5_2011_2012,
#'  SPS5_2017_2018))
#'
#' head(combined_SPS5)
#' tail(combined_SPS5)
#' @export
#' 
SPS_5_fun <-
  function(SPS_03, SPS_04, SPS_05, SPS_07, SPS_10) {
    
    # Raw scale
    SPS5_raw_scale <- 
      if_else2(!is.na(SPS_03) & !is.na(SPS_04) & !is.na(SPS_05)& 
                 !is.na(SPS_07) & !is.na(SPS_10), 
               SPS_03 + SPS_04 + SPS_05 + SPS_07 + SPS_10, NA)
    
    
    # Final score
    SPS_5 <- if_else2(SPS5_raw_scale <0, 0,
                           if_else2(SPS5_raw_scale >20, 20,
                                    if_else2(!is.na(SPS5_raw_scale), SPS5_raw_scale,
                                             "NA(b)")))
    return(SPS_5)
  }