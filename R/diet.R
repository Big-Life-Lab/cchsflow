#' @title Diet score
#'
#' @description This function creates a derived diet variable (diet_score)
#'  based on consumption of fruit, salad, potatoes, carrots, other vegetables
#'  and juice. 2 baseline points plus summation of total points for diet
#'  attributes. Negative overall scores are recoded to 0, resulting in a range
#'  from 0 to 10.
#' 
#' \itemize{
#'   \item 1 point per daily fruit and vegetable consumption, excluding fruit 
#'   juice (maximum 8 points).
#'   \item -2 points for high potato intake (≥7 (males),
#'    ≥5 (females) times/week)
#'   \item -2 points for no carrot intake
#'   \item -2 points per daily frquency of fruit juce consumption greater than 
#'   once/day (maximum -10 points)
#'  }
#'
#' @details While diet score can be calculated for all survey respondents, in
#'  the 2005 CCHS survey cycle, fruit and vegetable consumption was an optional
#'  section in which certain provinces had opted in to be asked to respondents.
#'  In this survey cycle, fruit and vegetable consumption was asked to
#'  respondents in British Columbia, Ontario, Alberta, and Prince Edward Island.
#'  As such, diet score has a large number of missing respondents for this 
#'  cycle.
#'
#' @param FVCDFRU daily consumption of fruit
#'
#' @param FVCDSAL daily consumption of green salad
#'
#' @param FVCDPOT daily consumption of potatoes
#'
#' @param FVCDCAR daily consumption of carrots
#'
#' @param FVCDVEG daily consumption of other vegetables
#'
#' @param FVCDJUI daily consumption of fruit juice
#' 
#' @param DHH_SEX sex; 1 = male, 2 = female
#'
#' @examples
#' # Using the 'diet_score_fun' function to create the derived diet variable  
#' # across CCHS cycles.
#' # diet_score_fun() is specified in the variable_details.csv
#'
#' # To create a harmonized diet_score variable across CCHS cycles, use 
#' # rec_with_table() for each CCHS cycle and specify diet_score_fun and the
#' # required base variables.
#' # Using merge_rec_data(), you can combine smoke_simple across cycles
#'
#' library(cchsflow)
#'
#' diet_score2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "FVCDFRU", "FVCDSAL", "FVCDPOT", "FVCDCAR", "FVCDVEG", "FVCDJUI", 
#'     "DHH_SEX", "diet_score_fun"
#'   )
#' )
#'
#' head(diet_score2009_2010)
#'
#' diet_score2011_2012 <- rec_with_table(
#'   cchs2011_2012_p,c(
#'     "FVCDFRU", "FVCDSAL", "FVCDPOT", "FVCDCAR", "FVCDVEG", "FVCDJUI", 
#'     "DHH_SEX", "diet_score_fun"
#'   )
#' )
#'
#' tail(diet_score2011_2012)
#'
#' combined_diet_score <- suppressWarnings(merge_rec_data(diet_score2009_2010,
#'  diet_score2011_2012))
#'
#' head(combined_diet_score)
#' tail(combined_diet_score)
#' @export
diet_score_fun <-
  function(FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG, FVCDJUI, DHH_SEX) {
    
    # Total fruit and vegetables, excluding fruit juice
      total_fruitveg <- 
        if_else2(!is.na(FVCDFRU) & !is.na(FVCDSAL) & !is.na(FVCDPOT) & 
                   !is.na(FVCDCAR)  & !is.na(FVCDVEG), FVCDFRU + FVCDSAL + 
                   FVCDPOT + FVCDCAR + FVCDVEG, NA)
    
    # Maximum total fruit and vegetables = 8
      max_fruitveg <-
        if_else2(is.na(total_fruitveg), NA,
                 if_else2(total_fruitveg>8, 8, total_fruitveg))
    
    # High potato intake flag
      daily_pot_limit <-
        if_else2(DHH_SEX==1, 1,
                 if_else2(DHH_SEX==2, 5/7, NA))
      FVCDPOT_high <-
        if_else2(is.na(FVCDPOT), NA,
                 if_else2(FVCDPOT>=(daily_pot_limit), 1, 0))

    # No carrot intake flag
      FVCDCAR_nil <-
        if_else2(is.na(FVCDCAR), NA,
                 if_else2(FVCDCAR==0, 1, 0))

    # High juice intake flag
      FVCDJUI_high <-
        if_else2(is.na(FVCDJUI), NA,
                 if_else2(FVCDJUI <=1, 0, FVCDJUI - 1))

   diet_raw_score <- if_else2(!is.na(max_fruitveg) & !is.na(FVCDPOT_high) & 
                     !is.na(FVCDCAR_nil) & !is.na(FVCDJUI_high),  2 +
                       max_fruitveg - (2*FVCDPOT_high) - (2*FVCDCAR_nil) - 
                       (2*FVCDJUI_high), NA)
   
  diet_score <- if_else2(diet_raw_score <0, 0,
            if_else2(diet_raw_score >10, 10,
                    if_else2(!is.na(diet_raw_score), diet_raw_score,
                             tagged_na("b"))))
   return(diet_score)
  }

