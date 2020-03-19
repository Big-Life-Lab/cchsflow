#' @title Participation and Activity Limitation 
#' 
#' @description This is a derived variable used in the CCHS (RACDPAL) to
#'  classify respondents according to the frequency with which they experience
#'  activity limitations due to disability.
#'  
#' @details This derived variable is generated in CCHS cycles 2003-2014. The
#'  2001 CCHS cycle, however, contains the same base variables used to derive
#'  this variable. To include respondents in the 2001 CCHS cycle, this custom
#'  function was created using the same derivation conditions used in later
#'  cycles.
#' 
#' @param RAC_1 Has difficulty with activities due to disability
#' 
#' @param RAC_2A Reduction in activities at home due to disability
#' 
#' @param RAC_2B Reduction in activities at school or work due to disability
#' 
#' @param RAC_2C Reduction in other activities
#' 
#' @return the CCHS derived variable RACDPAL with 3 categories:
#'  \enumerate{
#'   \item Sometimes
#'   \item Often
#'   \item Never
#'   }
#' 
#' @examples 
#' # Using RACDPAL_fun() to transform RACDPAL in 2001. 
#' # RACDPAL_fun() is specified in variable_details.csv along with the
#' # CCHS variables and cycles included.
#'  
#' # To transform RACDPAL, use rec_with_table() for each the 2001 cycle
#' # and specify RACDPAL, along with the various ADL variables.
#' 
#' library(cchsflow)
#' 
#' RACDPAL_2001 <- rec_with_table(
#'   cchs2001_p, c(
#'     "RAC_1", "RAC_2A", "RAC_2B", "RAC_2C", "RACDPAL"
#'   )
#' )
#' 
#' head(RACDPAL_2001)
#' 
#' # Note: In other CCHS cycles you only need to specify RACDPAL as the variable
#' # was included in those survey cycles.
#' 
#' # Using RACDPAL_fun() with user inputted data.
#' 
#' # Let's say you're an individual that sometimes has difficulties with
#' # activities due to disability, sometimes has a reduction in activities at
#' # home, often has a reduction at school or work, and never has a reduction
#' # in other activities. Your participation and activity limitation can be
#' # determined as follows:
#' 
#' library(cchsflow)
#' RACDPAL <- RACDPAL_fun(1, 1, 2, 3)
#' print(RACDPAL)
#' 
#' @export

RACDPAL_fun <- function(RAC_1, RAC_2A, RAC_2B, RAC_2C){
  # Check to see if all values are in range
  if_else2((RAC_1 %in% 1:3) & (RAC_2A %in% 1:3) & (RAC_2B %in% 1:4) &
             (RAC_2C %in% 1:3),
           # Check if respondents said reductions impacted them often
           if_else2(RAC_1 == 2 | RAC_2A == 2 | RAC_2B == 2 | RAC_2C == 2, 2,
                    # Check if respondents said reductions impacted them
                    # sometimes
                    if_else2(RAC_1 == 1 | RAC_2A == 1 | RAC_2B == 1 |
                               RAC_2C == 1, 1,
                             # Check if respondents said reductions never
                             # impacted them
                             if_else2(RAC_1 == 3 & RAC_2A == 3 &
                                        (RAC_2B %in% 3:4) &
                                        RAC_2C == 3, 3, NA))), NA
  )
}