multiple_conditions_fun <- 
  function(CCC_121,CCC_131,CCC_151,CCC_171,CCC_280,resp_condition,CCC_051){
    
    # Convert variables to numeric
    CCC_121 <- as.numeric(CCC_121)
    CCC_131 <- as.numeric(CCC_131)
    CCC_151 <- as.numeric(CCC_151)
    CCC_171 <- as.numeric(CCC_171)
    CCC_280 <- as.numeric(CCC_280)
    resp_condition <- as.numeric(resp_condition)
    CCC_051 <- as.numeric(CCC_051)
    
    # verify for NA
    CCC_121 <- if_else2(is.na(CCC_121), 0, CCC_121)
    CCC_131 <- if_else2(is.na(CCC_131), 0, CCC_131)
    CCC_151 <- if_else2(is.na(CCC_151), 0, CCC_151)
    CCC_171 <- if_else2(is.na(CCC_171), 0, CCC_171)
    CCC_280 <- if_else2(is.na(CCC_280), 0, CCC_280)
    resp_condition <- if_else2(is.na(resp_condition), 0, resp_condition)
    CCC_051 <- if_else2(is.na(CCC_051), 0, CCC_051)
    
    # adjust resp_condition to yes = 1, no = 2
    resp_condition <- if_else2(resp_condition %in% c(1:2), 1,
                               if_else2(resp_condition == 3, 2, 0))
    
    # Calculate number of conditions based on yes
    conditions <- (CCC_121%%2) + (CCC_131%%2) + (CCC_151%%2) +(CCC_171%%2) +
      (CCC_280%%2) + (resp_condition%%2) + (CCC_051%%2)
    
    if_else2(conditions>= 5, "5+", conditions)
  }