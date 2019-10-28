# Custom ifelse for evaluating NA
ifelse2 <- function(x, a, b) {
  falseifNA <- function(x) {
    ifelse(is.na(x), FALSE, x)
  }
  ifelse(falseifNA(x), a, b)
}

#BMI derived variable
BMI_derived <- 
  function(HWTGHTM, 
           HWTGWTK) {
    ifelse2((!is.na(HWTGHTM)) & (!is.na(HWTGWTK)), 
            (HWTGWTK/(HWTGHTM*HWTGHTM)), NA)
  }

#Smoking variables
TypeOfSmoker <- SMKDSTY 
Age_cont <- DHHGAGE_cont
stpd <- SMK_09A_B #former daily (quit <3 years ago)
stpdy <- SMKG09C #former daily (quit >=3 years ago)
agec1 <- SMKG01C_cont #age smoked first cigarette
agecigd <- SMKG203_cont #age started smoking daily
agecigfd <- SMKG207_cont #age started smoking daily (former daily)
cigdayd <- SMK_204 # number of cigarettes smoked per day (daily smoker)
cigdayo <- SMK_05B # number of cigarettes smoked per day (occasional smoker)
cigdayf <- SMK_208 # number of cigarettes smoked per day (former daily)
dayocc <- SMK_05C # number of days smoked at least one cigarette
s100 <- SMK_01A # smoked 100 cigarettes in lifetime (y/n)

#Smoking pack-years
PackYears_fun <-
  function(TypeOfSmoker, Age_cont, stpd, stpdy, agecigd, agecigfd, cigdayd, cigdayo, cigdayf, dayocc, agec1, s100) {
    #Time since quit for former daily smokers
    tsq_ds_fun <- function(stpd, stpdy) {
      stpdy <-
        ifelse2(stpdy==1, 4,
        ifelse2(stpdy==2, 8,
        ifelse2(stpdy==3, 12, NA)))
      tsq_ds <-
        ifelse2(stpd==1, 0.5,
        ifelse2(stpd==2, 1.5,
        ifelse2(stpd==3, 2.5,
        ifelse2(stpd==4, stpdy, NA))))
    }
    tsq_ds<-tsq_ds_fun(stpd, stpdy)
    # PackYears for Daily Smoker
    ifelse2(TypeOfSmoker==1, pmax(((Age_cont - agecigd)*(cigdayd/20)), 0.0137),
    # PackYears for Occasional Smoker (former daily)     
    ifelse2(TypeOfSmoker==2, pmax(((Age_cont - agecigfd - tsq_ds)*(cigdayf/20)), 0.0137) + (pmax((cigdayo*dayocc/30), 1)*tsq_ds),
    # PackYears for Occasional Smoker (never daily)      
    ifelse2(TypeOfSmoker==3, (pmax((cigdayo*dayocc/30), 1)/20)*(Age_cont - agec1),
    # PackYears for former daily smoker (non-smoker now)      
    ifelse2(TypeOfSmoker==4, pmax(((Age_cont - agecigfd - tsq_ds)*(cigdayf/20)), 0.0137),
    # PackYears for former occasional smoker (non-smoker now) who smoked at least 100 cigarettes lifetime      
    ifelse2(TypeOfSmoker==5 & s100==1, 0.0137,
    # PackYears for former occasional smoker (non-smoker now) who have not smoked at least 100 cigarettes lifetime      
    ifelse2(TypeOfSmoker==5 & s100==2, 0.007,
    # Non-smoker      
    ifelse2(TypeOfSmoker==6, 0, NA)))))))
  }

# Percent time in Canada
Age_cont <- DHHGAGE_cont
BirthCountry <- SDCGCBG
TimeCanada <- SDCGRES
Pct_time_fun <-
  function(Age_cont, BirthCountry, TimeCanada) {
    TimeCanada_fun <- function(TimeCanada) {
      ifelse2(TimeCanada == 1, 4.5,
              ifelse2(TimeCanada == 2, 15, NA))
    }
    TimeCanada <- TimeCanada_fun(TimeCanada)
    ifelse2(BirthCountry == 1, 100,
            ifelse2(BirthCountry == 2, (TimeCanada/Age_cont)*100, NA))
  }
