#ICES Working Data
library(cchsflow)
library(recodeflow)
library(tidyverse)
set.seed(123)

#2001 SURVEY CYCLE
#check for working variables from 2001 survey
cchs2001_p$DHHAGAGE
cchs2001_p$DHHA_SEX
cchs2001_p$HWTAGBMI
cchs2001_p$EDUADH04
cchs2001_p$DHHAGMS
cchs2001_p$SDCAFIMM
cchs2001_p$ALCADTYP
cchs2001_p$CCCA_071
cchs2001_p$CCCA_91B
cchs2001_p$CCCA_111
cchs2001_p$CCCA_121
cchs2001_p$CCCA_151
cchs2001_p$FVCADJUI
cchs2001_p$FVCADPOT
cchs2001_p$FVCADTOT
cchs2001_p$GENA_01
cchs2001_p$GENA_03
cchs2001_p$GENA_04
cchs2001_p$GENA_05
cchs2001_p$GENA_07
cchs2001_p$GENA_10
cchs2001_p$PACAFLEI
cchs2001_p$RACA_6D
cchs2001_p$SDCAGRAC
cchs2001_p$SMKA_05B
cchs2001_p$SMKA_05C
cchs2001_p$SMKA_09A
cchs2001_p$SMKA_208
cchs2001_p$SMKADSTY
cchs2001_p$SMKAG01C
cchs2001_p$SMKAG203
cchs2001_p$SMKAG207

#create subset with working variables only from 2001 survey
cchs2001_p_sub<-cchs2001_p[c("DHHAGAGE", "DHHA_SEX",
                             "HWTAGBMI", "EDUADH04", 
                             "DHHAGMS", "SDCAFIMM",
                             "ALCADTYP", "CCCA_071",
                             "CCCA_91B", "CCCA_111",
                             "CCCA_121", "CCCA_151",
                             "FVCADJUI", "FVCADPOT",
                             "GENA_01", "GENA_03",
                             "GENA_04", "GENA_05", 
                             "GENA_07", "GENA_10", 
                             "PACAFLEI", "RACA_6D", 
                             "SDCAGRAC", "SMKA_05B",
                             "SMKA_05C", "SMKA_09A",
                             "SMKA_208", "SMKADSTY",
                             "SMKAG01C", "SMKAG203",
                             "SMKAG207")]

cchs2001_p_sub2 <- cchs2001_p_sub |>
  #Creating continuous age variable in survey cycle by sampling random values 
  #from age range for specific age category
  mutate(DHHGAGE_cont = case_when(
    cchs2001_p_sub$DHHAGAGE==1~sample(c(12:14), length(cchs2001_p_sub$DHHAGAGE==1), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==2~sample(c(15:19), length(cchs2001_p_sub$DHHAGAGE==2), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==3~sample(c(20:24), length(cchs2001_p_sub$DHHAGAGE==3), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==4~sample(c(25:29), length(cchs2001_p_sub$DHHAGAGE==4), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==5~sample(c(30:34), length(cchs2001_p_sub$DHHAGAGE==5), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==6~sample(c(35:39), length(cchs2001_p_sub$DHHAGAGE==6), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==7~sample(c(40:44), length(cchs2001_p_sub$DHHAGAGE==7), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==8~sample(c(45:49), length(cchs2001_p_sub$DHHAGAGE==8), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==9~sample(c(50:54), length(cchs2001_p_sub$DHHAGAGE==9), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==10~sample(c(55:59), length(cchs2001_p_sub$DHHAGAGE==10), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==11~sample(c(60:64), length(cchs2001_p_sub$DHHAGAGE==11), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==12~sample(c(65:69), length(cchs2001_p_sub$DHHAGAGE==12), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==13~sample(c(70:74), length(cchs2001_p_sub$DHHAGAGE==13), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==14~sample(c(75:79), length(cchs2001_p_sub$DHHAGAGE==14), replace = TRUE),
    cchs2001_p_sub$DHHAGAGE==15~sample(c(80:102), length(cchs2001_p_sub$DHHAGAGE==15), replace = TRUE)))|>
  #Creating ADL score across survey cycle by sampling random values 
  #from DemPoRT score range 
  mutate(ADL_score_5 = sample(c(1:5), nrow(cchs2001_p_sub), replace = TRUE))|>
  #Recoding alcohol type 
  mutate(ALCDTMM = case_when(
    cchs2001_p_sub$ALCADTYP==1~1,
    cchs2001_p_sub$ALCADTYP==2~2,
    cchs2001_p_sub$ALCADTYP==3~3,
    cchs2001_p_sub$ALCADTYP==4~3))|>
  #Creating hearing variables 
  mutate(HUIGHER = sample(c(1:3), nrow(cchs2001_p_sub), replace = TRUE))|>
  mutate(HUI06 = sample(c(1:2), nrow(cchs2001_p_sub), replace = TRUE))|>
  mutate(HUI07 = sample(c(1:2), nrow(cchs2001_p_sub), replace = TRUE))|>
  mutate(HUI08 = sample(c(1:2), nrow(cchs2001_p_sub), replace = TRUE))|>
  mutate(HUI09 = sample(c(1:2), nrow(cchs2001_p_sub), replace = TRUE))|>
  #Creating pack years variable 
  mutate(packyears = case_when(
    cchs2001_p_sub$DHHA_SEX==1~sample(c(0:112), length(cchs2001_p_sub$DHHA_SEX==1), replace = TRUE),
    cchs2001_p_sub$DHHA_SEX==2~sample(c(0:78), length(cchs2001_p_sub$DHHA_SEX==2), replace = TRUE)))|>
  #Create ethnicity across survey cycle by sampling random values 
  #from DemPoRT ethnicity group range 
  mutate(SDCDCGT_B = sample(c(1:7), nrow(cchs2001_p_sub), replace = TRUE))|>
  #Create sleep variables across survey cycles
  mutate(SPLG01 = case_when(
    cchs2001_p_sub$GENA_03==1~1,
    cchs2001_p_sub$GENA_03==2~1,
    cchs2001_p_sub$GENA_03==3~2,
    cchs2001_p_sub$GENA_03==4~3,
    cchs2001_p_sub$GENA_03==5~4,
    cchs2001_p_sub$GENA_03==6~5,
    cchs2001_p_sub$GENA_03==7~6,
    cchs2001_p_sub$GENA_03==8~7,
    cchs2001_p_sub$GENA_03==9~8,
    cchs2001_p_sub$GENA_03==10~9,
    cchs2001_p_sub$GENA_03==11~10,
    cchs2001_p_sub$GENA_03==12~11))|>
  #Create necessary continuous smoking variables
  #SMKG203
  mutate(SMKG203_cont = case_when(
    cchs2001_p_sub$SMKAG203==1~8,
    cchs2001_p_sub$SMKAG203==2~13,
    cchs2001_p_sub$SMKAG203==3~17,
    cchs2001_p_sub$SMKAG203==4~22,
    cchs2001_p_sub$SMKAG203==5~27,
    cchs2001_p_sub$SMKAG203==6~32,
    cchs2001_p_sub$SMKAG203==7~37,
    cchs2001_p_sub$SMKAG203==8~42,
    cchs2001_p_sub$SMKAG203==9~47,
    cchs2001_p_sub$SMKAG203==10~55))|>
  #SMKG207
  mutate(SMKG207_cont = case_when(
    cchs2001_p_sub$SMKAG207==1~8,
    cchs2001_p_sub$SMKAG207==2~13,
    cchs2001_p_sub$SMKAG207==3~17,
    cchs2001_p_sub$SMKAG207==4~22,
    cchs2001_p_sub$SMKAG207==5~27,
    cchs2001_p_sub$SMKAG207==6~32,
    cchs2001_p_sub$SMKAG207==7~37,
    cchs2001_p_sub$SMKAG207==8~42,
    cchs2001_p_sub$SMKAG207==9~47,
    cchs2001_p_sub$SMKAG207==10~55))|>
  #SMKDTSY (6 categories to 5)
  mutate(SMKDSTY = case_when(
    cchs2001_p_sub$SMKADSTY==1~1,
    cchs2001_p_sub$SMKADSTY==2~2,
    cchs2001_p_sub$SMKADSTY==3~2,
    cchs2001_p_sub$SMKADSTY==4~3,
    cchs2001_p_sub$SMKADSTY==5~4,
    cchs2001_p_sub$SMKADSTY==6~5))
  #Renaming variables to most up-to-date versions 
  rename(DHHAGSEX == DHH_SEX)|>
  rename(HWTAGBMI == HWTGBMI)|>
  rename(EDUADH04 == EDUDH04)|>
  rename(DHHAGMS == DHHGMS)|>
  rename(GENA_01 == GEN_01)|>
  rename(CCCA_071 == CCC_071)|>
  rename(CCCA_91B = CCC_91)|>
  rename(CCCA_111 == CCC_111)|>
  rename(CCCA_121 == CCC_121)|>
  rename(CCCA_151 == CCC_151)|>
  rename(FVCADJUI == FVCDJUI)|>
  rename(FVCADPOT == FVCDPOT)|>
  rename(FVCADTOT == FVCDTOT)|>
  rename(GENA_04 == SLP_02)|>
  rename(GENA_05 == SLP_03)|>
  rename(GENA_07 == GEN_07)|>
  rename(GENA_10 == GEN_10)|>
  rename(PACAFLEI == PACFLEI)|>
  rename(RACA_6D == RAC_6D)|>
  rename(SDCAGRAC == SDCGCGT)|>
  rename(SMKA_05B == SMK_05B)|>
  rename(SMKA_05C == SMK_05C)|>
  rename(SMKA_09A == SMK_09A)|>
  rename(SMKA_208 == SMK_208)|>
  rename(SMKAG01C == SMKG01C)
  
#2003 SURVEY CYCLE
#check working variables from 2003 survey
cchs2003_p$DHHCGAGE
cchs2003_p$DHHC_SEX
cchs2003_p$HWTCGBMI
cchs2003_p$EDUCDH04
cchs2003_p$DHHCGMS
cchs2003_p$SDCCFIMM
cchs2003_p$ALCCDTYP
cchs2003_p$CCCC_071
cchs2003_p$CCCC_91B
cchs2003_p$CCCC_111
cchs2003_p$CCCC_121
cchs2003_p$CCCC_151
cchs2003_p$FVCCDJUI
cchs2003_p$FVCCDPOT
cchs2003_p$FVCCDTOT
cchs2003_p$GENC_01
cchs2003_p$GENC_07
cchs2003_p$GENC_10
cchs2003_p$PACCFLEI
cchs2003_p$RACC_6D
cchs2003_p$SDCCGRAC
cchs2003_p$RACC_6D
cchs2001_p$SDCCGRAC
cchs2003_p$SMKC_05B
cchs2003_p$SMKC_05C
cchs2003_p$SMKC_09A
cchs2003_p$SMKC_208
cchs2003_p$SMKCDSTY
cchs2003_p$SMKCG01C
cchs2003_p$SMKCG203
cchs2003_p$SMKCG207

#create subset with working variables only from 2003 survey
cchs2003_p_sub<-cchs2003_p[c("DHHCGAGE", "DHHC_SEX",
                             "HWTCGBMI", "EDUCDH04", 
                             "DHHCGMS", "SDCCFIMM",
                             "ALCCDTYP", "CCCC_071",
                             "CCCC_91B", "CCCC_111",
                             "CCCC_121", "CCCC_151",
                             "FVCCDJUI", "FVCCDPOT",
                             "GENC_01", "GENC_07", 
                             "GENC_10", "PACCFLEI",
                             "RACC_6D", "SDCCGRAC",
                             "SMKC_05B", "SMKC_05C",
                             "SMKC_09A", "SMKC_208",
                             "SMKCDSTY", "SMKCG01C",
                             "SMKCG203", "SMKCG207")]

cchs2003_p_sub <- cchs2003_p_sub |>
  #Creating continuous age variable in survey cycle by sampling random values 
  #from age range for specific age category
  mutate(DHHGAGE_cont = case_when(
    cchs2003_p_sub$DHHCGAGE==1~sample(c(12:14), length(cchs2003_p_sub$DHHCGAGE==1), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==2~sample(c(15:19), length(cchs2003_p_sub$DHHCGAGE==2), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==3~sample(c(20:24), length(cchs2003_p_sub$DHHCGAGE==3), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==4~sample(c(25:29), length(cchs2003_p_sub$DHHCGAGE==4), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==5~sample(c(30:34), length(cchs2003_p_sub$DHHCGAGE==5), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==6~sample(c(35:39), length(cchs2003_p_sub$DHHCGAGE==6), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==7~sample(c(40:44), length(cchs2003_p_sub$DHHCGAGE==7), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==8~sample(c(45:49), length(cchs2003_p_sub$DHHCGAGE==8), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==9~sample(c(50:54), length(cchs2003_p_sub$DHHCGAGE==9), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==10~sample(c(55:59), length(cchs2003_p_sub$DHHCGAGE==10), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==11~sample(c(60:64), length(cchs2003_p_sub$DHHCGAGE==11), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==12~sample(c(65:69), length(cchs2003_p_sub$DHHCGAGE==12), replace = TRUE),
    cchs2003_p_sub$DHHCAGAGE==13~sample(c(70:74), length(cchs2003_p_sub$DHHCGAGE==13), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==14~sample(c(75:79), length(cchs2003_p_sub$DHHCGAGE==14), replace = TRUE),
    cchs2003_p_sub$DHHCGAGE==15~sample(c(80:102), length(cchs2003_p_sub$DHHCGAGE==15), replace = TRUE)))|>
  #Creating ADL score across survey cycle by sampling random values 
  #from DemPoRT score range 
  mutate(ADL_score_6 = sample(c(1:6), nrow(cchs2003_p_sub), replace = TRUE))|>
  #Recoding alcohol type 
  mutate(ALCDTMM = case_when(
    cchs2003_p_sub$ALCCDTYP==1~1,
    cchs2003_p_sub$ALCCDTYP==2~2,
    cchs2003_p_sub$ALCCDTYP==3~3,
    cchs2003_p_sub$ALCCDTYP==4~3))|>
  #Creating hearing variables 
  mutate(HUIGHER = sample(c(1:3), nrow(cchs2003_p_sub), replace = TRUE))|>
  mutate(HUI06 = sample(c(1:2), nrow(cchs2003_p_sub), replace = TRUE))|>
  mutate(HUI07 = sample(c(1:2), nrow(cchs2003_p_sub), replace = TRUE))|>
  mutate(HUI08 = sample(c(1:2), nrow(cchs2003_p_sub), replace = TRUE))|>
  mutate(HUI09 = sample(c(1:2), nrow(cchs2003_p_sub), replace = TRUE))|>
  #Creating pack years variable 
  mutate(pack_years = case_when(
    cchs2003_p_sub$DHHC_SEX==1~sample(c(0:112), length(cchs2003_p_sub$DHHC_SEX==1), replace = TRUE),
    cchs2003_p_sub$DHHC_SEX==2~sample(c(0:78), length(cchs2003_p_sub$DHHC_SEX==2), replace = TRUE)))|>
  #Create ethnicity across survey cycle by sampling random values 
  #from DemPoRT ethnicity group range 
  mutate(SDCDCGT_B = sample(c(1:7), nrow(cchs2003_p_sub), replace = TRUE))|>
  # #Create sleep variables across survey cycles
  # mutate(SLP_02_A = sample(c(1:3), nrow(cchs2003_p_sub), replace = TRUE))|>
  # mutate(SLP_03_A = sample(c(1:3), nrow(cchs2003_p_sub), replace = TRUE))|>
  # mutate(SPLG01 = sample(c(1:11), nrow(cchs2003_p_sub), replace = TRUE))|>
  #Create necessary continuous smoking variables
  #SMKG203
  mutate(SMKG203_cont = case_when(
    cchs2003_p_sub$SMKCG203==1~8,
    cchs2003_p_sub$SMKCG203==2~13,
    cchs2003_p_sub$SMKCG203==3~17,
    cchs2003_p_sub$SMKCG203==4~22,
    cchs2003_p_sub$SMKCG203==5~27,
    cchs2003_p_sub$SMKCG203==6~32,
    cchs2003_p_sub$SMKCG203==7~37,
    cchs2003_p_sub$SMKCG203==8~42,
    cchs2003_p_sub$SMKCG203==9~47,
    cchs2003_p_sub$SMKCG203==10~55))|>
  #SMKG207
  mutate(SMKG207_cont = case_when(
    cchs2003_p_sub$SMKCG207==1~8,
    cchs2003_p_sub$SMKCG207==2~13,
    cchs2003_p_sub$SMKCG207==3~17,
    cchs2003_p_sub$SMKCG207==4~22,
    cchs2003_p_sub$SMKCG207==5~27,
    cchs2003_p_sub$SMKCG207==6~32,
    cchs2003_p_sub$SMKCG207==7~37,
    cchs2003_p_sub$SMKCG207==8~42,
    cchs2003_p_sub$SMKCG207==9~47,
    cchs2003_p_sub$SMKCG207==10~55))|>
  #SMKDTSY (6 categories to 5)
  mutate(SMKDSTY = case_when(
    cchs2003_p_sub$SMKCDSTY==1~1,
    cchs2003_p_sub$SMKCDTSY==2~2,
    cchs2003_p_sub$SMKCDTSY==3~2,
    cchs2003_p_sub$SMKCDSTY==4~3,
    cchs2003_p_sub$SMKCDSTY==5~4,
    cchs2003_p_sub$SMKCDSTY==6~5))|>
  #Renaming variables to most up-to-date versions 
  rename(DHHC_SEX == DHH_SEX)|>
  rename(HWTCGBMI == HWTGBMI)|>
  rename(EDUCDH04 == EDUDH04)|>
  rename(DHHCGMS == DHHGMS)|>
  rename(GENC_01 == GEN_01)|>
  rename(CCCC_071 == CCC_071)|>
  rename(CCCC_91B = CCC_91)|>
  rename(CCCC_111 == CCC_111)|>
  rename(CCCC_121 == CCC_121)|>
  rename(CCCC_151 == CCC_151)|>
  rename(FVCCDJUI == FVCDJUI)|>
  rename(FVCCDPOT == FVCDPOT)|>
  rename(FVCCDTOT == FVCDTOT)|>
  rename(GENC_07 == GEN_07)|>
  rename(GENC_10 == GEN_10)|>
  rename(PACCFLEI == PACFLEI)|>
  rename(RACC_6D == RAC_6D)|>
  rename(SDCCGRAC == SDCGCGT)|>
  rename(SMKC_05B == SMK_05B)|>
  rename(SMKC_05C == SMK_05C)|>
  rename(SMKC_09A == SMK_09A)|>
  rename(SMKC_208 == SMK_208)|>
  rename(SMKCDSTY == SMKDSTY)|>
  rename(SMKCG01C == SMKG01C)

#2005 SURVEY CYCLE
#check working variables from 2005 survey
cchs2005_p$DHHEGAGE
cchs2005_p$DHHE_SEX
cchs2005_p$HWTEGBMI
cchs2005_p$EDUEDH04
cchs2005_p$DHHEGMS
cchs2005_p$SDCEFIMM
cchs2005_p$ALCEDTYP
cchs2005_p$CCCE_071
cchs2005_p$CCCE_91F
cchs2005_p$CCCE_111
cchs2005_p$CCCE_121
cchs2005_p$CCCE_151
cchs2005_p$FVCEDJUI
cchs2005_p$FVCEDPOT
cchs2005_p$FVCEDTOT
cchs2005_p$GENE_01
cchs2005_p$GENE_07
cchs2005_p$GENE_10
cchs2005_p$PACEFLEI
cchs2005_p$RACE_6D
cchs2005_p$sdcegr
cchs2005_p$SDCEGCGT
cchs2003_p$SMKE_05B
cchs2003_p$SMKE_05C
cchs2003_p$SMKE_09A
cchs2003_p$SMKE_208
cchs2003_p$SMKEDSTY
cchs2003_p$SMKEG01C
cchs2005_p$SMKEG203
cchs2005_p$SMKEG207

#create subset with working variables only from 2005 survey
cchs2005_p_sub<-cchs2005_p[c("DHHEGAGE", "DHHE_SEX",
                             "HWTEGBMI", "EDUEDH04",
                             "DHHEGMS", "SDCEFIMM",
                             "ALCEDTYP", "CCCE_071",
                             "CCCE_91F", "CCCE_111",
                             "CCCE_121", "CCCE_151",
                             "FVCEDJUI", "FVCEDPOT",
                             "FVCEDTOT", "GENE_01",
                             "GENE_07", "GENE_10",
                             "PACEFLEI", "RACE_6D",
                             "SDCEGCGT", "SMKE_05B",
                             "SMKE_05C", "SMKE_09A",
                             "SMKE_208", "SMKEDSTY",
                             "SMKEG01C", "SMKEG203",
                             "SMKEG207")]

cchs2005_p <- cchs2005_p |>
  #Creating continuous age variable in survey cycle by sampling random values 
  #from age range for specific age category
  mutate(DHHGEGE_cont = case_when(
    cchs2005_p_sub$DHHEGAGE==1~sample(c(12:14), length(cchs2005_p_sub$DHHEGAGE==1), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==2~sample(c(15:17), length(cchs2005_p_sub$DHHEGAGE==2), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==3~sample(c(18:19), length(cchs2005_p_sub$DHHEGAGE==3), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==4~sample(c(20:24), length(cchs2005_p_sub$DHHEGAGE==4), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==5~sample(c(25:29), length(cchs2005_p_sub$DHHEGAGE==5), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==6~sample(c(30:34), length(cchs2005_p_sub$DHHEGAGE==6), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==7~sample(c(35:39), length(cchs2005_p_sub$DHHEGAGE==7), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==8~sample(c(40:44), length(cchs2005_p_sub$DHHEGAGE==8), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==9~sample(c(45:49), length(cchs2005_p_sub$DHHEGAGE==9), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==10~sample(c(50:54), length(cchs2005_p_sub$DHHEGAGE==10), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==11~sample(c(55:59), length(cchs2005_p_sub$DHHEGAGE==11), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==12~sample(c(60:64), length(cchs2005_p_sub$DHHEGAGE==12), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==13~sample(c(65:69), length(cchs2005_p_sub$DHHEGAGE==13), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==14~sample(c(70:74), length(cchs2005_p_sub$DHHEGAGE==14), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==15~sample(c(75:79), length(cchs2005_p_sub$DHHEGAGE==15), replace = TRUE),
    cchs2005_p_sub$DHHEGAGE==16~sample(c(80:102), length(cchs2005_p_sub$DHHEGAGE==16), replace = TRUE)))|>
  #Creating ADL score across survey cycle by sampling random values 
  #from DemPoRT score range 
  mutate(ADL_score_6 = sample(c(1:6), nrow(cchs2005_p_sub), replace = TRUE))|>
  #Recoding alcohol type 
  mutate(ALCDTMM = case_when(
  cchs2005_p_sub$ALCEDTYP==1~1,
  cchs2005_p_sub$ALCEDTYP==2~2,
  cchs2005_p_sub$ALCEDTYP==3~3,
  cchs2005_p_sub$ALCEDTYP==4~3))|>
  #Creating hearing variables 
  #mutate(HUIGHER = sample(c(1:3), nrow(cchs2005_p_sub), replace = TRUE))|>
  #mutate(HUI06 = sample(c(1:2), nrow(cchs2005_p_sub), replace = TRUE))|>
  #mutate(HUI07 = sample(c(1:2), nrow(cchs2005_p_sub), replace = TRUE))|>
  #mutate(HUI08 = sample(c(1:2), nrow(cchs2005_p_sub), replace = TRUE))|>
  #mutate(HUI09 = sample(c(1:2), nrow(cchs2005_p_sub), replace = TRUE))|>
  #Creating pack years variable 
  mutate(pack_years = case_when(
    cchs2005_p_sub$DHHE_SEX==1~sample(c(0:112), length(cchs2005_p_sub$DHHE_SEX==1), replace = TRUE),
    cchs2005_p_sub$DHHE_SEX==2~sample(c(0:78), length(cchs2005_p_sub$DHHE_SEX==2), replace = TRUE)))|>
  #Create ethnicity across survey cycle by sampling random values 
  #from DemPoRT ethnicity group range 
  mutate(SDCDCGT_B = sample(c(1:7), nrow(cchs2005_p_sub), replace = TRUE))|>
  # #Create sleep variables across survey cycles
  # mutate(SLP_02_A = sample(c(1:3), nrow(cchs2005_p_sub), replace = TRUE))|>
  # mutate(SLP_03_A = sample(c(1:3), nrow(cchs2005_p_sub), replace = TRUE))|>
  # mutate(SPLG01 = sample(c(1:11), nrow(cchs2005_p_sub), replace = TRUE))|>
  #Create necessary continuous smoking variables
  #SMKG203
  mutate(SMKG203_cont = case_when(
    cchs2005_p_sub$SMKEG203==1~8,
    cchs2005_p_sub$SMKEG203==2~13,
    cchs2005_p_sub$SMKEG203==3~16,
    cchs2005_p_sub$SMKEG203==4~18.5,
    cchs2005_p_sub$SMKEG203==5~22,
    cchs2005_p_sub$SMKEG203==6~27,
    cchs2005_p_sub$SMKEG203==7~32,
    cchs2005_p_sub$SMKEG203==8~37,
    cchs2005_p_sub$SMKEG203==9~42,
    cchs2005_p_sub$SMKEG203==10~47,
    cchs2005_p_sub$SMKEG203==11~55))|>
  #SMKG207
  mutate(SMKG207_cont = case_when(
    cchs2005_p_sub$SMKEG207==1~8,
    cchs2005_p_sub$SMKEG207==2~13,
    cchs2005_p_sub$SMKEG207==3~16,
    cchs2005_p_sub$SMKEG207==4~18.5,
    cchs2005_p_sub$SMKEG207==5~22,
    cchs2005_p_sub$SMKEG207==6~27,
    cchs2005_p_sub$SMKEG207==7~32,
    cchs2005_p_sub$SMKEG207==8~37,
    cchs2005_p_sub$SMKEG207==9~42,
    cchs2005_p_sub$SMKEG207==10~47,
    cchs2005_p_sub$SMKEG207==11~55))|>
  #SMKDTSY (6 categories to 5)
  mutate(SMKDSTY = case_when(
    cchs2005_p_sub$SMKEDSTY==1~1,
    cchs2005_p_sub$SMKEDTSY==2~2,
    cchs2005_p_sub$SMKEDTSY==3~2,
    cchs2005_p_sub$SMKEDSTY==4~3,
    cchs2005_p_sub$SMKEDSTY==5~4,
    cchs2005_p_sub$SMKEDSTY==6~5))|>
  #Renaming variables to detail sheet versions 
  rename(DHHE_SEX == DHH_SEX)|>
  rename(HWTEGBMI == HWTGBMI)|>
  rename(EDUEDH04 == EDUDH04)|>
  rename(DHHEGMS == DHHGMS)|>
  rename(GENE_01 == GEN_01)|>
  rename(CCCE_071 == CCC_071)|>
  rename(CCCE_91B = CCC_91)|>
  rename(CCCE_111 == CCC_111)|>
  rename(CCCE_121 == CCC_121)|>
  rename(CCCE_151 == CCC_151)|>
  rename(FVCEDJUI == FVCDJUI)|>
  rename(FVCEDPOT == FVCDPOT)|>
  rename(FVCEDTOT == FVCDTOT)|>
  rename(GENE_07 == GEN_07)|>
  rename(GENE_10 == GEN_10)|>
  rename(PACEFLEI == PACFLEI)|>
  rename(RACE_6D == RAC_6D)|>
  rename(SDCEGRAC == SDCGCGT)|>
  rename(SMKE_05B == SMK_05B)|>
  rename(SMKE_05C == SMK_05C)|>
  rename(SMKE_09A == SMK_09A)|>
  rename(SMKE_208 == SMK_208)|>
  rename(SMKEDSTY == SMKDSTY)|>
  rename(SMKEG01C == SMKG01C)

#2007-2008 SURVEY CYCLE
#pulling working variables from 2005 survey
cchs2007_2008_p$DHHGAGE
cchs2007_2008_p$DHH_SEX
cchs2007_2008_p$HWTGBMI
cchs2007_2008_p$EDUDR04
cchs2007_2008_p$DHHGMS
cchs2007_2008_p$SDCFIMM
cchs2007_2008_p$ALCDTTM
cchs2007_2008_p$CCC_071
cchs2007_2008_p$CCC_91F
cchs2007_2008_p$CCC_121
cchs2007_2008_p$CCC_151
cchs2007_2008_p$FVCDJUI
cchs2007_2008_p$FVCDPOT
cchs2007_2008_p$FVCDTOT
cchs2007_2008_p$GEN_01
cchs2007_2008_p$GEN_07
cchs2007_2008_p$GEN_10
cchs2007_2008_p$PACFLEI
cchs2007_2008_p$SDCGCGT
cchs2007_2008_p$SMK_05B
cchs2007_2008_p$SMK_05C
cchs2007_2008_p$SMK_09A
cchs2007_2008_p$SMK_208
cchs2007_2008_p$SMKDSTY
cchs2007_2008_p$SMKG01C
cchs2007_2008_p$SMKG203
cchs2007_2008_p$SMKG207

#create subset with working variables only from 2007-2008 survey
cchs2007_2008_p_sub<-cchs2007_2008_p[c("DHHGAGE", "DHH_SEX",
                             "HWTGBMI", "EDUDR04", 
                             "DHHGMS", "SDCFIMM",
                             "ALCDTTM", "CCC_071",
                             "CCC_91F", "CCC_121", 
                             "CCC_151", "FVCDJUI", 
                             "FVCDPOT", "GEN_01", 
                             "GEN_07", "GEN_10", 
                             "PACFLEI", "SDCGCGT",
                             "SMK_05B", "SMK_05C", 
                             "SMK_09A","SMK_208",
                             "SMKDSTY", "SMKG01C",
                             "SMKG203", "SMKG207")]

cchs2007_2008_p_sub <- cchs2007_2008_p_sub |>
  #Creating continuous age variable in survey cycle by sampling random values 
  #from age range for specific age category
  mutate(DHHGEGE_cont = case_when(
    cchs2007_2008_p_sub$DHHGAGE==1~sample(c(12:14), length(cchs2007_2008_p_sub$DHHGAGE==1), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==2~sample(c(15:17), length(cchs2007_2008_p_sub$DHHGAGE==2), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==3~sample(c(18:19), length(cchs2007_2008_p_sub$DHHGAGE==3), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==4~sample(c(20:24), length(cchs2007_2008_p_sub$DHHGAGE==4), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==5~sample(c(25:29), length(cchs2007_2008_p_sub$DHHGAGE==5), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==6~sample(c(30:34), length(cchs2007_2008_p_sub$DHHGAGE==6), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==7~sample(c(35:39), length(cchs2007_2008_p_sub$DHHGAGE==7), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==8~sample(c(40:44), length(cchs2007_2008_p_sub$DHHGAGE==8), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==9~sample(c(45:49), length(cchs2007_2008_p_sub$DHHGAGE==9), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==10~sample(c(50:54), length(cchs2007_2008_p_sub$DHHGAGE==10), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==11~sample(c(55:59), length(cchs2007_2008_p_sub$DHHGAGE==11), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==12~sample(c(60:64), length(cchs2007_2008_p_sub$DHHGAGE==12), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==13~sample(c(65:69), length(cchs2007_2008_p_sub$DHHGAGE==13), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==14~sample(c(70:74), length(cchs2007_2008_p_sub$DHHGAGE==14), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==15~sample(c(75:79), length(cchs2007_2008_p_sub$DHHGAGE==15), replace = TRUE),
    cchs2007_2008_p_sub$DHHGAGE==16~sample(c(80:102), length(cchs2007_2008_p_sub$DHHGAGE==16), replace = TRUE)))|>
  #Creating ADL score across survey cycle by sampling random values 
  #from DemPoRT score range 
  mutate(ADL_score_6 = sample(c(1:6), nrow(cchs2007_2008_p_sub), replace = TRUE))|>
  #Creating hearing variables 
  #mutate(HUIGHER = sample(c(1:3), nrow(cchs2007_2008_p_sub), replace = TRUE))|>
  #mutate(HUI06 = sample(c(1:2), nrow(cchs2007_2008_p_sub), replace = TRUE))|>
  #mutate(HUI07 = sample(c(1:2), nrow(cchs2007_2008_p_sub), replace = TRUE))|>
  #mutate(HUI08 = sample(c(1:2), nrow(cchs2007_2008_p_sub), replace = TRUE))|>
  #mutate(HUI09 = sample(c(1:2), nrow(cchs2007_2008_p_sub), replace = TRUE))|>
  #Creating pack years variable 
  mutate(pack_years = case_when(
    cchs2007_2008_p_sub$DHH_SEX==1~sample(c(0:112), length(cchs2007_2008_p_sub$DHH_SEX==1), replace = TRUE),
    cchs2007_2008_p_sub$DHH_SEX==2~sample(c(0:78), length(cchs2007_2008_p_sub$DHH_SEX==2), replace = TRUE)))|>
  #Create ethnicity across survey cycle by sampling random values 
  #from DemPoRT ethnicity group range 
  mutate(SDCDCGT_B = sample(c(1:7), nrow(cchs2007_2008_p_sub), replace = TRUE))
  #Create sleep variables across survey cycles
  #mutate(SLP_02 = sample(c(1:5), nrow(cchs2007_2008_p_sub), replace = TRUE))|>
  #mutate(SLP_03 = sample(c(1:5), nrow(cchs2007_2008_p_sub), replace = TRUE))|>
  #mutate(SPLG01 = sample(c(1:11), nrow(cchs2007_2008_p_sub), replace = TRUE))|>
  #Create necessary continuous smoking variables
  #SMKG203
  #SMKG203
  mutate(SMKG203_cont = case_when(
    cchs2007_2008_p_sub$SMKG203==1~8,
    cchs2007_2008_p_sub$SMKG203==2~13,
    cchs2007_2008_p_sub$SMKG203==3~16,
    cchs2007_2008_p_sub$SMKG203==4~18.5,
    cchs2007_2008_p_sub$SMKG203==5~22,
    cchs2007_2008_p_sub$SMKG203==6~27,
    cchs2007_2008_p_sub$SMKG203==7~32,
    cchs2007_2008_p_sub$SMKG203==8~37,
    cchs2007_2008_p_sub$SMKG203==9~42,
    cchs2007_2008_p_sub$SMKG203==10~47,
    cchs2007_2008_p_sub$SMKG203==11~55))|>
  #SMKG207
  mutate(SMKG207_cont = case_when(
    cchs2007_2008_p_sub$SMKG207==1~8,
    cchs2007_2008_p_sub$SMKG207==2~13,
    cchs2007_2008_p_sub$SMKG207==3~16,
    cchs2007_2008_p_sub$SMKG207==4~18.5,
    cchs2007_2008_p_sub$SMKG207==5~22,
    cchs2007_2008_p_sub$SMKG207==6~27,
    cchs2007_2008_p_sub$SMKG207==7~32,
    cchs2007_2008_p_sub$SMKG207==8~37,
    cchs2007_2008_p_sub$SMKG207==9~42,
    cchs2007_2008_p_sub$SMKG207==10~47,
    cchs2007_2008_p_sub$SMKG207==11~55))|>
  #SMKDTSY (6 categories to 5)
  mutate(SMKDSTY = case_when(
    cchs2007_2008_p_sub$SMKDSTY==1~1,
    cchs2007_2008_p_sub$SMKDTSY==2~2,
    cchs2007_2008_p_sub$SMKDTSY==3~2,
    cchs2007_2008_p_sub$SMKDSTY==4~3,
    cchs2007_2008_p_sub$SMKDSTY==5~4,
    cchs2007_2008_p_sub$SMKDSTY==6~5))

#2009-2010 SURVEY CYCLE
#pulling working variables from 2009-2010 survey
cchs2009_2010_p$DHHGAGE
cchs2009_2010_p$DHH_SEX
cchs2009_2010_p$HWTGBMI
cchs2009_2010_p$EDUDR04
cchs2009_2010_p$DHHGMS
cchs2009_2010_p$SDCFIMM
cchs2009_2010_p$ALCDTTM
cchs2009_2010_p$CCC_071
#cchs2009_2010_p$CCC_91F
cchs2009_2010_p$CCC_121
cchs2009_2010_p$CCC_151
cchs2009_2010_p$FVCDJUI
cchs2009_2010_p$FVCDPOT
cchs2009_2010_p$FVCDTOT
cchs2009_2010_p$GEN_01
cchs2009_2010_p$GEN_07
cchs2009_2010_p$GEN_10
cchs2009_2010_p$PACFLEI
cchs2009_2010_p$SDCGCGT
cchs2009_2010_p$SMK_05B
cchs2009_2010_p$SMK_05C
cchs2009_2010_p$SMK_09A
cchs2009_2010_p$SMK_208
cchs2009_2010_p$SMKDSTY
cchs2009_2010_p$SMKG01C
cchs2009_2010_p$SMKG203
cchs2009_2010_p$SMKG207
cchs2009_2010_p$HUIGHER
  
#create subset with working variables only from 2007-2008 survey
cchs2009_2010_p_sub<-cchs2009_2010_p[c("DHHGAGE", "DHH_SEX",
                                         "HWTGBMI", "EDUDR04", 
                                         "DHHGMS", "SDCFIMM",
                                         "ALCDTTM", "CCC_071",
                                         "CCC_121", "CCC_151", 
                                         "FVCDJUI", "FVCDPOT", 
                                         "GEN_01", "GEN_07",
                                         "GEN_10", "PACFLEI", 
                                         "SDCGCGT", "SMK_05B", 
                                         "SMK_05C", "SMK_09A", 
                                         "SMK_208", "SMKDSTY", 
                                         "SMKG01C", "SMKG203", 
                                         "SMKG207", "HUIGHER")]
  
cchs2009_2010_p_sub <- cchs2009_2010_p_sub |>
  #Creating continuous age variable in survey cycle by sampling random values 
  #from age range for specific age category
  mutate(DHHGEGE_cont = case_when(
    cchs2009_2010_p_sub$DHHGAGE==1~sample(c(12:14), length(cchs2009_2010_p_sub$DHHGAGE==1), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==2~sample(c(15:17), length(cchs2009_2010_p_sub$DHHGAGE==2), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==3~sample(c(18:19), length(cchs2009_2010_p_sub$DHHGAGE==3), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==4~sample(c(20:24), length(cchs2009_2010_p_sub$DHHGAGE==4), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==5~sample(c(25:29), length(cchs2009_2010_p_sub$DHHGAGE==5), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==6~sample(c(30:34), length(cchs2009_2010_p_sub$DHHGAGE==6), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==7~sample(c(35:39), length(cchs2009_2010_p_sub$DHHGAGE==7), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==8~sample(c(40:44), length(cchs2009_2010_p_sub$DHHGAGE==8), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==9~sample(c(45:49), length(cchs2009_2010_p_sub$DHHGAGE==9), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==10~sample(c(50:54), length(cchs2009_2010_p_sub$DHHGAGE==10), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==11~sample(c(55:59), length(cchs2009_2010_p_sub$DHHGAGE==11), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==12~sample(c(60:64), length(cchs2009_2010_p_sub$DHHGAGE==12), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==13~sample(c(65:69), length(cchs2009_2010_p_sub$DHHGAGE==13), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==14~sample(c(70:74), length(cchs2009_2010_p_sub$DHHGAGE==14), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==15~sample(c(75:79), length(cchs2009_2010_p_sub$DHHGAGE==15), replace = TRUE),
    cchs2009_2010_p_sub$DHHGAGE==16~sample(c(80:102), length(cchs2009_2010_p_sub$DHHGAGE==16), replace = TRUE)))|>
  #Creating ADL score across survey cycle by sampling random values 
  #from DemPoRT score range 
  mutate(ADL_score_6 = sample(c(1:6), nrow(cchs2009_2010_p_sub), replace = TRUE))|>
  #Creating hearing variables 
  mutate(HUI06 = sample(c(1:2), nrow(cchs2009_2010_p_sub), replace = TRUE))|>
  mutate(HUI07 = sample(c(1:2), nrow(cchs2009_2010_p_sub), replace = TRUE))|>
  mutate(HUI08 = sample(c(1:2), nrow(cchs2009_2010_p_sub), replace = TRUE))|>
  mutate(HUI09 = sample(c(1:2), nrow(cchs2009_2010_p_sub), replace = TRUE))|>
  #Creating pack years variable 
  mutate(pack_years = case_when(
    cchs2009_2010_p_sub$DHH_SEX==1~sample(c(0:112), length(cchs2009_2010_p_sub$DHH_SEX==1), replace = TRUE),
    cchs2009_2010_p_sub$DHH_SEX==2~sample(c(0:78), length(cchs2009_2010_p_sub$DHH_SEX==2), replace = TRUE)))|>
  #Create ethnicity across survey cycle by sampling random values 
  #from DemPoRT ethnicity group range 
  mutate(SDCDCGT_B = sample(c(1:7), nrow(cchs2009_2010_p_sub), replace = TRUE))
  #Create sleep variables across survey cycles
  #mutate(SLP_02 = sample(c(1:5), nrow(cchs2009_2010_p_sub), replace = TRUE))|>
  #mutate(SLP_03 = sample(c(1:5), nrow(cchs2009_2010_p_sub), replace = TRUE))|>
  #mutate(SPLG01_C = sample(c(1:12), nrow(cchs2009_2010_p_sub), replace = TRUE))|>
  #Create necessary continuous smoking variables
  #SMKG203
  mutate(SMKG203_cont = case_when(
    cchs2009_2010_p_sub$SMKG203==1~8,
    cchs2009_2010_p_sub$SMKG203==2~13,
    cchs2009_2010_p_sub$SMKG203==3~16,
    cchs2009_2010_p_sub$SMKG203==4~18.5,
    cchs2009_2010_p_sub$SMKG203==5~22,
    cchs2009_2010_p_sub$SMKG203==6~27,
    cchs2009_2010_p_sub$SMKG203==7~32,
    cchs2009_2010_p_sub$SMKG203==8~37,
    cchs2009_2010_p_sub$SMKG203==9~42,
    cchs2009_2010_p_sub$SMKG203==10~47,
    cchs2009_2010_p_sub$SMKG203==11~55))|>
  #SMKG207
  mutate(SMKG207_cont = case_when(
    cchs2009_2010_p_sub$SMKG207==1~8,
    cchs2009_2010_p_sub$SMKG207==2~13,
    cchs2009_2010_p_sub$SMKG207==3~16,
    cchs2009_2010_p_sub$SMKG207==4~18.5,
    cchs2009_2010_p_sub$SMKG207==5~22,
    cchs2009_2010_p_sub$SMKG207==6~27,
    cchs2009_2010_p_sub$SMKG207==7~32,
    cchs2009_2010_p_sub$SMKG207==8~37,
    cchs2009_2010_p_sub$SMKG207==9~42,
    cchs2009_2010_p_sub$SMKG207==10~47,
    cchs2009_2010_p_sub$SMKG207==11~55))|>
  #SMKDTSY (6 categories to 5)
  mutate(SMKDSTY = case_when(
    cchs2009_2010_p_sub$SMKDSTY==1~1,
    cchs2009_2010_p_sub$SMKDTSY==2~2,
    cchs2009_2010_p_sub$SMKDTSY==3~2,
    cchs2009_2010_p_sub$SMKDSTY==4~3,
    cchs2009_2010_p_sub$SMKDSTY==5~4,
    cchs2009_2010_p_sub$SMKDSTY==6~5))

#2011-2012 SURVEY CYCLE
#pulling working variables from 2005 survey
cchs2011_2012_p$DHHGAGE
cchs2011_2012_p$DHH_SEX
cchs2011_2012_p$HWTGBMI
cchs2011_2012_p$EDUDR04
cchs2011_2012_p$DHHGMS
cchs2011_2012_p$SDCFIMM
cchs2011_2012_p$ALCDTTM
cchs2011_2012_p$CCC_071
cchs2011_2012_p$CCC_91F
cchs2011_2012_p$CCC_121
cchs2011_2012_p$CCC_151
cchs2011_2012_p$FVCDJUI
cchs2011_2012_p$FVCDPOT
cchs2011_2012_p$FVCDTOT
cchs2011_2012_p$GEN_01
cchs2011_2012_p$GEN_07
cchs2011_2012_p$GEN_10
cchs2011_2012_p$PACFLEI
cchs2011_2012_p$SDCGCGT
cchs2011_2012_p$SMK_05B
cchs2011_2012_p$SMK_05C
cchs2011_2012_p$SMK_09A
cchs2011_2012_p$SMK_208
cchs2011_2012_p$SMKDSTY
cchs2011_2012_p$SMKG01C
cchs2011_2012_p$SMKG203
cchs2011_2012_p$SMKG208
cchs2011_2012_p$SLP_02
cchs2011_2012_p$SLP_03
cchs2011_2012_p$SLPG01

cchs20011_2012_p_sub<-cchs2011_2012_p[c("DHHGAGE", "DHH_SEX",
                                       "HWTGBMI", "EDUDR04", 
                                       "DHHGMS", "SDCFIMM",
                                       "ALCDTTM", "CCC_071",
                                       "CCC_121", "CCC_151", 
                                       "FVCDJUI", "FVCDPOT", 
                                       "GEN_01", "GEN_07", 
                                       "GEN_10", "PACFLEI", 
                                       "SDCGCGT", "SMK_05B", 
                                       "SMK_05C", "SMK_09A", 
                                       "SMK_208", "SMKDSTY", 
                                       "SMKG01C", "SMKG203", 
                                       "SMKG207", "SLP_02", 
                                       "SLP_03", "SLPG01")]

cchs2011_2012_p_sub <- cchs2011_2012_p_sub |>
  #Creating continuous age variable in survey cycle by sampling random values 
  #from age range for specific age category
  mutate(DHHGEGE_cont = case_when(
    cchs2011_2012_p_sub$DHHEGAGE==1~sample(c(12:14), length(cchs2011_2012_p_sub$DHHEGAGE==1), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==2~sample(c(15:17), length(cchs2011_2012_p_sub$DHHEGAGE==2), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==3~sample(c(18:19), length(cchs2011_2012_p_sub$DHHEGAGE==3), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==4~sample(c(20:24), length(cchs2011_2012_p_sub$DHHEGAGE==4), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==5~sample(c(25:29), length(cchs2011_2012_p_sub$DHHEGAGE==5), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==6~sample(c(30:34), length(cchs2011_2012_p_sub$DHHEGAGE==6), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==7~sample(c(35:39), length(cchs2011_2012_p_sub$DHHEGAGE==7), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==8~sample(c(40:44), length(cchs2011_2012_p_sub$DHHEGAGE==8), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==9~sample(c(45:49), length(cchs2011_2012_p_sub$DHHEGAGE==9), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==10~sample(c(50:54), length(cchs2011_2012_p_sub$DHHEGAGE==10), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==11~sample(c(55:59), length(cchs2011_2012_p_sub$DHHEGAGE==11), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==12~sample(c(60:64), length(cchs2011_2012_p_sub$DHHEGAGE==12), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==13~sample(c(65:69), length(cchs2011_2012_p_sub$DHHEGAGE==13), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==14~sample(c(70:74), length(cchs2011_2012_p_sub$DHHEGAGE==14), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==15~sample(c(75:79), length(cchs2011_2012_p_sub$DHHEGAGE==15), replace = TRUE),
    cchs2011_2012_p_sub$DHHEGAGE==16~sample(c(80:102), length(cchs2011_2012_p_sub$DHHEGAGE==16), replace = TRUE)))|>
  #Creating ADL score across survey cycle by sampling random values 
  #from DemPoRT score range 
  mutate(ADL_score_6 = sample(c(1:6), nrow(cchs20011_2012_p_sub), replace = TRUE))|>
  #Creating hearing variables 
  #mutate(HUIGHER = sample(c(1:3), nrow(cchs2011_2012_p_sub), replace = TRUE))|>
  # mutate(HUI06 = sample(c(1:2), nrow(cchs2011_2012_p_sub), replace = TRUE))|>
  # mutate(HUI07 = sample(c(1:2), nrow(cchs2011_2012_p_sub), replace = TRUE))|>
  # mutate(HUI08 = sample(c(1:2), nrow(cchs2011_2012_p_sub), replace = TRUE))|>
  # mutate(HUI09 = sample(c(1:2), nrow(cchs2011_2012_p_sub), replace = TRUE))|>
  #Creating pack years variable 
  mutate(pack_years = case_when(
    cchs2011_2012_p_sub$DHH_SEX==1~sample(c(0:112), length(cchs2011_2012_p_sub$DHH_SEX==1), replace = TRUE),
    cchs2011_2012_p_sub$DHH_SEX==2~sample(c(0:78), length(cchs2011_2012_p_sub$DHH_SEX==2), replace = TRUE)))|>
  #Create ethnicity across survey cycle by sampling random values 
  #from DemPoRT ethnicity group range 
  mutate(SDCDCGT_B = sample(c(1:7), nrow(cchs2011_2012_p_sub), replace = TRUE))|>
  #Create sleep variables across survey cycles
  #SLP_02
  mutate(SLP_02 = case_when(
    cchs2011_2012_p_sub$SLP_02==1~1,
    cchs2011_2012_p_sub$SLP_02==2~2,
    cchs2011_2012_p_sub$SLP_02==3~2,
    cchs2011_2012_p_sub$SLP_02==4~3,
    cchs2011_2012_p_sub$SLP_02==5~3))|>
  #SLP_03
  mutate(SLP_03 = case_when(
      cchs2011_2012_p_sub$SLP_03==1~1,
      cchs2011_2012_p_sub$SLP_03==2~2,
      cchs2011_2012_p_sub$SLP_03==3~2,
      cchs2011_2012_p_sub$SLP_03==4~3,
      cchs2011_2012_p_sub$SLP_03==5~3))|>
  #Create necessary continuous smoking variables
  #SMKG203
  mutate(SMKG203_cont = case_when(
    cchs2011_2012_p_sub$SMKG203==1~8,
    cchs2011_2012_p_sub$SMKG203==2~13,
    cchs2011_2012_p_sub$SMKG203==3~16,
    cchs2011_2012_p_sub$SMKG203==4~18.5,
    cchs2011_2012_p_sub$SMKG203==5~22,
    cchs2011_2012_p_sub$SMKG203==6~27,
    cchs2011_2012_p_sub$SMKG203==7~32,
    cchs2011_2012_p_sub$SMKG203==8~37,
    cchs2011_2012_p_sub$SMKG203==9~42,
    cchs2011_2012_p_sub$SMKG203==10~47,
    cchs2011_2012_p_sub$SMKG203==11~55))|>
  #SMKG207
  mutate(SMKG207_cont = case_when(
    cchs2011_2012_p_sub$SMKG207==1~8,
    cchs2011_2012_p_sub$SMKG207==2~13,
    cchs2011_2012_p_sub$SMKG207==3~16,
    cchs2011_2012_p_sub$SMKG207==4~18.5,
    cchs2011_2012_p_sub$SMKG207==5~22,
    cchs2011_2012_p_sub$SMKG207==6~27,
    cchs2011_2012_p_sub$SMKG207==7~32,
    cchs2011_2012_p_sub$SMKG207==8~37,
    cchs2011_2012_p_sub$SMKG207==9~42,
    cchs2011_2012_p_sub$SMKG207==10~47,
    cchs2011_2012_p_sub$SMKG207==11~55))|>
  #SMKDTSY (6 categories to 5)
  mutate(SMKDSTY = case_when(
    cchs2011_2012_p_sub$SMKDSTY==1~1,
    cchs2011_2012_p_sub$SMKDTSY==2~2,
    cchs2011_2012_p_sub$SMKDTSY==3~2,
    cchs2011_2012_p_sub$SMKDSTY==4~3,
    cchs2011_2012_p_sub$SMKDSTY==5~4,
    cchs2011_2012_p_sub$SMKDSTY==6~5))
  
#2013-2014 SURVEY CYCLE
#pulling working variables from 2005 survey
cchs2013_2014_p$DHHGAGE
cchs2013_2014_p$DHH_SEX
cchs2013_2014_p$HWTGBMI
cchs2013_2014_p$EDUDR04
cchs2013_2014_p$DHHGMS
cchs2013_2014_p$SDCFIMM
cchs2013_2014_p$ALCDTTM
cchs2013_2014_p$CCC_071
cchs2013_2014_p$CCC_91F
cchs2013_2014_p$CCC_121
cchs2013_2014_p$CCC_151
cchs2013_2014_p$FVCDJUI
cchs2013_2014_p$FVCDPOT
cchs2013_2014_p$FVCDTOT
cchs2013_2014_p$GEN_01
cchs2013_2014_p$GEN_07
cchs2013_2014_p$GEN_10
cchs2013_2014_p$PACFLEI
cchs2013_2014_p$SDCGCGT
cchs2013_2014_p$SMK_05B
cchs2013_2014_p$SMK_05C
cchs2013_2014_p$SMK_09A
cchs2013_2014_p$SMK_208
cchs2013_2014_p$SMKDSTY
cchs2013_2014_p$SMKG01C
cchs2013_2014_p$SMKG203
cchs2013_2014_p$SMKG207
cchs2013_2014_p$SLP_02
cchs2013_2014_p$SLP_03
cchs2013_2014_p$SLPG01
cchs2013_2014_p$HUIGHER

cchs2013_2014_p_sub<-cchs2013_2014_p[c("DHHGAGE", "DHH_SEX",
                                        "HWTGBMI", "EDUDR04", 
                                        "DHHGMS", "SDCFIMM",
                                        "ALCDTTM", "CCC_071",
                                        "CCC_121", "CCC_151", 
                                        "FVCDJUI", "FVCDPOT", 
                                        "GEN_01", "GEN_07", 
                                        "GEN_10", "PACFLEI", 
                                        "SDCGCGT", "SMK_05B", 
                                        "SMK_05C", "SMK_09A", 
                                        "SMK_208", "SMKDSTY", 
                                        "SMKG01C", "SMKG203", 
                                        "SMKG207", "SLP_02", 
                                        "SLP_03", "SLPG01", 
                                        "HUIGHER")]


cchs2013_2014_p_sub <- cchs2013_2014_p_sub |>
  #Creating continuous age variable in survey cycle by sampling random values 
  #from age range for specific age category
  mutate(DHHGEGE_cont = case_when(
    cchs2013_2014_p_sub$DHHEGAGE==1~sample(c(12:14), length(cchs2013_2014_p_sub$DHHEGAGE==1), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==2~sample(c(15:17), length(cchs2013_2014_p_sub$DHHEGAGE==2), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==3~sample(c(18:19), length(cchs2013_2014_p_sub$DHHEGAGE==3), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==4~sample(c(20:24), length(cchs2013_2014_p_sub$DHHEGAGE==4), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==5~sample(c(25:29), length(cchs2013_2014_p_sub$DHHEGAGE==5), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==6~sample(c(30:34), length(cchs2013_2014_p_sub$DHHEGAGE==6), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==7~sample(c(35:39), length(cchs2013_2014_p_sub$DHHEGAGE==7), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==8~sample(c(40:44), length(cchs2013_2014_p_sub$DHHEGAGE==8), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==9~sample(c(45:49), length(cchs2013_2014_p_sub$DHHEGAGE==9), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==10~sample(c(50:54), length(cchs2013_2014_p_sub$DHHEGAGE==10), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==11~sample(c(55:59), length(cchs2013_2014_p_sub$DHHEGAGE==11), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==12~sample(c(60:64), length(cchs2013_2014_p_sub$DHHEGAGE==12), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==13~sample(c(65:69), length(cchs2013_2014_p_sub$DHHEGAGE==13), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==14~sample(c(70:74), length(cchs2013_2014_p_sub$DHHEGAGE==14), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==15~sample(c(75:79), length(cchs2013_2014_p_sub$DHHEGAGE==15), replace = TRUE),
    cchs2013_2014_p_sub$DHHEGAGE==16~sample(c(80:102), length(cchs2013_2014_p_sub$DHHEGAGE==16), replace = TRUE)))|>
  #Creating ADL score across survey cycle by sampling random values 
  #from DemPoRT score range 
  mutate(ADL_score_6 = sample(c(1:6), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  #Creating hearing variables 
  mutate(HUI06 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  mutate(HUI07 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  mutate(HUI08 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  mutate(HUI09 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  #Creating pack years variable
  mutate(pack_years = case_when(
    cchs2013_2014_p_sub$DHH_SEX==1~sample(c(0:112), length(cchs2013_2014_p_sub$DHH_SEX==1), replace = TRUE),
    cchs2013_2014_p_sub$DHH_SEX==2~sample(c(0:78), length(cchs2013_2014_p_sub$DHH_SEX==2), replace = TRUE)))|>
  #Create ethnicity across survey cycle by sampling random values 
  #from DemPoRT ethnicity group range 
  mutate(SDCDCGT_B = sample(c(1:7), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  #Create sleep variables across survey cycles
  #SLP_02
  mutate(SLP_02 = case_when(
    cchs2013_2014_p_sub$SLP_02==1~1,
    cchs2013_2014_p_sub$SLP_02==2~2,
    cchs2013_2014_p_sub$SLP_02==3~2,
    cchs2013_2014_p_sub$SLP_02==4~3,
    cchs2013_2014_p_sub$SLP_02==5~3))|>
  #SLP_03
  mutate(SLP_03 = case_when(
    cchs2013_2014_p_sub$SLP_03==1~1,
    cchs2013_2014_p_sub$SLP_03==2~2,
    cchs2013_2014_p_sub$SLP_03==3~2,
    cchs2013_2014_p_sub$SLP_03==4~3,
    cchs2013_2014_p_sub$SLP_03==5~3))|>
  #Create necessary continuous smoking variables
  #SMKG203
  mutate(SMKG203_cont = case_when(
    cchs2013_2014_p_sub$SMKG203==1~8,
    cchs2013_2014_p_sub$SMKG203==2~13,
    cchs2013_2014_p_sub$SMKG203==3~16,
    cchs2013_2014_p_sub$SMKG203==4~18.5,
    cchs2013_2014_p_sub$SMKG203==5~22,
    cchs2013_2014_p_sub$SMKG203==6~27,
    cchs2013_2014_p_sub$SMKG203==7~32,
    cchs2013_2014_p_sub$SMKG203==8~37,
    cchs2013_2014_p_sub$SMKG203==9~42,
    cchs2013_2014_p_sub$SMKG203==10~47,
    cchs2013_2014_p_sub$SMKG203==11~55))|>
  #SMKG207
  mutate(SMKG207_cont = case_when(
    cchs2013_2014_p_sub$SMKG207==1~8,
    cchs2013_2014_p_sub$SMKG207==2~13,
    cchs2013_2014_p_sub$SMKG207==3~16,
    cchs2013_2014_p_sub$SMKG207==4~18.5,
    cchs2013_2014_p_sub$SMKG207==5~22,
    cchs2013_2014_p_sub$SMKG207==6~27,
    cchs2013_2014_p_sub$SMKG207==7~32,
    cchs2013_2014_p_sub$SMKG207==8~37,
    cchs2013_2014_p_sub$SMKG207==9~42,
    cchs2013_2014_p_sub$SMKG207==10~47,
    cchs2013_2014_p_sub$SMKG207==11~55))|>
  #SMKDTSY (6 categories to 5)
  mutate(SMKDSTY = case_when(
    cchs2013_2014_p_sub$SMKDSTY==1~1,
    cchs2013_2014_p_sub$SMKDTSY==2~2,
    cchs2013_2014_p_sub$SMKDTSY==3~2,
    cchs2013_2014_p_sub$SMKDSTY==4~3,
    cchs2013_2014_p_sub$SMKDSTY==5~4,
    cchs2013_2014_p_sub$SMKDSTY==6~5))
  
#2015-2016 SURVEY CYCLE
#pulling working variables from 2005 survey
cchs2015_2016_p$DHHGAGE
cchs2015_2016_p$DHH_SEX
cchs2015_2016_p$HWTDGBMI
cchs2015_2016_p$EHG2DVR3
cchs2015_2016_p$DHHGMS
cchs2015_2016_p$SDCDVIMM
cchs2015_2016_p$ALCDVTTM
cchs2015_2016_p$CCC_065
cchs2015_2016_p$CCC_030
cchs2015_2016_p$CCC_085
cchs2015_2016_p$CCC_090
cchs2015_2016_p$FVCDVJUI
cchs2015_2016_p$FVCDVPOT
cchs2015_2016_p$FVCDVTOT
cchs2015_2016_p$GEN_005
cchs2015_2016_p$GEN_020
cchs2015_2016_p$GEN_030
cchs2015_2016_p$PAYDVADL
cchs2015_2016_p$SDCDGCGT
cchs2015_2016_p$SMK_050
cchs2015_2016_p$SMK_055
cchs2015_2016_p$SMK_080
cchs2015_2016_p$SMK_075
cchs2015_2016_p$SMKDVSTY
cchs2015_2016_p$SMKG035
cchs2015_2016_p$SMKG040
cchs2015_2016_p$SLP_010
cchs2015_2016_p$SLP_015
cchs2015_2016_p$SLPG005

cchs2015_2016_p_sub<-cchs2015_2016_p[c("DHHGAGE", "DHH_SEX",
                                       "HWTDGBMI", "EHG2DVR3",
                                       "DHHGMS", "SDCDVIMM",
                                       "ALCDVTTM", "CCC_065",
                                       "CCC_030", "CCC_085",
                                       "CCC_090", "FVCDVJUI",
                                       "FVCDVPOT", "FVCDVTOT",
                                       "GEN_005", "GEN_020",
                                       "GEN_030", "PAYDVADL",
                                       "SDCDGCGT", "SMK_050",
                                       "SMK_055", "SMK_080",
                                       "SMK_075", "SMKDVSTY",
                                       "SMKG035", "SMKG040",
                                       "SLP_010", "SLP_015",
                                       "SLPG005")]

cchs2015_2016_p_sub <- cchs2015_2016_p_sub |>
  #Creating continuous age variable in survey cycle by sampling random values 
  #from age range for specific age category
  mutate(DHHGEGE_cont = case_when(
    cchs2015_2016_p$DHHEGAGE==1~sample(c(12:14), length(cchs2015_2016_p$DHHEGAGE==1), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==2~sample(c(15:17), length(cchs2015_2016_p$DHHEGAGE==2), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==3~sample(c(18:19), length(cchs2015_2016_p$DHHEGAGE==3), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==4~sample(c(20:24), length(cchs2015_2016_p$DHHEGAGE==4), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==5~sample(c(25:29), length(cchs2015_2016_p$DHHEGAGE==5), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==6~sample(c(30:34), length(cchs2015_2016_p$DHHEGAGE==6), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==7~sample(c(35:39), length(cchs2015_2016_p$DHHEGAGE==7), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==8~sample(c(40:44), length(cchs2015_2016_p$DHHEGAGE==8), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==9~sample(c(45:49), length(cchs2015_2016_p$DHHEGAGE==9), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==10~sample(c(50:54), length(cchs2015_2016_p$DHHEGAGE==10), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==11~sample(c(55:59), length(cchs2015_2016_p$DHHEGAGE==11), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==12~sample(c(60:64), length(cchs2015_2016_p$DHHEGAGE==12), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==13~sample(c(65:69), length(cchs2015_2016_p$DHHEGAGE==13), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==14~sample(c(70:74), length(cchs2015_2016_p$DHHEGAGE==14), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==15~sample(c(75:79), length(cchs2015_2016_p$DHHEGAGE==15), replace = TRUE),
    cchs2015_2016_p$DHHEGAGE==16~sample(c(80:102), length(cchs2015_2016_p$DHHEGAGE==16), replace = TRUE)))|>
  #Creating ADL score across survey cycle by sampling random values 
  #from DemPoRT score range 
  mutate(ADL_score_6 = sample(c(1:6), nrow(cchs2015_2016_p_sub), replace = TRUE))|>
  # #Creating hearing variables 
  # mutate(HUI06 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  # mutate(HUI07 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  # mutate(HUI08 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  # mutate(HUI09 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  #Creating pack years variable
  mutate(pack_years = case_when(
    cchs2015_2016_p_sub$DHH_SEX==1~sample(c(0:112), length(cchs2015_2016_p_sub$DHH_SEX==1), replace = TRUE),
    cchs2015_2016_p_sub$DHH_SEX==2~sample(c(0:78), length(cchs2015_2016_p_sub$DHH_SEX==2), replace = TRUE)))|>
  #Create ethnicity across survey cycle by sampling random values 
  #from DemPoRT ethnicity group range 
  mutate(SDCDCGT_B = sample(c(1:7), nrow(cchs2015_2016_p_sub), replace = TRUE))|>
  #Create sleep variables across survey cycles
  #SLP_02
  mutate(SLP_02 = case_when(
    cchs2015_2016_p_sub$SLP_010==1~1,
    cchs2015_2016_p_sub$SLP_010==2~2,
    cchs2015_2016_p_sub$SLP_010==3~2,
    cchs2015_2016_p_sub$SLP_010==4~3,
    cchs2015_2016_p_sub$SLP_010==5~3))|>
  #SLP_03
  mutate(SLP_03 = case_when(
    cchs2015_2016_p_sub$SLP_15==1~1,
    cchs2015_2016_p_sub$SLP_15==2~2,
    cchs2015_2016_p_sub$SLP_15==3~2,
    cchs2015_2016_p_sub$SLP_15==4~3,
    cchs2015_2016_p_sub$SLP_15==5~3))|>
  #SLPG01
    rename(SLPG005==SLPG01)|>
  #Create necessary continuous smoking variables
  #SMKG203
  mutate(SMKG203_cont = case_when(
    cchs2015_2016_p_sub$SMKG040==1~8,
    cchs2015_2016_p_sub$SMKG040==2~13,
    cchs2015_2016_p_sub$SMKG040==3~16,
    cchs2015_2016_p_sub$SMKG040==4~18.5,
    cchs2015_2016_p_sub$SMKG040==5~22,
    cchs2015_2016_p_sub$SMKG040==6~27,
    cchs2015_2016_p_sub$SMKG040==7~32,
    cchs2015_2016_p_sub$SMKG040==8~37,
    cchs2015_2016_p_sub$SMKG040==9~42,
    cchs2015_2016_p_sub$SMKG040==10~47,
    cchs2015_2016_p_sub$SMKG040==11~55))|>
  #SMKG207
  mutate(SMKG207_cont = case_when(
    cchs2015_2016_p_sub$SMKG040==1~8,
    cchs2015_2016_p_sub$SMKG040==2~13,
    cchs2015_2016_p_sub$SMKG040==3~16,
    cchs2015_2016_p_sub$SMKG040==4~18.5,
    cchs2015_2016_p_sub$SMKG040==5~22,
    cchs2015_2016_p_sub$SMKG040==6~27,
    cchs2015_2016_p_sub$SMKG040==7~32,
    cchs2015_2016_p_sub$SMKG040==8~37,
    cchs2015_2016_p_sub$SMKG040==9~42,
    cchs2015_2016_p_sub$SMKG040==10~47,
    cchs2015_2016_p_sub$SMKG040==11~55))|>
  #SMKDTSY (6 categories to 5)
  mutate(SMKDSTY = case_when(
    cchs2015_2016_p_sub$SMKDSTY==1~1,
    cchs2015_2016_p_sub$SMKDTSY==2~2,
    cchs2015_2016_p_sub$SMKDTSY==3~2,
    cchs2015_2016_p_sub$SMKDSTY==4~3,
    cchs2015_2016_p_sub$SMKDSTY==5~4,
    cchs2015_2016_p_sub$SMKDSTY==6~5))|>
  #Renaming variables to most up-to-date versions 
  rename(HWTDGBMI==HWTGBMI)|>
  rename(EHG2DVR3==EDUDR04)|>
  rename(SDCDVIMM==SDCFIMM)|>
  rename(ALCDVTMM==ALCDTTM)|>
  rename(CCC_065==CCC_071)|>
  rename(CCC_030==CCC_91F)|>
  rename(CCC_085==CCC_121)|>
  rename(CCC_090==CCC_151)|>
  rename(FVCDVJUI==FVCDJUI)|>
  rename(FVCDVPOT==FVCDPOT)|>
  rename(FVCDVTOT==FVCDTOT)|>
  rename(GEN_005==GEN_01)|>
  rename(GEN_020==GEN_07)|>
  rename(GEN_030==GEN_10)|>
  rename(PAYDVADL==PACFLEI)|>
  rename(SDCDGCGT==SDCGCGT)|>
  rename(SMK_050==SMK_05B)|>
  rename(SMK_055==SMK_05C)|>
  rename(SMK_080==SMK_09A)|>
  rename(SMK_075==SMK_208)|>
  rename(SMKG035==SMKG01C)|>
  rename(SLP_010==SLP_02)|>
  rename(SLP_015==SLP_03)|>
  rename(SLPG005==SLPG01)
  

#2017-2018 SURVEY CYCLE
#pulling working variables from 2017-2018 survey cylce
cchs2017_2018_p$DHHGAGE
cchs2017_2018_p$DHH_SEX
cchs2015_2016_p$HWTDGBMI
cchs2015_2016_p$EHG2DVR3
cchs2017_2018_p$DHHGMS
cchs2017_2018_p$SDCDVIMM
cchs2017_2018_p$ALCDVTTM
cchs2017_2018_p$CCC_065
cchs2017_2018_p$CCC_030
cchs2017_2018_p$CCC_085
cchs2017_2018_p$CCC_090
cchs2017_2018_p$FVCDVJUI
cchs2017_2018_p$FVCDVPOT
cchs2017_2018_p$FVCDVTOT
cchs2017_2018_p$GEN_005
cchs2017_2018_p$GEN_020
cchs2017_2018_p$GEN_030
cchs2017_2018_p$PAYDVADL
cchs2017_2018_p$SDCDGCGT
cchs2017_2018_p$SMK_050
cchs2017_2018_p$SMK_055
cchs2017_2018_p$SMK_080
cchs2017_2018_p$SMK_075
cchs2017_2018_p$SMKDVSTY
cchs2017_2018_p$SMKG035
cchs2017_2018_p$SMKG040
cchs2017_2018_p$SLP_010
cchs2017_2018_p$SLP_015
cchs2017_2018_p$SLPG005
  
cchs2017_2018_p_sub<-cchs2017_2018_p[,c("DHHGAGE", "DHH_SEX",
                                        "HWTDGBMI", "EHG2DVR3",
                                        "DHHGMS", "SDCDVIMM",
                                        "ALCDVTTM", "CCC_065",
                                        "CCC_030", "CCC_085",
                                        "CCC_090", "FVCDVJUI",
                                        "FVCDVPOT", "FVCDVTOT",
                                        "GEN_005", "GEN_020",
                                        "GEN_030", "PAYDVADL",
                                        "SDCDGCGT", "SMK_050",
                                        "SMK_055", "SMK_080",
                                        "SMK_075", "SMKDVSTY",
                                        "SMKG035", "SMKG040",
                                        "SLP_010", "SLP_015",
                                        "SLPG005")]
  
cchs2017_2018_p_sub <- cchs2017_2018_p_sub |>
  #Creating continuous age variable in survey cycle by sampling random values 
  #from age range for specific age category
  mutate(DHHGEGE_cont = case_when(
    cchs2017_2018_p_sub$DHHEGAGE==1~sample(c(12:14), length(cchs2017_2018_p_sub$DHHEGAGE==1), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==2~sample(c(15:17), length(cchs2017_2018_p_sub$DHHEGAGE==2), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==3~sample(c(18:19), length(cchs2017_2018_p_sub$DHHEGAGE==3), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==4~sample(c(20:24), length(cchs2017_2018_p_sub$DHHEGAGE==4), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==5~sample(c(25:29), length(cchs2017_2018_p_sub$DHHEGAGE==5), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==6~sample(c(30:34), length(cchs2017_2018_p_sub$DHHEGAGE==6), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==7~sample(c(35:39), length(cchs2017_2018_p_sub$DHHEGAGE==7), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==8~sample(c(40:44), length(cchs2017_2018_p_sub$DHHEGAGE==8), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==9~sample(c(45:49), length(cchs2017_2018_p_sub$DHHEGAGE==9), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==10~sample(c(50:54), length(cchs2017_2018_p_sub$DHHEGAGE==10), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==11~sample(c(55:59), length(cchs2017_2018_p_sub$DHHEGAGE==11), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==12~sample(c(60:64), length(cchs2017_2018_p_sub$DHHEGAGE==12), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==13~sample(c(65:69), length(cchs2017_2018_p_sub$DHHEGAGE==13), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==14~sample(c(70:74), length(cchs2017_2018_p_sub$DHHEGAGE==14), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==15~sample(c(75:79), length(cchs2017_2018_p_sub$DHHEGAGE==15), replace = TRUE),
    cchs2017_2018_p_sub$DHHEGAGE==16~sample(c(80:102), length(cchs2017_2018_p_sub$DHHEGAGE==16), replace = TRUE)))|>
  #Creating ADL score across survey cycle by sampling random values 
  #from DemPoRT score range 
  mutate(ADL_score_6 = sample(c(1:6), nrow(cchs2015_2016_p), replace = TRUE))|>
  # #Creating hearing variables 
  # mutate(HUI06 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  # mutate(HUI07 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  # mutate(HUI08 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  # mutate(HUI09 = sample(c(1:2), nrow(cchs2013_2014_p_sub), replace = TRUE))|>
  #Creating pack years variable
  mutate(pack_years = case_when(
    cchs2017_2018_p_sub$DHH_SEX==1~sample(c(0:112), length(cchs2017_2018_p_sub$DHH_SEX==1), replace = TRUE),
    cchs2017_2018_p_sub$DHH_SEX==2~sample(c(0:78), length(cchs2017_2018_p_sub$DHH_SEX==2), replace = TRUE)))|>
  #Create ethnicity across survey cycle by sampling random values 
  #from DemPoRT ethnicity group range 
  mutate(SDCDCGT_B = sample(c(1:7), nrow(cchs2017_2018_p_sub), replace = TRUE))|>
  #Create sleep variables across survey cycles
  #SLP_02
  mutate(SLP_02 = case_when(
    cchs2017_2018_p_sub$SLP_010==1~1,
    cchs2017_2018_p_sub$SLP_010==2~2,
    cchs2017_2018_p_sub$SLP_010==3~2,
    cchs2017_2018_p_sub$SLP_010==4~3,
    cchs2017_2018_p_sub$SLP_010==5~3))|>
  #SLP_03
  mutate(SLP_03 = case_when(
    cchs2017_2018_p_sub$SLP_15==1~1,
    cchs2017_2018_p_sub$SLP_15==2~2,
    cchs2017_2018_p_sub$SLP_15==3~2,
    cchs2017_2018_p_sub$SLP_15==4~3,
    cchs2017_2018_p_sub$SLP_15==5~3))|>
  #SLPG01
  rename(SLPG005==SLPG01)|>
  #Create necessary continuous smoking variables
  #SMKG203
  mutate(SMKG203_cont = case_when(
    cchs2017_2018_p_sub$SMKG040==1~8,
    cchs2017_2018_p_sub$SMKG040==2~13,
    cchs2017_2018_p_sub$SMKG040==3~16,
    cchs2017_2018_p_sub$SMKG040==4~18.5,
    cchs2017_2018_p_sub$SMKG040==5~22,
    cchs2017_2018_p_sub$SMKG040==6~27,
    cchs2017_2018_p_sub$SMKG040==7~32,
    cchs2017_2018_p_sub$SMKG040==8~37,
    cchs2017_2018_p_sub$SMKG040==9~42,
    cchs2017_2018_p_sub$SMKG040==10~47,
    cchs2017_2018_p_sub$SMKG040==11~55))|>
  #SMKG207
  mutate(SMKG207_cont = case_when(
    cchs2017_2018_p_sub$SMKG040==1~8,
    cchs2017_2018_p_sub$SMKG040==2~13,
    cchs2017_2018_p_sub$SMKG040==3~16,
    cchs2017_2018_p_sub$SMKG040==4~18.5,
    cchs2017_2018_p_sub$SMKG040==5~22,
    cchs2017_2018_p_sub$SMKG040==6~27,
    cchs2017_2018_p_sub$SMKG040==7~32,
    cchs2017_2018_p_sub$SMKG040==8~37,
    cchs2017_2018_p_sub$SMKG040==9~42,
    cchs2017_2018_p_sub$SMKG040==10~47,
    cchs2017_2018_p_sub$SMKG040==11~55))|>
  #SMKDTSY (6 categories to 5)
  mutate(SMKDSTY = case_when(
    cchs2017_2018_p_sub$SMKDSTY==1~1,
    cchs2017_2018_p_sub$SMKDTSY==2~2,
    cchs2017_2018_p_sub$SMKDTSY==3~2,
    cchs2017_2018_p_sub$SMKDSTY==4~3,
    cchs2017_2018_p_sub$SMKDSTY==5~4,
    cchs2017_2018_p_sub$SMKDSTY==6~5))|>
  #Renaming variables to most up-to-date versions 
  rename(HWTDGBMI==HWTGBMI)|>
  rename(EHG2DVR3==EDUDR04)|>
  rename(SDCDVIMM==SDCFIMM)|>
  rename(ALCDVTMM==ALCDTTM)|>
  rename(CCC_065==CCC_071)|>
  rename(CCC_030==CCC_91F)|>
  rename(CCC_085==CCC_121)|>
  rename(CCC_090==CCC_151)|>
  rename(FVCDVJUI==FVCDJUI)|>
  rename(FVCDVPOT==FVCDPOT)|>
  rename(FVCDVTOT==FVCDTOT)|>
  rename(GEN_005==GEN_01)|>
  rename(GEN_020==GEN_07)|>
  rename(GEN_030==GEN_10)|>
  rename(PAYDVADL==PACFLEI)|>
  rename(SDCDGCGT==SDCGCGT)|>
  rename(SMK_050==SMK_05B)|>
  rename(SMK_055==SMK_05C)|>
  rename(SMK_080==SMK_09A)|>
  rename(SMK_075==SMK_208)|>
  rename(SMKG035==SMKG01C)|>
  rename(SLP_010==SLP_02)|>
  rename(SLP_015==SLP_03)|>
  rename(SLPG005==SLPG01)

cchs_master<-rbind(c(cchs2001_p_sub, cchs2003_p_sub, cchs2005_p_sub, 
                     cchs2007_2008_p_sub, cchs2009_2010_p_sub, cchs2011_2012_p_sub,
                     cchs2013_2014_p_sub, cchs2015_2016_p_sub, cchs2017_2018_p_sub))