library(data.table);library(magrittr);library(jstable);library(dplyr);library(MatchIt)
options(survey.lonely.psu = "adjust")

alist <- aws.s3::s3readRDS("data/SAIHST/danbee/NHANES_AYA_US_KOR.rds", bucket = "zarathu")

nh <- alist$nh
knh <- alist$knh

nh <- nh %>% 
  mutate(race = ifelse(RIDRETH1 == 1 | RIDRETH1 == 2, 1,
                       ifelse(RIDRETH1 == 3, 2,
                              ifelse(RIDRETH1 == 4, 3, 4))),
         racecat = ifelse(RIDRETH1 == 1 | RIDRETH1 == 2, "Hispanic",
                          ifelse(RIDRETH1 == 3, "Non-Hispanic white",
                                 ifelse(RIDRETH1 == 4, "Non-Hispanic black", "NA"))))

#nh %>% group_by(racecat) %>% count() 
nh1 <- filter(nh, racecat != "NA")



#자료 결합을 위한 국가간 변수 response scale 통합 정리(KOR=한국 국건영, US=미국 국건영)--------------------------------
#number of people in household
#US
nh1$DMDHHSIZ <- with(nh1, ifelse(DMDHHSIZ == 1, 1,
                                 ifelse(DMDHHSIZ == 2 , 2,
                                        ifelse(DMDHHSIZ == 3, 3,
                                               ifelse(DMDHHSIZ == 4, 4,
                                                      ifelse(DMDHHSIZ == 5, 5,
                                                             ifelse(DMDHHSIZ == 6, 6,
                                                                    ifelse(DMDHHSIZ == 7, 7, NA))))))))
#KOR
knh$cfam <- with(knh, ifelse(cfam == 1, 1,
                             ifelse(cfam == 2, 2,
                                    ifelse(cfam == 3, 3,
                                           ifelse(cfam == 4, 4,
                                                  ifelse(cfam == 5, 5,
                                                         ifelse(cfam == 6, 6,
                                                                ifelse(cfam > 6 & cfam < 10, 7, NA))))))))


#alcohol
#US
nh1$ALQ110 <- with(nh1, ifelse(ALQ110 == 1, 1,
                               ifelse(ALQ110 == 2, 0, 8)))
nh1$ALQ101 <- with(nh1, ifelse(ALQ101 == 1, 1,
                               ifelse(ALQ101 == 2, 0, 8)))
nh1$alc <- with(nh1, ifelse(ALQ101 == 0 & ALQ110 == 0, 0,
                            ifelse(ALQ101 == 0 & ALQ110 == 1, 1,
                                   ifelse(ALQ101 == 1 & ALQ110 == 1, 2, NA))))
#KOR
knh$BD1 <- with(knh, ifelse(BD1 == 1, 0,
                            ifelse(BD1 == 2, 1, 8)))
knh$BD1_11 <- with(knh, ifelse(BD1_11 == 1, 0,
                               ifelse(BD1_11 == 2, 1,
                                      ifelse(BD1_11 == 3 | BD1_11 == 4 | BD1_11 == 5 | BD1_11 == 6, 2, 8))))
knh$alc <- with(knh, ifelse(BD1 == 0 & BD1_11 == 0, 0,
                            ifelse(BD1 == 1 & BD1_11 == 1, 1,
                                   ifelse(BD1 == 1 & BD1_11 == 2, 2, NA))))

#smoking
#US
nh1$SMQ621 <- with(nh1, ifelse(SMQ621 == 1, 0,
                               ifelse(SMQ621 == 2 | SMQ621 == 3 | SMQ621 == 4 | SMQ621 == 5 | SMQ621 == 6 | SMQ621 == 7 | SMQ621 == 8, 2, 8)))
nh1$SMQ040 <- with(nh1, ifelse(SMQ040 == 3, 1,
                               ifelse(SMQ040 == 2 | SMQ040 == 1, 2, 8)))
nh1$smk <- with(nh1, ifelse(SMQ621 == 0, 0,
                            ifelse(SMQ621 == 1 & SMQ040 == 1, 1,
                                   ifelse(SMQ621 == 1 & SMQ040 == 2, 2, NA))))
#KOR
knh$BS1_1 <- with(knh, ifelse(BS1_1 == 3, 0,
                              ifelse(BS1_1 == 1 | BS1_1 == 2, 1, 8)))
knh$BS3_1 <- with(knh, ifelse(BS3_1 == 3, 1,
                              ifelse(BS3_1 == 1 | BS3_1 == 2, 2, 8)))
knh$smk <- with(knh, ifelse(BS1_1 == 0, 0,
                            ifelse(BS1_1 == 1 & BS3_1 == 1, 1,
                                   ifelse(BS1_1 == 1 & BS3_1 == 2, 2, NA))))

#Household income
#KOR
knh$ainc_year <- knh$ainc * 12 #연 수입으로 전환
knh$ainc <- ifelse(knh$ainc_year < 500, 1,
                   ifelse(knh$ainc_year >= 500 & knh$ainc_year < 1000, 2,
                          ifelse(knh$ainc_year >= 1000 & knh$ainc_year < 1500, 3, 
                                 ifelse(knh$ainc_year >= 1500 & knh$ainc_year < 2000, 4,
                                        ifelse(knh$ainc_year >=2000 & knh$ainc_year < 2500, 5,
                                               ifelse(knh$ainc_year >=2500 & knh$ainc_year <3500, 6,
                                                      ifelse(knh$ainc_year >= 3500 & knh$ainc_year < 4500, 7,
                                                             ifelse(knh$ainc_year >= 4500 & knh$ainc_year < 5500, 8,
                                                                    ifelse(knh$ainc_year >= 5500 & knh$ainc_year < 6500, 9,
                                                                           ifelse(knh$ainc_year >= 6500 & knh$ainc_year < 7500, 10,
                                                                                  ifelse(knh$ainc_year >= 7500 & knh$ainc_year < 10000, 11, NA)))))))))))
##KOR categorical 코딩
knh$ainc <- ifelse(knh$ainc == 1 | knh$ainc == 2 | knh$ainc == 3 | knh$ainc == 4 | knh$ainc ==5, 1,
                   ifelse(knh$ainc == 6 | knh$ainc == 7 | knh$ainc == 8, 2,
                          ifelse(knh$ainc == 9 | knh$ainc == 10, 3,
                                 ifelse(knh$ainc ==11 | knh$ainc == 12, 4, NA))))

#US
nh1$INDHHIN2 <- with(nh1, ifelse(INDHHIN2 == 1 | INDHHIN2 == 2 | INDHHIN2 == 3 | INDHHIN2 == 4 | INDHHIN2 == 13, 1, 
                                 ifelse(INDHHIN2 == 5 | INDHHIN2 == 6 | INDHHIN2 == 7 | INDHHIN2 == 8, 2,
                                        ifelse(INDHHIN2 == 9 | INDHHIN2 == 10, 3,
                                               ifelse(INDHHIN2 == 14 | INDHHIN2 == 15, 4, NA)))))


#Current job status + type of job -> 1= employed, 2= unemployed, 직업유무 중 무는 unemployed이나, 유로 대답한 대상자들은 아래 직업종류(type of job)로 mock table에 기입 예정
#US
nh1$OCD150 <- with(nh1,ifelse(OCD150 == 1 | OCD150 == 2, 1, 
                              ifelse(OCD150 == 3 | OCD150 == 4, 2, NA)))
nh1$OCQ260 <- with(nh1,ifelse(OCQ260 == 1 | OCQ260 == 2 | OCQ260 == 3 | OCQ260 == 4, 1,
                              ifelse(OCQ260 == 5, 2,
                                     ifelse(OCQ260 == 6, 3, NA))))

#KOR
knh$EC1_1 <- with(knh,ifelse(EC1_1 == 1, 1, 
                             ifelse(EC1_1 == 2, 2, NA)))
knh$EC_stt_1 <- with(knh,ifelse(EC_stt_1 == 1, 1, 
                                ifelse(EC_stt_1 == 2, 2,
                                       ifelse(EC_stt_1 == 3, 3, NA))))

#Marital status
#KOR
knh$marri <- with(knh, ifelse(marri_1 == 1 & marri_2 == 1, 1,
                              ifelse(marri_1 == 1 & marri_2 == 3, 2,
                                     ifelse(marri_1 == 1 & marri_2 == 4, 3,
                                            ifelse(marri_1 == 1 & marri_2 == 2, 4,
                                                   ifelse(marri_1 == 2 & (marri_2 == 88 | marri_2 == 9 | marri_2 == 99), 5,
                                                          ifelse(marri_1 == 2 & marri_2 == 1, 6, NA)))))))
#US
nh1$DMDMARTL <- with(nh1, ifelse(DMDMARTL == 1, 1,
                                 ifelse(DMDMARTL == 2, 2,
                                        ifelse(DMDMARTL == 3, 3,
                                               ifelse(DMDMARTL == 4, 4,
                                                      ifelse(DMDMARTL == 5, 5,
                                                             ifelse(DMDMARTL == 6, 6, NA)))))))
#Education
#US
nh1$DMDEDUC2 <- with(nh1, ifelse(DMDEDUC2 == 1 | DMDEDUC2 == 2, 1,
                                 ifelse(DMDEDUC2 == 3, 2,
                                        ifelse(DMDEDUC2 == 4 | DMDEDUC2 == 5, 3, NA))))
#KOR
knh$edu <- with(knh, ifelse(edu == 1 | edu == 2, 1,
                            ifelse(edu == 3, 2,
                                   ifelse(edu == 4, 3, NA))))

#Comorbidities-> Ever diagnosed------
#Asthma
nh1$MCQ010 <- with(nh1, ifelse(MCQ010 == 1, 1,
                               ifelse(MCQ010 == 2, 0, 8)))
knh$DJ4_dg <- with(knh, ifelse(DJ4_dg == 1, 1,
                               ifelse(DJ4_dg == 0, 0, 8)))

#Arthritis
nh1$MCQ160A <- with(nh1, ifelse(MCQ160A == 1, 1,
                                ifelse(MCQ160A == 2, 0,8)))
knh$DM1_dg <- with(knh, ifelse(DM1_dg == 1, 1,
                               ifelse(DM1_dg == 0, 0, 8)))

#Hypertension
#US
nh1$BPQ020 <- with(nh1, ifelse(BPQ020 == 1, 1,
                               ifelse(BPQ020 == 2, 0, 8)))
#KOR
knh$DI1_dg <- with(knh, ifelse(DI1_dg == 1, 1,
                               ifelse(DI1_dg == 0, 0, 8)))

#Dyslipidemia / hyperlipidemia
#US
nh1$BPQ080 <- with(nh1, ifelse(BPQ080 == 1, 1,
                               ifelse(BPQ080 == 2, 0, 8)))
#KOR
knh$DI2_dg <- with(knh, ifelse(DI2_dg == 1, 1,
                               ifelse(DI2_dg == 0, 0, 8)))

#Diabetes
#US
nh1$DIQ010 <- with(nh1, ifelse(DIQ010 == 1, 1,
                               ifelse(DIQ010 == 2, 0, 8)))
#KOR
knh$DE1_dg <- with(knh, ifelse(DE1_dg == 1, 1,
                               ifelse(DE1_dg == 0, 0, 8)))

#Stroke
#US
nh1$MCQ160F <- with(nh1, ifelse(MCQ160F == 1, 1,
                                ifelse(MCQ160F == 2, 0, 8)))
#KOR
knh$DI3_dg <- with(knh, ifelse(DI3_dg == 1, 1,
                               ifelse(DI3_dg == 0, 0, 8)))

#Angina
#US
nh1$MCQ160D <- with(nh1, ifelse(MCQ160D == 1, 1,
                                ifelse(MCQ160D == 2, 0, 8)))
#KOR
knh$DI6_dg <- with(knh, ifelse(DI6_dg == 1, 1,
                               ifelse(DI6_dg == 0, 0, 8)))

#MI
#US
nh1$MCQ160E <- with(nh1, ifelse(MCQ160E == 1, 1,
                                ifelse(MCQ160E == 2, 0, 8)))
#KOR
knh$DI5_dg <- with(knh, ifelse(DI5_dg == 1, 1,
                               ifelse(DI5_dg == 0, 0, 8)))

#Thyroid problem
#US
nh1$MCQ160M <- with(nh1, ifelse(MCQ160M == 1, 1,
                                ifelse(MCQ160M == 2, 0, 8)))
#KOR
knh$DE2_dg <- with(knh, ifelse(DE2_dg == 1, 1,
                               ifelse(DE2_dg == 0, 0, 8)))

#Reproductive health------
#Ever pregnant
#US
nh1$RHQ131 <- with(nh1, ifelse(RHQ131 == 1, 1,
                               ifelse(RHQ131 ==2, 0, NA)))
#KOR
knh$LW_pr <- with(knh, ifelse(LW_pr == 1, 1,
                              ifelse(LW_pr ==2, 0, NA)))

#times have been pregnant
# #US
# nh1$RHQ160 <- with(nh1, ifelse(RHQ160 == 0, 0,
#                                ifelse(RHQ160 ==1, 1,
#                                       ifelse(RHQ160 >=2 & RHQ160 <=4, 2,
#                                              ifelse(RHQ160>=5 & RHQ160<=10, 3, 
#                                                     ifelse(RHQ160==1, 4, 0))))))
# #KOR
# knh$LW_pr_1 <- with(knh, ifelse(LW_pr_1 == 0, 0,
#                                 ifelse(LW_pr_1 ==1, 1,
#                                        ifelse(LW_pr_1 >=2 & LW_pr_1 <=4, 2,
#                                               ifelse(LW_pr_1 >=5 & LW_pr_1 <=10, 3,
#                                                      ifelse(LW_pr_1 >=11, 4, 0))))))

## Age at last menstrual period, mean(SD)
nh1[, r_last := ifelse(RHQ060 %in% c(777, 999), NA, RHQ060)]
knh %<>% mutate(r_last = ifelse(LW_mp_a %in% c(888, 999), NA, LW_mp_a))




#Physical activity--------
#vigorous activity: Yes or No
#US
nh1$PAQ650 <- with(nh1, ifelse(PAQ650 == 1, 1,
                               ifelse(PAQ650 ==2, 0, NA)))
#KOR
knh$BE3_75 <- with(knh, ifelse(BE3_75 == 1, 1,
                               ifelse(BE3_75 ==2, 0, NA)))
#vigorous activity: how many days / week
#US
nh1$PAQ655 <- with(nh1, ifelse(PAQ655 >= 1 & PAQ655 < 4, 1,
                               ifelse(PAQ655 >= 4 & PAQ655 < 7, 2,
                                      ifelse(PAQ655 == 7, 3,
                                             ifelse(PAQ655 == 0, 0, NA)))))
#KOR
knh$BE3_76 <- with(knh, ifelse(BE3_76 == 0, 0,
                               ifelse(BE3_76 >= 1 & BE3_76 < 4, 1,
                                      ifelse(BE3_76 >= 4 & BE3_76 < 7, 2,
                                             ifelse(BE3_76 == 7, 3, NA)))))
#moderate activity: Yes or No
#US
nh1$PAQ665 <- with(nh1, ifelse(PAQ665 == 1, 1,
                               ifelse(PAQ665 ==2, 0, NA)))
#KOR
knh$BE3_85 <- with(knh, ifelse(BE3_85 == 1, 1,
                               ifelse(BE3_85 ==2, 0, NA)))

#moderate activity: how many days / week
#US
nh1$PAQ670 <- with(nh1, ifelse(PAQ670 >= 1 & PAQ670 < 4, 1,
                               ifelse(PAQ670 >= 4 & PAQ670 < 7, 2,
                                      ifelse(PAQ670 == 7, 3,
                                             ifelse(PAQ670 == 0, 0, NA)))))
#KOR
knh$BE3_86 <- with(knh, ifelse(BE3_86 == 0, 0,
                               ifelse(BE3_86 >= 1 & BE3_86 < 4, 1,
                                      ifelse(BE3_86 >= 4 & BE3_86 < 7, 2,
                                             ifelse(BE3_86 == 7, 3, NA)))))

#BMI categories
#US
nh1$BMXBMI <- with(nh1, ifelse(BMXBMI < 18.5, 1,
                               ifelse(BMXBMI >= 18.5 & BMXBMI < 23.0, 2,
                                      ifelse(BMXBMI >= 23.0 & BMXBMI < 30.0, 3, 4))))
#KOR
knh$HE_BMI <- with(knh, ifelse(HE_BMI < 18.5, 1,
                               ifelse(HE_BMI >= 18.5 & HE_BMI < 23.0, 2,
                                      ifelse(HE_BMI >= 23.0 & HE_BMI < 30.0, 3, 4))))

## weighting
nh1$persweight <- nh1$WTINT2YR * 1/6
#knh <- knh %>% mutate(persweight = ifelse(year %in% c(2007, 2008, 2009), wt_itv, wt_itvex))
knh$persweight <- knh$wt_itvex

#국가 변수 추가
nh1$country <- "US"
knh$country <- "KOR"

#한국 국건영 자료에 인종 변수 추가 
knh$race <- "KOR"


## Cancer type
knh %<>% 
  mutate(stomach_c = as.integer(DC1_dg == 1), stomach_a = ifelse(DC1_ag %in% c(888, 999), NA, DC1_ag), 
         liver_c =  as.integer(DC2_dg == 1), liver_a = ifelse(DC2_ag %in% c(888, 999), NA, DC2_ag),
         colon_c =  as.integer(DC3_dg == 1), colon_a = ifelse(DC3_ag %in% c(888, 999), NA, DC3_ag),
         breast_c =  as.integer(DC4_dg == 1), breast_a = ifelse(DC4_ag %in% c(888, 999), NA, DC4_ag),
         cervix_c =  as.integer(DC5_dg == 1), cervix_a = ifelse(DC5_ag %in% c(888, 1000), NA, DC5_ag),
         lung_c =  as.integer(DC6_dg == 1), lung_a = ifelse(DC6_ag %in% c(888, 1000), NA, DC6_ag),
         thy_c =  as.integer(DC7_dg == 1), thy_a = ifelse(DC7_ag %in% c(888, 1000), NA, DC7_ag),
         other_c =  as.integer(DC11_dg == 1 | DC12_ag == 1), other_a = ifelse(DC11_ag %in% c(888, 1000), NA, DC11_ag)) 

nh1[, `:=`(stomach_c = as.integer(MCQ230A == 35 | MCQ230B == 35 | MCQ230C == 35), 
           liver_c = as.integer(MCQ230A == 22 | MCQ230B == 22 | MCQ230C == 22),
           colon_c = as.integer(MCQ230A == 16 | MCQ230B == 22 | MCQ230C == 22),
           breast_c = as.integer(MCQ230A == 14 | MCQ230B == 22 | MCQ230C == 22),
           cervix_c = as.integer(MCQ230A == 15 | MCQ230B == 22 | MCQ230C == 22),
           lung_c = as.integer(MCQ230A == 23 | MCQ230B == 22 | MCQ230C == 22),
           thy_c = as.integer(MCQ230A == 37 | MCQ230B == 22 | MCQ230C == 22),
           other_c = as.integer(MCQ230A %in% c(10:13, 17:21, 24:34, 36, 38, 39) | MCQ230B  %in% c(10:13, 17:21, 24:34, 36, 38, 39) | MCQ230C  %in% c(10:13, 17:21, 24:34, 36, 38, 39)))]

nh1[, `:=`(stomach_a = ifelse(MCQ240Z %in% c(77777,99999),NA,MCQ240Z),
           liver_a = ifelse(MCQ240M %in% c(77777,99999),NA,MCQ240M),
           colon_a = ifelse(MCQ240G %in% c(77777,99999),NA,MCQ240G),
           breast_a = ifelse(MCQ240E %in% c(77777,99999),NA,MCQ240E),
           cervix_a = ifelse(MCQ240F %in% c(77777,99999),NA,MCQ240F),
           lung_a = ifelse(MCQ240N %in% c(77777,99999),NA,MCQ240N),
           thy_a = ifelse(MCQ240BB %in% c(77777,99999),NA,MCQ240BB),
           other_a = ifelse(MCQ240DD %in% c(77777,100000),NA,MCQ240DD))]

#미국+한국 통합 변수로 변경-------------------------------
nh1 <- rename(nh1, art_d = MCQ160A, ast_d = MCQ010, r_preg = RHQ131, house_inc = INDHHIN2, p_age = RIDAGEYR, p_id = SDMVPSU, 
              p_sex = RIAGENDR, p_edu = DMDEDUC2, p_marri = DMDMARTL, job_stat = OCD150, job_type = OCQ260, r_pregn = RHQ160,
              job_hr = OCQ180, g_health = HSD010, hbp_d = BPQ020, chol_d = BPQ080, diab_d = DIQ010, stk_d = MCQ160F, phy_v = PAQ650,
              ang_d = MCQ160D, mi_d = MCQ160E, thy_d = MCQ160M, pf_lim = PFQ059, persweight = persweight, strata = SDMVSTRA, phy_vn = PAQ655,
              phy_m = PAQ665, phy_mn = PAQ670, n_fam = DMDHHSIZ, BMI = BMXBMI)
knh <- rename(knh, art_d = DM1_dg, ast_d = DJ4_dg, r_preg = LW_pr, racecat = race, house_inc = ainc, p_age = age, p_id = psu, p_sex = sex, 
              p_edu = edu, p_marri = marri, job_stat = EC1_1, job_type = EC_stt_1, job_hr = EC_wht_23, g_health = D_1_1, r_pregn = LW_pr_1,
              hbp_d = DI1_dg, chol_d = DI2_dg, diab_d = DE1_dg, stk_d = DI3_dg, ang_d = DI6_dg, mi_d = DI5_dg, thy_d = DE2_dg, phy_v = BE3_75,
              pf_lim = LQ4_00, persweight = persweight, strata = kstrata, phy_vn = BE3_76, phy_m = BE3_85, phy_mn = BE3_86, n_fam = cfam, BMI = HE_BMI)
nh1$p_id <- nh1$p_id %>% as.character()
nh1$racecat <- nh1$racecat %>% as.character()
knh$racecat <- knh$racecat %>% as.character()

nh1$ast_d

## Combine 2 data
varlist <- list(Base = c("cancer", "AYA", "p_age", "country", "racecat", "p_sex", "BMI", "p_edu", "p_marri", "job_stat", "job_type",  "house_inc",
                         "job_hr", "g_health", "n_fam", "alc", "smk"),
                Disease = c("art_d", "ast_d", "hbp_d", "chol_d", "diab_d", "stk_d", "ang_d", "mi_d", "thy_d"),
                Physical = c("pf_lim", "phy_v", "phy_vn", "phy_m", "phy_mn"), 
                # Reproduce = c("r_pregn", "r_preg", "r_last"),
                Reproduce = c("r_preg", "r_last"),
                CancerType = c("stomach_c", "liver_c", "colon_c", "breast_c", "cervix_c", "lung_c", "thy_c", "other_c"),
                Survey = c("p_id", "strata", "persweight"),
                CancerAge = c("stomach_a","liver_a","colon_a","breast_a","cervix_a","lung_a","thy_a","other_a"))


tot <- dplyr::bind_rows(nh1, knh)[
  , `:=`(cancer = ifelse(MCQ220 == 1 | DC1_dg == 1 | DC2_dg == 1 | DC3_dg == 1 | DC4_dg == 1 | DC5_dg == 1 | DC6_dg == 1 | 
                           DC7_dg == 1 | DC11_dg == 1 | DC12_dg == 1, 1, 0),
         AYA = ifelse((MCQ240A >= 15 & MCQ240A <= 39) | 
                        (MCQ240B >= 15 & MCQ240B <= 39) |
                        (MCQ240C >= 15 & MCQ240C <= 39) |
                        (MCQ240D >= 15 & MCQ240D <= 39) | 
                        (MCQ240E >= 15 & MCQ240E <= 39) |
                        (MCQ240F >= 15 & MCQ240F <= 39) |
                        (MCQ240G >= 15 & MCQ240G <= 39) | 
                        (MCQ240H >= 15 & MCQ240H <= 39) |
                        (MCQ240I >= 15 & MCQ240I <= 39) |
                        (MCQ240J >= 15 & MCQ240J <= 39) | 
                        (MCQ240K >= 15 & MCQ240K <= 39) |
                        (MCQ240L >= 15 & MCQ240L <= 39) |
                        (MCQ240M >= 15 & MCQ240M <= 39) | 
                        (MCQ240N >= 15 & MCQ240N <= 39) |
                        (MCQ240O >= 15 & MCQ240O <= 39) |
                        (MCQ240P >= 15 & MCQ240P <= 39) | 
                        (MCQ240Q >= 15 & MCQ240Q <= 39) |
                        (MCQ240R >= 15 & MCQ240R <= 39) | 
                        (MCQ240S >= 15 & MCQ240S <= 39) |
                        (MCQ240T >= 15 & MCQ240T <= 39) |
                        (MCQ240U >= 15 & MCQ240U <= 39) |
                        (MCQ240V >= 15 & MCQ240V <= 39) |
                        (MCQ240W >= 15 & MCQ240W <= 39) |
                        (MCQ240X >= 15 & MCQ240X <= 39) | 
                        (MCQ240Y >= 15 & MCQ240Y <= 39) |
                        (MCQ240Z >= 15 & MCQ240Z <= 39) |
                        (MCQ240AA >= 15 & MCQ240AA <= 39) | 
                        (MCQ240BB >= 15 & MCQ240BB <= 39) |
                        (MCQ240CC >= 15 & MCQ240CC <= 39) |
                        (MCQ240DD >= 15 & MCQ240DD <= 39) | 
                        (MCQ240DK >= 15 & MCQ240DK <= 39) |
                        (DC1_ag >= 15 & DC1_ag <= 39) |
                        (DC2_ag >= 15 & DC2_ag <= 39)  |
                        (DC3_ag >= 15 & DC3_ag <= 39)  |
                        (DC4_ag >= 15 & DC4_ag <= 39)  |
                        (DC5_ag >= 15 & DC5_ag <= 39)  |
                        (DC6_ag >= 15 & DC6_ag <= 39)  |
                        (DC7_ag >= 15 & DC7_ag <= 39)  |
                        (DC11_ag >= 15 & DC11_ag <= 39)  |
                        (DC12_ag >= 15 & DC12_ag <= 39), 1, 0))][, `:=`(cancer = as.integer(!is.na(cancer)),
                                                                        AYA = as.integer(!is.na(AYA)))][!is.na(persweight) & p_age >= 15, .SD, .SDcols = unlist(varlist)]
#yrcancer = p_age - pmin(MCQ240A, MCQ240B, MCQ240C, MCQ240D, MCQ240E,
#                        MCQ240F, MCQ240G, MCQ240H, MCQ240I, MCQ240J,
#                        MCQ240K, MCQ240L, MCQ240M, MCQ240N, MCQ240O,
#                        MCQ240P, MCQ240Q, MCQ240R, MCQ240S, MCQ240T,
#                        MCQ240U, MCQ240V, MCQ240W, MCQ240X, MCQ240Y,
#                        MCQ240Z, MCQ240AA, MCQ240BB, MCQ240CC, MCQ240DD,
#                        MCQ240DK, DC1_ag, DC2_ag, DC3_ag, DC4_ag,
#                        DC5_ag, DC6_ag, DC7_ag, DC11_ag, na.rm = T)

tot<-tot[!(AYA==0 & cancer==1),.SD,.SD]

tot <- tot[, `:=`(job_hr = ifelse(job_hr %in% c(888, 99999), NA, job_hr),
                  g_health = ifelse(g_health ==9, NA, g_health),
                  pf_lim = ifelse(pf_lim ==9, NA, pf_lim))][]

for (v in c("p_edu", "p_marri", "job_stat", "job_type", "house_inc", varlist$Disease, "phy_vn", "phy_mn", "alc", "smk")){
  if(!is.null(tot[[v]][tot[[v]] == 8])){
    tot[[v]][tot[[v]] == 8] <- NA
  }
}

natozero <- c(unlist(varlist[c("Disease", "CancerType")]),"r_preg")
for (v in natozero){
  if(!is.null(tot[[v]][is.na(tot[[v]]) | tot[[v]] == 8])){
    tot[[v]][is.na(tot[[v]]) | tot[[v]] == 8] <- 0
  }
}

tot<-na.omit(tot, cols = c("p_edu","p_marri","house_inc","job_stat","ast_d","hbp_d","chol_d","diab_d","stk_d","ang_d","mi_d","thy_d"))

set.seed(1)
test <- matchit(AYA ~ p_age + p_sex + racecat, data = tot,
                method = "nearest", distance = "glm", ratio = 5)

out <- match.data(test)[, .SD, .SDcols = -c("distance", "weights", "subclass")]

factor_vars <- c("cancer", "AYA", "country", "racecat", "p_sex", "p_edu", "p_marri", "job_stat", "job_type", "r_preg", "house_inc", "g_health",
                 varlist$Disease, varlist$Physical, varlist$CancerType,"BMI")

out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
#out.svy <- svydesign(data = tot.mat, id =~p_id, strata = ~strata, weights = ~persweight, nest = T)
out.label <- mk.lev(out)

saveRDS(out, "NHANES_AYA_final.RDS")
