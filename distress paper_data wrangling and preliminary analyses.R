#data preparation to run analyses of psych distress in PPlus baseline and follow-up
#install packages
install.packages("dplyr")
install.packages("psych")
install.packages("sas7bdat")
install.packages("survey")
install.packages("car")

#load libraries
library(dplyr)
library(psych)
library(sas7bdat)
library(survey)
library(gmodels) 
library(ggplot2)
library(car)
library(twang)
library(xtable)

#set working directory
setwd("~/PHRESH and greenspace/PHRESH Plus papers/neighborhoods and depression/data")

#read in data files
#wave 1 and pplus1 both contain obs from baseline data collection (N = 1051) but different variables
wave1 <- read.sas7bdat("phreshplus_hh_11182013.sas7bdat")
pplus1 <- read.sas7bdat("pplus1.sas7bdat")
wave2 <- read.sas7bdat("phreshplus2016_hh_20170120.sas7bdat")
both <- read.sas7bdat("both.sas7bdat")

#convert NaN values to NA so that they will be recognized as missing in subsequent analyses
wave1[ is.na(wave1) ] <- NA
pplus1[ is.na(pplus1) ] <- NA
wave2[ is.na(wave2) ] <- NA
both[ is.na(both) ] <- NA

#create indicator for resides in Hill (1) vs. Homewood (0) at baseline
pplus1$hill[pplus1$geo_nhd2 == 'I'] <- 1
pplus1$hill[pplus1$geo_nhd2 == 'C'] <- 0

#create indicator for whether respondent was in Homewood (control "C") or Hill (intervention "I") at 
#wave 1 based on GIS data from "movers" data
pplus1$incandi <- 0
pplus1$incandi[(pplus1$geo_nhd2 == 'C' | pplus1$geo_nhd2 == 'I')] <- 1

#rename additional desired vars from wave 1 data and add them to data both
wave1$newsa_is_1 = wave1$newsa_is
wave1$newsa_surround_1 = wave1$newsa_surround
wave1$crossing_1 = wave1$crossing
wave1$crossing_n_1 = wave1$crossing_n
wave1$mixed_1 = wave1$mixed
wave1$mixed_n_1 = wave1$mixed_n
wave1$walkscore_1 = wave1$walkscore
wave1$walkscore_n_1 = wave1$walkscore_n
wave1$id_nhood_1 = wave1$ID_NHOOD

#create binary version of wave1$race_black
wave1$race_black_1[wave1$race_black==1] <- 1
wave1$race_black_1[wave1$race_black==0 | wave1$race_black==2] <- 0

#age at T1: create categories
#1 = <35
#2 = 35-44
#3 = 45-54
#4 = 55-64
#5 = 65-74
#6 = 75+
wave1$agecat[wave1$Age < 35] <- 1
wave1$agecat[wave1$Age >= 35 & wave1$Age < 45] <- 2
wave1$agecat[wave1$Age >= 45 & wave1$Age < 55] <- 3
wave1$agecat[wave1$Age >= 55 & wave1$Age < 65] <- 4
wave1$agecat[wave1$Age >= 65 & wave1$Age < 75] <- 5
wave1$agecat[wave1$Age >= 75] <- 6

# college degree: 1 = has at least college degree, 0 = does not have college degree
pplus1$college_1[pplus1$i_highest_edu_1==4] <- 1
pplus1$college_1[pplus1$i_highest_edu_1==1 | pplus1$i_highest_edu_1==2 | pplus1$i_highest_edu_1==3] <- 0

# weight status: 1 = not overweight, 2 = overweight, 3 = obese
pplus1$wtstat1[pplus1$bmi_1 < 25] <- 1
pplus1$wtstat1[pplus1$bmi_1 >= 25 & pplus1$bmi_1 < 30] <- 2
pplus1$wtstat1[pplus1$bmi_1 >= 30] <- 3

# LIVING WITH OTHERS

# create binary vars for living with others vs. alone

# T1
wave1$livotht1 <- NA
wave1$livotht1[wave1$nhh >= 2] <- 1
wave1$livotht1[wave1$nhh == 1] <- 0

#create subset of wave 1 data that includes only additional desired vars from wave 1 and id var
wave1subset <- select(wave1, id_merge, newsa_is_1, newsa_surround_1, crossing_1, crossing_n_1, mixed_1, mixed_n_1, 
                      walkscore_1, walkscore_n_1, id_nhood_1, agecat, race_black_1, livotht1)

#merge wave1subset and pplus1 to create w1all
w1all <- merge(wave1subset, pplus1, by="id_merge", all = FALSE)

#create subset of data that includes only respondents who lived in Hill or Homewood at wave1

w1hdhw <- filter(w1all, incandi==1)

#create dataset that includes only wave 1 cases with complete data on variables in cross-sectional model

#create indicator for whether respondents had missing data on any variables included in wave 1 cross-sectional model
w1hdhw$missany1 <- 0
w1hdhw$missany1[is.na(w1hdhw$k6_1 == 1) | 
               is.na(w1hdhw$Age == 1) | 
               is.na(w1hdhw$male == 1) | 
               is.na(w1hdhw$i_married_1 == 1) | 
               is.na(w1hdhw$livotht1 == 1) | 
               is.na(w1hdhw$i_highest_edu_1 == 1) | 
               is.na(w1hdhw$i_adj_inc2_1 == 1) | 
               is.na(w1hdhw$i_car_access_1 == 1) |
               is.na(w1hdhw$bmi_1 == 1) |
               is.na(w1hdhw$newsa_is_1 == 1) |
               is.na(w1hdhw$newsa_surround_1 == 1) |
               is.na(w1hdhw$soc_cohesion_1 == 1) |
               is.na(w1hdhw$nbhd_safety_1 == 1) |
               is.na(w1hdhw$total_crime2012 == 1) |
               is.na(w1hdhw$activetrans_n_1 == 1) |
               is.na(w1hdhw$aesthetics_n_1 == 1) |
               is.na(w1hdhw$amenities_n_1 == 1) |
               is.na(w1hdhw$crossing_n_1 == 1) |
               is.na(w1hdhw$mixed_n_1 == 1) |
               is.na(w1hdhw$walkscore_n_1 == 1)] <- 1

table(w1hdhw$missany1)

# WAVE 2 DATA--PREPARE TO MERGE WITH WAVE 1

#rename new vars from wave 2 data
wave2$id_merge = wave2$ID_PHRESH
wave2$car_access_2 = wave2$car_access
wave2$crossing_2 = wave2$crossing
wave2$crossing_n_2 = wave2$crossing_n
wave2$mixed_2 = wave2$mixed
wave2$mixed_n_2 = wave2$mixed_n
wave2$walkscore_2 = wave2$walkscore
wave2$walkscore_n_2 = wave2$walkscore_n
wave2$nvaliddays_2 = wave2$nvaliddays
wave2$mvpa_10_2 = wave2$mvpa_e5s_b10m80_t100_enmo
wave2$mvpa_5_2 = wave2$mvpa_e5s_b5m80_t100_enmo
wave2$id_nhood_2 = wave2$ID_NHOOD
wave2$nhh_2 = wave2$nhh


# LIVING WITH OTHERS

# create binary vars for living with others vs. alone

# T2

wave2$livotht2 <- NA
wave2$livotht2[wave2$nhh_2 >= 2] <- 1
wave2$livotht2[wave2$nhh_2 == 1] <- 0

#create subset of wave 2 data that includes additional vars of interest and id var
wave2subset <- select(wave2, id_merge, car_access_2, crossing_2, crossing_n_2, 
                      mixed_2, mixed_n_2, walkscore_2, walkscore_n_2, nvaliddays_2, 
                      mvpa_10_2, mvpa_5_2, id_nhood_2, livotht2)

#merge subsets of wave1 and wave2 data with new vars
w1w2 <- merge(wave1subset, wave2subset, by="id_merge", all = FALSE)


#w1w2$inhdhw <- 0
#w1w2$inhdhw[(w1w2$id_nhood_1 == 'hd' & w1w2$id_nhood_2 == 'H') |
 #             (w1w2$id_nhood_1 == 'hw' & w1w2$id_nhood_2 == 'O')] <- 1

#create indicator for whether respondent was in Homewood (control "C") or Hill (intervention "I") at 
#both waves based on GIS data from "movers" data
both$inhdhw <- 0
both$inhdhw[(both$geo_nhd2 == 'C' | both$geo_nhd2 == 'I') 
            & (both$geo_nhd4 == 'C' | both$geo_nhd4 == 'I')] <- 1

table(both$inhdhw)

# filter both to include only respondents who were in Homewood or Hill at both waves (because
# respondents who weren't in one of those nhoods wouldn't have street segment data)

bothinhdhw <- filter(both, inhdhw == 1)

# merge data frames by id and limit data to include cases in both and w1w2 dfs
w2hdhw <- merge(bothinhdhw, w1w2, by="id_merge", all = FALSE)


#examine proportion of missing values on street segment vars within inhdhw ==1 and inhdhw==0 to see if 
#missingness is almost exclusively among those with inhdhw == 0 (not in Hill or Hw)

wave1 %>% group_by(id_nhood_1) %>% summarise(mean(is.na(activetrans_n)))

wave2 %>% group_by(id_nhood_2) %>% summarise(mean(is.na(activetrans_n)))

#DIFF SCORES: create difference scores for vars in both wave1 and wave2

# OUTCOME
# psych distress as measured by k-6
w2hdhw$k6_diff = w2hdhw$k6_2 - w2hdhw$k6_1

#create categories of changers based on movement between categories of psych distress vs. not
#1 = negative at T1 and T2
#2 = positive at T1 and negative at T2
#3 = negative at T1 and positive at T2
#4 = positive at T1 and T2
w2hdhw$k6_distresscat[w2hdhw$k6_distress_1 == 0 & w2hdhw$k6_distress_2 == 0] <- 1
w2hdhw$k6_distresscat[w2hdhw$k6_distress_1 == 1 & w2hdhw$k6_distress_2 == 0] <- 2
w2hdhw$k6_distresscat[w2hdhw$k6_distress_1 == 0 & w2hdhw$k6_distress_2 == 1] <- 3
w2hdhw$k6_distresscat[w2hdhw$k6_distress_1 == 1 & w2hdhw$k6_distress_2 == 1] <- 4

# create binary categories for each category of distress
w2hdhw$k6_distresscat1[w2hdhw$k6_distresscat==1] <- 1
w2hdhw$k6_distresscat1[w2hdhw$k6_distresscat==2 | w2hdhw$k6_distresscat==3 | w2hdhw$k6_distresscat==4] <- 0

w2hdhw$k6_distresscat2[w2hdhw$k6_distresscat==2] <- 1
w2hdhw$k6_distresscat2[w2hdhw$k6_distresscat==1 | w2hdhw$k6_distresscat==3 | w2hdhw$k6_distresscat==4] <- 0

w2hdhw$k6_distresscat3[w2hdhw$k6_distresscat==3] <- 1
w2hdhw$k6_distresscat3[w2hdhw$k6_distresscat==1 | w2hdhw$k6_distresscat==2 | w2hdhw$k6_distresscat==4] <- 0

w2hdhw$k6_distresscat4[w2hdhw$k6_distresscat==4] <- 1
w2hdhw$k6_distresscat4[w2hdhw$k6_distresscat==1 | w2hdhw$k6_distresscat==2 | w2hdhw$k6_distresscat==3] <- 0

#INDIVIDUAL CHARACTERISTICS

# MARITAL STATUS

# for descriptive stats, create var to represent categories of change in marital status:
# 1 = married at T1 and T2
# 2 = not married at T1 and T2
# 3 = married at T1 and not married at T2
# 4 = not married at T1 and married at T2

w2hdhw$mar_diffcat[w2hdhw$i_married_1 == 1 & w2hdhw$married_2 == 1] <- 1
w2hdhw$mar_diffcat[w2hdhw$i_married_1 == 0 & w2hdhw$married_2 == 0] <- 2
w2hdhw$mar_diffcat[w2hdhw$i_married_1 == 1 & w2hdhw$married_2 == 0] <- 3
w2hdhw$mar_diffcat[w2hdhw$i_married_1 == 0 & w2hdhw$married_2 == 1] <- 4

# create binary vars to represent different categories of change in marital status
w2hdhw$mar_diffcat1[w2hdhw$mar_diffcat==1] <- 1
w2hdhw$mar_diffcat1[w2hdhw$mar_diffcat==2 | w2hdhw$mar_diffcat==3 | w2hdhw$mar_diffcat==4] <- 0

w2hdhw$mar_diffcat2[w2hdhw$mar_diffcat==2] <- 1
w2hdhw$mar_diffcat2[w2hdhw$mar_diffcat==1 | w2hdhw$mar_diffcat==3 | w2hdhw$mar_diffcat==4] <- 0

w2hdhw$mar_diffcat3[w2hdhw$mar_diffcat==3] <- 1
w2hdhw$mar_diffcat3[w2hdhw$mar_diffcat==1 | w2hdhw$mar_diffcat==2 | w2hdhw$mar_diffcat==4] <- 0

w2hdhw$mar_diffcat4[w2hdhw$mar_diffcat==4] <- 1
w2hdhw$mar_diffcat4[w2hdhw$mar_diffcat==1 | w2hdhw$mar_diffcat==2 | w2hdhw$mar_diffcat==3] <- 0

# for regression models, create var to represent change in marital status: 1 = not married at T1, married at T2; 0 = 
# no change in marital status, -1 = married at T1, not married at T2

w2hdhw$mar_diff[w2hdhw$i_married_1 == 0 & w2hdhw$married_2 == 1] <- 1
w2hdhw$mar_diff[(w2hdhw$i_married_1 == 0 & w2hdhw$married_2 == 0) |
                    (w2hdhw$i_married_1 == 1 & w2hdhw$married_2 == 1)] <- 0
w2hdhw$mar_diff[w2hdhw$i_married_1 == 1 & w2hdhw$married_2 == 0] <- -1

# create dummy-coded vars with reference category set to no change in marital status
w2hdhw$mar_gain <- 0
w2hdhw$mar_gain[w2hdhw$mar_diff==1] <- 1

w2hdhw$mar_loss <- 0
w2hdhw$mar_loss[w2hdhw$mar_diff==-1] <- 1

table(w2hdhw$mar_diff, w2hdhw$mar_gain)
table(w2hdhw$mar_diff, w2hdhw$mar_loss)

# LIVING WITH OTHERS

# for descriptive stats, create vars to represent categories of change in living with others:
# 1 = living with others at T1 and T2
# 2 = living alone at T1 and T2
# 3 = living with others at T1 and alone at T2
# 4 = living alone at T1 and with others at T2

w2hdhw$livothdiffcat[w2hdhw$livotht1==1 & w2hdhw$livotht2==1] <- 1
w2hdhw$livothdiffcat[w2hdhw$livotht1==0 & w2hdhw$livotht2==0] <- 2
w2hdhw$livothdiffcat[w2hdhw$livotht1==1 & w2hdhw$livotht2==0] <- 3
w2hdhw$livothdiffcat[w2hdhw$livotht1==0 & w2hdhw$livotht2==1] <- 4

# create binary variables for computation of SEs
w2hdhw$livothdiffcat1[w2hdhw$livothdiffcat==1] <- 1
w2hdhw$livothdiffcat1[w2hdhw$livothdiffcat==2 | w2hdhw$livothdiffcat==3 | w2hdhw$livothdiffcat==4] <- 0

w2hdhw$livothdiffcat2[w2hdhw$livothdiffcat==2] <- 1
w2hdhw$livothdiffcat2[w2hdhw$livothdiffcat==1 | w2hdhw$livothdiffcat==3 | w2hdhw$livothdiffcat==4] <- 0

w2hdhw$livothdiffcat3[w2hdhw$livothdiffcat==3] <- 1
w2hdhw$livothdiffcat3[w2hdhw$livothdiffcat==1 | w2hdhw$livothdiffcat==2 | w2hdhw$livothdiffcat==4] <- 0

w2hdhw$livothdiffcat4[w2hdhw$livothdiffcat==4] <- 1
w2hdhw$livothdiffcat4[w2hdhw$livothdiffcat==1 | w2hdhw$livothdiffcat==2 | w2hdhw$livothdiffcat==3] <- 0


#for regression models, create vars to represent change in living with others from T1 to T2: 
#1 = changed from living alone at T1 to living with others at T2
#0 = no change in living with others vs. alone
#-1 = changed from living with others at T1 to living alone at T2
w2hdhw$liv_diff <- NA
w2hdhw$liv_diff[w2hdhw$nhh_1 == 1 & w2hdhw$nhh_2 >= 2] <- 1
w2hdhw$liv_diff[(w2hdhw$nhh_1 == 1 & w2hdhw$nhh_2 == 1) | 
                  (w2hdhw$nhh_1 >= 2 & w2hdhw$nhh_2 >= 2)] <- 0
w2hdhw$liv_diff[w2hdhw$nhh_1 >= 2 & w2hdhw$nhh_2 == 1] <- -1

# create dummy-coded vars for change in living with others with no change as reference category
w2hdhw$liv_gain <- NA
w2hdhw$liv_gain[w2hdhw$liv_diff == 0 | w2hdhw$liv_diff == -1] <- 0
w2hdhw$liv_gain[w2hdhw$liv_diff == 1] <- 1

w2hdhw$liv_loss <- NA
w2hdhw$liv_loss[w2hdhw$liv_diff == 0 | w2hdhw$liv_diff == 1] <- 0
w2hdhw$liv_loss[w2hdhw$liv_diff == -1] <- 1

#per capita annual household income 
w2hdhw$i_adj_inc2_diff = w2hdhw$i_adj_inc2_2 - w2hdhw$i_adj_inc2_1

#vehicle access
table(w2hdhw$i_car_access_1, w2hdhw$car_access_2)

# for descriptive stats, create var to represent categories of change in car access
# 1 = had car access at T1 and T2
# 2 = lacked car access at T1 and T2
# 3 = had car access at T1 and lacked car access at T2
# 4 = lacked car access at T1 and had car access at T2

w2hdhw$caraccessdiffcat[w2hdhw$i_car_access_1==1 & w2hdhw$car_access_2==1] <- 1
w2hdhw$caraccessdiffcat[w2hdhw$i_car_access_1==0 & w2hdhw$car_access_2==0] <- 2
w2hdhw$caraccessdiffcat[w2hdhw$i_car_access_1==1 & w2hdhw$car_access_2==0] <- 3
w2hdhw$caraccessdiffcat[w2hdhw$i_car_access_1==0 & w2hdhw$car_access_2==1] <- 4

# create binary variables for change in car access
w2hdhw$caraccessdiffcat1[w2hdhw$caraccessdiffcat==1] <- 1
w2hdhw$caraccessdiffcat1[w2hdhw$caraccessdiffcat==2 | w2hdhw$caraccessdiffcat==3 | w2hdhw$caraccessdiffcat==4] <- 0

w2hdhw$caraccessdiffcat2[w2hdhw$caraccessdiffcat==2] <- 1
w2hdhw$caraccessdiffcat2[w2hdhw$caraccessdiffcat==1 | w2hdhw$caraccessdiffcat==3 | w2hdhw$caraccessdiffcat==4] <- 0

w2hdhw$caraccessdiffcat3[w2hdhw$caraccessdiffcat==3] <- 1
w2hdhw$caraccessdiffcat3[w2hdhw$caraccessdiffcat==1 | w2hdhw$caraccessdiffcat==2 | w2hdhw$caraccessdiffcat==4] <- 0

w2hdhw$caraccessdiffcat4[w2hdhw$caraccessdiffcat==4] <- 1
w2hdhw$caraccessdiffcat4[w2hdhw$caraccessdiffcat==1 | w2hdhw$caraccessdiffcat==2 | w2hdhw$caraccessdiffcat==3] <- 0

#create var to represent change in car access: 1 = gained car access, 0 = no change
# in car access, -1 = lost car access
w2hdhw$car_access_diff[w2hdhw$i_car_access_1==0 & w2hdhw$car_access_2==1] <- 1
w2hdhw$car_access_diff[(w2hdhw$i_car_access_1==0 & w2hdhw$car_access_2==0) | 
                           (w2hdhw$i_car_access_1==1 & w2hdhw$car_access_2==1)] <- 0
w2hdhw$car_access_diff[w2hdhw$i_car_access_1==1 & w2hdhw$car_access_2==0] <- -1

#create dummy-coded vars with reference category set to no change in car access
w2hdhw$car_access_gain[w2hdhw$car_access_diff==1] <- 1
w2hdhw$car_access_gain[(w2hdhw$car_access_diff==0) | (w2hdhw$car_access_diff==-1)] <- 0

w2hdhw$car_access_loss[w2hdhw$car_access_diff==-1] <- 1
w2hdhw$car_access_loss[(w2hdhw$car_access_diff==1) | (w2hdhw$car_access_diff==0)] <- 0

table(w2hdhw$car_access_gain, w2hdhw$car_access_loss)

#bmi
w2hdhw$bmi_diff = w2hdhw$bmi_2 - w2hdhw$bmi_1

#ENVIRONMENTAL/NEIGHBORHOOD CHARACTERISTICS

#NEWS-A infrastructure scale (walkability)
w2hdhw$newsa_is_diff = w2hdhw$newsa_is_2 - w2hdhw$newsa_is_1

#NEWS-A neighborhood aesthetics (surroundings)
w2hdhw$newsa_surround_diff = w2hdhw$newsa_surround_2 - w2hdhw$newsa_surround_1

#Social cohesion among neighbors
w2hdhw$soc_cohesion_diff = w2hdhw$soc_cohesion_2 - w2hdhw$soc_cohesion_1

#perceived neighborhood safety
w2hdhw$nbhd_safety_diff = w2hdhw$nbhd_safety_2 - w2hdhw$nbhd_safety_1

#crime--difference between total crime in 2015 (year before Plus follow-up)
#and 2012 (year before Plus baseline)
w2hdhw$total_crime_diff = w2hdhw$total_crime2015 - w2hdhw$total_crime2012

#street segment variables of potential interest

#active transport
w2hdhw$activetrans_n_diff = w2hdhw$activetrans_n_2 - w2hdhw$activetrans_n_1

#aesthetics
w2hdhw$aesthetics_n_diff= w2hdhw$aesthetics_n_2 - w2hdhw$aesthetics_n_1

#amenities
w2hdhw$amenities_n_diff = w2hdhw$amenities_n_2 - w2hdhw$amenities_n_1

#crossings
w2hdhw$crossing_n_diff = w2hdhw$crossing_n_2 - w2hdhw$crossing_n_1

#mixed land use
w2hdhw$mixed_n_diff = w2hdhw$mixed_n_2 - w2hdhw$mixed_n_1

#walkability
w2hdhw$walkscore_n_diff = w2hdhw$walkscore_n_2 - w2hdhw$walkscore_n_1

#convert all missing values to NA
w2hdhw[ is.na(w2hdhw) ] <- NA

#create indicator for whether respondents had missing data on any variables included in longitudinal model
w2hdhw$missany2 <- 0
w2hdhw$missany2[is.na(w2hdhw$k6_diff == 1) | 
                   is.na(w2hdhw$Age == 1) | 
                   is.na(w2hdhw$male == 1) | 
                   is.na(w2hdhw$mar_diff == 1) | 
                   is.na(w2hdhw$liv_diff == 1) | 
                   is.na(w2hdhw$i_highest_edu_1 == 1) | 
                   is.na(w2hdhw$i_adj_inc2_diff == 1) | 
                   is.na(w2hdhw$car_access_diff == 1) |
                   is.na(w2hdhw$bmi_diff == 1) |
                   is.na(w2hdhw$newsa_is_diff == 1) |
                   is.na(w2hdhw$newsa_surround_diff == 1) |
                   is.na(w2hdhw$soc_cohesion_diff == 1) |
                   is.na(w2hdhw$nbhd_safety_diff == 1) |
                   is.na(w2hdhw$total_crime_diff == 1) |
                   is.na(w2hdhw$activetrans_n_diff == 1) |
                   is.na(w2hdhw$aesthetics_n_diff == 1) |
                   is.na(w2hdhw$amenities_n_diff == 1) |
                   is.na(w2hdhw$crossing_n_diff == 1) |
                   is.na(w2hdhw$mixed_n_diff == 1) |
                   is.na(w2hdhw$walkscore_n_diff == 1)] <- 1

table(w2hdhw$missany2)

#filter both_new to include only cases with complete data on individual and environmental characteristics in longitudinal model
both_new <- filter(w2hdhw, missany2 == 0)

#extract variable missw2 from both_new and merge back to w1hdhw
both_new1 <- select(both_new, id_merge, missany2, k6_diff)

w1hdhw <- merge(w1hdhw, both_new1, by="id_merge", all = TRUE)

#create variable nomissw1 that is reverse-scored version of missany1 for creation of nonresponse weights
#1 = no missingness on any variables in cross-sectional model, 0 = missingness on at least one variable in model

w1hdhw$nomissw1[w1hdhw$missany1==1] <- 0
w1hdhw$nomissw1[w1hdhw$missany1==0] <- 1

#create variable nomissw2 that is reverse-scored version of missany2 
#to indicate any missingness on variables in longitudinal model and use in nonresponse weights
# 1 = no missingness on any variables in model, 0 = missingness on at least one variable in model

w1hdhw$nomissw2 <- 0
w1hdhw$nomissw2[w1hdhw$missany2==0] <- 1


#filter w1hdhw1 to include only cases with complete data on variables in cross-sectional model
#missany1: 1 = missing data on at least one variable in cross-sectional model, 0 = no missing on any vars in model
w1p1 <- filter(w1hdhw, missany1 == 0)

nrow(w1p1)
str(w1p1)

#examine # of cases in each dataset
nrow(w1all)
nrow(w1hdhw)
nrow(w1p1)
nrow(w2hdhw)
nrow(both_new)

# summary of datasets created

#WAVE 1 only datasets
#w1all = all wave 1 survey completers (N = 1,051)
#w1hdhw = wave 1 survey completers in Hill or Homewood based on GIS data (N = 980)
#w1p1 = wave 1 survey completers in Hill or Homewood who have complete data on all vars in cross-sectional model (N = 894)

#WAVE 2 datasets
#w2hdhw = wave 2 and 1 survey completers in Hill or Homewood at both waves based on GIS data (N = 612)
#both_new = wave 2 and 1 survey completers in Hill or Homewood at both waves with complete data on all vars in longitudinal model (N = 544)

# COMPARE FULL SAMPLE OF WAVE 1 COMPLETERS IN HILL AND HWD (w1hdhw) TO WAVE 1 SUBSET WITH COMPLETE DATA ON ALL MODEL VARS (w1p1)

# T1 (w1hdhw)

describe(w1hdhw$male)
describe(w1hdhw$race_black_1)
describe(w1hdhw$Age)
describe(w1hdhw$i_married_1)
describe(w1hdhw$livotht1)
prop.table(table(w1hdhw$i_highest_edu_1))
describe(w1hdhw$i_car_access_1)
describe(w1hdhw$i_adj_inc2_1)
describe(w1hdhw$bmi_1)
describe(w1hdhw$k6_1)
describe(w1hdhw$newsa_is_1)
describe(w1hdhw$newsa_surround_1)
describe(w1hdhw$soc_cohesion_1)
describe(w1hdhw$nbhd_safety_1)
describe(w1hdhw$total_crime2012)
describe(w1hdhw$activetrans_n_1)
describe(w1hdhw$aesthetics_n_1)
describe(w1hdhw$amenities_n_1)
describe(w1hdhw$crossing_n_1)
describe(w1hdhw$mixed_n_1)
describe(w1hdhw$walkscore_n_1)
# note that street segment vars (activetrans_n through walkscore_n) have higher rates of missingness than other variables


# T1 (w1p1)

describe(w1p1$male)
describe(w1p1$race_black_1)
describe(w1p1$Age)
describe(w1p1$i_married_1)
describe(w1p1$livotht1)
prop.table(table(w1p1$i_highest_edu_1))
describe(w1p1$i_car_access_1)
describe(w1p1$i_adj_inc2_1)
describe(w1p1$bmi_1)
describe(w1p1$k6_1)
describe(w1p1$newsa_is_1)
describe(w1p1$newsa_surround_1)
describe(w1p1$soc_cohesion_1)
describe(w1p1$nbhd_safety_1)
describe(w1p1$total_crime2012)

#create subset of data that includes only individual and environmental predictors to prepare for lapply
crossseccor <- select(w1p1, Age, male, i_married_1, livotht1, i_highest_edu_1, i_adj_inc2_1, i_car_access_1, 
                      bmi_1, newsa_is_1, newsa_surround_1, soc_cohesion_1, nbhd_safety_1, total_crime2012,
                      aesthetics_n_1, amenities_n_1, walkscore_n_1, hill)

#create function within lapply to run correlations between amenities and aesthetics and other predictors to see
#if any are candidates to use for multiple imputation of missing data on amenities and aesthetics

lapply(crossseccor, function(x) {
  y <- cor(w1p1$amenities_n_1, x)
  return(y)
})
#max correlation between amenities and other variables = .24 (Age)

lapply(crossseccor, function(x) {
  y <- cor(w1p1$aesthetics_n_1, x)
  return(y)
})

# max correlation between aesthetics and other variables = .25 (Age)

# no other predictors appear to be good candidates for inclusion in multiple imputation model

# differences between w1hdhw and w1p1 are not large, but sample loss (1 - 894/980) is 8.8%--enough to indicate that non-response weights
# may be benefical

# CREATE NONRESPONSE WEIGHTS FOR CROSS-SECTIONAL WAVE 1 SUBSET 

# NONRESPONSE WEIGHTS = 1/p of response
#use ps to model response
#let respondents be treated group (as analog to propensity score creation--average treated effect (ATE))
crswt.ps <- ps(nomissw1 ~ male + Age + i_married_1 + nhh_1 + i_yrs_neigh_1 + hill + 
                 i_adj_inc2_1 + i_employed_1 + i_car_access_1 + bmi_1, 
               data=w1hdhw, 
               stop.method=c("es.mean","ks.max"),
               n.trees=5000,
               verbose=FALSE,
               estimand="ATE")

#n.trees: specifies the maximum number of iterations that gbm will run. 
#ps() will issue a warning if the estimated optimal number of iterations is too close to the bound selected in this argument because it indicates that balance may improve if more
#complex models (i.e., those with more trees) are considered. Increase n.trees or decrease shrinkage if this warning appears.

# interaction.depth: controls the level of interactions allowed in the GBM. The default is 3.

#shrinkage: used to enhance the smoothness of resulting model. The shrinkage argument controls the amount of shrinkage. 
#Small values such as 0.005 or 0.001 yield smooth fits but require greater values of n.trees to achieve adequate fits.

#plot weights to:
#check that n.trees was set large enough (so that balance would not seem to be improved if more complex models were considered)
#for es.mean.ATE, the measure is the average effect size difference between the two groups (respondents and non-respondents)
#for ks.max.ATE, the measure is the largest of the KS statistics

#As a default, the plot() function applied to a ps object gives the balance measures as a function of the number of iterations
#in the GBM algorithm, with higher iterations corresponding to more complicated fitted models.  

plot(crswt.ps)

#in this plot, the average effect size difference (es.mean.ATE) is minimized by roughly 1,000 iterations and then increases 
#with additional iterations--no evidence that balance would be improved by increasing # of iterations, so n.trees used is okay
#same conclusion based on ks.max.ATE--roughly 1,000 iterations minimized the largest of the KS statistics computed for the
#covariates

#summary outputs the sample sizes of the groups and balance measures
summary(crswt.ps)

#extract weights as object using "get.weights" function 
w1hdhw$crsw <- get.weights(crswt.ps, stop.method="es.mean")

#generate balance table to compare participants with and without missing data with and without weights
#to see if weights effectively reduce differences between participants with and without missing data

#need to create new dataframe with full sample and respondents stacked into a dataframe--need to "trick" the program to run
#balance table for non-response weights instead of propensity weights for which it is designed by default
#create pseudo-indicator nr2 to indicate pseudo-treatment group (full sample; nr2=1) and pseudo-control group (respondents; nr2=0)

w1hdhw3 <- rbind(data.frame(w1hdhw, nr3=1, wgt3=1), 
                 data.frame(w1hdhw, nr3=0, wgt3=w1hdhw$crsw)[w1hdhw$nomissw1==1,])

#run dx.wts() to obtain balance statistics

crswtdxwts <- dx.wts(x=w1hdhw3$wgt3, 
                     data=w1hdhw3,
                     estimand="ATT",
                     vars=c("male", "Age", "i_married_1", "nhh_1", "i_yrs_neigh_1", "hill", 
                            "i_adj_inc2_1", "i_employed_1", "i_car_access_1", "bmi_1"),
                     treat.var="nr3")

crswt.balance <- bal.table(crswtdxwts)[[2]][,c("tx.mn", "ct.mn", "std.eff.sz", "ks")]
names(crswt.balance) <- c("Overall Sample", "Weighted responders", "Std ES", "KS")
xtable(crswt.balance,
       caption = "Balance of the nonrespondents and respondents",
       label = "tab:balance2",
       digits = c(0, 2, 2, 2, 2),
       align = c("1", "r", "r", "r", "r"))

#evaluate balance--compare unweighted respondents to weighted respondents
crswt.balance #note that the full sample (represented as the tx group) very closely resembles the respondents (control group)

# extract weights from w1hdhw and merge back into w1p1 for substantive analyses

#create subset of w1hdhw that includes only weights for wave 1 cross-sectional analyses and id variable
wave1weight <- select(w1hdhw, id_merge, crsw)
summary(wave1weight)


#merge wave1weight back in with w1p1 and limit to include only cases in both datasets for final N of 894
w1p1 <- merge(w1p1, wave1weight, by="id_merge", all = FALSE)

# GIVEN AMOUNT OF ATTRITION THAT OCCURRED FROM WAVE 1 TO WAVE 2 AND MISSINGNESS ON WAVE 2 VARS, CREATE NONRESPONSE WEIGHTS FOR LONGITUDINAL SUBSET FOR MODEL TO PREDICT CHANGE IN DISTRESS FROM W1 TO W2

# NONRESPONSE WEIGHTS = 1/p of response
#use ps to model response
#let respondents be treated group (as analog to propensity score creation--average treated effect (ATE))
longwt.ps <- ps(nomissw2 ~ male + Age + i_married_1 + nhh_1 + i_yrs_neigh_1 + hill + 
                  i_adj_inc2_1 + i_employed_1 + i_car_access_1 + bmi_1, 
                data=w1hdhw, 
                stop.method=c("es.mean","ks.max"),
                n.trees=5000,
                verbose=FALSE,
                estimand="ATE")


#n.trees: specifies the maximum number of iterations that gbm will run. 
#ps() will issue a warning if the estimated optimal number of iterations is too close to the bound selected in this argument because it indicates that balance may improve if more
#complex models (i.e., those with more trees) are considered. Increase n.trees or decrease shrinkage if this warning appears.

# interaction.depth: controls the level of interactions allowed in the GBM. The default is 3.

#shrinkage: used to enhance the smoothness of resulting model. The shrinkage argument controls the amount of shrinkage. 
#Small values such as 0.005 or 0.001 yield smooth fits but require greater values of n.trees to achieve adequate fits.

#plot weights to:
#check that n.trees was set large enough (so that balance would not seem to be improved if more complex models were considered)
#for es.mean.ATE, the measure is the average effect size difference between the two groups (respondents and non-respondents)
#for ks.max.ATE, the measure is the largest of the KS statistics

#As a default, the plot() function applied to a ps object gives the balance measures as a function of the number of iterations
#in the GBM algorithm, with higher iterations corresponding to more complicated fitted models.  

plot(longwt.ps)

#in this plot, the average effect size difference (es.mean.ATE) is minimized by a little over 1,000 iterations and then increases 
#with additional iterations--no evidence that balance would be improved by increasing # of iterations, so n.trees used is okay
#same conclusion based on ks.max.ATE--a little over 1,000 iterations minimized the largest of the KS statistics computed for the
#covariates


#summary outputs the sample sizes of the groups and balance measures
summary(longwt.ps)

#extract weights as object using "get.weights" function 
w1hdhw$longw <- get.weights(longwt.ps, stop.method="es.mean")

#generate balance table to compare participants with and without missing data with and without weights
#to see if weights effectively reduce differences between participants with and without missing data

#need to create new dataframe with full sample and respondents stacked into a dataframe--need to "trick" the program to run
#balance table for non-response weights instead of propensity weights for which it is designed by default
#create pseudo-indicator nr2 to indicate pseudo-treatment group (full sample; nr2=1) and pseudo-control group (respondents; nr2=0)

w1hdhw2 <- rbind(data.frame(w1hdhw, nr2=1, wgt2=1), 
                 data.frame(w1hdhw, nr2=0, wgt2=w1hdhw$longw)[w1hdhw$nomissw2==1,])


#run dx.wts() to obtain balance statistics

longwtdxwts <- dx.wts(x=w1hdhw2$wgt2, 
                      data=w1hdhw2,
                      estimand="ATT",
                      vars=c("male", "Age", "i_married_1", "nhh_1", "i_yrs_neigh_1", "hill", 
                             "i_adj_inc2_1", "i_employed_1", "i_car_access_1", "bmi_1"),
                      treat.var="nr2")

longwt.balance <- bal.table(longwtdxwts)[[2]][,c("tx.mn", "ct.mn", "std.eff.sz", "ks")]
names(longwt.balance) <- c("OverallS Sample", "Weighted responders", "Std ES", "KS")
xtable(longwt.balance,
       caption = "Balance of the nonrespondents and respondents",
       label = "tab:balance2",
       digits = c(0, 2, 2, 2, 2),
       align = c("1", "r", "r", "r", "r"))


#evaluate balance--compare unweighted respondents to weighted respondents
longwt.balance #note that the full sample (represented as the tx group) very closely resembles the respondents (control group)

#create subset of w1hdhw that includes only weights for longitudinal analyses and id variable
longweight <- select(w1hdhw, id_merge, longw)
summary(longweight)

#merge longweight back in with both_new and limit to include only cases in both datasets for final N of 544
both_new <- merge(both_new, longweight, by="id_merge", all = FALSE)

describe(both_new$longw)

# write out the 2 main datasets to be used in analyses 

w1p1 <- write.csv(w1p1, 'w1p1.csv')

both_new <- write.csv(both_new, 'both_new.csv')






