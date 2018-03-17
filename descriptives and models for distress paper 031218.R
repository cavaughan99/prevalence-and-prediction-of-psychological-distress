#cross-sectional and longitudinal analyses of 
#individual and environmental predictors of psychological distress in PHRESH Plus

#install package
install.packages("arm")

#load packages
library(dplyr)
library(psych)
library(sas7bdat)
library(survey)
library(gmodels)
library(ggplot2)
library(arm)

#set working directory
setwd("~/PHRESH and greenspace/PHRESH Plus papers/neighborhoods and depression/data")

# USE w1p1 (N = 894) for all wave 1 cross-sectional analyses with weight crsw to represent full sample of Hill and Homewood residents at wave 1 (N = 980)
# USE both_new (N = 544) for all longitudinal analyses with weight 

# read in data files
w1p1 <- read.csv("w1p1.csv")
both_new <- read.csv("both_new.csv")

# DESCRIPTIVE ANALYSES WITH WEIGHTS

# T1 (w1p1)

# create design object for use in weighted analyses
designwave1 <- svydesign(ids=~1, weights=~crsw, data=w1p1)

# DEMOGRAPHIC CHARACTERISTICS 
# output weighted proportions and 95% CIs
options(digits=10)
svyciprop(~male, designwave1, method="logit") 
svyciprop(~race_black_1, designwave1, method="logit")

# output weighted mean and 95% CI
svymean(~Age, designwave1, na.rm=TRUE)
confint(svymean(~Age, designwave1, na.rm=TRUE))

# SOCIOECONOMIC STATUS
svyciprop(~college_1, designwave1, na.rm=TRUE)
svyciprop(~i_car_access_1, designwave1, na.rm=TRUE)
svymean(~i_adj_inc2_1, designwave1, na.rm=TRUE)
confint(svymean(~i_adj_inc2_1, designwave1, na.rm=TRUE))

# HOUSEHOLD COMPOSITION
svyciprop(~i_married_1, designwave1, na.rm=TRUE)
svyciprop(~livotht1, designwave1, na.rm=TRUE)

# WEIGHT STATUS AND BMI
svymean(~bmi_1, designwave1, na.rm=TRUE)
confint(svymean(~bmi_1, designwave1, na.rm=TRUE))

# PSYCHOLOGICAL DISTRESS
svyciprop(~k6_distress_1, designwave1, na.rm=TRUE)

svymean(~k6_1, designwave1, na.rm=TRUE)
confint(svymean(~k6_1, designwave1, na.rm=TRUE))

# PERCEIVED NEIGHBORHOOD CHARACTERISTICS
svymean(~newsa_is_1, designwave1, na.rm=TRUE)
confint(svymean(~newsa_is_1, designwave1, na.rm=TRUE))

svymean(~newsa_surround_1, designwave1, na.rm=TRUE)
confint(svymean(~newsa_surround_1, designwave1, na.rm=TRUE))

svymean(~soc_cohesion_1, design=designwave1, na.rm=TRUE)
confint(svymean(~soc_cohesion_1, design=designwave1, na.rm=TRUE))

svymean(~nbhd_safety_1, design=designwave1, na.rm=TRUE)
confint(svymean(~nbhd_safety_1, design=designwave1, na.rm=TRUE))

# OBJECTIVELY-MEASURED NEIGHBORHOOD CHARACTERISTICS
svymean(~total_crime2012, design=designwave1, na.rm=TRUE)
confint(svymean(~total_crime2012, design=designwave1, na.rm=TRUE))

svymean(~activetrans_n_1, design=designwave1, na.rm=TRUE)
confint(svymean(~activetrans_n_1, design=designwave1, na.rm=TRUE))

svymean(~amenities_n_1, design=designwave1, na.rm=TRUE)
confint(svymean(~amenities_n_1, design=designwave1, na.rm=TRUE))

svymean(~aesthetics_n_1, design=designwave1, na.rm=TRUE)
confint(svymean(~aesthetics_n_1, design=designwave1, na.rm=TRUE))

svymean(~crossing_n_1, design=designwave1, na.rm=TRUE)
confint(svymean(~crossing_n_1, design=designwave1, na.rm=TRUE))

svymean(~mixed_n_1, design=designwave1, na.rm=TRUE)
confint(svymean(~mixed_n_1, design=designwave1, na.rm=TRUE))

svymean(~walkscore_n_1, design=designwave1, na.rm=TRUE)
confint(svymean(~walkscore_n_1, design=designwave1, na.rm=TRUE))

# T2 (both_new)

# create design object for use in weighted analyses of longitudinal subset with complete data on all vars in longitudinal models
designphr <- svydesign(ids=~1, weights=~longw, data=both_new)

# SOCIOECONOMIC STATUS
svymean(both_new$i_adj_inc2_2, designphr, na.rm=TRUE)
describe(both_new$i_adj_inc2_2)

# WEIGHT STATUS AND BMI
svymean(both_new$bmi_2, designphr, na.rm=TRUE)

# PSYCHOLOGICAL DISTRESS
svymean(both_new$k6_2, designphr, na.rm=TRUE)

# PERCEIVED NEIGHBORHOOD CHARACTERISTICS
svymean(both_new$newsa_is_2, designphr, na.rm=TRUE)
svymean(both_new$newsa_surround_2, designphr, na.rm=TRUE)
svymean(both_new$soc_cohesion_2, design=designphr, na.rm=TRUE)
svymean(both_new$nbhd_safety_2, design=designphr, na.rm=TRUE)

# OBJECTIVELY-MEASURED NEIGHBORHOOD CHARACTERISTICS
svymean(both_new$total_crime2015, design=designphr, na.rm=TRUE)
svymean(both_new$activetrans_n_2, design=designphr, na.rm=TRUE)
svymean(both_new$aesthetics_n_2, design=designphr, na.rm=TRUE)
svymean(both_new$amenities_n_2, design=designphr, na.rm=TRUE)
svymean(both_new$crossing_n_2, design=designphr, na.rm=TRUE)
svymean(both_new$mixed_n_2, design=designphr, na.rm=TRUE)
svymean(both_new$walkscore_n_2, design=designphr, na.rm=TRUE)

# CHANGE FROM T1 TO T2 (DESCRIPTIVES, both_new)

# SOCIOECONOMIC STATUS

# car access
# 1 = had car access at T1 and T2
# 2 = lacked car access at T1 and T2
# 3 = had car access at T1 and lacked car access at T2
# 4 = lacked car access at T1 and had car access at T2
svyciprop(~caraccessdiffcat1, designphr, na.rm=TRUE)
svyciprop(~caraccessdiffcat2, designphr, na.rm=TRUE)
svyciprop(~caraccessdiffcat3, designphr, na.rm=TRUE)
svyciprop(~caraccessdiffcat4, designphr, na.rm=TRUE)

# annual per capita household income
svymean(~i_adj_inc2_diff, design=designphr, na.rm=TRUE)
confint(svymean(~i_adj_inc2_diff, design=designphr, na.rm=TRUE))

# HOUSEHOLD COMPOSITION

# marital status
# 1 = married at T1 and T2
# 2 = not married at T1 and T2
# 3 = married at T1 and not married at T2
# 4 = not married at T1 and married at T2
svyciprop(~mar_diffcat1, designphr, na.rm=TRUE)
svyciprop(~mar_diffcat2, designphr, na.rm=TRUE)
svyciprop(~mar_diffcat3, designphr, na.rm=TRUE)
svyciprop(~mar_diffcat4, designphr, na.rm=TRUE)

# living with others
# 1 = living with others at T1 and T2
# 2 = living alone at T1 and T2
# 3 = living with others at T1 and alone at T2
# 4 = living alone at T1 and with others at T2
svyciprop(~livothdiffcat1, designphr, na.rm=TRUE)
svyciprop(~livothdiffcat2, designphr, na.rm=TRUE)
svyciprop(~livothdiffcat3, designphr, na.rm=TRUE)
svyciprop(~livothdiffcat4, designphr, na.rm=TRUE)

# BMI
svymean(~bmi_diff, designphr, na.rm=TRUE)
confint(svymean(~bmi_diff, designphr, na.rm=TRUE))

# PSYCHOLOGICAL DISTRESS
# compute weighted mean of k6 difference scores
svymean(~k6_diff, designphr, na.rm=TRUE)
confint(svymean(~k6_diff, designphr, na.rm=TRUE))

# compute weighted proportions for each category of distress change
#1 = negative for psych distress at T1 and T2
#2 = positive at T1 and negative at T2
#3 = negative at T1 and positive at T2
#4 = positive at T1 and T2
svyciprop(~k6_distresscat1, designphr, na.rm=TRUE)
svyciprop(~k6_distresscat2, designphr, na.rm=TRUE)
svyciprop(~k6_distresscat3, designphr, na.rm=TRUE)
svyciprop(~k6_distresscat4, designphr, na.rm=TRUE)

# PERCEIVED NEIGHBORHOOD CHARACTERISTICS
svymean(~newsa_is_diff, designphr, na.rm=TRUE)
confint(svymean(~newsa_is_diff, designphr, na.rm=TRUE))

svymean(~newsa_surround_diff, designphr, na.rm=TRUE)
confint(svymean(~newsa_surround_diff, designphr, na.rm=TRUE))

svymean(~soc_cohesion_diff, design=designphr, na.rm=TRUE)
confint(svymean(~soc_cohesion_diff, design=designphr, na.rm=TRUE))

svymean(~nbhd_safety_diff, design=designphr, na.rm=TRUE)
confint(svymean(~nbhd_safety_diff, design=designphr, na.rm=TRUE))

# OBJECTIVELY-MEASURED NEIGHBORHOOD CHARACTERISTICS
svymean(~total_crime_diff, design=designphr, na.rm=TRUE)
confint(svymean(~total_crime_diff, design=designphr, na.rm=TRUE))

svymean(~activetrans_n_diff, design=designphr, na.rm=TRUE)
confint(svymean(~activetrans_n_diff, design=designphr, na.rm=TRUE))

svymean(~amenities_n_diff, design=designphr, na.rm=TRUE)
confint(svymean(~amenities_n_diff, design=designphr, na.rm=TRUE))

svymean(~aesthetics_n_diff, design=designphr, na.rm=TRUE)
confint(svymean(~aesthetics_n_diff, design=designphr, na.rm=TRUE))

svymean(~crossing_n_diff, design=designphr, na.rm=TRUE)
confint(svymean(~crossing_n_diff, design=designphr, na.rm=TRUE))

svymean(~mixed_n_diff, design=designphr, na.rm=TRUE)
confint(svymean(~mixed_n_diff, design=designphr, na.rm=TRUE))

svymean(~walkscore_n_diff, design=designphr, na.rm=TRUE)
confint(svymean(~walkscore_n_diff, design=designphr, na.rm=TRUE))

# CROSS-SECTIONAL WAVE 1 MODEL DIAGNOSTICS

#examine distribution of k6_1
hist(w1p1$k6_1)
describe(w1p1$k6_1)
table(w1p1$k6_1)

#cross-sectional model with baseline data: individual characteristics only

crosssecind <- svyglm(k6_1 ~ Age + male + 
                    i_married_1 + livotht1 + 
                    i_highest_edu_1 + i_adj_inc2_1 + i_car_access_1 + 
                    bmi_1 + hill, data=w1p1, design=designwave1)

summary(crosssecind)

#compute vifs for predictors to assess multicollinearity
vif(crosssecind) # all vifs < 2, so no apparent issues with multicollinearity

#examine distribution of residuals to determine whether assumption of normality for linear regression holds
hist(crosssecind$residuals) # slight skew to the right, but generally approximates normal distribution

#cross-sectional model with baseline data: environmental characteristics only
crosssecenv <- svyglm(k6_1 ~ newsa_is_1 + newsa_surround_1 + soc_cohesion_1 + nbhd_safety_1
                  + total_crime2012 + activetrans_n_1 + aesthetics_n_1 + amenities_n_1 
                  + crossing_n_1 + mixed_n_1 + walkscore_n_1 + hill, data=w1p1, design=designwave1)


summary(crosssecenv)

#compute vifs for predictors to assess multicollinearity
vif(crosssecenv)

#predictors in crosssecenv with problematic levels of multicollinearity are activetrans_n_1 (VIF of 19.56), 
#crossing_n_1 (VIF of 18.77), and mixed_n_1 (VIF of 16.42)
cor(w1p1$activetrans_n_1, w1p1$crossing_n_1) #these are correlated at .95--keep only one in the model
cor(w1p1$activetrans_n_1, w1p1$mixed_n_1) #these are correlated at .94--keep only one in the model
cor(w1p1$crossing_n_1, w1p1$mixed_n_1) #these are correlated at .91--keep only one in the model

#trimmed model for environmental characteristics that excludes crossing_n_1 and mixed_n_1 to avert problems with
#multicollinearity

crosssecenvtrim <- svyglm(k6_1 ~ newsa_is_1 + newsa_surround_1 + soc_cohesion_1 + nbhd_safety_1
                      + total_crime2012 + activetrans_n_1 + aesthetics_n_1 + amenities_n_1 
                      + walkscore_n_1 + hill, data=w1p1, design=designwave1)


summary(crosssecenvtrim)

#check multicollinearity of trimmed model
vif(crosssecenvtrim) 

cor(w1p1$activetrans_n_1, w1p1$aesthetics_n_1) #activetrans_n_1 and aesthetics_n_1 are correlated at .92--keep only one

#second trimmed model for environmental chars with multicollinear predictors removed
crosssecenvtrim2 <- svyglm(k6_1 ~ newsa_is_1 + newsa_surround_1 + soc_cohesion_1 + nbhd_safety_1
                       + total_crime2012 + aesthetics_n_1 + amenities_n_1 
                       + walkscore_n_1 + hill, data=w1p1, design=designwave1)

summary(crosssecenvtrim2)

vif(crosssecenvtrim2) #max VIF in ultimate trimmed model is 2.5, so no more changes made

#check normality of residuals
hist(crosssecenvtrim2$residuals) #distribution is slightly skewed to the right, but generally approximates normality

# FINAL WEIGHTED CROSS-SECTIONAL WAVE 1 MODEL WITH INDIVIDUAL AND ENVIRONMENTAL CHARACTERISTICS

crsindenvwgt <- svyglm(k6_1 ~ Age + male + 
                     i_married_1 + livotht1 +
                     i_highest_edu_1 + i_adj_inc2_1 +  i_car_access_1 + 
                     bmi_1 + 
                     newsa_is_1 + newsa_surround_1 + soc_cohesion_1 + nbhd_safety_1 + 
                     total_crime2012 + aesthetics_n_1 + amenities_n_1 + 
                     walkscore_n_1 + hill, data=w1p1, design=designwave1)

summary(crsindenvwgt)

vif(crsindenvwgt) # max VIF is 2.72, so no apparent issues with multicollinearity

hist(crsindenvwgt$residuals) #distribution generally approximates normality

# WEIGHTED LONGITUDINAL MODEL WITH INDIVIDUAL AND ENVIRONMENTAL CHARACTERISTICS

# MODEL DIAGNOSTICS

# examine distribution of outcome (change in psych distress)
hist(both_new$k6_diff)

# longitudinal model: individual characteristics
longind <- svyglm(k6_diff ~ Age + male + 
                mar_gain + mar_loss + liv_gain + liv_loss +
                i_highest_edu_1 + i_adj_inc2_diff + car_access_gain + car_access_loss +
                bmi_diff + hill, data=both_new, design=designphr)
summary(longind)

# assess multicollinearity of predictors
vif(longind) # all vifs <= 1.53, so no issues with multicollinearity

# examine distribution of residuals
hist(longind$residuals) 
describe(longind$residuals) 

# longitudinal model: environmental characteristics
longenv <- svyglm(k6_diff ~ newsa_is_diff + newsa_surround_diff + soc_cohesion_diff + nbhd_safety_diff
                  + total_crime_diff + activetrans_n_diff + aesthetics_n_diff + amenities_n_diff +
                    crossing_n_diff + mixed_n_diff + walkscore_n_diff + hill, data=both_new, design=designphr)

summary(longenv)

# examine multicollinearity of environmental predictors
vif(longenv) # activetrans_n_diff has a vif of 11.86--need to get rid of
# next highest vif is 5.14 (crossing_n_diff)

#create subset of data that includes only environmental predictors to prepare for lapply
both_newpredenv <- select(both_new, newsa_is_diff, newsa_surround_diff, soc_cohesion_diff, nbhd_safety_diff,
                          total_crime_diff, activetrans_n_diff, aesthetics_n_diff, amenities_n_diff,
                          crossing_n_diff, mixed_n_diff, walkscore_n_diff)

#create function within lapply to run correlations between activetrans_n_diff and other predictors to see
# what it is highly correlated with

lapply(both_newpredenv, function(x) {
  y <- cor(both_new$activetrans_n_diff, x)
  return(y)
})

# activetrans_n_diff is correlated with mixed_n_diff at .84
# try rerunning environmental predictor model without activetrans_n_diff

longenvtrim <- svyglm(k6_diff ~ newsa_is_diff + newsa_surround_diff + soc_cohesion_diff + nbhd_safety_diff
              + total_crime_diff + aesthetics_n_diff + amenities_n_diff +
                crossing_n_diff + mixed_n_diff + walkscore_n_diff + hill, data=both_new, design=designphr)

summary(longenvtrim)

vif(longenvtrim) # max vif is 3.3, so no need to remove additional predictors

# examine distribution of residuals
hist(longenvtrim$residuals)

#longitudinal model: individual and environmental characteristics
longindenvwt <- svyglm(k6_diff ~ Age + male + 
                       mar_gain + mar_loss + liv_gain + liv_loss +
                       i_highest_edu_1 + i_adj_inc2_diff + car_access_gain + car_access_loss + 
                       bmi_diff + 
                       newsa_is_diff + newsa_surround_diff + soc_cohesion_diff + nbhd_safety_diff + 
                       total_crime_diff + 
                       aesthetics_n_diff + amenities_n_diff +
                       crossing_n_diff + mixed_n_diff + walkscore_n_diff + hill, data=both_new, design=designphr)
summary(longindenvwt)

# examine multicollinearity of predictors
vif(longindenvwt) # max VIF is 3.93

# examine distribution of residuals
hist(longindenvwt$residuals) 
describe(longindenvwt$residuals)

















