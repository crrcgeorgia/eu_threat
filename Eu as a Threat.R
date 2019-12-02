#EU as a Threat
library(haven)
library(descr)
library(Matching)
library(rgenoud)
library(psych)
library(sandwich)
library(lmtest)
library(multiwayvcov)
library(survey)
library(margins)
library(ggeffects)
library(sjmisc)
library(ggplot2)
library(effects)
library(MASS)
library(ggeffects)
library(haven)

setwd("d:/dustin/Desktop/Desktop/ceu stats folder")
EU19 <- read_dta("EU_2019_09.04.19_public.dta")
names(EU19)
table(as_factor(EU19$EUTHREAT))
EU19$EUTHREAT_r<-EU19$EUTHREAT
EU19$EUTHREAT_r[EU19$EUTHREAT_r==-2]<-NA
EU19$EUTHREAT_r[EU19$EUTHREAT_r<=2]<-0
EU19$EUTHREAT_r[EU19$EUTHREAT_r>=3]<-1

table(EU19$EUTHREAT_r)

EU19$substratum_r<-EU19$substratum
table(as_factor(EU19$substratum_r))
table(EU19$substratum_r)
EU19$substratum_r[EU19$substratum_r==10]<-1
EU19$substratum_r[EU19$substratum_r==411]<-3
EU19$substratum_r[EU19$substratum_r==412]<-2
EU19$substratum_r[EU19$substratum_r==421]<-3
EU19$substratum_r[EU19$substratum_r==422]<-2
EU19$substratum_r[EU19$substratum_r>=31]<-3
EU19$substratum_r[EU19$substratum_r>=21]<-2
table(EU19$substratum_r)
EU19$substratum_r<-as_factor(EU19$substratum_r)

EU19$agegroup<-EU19$age
EU19$agegroup[EU19$agegroup<=35]<-1
EU19$agegroup[EU19$agegroup>=56]<-3
EU19$agegroup[EU19$agegroup>=36]<-2
table(EU19$agegroup)
EU19$agegroup<-as_factor(EU19$agegroup)
names(EU19)

table(EU19$sex)
str(EU19$sex)
EU19$sex<-as_factor(EU19$sex)
table(EU19$ETHNIC)
EU19$ETHNIC_r<-EU19$ETHNIC
EU19$ETHNIC_r[EU19$ETHNIC_r<=-1]<-NA
EU19$ETHNIC_r[EU19$ETHNIC_r==3]<-0
EU19$ETHNIC_r[EU19$ETHNIC_r>=1]<-1
table(EU19$ETHNIC_r)
EU19$ETHNIC_r<-as_factor(EU19$ETHNIC_r)

table(EU19$EDUDGR)
table(as_factor(EU19$EDUDGR))
EU19$EDUDGR_r<-EU19$EDUDGR
EU19$EDUDGR_r[EU19$EDUDGR_r<=-1]<-NA
EU19$EDUDGR_r[EU19$EDUDGR_r<=3]<-1
EU19$EDUDGR_r[EU19$EDUDGR_r==4]<-2
EU19$EDUDGR_r[EU19$EDUDGR_r>=5]<-3
table(EU19$EDUDGR_r)
EU19$EDUDGR_r<-as_factor(EU19$EDUDGR_r)
EU19svy <- svydesign(id=~psu, strata=~substratum, 
                      weights=~indwt,  
                      data=EU19)

table(EU19svy$variables$ETHNIC)

model1<-svyglm(EUTHREAT_r~ agegroup + substratum_r +sex + ETHNIC_r +EDUDGR_r, design = EU19svy, family = "binomial" )
summary(model1)

ggemmeans(model1,  "agegroup", data = EU19svy$variables)
ggemmeans(model1,  "substratum_r", data = EU19svy$variables)
ggemmeans(model1,  "sex", data = EU19svy$variables)
ggemmeans(model1,  "ETHNIC_r", data = EU19svy$variables)
ggemmeans(model1,  "EDUDGR_r", data = EU19svy$variables)


table(as_factor(EU19svy$variables$EUMEMVOT))

EU19svy$variables$EUMEMVOT_r<-EU19svy$variables$EUMEMVOT
EU19svy$variables$EUMEMVOT_r[EU19svy$variables$EUMEMVOT_r!=1]<-0
EU19svy$variables$EUMEMVOT_r<-as_factor(EU19svy$variables$EUMEMVOT_r)

model2<-svyglm(EUMEMVOT_r~ EUTHREAT_r+agegroup + substratum_r +sex + ETHNIC_r +EDUDGR_r, design = EU19svy, family = "binomial" )
summary(model2)
ggemmeans(model2,  "EUTHREAT_r", data = EU19svy$variables)

crosstab(EU19svy$variables$EUTHREAT,EU19svy$variables$EUMEMVOT, w= EU19svy$variables$indwt, prop.r = TRUE)
crosstab(EU19svy$variables$EUTHREAT_r,EU19svy$variables$EUMEMVOT, w= EU19svy$variables$indwt, prop.r = TRUE)
crosstab(EU19svy$variables$EUMEMVOT,EU19svy$variables$EUTHREAT_r, w= EU19svy$variables$indwt, prop.c = TRUE)

hist(EU19$EUTHREAT)
freq(as_factor(EU19$EUTHREAT), EU19$indwt)
EU19$EUTHREAT_a<-EU19$EUTHREAT
EU19$EUTHREAT_a[EU19$EUTHREAT_a<=-1]<-NA
hist(EU19$EUTHREAT_a)