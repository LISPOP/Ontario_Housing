#This compares our survey with some key housing variables.
source("R_Scripts/2_recodes.R")
#Read in SFS survey
sfs<-read_sav("Data/SFS_individual_2019.sav")
lookfor(sfs, "own")
lookfor(sfs, "weight")
library(survey)
library(srvyr)

sfs$PPVRES<-as_factor(sfs$PPVRES)
sfs %>% 
#  as_factor() %>% 
as_survey_design(ids=1, weights=PWEIGHT)->sfs_des

sfs_des %>% 
  mutate(own=Recode(as.numeric(PFTENUR), "1='Own' ;2='Own' ; 3='Other'; else=NA", levels=c("Own", "Other")))->sfs_des
sfs_des %>% 
  filter(PPVRES=="Ontario")->sfs_des

library(dplyr)
#Make survey design for on22
on22_des<-as_survey_design(on22, ids=1)

#Get populationtotals
svytotal(~own, sfs_des) 
own_pop<-data.frame(Own=as.factor(c("Own","Other")), freq=c(2254301, 3747265))

svytotal(~Own, on22_des)
own_pop
on22_des %>% 
  filter(!is.na(Own)) %>% 
postStratify(., strata=~Own, own_pop)->on22_des
on22_des %>% 
  group_by(Own) %>% 
  summarize(survey_mean())

on22_des %>%
  group_by(Housing_Status) %>%
  summarize(n = unweighted(n()), weighted_n=survey_total()) %>% 
  mutate(pct=n/sum(n), weighted_pct=weighted_n/sum(weighted_n))


