# Load Libraries
library(haven)
library(tidyverse)
library(modelsummary)

#Import
on22<-read_dta(file="Data/Housing_02_06_100_Percent_Complete.dta")
names(on22)
lookfor(on22, "cond")
on22$Q35_1
on22$Q35_1
names(on22)
on22$Q35a
on22$SCREEN10_Experiment1_DO_Control
#filter out non-consents
on22 %>% 
  filter(Consent2<2)->on22

#Clean Underscores before names
names(on22)<-str_remove_all(names(on22), "^_")
#Look for variables
library(labelled)
names(on22)


# 
