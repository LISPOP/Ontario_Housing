# Load Libraries
library(haven)
library(tidyverse)
library(modelsummary)

#Import
on22<-read_dta(file="Data/lispop_housing_ontario_survey_may_2022.dta")

#filter out non-consents
on22 %>% 
  filter(Consent2<2)->on22
#Look for variables
library(labelled)

var_label(on22) %>% 
  View()

# 
