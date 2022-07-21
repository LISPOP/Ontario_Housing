# Load Libraries
library(haven)
library(tidyverse)
library(modelsummary)
library(here)

#Import
on22<-read_dta(file=here("Data/Housing_02_06_100_Percent_Complete.dta"))

#on22<-read_sav(file=here("Data", "Housing_06_06.sav"))


#filter out non-consents
on22 %>% 
  filter(Consent2<2)->on22

#Clean Underscores before names
names(on22)<-str_remove_all(names(on22), "^_")
#Rename Experimental Group variables
on22 %>%
  rename("Social"=`v7`, "Private"=`v8`, "Public"=`v9`, "Control"=starts_with('SCREEN10'))->on22
# Filter out DO variables
on22 %>% 
  select(-contains("_DO_"))->on22
names(on22)
#Filter out v1 variables
on22 %>% 
  select(-matches("^v[0-9]"))->on22

#Look for variables
library(labelled)
names(on22)


# Spit out Most IMportant problem and hjousing

# look_for(on22, "most important")
# on22 %>% 
#   select(pid, Q3) %>% 
#   write.csv(., file=here("data", "most_important_problem.csv"))
nrow(on22)
