# Load Libraries
library(haven)
library(tidyverse)
library(modelsummary)
library(here)

#Import
on22<-read_sav(file=here("Data", "Housing_06_06.sav"))

#filter out non-consents
on22 %>% 
  filter(Consent2<2)->on22

#Clean Underscores before names
names(on22)<-str_remove_all(names(on22), "^_")
#Look for variables
library(labelled)
names(on22)


# Spit out Most IMportant problem and hjousing

look_for(on22, "most important")
on22 %>% 
  select(pid, Q3) %>% 
  write.csv(., file=here("data", "most_important_problem.csv"))

