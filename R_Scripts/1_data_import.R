# Load Libraries
library(haven)
library(tidyverse)
library(modelsummary)
library(here)

#Import
on22<-read_dta(file=here("Data/Housing_02_06_100_Percent_Complete.dta"))
#Merge with the geocoded file

on22_geocoded<-read_sav(file=here("Data/opes22_2022-09-26-geocoded.sav"))
names(on22_geocoded)
on22_geocoded %>% 
  select(ResponseId, FSA:pop_density)->on22_geocoded
#on22<-read_sav(file=here("Data", "Housing_06_06.sav"))
names(on22_geocoded)

on22 %>% 
  left_join(., on22_geocoded) %>% 
  mutate(geo_good=case_when(
    is.na(FED2013) == FALSE & is.na(CSD) == FALSE ~ 1,
    TRUE~0
  ))->on22
table(on22$geo_good)
names(on22)
on22 %>% 
  filter(geo_good==1)->on22
#filter out non-consents
on22 %>% 
  filter(Consent2<2)->on22

#Grab Ontario Vote Results
vote22<-data.frame(Party=c("PC", "NDP", "Liberal", "Green"), Share=c(40.82, 23.74, 23.85, 5.96))
#Read in Ontario costs
# library(cancensus)
# census_data <- get_census(dataset='CA21', 
#                           regions=list(PR="35"), 
#                           vectors=c("v_CA21_4290","v_CA21_4289","v_CA21_4309","v_CA21_4317"), 
#                           labels="detailed", geo_format=NA, level='CSD')
# write_csv(census_data, file=here("Data/ontario_shelter_costs.csv"))
#Read in Shelter costs
on_shelter_costs<-read.csv(file=here("Data/ontario_shelter_costs.csv"))
names(on22)
on22 %>% 
  left_join(., on_shelter_costs, by=c("CSD"="GeoUID"))->on22
names(on22)
on22 %>% 
  rename("hh_more_than_30"=268, 
         "hh_less_than_30"=269,
         "median_monthly_mortgage"=270, 
         "median_monthly_rent"=271) ->on22


names(on22)
#Clean Underscores before names

names(on22)<-str_remove_all(names(on22), "^_")
names(on22)
#Rename Experimental Group variables
on22 %>%
  rename("Social"=`v7`, "Private"=`v8`, "Public"=`v9`, "Control"=starts_with('SCREEN10'))->on22
names(on22)
# Filter out DO variables
on22 %>% 
  select(-contains("_DO_"))->on22
names(on22)
#Filter out v1 variables
on22 %>% 
  select(-matches("^v[0-9]"))->on22
names(on22)
#Look for variables
library(labelled)
names(on22)


# Spit out Most IMportant problem and hjousing

# look_for(on22, "most important")
# on22 %>% 
#   select(pid, Q3) %>% 
#   write.csv(., file=here("data", "most_important_problem.csv"))
nrow(on22)
names(on22)
#clean names for SPSS export
on22 %>% 
  rename(area_sq_km=169, region_name=168)->on22
