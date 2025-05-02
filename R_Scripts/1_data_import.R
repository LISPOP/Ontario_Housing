# Load Libraries
library(haven)
library(tidyverse)
library(modelsummary)
library(here)
library(labelled)
library(cancensus)
#Set cancencus cache and key for a few scripts
#set_cancensus_api_key(key="CensusMapper_e0bb5e9bb16c197f306a580284d35b5b")
#This sets the cancensus cache here in the project directory
set_cancensus_cache_path(cache_path=here("Data/cancensus_cache_statscan_data"))

#Import
on22<-read_dta(file="Data/Housing_02_06_100_Percent_Complete.dta")
names(on22)
#### Postal Code Evaluation
#This modifies postal codes to be properly formatted
on22%>% 
  mutate(postal_code=str_to_upper(str_remove_all(Q47, " ")))->on22
#This provides a count of how many respondents have provided 6-digit postal codes
on22 %>% 
  select(postal_code) %>% 
map_df(., nchar)  %>% 
 count(postal_code)
on22 %>% 
  filter(nchar(postal_code)==7) %>% 
  select(ResponseId, postal_code)
#this code trims a hyphen, trims one postal code at 6 characters,
# filters to try to identify them and shows what is left
# It should show 0 rows

on22 %>% 
  mutate(postal_code=str_remove_all(postal_code, "-")) %>% 
  mutate(postal_code=str_sub(postal_code, 1,6)) %>% 
  filter(nchar(postal_code)==7) %>% 
  select(ResponseId, postal_code)
#Save
on22 %>% 
  mutate(postal_code=str_remove_all(postal_code, "-")) %>% 
  mutate(postal_code=str_sub(postal_code, 1,6))->on22



#This script gets the dissemination areas 
source("R_Scripts/2_pccf_merge_weight.R")

#on22$PRCDDA<-as.numeric(on22$PRCDDA)

#This script gets the CSD 
#Merge with the geocoded file Provided by Tim Gravelle
# on22_geocoded<-read_sav(file="Data/opes22_2022-09-26-geocoded.sav")
# names(on22_geocoded)
# #Keep only the variables that Tim provided in the on22_geocoded_survey
# names(on22_geocoded)
# on22_geocoded %>%
#   select(ResponseId, FSA:CSDTYPE)->on22_geocoded
# #on22<-read_sav(file=here("Data", "Housing_06_06.sav"))
# names(on22_geocoded)
# 
# #Provide a count of year of birth
# on22 %>% 
#   select(Q37_DO_NOT_USE, yob) %>% 
#   as_factor() %>% 
#   group_by(Q37_DO_NOT_USE, yob) %>% 
#   count()
# 
# #make a geo_good variable for respondents whose postal code matches a Census Subdivision
# on22 %>%
#   left_join(., on22_geocoded) %>%
#   mutate(geo_good=case_when(
#     is.na(FED2013) == FALSE & is.na(CSD) == FALSE ~ 1,
#     TRUE~0
#   ))->on22



# table(on22$geo_good)
# 





#This script gets the census data for the dissemination areas
source(here("R_scripts/2_statscan_census_data.R"))

#This merges the Dissemination Areas in on22 with the data gathered in the above script


on_statscan_da$DA2021<-as.numeric(on_statscan_da$DA2021)
on22 %>% 
  left_join(on_statscan_da, by="DA2021") ->on22

#Note in on22 the variable is called PRCDDA and in the on_statscan object it is GeoUID
# on22$PRCDDA
# as.character(on22$CSDuid)
# on_statscan_csd
# on22 %>% 
#   left_join(., on_statscan_da, by=c("PRCDDA"="GeoUID_da"))->on22
# #Get the population variable from on_statscan_csd
# on_statscan_csd %>% 
#   mutate(GeoUID_csd=as.numeric(GeoUID_csd)) %>% 
#   select(GeoUID_csd, Population_csd)->out
# names(on22)
# names(on_statscan_csd)
# on22 %>% 
#   left_join(., out, by=c("CSDuid"="GeoUID_csd"))->on22


# Uncomment this line if we are using CSD level data. If not, disregard
# on22 %>% 
#   left_join(., on_statscan_csd, by=c("CSDuid"="GeoUID_csd"))->on22

#Clean Underscores before names
names(on22)<-str_remove_all(names(on22), "^_")
names(on22)
#Rename Experimental Group variables
names(on22)
on22 %>%
  rename("National"=`v7`, "Individual"=`v8`, "Community"=`v9`, "Control"=starts_with('SCREEN10'))->on22
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
  select(starts_with("Q32"))

#Check value labels for solutions
val_labels(on22$Q33a_1)
val_labels(on22$Q33a_2)
val_labels(on22$Q33a_3)
val_labels(on22$Q33a_4)
val_labels(on22$Q33a_5)
val_labels(on22$Q33a_6)
table(as_factor(on22$Q33a_1))
table(as_factor(on22$Q33a_1), on22$Q33a_1)
#Conclusion in the Q33 set, the #1 corresponded to 0 on the screen; 
# the # 11, corresponded to the 10 on the screen and 12 corresponded to Dont' know
#


