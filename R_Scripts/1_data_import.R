# Load Libraries
library(haven)
library(tidyverse)
library(modelsummary)
library(here)
library(labelled)
#Import
on22<-read_dta(file="Data/Housing_02_06_100_Percent_Complete.dta")
names(on22)
#Check postal codes

look_for(on22, "postal")
#Count good postal codes
str_to_upper(on22$Q47) 
str_remove_all(on22$Q47, " ")
#This modifies postal codes to be properly formatted
on22%>% 
  mutate(postal_code=str_to_upper(str_remove_all(Q47, " ")))->on22
#This provides a count of how many respondents have provided 6-digit postal codes
on22 %>% 
  select(postal_code) %>% 
map_df(., nchar)  %>% 
 count(postal_code)

#Merge with the geocoded file Provided by Tim Gravelle
on22_geocoded<-read_sav(file="Data/opes22_2022-09-26-geocoded.sav")
names(on22_geocoded)
#Keep only the variables that Tim provided in the on22_geocoded_survey
on22_geocoded %>% 
  select(ResponseId, FSA:pop_density)->on22_geocoded
#on22<-read_sav(file=here("Data", "Housing_06_06.sav"))
names(on22_geocoded)

#Provide a count of year of birth
on22 %>% 
  select(Q37_DO_NOT_USE, yob) %>% 
  as_factor() %>% 
  group_by(Q37_DO_NOT_USE, yob) %>% 
  count()

#make a geo_good variable for respondents whose postal code matches a Census Subdivision
on22 %>% 
  left_join(., on22_geocoded) %>% 
  mutate(geo_good=case_when(
    is.na(FED2013) == FALSE & is.na(CSD) == FALSE ~ 1,
    TRUE~0
  ))->on22
table(on22$geo_good)

#Keep only those good cases
on22 %>% 
  filter(geo_good==1)->on22
#filter out non-consents
on22 %>% 
  filter(Consent2<2)->on22

#Grab Ontario Vote Results
vote22<-data.frame(Party=c("PC", "NDP", "Liberal", "Green"), 
                   Share=c(40.82, 23.74, 23.85, 5.96))
#Read in Ontario costs
# library(cancensus)
# To make this work it is necessary to get an API key 
# at Census Mapper
# census_data <- get_census(dataset='CA21',
#                           regions=list(PR="35"),
#                           vectors=c("v_CA21_4290","v_CA21_4289","v_CA21_4309","v_CA21_4317"),
#                           labels="detailed", geo_format=NA, level='DA')
#write_csv(census_data, file=here("Data/ontario_shelter_costs.csv"))
#Read in Shelter costs
#Import the data file on shelter costs in the Canadian census
on_shelter_costs<-read.csv(file=here("Data/ontario_shelter_costs.csv"))
names(on22)
#Merge with the on22 data set by the CSD name
on22 %>% 
  left_join(., on_shelter_costs, by=c("CSD"="GeoUID"))->on22
names(on22)


#Rename the relevant variables
#These are community level variables about the percentage of houses in each municipality in housing poverty
#And median monthly mortgage and rent
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
  rename(area_sq_km=`Area..sq.km.`, region_name=`Region.Name`)->on22

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

val_labels(on22$Q80_1)
val_labels(on22$Q80_2)
val_labels(on22$Q80_3)
val_labels(on22$Q80_4)
val_labels(on22$Q80_5)
val_labels(on22$Q80_6)
table(as_factor(on22$Q80_1))
table(as_factor(on22$Q80_1), on22$Q80_1)
#It appears to be the same for both. 
#Summarize both sets

on22 %>% 
  select(starts_with("Q33")) %>% 
  summary() #Note that the max is 12

on22 %>% 
  select(starts_with("Q80")) %>% 
  summary() 


