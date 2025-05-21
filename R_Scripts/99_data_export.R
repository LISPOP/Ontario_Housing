# Load Libraries
library(haven)
library(tidyverse)
library(modelsummary)
library(here)
library(labelled)
#Import Original File

on22<-read_dta(file="Data/Housing_02_06_100_Percent_Complete.dta")


names(on22)
on22$`_v7`
on22$`_v6`
on22$`_v5`
table(on22$`_v9`)
table(on22$`SCREEN10_Experiment1_DO_Control`)
on22 %>%
  rename("Social"=`_v7`, "Private"=`_v8`, "Public"=`_v9`, "Control"=starts_with('SCREEN10'))->on22
names(on22)
# Confirm that the poorly labelled year of birth variable is correctly converted to youb
#note this also deletes the corrupted age variable
# but it can be confirmed that the yob variable is correct
# in previously executed code 
on22 %>% 
  select(Q37_DO_NOT_USE, yob) %>% 
  as_factor() %>% 
  group_by(Q37_DO_NOT_USE, yob) %>% 
  count()
# Filter out DO variables
on22 %>% 

  select(-contains("_DO_"))->on22
names(on22)
#Filter out v1 variables
on22 %>% 
  select(-matches("^_v[0-9]"))->on22
#Filter out identifiers
on22 %>% 
  select(-c(3,9:14))->on22


names(on22)
#Not sure what these variabes are deleting them.
on22 %>% 
  select(-psid:-pid)->on22

# delete postal code
on22$Q47
on22 %>% 
  select(-Q47)->on22
names(on22)
names(on22)
max(on22$EndDate)
min(on22$StartDate)
names(on22)

nrow(on22)
table(on22$Consent2)
#Merge with the geocoded file Provided by Tim Gravelle
on22_geocoded<-read_sav(file="Data/opes22_2022-09-26-geocoded.sav")
names(on22_geocoded)
#Keep only the variables that Tim provided in the on22_geocoded_survey
on22_geocoded %>% 
  select(ResponseId, FSA:pop_density)->on22_geocoded


names(on22)
#on22<-read_sav(file=here("Data", "Housing_06_06.sav"))
names(on22_geocoded)


#make a geo_good variable for respondents whose postal code matches a Census Subdivision
on22 %>% 
  left_join(., on22_geocoded) %>% 
  mutate(geo_good=case_when(
    is.na(FED2013) == FALSE & is.na(CSD) == FALSE ~ 1,
    TRUE~0
  ))->on22
names(on22)
# Remove X and Y
on22 %>% 
  select(-X:-Y)->on22
# Remove age and age groups

on22 %>% 
  select(-age:-agegrps)->on22
#Remove responseID
on22 %>% 
  select(-ResponseId)->on22
#Provide variable labels
var_label(on22$Social)<-("R prompted with treatment 3 Social")
var_label(on22$Private)<-("R prompted with treatment 2  Private")
var_label(on22$Public)<-("R prompted with treatment 1 Public")
var_label(on22$Control)<-("R control group")

var_label(on22$gender)<-c("R gender")

var_label(on22$geo_good)<-c("R has been assigned to a federal electoral district and census subdivision")
var_label(on22$FSA)<-c("Rs forward sortation area")
var_label(on22$PR)<-c("Rs Province")
var_label(on22$FED2013)<-c("Rs FED from the 2013 representation order as per Statistics Canada")
var_label(on22$CSD)<-c("Rs Census subdivision as per Statistics Canada")
var_label(on22$PCtype)<-c("Type of population center as per Statistics Canada")
var_label(on22$FEDENAME)<-c("Name of Rs Federal Electoral District as per Statistics Canada")
var_label(on22$CSDNAME)<-c("Name of Rs CSD as per Statistics Canada")
var_label(on22$CSDTYPE)<-c("Type of Rs CSD as per Statistics Canada")
var_label(on22$LANDAREA)<-c("Land area of Rs CSD") 
var_label(on22$pop_2021)<-c("2021 population of Rs CSD")
var_label(on22$pop_density)<-c("2021 population density of Rs CSD")

  
# Export
on22 %>% 
  write_sav(., path=here("data/opes_2022_geocoded.sav"))

on22 %>% 
  write_dta(., path=here("data/opes_2022_geocoded.dta"))
on22 %>% 
write_sas(., path=here("data/opes_2022_geocoded.sas"))
on22 %>% 
  write_csv(., path=here("data/opes_2022_geocoded.csv"))
on22 %>% 
  save(., file=here("data/opes_2022_gecoded.rdata"))
sessionInfo()
# #Keep only those good cases
# on22 %>% 
#   filter(geo_good==1)->on22
# #filter out non-consents
# on22 %>% 
#   filter(Consent2<2)->on22
# 
# #Grab Ontario Vote Results
# vote22<-data.frame(Party=c("PC", "NDP", "Liberal", "Green"), 
#                    Share=c(40.82, 23.74, 23.85, 5.96))
# #Read in Ontario Shelter costs
# #Import the data file on shelter costs in the Canadian census
# on_shelter_costs<-read.csv(file=here("Data/ontario_shelter_costs.csv"))
# names(on22)
# #Merge with the on22 data set by the CSD name
# on22 %>% 
#   left_join(., on_shelter_costs, by=c("CSD"="GeoUID"))->on22
# names(on22)
# 
# 
# #Rename the relevant variables
# #These are community level variables about the percentage of houses in each municipality in housing poverty
# #And median monthly mortgage and rent
# on22 %>% 
#   rename("hh_more_than_30"=268, 
#          "hh_less_than_30"=269,
#          "median_monthly_mortgage"=270, 
#          "median_monthly_rent"=271) ->on22
# 
# 
# names(on22)
# #Clean Underscores before names
# names(on22)<-str_remove_all(names(on22), "^_")
# names(on22)
# 
# # Filter out DO variables
# on22 %>% 
#   select(-contains("_DO_"))->on22
# names(on22)
# #Filter out v1 variables
# on22 %>% 
#   select(-matches("^v[0-9]"))->on22
# names(on22)
# #Look for variables
# library(labelled)
# names(on22)
# 
# 
# nrow(on22)
# names(on22)
# #clean names for SPSS export
# on22 %>% 
#   rename(area_sq_km=169, region_name=168)->on22
# 
# #Most Important Problem
# 
# library(readxl)
# #Read in the coded responses to mip done by LISPOP RA. 
# #column A is the raw response # Column B is RAs MIP code
# mip<-read_excel(path=here("Data/mip.xlsx"), range="A1:B967", col_types=c("text", "numeric"))
# #Convert the mip problem in on22 to all lower case
# #Store in on22$mip
# #The on22 mip responses are now identical to the mip responses in RAs excel file
# on22$mip<-str_to_lower(on22$Q3)
# #Now merge on22 with the mip object
# #Using the mip variable as the key
# nrow(mip)
# mip %>% 
#   distinct()->mip
# #Filter out unique responses
# on22 %>% 
#   left_join(.,mip, by=c("mip"="mip"))->on22
# names(on22)
# 
# #Now read in the RAs code LABELS
# mip_categories<-read_excel(path=here("Data/mip_categories_final.xlsx"))
# 
# mip_categories
# on22 %>% 
#   left_join(., mip_categories, by=c("mip_code"="Code Number"))->on22
# names(on22)
# on22$Topic<-as.factor(on22$Topic)
# 
# names(on22)
# 
# on22 %>% 
#   select(1)
on22$Q47
names(on22)
