library(tidyverse)
#install.packages("cancensus")
library(cancensus)
library(here)
#This gets the data for variables of interest for Ontario DAs
# It does not get the geometries
on_statscan_da <- get_census(dataset='CA21',regions=list(PR="35"),
 vectors=c("v_CA21_434","v_CA21_435","v_CA21_440","v_CA21_436","v_CA21_437","v_CA21_438","v_CA21_439","v_CA21_4290", "v_CA21_4309", "v_CA21_4317"),
labels="detailed", geo_format=NA, level='DA')

# #check
head(on_statscan_da)
names(on_statscan_da)
on_statscan_da %>%
  rename("total_occupied_private_dwellings"=12,
  "single_detached_houses"=13,
         "apartment_in_building_plus_5"=14,
         "semi_detached_house"=15,
         "row_house"=16,
         "apartment_in_duplex"=17,
         "apartment_in_building_less_5"=18,
         "households_more_than_30"=19,
         "median_shelter_costs_owned"=20,
         "median_shelter_costs_rented"=21
         )->on_statscan_da

#Normalize data
on_statscan_da %>%
  mutate(single_detached_houses_pct=single_detached_houses/total_occupied_private_dwellings,
         semi_detached_house_pct=semi_detached_house/total_occupied_private_dwellings,
         row_house_pct=row_house/total_occupied_private_dwellings,
         apartment_in_duplex_pct=apartment_in_duplex/total_occupied_private_dwellings,
         apartment_in_building_less_5_pct=apartment_in_building_less_5/total_occupied_private_dwellings,
         apartment_in_building_plus_5_pct=apartment_in_building_plus_5/total_occupied_private_dwellings)->on_statscan_da
on_statscan_da$Population
#Create population density
on_statscan_da %>%
  mutate(pop_density=Population/`Area (sq km)`)->on_statscan_da
names(on_statscan_da)
on_statscan_da %>% 
  rename_with(., ~paste0(., "_da"))->on_statscan_da
#This drops superfluous variables that we don't need for merging with ON22
# Tim has already got these
names(on_statscan_da)
#Get CSD Population
get_census("CA21", regions=list(PR="35"),level="CSD", vectors="v_CA21_1") %>% 
  select(c("GeoUID", "Population"))->csd_population
names(csd_population)

on_statscan_da %>% 
  select(DA2021=1, CSD_UID=CSD_UID_da,total_occupied_private_dwellings_da:pop_density_da)->on_statscan_da

on_statscan_da %>% 
  left_join(., csd_population, by=c("CSD_UID"="GeoUID"))->on_statscan_da
#Convert DA to Numeric
on_statscan_da$DA2021<-as.numeric(on_statscan_da$DA2021)
names(on_statscan_da)
#This writes the data out for the record.
write_csv(on_statscan_da, file=here("Data/ontario_statscan_data_da.csv"))
#Get CSD level data
#Get CSD data
# on_statscan_csd <- get_census(dataset='CA21',regions=list(PR="35"),
#                           vectors=c("v_CA21_434","v_CA21_435","v_CA21_440","v_CA21_436","v_CA21_437","v_CA21_438","v_CA21_439","v_CA21_4290", "v_CA21_4309", "v_CA21_4317"),
#                           labels="detailed", geo_format=NA, level='CSD')
# #Get CSD data
# names(on_statscan_csd)
# on_statscan_csd %>%
#   rename("total_occupied_private_dwellings"=11,
#          "single_detached_houses"=12,
#          "apartment_in_building_plus_5"=13,
#          "semi_detached_house"=14,
#          "row_house"=15,
#          "apartment_in_duplex"=16,
#          "apartment_in_building_less_5"=17,
#          "households_more_than_30"=18,
#          "median_shelter_costs_owned"=19,
#          "median_shelter_costs_rented"=20
#   )->on_statscan_csd
# 
# on_statscan_csd %>%
#   mutate(single_detached_houses_pct=single_detached_houses/total_occupied_private_dwellings,
#          semi_detached_house_pct=semi_detached_house/total_occupied_private_dwellings,
#          row_house_pct=row_house/total_occupied_private_dwellings,
#          apartment_in_duplex_pct=apartment_in_duplex/total_occupied_private_dwellings,
#          apartment_in_building_less_5_pct=apartment_in_building_less_5/total_occupied_private_dwellings,
#          apartment_in_building_plus_5_pct=apartment_in_building_plus_5/total_occupied_private_dwellings)->on_statscan_csd
# #Create population density
# on_statscan_csd %>%
#   mutate(pop_density=Population/`Area (sq km)`)->on_statscan_csd
# 
# on_statscan_csd %>% 
#   rename_with(., ~paste0(., "_csd"))->on_statscan_csd
# write_csv(on_statscan_csd, file=here("Data/ontario_statscan_data_csd.csv"))
# 
#Read in Shelter costs
#Import the data file on shelter costs in the Canadian census
# on_statscan_da<-read.csv(file=here("Data/ontario_statscan_data_da.csv"))
# on_statscan_csd<-read.csv(file=here("Data/ontario_statscan_data_csd.csv"))

#### Import Postal Code Data ####

postal_codes <- read_json("Data/canadapostalcodeslist.txt",  simplifyVector = TRUE)

postal_codes <- bind_rows(postal_codes, .id="postal_code")


postal_codes_df <- data.frame(Postal_codes = names(postal_codes),
                              city = unlist(postal_codes))


