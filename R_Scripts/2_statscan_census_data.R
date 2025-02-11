# library(cancensus)
# #To make this work it is necessary to get an API key
# #at Census Mapper
# # Read documentation for census_mapper to set yours permanently in R
# census_data <- get_census(dataset='CA21',regions=list(PR="35"),
#  vectors=c("v_CA21_434","v_CA21_435","v_CA21_440","v_CA21_436","v_CA21_437","v_CA21_438","v_CA21_439","v_CA21_4290", "v_CA21_4309", "v_CA21_4317"),
# labels="detailed", geo_format=NA, level='DA')
# # #check
# head(census_data)
# names(census_data)
# census_data %>%
#   rename("total_occupied_private_dwellings"=12,
#   "single_detached_houses"=13,
#          "apartment_in_building_plus_5"=14,
#          "semi_detached_house"=15,
#          "row_house"=16,
#          "apartment_in_duplex"=17,
#          "apartment_in_building_less_5"=18,
#          "households_more_than_30"=19,
#          "median_shelter_costs_owned"=20,
#          "median_shelter_costs_rented"=21
#          )->census_data
# census_data %>%
#   view()
# #Normalize data
# census_data %>%
#   mutate(single_detached_houses_pct=single_detached_houses/total_occupied_private_dwellings,
#          semi_detached_house_pct=semi_detached_house/total_occupied_private_dwellings,
#          row_house_pct=row_house/total_occupied_private_dwellings,
#          apartment_in_duplex_pct=apartment_in_duplex/total_occupied_private_dwellings,
#          apartment_in_building_less_5_pct=apartment_in_building_less_5/total_occupied_private_dwellings,
#          apartment_in_building_plus_5_pct=apartment_in_building_plus_5/total_occupied_private_dwellings)->census_data
# #Create population density
# census_data %>%
#   mutate(pop_density=Population/`Area (sq km)`)->census_data
# 
#write_csv(census_data, file=here("Data/ontario_statscan_data.csv"))
#census_data %>% names()
#Read in Shelter costs
#Import the data file on shelter costs in the Canadian census
on_statscan<-read.csv(file=here("Data/ontario_statscan_data.csv"))
on_statscan %>% names()
