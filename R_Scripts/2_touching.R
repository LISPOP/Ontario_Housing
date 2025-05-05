
#### SCRIPT TO CREATE TOUCHING VARIABLE ###

# To run this script you must download the shape files from dropbox and add them to the data folder.
# This file takes a long time to run

# Load shape file for FSA for Ontario



#shape_file <- st_read("Data/Ontario_geography/lfsa000b21a_e.shp")
#options(cancensus.api_key = "CensusMapper_164474c9705b09054f031a9bff098515")
on_statscan_da_sf <- get_census(dataset='CA21',regions=list(PR="35"),
                              vectors=c("v_CA21_434","v_CA21_435","v_CA21_440","v_CA21_436","v_CA21_437","v_CA21_438","v_CA21_439","v_CA21_4290", "v_CA21_4309", "v_CA21_4317"),
                              labels="detailed", geo_format="sf", level='DA')



# Generate touch matrix for geography data set 
on22_touch_matrix <- st_touches(on_statscan_da_sf, sparse = FALSE) 

sum(on22_touch_matrix) / 2 


shape_file <- st_read("Data/Ontario_geography/lfsa000b21a_e.shp")


# Join shape file with on22 dataset by FSA
on22_geography <- right_join(shape_file, on22, by = c("CFSAUID" = "FSA"))

# Generate touch matrix for geography data set 
on22_touch_matrix <- st_touches(on22_geography, sparse = FALSE) 

# Create a new column to store touching FSA IDs for each FSA
on22_geography$touching_FSAs <- apply(on22_touch_matrix, 1, function(row) {

  # Extract indices of FSAs that touch the current FSA (row)
  touching_indices <- which(row)  # Indices of touching FSAs
  touching_FSAs <- on22_geography$CFSAUID[touching_indices]  # Get FSA IDs of touching FSAs
  return(touching_FSAs)
})


# Join shape file with on22 dataset by FSA
on22_geography <- right_join(shape_file, on22, by = c("CFSAUID" = "FSA"))


