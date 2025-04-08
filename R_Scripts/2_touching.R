#The better approach is to use cancensus
#Ideally a user would have an API key 

library(sf)
#Join the respondents' Dissemination areas to the Ontario Dissemination areas to add geometry for each respondents' DA
#Get the geometry files for dissemination areas for Ontario
da_geometry<-get_census("CA21", regions=list(PR="35"), level="DA", geo_format="sf")
names(da_geometry)
on22$PRCDDA
on22 %>% 
  left_join(., da_geometry, by=c("PRCDDA"="GeoUID"))->on22
#This should give us the touch matrix
#It takes all respondents' DAs which are now stored in on22$geometry
# and returns TRUE if DA x touches any of da_geometry
# I will pass it over to Rafael to finish this off here

on22_touch_matrix<-st_touches(on22$geometry, da_geometry$geometry)
#### SCRIPT TO CREATE TOUCHING VARIABLE ####

# To run this script you must download the shape files from dropbox and add them to the data folder.
# This file takes a long time to run

# Load shape file for FSA for Ontario
#shape_file <- st_read("Data/Ontario_geography/lfsa000b21a_e.shp")

# Join shape file with on22 dataset by FSA
#on22_geography <- right_join(shape_file, on22, by = c("CFSAUID" = "FSA"))

# Generate touch matrix for geography data set 
# on22_touch_matrix <- st_touches(on22_geography, sparse = FALSE) 
# 
# # Create a new column to store touching FSA IDs for each FSA
# on22_geography$touching_FSAs <- apply(on22_touch_matrix, 1, function(row) {
#   # Extract indices of FSAs that touch the current FSA (row)
#   touching_indices <- which(row)  # Indices of touching FSAs
#   touching_FSAs <- on22_geography$CFSAUID[touching_indices]  # Get FSA IDs of touching FSAs
#   return(touching_FSAs)
# })
# 
# 
