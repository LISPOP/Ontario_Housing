#This line is a prerequisite
#When the code is ship-shape, I will modify this to integrate it to the rest of the code
# properly
source("R_Scripts/1_data_import.R")
#This approach to getting the dissemination areas uses the cancensus package
# I have set my API key from census mapper in the script data_import
#If necessary, a user can enter their own here by uncommenting this line
library(cancensus)
#set_cancensus_api_key(key='insert key here', overwrite=T, install=F)
#More important however is to take note of the cancensus cache pathe
# Which I have set to be in the folder "data/cancensus_cache_statscan_data"
#Because I have run this code before, all the data has been cached there and 
# Can be quickly importated here with the calls below

# I ha
library(sf)
#Get the geometry files for dissemination areas for Ontario
#This usues a function in the cancensus package to directly download the geemotries from 
# Statistics Canada
da_geometry<-get_statcan_geographies(census_year="2021",level="DA", type="digital", cache_path=here("Data/cancensus_cache_statscan_data/"))
#da_geometry<-get_census("CA21", regions=list(PR="35"), level="DA", geo_format="sf")
names(da_geometry)
# This line adds the correct DA boundary file (geometry) from da_geometry to the
# respondents in on22 based on the DA that was assigned by their postal_code
on22 %>% 
  left_join(., da_geometry, by=c("PRCDDA"="DAUID"))->on22
#This should give us the touch matrix
#It takes all respondents' DAs which are now stored in on22$geometry
# and returns TRUE if DA x touches any of da_geometry
# I will pass it over to Rafael to finish this off here

on22_touch_matrix<-st_touches(on22$geometry, da_geometry$geometry)
on22_touch_matrix # WHAT DO THESE NUMBERS MEAN?????????
da_geometry$geometry[22404]
#OK, I think 22404 is the 22404th DA in the list of Ontario DAs that are stored in 
#da_geometry$geometry
on22_touch_matrix
#Now we don't need the actualy geometric boundaries, 
# What we need is the identifying numbers of the boundaries. We can feed these
# then to cancensus and get the statistics for the DAs that touch each Rs DA.
da
on22_touch_matrix[1] 
# Rafael, maybe your code below can get the identifying #s!!

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
