
#### SCRIPT TO CREATE TOUCHING VARIABLE ###

# To run this script you must download the shape files from dropbox and add them to the data folder.
# This file takes a long time to run

# Load shape file for FSA for Ontario
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


