###############################################################################
############ THE CODE USED TO IMPORT BUILDING HEIGHT DATA FROM ################
################# https://open.toronto.ca/dataset/3d-massing/ #################
###############################################################################

source("R_Scripts/0_Functions.R")

### Import building data

buildings <- read_sf("Data/3D Massing (WGS84)/3DMassingShapefile_2023_WGS84.shp")

### Merge building data with census data

st_crs(da.2021.0)
st_crs(buildings)

DAs <- st_transform(da.2021.0, st_crs(buildings))

buildings_DA <- st_join(buildings, DAs, join = st_within)


#### Create average Height per DA variable 

DA_height <- buildings_DA %>% 
  group_by(DAUID) %>% 
  summarise(Average_Height = mean(MAX_HEIGHT, na.rm = TRUE)) %>% 
  mutate(DAUID = as.numeric(DAUID))
