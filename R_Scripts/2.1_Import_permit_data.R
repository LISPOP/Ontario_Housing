# Note run R_Scripts/2_recodes.R
#source("R_Scripts/2_recodes.R")
## Load sf library
library(sf)
###### Code to Import Building Permit Data #####
count(on22, Comm_Name)
#### Toronto Permit Data as CSV

toronto_permits <- read.csv("Data/Building_permit_data/Cleared Building Permits since 2017.csv")

toronto_permits <- toronto_permits %>% 
  mutate(change = ifelse(PROPOSED_USE == CURRENT_USE, 0, 1))

toronto_permits_change <- toronto_permits %>% 
  filter(change == 1)

toronto_permits <- toronto_permits %>% 
  mutate(Address = paste(STREET_NUM, STREET_NAME, STREET_TYPE),
         Address = str_to_title(Address))
  
# Toronto address points, downloaded from Toronto Open Data (full address + point only)
# https://open.toronto.ca/dataset/address-points-municipal-toronto-one-address-repository/
toronto_address_url <- "https://ckan0.cf.opendata.inter.prod-toronto.ca/datastore/dump/0b3756af-9caf-4f0f-ac28-9c6617adede4?fields=ADDRESS_FULL,geometry&format=csv"
Toronto_address_points <- read_csv(toronto_address_url, col_types = "cc") %>%
  mutate(lon = as.numeric(str_extract(geometry, "(?<=\\[)-?[0-9.]+")),
         lat = as.numeric(str_extract(geometry, "-?[0-9.]+(?=\\])"))) %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  select(-geometry) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

Toronto_address_points <- Toronto_address_points %>%
  distinct(ADDRESS_FULL, .keep_all = TRUE)

toronto_permits <- toronto_permits %>% 
  left_join(Toronto_address_points %>%  select(ADDRESS_FULL, geometry),
            by = c("Address" = "ADDRESS_FULL"))


toronto_permits %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, colour = as.factor(change))) + 
  theme_void()
#### Barrie Permit Data as sf

barrie_permits <- read_sf("Data/Building_permit_data/Planning_Applications/Planning_Applications.shp")

barrie_permits %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry))

#### Burlington Permit Data as sf

burlington_permits <- read_sf("Data/Building_permit_data/Building_Permits/Building_Permits.shp")

#### Waterloo Permit Data as sf

Waterloo_permits <- read_sf("Data/Building_permit_data/City_of_Waterloo_Building_Permits_-5783174737437332337/BUILDING_PERMITS_wgs84.shp")

#### Kitchener Permit Data as sf

Kitchener_permits <- read_sf("Data/Building_permit_data/Building_Permits_755659247081512046/Building_Permit.shp")

#### Hamilton Permit Data as sf

Hamilton_permits <- read_sf("Data/Building_permit_data/Building_and_Demolition_Permits_2017_to_Present/Building_and_Demolition_Permits_2017_to_Present.shp")
glimpse(Hamilton_permits)
Hamilton_permits %>%
  st_drop_geometry(.) %>%
  write_csv(., file=here("Data/Building_permit_data/Hamilton/hamilton.csv"))
#### Ottawa Permits as csv

Ottawa_permits_2020 <- readxl::read_xls("Data/Building_permit_data/permits-2020.xls")
Ottawa_permits_2021 <- readxl::read_xls("Data/Building_permit_data/permits_2021_EN.xls")

#### Mississauga Permits as sf

Mississauga_permits <- read_sf("Data/Building_permit_data/Issued_Building_Permits_5749794004220210135/Building_Permits.shp")
#Strip geography 
st_drop_geometry(Mississauga_permits) %>% 
  write_csv(., file=here("Data/Building_permit_data/Mississauga/mississauga.csv"))
#### Niagara Falls Permits as sf 

Niagara_permits <- read_sf("Data/Building_permit_data/Niagara_Falls_Completed_Building_Permits_-2696795109850114882/OD_vw_LM_BuildingPermits_Completed_Public.shp")

#### Oakville Permits as sf

Oakville_permits <- read_sf("Data/Building_permit_data/Site_Alteration_Permits/Site_Alteration_Permits.shp")

#### St. Catherines Permits as sf

st_catherines_permits <- read_sf("Data/Building_permit_data/Building_Permits_Public_8302049438281531469/Building_Permits_(2010-Present).shp")

