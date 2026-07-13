###### Code to Import Building Permit Data #####


#### Toronto Permit Data as CSV

toronto_permits <- read.csv("Data/Building_permit_data/Cleared Building Permits since 2017.csv")

toronto_permits <- toronto_permits %>% 
  mutate(change = ifelse(PROPOSED_USE == CURRENT_USE, 0, 1))

toronto_permits_change <- toronto_permits %>% 
  filter(change == 1)

table(toronto_permits_change$DWELLING_UNITS_CREATED)
#### Barrie Permit Data as sf

barrie_permits <- read_sf("Data/Building_permit_data/Planning_Applications/Planning_Applications.shp")

#### Burlington Permit Data as sf

burlington_permits <- read_sf("Data/Building_permit_data/Building_Permits/Building_Permits.shp")

#### Waterloo Permit Data as sf

Waterloo_permits <- read_sf("Data/Building_permit_data/City_of_Waterloo_Building_Permits_-5783174737437332337/BUILDING_PERMITS_wgs84.shp")

#### Kitchener Permit Data as sf

Kitchener_permits <- read_sf("Data/Building_permit_data/Building_Permits_755659247081512046/Building_Permit.shp")

#### Hamilton Permit Data as sf

Hamilton_permits <- read_sf("Data/Building_permit_data/Building_and_Demolition_Permits_2017_to_Present/Building_and_Demolition_Permits_2017_to_Present.shp")

#### Ottawa Permits as csv

Ottawa_permits_2020 <- readxl::read_xls("Data/Building_permit_data/permits-2020.xls")
Ottawa_permits_2021 <- readxl::read_xls("Data/Building_permit_data/permits_2021_EN.xls")

#### Mississauga Permits as sf

Mississauga_permits <- read_sf("Data/Building_permit_data/Issued_Building_Permits_5749794004220210135/Building_Permits.shp")

#### Niagara Falls Permits as sf 

Niagara_permits <- read_sf("Data/Building_permit_data/Niagara_Falls_Completed_Building_Permits_-2696795109850114882/OD_vw_LM_BuildingPermits_Completed_Public.shp")

#### Oakville Permits as sf

Oakville_permits <- read_sf("Data/Building_permit_data/Site_Alteration_Permits/Site_Alteration_Permits.shp")

#### St. Catherines Permits as sf

st_catherines_permits <- read_sf("Data/Building_permit_data/Building_Permits_Public_8302049438281531469/Building_Permits_(2010-Present).shp")

