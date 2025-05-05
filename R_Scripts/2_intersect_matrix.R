# this code is Tim's code to generate the touch matrix of DAs in Ontario
library(sf)
library(here)
library(dplyr)
library(tidyverse)
#Get the geometry files for dissemination areas for Ontario
#This usues a function in the cancensus package to directly download the geemotries from 
# Statistics Canada
da.2021.0<-get_statcan_geographies(census_year="2021",level="DA", type="digital", cache_path=here("Data/cancensus_cache_statscan_data/"))
#da_geometry<-get_census("CA21", regions=list(PR="35"), level="DA", geo_format="sf")

da.2021.0 %>%
  filter(PRUID == "35") %>%
  as_tibble() %>%
  select(LANDAREA) %>%
  summary()

da.2021.1 <- da.2021.0 %>%
  filter(PRUID == "35") %>%
  mutate(row_id = as.integer(row_number())) %>%
  mutate(DA2021 = as.integer(DAUID)) %>%
  select(row_id, DA2021, geometry)

da.intersect.0 <- da.2021.1 %>%
  st_intersects(., remove_self = TRUE) %>%
  as.data.frame()

da.intersect.1 <- da.intersect.0 %>%
  left_join(., (da.2021.1 %>% as_tibble() %>% select(row_id, DA2021)), by = c("col.id" = "row_id")) %>%
  mutate(DA2021_intersect = DA2021) %>%
  select(-col.id, -DA2021) %>%
  left_join(., (da.2021.1 %>% as_tibble() %>% select(row_id, DA2021)), by = c("row.id" = "row_id")) %>%
  select(DA2021, DA2021_intersect) %>%
  arrange(DA2021, DA2021_intersect)

da.intersect.2 <- da.intersect.0 %>%
  left_join(., (da.2021.1 %>% as_tibble() %>% select(row_id, DA2021)), by = c("col.id" = "row_id")) %>%
  mutate(DA2021_intersect1 = DA2021) %>%
  select(-DA2021) %>%
  full_join(da.intersect.0, by = c("col.id" = "col.id"), relationship = "many-to-many") %>%
  left_join(., (da.2021.1 %>% as_tibble() %>% select(row_id, DA2021)), by = c("row.id.y" = "row_id")) %>%
  mutate(DA2021_intersect2 = DA2021) %>%
  select(-DA2021) %>%
  left_join(., (da.2021.1 %>% as_tibble() %>% select(row_id, DA2021)), by = c("row.id.x" = "row_id")) %>%
  select(DA2021, DA2021_intersect1, DA2021_intersect2) %>%
  pivot_longer(., cols = c(DA2021_intersect1, DA2021_intersect2), values_to = "DA2021_intersect") %>%
  distinct(DA2021, DA2021_intersect) %>%
  filter(DA2021 != DA2021_intersect) %>%
  arrange(DA2021, DA2021_intersect)

da.intersect.1 %>%
  group_by(DA2021) %>%
  summarise(n = n()) %>%
  summary()

da.intersect.2 %>%
  group_by(DA2021) %>%
  summarise(n = n()) %>%
  summary()
# names(on_statscan_da)
# names(da.intersect.1)
# glimpse(on_statscan_da)
# glimpse(da.intersect.1)
#THIS IS ESSENTIAL!!!!!!!!!!!!
# YOU HAVE TO GET THE STATISTICS FOR THE INTERSECTING DAS; NOT THE ORIGINAL DAS
# YOU HAVE TO DO THE JOIN USING THE VARIABLE DA2021_INTERSECT=DA_2021
# SO THAT WE GRAB THE STATS FOR THE= THE INTERSECTING DAS 
on_statscan_da %>% 
  left_join(., da.intersect.1, by="DA2021") %>% view()
da.intersect.1 %>% 
  #If you insert a view() after this line, you should see
  # Several rows for each dissemination area; one row for each DA that intersects each DA
  left_join(., on_statscan_da, by=c("DA2021_intersect"="DA2021")) %>%
  #This forms groups of each DA
  group_by(DA2021) %>% 
  #And then calculates the average of the intersecting DAs for each DA; it does it for several variables 
  #The averages are stored with the suffix intersect1 indicating that it is the first order intersecting
  #Note that I use mutate rather than summarize here because I want to keep all the rows of the in
  # intersecting DAs to preserve the intersection matrix
  # It is really worth interrupting this chain, step by step with a view() to see what is going on
  # At the end; there should be multiple rows for each DA2021 becausethe 
  # intersect matrix does;We don't want to lose that structured
  # But it should contain multiple rows of the same statistics for each DA, because that is after averaging
  # The values of the intersecting DAs. 
  mutate(., across(households_more_than_30_da:pop_density_da, ~mean(.,  na.rm=T), .names="{.col}_intersect1")) %>% 
  #This drops the columns that contain the individual statistics for the intersecting DAs
  # WE only want the averages which are stored with the suffix _intersect1
  select(-c(total_occupied_private_dwellings_da:Population)) ->da.intersect.1
#Repeat with the second-order intersecting DAs
da.intersect.2 %>% 
  left_join(., on_statscan_da, by=c("DA2021_intersect"="DA2021")) %>% 
  group_by(DA2021) %>% 
  mutate(., across(households_more_than_30_da:pop_density_da, ~mean(.,  na.rm=T), .names="{.col}_intersect2")) %>% 
  select(-c(total_occupied_private_dwellings_da:Population)) ->da.intersect.2

#Get Vectors


# Export 
# write_csv(da.intersect.1, here("data/ON DA intersections.csv"))
# write_csv(da.intersect.2, here("data/ON DA 1-2 order intersections.csv"))
