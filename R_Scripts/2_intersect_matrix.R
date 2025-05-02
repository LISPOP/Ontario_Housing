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

# Export 
# write_csv(da.intersect.1, here("data/ON DA intersections.csv"))
# write_csv(da.intersect.2, here("data/ON DA 1-2 order intersections.csv"))
