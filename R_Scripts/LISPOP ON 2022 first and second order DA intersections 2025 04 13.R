rm(list=ls())
setwd ("C:/Users/timgr/OneDrive - Wilfrid Laurier University/Academic/LISPOP ON Housing/")
options(scipen=999)
select <- dplyr::select
map <- purrr::map

library(tidyverse)
library(sf)

da.2021.0 <- read_sf("C:/Users/timgr/OneDrive - Wilfrid Laurier University/Geocoding/DA/lda_000a21a_e.shp")

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

write_csv(da.intersect.1, "ON DA intersections.csv")
write_csv(da.intersect.2, "ON DA 1-2 order intersections.csv")
