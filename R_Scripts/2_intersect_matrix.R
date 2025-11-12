# this code is Tim's code to generate the touch matrix of DAs in Ontario
library(sf)
library(here)
library(dplyr)
library(tidyverse)
library(cancensus)
#Get the geometry files for dissemination areas for Ontario
#This usues a function in the cancensus package to directly download the geemotries from 
# Statistics Canada
da.2021.0 <- get_statcan_geographies(census_year="2021",level="DA", type="digital", cache_path=here("Data/cancensus_cache_statscan_data/"))

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
  left_join(., da.intersect.1, by="DA2021") # %>% view()
da.intersect.1 %>% 
  #If you insert a view() after this line, you should see
  # Several rows for each dissemination area; one row for each DA that intersects each DA
  left_join(., on_statscan_da, by=c("DA2021_intersect"="DA2021")) %>%
  left_join(., as.data.frame(DA_height) %>% 
              select(`Average Height`, DAUID) %>% 
              mutate(DAUID = as.numeric(DAUID)), by = c("DA2021_intersect" = "DAUID")) %>% 
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
  mutate(., across(c(households_more_than_30_da:pop_density_da, `Average Height`), ~mean(.,  na.rm=T), .names="{.col}_intersect1")) %>% 
  #This drops the columns that contain the individual statistics for the intersecting DAs
  # WE only want the averages which are stored with the suffix _intersect1
  select(-c(total_occupied_private_dwellings_da:Population, `Average Height`)) ->da.intersect.1
#Repeat with the second-order intersecting DAs
da.intersect.2 %>% 
  left_join(., on_statscan_da, by=c("DA2021_intersect"="DA2021")) %>% 
  left_join(., as.data.frame(DA_height) %>% 
              select(`Average Height`, DAUID) %>% 
              mutate(DAUID = as.numeric(DAUID)), by = c("DA2021_intersect" = "DAUID")) %>% 
  group_by(DA2021) %>% 
  mutate(., across(c(households_more_than_30_da:pop_density_da, `Average Height`), ~mean(.,  na.rm=T), .names="{.col}_intersect2")) %>% 
  select(-c(total_occupied_private_dwellings_da:Population, `Average Height`)) ->da.intersect.2

#Get Vectors

laurier <- da.2021.1 %>% 
  filter(DA2021 == 35300298)

neigbours <- da.2021.1 %>% 
  filter(st_touches(., laurier, sparse = FALSE)[,1]) %>% 
  mutate(type = "Neigbouring DA")


second_neighbors_index <- st_touches(da.2021.1, neigbours)
second_neighbors_ids <- unique(unlist(second_neighbors_index))

second_neighbors <- da.2021.1[second_neighbors_ids, ]

exclude_ids <- c(laurier$DA2021, neigbours$DA2021)

next_neighbors <- second_neighbors %>%
  filter(!(DA2021 %in% exclude_ids)) %>%
  mutate(type = "Next Neighbor")

laurier$type <- "Wilfrid Laurier University"


intersect_example <- bind_rows(laurier, neigbours, next_neighbors)


waterloo <- read_sf("Data/Municipal_Boundary_-8493345633645527618/Municipal_Boundary.shp") 

waterloo <- waterloo %>% 
  filter(MUNICIPALI == "WATERLOO")

waterloo <- st_transform(waterloo, st_crs(da.2021.1))

# Filter DAs that intersect or are within the city boundary
waterloo_das <- da.2021.1 %>%
  filter(st_intersects(., waterloo, sparse = FALSE)[,1])



laurier <- waterloo_das %>% 
  filter(DA2021 == 35300298)

neigbours <- waterloo_das %>% 
  filter(st_touches(., laurier, sparse = FALSE)[,1]) %>% 
  filter(DA2021 != laurier$DA2021) %>%
  mutate(type = "Neigbouring DA")


candidate_pool <- da.2021.1 %>%
  filter(st_intersects(., st_union(neigbours), sparse = FALSE)[,1])


next_neighbors <- candidate_pool %>%
  # filter(st_intersects(., neigbours, sparse = FALSE)[,1]) %>%
  filter(!(DA2021 %in% c(laurier$DA2021, neigbours$DA2021))) %>%
  mutate(type = "Next Neighbour")
# 
# second_neighbors_index <- st_touches(waterloo_das, neigbours)
# second_neighbors_ids <- unique(unlist(second_neighbors_index))

# second_neighbors <- waterloo_das[second_neighbors_ids, ]
# 
# exclude_ids <- c(laurier$DA2021, neigbours$DA2021)
# 
# next_neighbors <- second_neighbors %>%
#   filter(!(DA2021 %in% exclude_ids)) %>%
#   mutate(type = "Next Neighbor")

laurier$type <- "Wilfrid Laurier University"


intersect_example <- bind_rows(laurier, neigbours, next_neighbors)
intersect_example <- intersect_example %>% 
  mutate(type = factor(type, levels = c("Wilfrid Laurier University", "Neigbouring DA", "Next Neighbour")))
waterloo_das <- waterloo_das %>% 
    filter(!DA2021 %in% c(35300943, 35300747, 35300904, 35301007, 35300832, 35300774, 35300819))
  ggplot() +
geom_sf(data =  waterloo_das, fill = NA, color = "grey") +
  geom_sf(data = intersect_example, aes(fill = type), color = "black", size = 0.4) +
  theme_minimal() +
  labs(title = "Laurier with Neighbors and Next Neighbors", fill = "Dissemination Area") +
    theme(legend.position = "bottom",
          axis.text=element_blank(),
          panel.grid=element_blank()
          )
  ggsave(here("Plots/laurier_das.png"))

# Export 
# write_csv(da.intersect.1, here("data/ON DA intersections.csv"))
# write_csv(da.intersect.2, here("data/ON DA 1-2 order intersections.csv"))
