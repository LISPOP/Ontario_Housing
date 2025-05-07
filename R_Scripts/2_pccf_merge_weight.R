# rm(list=ls())
# setwd ("C:/Users/timgr/OneDrive - Wilfrid Laurier University/Academic/LISPOP ON Housing/")
# options(scipen=999)
# select <- dplyr::select
# map <- purrr::map

library(tidyverse)
library(haven)
library(sf)
library(survey)
library(lubridate)
library(data.table)
library(units)



## Read in survey data from LISPOP
#on22 <- read_sav("opes22_2022-09-24.sav")

var_names_labels <- as_tibble(colnames(on22)) %>%
  rename(vname = value) %>%
  bind_cols(as_tibble(sjlabelled::get_label(on22))) %>%
  rename(vlab = value)

xtabs(~ sjlabelled::as_character(Q38), addNA = TRUE, na.action = NULL, data = on22)
xtabs(~ yob, addNA = TRUE, na.action = NULL, data = on22)
xtabs(~ sjlabelled::as_character(Q39), addNA = TRUE, na.action = NULL, data = on22)
xtabs(~ sjlabelled::as_character(Q40), addNA = TRUE, na.action = NULL, data = on22)


## Clean data: identify respondents in Ontario
data.1 <- on22 %>%
  mutate(postal.code = str_to_upper(Q47)) %>%
  mutate(postal.code = str_replace_all(postal.code, "[:punct:]", "")) %>%
  mutate(postal.code = str_replace_all(postal.code, "[:blank:]", " ")) %>%
  mutate(postal.code = str_replace_all(postal.code, " ", "")) %>%
  mutate(postal.code = str_trim(postal.code, side = c("both"))) %>%
  mutate(FSA = str_sub(postal.code, 1, 3)) %>%
  distinct(ResponseId, .keep_all = TRUE)

#CReate

## Read in PCCF
pccf.24 <- read_fwf(here('Data/PCCF/TXT/PCCF_FCCP_V2409_2021.txt'),
                    guess_max = 500000, trim_ws = TRUE, skip = 0, locale = readr::locale(encoding = "latin1"),
                    fwf_cols(postal.code = c(1,6),
                             FSA = c(7,9),
                             PR = c(10,11),
                             FED2013 = c(116,120),
                             CSD2021 = c(16,22),
                             CSDNAME2021 = c(23,92),
                             CMA2021 = c(99,101),
                             CT2021 = c(103,109),
                             DA2021 = c(126,133),
                             Y = c(138,148),
                             X = c(149,161),
                             SLI = c(162,162),
                             PCtype = c(163,163),
                             Comm_Name = c(164,193),
                             #Birth_Date = c(196,203),
                             Ret_Date = c(204,211),
                             QI = c(213,215),
                             Source = c(216,216)
                    )) %>%
  filter(PR == 35 & Ret_Date == 19000001 & SLI == 1) %>%
  arrange(postal.code)

# Get Necessary Statcan Geographies

library(cancensus)
#get fsa file


# 
# ## Create FSA lat-lon coordinates from FSA centroids
#fsa.shp <- read_sf("C:/Users/timgr/OneDrive - Wilfrid Laurier University/Geocoding/FSA/lfsa000b21a_e.shp") %>%
fsa.shp<-get_statcan_geographies("2021",type="cartographic", level=c("FSA"), timeout=100) %>% 
  mutate(FSA = CFSAUID) %>%
  mutate(PR = as.integer(PRUID)) %>%
  group_by(FSA, PR) %>%
  summarise() %>%
  st_centroid() %>%
  ungroup() %>%
  st_transform(., crs = 4326) %>%
  as_tibble() %>%
  filter(PR == 35) %>%
  mutate(latlon = as.character(geometry)) %>%
  mutate(X = as.numeric(str_sub(latlon, 3, (str_locate(latlon, ",")[,1] - 1)))) %>%
  mutate(Y = as.numeric(str_sub(latlon, (str_locate(latlon, ",")[,1] + 1), (str_length(latlon) - 1)))) %>%
  select(FSA, PR, Y, X) %>%
  distinct(FSA, .keep_all = TRUE) %>%
  arrange(FSA)
#Get DA file
da.shp<- get_statcan_geographies("2021",type="digital", level=c("DA"), timeout=100)%>% 
  st_transform(., crs = 3347) %>% 
  mutate(DAUID=as.integer(DAUID)) %>% 
  mutate(CDUID=str_sub(DAUID,start=1, end=4))

# ## FED digital boundary file
#fed.shp <- read_sf("C:/Users/timgr/OneDrive - Wilfrid Laurier University/Geocoding/FED 2013/lfed000a21a_e.shp") %>%
#Get fed file
fed.shp<- get_statcan_geographies("2021",type="digital", level=c("FED"), timeout=100)%>% 
  st_transform(., crs = 3347) %>%
  mutate(FED2013 = as.integer(FEDUID)) %>%
  mutate(PR = as.integer(PRUID)) %>%
  filter(PR %in% 10:59) %>%
  select(FED2013, FEDENAME, geometry)


fed.names <- as.data.frame(fed.shp) %>%
  as_tibble() %>%
  select(-geometry)

# 
# ## CSD digital boundary file
# csd.shp <- read_sf("C:/Users/timgr/OneDrive - Wilfrid Laurier University/Geocoding/CSD/lcsd000b21a_e.shp") %>%
#Get fed file
csd.shp<-get_statcan_geographies("2021",type="digital", level=c("CSD"), timeout=100) %>% 
  st_transform(., crs = 3347) %>%
  mutate(CSD2021 = as.integer(CSDUID)) %>%
  mutate(CSDNAME2021 = CSDNAME) %>%
  mutate(PR = as.integer(PRUID)) %>%
  filter(PR %in% 10:59) %>%
  select(CSD2021, CSDNAME2021, LANDAREA, geometry)

# 
# ## Join to PCCF
#names(data.1)

data.1a <- data.1 %>%
  left_join(select(pccf.24, -FSA), by = "postal.code") %>%
  mutate(FED2013 = as.integer(FED2013)) %>%
  mutate(CSD2021 = as.integer(CSD2021)) %>%
  mutate(DA2021 = as.integer(DA2021)) %>% 
  mutate(first_match=case_when(
    is.na(DA2021)~0,
    !is.na(DA2021)~1
  ))


# ## Secondary match on FSA
data.1b <- data.1a %>%
  filter(is.na(Y) == TRUE | is.na(X) == TRUE) %>%names()
  select(c(ResponseId:pid, FSA, Duration__in_seconds_)) %>%
  inner_join(fsa.shp, by = "FSA") %>%
  bind_rows(., data.1a %>% filter(is.na(Y) == FALSE & is.na(X) == FALSE & is.na(FED2013) == TRUE)) %>%
  select(ResponseId:pid, postal.code, FSA, Y, X, Duration__in_seconds_) %>%
  st_as_sf(., coords = c("X","Y"), crs = 4326) %>%
  st_transform(., crs = 3347) 


data.1c <- data.1b %>%
  st_join(., fed.shp, join = st_within) %>%
  as_tibble()


data.1d <- data.1b %>%
  st_join(., csd.shp, join = st_within) %>%
  as_tibble() %>%
  arrange(FSA) %>%
  fill(c(CSD2021:LANDAREA), .direction = "down")

data.1e <- data.1b %>%
  left_join((select(data.1c, ResponseId, FED2013, FEDENAME)), by = "ResponseId") %>%
  left_join((select(data.1d, ResponseId, CSD2021, CSDNAME2021)), by = "ResponseId") %>%
  st_transform(., crs = 4326) %>%
  mutate(latlon = as.character(geometry)) %>%
  as_tibble() %>%
  mutate(X = as.numeric(str_sub(latlon, 3, (str_locate(latlon, ",")[,1] - 1)))) %>%
  mutate(Y = as.numeric(str_sub(latlon, (str_locate(latlon, ",")[,1] + 1), (str_length(latlon) - 1)))) %>%
  mutate(PR = as.integer(str_sub(CSD2021, 1, 2))) %>%
  select(-geometry, -latlon)

table(is.na(data.1e$Duration__in_seconds_))
## Secondary match on lat/lon from Qualtrics geolocation
data.1f <- data.1a %>%
  filter(is.na(Y) == TRUE | is.na(X) == TRUE) %>%
  anti_join(., select(data.1e, ResponseId), by = "ResponseId") %>%
  mutate(Y = as.numeric(LocationLatitude)) %>%
  mutate(X = as.numeric(LocationLongitude)) %>%
  select(ResponseId:pid, postal.code, FSA, Y, X, Duration__in_seconds_) %>%
  st_as_sf(., coords = c("X","Y"), crs = 4326) %>%
  st_transform(., crs = 3347)

data.1g <- data.1f %>%
  st_join(., fed.shp, join = st_within) %>%
  as_tibble()

data.1h <- data.1f %>%
  st_join(., csd.shp, join = st_within) %>%
  as_tibble()

data.1i <- data.1f %>%
  st_transform(., crs = 4326) %>%
  left_join((select(data.1g, ResponseId, FED2013, FEDENAME)), by = "ResponseId") %>%
  left_join((select(data.1h, ResponseId, CSD2021, CSDNAME2021)), by = "ResponseId") %>%
  mutate(latlon = as.character(geometry)) %>%
  as_tibble() %>%
  mutate(X = as.numeric(str_sub(latlon, 3, (str_locate(latlon, ",")[,1] - 1)))) %>%
  mutate(Y = as.numeric(str_sub(latlon, (str_locate(latlon, ",")[,1] + 1), (str_length(latlon) - 1)))) %>%
  mutate(PR = as.integer(str_sub(CSD2021, 1, 2))) %>%
  select(-geometry, -latlon)


## The whole shebang
data.2 <- data.1a %>%
  filter(is.na(Y) == FALSE | is.na(X) == FALSE) %>%
  bind_rows(data.1e) %>%
  bind_rows(data.1i) %>% 
  select(-psid, -CSDNAME2021, -SLI, -Ret_Date:-FEDENAME) %>% 
  left_join(fed.names, by = "FED2013") %>%
  left_join((csd.shp %>% as.data.frame() %>% select(-geometry)), by = "CSD2021") %>%
  mutate(PR = case_when(
    is.na(PR) == FALSE ~ PR,
    is.na(PR) == TRUE & is.na(FED2013) == FALSE ~ as.integer(str_sub(as.character(FED2013), 1, 2))
  )) %>%
  mutate(VOTE2018 = as_factor(case_when(
    Q6a == 2 ~ "DNV",
    Q6a == 1 & Q6b == 2 ~ "PCPO",
    Q6a == 1 & Q6b == 1 ~ "LPO",
    Q6a == 1 & Q6b == 3 ~ "NDPO",
    Q6a == 1 & Q6b == 4 ~ "GPO",
    Q6a == 1 & Q6b == 5 ~ "OTH",
    TRUE ~ "DNV"
  ))) %>%
  mutate(VOTE2018 = fct_relevel(VOTE2018, "PCPO", "LPO", "NDPO", "GPO", "OTH", "DNV")) %>%
  mutate(VOTE2022 = as_factor(case_when(
    Q8 == 2 | Q9 == 2 | Q10 == 2 | Q11 == 2 ~ "PCPO",
    Q8 == 1 | Q9 == 1 | Q10 == 1 | Q11 == 1 ~ "LPO",
    Q8 == 3 | Q9 == 3 | Q10 == 3 | Q11 == 3 ~ "NDPO",
    Q8 == 4 | Q9 == 4 | Q10 == 4 | Q11 == 4 ~ "GPO",
    Q8 == 5 | Q9 == 5 | Q10 == 5 | Q11 == 5 ~ "OTH",
    TRUE ~ "NV"
  ))) %>%
  mutate(VOTE2022 = fct_relevel(VOTE2022, "PCPO", "LPO", "NDPO", "GPO", "OTH", "NV")) %>%
  mutate(SEX = as_factor(case_when(
    Q38 == 1 ~ "Male",
    Q38 == 2 ~ "Female",
    TRUE ~ sample(c("Male", "Female"), n(), replace = TRUE)
  ))) %>%
  mutate(SEX = fct_relevel(SEX, "Male", "Female")) %>%
  mutate(age_years = 2022 - as.integer(yob)) %>%
  mutate(AGE = as_factor(case_when(
    age_years %in% 18:24 ~ "18-24",
    age_years %in% 25:34 ~ "25-34",
    age_years %in% 35:44 ~ "35-44",
    age_years %in% 45:54 ~ "45-54",
    age_years %in% 55:64 ~ "55-64",
    age_years %in% 65:74 ~ "65-74",
    age_years %in% 75:120 ~ "75+",
    TRUE ~ sample(c("25-34", "35-44", "45-54", "55-64", "65-74", "75+"), n(), replace = TRUE)
  ))) %>%
  mutate(AGE = fct_relevel(AGE, "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")) %>%
  mutate(EDU5 = as_factor(case_when(
    Q39 %in% 1:4 ~ "Less than HS",
    Q39 %in% c(5, 6, 8) ~ "High school",
    Q39 == 7 ~ "College/trades",
    Q39 == 9 ~ "Bachelor degree",
    Q39 %in% 10:11 ~ "Graduate degree",
    TRUE ~ "Less than HS"
  ))) %>%
  mutate(EDU5 = fct_relevel(EDU5, "Less than HS", "High school", "College/trades", "Bachelor degree", "Graduate degree")) %>%
  mutate(EDU4 = as_factor(case_when(
    Q39 %in% 1:5 ~ "HS or less",
    Q39 %in% c(6, 8) ~ "HS or less",
    Q39 %in% c(6, 8) ~ "HS or less",
    Q39 == 7 ~ "College/trades",
    Q39 == 9 ~ "Bachelor degree",
    Q39 %in% 10:11 ~ "Graduate degree",
    TRUE ~ "HS or less"
  ))) %>%
  mutate(EMP = as_factor(case_when(
    Q40 %in% 1:3 ~ "Employed",
    Q40 == 5 ~ "Unemployed",
    Q40 %in% c(4, 6, 7, 8) ~ "NILF",
    Q40 %in% 9:11 ~ "Employed",
    Q40 == 12 & str_detect(str_to_lower(Q40_12_TEXT), "freelance") == TRUE ~ "Employed",
    Q40 == 12 & str_detect(str_to_lower(Q40_12_TEXT), "retired") == TRUE ~ "NILF",
    Q40 == 12 & str_detect(str_to_lower(Q40_12_TEXT), "disability") == TRUE ~ "NILF",
    Q40 == 12 & str_detect(str_to_lower(Q40_12_TEXT), "leave") == TRUE ~ "NILF",
    Q40 == 12 & str_detect(str_to_lower(Q40_12_TEXT), "looking") == TRUE ~ "Unemployed",
    TRUE ~ "NILF"
  ))) %>%
  mutate(EMP = fct_relevel(EMP, "Employed", "Unemployed", "NILF")) %>%
  mutate(HH_SIZE = as_factor(case_when(
    Q46 %in% 1:5 ~ Q46,
    Q46 %in% 6:99 ~ 5,
    TRUE ~ 1
  ))) %>%
  mutate(HH_SIZE = fct_relevel(HH_SIZE, "1", "2", "3", "4", "5")) %>%
  mutate(EDU4 = fct_relevel(EDU4, "HS or less", "College/trades", "Bachelor degree", "Graduate degree")) %>%
  filter((age_years >= 18 | is.na(age_years) == TRUE)
         & PR == 35) %>%
  mutate(WEIGHT = as.numeric(1)) %>%
  mutate(non_prob = as.integer(1))

xtabs(~ PR, addNA = TRUE, na.action = NULL, data = data.2)
xtabs(~ SEX, addNA = TRUE, na.action = NULL, data = data.2)
xtabs(~ AGE, addNA = TRUE, na.action = NULL, data = data.2)
xtabs(~ EDU4, addNA = TRUE, na.action = NULL, data = data.2)
xtabs(~ EDU5, addNA = TRUE, na.action = NULL, data = data.2)
xtabs(~ EMP, addNA = TRUE, na.action = NULL, data = data.2)
xtabs(~ HH_SIZE, addNA = TRUE, na.action = NULL, data = data.2)
xtabs(~ VOTE2018, addNA = TRUE, na.action = NULL, data = data.2)
xtabs(~ VOTE2018, addNA = TRUE, na.action = NULL, data = data.2) %>% prop.table()*100
xtabs(~ VOTE2022, addNA = TRUE, na.action = NULL, data = data.2)
xtabs(~ VOTE2022, addNA = TRUE, na.action = NULL, data = data.2) %>% prop.table()*100

# Spit out REspondents that do not have DAs
data.2 %>% 
  filter(is.na(DA2021)) %>% 
  select(ResponseId,postal_code, postal.code, FSA, X, Y, Comm_Name) %>% write_csv(., file=here("Data/no_DA_fsa_lat_lon.csv"))


## 2021 Census PUMF as reference survey + weighting targets
pumf.2021.0 <- read_csv(here("Data/2021_individual_pumf.csv"))
look_for(pumf.2021.0, "weight")
summary(pumf.2021.0$WEIGHT)
pumf.2021.1 <- pumf.2021.0 %>%
  mutate(ResponseId = as.character(PPSORT)) %>%
  filter(AGEGRP %in% 7:21 & Citizen %in% 1:2 & PR == 35) %>%
  mutate(SEX = as_factor(case_when(
    Gender == 2 ~ "Male",
    Gender == 1 ~ "Female"
  ))) %>%
  mutate(SEX = fct_relevel(SEX, "Male", "Female")) %>%
  mutate(AGE = as_factor(case_when(
    AGEGRP %in% 7:8 ~ "18-24",
    AGEGRP %in% 9:10 ~ "25-34",
    AGEGRP %in% 11:12 ~ "35-44",
    AGEGRP %in% 13:14 ~ "45-54",
    AGEGRP %in% 15:16 ~ "55-64",
    AGEGRP %in% 17:18 ~ "65-74",
    AGEGRP %in% 17:21 ~ "75+"
  ))) %>%
  mutate(EDU5 = as_factor(case_when(
    HDGREE == 1 ~ "Less than HS",
    HDGREE == 2 ~ "High school",
    HDGREE %in% 3:8 ~ "College/trades",
    HDGREE %in% 9:10 ~ "Bachelor degree",
    HDGREE %in% 11:13 ~ "Graduate degree",
    HDGREE == 88 ~ "Less than HS"
  ))) %>%
  mutate(EDU5 = fct_relevel(EDU5, "Less than HS", "High school", "College/trades", "Bachelor degree", "Graduate degree")) %>%
  mutate(EDU4 = as_factor(case_when(
    HDGREE %in% 1:2 ~ "HS or less",
    HDGREE %in% 3:8 ~ "College/trades",
    HDGREE %in% 9:10 ~ "Bachelor degree",
    HDGREE %in% 11:13 ~ "Graduate degree",
    HDGREE == 88 ~ "HS or less"
  ))) %>%
  mutate(EDU4 = fct_relevel(EDU4, "HS or less", "College/trades", "Bachelor degree", "Graduate degree")) %>%
  mutate(EMP = as_factor(case_when(
    LFACT %in% 1:2 ~ "Employed",
    LFACT %in% 3:10 ~ "Unemployed",
    LFACT %in% 11:88 ~ "NILF",
    TRUE ~ "NILF"
  ))) %>%
  mutate(EMP = fct_relevel(EMP, "Employed", "Unemployed", "NILF")) %>%
  mutate(HH_SIZE = case_when(
    HHSIZE %in% 1:5 ~ HHSIZE,
    HHSIZE %in% 6:7 ~ 5,
    TRUE ~ 1
  )) %>% 
  mutate(HH_SIZE =factor(HH_SIZE, levels=c("1", "2", "3", "4", "5"))) %>%
  mutate(non_prob = as.integer(0))

xtabs(~ PR, addNA = TRUE, na.action = NULL, data = pumf.2021.1)
xtabs(~ SEX, addNA = TRUE, na.action = NULL, data = pumf.2021.1)
xtabs(~ AGE, addNA = TRUE, na.action = NULL, data = pumf.2021.1)
xtabs(~ EDU4, addNA = TRUE, na.action = NULL, data = pumf.2021.1)
xtabs(~ EDU5, addNA = TRUE, na.action = NULL, data = pumf.2021.1)
xtabs(~ EMP, addNA = TRUE, na.action = NULL, data = pumf.2021.1)
xtabs(~ HH_SIZE, addNA = TRUE, na.action = NULL, data = pumf.2021.1)
summary(pumf.2021.1$WEIGHT)


target.sex <- pumf.2021.1 %>%
  group_by(SEX) %>%
  summarise(n = sum(WEIGHT)) %>%
  ungroup() %>%
  mutate(target = n / sum(n)) %>%
  select(-n)
print(target.sex)

target.age <- pumf.2021.1 %>%
  group_by(AGE) %>%
  summarise(n = sum(WEIGHT)) %>%
  ungroup() %>%
  mutate(target = n / sum(n)) %>%
  select(-n)
print(target.age)

target.edu <- pumf.2021.1 %>%
  group_by(EDU4) %>%
  summarise(n = sum(WEIGHT)) %>%
  ungroup() %>%
  mutate(target = n / sum(n)) %>%
  select(-n)
print(target.edu)

target.sex.age <- pumf.2021.1 %>%
  group_by(SEX, AGE) %>%
  summarise(n = sum(WEIGHT)) %>%
  ungroup() %>%
  mutate(target = n / sum(n)) %>%
  select(-n)
print(target.sex.age)

target.sex.edu <- pumf.2021.1 %>%
  group_by(SEX, EDU4) %>%
  summarise(n = sum(WEIGHT)) %>%
  ungroup() %>%
  mutate(target = n / sum(n)) %>%
  select(-n)
print(target.sex.edu)
# 
# 
# ## 2018 and 2022 vote targets
# ## Source: https://results.elections.on.ca/en/data-explorer?fromYear=2018&toYear=2022&levelOfDetail=party
elec.2022.0 <- read_csv(here("Data/ontario_election_results_2022.csv"))

elec.2022.1 <- elec.2022.0 %>%
  mutate(N = as.integer(`Votes Cast`)) %>%
  mutate(VOTE2022 = case_when(
    Party == "Progressive Conservative Party of Ontario" ~ "PCPO",
    Party == "Ontario Liberal Party" ~ "LPO",
    Party == "New Democratic Party of Ontario" ~ "NDPO",
    Party == "Green Party of Ontario" ~ "GPO",
    TRUE ~ "OTH"
  )) %>%
  group_by(VOTE2022) %>%
  summarise(N = sum(N)) %>%
  ungroup() %>%
  arrange(desc(N))
# 
sample.vote.2022 <- data.2 %>%
  group_by(VOTE2022) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(percent = n / sum(n))

target.vote.2022 <- elec.2022.1 %>%
  mutate(target = N / sum(N)) %>%
  bind_cols(., (sample.vote.2022 %>%
                  filter(VOTE2022 == "NV") %>%
                  select(percent) %>%
                  rename(nv.percent = percent))) %>%
  mutate(target = target * (1 - nv.percent)) %>%
  bind_rows(., (sample.vote.2022 %>%
                  filter(VOTE2022 == "NV") %>%
                  select(VOTE2022, percent) %>%
                  rename(target = percent))) %>%
  select(VOTE2022, target)
target.vote.2022
sum(target.vote.2022$target)

# 
elec.2018.0 <- read_csv(here("Data/ontario_election_results_2018.csv"))
# 
elec.2018.1 <- elec.2018.0 %>%
  mutate(N = as.integer(`Votes Cast`)) %>%
  mutate(VOTE2018 = case_when(
    Party == "Progressive Conservative Party of Ontario" ~ "PCPO",
    Party == "Ontario Liberal Party" ~ "LPO",
    Party == "New Democratic Party of Ontario" ~ "NDPO",
    Party == "Green Party of Ontario" ~ "GPO",
    TRUE ~ "OTH"
  )) %>%
  group_by(VOTE2018) %>%
  summarise(N = sum(N)) %>%
  ungroup() %>%
  arrange(desc(N))

sample.vote.2018 <- data.2 %>%
  group_by(VOTE2018) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(percent = n / sum(n))

target.vote.2018 <- elec.2018.1 %>%
  mutate(target = N / sum(N)) %>%
  bind_cols(., (sample.vote.2018 %>%
                  filter(VOTE2018 == "DNV") %>%
                  select(percent) %>%
                  rename(nv.percent = percent))) %>%
  mutate(target = target * (1 - nv.percent)) %>%
  bind_rows(., (sample.vote.2018 %>%
                  filter(VOTE2018 == "DNV") %>%
                  select(VOTE2018, percent) %>%
                  rename(target = percent))) %>%
  select(VOTE2018, target)
target.vote.2018
sum(target.vote.2018$target)


## Pseudo-selection weights
prob.non.prob.0 <- bind_rows((select(data.2, ResponseId, WEIGHT, non_prob, SEX, AGE, EDU5, EMP, HH_SIZE)),
                             (select(pumf.2021.1, ResponseId, WEIGHT, non_prob, SEX, AGE, EDU5, EMP, HH_SIZE)))

xtabs(~ SEX + non_prob, addNA = TRUE, na.action = NULL, data = prob.non.prob.0)
xtabs(~ AGE + non_prob, addNA = TRUE, na.action = NULL, data = prob.non.prob.0)
xtabs(~ EDU5 + non_prob, addNA = TRUE, na.action = NULL, data = prob.non.prob.0)
xtabs(~ EMP + non_prob, addNA = TRUE, na.action = NULL, data = prob.non.prob.0)
xtabs(~ HH_SIZE + non_prob, addNA = TRUE, na.action = NULL, data = prob.non.prob.0)

m.nonprob.logit <- glm(non_prob ~ SEX + AGE + EDU5 + EMP + HH_SIZE
                       + SEX*AGE + SEX*EDU5 + SEX*EMP + AGE*EDU5,
                       binomial(link = "logit"), weights = WEIGHT, data = prob.non.prob.0)
jtools::summ(m.nonprob.logit, digits = 3)

prob.non.prob.1 <- prob.non.prob.0 %>%
  mutate(p.nonprob.logit = predict(m.nonprob.logit, type = "response")) %>%
  mutate(p.prob.logit = 1 - p.nonprob.logit) %>%
  filter(non_prob == 1) %>%
  mutate(ipsw.logit = 1 / p.nonprob.logit) %>%
  mutate(ipsw.logit = ipsw.logit * (n() / sum(ipsw.logit))) %>%
  mutate(ipsw.logit = case_when(
    ipsw.logit > 4 ~ 4,
  ipsw.logit < 0.2 ~ 0.2,
    TRUE ~ ipsw.logit
  )) %>%
  select(ResponseId, p.nonprob.logit, ipsw.logit)

summary(prob.non.prob.1$ipsw.logit)
hist(prob.non.prob.1$ipsw.logit)

data.2 <- data.2 %>%
  left_join(., (select(prob.non.prob.1, ResponseId, ipsw.logit)), by = "ResponseId") %>%
  select(-WEIGHT, -non_prob)


## Rake weights
p <- c(0,.005,.01,.015,.02,.025,.04,.05,.1,.25,.5,.75,.9,.95,.96,.975,.98,.985,.99,.995,1)
p.names <- map_chr(p, ~paste0(.x*100, "pctl"))
p.list <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% set_names(nm = p.names)

data.3 <- rake(design = svydesign(ids = ~1, data = data.2, weights = data.2$ipsw.logit),
                 sample.margins = list(~SEX+AGE,
                                       ~SEX+EDU4,
                                       ~VOTE2022,
                                       ~VOTE2018,
                                       ~EDU4,
                                       ~AGE,
                                       ~SEX
                 ),
                 population.margins = list(target.sex.age,
                                           target.sex.edu,
                                           target.vote.2022,
                                           target.vote.2018,
                                           target.edu,
                                           target.age,
                                           target.sex
                 ),
                 control = list(maxit = 200, epsilon = 1e8, verbose = FALSE))
data.3

data.3 <- data.2 %>%
  mutate(weight = weights(data.3)) %>%
  mutate(weight = weight * (n() / sum(weight)))

weight.pctiles <- data.3 %>%
  select(weight) %>%
  summarize_at(vars(weight), p.list)
print(weight.pctiles %>% pivot_longer(., cols = `0pctl`:`100pctl`), n = 25)

data.4 <- data.3 %>%
  mutate(weight = case_when(
    weight <= 0.2 ~ 0.2,
    weight >= 3.6 ~ 3.6,
    #weight <= weight.pctiles$`2pctl` ~ weight.pctiles$`2pctl`,
    #weight >= weight.pctiles$`98pctl` ~ weight.pctiles$`98pctl`,
    TRUE ~ weight
  )) %>%
  mutate(weight = weight * (n() / sum(weight)))

data.5 <- rake(design = svydesign(ids = ~1, data = data.4, weights = data.4$weight),
               sample.margins = list(~EDU4,
                                     ~AGE,
                                     ~SEX
               ),
               population.margins = list(target.edu,
                                         target.age,
                                         target.sex
               ),
               control = list(maxit = 200, epsilon = 1e8, verbose = FALSE))

data.5 <- data.2 %>%
  mutate(weight = weights(data.5)) %>%
  mutate(weight = weight * (n() / sum(weight)))

weight.pctiles <- data.5 %>%
  select(weight) %>%
  summarize_at(vars(weight), p.list)
print(weight.pctiles %>% pivot_longer(., cols = `0pctl`:`100pctl`), n = 25)

data.6 <- data.5 %>%
  mutate(weight = case_when(
    weight <= weight.pctiles$`1.5pctl` ~ weight.pctiles$`1.5pctl`,
    weight >= weight.pctiles$`98.5pctl` ~ weight.pctiles$`98.5pctl`,
    TRUE ~ weight
  )) %>%
  mutate(weight = weight * (n() / sum(weight)))

summary(data.6$weight)
PracTools::deffK(data.6$weight)


## Compare weighted data to targets
compare.sex <- data.6 %>%
  group_by(SEX) %>%
  summarise(n = sum(weight)) %>%
  ungroup() %>%
  mutate(wtd_pct = n / sum(n)) %>%
  left_join(target.sex, by = "SEX") %>%
  mutate(wtd_pct = wtd_pct*100,
         target = target*100) %>%
  ungroup() %>%
  mutate(diff = wtd_pct - target)
compare.sex

compare.age <- data.6 %>%
  group_by(AGE) %>%
  summarise(n = sum(weight)) %>%
  ungroup() %>%
  mutate(wtd_pct = n / sum(n)) %>%
  left_join(target.age, by = "AGE") %>%
  mutate(wtd_pct = wtd_pct*100,
         target = target*100) %>%
  ungroup() %>%
  mutate(diff = wtd_pct - target)
compare.age

compare.edu <- data.6 %>%
  group_by(EDU4) %>%
  summarise(n = sum(weight)) %>%
  ungroup() %>%
  mutate(wtd_pct = n / sum(n)) %>%
  left_join(target.edu, by = "EDU4") %>%
  mutate(wtd_pct = wtd_pct*100,
         target = target*100) %>%
  ungroup() %>%
  mutate(diff = wtd_pct - target)
compare.edu

compare.vote.2022 <- data.6 %>%
  group_by(VOTE2022) %>%
  summarise(n = sum(weight)) %>%
  ungroup() %>%
  mutate(wtd_pct = n / sum(n)) %>%
  left_join(target.vote.2022, by = "VOTE2022") %>%
  mutate(wtd_pct = wtd_pct*100,
         target = target*100) %>%
  ungroup() %>%
  mutate(diff = wtd_pct - target)
compare.vote.2022

compare.vote.2018 <- data.6 %>%
  group_by(VOTE2018) %>%
  summarise(n = sum(weight)) %>%
  ungroup() %>%
  mutate(wtd_pct = n / sum(n)) %>%
  left_join(target.vote.2018, by = "VOTE2018") %>%
  mutate(wtd_pct = wtd_pct*100,
         target = target*100) %>%
  ungroup() %>%
  mutate(diff = wtd_pct - target)
compare.vote.2018



## Save out data
on22<- data.6 %>%
  select(-c(VOTE2018:HH_SIZE), -ipsw.logit)
nrow(on22)
sum(on22$weight)
mean(on22$weight)
summary(on22$weight)

table(is.na(data.6$DA2021), useNA = "ifany")
table(is.na(on22$Duration__in_seconds_))


# 
# write_sav(data_out, "opes22_wtd_20250410.sav", compress = TRUE)
# saveRDS(data_out, "opes22_wtd_20250410.rds")