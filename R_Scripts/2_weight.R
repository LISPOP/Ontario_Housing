
## 2021 Census PUMF as reference survey + weighting targets
pumf.2021.0 <- read_csv(here("Data/2021_individual_pumf.csv"))

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

# summary(prob.non.prob.1$ipsw.logit)
# hist(prob.non.prob.1$ipsw.logit)

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

# summary(data.6$weight)
# PracTools::deffK(data.6$weight)


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