#### Missing Values 
source("R_Scripts/2_recodes.R")
# 1. Make a variable postal_code_good that returns 1 if nchar(postal_code)==6; else return missing N
# 2. make a variable da_good that returns 1 if PRCDA is not missing; else returns missing
# 3. Select those two variables above as well as the variables we are likely to use
# e.g. renter status, provincial party id, experimental group, anything else?
# Select postal_code_good, da_good and a few other key variables: party _ID
o
on22%>%
  mutate(postal_code_good = ifelse(nchar(postal_code)==6, "1", NA))->on22

on22%>%
  mutate(da_good = ifelse(nchar(DA2021)==8, "1", NA))->on22

on22 %>% 
  as_factor() %>% 
  select(da_good, postal_code_good, Housing_Status, Experimental_Group, Q23) %>% 
  summarise(across(everything(), ~sum(is.na(.))))
on22 %>% 
  select(DA2021, postal_code, Housing_Status, Q23) %>% 
  summary()
on22 %>% 
  mutate(sum_NA=rowSums(is.na(.)))->on22
summary(on22$sum_NA)

on22 %>% 
  filter(sum_NA>100) %>% view()
on22 %>% 
  ggplot(., aes(x=Progress))+geom_histogram()
mean(on22$Progress, na.rm=T)
