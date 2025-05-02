#### Missing Values 
source("R_Scripts/2_recodes.R")
# 1. Make a variable postal_code_good that returns 1 if nchar(postal_code)==6; else return missing N
# 2. make a variable da_good that returns 1 if PRCDA is not missing; else returns missing
# 3. Select those two variables above as well as the variables we are likely to use
# e.g. renter status, provincial party id, experimental group, anything else?
# Select postal_code_good, da_good and a few other key variables: party _ID

on22%>%
  mutate(postal_code_good = ifelse(nchar(postal_code)==6, "1", NA))->on22

on22%>%
  mutate(da_good = ifelse(nchar(PRCDDA)==8, "1", NA))->on22

on22 %>% 
  as_factor() %>% 
  select(da_good, postal_code_good, Housing_Status, Experimental_Group, Q23) %>% 
  summarise(across(everything(), ~sum(is.na(.))))

# # #Keep only those good cases
# on22 %>%
#   filter(geo_good==1)->on22
# #filter out non-consents
# on22 %>% 
#   filter(Consent2<2)->on22
