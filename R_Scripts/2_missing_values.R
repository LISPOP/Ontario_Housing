#### Missing Values 
source("R_Scripts/2_recodes.R")
# Make a variable postal_code_good that returns 1 if nchar(postal_code)==6; else return missing N
# make a variable da_good that returns 1 if PRCDA is not missing; else returns missing

# Select postal_code_good, da_good and a few other key variables: party _ID

on22 %>% 
  select(PRCDDA) %>% 
  summary()
# #Keep only those good cases
on22 %>%
  filter(geo_good==1)->on22
#filter out non-consents
on22 %>% 
  filter(Consent2<2)->on22
