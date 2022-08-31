source("R_Scripts/2_recodes.R")

#Regressions for Causes
# Step 1 select the variables you need.
names(on22)
lookfor(on22, "cause")
#Causes are stored in Q32_1_x, converted 0 to 1
#First run a check
on22 %>% 
  select(starts_with("Q32")& ends_with("_x"))
#Now add in demographics
names(on22)
on22 %>% 
  select(starts_with("Q32")& ends_with("_x"), gender, Housing_Status, partisanship)
  
