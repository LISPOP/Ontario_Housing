#### This script is for descriptive statistics and tables
#Table of Descriptives for Housing Status
library(janitor)
#Housng Status
tabyl(on22$Housing_Status, sort= TRUE) 
names(on22)
on22 %>% 
  as_factor() %>% 
  tabyl(Housing_Status, Q6b)

#Cross tab 2018 vote by 2021 vote 

#Cross tab vote switching variable by housing status

# Other stuff! Feel free to freelance. 
