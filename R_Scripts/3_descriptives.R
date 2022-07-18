#### This script is for descriptive statistics and tables
#Table of Descriptives for Housing Status
library(tabyl)
kbl(tabyl(on22$Housing_Status, sort= TRUE)) %>% 
  kable_styling()
