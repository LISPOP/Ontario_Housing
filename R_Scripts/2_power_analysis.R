# how many respondents do we have??

#source("R_Scripts/1_data_import.R")

nrow(on22)

library(pwr)


#Power Test Number of coefficients
# Controls
# age, gender, education, income 4
# variables of interest
# partisanship 4 categories (PC, Liberal, NDP, Other) 3
# housing status 1 
# Experimental treatment 4 groups, so 3 dummies 3
# Plus interation = 4-1 * 4-1 = 9
# Towers 



# Total k =  20
library(pwr)
?pwr.f2.test
nrow(on22)
pwr.f2.test(u=20, v=nrow(on22)-20, f2="small")

names(on22)
