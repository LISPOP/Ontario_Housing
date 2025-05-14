# how many respondents do we have??

#source("R_Scripts/1_data_import.R")

nrow(on22)

library(pwr)


#Power Test Number of coefficients
# Controls
# age, gender, education, income 4
# variables of interest
# partisanship 4 categories (PC, Liberal, NDP, Other)
# housing status 1 
# Experimental treatment 4 groups, so 3 dummies
# Plus interation = 4-1 * 4-1 = 9




# Total k =  48
library(pwr)
pwr.f2.test(u=52, v=nrow(on22)-48, f2="small")

names(on22)
