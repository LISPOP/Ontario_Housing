# how many respondents do we have??

source("R_Scripts/1_data_import.R")

nrow(on22)
install.packages("pwr")
library(pwr)
?pwr.f2.test

#Power Test Number of coefficients
# Controls
# age, gender, education, income
# variables of interest
# ideology (or vote choice) 1 continuous
# Experimental treatment 4 groups, so 3 dummies
# Building type rated single-detached, row, low-rise, tower 4 so 3 dummies
# Built environments percent single detached, percent row/semi-detached, percent low-rise
# percent high rise = 4 continuous

# Main effects 
# 1 continuous + 3 experimental + 3 building type + 4 continuous built environment = 7
# two-way interactions
# ideology + 3 experimental dummies = 4
# ideology + 3 building type = 4
# ideology + 4 continuous built environment = 4
# experimental dummies x building type = 3*3 = 9
# experimental dummies x continuous built environment = 3*4 = 12
# Three-way Interactions
# ideology + 3 experimental dummies # 3 building type + 4 continuous built environment = 8

# Total k =  48
library(pwr)
pwr.f2.test(u=52, v=nrow(on22)-48, f2="small")

names(on22)
