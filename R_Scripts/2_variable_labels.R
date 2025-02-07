library(here)
#source(here("R_Scripts", "1_data_import.R"))
# Variable labels 

on22 %>% 
  select(starts_with("Q31")) %>% 
  var_label()
#Please look in the data dictionary and provide a meaningful, systematic variable label for each one


#Q12
on22 %>% 
  select(starts_with("Q12")) %>% 
  var_label()
var_label(on22$Q12_1)<-c("Won't vote for - Liberal")
var_label(on22$Q12_2)<-c("Won't vote for - PC")
var_label(on22$Q12_3)<-c("Won't vote for - NDP")
var_label(on22$Q12_4)<-c("Won't vote for - Green")
var_label(on22$Q12_5)<-c("Won't vote for - Another party")
var_label(on22$Q12_6)<-c("Won't vote for - Don't know")
var_label(on22$Q12_7)<-c("Won't vote for - Could vote for any")

#Q13
on22 %>% 
  select(starts_with("Q13")) %>% 
  var_label()

var_label(on22$Q13_1)<-c("Feelings about party - Liberal")
var_label(on22$Q13_2)<-c("Feelings about party - PC")
var_label(on22$Q13_3)<-c("Feelings about party - NDP")
var_label(on22$Q13_4)<-c("Feelings about party - Green")

#Q14
on22 %>% 
  select(starts_with("Q14")) %>% 
  var_label()

var_label(on22$Q14_1)<-c("Feelings about leader - Doug Ford")
var_label(on22$Q14_2)<-c("Feelings about leader - Steven Del Duca")
var_label(on22$Q14_3)<-c("Feelings about leader - Andrea Horwath")
var_label(on22$Q14_4)<-c("Feelings about leader - Mike Schreiner")

#Q15
on22 %>% 
  select(starts_with("Q15")) %>% 
  var_label()

var_label(on22$Q15_1)<-c("Best party for issue - Healthcare")
var_label(on22$Q15_2)<-c("Best party for issue - Education")
var_label(on22$Q15_3)<-c("Best party for issue - Environment")
var_label(on22$Q15_4)<-c("Best party for issue - Crime and justice")
var_label(on22$Q15_5)<-c("Best party for issue - Immigration and minorities")
var_label(on22$Q15_6)<-c("Best party for issue - Economy")
var_label(on22$Q15_7)<-c("Best party for issue - Affordable housing")

#Q25
on22 %>% 
  select(starts_with("Q25")) %>% 
  var_label

var_label(on22$Q25)<-c("Trust of others")

#Q27
on22 %>% 
  select(starts_with("Q27")) %>% 
  var_label

var_label(on22$Q27)<-c("Current living situation")

#Q31
on22 %>% 
  select(starts_with("Q31")) %>% 
  var_label()

var_label(on22$Q31_1)<-c("Affordability of $800 per month")
var_label(on22$Q31_2)<-c("Affordability of $1200 per month")
var_label(on22$Q31_3)<-c("Affordability of $1600 per month")
var_label(on22$Q31_4)<-c("Affordability of $2000 per month")
var_label(on22$Q31_5)<-c("Affordability of $2400 per month")
var_label(on22$Q31_6)<-c("Affordability of $2800 per month")

#Q32
on22 %>% 
  select(starts_with("Q32")) %>% 
  var_label()

var_label(on22$Q32_1)<-c("Causes - Investor speculation")
var_label(on22$Q32_2)<-c("Causes - Low interest rates")
var_label(on22$Q32_3)<-c("Causes - Environmental protections")
var_label(on22$Q32_4)<-c("Causes - Municipal red tape")
var_label(on22$Q32_5)<-c("Causes - NIMBYs")
var_label(on22$Q32_6)<-c("Causes - Urban sprawl")
var_label(on22$Q32_7)<-c("Causes - Low public housing investment")
var_label(on22$Q32_8)<-c("Causes - Low rent control ")
var_label(on22$Q32_9)<-c("Causes - Too many immigrants")

var_label(on22$Q32_1_x)<-c("Causes - Investor speculation")
var_label(on22$Q32_2_x)<-c("Causes - Low interest rates")
var_label(on22$Q32_3_x)<-c("Causes - Environmental protections")
var_label(on22$Q32_4_x)<-c("Causes - Municipal red tape")
var_label(on22$Q32_5_x)<-c("Causes - NIMBYs")
var_label(on22$Q32_6_x)<-c("Causes - Urban sprawl")
var_label(on22$Q32_7_x)<-c("Causes - Low public housing investment")
var_label(on22$Q32_8_x)<-c("Causes - Low rent control ")
var_label(on22$Q32_9_x)<-c("Causes - Too many immigrants")
#Q33a
on22 %>% 
  select(starts_with("Q33a")) %>% 
  var_label()

var_label(on22$Q33a_1)<-c("Support for policy - More affordable public housing")
var_label(on22$Q33a_2)<-c("Support for policy - Taxes for owning multiple houses")
var_label(on22$Q33a_3)<-c("Support for policy - Increasing taxes for foreign home-buyers")
var_label(on22$Q33a_4)<-c("Support for policy - More non-single housing properties")
var_label(on22$Q33a_5)<-c("Support for policy - Require developers to build more affordable housing")
var_label(on22$Q33a_6)<-c("Support for policy - Add more properties to existing units")

var_label(on22$Q33a_1_x)<-c("Support for policy - More affordable public housing")
var_label(on22$Q33a_2_x)<-c("Support for policy - Taxes for owning multiple houses")
var_label(on22$Q33a_3_x)<-c("Support for policy - Increasing taxes for foreign home-buyers")
var_label(on22$Q33a_4_x)<-c("Support for policy - More non-single housing properties")
var_label(on22$Q33a_5_x)<-c("Support for policy - Require developers to build more affordable housing")
var_label(on22$Q33a_6_x)<-c("Support for policy - Add more properties to existing units")
# 
var_label(on22$Q33a_1_y)<-c("Support for policy - More affordable public housing")
var_label(on22$Q33a_2_y)<-c("Support for policy - Taxes for owning multiple houses")
var_label(on22$Q33a_3_y)<-c("Support for policy - Increasing taxes for foreign home-buyers")
var_label(on22$Q33a_4_y)<-c("Support for policy - More non-single housing properties")
var_label(on22$Q33a_5_y)<-c("Support for policy - Require developers to build more affordable housing")
var_label(on22$Q33a_6_y)<-c("Support for policy - Add more properties to existing units")
#Q80
on22 %>% 
  select(starts_with("Q80")) %>% 
  var_label()

var_label(on22$Q80_1)<-c("Support for policy - Reduce heritage designation laws")
var_label(on22$Q80_2)<-c("Support for policy - Eliminate density and height restrictions")
var_label(on22$Q80_3)<-c("Support for policy - Increase housing supply")
var_label(on22$Q80_4)<-c("Support for policy - Government loans for new buyers")
var_label(on22$Q80_5)<-c("Support for policy - Eliminate housing transfer taxes")
var_label(on22$Q80_6)<-c("Support for policy - More rent control")

var_label(on22$Q80_1_x)<-c("Support for policy - Reduce heritage designation laws")
var_label(on22$Q80_2_x)<-c("Support for policy - Eliminate density and height restrictions")
var_label(on22$Q80_3_x)<-c("Support for policy - Increase housing supply")
var_label(on22$Q80_4_x)<-c("Support for policy - Government loans for new buyers")
var_label(on22$Q80_5_x)<-c("Support for policy - Eliminate housing transfer taxes")
var_label(on22$Q80_6_x)<-c("Support for policy - More rent control")
var_label(on22$Q80_1_y)<-c("Support for policy - Reduce heritage designation laws")
var_label(on22$Q80_2_y)<-c("Support for policy - Eliminate density and height restrictions")
var_label(on22$Q80_3_y)<-c("Support for policy - Increase housing supply")
var_label(on22$Q80_4_y)<-c("Support for policy - Government loans for new buyers")
var_label(on22$Q80_5_y)<-c("Support for policy - Eliminate housing transfer taxes")
var_label(on22$Q80_6_y)<-c("Support for policy - More rent control")
#Spending Variable Labels
var_label(on22$Q16)<-c("Spending - Education")
var_label(on22$Q17)<-c("Spending - Environment")
var_label(on22$Q18)<-c("Spending - Fighting Crime")
var_label(on22$Q19)<-c("Spending - Health Care")
var_label(on22$Q20)<-c("Spending - Social Programs")
var_label(on22$Q21)<-c("Spending - Affordable Housing")

var_label(on22$Q16_x)<-c("Spending - Education 0 to 1")
var_label(on22$Q17_x)<-c("Spending - Environment 0 to 1")
var_label(on22$Q18_x)<-c("Spending - Fighting Crime 0 to 1")
var_label(on22$Q19_x)<-c("Spending - Health Care 0 to 1")
var_label(on22$Q20_x)<-c("Spending - Social Programs 0 to 1")
var_label(on22$Q21_x)<-c("Spending - Affordable Housing 0 to 1")
#Q34
on22 %>% 
  select(starts_with("Q34")) %>% 
  var_label()
on22$Q34_1
var_label(on22$Q34_1)<-c("Lower Taxes - Affordable public housing")
var_label(on22$Q34_2)<-c(c("Increased Health and Education - Affordable public housing"))
var_label(on22$Q34_3)<-c("Stable debt and deficit - Affordable public housing")
var_label(on22$Q34_4)<-c("Local control over zoning - Provincial control over zoning")
var_label(on22$Q34_5)<-c("Maintaining green space - Reduce environmental regulations")
var_label(on22$Q34_1_x)<-c("Lower Taxes - Affordable public housing")
var_label(on22$Q34_2_x)<-c(c("Increased Health and Education - Affordable public housing"))
var_label(on22$Q34_3_x)<-c("Stable debt and deficit - Affordable public housing")
var_label(on22$Q34_4_x)<-c("Local control over zoning - Provincial control over zoning")
var_label(on22$Q34_5_x)<-c("Maintaining green space - Reduce environmental regulations")
#Partisaship
var_label(on22$partisanship)<-c("R partisanship, Ontario, includes only very and fairly strongly partisans")

#Set variable names

on22 %>% set_variable_labels(non_partisan="R is Non Partisan", 
                             Renter="R is a renter",
                             Satisfied_Renter="R is a satisfied Renter",
                             PC="R voted Progressive Conservative in 2022",
                             Degree="R has at least a bachelor",
                             #Speculator="R is a landlord, speculator",
                             Density="Rs built environment, Suburban as reference level",
                             #`First_Time_Buyer`="R is a first time homebuyer",
                             Homeowner="R is a satisfied homeowner, not looking to purchase",
                             Housing_Status="Rs Housing Status",
                             #geo_good="Good Postal Code to FED and CSD match",
                             Q31_1_x="$800/month",
                             Q31_2_x="$1200/month",
                             Q31_3_x="$1600/month",
                             Q31_4_x="$2000/month",
                             Q31_5_x="$2400/month",
                             Q31_6_x="$2800/month",
                             Vote="Party voted for", 
                             YIMBY="Support building 1.5 million homes and support abolishing single family dwelling zoning", 
                             NIMBY="Support building 1.5 million homes and oppose abolishing single family dwelling zoning")->on22
names(on22)
