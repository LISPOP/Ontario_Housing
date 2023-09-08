source("R_Scripts/2_recodes.R")
library(broom)
# Regression
#This sets the reference category for the variable own_affordrable to be 
# pro-affordable housing renters
# The point is to try to mirror the analysis set in Nall and Plumb
on22$own_affordable<-relevel(on22$own_affordable, "Pro-Affordable Housing Non-Homeowner")
table(on22$Homeowner, on22$Housing_Status)

#Load libraries necessary for this
# Install if necessary
library(modelsummary)
library(flextable)
# This section is the cause Regressions
names(on22)
#Make the regresssion function
# This is where covariates can be added or subtracted
cause_ols1<-function(x) lm(value ~ male+
                             #PC_Vote22+
                             income_digits+
                             Degree+
                             Size+
                             Housing_Status+
                             Under_35+Over_55, data=x)

#Now run the regression on each of the cause variables
#Remind which are the cause variables
cause_var_labels
#Q32_1_x and so on. 
on22 %>% 
  pivot_longer(., Q32_1_x:Q32_9_x) %>% 
  nest(-name)  %>% 
mutate(m1=map(data, cause_ols1)) %>% 
  mutate(m1_tidied=map(m1, tidy))->cause_ols_list
#Name the models stored in cause_ols_list$m1
names(cause_ols_list$m1)<-cause_var_labels$label
modelsummary(cause_ols_list$m1, 
             #Print significance Stars
             stars=T, 
             #Omit the intercept
             coef_omit=c("Intercept|Other"),
             #Omit these goodness of fit statistics
             gof_omit=c("AIC|BIC|^Log|^F"), 
             #Rename coefficients to make it pretty
             #These must match the actual variable names
            coef_rename=c("maleMale"="Male",
                          "PCPC"="PC Voter",
                          "income_digits"="Income", 
                          "SizeSmall"="Size (Small)",
                          "SizeLarge"="Size (Large)",
                          "SizeToronto/Ottawa"="Size (Toronto/Ottawa)",
                          "Housing_StatusFirst-Time Homebuyer"="Housing Status (First Time Buyer)",
                          "Housing_StatusSatisfied Renter"="Housing Status (Renter Not Purchasing)"), 
            #Number of digits
            fmt=2, 
            #export the output as flextable
            output="flextable") ->cause_ols_table 
cause_ols_table
  #Save the flextable as a word document with this filename
  save_as_docx(cause_ols_table,path=here("Tables", "cause_ols_regressions.docx"))

names(on22)
  #Make the regresssion function
  # This is where covariates can be added or subtracted
  solution_ols1<-function(x) lm(value ~ male+
                               #PC_Vote22+
                               income_digits+
                                 Degree+
                               Size+
                               Housing_Status+
                                 Under_35+
                                 Over_55, data=x)
  solution_ols1
  #Now run the regression on each of the cause variables
  #Remind which are the cause variables
 solution_var_labels
 table(on22$Over_55)
 table(on22$Under_35)
  #Q32_1_x and so on. 
  on22 %>% 
    pivot_longer(., Q33a_1_x:Q80_6_x) %>% 
    nest(-name)  %>% 
    mutate(m1=map(data, solution_ols1)) %>% 
    mutate(m1_tidied=map(m1, tidy))->solution_ols_list
  solution_ols_list
  #Name the models stored in cause_ols_list$m1
  names(solution_ols_list$m1)<-solution_var_labels$label
  modelsummary(solution_ols_list$m1, 
               #Print significance Stars
               stars=T, 
               #Omit the intercept
               coef_omit=c("Intercept"),
               #Omit these goodness of fit statistics
               gof_omit=c("AIC|BIC|^Log|^F"), 
               #Rename coefficients to make it pretty
               #These must match the actual variable names
               coef_rename=c("maleMale"="Male",
                             "PCPC"="PC Voter",
                             "income_digits"="Income", 
                             "SizeSmall"="Size (Small)",
                             "SizeLarge"="Size (Large)",
                             "SizeToronto/Ottawa"="Size (Toronto/Ottawa)",
                             "Housing_StatusFirst-Time Homebuyer"="Housing Status (First Time Buyer)",
                             "Housing_StatusSatisfied Renter"="Housing Status (Renter Not Purchasing)"), 
               #Number of digits
               fmt=2, 
               #export the output as flextable
               output="flextable") %>% 
    fontsize(size=8)->solution_ols_table

  #Save the flextable as a word document with this filename
  save_as_docx(solution_ols_table,path=here("Tables", "solution_ols_regressions.docx"))
  
  
# What is going on with past PC voters
m1<-  lm(on22$Q32_9_x ~ PC_Vote22, data=on22)
m2<-lm(Q32_9_x~PC_Vote22+income_digits+Degree+male+Size, data=on22)

