source("R_Scripts/1_data_import.R")
library(car)
#### Q31
# Variable labels 

on22 %>% 
  select(starts_with("Q31")) %>% 
  var_label()
#Please look in the data dictionary and provide a meaningful, systematic variable label for each one

var_label(on22$Q31_1)<-c("Affordability of $800 per month")
# On many of the batteries of questions, there is a mismatch between the value and the value label.
# 
table(on22$Q31_1)
summary(on22$Q31_1)
val_labels(on22$Q31_1) #A 1 in the data is actually is labelled as a 0
# It would be desirable to change the data because the survey went from 0 to 10
# The solution. 
# We store the standard 0 to 11 value labels in an object. 

val_labels(on22$Q31_1)<-c("1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "8"=8, "9"=9, "10"=10, "11"=11)

# We can do this for multiple variables at once. 
#Start with the data frame
on22 %>% 
  #Because we are transforming variables we use mutate
  mutate(
    #The command across says do the thing that follows across these variables
    #You can specify which variables using a bunch of different convenient select helpers
    # see ?select
    #In this case, we will be working with all variables that start with Q31_
    across(starts_with("Q31_"), ~
             {
      #Here is where we specify which labels go with which value
      val_labels(.x)<-c("1 Unable to Afford"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "8"=8, "9"=9, "10"=10, "11 - Able to Afford"=11)
    #It is necessary to return the .x
      .x
      }))->on22

#Now test:
on22 %>% 
  select(starts_with("Q31_")) %>% 
  val_labels

#Then for the same battery, I would like to create a parallel set of variables that takes scales
# everything from 0 to 1 and sets the don't  know to be the middle value. 
# for this it is really important to look at the SPSS file to see what the don't knows are
# If there are any!

on22 %>% 
  select(starts_with("Q31_")) %>% 
  summary()
#These variables go to 11, which is*not* a don't know, so it is a poor example. 
#But if we pretended that there was a 12 which was a don't know, we would proceed as follows
#Note, to find the midpoint, it's often helpful to just literally count on your fingers
on22$Q31_1

#load the scales library
library(scales)
#This code does everything
#Data frame
on22 %>% 
  #we are transforming variables so we mutate
  mutate(
    #We are working across several variables, so we use across
    across(
      #We can use these select helpers to pick what we are working with 
      # see ?select
      #We open the function with ~{
      starts_with("Q31_"), ~{
        #This does two things:
        #The Recode() takes the don't know value and sets it to the midpoint
        #And the rescale(as.numeric() bit just transforms everything to a range of 0 to 1
       rescale(as.numeric(Recode(.x, "12=6")))
        #The }closes the function and the .names argument makes the new variables to have the mames
        # of the original variable with _x and saves it back in on22
      }, .names="{.col}_x")) ->on22
#Check 
names(on22)
on22 %>% 
  select(starts_with("Q31")) %>% 
  summary()
# this has to be repeated, for each battery in the data-set. 

#### Q32 ####
#### Q33a and Q80 ####

#### Experiment####
#This folds down the four variables that distinguish the treatment group.
lookfor(on22, "experiment")
on22 %>%
rename("Social"=`v7`, "Private"=`v8`, "Public"=`v9`, "Control"=starts_with('SCREEN10'))->on22
#Count missing values in experimental group

on22 %>% 
  rowwise() %>% 
  mutate(experimental_missings=sum(is.na(c_across(Social:Control)))) %>% 
  ungroup()->on22
names(on22)
# on22 %>% 
#   filter(experimental_missings==4) %>% 
#   select(Consent2, experimental_missings, `_v7`:`SCREEN10_Experiment1_DO_Control`, ResponseId, RecordedDate)  %>% 
#   write_csv(., file="Data/missing_experimental.csv")
#   
on22 %>% 
  pivot_longer(Social:Control, names_to="Experimental_Group", values_to=c("Value")) ->on22
on22 %>% 
  filter(Value==1) %>% 
  select(Experimental_Group:Value)
#Drop the Value variable; unnecessary
on22 %>% 
  filter(Value==1) %>% 
  select(-Value) ->on22

# Currently the value labels for the experimental question run from 1 to 11; 12 is don't know
# I'm going to set 12 to be in the  middle which is 5
  on22 %>% 
    mutate(across(starts_with('Q35_'),~(car::Recode(.x, "12=6")), .names="{.col}_exp")) %>%
    mutate(across(ends_with('_exp'), ~scales::rescale(as.numeric(.x)))) %>% 
select(ends_with('_exp')) 
  #Start with the data frame
  on22 %>% 
    #We are changing existing variables, so this is a mutate
    mutate(
      #Across does a function across a series of columns
      across(
        #We use the select() helpers ?select to select which columns we are manipulating
        #In this case, it is the responses to teh experimental question, that 
        #are stored as variables Q35_
        #Then we use the Recode function in the car package to recode the number 12
        # to be the number 6
        #The .names="{.col}_exp") allows us to store the new variables with the 
        # column name of the original variable joined with _exp
        # This is important because it will allow us to keep the original variable
        # But pick out the recoded and transformed variable
        starts_with('Q35_'),~(car::Recode(.x, "12=6")), .names="{.col}_exp")) %>%
    #Then we are going to do another transformation.So we need another mutate()
    mutate(
      #And we need another across, and this time we are selecting the variables that 
      # We just made above that end with _exp
      # And we are using the rescale function in the scales package to rescale the variables
      # from 1 to 11 to 0 to 1. 
      across(ends_with('_exp'), ~scales::rescale(as.numeric(.x))))->on22  

  #Now we need to rename the variables to be meaningful.
  #The only way to get this is by looking in the questionnaire. 
on22 %>% set_variable_labels(Q35_1_exp="6 Storey rental building", 
                    Q35_2_exp="15 Storey rental tower", 
                    Q35_3_exp="6 Storey condominium building",
                    Q35_4_exp="15 Storey condominium Tower",
                    Q35_5_exp="Single detached house",
                    Q35_6_exp="Semi-detached house")->on22
#### Causes ####
# repeat the above process 
# Currently the value labels for the cause question run from 1 to 11; 12 is don't know
# I'm going to set 12 to be in the  middle which is 5
on22 %>% 
  mutate(across(starts_with('Q32_'),~(car::Recode(.x, "12=6")), .names="{.col}_cause")) %>%
  mutate(across(ends_with('_cause'), ~scales::rescale(as.numeric(.x)))) ->on22
  #select(ends_with('_cause')) ->on22
# ## This needs to be filled in for each new variable created
# on22 %>% set_variable_labels(Q32_1_cause="Speculation by Investors", 
#                              Q32_2_cause="Low Interest Rates", 
#                              Q32_3_cause="Environmental rules limiting construction",
#                              )->on22
# Adjust # of surveys taken

on22 %>% 
  mutate(Q48_x=car::Recode(Q48, "1=0; 2=1; 3=2; 4=3; 5=4; 6=5"))->on22
val_labels(on22$Q48_x)
val_labels(on22$Q48)
table(on22$Q48_x, on22$Q48)
on22$Q48_x
val_labels(on22$Q48_x)<-c(`5+`=5)
on22$Q48_x

# Non-Partisan
on22$non_partisan<-Recode(on22$Q23, "6=1; else=0")
val_labels(on22$non_partisan)<-c("Non-Partisan"=1, "Partisan"=0)

lookfor(on22, "vote")
on22 %>% 
  select(Q7, Q8)
#### Change Value Labels #### 
# Set Missings for Don't knows.
# The problem
on22$Q3
  