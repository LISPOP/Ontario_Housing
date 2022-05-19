source("R_Scripts/1_data_import.R")
#### This section contains transformations (e.g. recodes) and reshaping

#### Experiment####
#This folds down the four variables that distinguish the treatment group.
lookfor(on22, "experiment")
on22 %>%
rename("Social"=`_v7`, "Private"=`_v8`, "Public"=`_v9`, "Control"=starts_with('SCREEN10'))->on22
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

#### Spit out Recoded Variables ####
  