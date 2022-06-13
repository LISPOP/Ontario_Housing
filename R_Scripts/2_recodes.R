source("R_Scripts/1_data_import.R")
source("R_Scripts/2_value_labels.R")
source("R_Scripts/2_variable_labels.R")
library(car)
#### Insider Outsider Variable
# Combine those Q27 and Q30 into one variable: 
# 1. Landlords (Q27) who are staying put (Q30)
# Homeowners (Q27) who are staying put (Q30)
# Non homeowners (Q27) who want to buy (Q30)
# Non- homeowners (Q27)  who want to rent (Q30)
# All others

#Use mutate and case_when()
# on22 %>% 
#   mutate(Status=case_when(
#     Q27=="Landlorc"&Q30=="Satying Put" ~ "Speculator",
#     TRUE ~ "Other"
#   ))

#### Experiment####
# #This folds down the four variables that distinguish the treatment group.
# lookfor(on22, "experiment")
# lookfor(on22, "taxes")
# var_label(on22) %>% View()
# on22 %>%
# rename("Social"=`v7`, "Private"=`v8`, "Public"=`v9`, "Control"=starts_with('SCREEN10'))->on22
# #Count missing values in experimental group
# 
# on22 %>% 
#   rowwise() %>% 
#   mutate(experimental_missings=sum(is.na(c_across(Social:Control)))) %>% 
#   ungroup()->on22
# names(on22)
# on22 %>% 
#   filter(experimental_missings==4) %>% 
#   select(Consent2, experimental_missings, `_v7`:`SCREEN10_Experiment1_DO_Control`, ResponseId, RecordedDate)  %>% 
#   write_csv(., file="Data/missing_experimental.csv")
#   
# on22 %>% 
#   pivot_longer(Social:Control, names_to="Experimental_Group", values_to=c("Value")) ->on22
# on22 %>% 
#   filter(Value==1) %>% 
#   select(Experimental_Group:Value)
# #Drop the Value variable; unnecessary
# on22 %>% 
#   filter(Value==1) %>% 
#   select(-Value) ->on22

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

# Age Calculation
on22 %>% 
  select(starts_with("DOB")) %>% 
  summary()

#Recode date of birth from qualtrics data
on22$Q37_DO_NOT_USE
on22 %>% 
  mutate(
    DOB=case_when(
      Q37_DO_NOT_USE==4~1920,
      Q37_DO_NOT_USE==5~1921,
      Q37_DO_NOT_USE==7~1922,
      Q37_DO_NOT_USE==8~1923,
      Q37_DO_NOT_USE==9~1924,
      Q37_DO_NOT_USE==10~1925,
      Q37_DO_NOT_USE==11~1926,
      Q37_DO_NOT_USE==12~1927,
      Q37_DO_NOT_USE==13~1928,
      Q37_DO_NOT_USE==14~1929,
      Q37_DO_NOT_USE==15~1930,
      Q37_DO_NOT_USE==16~1931,
      Q37_DO_NOT_USE==17~1932,
      Q37_DO_NOT_USE==18~1933,
      Q37_DO_NOT_USE==19~1934,
      Q37_DO_NOT_USE==20~1935,
      Q37_DO_NOT_USE==21~1936,
      Q37_DO_NOT_USE==22~1937,
      Q37_DO_NOT_USE==23~1938,
      Q37_DO_NOT_USE==24~1939,
      Q37_DO_NOT_USE==25~1940,
      Q37_DO_NOT_USE==26~1941,
      Q37_DO_NOT_USE==27~1942,
      Q37_DO_NOT_USE==28~1943,
      Q37_DO_NOT_USE==29~1944,
      Q37_DO_NOT_USE==30~1945,
      Q37_DO_NOT_USE==31~1946,
      Q37_DO_NOT_USE==32~1947,
      Q37_DO_NOT_USE==33~1948,
      Q37_DO_NOT_USE==34~1949,
      Q37_DO_NOT_USE==35~1950,
      Q37_DO_NOT_USE==36~1951,
      Q37_DO_NOT_USE==37~1952,
      Q37_DO_NOT_USE==38~1953,
      Q37_DO_NOT_USE==39~1954,
      Q37_DO_NOT_USE==40~1955,
      Q37_DO_NOT_USE==41~1956,
      Q37_DO_NOT_USE==42~1957,
      Q37_DO_NOT_USE==43~1958,
      Q37_DO_NOT_USE==44~1959,
      Q37_DO_NOT_USE==45~1960,
      Q37_DO_NOT_USE==46~1961,
      Q37_DO_NOT_USE==47~1962,
      Q37_DO_NOT_USE==48~1963,
      Q37_DO_NOT_USE==49~1964,
      Q37_DO_NOT_USE==50~1965,
      Q37_DO_NOT_USE==51~1966,
      Q37_DO_NOT_USE==52~1967,
      Q37_DO_NOT_USE==53~1968,
      Q37_DO_NOT_USE==54~1969,
      Q37_DO_NOT_USE==55~1970,
      Q37_DO_NOT_USE==56~1971,
      Q37_DO_NOT_USE==57~1972,
      Q37_DO_NOT_USE==58~1973,
      Q37_DO_NOT_USE==59~1974,
      Q37_DO_NOT_USE==60~1975,
      Q37_DO_NOT_USE==61~1976,
      Q37_DO_NOT_USE==62~1977,
      Q37_DO_NOT_USE==63~1978,
      Q37_DO_NOT_USE==64~1979,
      Q37_DO_NOT_USE==65~1980,
      Q37_DO_NOT_USE==66~1981,
      Q37_DO_NOT_USE==67~1982,
      Q37_DO_NOT_USE==68~1983,
      Q37_DO_NOT_USE==69~1984,
      Q37_DO_NOT_USE==70~1985,
      Q37_DO_NOT_USE==71~1986,
      Q37_DO_NOT_USE==72~1987,
      Q37_DO_NOT_USE==73~1988,
      Q37_DO_NOT_USE==74~1989,
      Q37_DO_NOT_USE==75~1990,
      Q37_DO_NOT_USE==76~1991,
      Q37_DO_NOT_USE==77~1992,
      Q37_DO_NOT_USE==78~1993,
      Q37_DO_NOT_USE==79~1994,
      Q37_DO_NOT_USE==80~1995,
      Q37_DO_NOT_USE==81~1996,
      Q37_DO_NOT_USE==82~1997,
      Q37_DO_NOT_USE==83~1998,
      Q37_DO_NOT_USE==84~1999,
      Q37_DO_NOT_USE==85~2000,
      Q37_DO_NOT_USE==86~2001,
      Q37_DO_NOT_USE==87~2002,
      Q37_DO_NOT_USE==88~2003,
      Q37_DO_NOT_USE==89~2004,
      Q37_DO_NOT_USE==90~2005,
      Q37_DO_NOT_USE==91~2006,
      Q37_DO_NOT_USE==92~2007,
      Q37_DO_NOT_USE==93~2008
      )
  )->on22



#Calculating age in new variable: age
  on22$age<-(2022-on22$DOB)

#check age
on22 %>% 
  select(starts_with("age")) %>% 
  summary()

#### Odd Voting Combinations
on22 %>% 
  mutate(voting_flag=case_when(
    #If vote intention is Liberal or reported vote is Liberal and variable won't vote against Liberal is also 1, set to 1
    (Q8==1| Q10==1) & Q12_1==1 ~ 1, 
#Repeat for Conservatives, NDP and Greens
  ))

  
