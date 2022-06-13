source("R_Scripts/1_data_import.R")
#library(car)
#### Insider Outsider Variable
# Combine those Q27 and Q30 into one variable: 
# 1. Landlords (Q27) who are staying put (Q30)
# Homeowners (Q27) who are staying put (Q30)
# Non homeowners (Q27) who want to buy (Q30)
# Non- homeowners (Q27)  who want to rent (Q30)
# All others

#Use mutate and case_when()
#Landlords who are saying Put
 on22 %>% 
   mutate(Status=case_when(
     Q28==1 & Q30==2 ~ "Speculator",
     #Put all the separate conditions in the same mutate - case_when command, separated by a comma. 
     Q27==1 & Q30==2 ~ "Satisfied Homeowner",
     Q27==2 & Q30==1 ~ "First-Time Homebuyer",
     Q27==2 & Q30==2 ~ "Satisfied renter",
     TRUE ~ "Other"
     #To actually save the results one needs to reassign the results of the foregoing back into on22
   ))->on22
 
 on22 %>% 
   select(starts_with("")) %>% 
   summary()

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

# Age Calculation
on22 %>% 
  select(starts_with("DOB")) %>% 
  summary()

#Recode date of birth from qualtrics data
on22 %>% 
  mutate(
    DOB=case_when(
      Q37=="4"~1920,
      Q37=="5"~1921,
      Q37=="7"~1922,
      Q37=="8"~1923,
      Q37=="9"~1924,
      Q37=="10"~1925,
      Q37=="11"~1926,
      Q37=="12"~1927,
      Q37=="13"~1928,
      Q37=="14"~1929,
      Q37=="15"~1930,
      Q37=="16"~1931,
      Q37=="17"~1932,
      Q37=="18"~1933,
      Q37=="19"~1934,
      Q37=="20"~1935,
      Q37=="21"~1936,
      Q37=="22"~1937,
      Q37=="23"~1938,
      Q37=="24"~1939,
      Q37=="25"~1940,
      Q37=="26"~1941,
      Q37=="27"~1942,
      Q37=="28"~1943,
      Q37=="29"~1944,
      Q37=="30"~1945,
      Q37=="31"~1946,
      Q37=="32"~1947,
      Q37=="33"~1948,
      Q37=="34"~1949,
      Q37=="35"~1950,
      Q37=="36"~1951,
      Q37=="37"~1952,
      Q37=="38"~1953,
      Q37=="39"~1954,
      Q37=="40"~1955,
      Q37=="41"~1956,
      Q37=="42"~1957,
      Q37=="43"~1958,
      Q37=="44"~1959,
      Q37=="45"~1960,
      Q37=="46"~1961,
      Q37=="47"~1962,
      Q37=="48"~1963,
      Q37=="49"~1964,
      Q37=="50"~1965,
      Q37=="51"~1966,
      Q37=="52"~1967,
      Q37=="53"~1968,
      Q37=="54"~1969,
      Q37=="55"~1970,
      Q37=="56"~1971,
      Q37=="57"~1972,
      Q37=="58"~1973,
      Q37=="59"~1974,
      Q37=="60"~1975,
      Q37=="61"~1976,
      Q37=="62"~1977,
      Q37=="63"~1978,
      Q37=="64"~1979,
      Q37=="65"~1980,
      Q37=="66"~1981,
      Q37=="67"~1982,
      Q37=="68"~1983,
      Q37=="69"~1984,
      Q37=="70"~1985,
      Q37=="71"~1986,
      Q37=="72"~1987,
      Q37=="73"~1988,
      Q37=="74"~1989,
      Q37=="75"~1990,
      Q37=="76"~1991,
      Q37=="77"~1992,
      Q37=="78"~1993,
      Q37=="79"~1994,
      Q37=="80"~1995,
      Q37=="81"~1996,
      Q37=="82"~1997,
      Q37=="83"~1998,
      Q37=="84"~1999,
      Q37=="85"~2000,
      Q37=="86"~2001,
      Q37=="87"~2002,
      Q37=="88"~2003,
      Q37=="89"~2004,
      Q37=="90"~2005,
      Q37=="91"~2006,
      Q37=="92"~2007,
      Q37=="93"~2008
      
      )
  )->on22

#Calculating age in new variable: age
  on22$age<-(2022-on22$DOB)

#check age
on22 %>% 
  select(starts_with("age")) %>% 
  summary()

#Swing Voter Variable
on22 %>% 
  mutate(Status=case_when(
    (Q6b==1 | Q6b==3 |Q6b==4) & (Q8==2 | Q9==2 | Q10==2 | Q11==2) ~ "Swing Voter",
    TRUE ~ "Other"
  ))->on22
    
#Apathetic Voter Variable
    on22 %>% 
      mutate(Status=case_when(   
    (Q6a==1) & (Q7==3|Q7==4) ~ "Apathetic Voter",
    TRUE ~ "Other"
  ))->on22

#### Odd Voting Combinations
on22 %>% 
  mutate(voting_flag=case_when(
    #If vote intention is Liberal or reported vote is Liberal and variable won't vote against Liberal is also 1, set to 1
    (Q8==1| Q10==1) & Q12_1==1 ~ 1, 
    (Q8==2| Q10==2) & Q12_1==2 ~ 1, #PC
    (Q8==3| Q10==3) & Q12_1==3 ~ 1, #NDP
    (Q8==4| Q10==4) & Q12_1==4 ~ 1, #GRN
    TRUE ~ 0
   ))->on22


on22 %>% 
  mutate(
    across(starts_with("voting_flag"), ~
             {
               val_labels(.x)<-c("Non-contradicting voters"=0, "Contradicting Voters"=1)
               .x
             }  )
  )->on22

lookfor(on22, "duration")
lookfor(on22, "voting_flag")


# age flag

on22 %>% 
  mutate(below18=case_when(
    age<18~1,
    TRUE~0
  ))->on22
on22 %>% 
  mutate(above95=case_when(
    age>94~1,
    TRUE~0
  ))->on22
