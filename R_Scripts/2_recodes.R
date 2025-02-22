source("R_Scripts/1_data_import.R")
names(on22)

names(on22)
nrow(on22)

library(car)

#### Insider Outsider Variable
# Combine those Q27 and Q30 into one variable: 
# 1. Landlords (Q27) who are staying put (Q30)
# Homeowners (Q27) who are staying put (Q30)
# Non homeowners (Q27) who want to buy (Q30)
# Non- homeowners (Q27)  who want to rent (Q30)
# All others

#Recoding Q6b to simplify party names
on22$Vote<-Recode(as.numeric(on22$Q6b), "1='Liberal'; 2='PC' ; 3='NDP' ; 4='Green'")
on22 %>% 
  mutate(Vote=case_when(
    Q6b==1~"PC",
    Q6b==  2~"Liberal",
    Q6b==3~"NDP",
    Q6b==4~"Green"
  ))->on22

on22$Vote<-factor(on22$Vote, levels=c("PC", "Liberal", "NDP", "Green"))
names(on22)


# Making partisanship variable for self-identifying partisans

on22 %>%
  mutate(partisanship=case_when(
    Q23 == 1 & Q24 <3 ~ "Liberal",
    Q23 == 2 & Q24 <3  ~ "NDP",
    Q23 == 3 & Q24 <3 ~ "PC",
    Q23 == 4 & Q24 <3 ~ "Green",
      TRUE ~ "Independent"
  ))->on22
on22$partisanship<-factor(on22$partisanship, levels=c("PC", "NDP", "Liberal", "Green", "Independent"))
#Use mutate and case_when()
table(on22$Q28)
var_label(on22$Q28)
var_label(on22$Q30)

lookfor(on22, "rent")
on22$Q27
table(as_factor(on22$Q27), as_factor(on22$Q28))
on22$Q27
on22$Q30
table(as_factor(on22$Q27), as_factor(on22$Q30))

#Landlords who are staying Put
on22$Q28
table(as_factor(on22$Q28))
table(as_factor(on22$Q28), as_factor(on22$Q30))
on22 %>% 
  mutate(Housing_Status=case_when(
    #Put all the separate conditions in the same mutate - case_when command, separated by a comma. 
    Q27==1 ~ "Homeowner", #Those that own
    Q27==2 & Q30==1 ~ "Seeking to purchase", #Those that rent and want to buy 
    Q27==3 & Q30==1 ~ "Seeking to purchase", #Those who live with fam and want to buy
    Q27==2 & Q30==2 ~ "Not seeking to purchase", #Those who rent and want to stay
    Q27==3 & Q30==2 ~ "Not seeking to purchase", #Those who live with fam and want to stay
    Q27==2 & Q30==3 ~ "Not seeking to purchase", #Those who rent and want to move to another rental
    Q27==3 & Q30==3 ~ "Not seeking to purchase", #Those who live with fam and want to move to a rental
    TRUE ~ "Other"
    #To actually save the results one needs to reassign the results of the foregoing back into on22
  ))->on22
table(on22$Housing_Status)
 val_labels(on22$Q27)
table(on22$Housing_Status, as_factor(on22$Q27))
names(on22)
# on22 %>% 
#   select(Housing_Status, Q27, Q28, Q30) %>% 
#   as_factor() %>% 
#   filter(Housing_Status=="Other") %>% 
#   View()
#   
#Reordering Housing_Status variable  
on22$Housing_Status<-factor(on22$Housing_Status, 
                            levels=c("Homeowner", 
                                     "Seeking to purchase", 
                                     "Not seeking to purchase", 
                                     "Other"))

 #### Experiment####
 
 
#This folds down the four variables that distinguish the treatment group.


#Count missing values in experimental group
# Got this from here: 
# https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
on22 %>% 
  mutate(experimental_missings=rowSums(is.na(select(., National:Control)))) ->on22

#Old garbage way.
# on22 %>% 
#   rowwise() %>% 
#   mutate(experimental_missings=sum(is.na(c_across(National:Control)))) %>%
#   select(experimental_missings)
#   ungroup()->on22
# names(on22)

# on22 %>% 
#   filter(experimental_missings==4) %>% 
#   select(Consent2, experimental_missings, `_v7`:`SCREEN10_Experiment1_DO_Control`, ResponseId, RecordedDate)  %>% 
#   write_csv(., file="Data/missing_experimental.csv")
#   

# This command pivots the four treatment group columns down.
on22 %>%
  #Resondents are assigned 1 if they were in respective treatment group
  #That means respondents will now have NAs for those treatment groups of which they were not apart
  pivot_longer(National:Control, names_to="Experimental_Group", values_to=c("Value")) ->on22
#Check now that each respondent has only a 1 for their treatment group
on22 %>%
  filter(Value==1) %>%
  select(Experimental_Group:Value)
#Drop the Value variable; unnecessary
on22 %>%
  filter(Value==1) %>%
  select(-Value) ->on22
names(on22)
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
on22 %>% set_variable_labels(Q35_1_exp="rental_6_storey", 
                    Q35_2_exp="rental_15_storey", 
                    Q35_3_exp="condo_6_storey",
                    Q35_4_exp="condo_15_storey",
                    Q35_5_exp="single_detached",
                    Q35_6_exp="semi_detached")->on22
#### Rescale Q31

#Step 1, check to see what we are dealing with
# Here it is important to look for missing values
# Double-check in the SPSS file
on22 %>% 
  select(starts_with("Q31")) %>% 
  summary()
# For each one, we subtract 1 from the value, because the original values ran from 0 to 10
#Data frame
on22 %>% 
  #We are transforming variables, so we use mutate
  mutate(
    #across() lets us do something across each variable
    #We can use select helpers (see ?select) to pick what to work on
    #The ~ indicates the start of a function
    across(starts_with("Q31"), ~
             #The function is to subtract 1 from each variable
             .x-1
    )) %>% 
  #Select the variables
  select(starts_with("Q31")) %>% 
  #And see if it worked
  summary() #That worked.

#Repeat to save. 
#This is just copying and pasting
on22 %>% 
  mutate(
    across(starts_with("Q31"), ~
             .x-1
           #Note that I am resaving over on22
    ))->on22
#Rescale Q31 to 0 to 1
on22 %>% 
  mutate(
    across(matches("Q31_[0-9]$"), ~scales::rescale(as.numeric(.x)), .names="{.col}_x")
  )->on22

on22 %>% 
  select(starts_with("Q31")) %>% 
  summary()
#### Causes ####
# repeat the above process 
####n Q32
on22 %>% 
  select(starts_with("Q32")) %>% 
  summary()

on22 %>% 
  mutate(
    across(starts_with("Q32"), ~
             .x-1
    )) %>% 
  select(starts_with("Q32")) %>% 
  summary()

on22 %>% 
  mutate(
    across(starts_with("Q32"), ~
             .x-1
    ))->on22

on22 %>% 
  select(starts_with("Q32")) %>% 
  summary()


on22 %>% 
  mutate(
    across(matches("Q32_[0-9]$"), ~scales::rescale(car::Recode(as.numeric(.x), "11=5")), .names="{.col}_x")) ->on22
on22 %>% 
  select(ends_with("_x")) %>% 
  var_label()


table(on22$Q32_1, on22$Q32_1_x)
lookfor(on22, "rent")
table(on22$Q32_8, on22$Housing_Status)
on22 %>% 
  group_by(Housing_Status) %>% 
  summarize(Average=mean(Q32_8_x, na.rm=T))
#Assign variable labels

#Check
on22 %>% 
  select(starts_with("Q32"))

#### Rescale Q33
#Check

on22 %>% 
  select(starts_with("Q33")) %>% 
  val_labels()
on22 %>% 
  select(starts_with("Q33")) %>% 
  summary() 
#Subtract 1
on22 %>% 
  mutate(
    across(matches("Q33a_[0-9]$"), ~
             .x-1
    )) %>% 
  select(starts_with("Q33a")) %>% 
  summary() 

on22 %>% 
  mutate(
    across(matches("Q33a_[0-9]$"), ~
             .x-1
    )) ->on22

#the max should be 11 here, those are the don\'t knows
on22 %>% 
  select(starts_with("Q33a")) %>% 
  summary() 
on22 %>% 
  mutate(
    across(
      matches("Q33a_[0-9]$"), ~{
        scales::rescale(car::Recode(as.numeric(.x), "11=5"))
      }, .names="{.col}_x" ))->on22

#### Rescale Q80
#Q80
on22 %>% 
  select(starts_with("Q80")) %>% 
  summary()

on22 %>% 
  mutate(
    across(matches("Q80_[0-9]$"), ~
             .x-1
    )) %>% 
  select(starts_with("Q80")) %>% 
  summary() 

on22 %>% 
  mutate(
    across(matches("Q80_[0-9]$"), ~
             .x-1
    ))->on22
#Rescale Q80
on22 %>% 
  mutate(
    across(
      matches("Q80_[0-9]$"), ~{
        scales::rescale(car::Recode(as.numeric(.x), "11=5"))
      }, .names="{.col}_x" ))->on22

### Reocde Q33 and Q80 to categorical variable
on22 %>% 
  mutate(across(matches("Q33a_[0-9]$|Q80_[0-9]$"), 
                .fns=function(x) car::Recode(as.numeric(x), 
                                             "5:10='Support'; 0:4='Not Support'; 11='Not Support'", 
                                             levels=c("Not Support", "Support")), .names="{.col}_y"))->on22

#### Rescale Q34
on22$Q34_1
on22 %>% 
  mutate(
    across(
      starts_with("Q34"), ~{
        scales::rescale(abs(.x-100))
      }, .names="{.col}_x" )) ->on22
# 
# on22 %>%
#   select(starts_with("Q34")) %>%
# cor(., use="complete.obs")
# on22 %>% 
#   select(starts_with("Q34")) %>% 
#   summary()
# Adjust # of surveys taken


on22 %>% 
  mutate(Q48_x=car::Recode(Q48, "1=0; 2=1; 3=2; 4=3; 5=4; 6=5"))->on22

val_labels(on22$Q48_x)<-c(`5+`=5)


# Non-Partisan
on22$non_partisan<-Recode(on22$Q23, "6=1; else=0")
val_labels(on22$non_partisan)<-c("Non-Partisan"=1, "Partisan"=0)
names(on22)
# # Age Calculation
# on22 %>% 
#   select(starts_with("DOB")) %>% 
#   summary()
# 
# #Recode date of birth from qualtrics data
# on22$Q37_DO_NOT_USE
# 
# on22 %>% 
#   mutate(
#     DOB=case_when(
#       Q37_DO_NOT_USE==4~1920,
#       Q37_DO_NOT_USE==5~1921,
#       Q37_DO_NOT_USE==7~1922,
#       Q37_DO_NOT_USE==8~1923,
#       Q37_DO_NOT_USE==9~1924,
#       Q37_DO_NOT_USE==10~1925,
#       Q37_DO_NOT_USE==11~1926,
#       Q37_DO_NOT_USE==12~1927,
#       Q37_DO_NOT_USE==13~1928,
#       Q37_DO_NOT_USE==14~1929,
#       Q37_DO_NOT_USE==15~1930,
#       Q37_DO_NOT_USE==16~1931,
#       Q37_DO_NOT_USE==17~1932,
#       Q37_DO_NOT_USE==18~1933,
#       Q37_DO_NOT_USE==19~1934,
#       Q37_DO_NOT_USE==20~1935,
#       Q37_DO_NOT_USE==21~1936,
#       Q37_DO_NOT_USE==22~1937,
#       Q37_DO_NOT_USE==23~1938,
#       Q37_DO_NOT_USE==24~1939,
#       Q37_DO_NOT_USE==25~1940,
#       Q37_DO_NOT_USE==26~1941,
#       Q37_DO_NOT_USE==27~1942,
#       Q37_DO_NOT_USE==28~1943,
#       Q37_DO_NOT_USE==29~1944,
#       Q37_DO_NOT_USE==30~1945,
#       Q37_DO_NOT_USE==31~1946,
#       Q37_DO_NOT_USE==32~1947,
#       Q37_DO_NOT_USE==33~1948,
#       Q37_DO_NOT_USE==34~1949,
#       Q37_DO_NOT_USE==35~1950,
#       Q37_DO_NOT_USE==36~1951,
#       Q37_DO_NOT_USE==37~1952,
#       Q37_DO_NOT_USE==38~1953,
#       Q37_DO_NOT_USE==39~1954,
#       Q37_DO_NOT_USE==40~1955,
#       Q37_DO_NOT_USE==41~1956,
#       Q37_DO_NOT_USE==42~1957,
#       Q37_DO_NOT_USE==43~1958,
#       Q37_DO_NOT_USE==44~1959,
#       Q37_DO_NOT_USE==45~1960,
#       Q37_DO_NOT_USE==46~1961,
#       Q37_DO_NOT_USE==47~1962,
#       Q37_DO_NOT_USE==48~1963,
#       Q37_DO_NOT_USE==49~1964,
#       Q37_DO_NOT_USE==50~1965,
#       Q37_DO_NOT_USE==51~1966,
#       Q37_DO_NOT_USE==52~1967,
#       Q37_DO_NOT_USE==53~1968,
#       Q37_DO_NOT_USE==54~1969,
#       Q37_DO_NOT_USE==55~1970,
#       Q37_DO_NOT_USE==56~1971,
#       Q37_DO_NOT_USE==57~1972,
#       Q37_DO_NOT_USE==58~1973,
#       Q37_DO_NOT_USE==59~1974,
#       Q37_DO_NOT_USE==60~1975,
#       Q37_DO_NOT_USE==61~1976,
#       Q37_DO_NOT_USE==62~1977,
#       Q37_DO_NOT_USE==63~1978,
#       Q37_DO_NOT_USE==64~1979,
#       Q37_DO_NOT_USE==65~1980,
#       Q37_DO_NOT_USE==66~1981,
#       Q37_DO_NOT_USE==67~1982,
#       Q37_DO_NOT_USE==68~1983,
#       Q37_DO_NOT_USE==69~1984,
#       Q37_DO_NOT_USE==70~1985,
#       Q37_DO_NOT_USE==71~1986,
#       Q37_DO_NOT_USE==72~1987,
#       Q37_DO_NOT_USE==73~1988,
#       Q37_DO_NOT_USE==74~1989,
#       Q37_DO_NOT_USE==75~1990,
#       Q37_DO_NOT_USE==76~1991,
#       Q37_DO_NOT_USE==77~1992,
#       Q37_DO_NOT_USE==78~1993,
#       Q37_DO_NOT_USE==79~1994,
#       Q37_DO_NOT_USE==80~1995,
#       Q37_DO_NOT_USE==81~1996,
#       Q37_DO_NOT_USE==82~1997,
#       Q37_DO_NOT_USE==83~1998,
#       Q37_DO_NOT_USE==84~1999,
#       Q37_DO_NOT_USE==85~2000,
#       Q37_DO_NOT_USE==86~2001,
#       Q37_DO_NOT_USE==87~2002,
#       Q37_DO_NOT_USE==88~2003,
#       Q37_DO_NOT_USE==89~2004,
#       Q37_DO_NOT_USE==90~2005,
#       Q37_DO_NOT_USE==91~2006,
#       Q37_DO_NOT_USE==92~2007,
#       Q37_DO_NOT_USE==93~2008
#       )
#   )->on22
# 


#Calculating age in new variable: age
on22$age<-(2022-on22$yob)

#check age
# on22 %>% 
#   select(starts_with("age")) %>% 
#   summary()



# Time Flag
names(on22)
#Less than 100000 seconds?
on22 %>% 
  mutate(time_flag=case_when(
    `Duration__in_seconds_`>3600 ~1,
    TRUE ~0
  ))->on22
#### Odd Voting Combinations
on22 %>% 
  mutate(voting_flag=case_when(
    #If vote intention is Liberal or reported vote is Liberal and variable won't vote against Liberal is also 1, set to 1
    (Q8==1| Q10==1) & Q12_1==1 ~ 1, 
    (Q8==2| Q10==2) & Q12_2==2 ~ 1, #PC
    (Q8==3| Q10==3) & Q12_3==3 ~ 1, #NDP
    (Q8==4| Q10==4) & Q12_4==4 ~ 1, #GRN
    TRUE ~ 0
   ))->on22
names(on22)


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
#How many respondents digits
lookfor(on22, "incom")
on22 %>% 
  mutate(income_digits=unlist(map(.$Q42, nchar)))->on22
on22$income_digits
names(on22)

#Swing Voter Variable

on22 %>% 
  mutate(Swing=case_when(
    (Q6b==1 | Q6b==3 |Q6b==4) & (Q8==2 | Q9==2 | Q10==2 | Q11==2) ~ "Swing Voter",
    TRUE ~ "Other"
  ))->on22
table(on22$Swing)
on22$Swing<-factor(on22$Swing, levels=c("Swing Voter", "Other"))
on22$Q7
on22 %>% 
  mutate(Abstain=case_when(
    (Q6a==1) & (Q7==3|Q7==4) ~ "Apathetic Voter",
    Q7==6~ NA_character_,
    TRUE ~ "Other"
  ))->on22
on22$Abstain<-factor(on22$Abstain, levels=c("Apathetic Voter", "Other"))

#Degree
lookfor(on22, "degree")
on22$Q39
on22$Degree<-Recode(as.numeric(on22$Q39), "9:11='Degree'; else='No degree'", 
                    levels=c("No degree", "Degree"))
lookfor(on22, "urban")
on22$Density<-as_factor(on22$Q41)
levels(on22$Density)<-c("Suburban", "Urban", "Rural")
#Renter variabvle

#levels(on22$Housing_Status)<-c("Satisfied Homeowner", "First-Time Homebuyer", "Speculator", "Satisfied Renter", "Other")
on22 %>% 
  mutate(Renter=case_when(
    Q27==2~ 'Renter',
    TRUE ~ 'Other',
  ))->on22
#Satisfied_Renter
on22 %>% 
  mutate(Satisfied_Renter=case_when(
    Q27==2&Q30==2~ 'Satisfied Renter',
    TRUE ~ 'Other',
  ))->on22
#on22$`First_Time_Buyer`<-Recode(on22$Housing_Status, "'First-Time Homebuyer'='First-Time Homebuyer'; else='Other'", levels=c("Other", "First-Time Homebuyer"))
on22$`Homeowner`<-Recode(on22$Housing_Status, "'Homeowner'='Homeowner'; else='Other'", levels=c("Other", "Homeowner"))
#on22$`Speculator`<-Recode(on22$Housing_Status, "'Speculator'='Speculator'; else='Other'", levels=c("Other", "Speculator"))
on22$`Buyer`<-Recode(on22$Housing_Status, "'Seeking to purchase'='Buyer' ; else='Other'", as.factor=T, levels=c("Other", "Buyer"))
#Causes by renter/non-renter dummy variable
# on22$renter<-ifelse(on22$Q27==2,1,0)
# val_labels(on22$renter)<-c("Renter"=1, "Non-Renter"=0)
nrow(on22)
names(on22)

#### Extract Region from postal code
on22$region<-str_sub(on22$Postal_code, end=1L)
on22$region
table(on22$Density, on22$region)
on22$region<-Recode(on22$region, "'K'='Eastern Ontario' ;
'L'='Central Ontario' ; 
       'M'='Metropolitan Toronto'; 'N'='SW Ontario' ; 'P'='Northern Ontario' ; else='Other' ", 
       levels=c("Metropolitan Toronto", "Central Ontario", "SW Ontario", "Eastern Ontario", "Northern Ontario"),
       as.factor=T)


#### Cognitive Non-Partisanship
lookfor(on22, "interest")
lookfor(on22, "provincial")
summary(on22$Q4_1)
on22$Q5_1
on22 %>% 
  mutate(cognitive_non_partisan=case_when(
    Q4_1 >5 & Q23==6~"Cognitive Non-Partisan",
    TRUE ~ "Other"
  ))->on22
table(on22$cognitive_non_partisan)



### Age
on22$Over_55<-Recode(as.numeric(on22$agegrps), "5:6='Over 55' ; 1:4='Under 55'", 
                     as.factor=T, levels=c("Under 55", "Over 55"))
on22$Under_35<-Recode(as.numeric(on22$agegrps), "1:2='Under 35' ; 
                      3:6='Over 36'", 
                      as.factor=T, levels=c("Over 36","Under 35" ))
table(on22$Over_55)

#### Gender
on22$gender
on22$male<-Recode(as.numeric(on22$gender), 
                  "1='Male' ;2:3='Non-Male'", 
                  as.factor=T,
                  levels=c("Non-Male", "Male"))
#### Combine attitude to affordable housing and homeownership status

lookfor(on22, "affordable")
lookfor(on22, "own")

on22 %>% 
  mutate(own_affordable=case_when(
    Q27==1 &  Q21==3 ~ "Pro-Affordable Housing Homeowner",
    Q27==1 & Q21<3 ~ "Anti-Affordable Housing Homeowner",
    Q27>1 & Q21==3 ~ "Pro-Affordable Housing Non-Homeowner",
    Q27>1 & Q21 <3 ~ "Anti-Affordable Housing Non-Homeowner"
  ))->on22
table(on22$own_affordable)
on22$Q21
on22$affordable<-Recode(as.numeric(on22$Q21), "1:2='conservative_housing' ; 
                        3='liberal_housing'", 
                        levels=c("liberal_housing", 
                              "conservative_housing"))


table(on22$Homeowner, as_factor(on22$Q27))

on22$own_affordable<-factor(on22$own_affordable, levels=c("Pro-Affordable Housing Homeowner", 
       "Anti-Affordable Housing Homeowner",
       "Pro-Affordable Housing Non-Homeowner",
       "Anti-Affordable Housing Non-Homeowner"))


names(on22)
qplot(pop_density, geom="histogram", data=on22)
qplot(Population, geom="histogram", data=on22)
# on22 %>% 
#   group_by(CSDNAME) %>% 
#   summarize(density=mean(pop_density, na.rm=T)) %>% 
#   arrange(desc(density)) %>% 
#   View()
# on22 %>%
#   group_by(CSDNAME) %>%
#   summarize(pop=mean(pop_2021, na.rm=T)) %>%
#   arrange(desc(pop)) %>% View()

on22$Size<-Recode(on22$Population, "0:25000='Rural';
25001:99999='Small' ;
       100000:499999='Medium';
       500000:1020000='Large' ;
       2000000:3000000='Toronto/Ottawa'", as.factor=T, 
                  levels=c("Medium","Rural", "Small", "Large", "Toronto/Ottawa"))
table(on22$Size)
#### Vote Intention
val_labels(on22$Q7)
val_labels(on22$Q8)
on22 %>% 
  mutate(Vote_Intention_Likely=case_when(
   ( Q7==1|Q7==2 |Q7==5 )& Q8==1 ~ "Liberal",
   ( Q7==1|Q7==2 |Q7==5 )& Q8==2 ~ "PC",
   ( Q7==1|Q7==2 |Q7==5 )& Q8==3 ~ "NDP",
   ( Q7==1|Q7==2 |Q7==5 )& Q8==4 ~ "Green",
  ))->on22
on22$Q7
var_label(on22$Q9)
table(on22$Q9)
table(on22$Q7, on22$Q9)
on22 %>% 
  mutate(Vote_Intention_Unlikely=case_when(
    ( Q7==3|Q7==4 )& Q9==1 ~ "Liberal",
    ( Q7==3|Q7==4 )& Q9==2  ~ "PC",
    ( Q7==3|Q7==4 )& Q9==3  ~ "NDP",
    ( Q7==3|Q7==4 )& Q9==4  ~ "Green",
  ))->on22

on22 %>% 
  mutate(Vote_Intention_All=case_when(
     Q8==1 ~ "Liberal",
    Q8==2 ~ "PC",
   Q8==3 ~ "NDP",
    Q8==4  ~ "Green",
  ))->on22
#Convert to factors

on22$Vote_Intention_Likely<-factor(on22$Vote_Intention_Likely, levels=c("PC", "Liberal", "NDP", "Green"))
on22$Vote_Intention_Unlikely<-factor(on22$Vote_Intention_Unlikely, levels=c("PC", "Liberal", "NDP", "Green"))
on22$Vote_Intention_All<-factor(on22$Vote_Intention_All, levels=c("PC", "Liberal", "NDP", "Green"))
table(on22$Vote_Intention_Unlikely)

#Conservative Dummy Variables
lookfor(on22, "vote")
on22$PC<-Recode(on22$Vote, "'PC'='PC'; else='Other'", levels=c("Other", "PC"))

on22$PC_Vote22<-Recode(on22$Vote_Intention_Likely, "'PC'='PC' ;
       else='Other'", levels=c("Other", "PC"))




#This table shows PC Voters in rows and swing voters in columns
# There are 84 Voters who voted Liberal/ NDP in the last time and voted PC this time
table(on22$PC_Vote22,on22$Swing)
table(on22$PC_Vote22, on22$Abstain)

#Most Important Problem
#Read in most important problem responsesr
library(readxl)
#Read in the coded responses to mip done by Molly. 
#column A is the raw response # Column B is her code
mip<-read_excel(path=here("Data/mip.xlsx"), range="A1:B967", col_types=c("text", "numeric"))
#Convert the mip problem in on22 to all lower case
#Store in on22$mip
#The on22 mip responses are now identical to the mip responses in MOlly's excel file
on22$mip<-str_to_lower(on22$Q3)
#Now merge on22 with the mip object
#Using the mip variable as the key
nrow(mip)
mip %>% 
  distinct()->mip
#Filter out unique responses
on22 %>% 
  left_join(.,mip, by=c("mip"="mip"))->on22
names(on22)

#Now read in the Molly's code LABELS
mip_categories<-read_excel(path=here("Data/mip_categories_final.xlsx"))

mip_categories
on22 %>% 
  left_join(., mip_categories, by=c("mip_code"="Code Number"))->on22
names(on22)
on22$Topic<-as.factor(on22$Topic)


#Housing and Cost of Living Issues recoded

on22 %>% 
  mutate(MIP_Cost_Housing=case_when(
    Topic=="Cost of living, living expenses / wages / Inflation" ~ "Cost of Living",
    Topic=="Housing" ~ "Housing",
    TRUE ~ "Other"
  ))->on22
table(on22$MIP_Cost_Housing)
on22$MIP_Cost_Housing<-factor(on22$MIP_Cost_Housing, levels=c("Cost of Living", "Housing", "Other"))

on22 %>% 
  mutate(MIP_top3=case_when(
    Topic=="Cost of living, living expenses / wages / Inflation" ~ "Cost of Living",
    Topic=="Housing" ~ "Housing",
    Topic=="Health care issues (non-COVID)" ~ "Health Care",
    TRUE ~ "Other"
  ))->on22
table(on22$MIP_top3)
on22$MIP_top3<-factor(on22$MIP_top3, levels=c("Cost of Living", "Housing", "Health Care", "Other"))

on22 %>% 
  mutate(MIP_top5=case_when(
    Topic=="Cost of living, living expenses / wages / Inflation" ~ "Cost of Living",
    Topic=="Housing" ~ "Housing",
    Topic=="Health care issues (non-COVID)" ~ "Health Care",
    Topic=="Environmental / ecological issues / climate change" ~ "Environment",
    Topic=="Economy" ~ "Economy",
    TRUE ~ "Other"
  ))->on22
table(on22$MIP_top5)
on22$MIP_top5<-factor(on22$MIP_top5, levels=c("Cost of Living", "Housing", "Health Care", "Environment", "Economy", "Other"))

table(on22$Topic)

#### Party Handling
# These variables are the party best handling certain issues
# variable labels are assigned in the 2_variable_label scripts
# This code simply sets the factor levels to be equivalent to vote choice
on22 %>% 
  #select(starts_with("Q15")) %>% 
  #as_factor() %>% 
  mutate(across(.cols=starts_with("Q15"), .fns=function(x) Recode(as_factor(x), "'Ontario Liberal Party'='Liberal';
                      'Ontario New Democratic Party'='NDP' ;
                      'Progressive Conservative Party of Ontario'='PC' ;
                      'Green Party of Ontario'='Green'", levels=c("PC", "NDP", "Liberal", "Green"))))->on22

#### YIMBY and NIMBYism

on22 %>% 
  mutate(YIMBY=case_when(
    on22$Q80_3_y=="Support" & on22$Q33a_6_y=="Support" ~ "YIMBY",
    TRUE ~ "Not YIMBY")
  )->on22
lookfor(on22, "own")
on22 %>% 
  mutate(NIMBY=case_when(
    on22$Q80_3_y=="Support" & on22$Q33a_6_y=="Not Support" &Q27==1~ "NIMBY",
    TRUE ~ "Not NIMBY")
  )->on22
table(on22$NIMBY)
## Make a speculator versus everyone else variable. 

#Create new variable for housing status

###Create new variables for rescaling spending preferences
on22$Q16_x<-Recode(as.numeric(on22$Q16), "1=0; 2=0.5 ; 3=1")
table(on22$Q16_x)

on22$Q17_x<-Recode(as.numeric(on22$Q17), "1=0; 2=0.5 ; 3=1")
table(on22$Q17_x)

on22$Q18_x<-Recode(as.numeric(on22$Q18), "1=1; 2=0.5 ; 3=0")
table(on22$Q18_x)

on22$Q19_x<-Recode(as.numeric(on22$Q19), "1=0; 2=0.5 ; 3=1")
table(on22$Q19_x)

on22$Q20_x<-Recode(as.numeric(on22$Q20), "1=0; 2=0.5 ; 3=1")
table(on22$Q20_x)

on22$Q21_x<-Recode(as.numeric(on22$Q21), "1=0; 2=0.5 ; 3=1")
table(on22$Q21_x)

on22$Ideology<-rowMeans(on22[ , c("Q16_x","Q17_x", "Q18_x", "Q19_x", "Q20_x", "Q21_x")], na.rm=TRUE)

#### Political Interest ####

on22$Interest<-cut(on22$Q4_1, breaks=3, labels=c("Low", "Medium", "High"))

#Run a script setting value and variable labels
source("R_Scripts/2_value_labels.R")
source("R_Scripts/2_variable_labels.R")

# We have to take the variable labels  in the original cause variables and match them to the ones that end in _x
# 
#Get variable labels and Store them. 
#This is great way to get a batch of variable labels
on22 %>% 
  #Select what you are looking to work with
  #In this case it is the batch of rescaled cause variables
  select(Q32_1_x:Q32_9_x) %>% 
  #Use the command look_for() in the labelled library, must be loaded!
  #Store in something meaningful
  look_for()->cause_var_labels
#Inspect
cause_var_labels#Note that the variable name is stored in variable and the actual label is stored in label
#Here we remove the bit about Causes - from each entry and save it back into the label variable
cause_var_labels$label<-str_remove_all(cause_var_labels$label, "Causes - ")
#Check what has happened
cause_var_labels
#make solution variable label data frame

on22 %>% 
  select(Q33a_1_x:Q80_6_x) %>% 
  look_for()->solution_var_labels

#Inspect
solution_var_labels$label<-str_remove_all(solution_var_labels$label, "Support for policy - ")

lookfor(on22, "purchase")
#### Experiment #### 

on22 %>% 
  select(ends_with('_exp')) %>% 
  var_label()->experimental_variable_labels
experimental_variable_labels
on22 %>%
  # This renames the names of the Developmental approval ratings
  # With the type of development
  rename_with(~ unlist(experimental_variable_labels), ends_with('_exp'))->on22
#Check

#on22$Experimental_Group<-Recode(on22$Experimental_Group,as.factor=T, "'Control'='Control' ; 'Private'='Individual' ; 'Public'='Community';'Social'='National'", 
#levels=c("Control" ,"Individual", "Community", "National"))
on22$Experimental_Group<-factor(on22$Experimental_Group, levels=c("Control", "Individual", "Community", "National"))
table(on22$Experimental_Group)
names(on22)
#### Stack for the experiment ####
# This code stacks on22 with six rows for each respondent, one row for each respondent's 
# level of support for a type of development
names(on22)
on22 %>% 
  pivot_longer(., cols="rental_6_storey":"semi_detached", 
               names_to="Development", values_to="Development_Support")->on22_stacked
on22_stacked$Development<-factor(on22_stacked$Development,
                                 levels=c("rental_6_storey",
                                          "rental_15_storey",
                                          "condo_6_storey",
                                          "condo_15_storey",
                                          "single_detached",
                                          "semi_detached"))
names(on22)
