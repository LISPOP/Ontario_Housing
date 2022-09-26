library(here)
#source(here("R_Scripts", "1_data_import.R"))
#install.packages("modelsummary")
#Value Labels
# For each of the batteries we identified, we have to adjust the values and value labels

#### Q31
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

# Step 2, change the value labels. 

on22 %>% 
  select(starts_with("Q31")) %>% 
val_labels()

on22 %>% 
  mutate(
    across(starts_with("Q31"), ~
             {
               #Set the value labels
               #Note, here, you want to also set a don't know, if possible
  val_labels(.x)<-c("0 - Unable to Afford"=0, "10 - Able to Afford"=10)
 .x
   }  )
  )->on22

#Check

on22 %>% 
  select(starts_with("Q31")) %>% 
  val_labels()

on22 %>% 
  select(starts_with("Q31")) %>% 
  summary()

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

# Step 2, change the value labels. 

on22 %>% 
  select(starts_with("Q32")) %>% 
  val_labels()

on22 %>% 
  mutate(
    across(starts_with("Q32"), ~
             {
               val_labels(.x)<-c("0 - Not at all a cause"=0, "10 - A significant cause"=10, "11 - Don't Know"=11)
               .x
             }  )
  )->on22

on22 %>% 
  select(starts_with("Q32")) %>% 
  summary() 


#Check

on22 %>% 
  select(starts_with("Q32")) %>% 
  val_labels()

####n Q33
on22 %>% 
  select(starts_with("Q33")) %>% 
  summary()

on22 %>% 
  mutate(
    across(starts_with("Q33"), ~
             .x-1
    )) %>% 
  select(starts_with("Q33")) %>% 
  summary() 

on22 %>% 
  mutate(
    across(starts_with("Q33"), ~
             .x-1
    ))->on22

# Step 2, change the value labels. 

on22 %>% 
  select(starts_with("Q33")) %>% 
  val_labels()

on22 %>% 
  mutate(
    across(starts_with("Q33"), ~
             {
               val_labels(.x)<-c("0 - Strongly oppose"=0, "10 - Strongly support"=10)
               .x
             }  )
  )->on22




#Q80
on22 %>% 
  select(starts_with("Q80")) %>% 
  summary()

on22 %>% 
  mutate(
    across(starts_with("Q80"), ~
             .x-1
    )) %>% 
  select(starts_with("Q80")) %>% 
  summary() 

on22 %>% 
  mutate(
    across(starts_with("Q80"), ~
             .x-1
    ))->on22

# Step 2, change the value labels. 

on22 %>% 
  select(starts_with("Q80")) %>% 
  val_labels()

on22 %>% 
  mutate(
    across(starts_with("Q80"), ~
             {
               val_labels(.x)<-c("0 - Strongly oppose"=0, "10 - Strongly support"=10)
               .x
             }  )
  )->on22

#Check

on22 %>% 
  select(starts_with("Q80")) %>% 
  val_labels()


