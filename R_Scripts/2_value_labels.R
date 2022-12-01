library(here)
#source(here("R_Scripts", "1_data_import.R"))
#install.packages("modelsummary")
#Value Labels
# For each of the batteries we identified, we have to adjust the values and value labels

#### Q31


# Step 2, change the value labels. 

on22 %>% 
  select(starts_with("Q31")) %>% 
val_labels()

on22 %>% 
  mutate(
    across(matches("Q31_[0-9]$"), ~
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


# Step 2, change the value labels. 

on22 %>% 
  select(starts_with("Q32")) %>% 
  val_labels()

on22 %>% 
  mutate(
    across(matches("Q32_[0-9]$"), ~
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

# Step 2, change the value labels for Q33 to match the 1 subtraction done in 2_recodes.R

on22 %>% 
  select(starts_with("Q33")) %>% 
  val_labels()

on22 %>% 
  mutate(
    across(matches("Q33_[0-9]$"), ~
             {
               val_labels(.x)<-c("0 - Strongly oppose"=0, "10 - Strongly support"=10, "Don\'t Know"=11)
               .x
             }  )
  )->on22






# Step 2, change the value labels. 

on22 %>% 
  select(starts_with("Q80")) %>% 
  val_labels()

on22 %>% 
  mutate(
    across(matches("Q80_[0-9]$"), ~
             {
               val_labels(.x)<-c("0 - Strongly oppose"=0, "10 - Strongly support"=10, "11 - Don't Know"=11)
               .x
             }  )
  )->on22

#Check

on22 %>% 
  select(starts_with("Q80")) %>% 
  val_labels()


