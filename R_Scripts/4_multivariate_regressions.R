source("R_Scripts/4_Analysis.R")

#### Selecting The Variables
# Step 1 select the variables you need.
names(on22)
lookfor(on22, "cause")
#Causes are stored in Q32_1_x, converted 0 to 1
#First run a check
on22 %>% 
  select(starts_with("Q32")& ends_with("_x"))
#Now add in demographics
names(on22)
on22 %>% 
  select(starts_with("Q32")& ends_with("_x"), gender, Housing_Status, partisanship, Degree, Density, PC)

#The next thing is that we want to fit one regression for each of Q32_1_x, Q32_2_x, etc. 
#We need to pivot all the IVs down into two columns: 1 will contain the name of the IV (e.g. Q32_1_x, 2_x, etc. ) And the other will contain the Rs score for each question.

on22 %>% 
  select(starts_with("Q32")& ends_with("_x"), gender, Housing_Status, partisanship, Degree, Density, PC) %>% 
#Pivotting down columns that end in _x 
  #We make the name of the column that contains the IV name equal to Variable and we make the column that contains the score equal to Score
  #Note how many rows we now have in the data-set, we have over 17000 rows, because each R answered each of Q32_1, Q32_2, etc. 
  pivot_longer(starts_with("Q32_"), names_to="Variable", values_to="Score")


#Now we need to run the regression for each instance of Variable
#The first thing to do is to *nest* the data frame so that the variables we will use in the regression are tucked into one row for each variable asked
#So the only variable that we do not want to nest is the name of the question, which is stored in "Variable"
on22 %>% 
  select(starts_with("Q32")& ends_with("_x"), gender, Housing_Status, partisanship, Degree, Density, PC) %>% 
  #Pivotting down columns that end in _x 
  #We make the name of the column that contains the IV name equal to Variable and we make the column that contains the score equal to Score
  #Note how many rows we now have in the data-set, we have over 17000 rows, because each R answered each of Q32_1, Q32_2, etc. 
  pivot_longer(starts_with("Q32_"), names_to="Variable", values_to="Score") %>% 
  nest(data=-Variable)
#Now look at the new "data" column. Each row contains its own data frame with the variables we need for the regression. 

#So the only variable that we do not want to nest is the name of the question, which is stored in "Variable"
on22 %>% 
  select(starts_with("Q32")& ends_with("_x"), gender, Housing_Status,Degree, Density, PC, Degree) %>% 
  #Pivotting down columns that end in _x 
  #We make the name of the column that contains the IV name equal to Variable and we make the column that contains the score equal to Score
  #Note how many rows we now have in the data-set, we have over 17000 rows, because each R answered each of Q32_1, Q32_2, etc. 
  pivot_longer(starts_with("Q32_"), names_to="Variable", values_to="Score") %>% 
  nest(data=-Variable) %>%
  #We are going to *add* a column to this data frame that we have created which will contain the model results. To do so, we will use the mutate command. 
  #We will call the first set of models we run m1
  #The next line makes a function that is the model call with all the variables we need
  #It runs that function on each row of the variable "data"
  mutate(m1=map(data, function(x) lm(Score~gender+Density+Degree+PC+Housing_Status, data=x)))
#This is hard to read. So we use a handy broom and unnest function. 
on22 %>% 
  select(starts_with("Q32")& ends_with("_x"), gender, Housing_Status,Degree, Density, PC, Degree) %>% 
  #Pivotting down columns that end in _x 
  #We make the name of the column that contains the IV name equal to Variable and we make the column that contains the score equal to Score
  #Note how many rows we now have in the data-set, we have over 17000 rows, because each R answered each of Q32_1, Q32_2, etc. 
  pivot_longer(starts_with("Q32_"), names_to="Variable", values_to="Score") %>% 
  nest(data=-Variable) %>%
  #We are going to *add* a column to this data frame that we have created which will contain the model results. To do so, we will use the mutate command. 
  #We will call the first set of models we run m1
  #The next line makes a function that is the model call with all the variables we need
  #It runs that function on each row of the variable "data"
  mutate(m1=map(data, function(x) lm(Score~gender+Density+Degree+PC+Housing_Status, data=x)))->cause_models
cause_models
#he result is hard to work with, but you can do it. 
#We can map onto each of cause_models$m1 the summary function
map(cause_models$m1, summary)

#### Tidying the Regressions
#But I like to go a step further and "tidy" the model summaryies
library(broom)
cause_models %>% 
  #This puts each model in a proper table
  mutate(m1_tidy=map(m1, tidy))

cause_models %>% 
  #This puts each model in a proper table
  mutate(m1_tidy=map(m1, tidy)) %>% 
  unnest(m1_tidy)

cause_models %>% 
  #This puts each model in a proper table
  mutate(m1_tidy=map(m1, tidy)) %>% 
  unnest(m1_tidy)->cause_models
cause_models
#Now we actually have the coefficients for each covariate and each IV and each DV in a nice neat table
#View(cause_models)
# IN the previous script we also got variable labels for each cause in this object
cause_var_labels

#### Printing the Regressions
library(modelsummary)
modelsummary(cause_models$m1)




         m1_tidy=map(m1, tidy))->cause_models


