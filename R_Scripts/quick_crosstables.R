source("R_Scripts/3_descriptives.R")
#How to do quick and dirty multiple crosstabs

library(janitor)
library(gt)
#Get Quick and dirty crosstabs into an html file which can be plunked into 
on22 %>% 
  tabyl(Vote_Intention_Likely) %>% 
  gt() %>% 
  gtsave(., filename=here("Tables", "vote_distribution.html"))
on22 %>% 
  tabyl(Q23) %>% 
  gt() %>% 
  gtsave(., filename=here("Tables", "partisanship.html"))
#How to do frequencies of multiple variables
# the pivot longer function is basically unavoidable
#Recall what it does
on22 %>% 
  #Pivots down all variables that start with Q15
  pivot_longer(., cols=starts_with("Q15")) #Default new variables are 'name' and 'value'
#Recall what it does
on22 %>% 
  #Pivots down all variables that start with Q15
  pivot_longer(., cols=starts_with("Q15")) %>% 
  #Default new variables are 'name' and 'value'
names()
#Recall what it does
on22 %>% 
  #Pivots down all variables that start with Q15
  pivot_longer(., cols=starts_with("Q15")) %>% 
#Create tabyl of "name" by "value
  tabyl(name, value) 
#Add percentages
#Recall what it does
on22 %>% 
  #Pivots down all variables that start with Q15
  pivot_longer(., cols=starts_with("Q15")) %>% 
  #Create tabyl of "name" by "value
  tabyl(name, value) %>% 
  #add percentages
  adorn_percentages()


#Recall what it does
on22 %>% 
  #Pivots down all variables that start with Q15
  pivot_longer(., cols=starts_with("Q15")) %>% 
  #Create tabyl of "name" by "value
  tabyl(name, value) %>% 
  #add percentages
  adorn_percentages()
#Add ns()
#Recall what it does
on22 %>% 
  #Pivots down all variables that start with Q15
  pivot_longer(., cols=starts_with("Q15")) %>% 
  #Create tabyl of "name" by "value
  tabyl(name, value) %>% 
  #add percentages
  adorn_percentages() %>% 
#Add ns()
adorn_ns()

#Recall what it does
on22 %>% 
  #Pivots down all variables that start with Q15
  pivot_longer(., cols=starts_with("Q15")) %>% 
  #Create tabyl of "name" by "value
  tabyl(name, value) %>% 
  #add percentages
  adorn_percentages() %>% 
  #Add ns()
  adorn_ns() %>% 
#Turn to gt()
  gt() 

#Recall what it does
on22 %>% 
  #Pivots down all variables that start with Q15
  pivot_longer(., cols=starts_with("Q15")) %>% 
  #Create tabyl of "name" by "value
  tabyl(name, value) %>% 
  #add percentages
  adorn_percentages() %>% 
  #Add ns()
  adorn_ns() %>% 
  #Turn to gt()
  gt() %>% 
  #Save out
  gtsave(., here("Tables", "issue_ownership.html"))
