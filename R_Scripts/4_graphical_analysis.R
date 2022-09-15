source("R_Scripts/2_recodes.R")
#Install wlucolrs if necessary
#remotes::install_github("sjkiss/wlucolors")
library(wlucolors)
theme_set(theme_classic())
#### Causes ####
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
on22$Housing_Status
on22$Housing_Status<-factor(on22$Housing_Status, levels=c("First-Time Homebuyer", "Speculator", "Satisfied Homeowner", "Satisfied Renter", "Other"))

#Now the graph
on22 %>% 
  #Change the Housing STatus variable so that First Time Homebuyers is first
  mutate(Housing_Status=fct_relevel(Housing_Status, "First-Time Homebuyer")) %>% 
  #select what you're working with; in this case the batch of cause variables and housing status
  select(Q32_1_x:Q32_9_x, Housing_Status) %>% 
  #pivot them longer, except for the grouping variable of interest
  #Note that I am *not* pivotting housing status down. Note also that I am setting hte name of the new column to be "variable"
  #The point here is to make it match with the variable above in cause_var_labels
  pivot_longer(., cols=-Housing_Status, names_to=c("variable")) %>% 
  #Form groups by Housing STatus and the variable of interest
group_by(Housing_Status, variable) %>% 
  filter(value!="NA") %>% 
  filter(Housing_Status!="Other") %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  #this is the merge
  #It joins what came above in the pipe with the cause_var_labels object
  #Because both have variables `variable` It automatically merges on that variable
  left_join(., cause_var_labels) %>% 
#And graph
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=Housing_Status))+ylim(c(0,1))+
  geom_pointrange(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), position=position_jitter(width=0.25))+coord_flip()+
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")

on22 %>% 
  mutate(renter=fct_relevel(as_factor(renter), "Renter")) %>% 
  select(Q32_1_x:Q32_9_x, renter) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-renter, names_to=c("variable")) %>% 
  group_by(renter, variable) %>% 
  filter(!is.na(renter)) %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., cause_var_labels) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=renter))+ylim(c(0,1))+
  geom_pointrange(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), position=position_jitter(width=0.25))+coord_flip()+
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")
#scale_x_discrete(labels = c("Underinvestment in \npublic affordable housing","Speculation by investors",  "Limited rent control","Excessive foreign \nimmigration to ontario","Urban sprawl","Low interest rates","Municipal red tape","Neighbourhood opposition","Environmental protection"))+

#Causes by Partisanship
on22 %>% 
  #mutate(renter=fct_relevel(as_factor(partisanship), "Partisanship")) %>% 
  #Pick the variables working with
  select(Q32_1_x:Q32_9_x, partisanship) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-partisanship,names_to=c("variable")) %>% 
  group_by(partisanship, variable) %>% 
  filter(partisanship!="NA") %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., cause_var_labels) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=partisanship))+ylim(c(0,1))+
  geom_pointrange(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), position=position_jitter(width=0.25))+coord_flip()+
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")

names(on22)

#### Solutions ####
on22 %>% 
  select(Q33a_1_x:Q80_6_x) %>% 
  look_for()->solution_var_labels
#Inspect
solution_var_labels$label<=str_remove_all(solution_var_labels$label, "Support for policy - ")
#Note that the variable name is stored in variable and the actual label is stored in label

#Solutions by insider/outsider
on22 %>% 
  mutate(Housing_Status=fct_relevel(Housing_Status, "First-Time Homebuyer")) %>% 
  select(Q33a_1_x:Q80_6_x, Housing_Status) %>% 
  pivot_longer(., cols=-Housing_Status, names_to=c("variable")) %>% 
  group_by(Housing_Status, variable) %>% 
  filter(value!="NA") %>% 
  filter(Housing_Status!="Other") %>%
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  mutate(label=str_remove_all(label, "Support for policy - ")) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=Housing_Status))+ylim(c(0,1))+
  geom_pointrange(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), position=position_jitter(width=0.25))+coord_flip()+
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")

#Solutions by renter/non-renter dummy variable
on22 %>% 
  mutate(renter=fct_relevel(as_factor(renter), "Renter")) %>% 
  select(Q33a_1_x:Q80_6_x, renter) %>% 
  pivot_longer(., cols=-renter, names_to=c("variable")) %>% 
  group_by(renter, variable) %>% 
  filter(value!="NA") %>% 
  filter(renter!="NA") %>%
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  mutate(label=str_remove_all(label, "Support for policy - ")) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=renter))+ylim(c(0,1))+
  geom_pointrange(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), position=position_jitter(width=0.25))+coord_flip()+
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")

#Solutiosn By Partisanship
on22 %>% 
  #Pick the variables working with
  select(Q33a_1_x:Q80_6_x, partisanship) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-partisanship, names_to=c("variable")) %>% 
  group_by(partisanship, variable) %>% 
  filter(!is.na(value)) %>% 
  filter(partisanship!=5) %>% #Filter out R's that identify as "Other" 
  filter(partisanship!=6) %>%# Filter out R's that identify as "None of these"
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=partisanship))+ylim(c(0,1))+
  geom_pointrange(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), position=position_jitter(width=0.25))+coord_flip()+
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")


