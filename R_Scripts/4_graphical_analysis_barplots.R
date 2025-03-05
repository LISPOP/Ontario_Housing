source("R_Scripts/2_recodes.R")
#Install wlucolrs if necessary
#remotes::install_github("sjkiss/wlucolors")
library(wlucolors)
theme_set(theme_classic())
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
mutate(Agree=case_when(
  value>0.5~1,
  TRUE~ 0
)) %>% # summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  #this is the merge
  #It joins what came above in the pipe with the cause_var_labels object
  #Because both have variables `variable` It automatically merges on that variable
  left_join(., cause_var_labels) %>% 
  group_by(Housing_Status, variable, label, Agree) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n)*100) %>% 
  filter(Agree==1) %>% 
  ggplot(., aes(y=fct_reorder(label, Percent), x=Percent, fill=Housing_Status))+
  geom_col(position="dodge")+
  geom_vline(xintercept=50, linetype=2)+
  labs(y="Cause", x="Percent Agreeing")

##CAUSES BY RENTING STATUS
on22 %>% 
  #Change the Housing STatus variable so that First Time Renter is first
  mutate(Renter=fct_relevel(as_factor(Renter), "Renter")) %>% 
  #select what you're working with; in this case the batch of cause variables and housing status
  select(Q32_1_x:Q32_9_x, Renter) %>% 
  pivot_longer(., cols=-Renter, names_to=c("variable")) %>% 
  group_by(Renter, variable) %>% 
  filter(value!="NA") %>% 
  filter(Renter!="Other") %>% 
  mutate(Agree=case_when(
    value>0.5~1,
    TRUE~ 0)) %>% 
  left_join(., cause_var_labels) %>% 
  group_by(Renter, variable, label, Agree) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n)*100) %>% 
  filter(Agree==1) %>% 
  ggplot(., aes(y=fct_reorder(label, Percent), x=Percent, fill=Renter))+
  geom_col(position="dodge")+geom_vline(xintercept=50, linetype=2)+labs(y="Cause", x="Percent Agreeing")

##CAUSES BY PARTISANSHIP
on22 %>% 
  mutate(Renter=fct_relevel(as_factor(partisanship), "Partisanship")) %>% 
  select(Q32_1_x:Q32_9_x, partisanship) %>% 
  pivot_longer(., cols=-partisanship, names_to=c("variable")) %>% 
  group_by(partisanship, variable) %>% 
  filter(value!="NA") %>% 
  filter(partisanship!="Other") %>% 
  mutate(Agree=case_when(
    value>0.5~1,
    TRUE~ 0)) %>% 
  left_join(., cause_var_labels) %>% 
  group_by(partisanship, variable, label, Agree) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n)*100) %>% 
  filter(Agree==1) %>% 
  ggplot(., aes(y=fct_reorder(label, Percent), x=Percent, fill=partisanship))+
  geom_col(position="dodge")+geom_vline(xintercept=50, linetype=2)+labs(y="Cause", x="Percent Agreeing")
  
#### Solutions ####
##Housing Status
on22 %>% 
  mutate(Housing_Status=fct_relevel(Housing_Status, "First-Time Homebuyer")) %>% 
  select(Q33a_1_x:Q80_6_x, Housing_Status) %>% 
  pivot_longer(., cols=-Housing_Status, names_to=c("variable")) %>% 
  group_by(Housing_Status, variable) %>% 
  filter(value!="NA") %>% 
  filter(Housing_Status!="Other") %>% 
  mutate(Agree=case_when(
    value>0.5~1,
    TRUE~ 0
  )) %>% 
  left_join(., solution_var_labels) %>% 
  mutate(label=str_remove_all(label, "Support for policy - ")) %>%
  group_by(Housing_Status, variable, label, Agree) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n)*100) %>% 
  filter(Agree==1) %>% 
  ggplot(., aes(y=fct_reorder(label, Percent), x=Percent, fill=Housing_Status))+
  geom_col(position="dodge")+geom_vline(xintercept=50, linetype=2)+labs(y="Support for Policy", x="Percent Agreeing")

##RENTING STATUS
on22 %>% 
  #Change the Housing STatus variable so that First Time Renter is first
  mutate(Renter=fct_relevel(as_factor(Renter), "Renter")) %>% 
  #select what you're working with; in this case the batch of cause variables and housing status
  select(Q33a_1_x:Q80_6_x, Renter) %>% 
  pivot_longer(., cols=-Renter, names_to=c("variable")) %>% 
  group_by(Renter, variable) %>% 
  filter(value!="NA") %>% 
  filter(Renter!="Other") %>% 
  mutate(Agree=case_when(
    value>0.5~1,
    TRUE~ 0)) %>% 
  left_join(., solution_var_labels) %>% 
  mutate(label=str_remove_all(label, "Support for policy - ")) %>%
  group_by(Renter, variable, label, Agree) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n)*100) %>% 
  filter(Agree==1) %>% 
  ggplot(., aes(y=fct_reorder(label, Percent), x=Percent, fill=Renter))+
  geom_col(position="dodge")+geom_vline(xintercept=50, linetype=2)+labs(y="Support for Policy", x="Percent Agreeing")

##PARTISANSHIP
on22 %>% 
  mutate(Renter=fct_relevel(as_factor(partisanship), "Partisanship")) %>% 
  select(Q33a_1_x:Q80_6_x, partisanship) %>% 
  pivot_longer(., cols=-partisanship, names_to=c("variable")) %>% 
  group_by(partisanship, variable) %>% 
  filter(value!="NA") %>% 
  filter(partisanship!="Other") %>% 
  mutate(Agree=case_when(
    value>0.5~1,
    TRUE~ 0)) %>% 
  left_join(., solution_var_labels) %>% 
  mutate(label=str_remove_all(label, "Support for policy - ")) %>%
  group_by(partisanship, variable, label, Agree) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n)*100) %>% 
  filter(Agree==1) %>% 
  ggplot(., aes(y=fct_reorder(label, Percent), x=Percent, fill=partisanship))+
  geom_col(position="dodge")+geom_vline(xintercept=50, linetype=2)+labs(y="Cause", x="Percent Agreeing")

##MIP
