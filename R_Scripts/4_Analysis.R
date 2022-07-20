source("R_Scripts/2_recodes.R")
#### Experiment
lookfor(on22, "social")
theme_set(theme_minimal())
on22 %>% 
  select(ends_with('_exp')) %>% 
  var_label()->experimental_variable_labels
experimental_variable_labels
on22 %>%
rename_with(~ unlist(experimental_variable_labels), ends_with('_exp'))->on22
names(on22)
on22 %>% 
  pivot_longer(., cols="6 Storey rental building":"Semi-detached house", names_to="Development", values_to="Development Support") ->on22
on22 %>% 
  select(Experimental_Group, Development, `Development Support`) %>% 
  group_by(Experimental_Group, Development) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n)) %>% 
ggplot(., aes(x=Average, y=Development, col=Experimental_Group))+
  geom_point()+
  xlim(c(0,1))+
  geom_errorbar(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se))) %>% 
  labs(title="Normative Argumentation And Support For Residential Development")
ggsave(filename="Plots/experiment_averages_point.png", width=8,  height=4)

#### Causes
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

on22$Housing_Status
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
  #Here we mutate and take out the unnecessary `Causes - ` bit.
  mutate(label=str_remove_all(label, "Causes - ")) %>% 
#And graph
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=Housing_Status))+geom_point()+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")

on22$renter
on22 %>% 
  mutate(renter=fct_relevel(renter, "renter")) %>% 
  select(Q32_1_x:Q32_9_x, renter) %>% 
  pivot_longer(., cols=-renter, names_to=c("variable")) %>% 
  group_by(renter, variable) %>% 
  filter(value!="NA") %>% 
  filter(renter!="NA") %>%
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., cause_var_labels) %>% 
  mutate(label=str_remove_all(label, "Causes - ")) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=renter))+geom_point()+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")

on22$Q23
on22 %>% 
  mutate(Q23=fct_relevel(Q23, "Liberal")) %>% 
  select(Q32_1_x:Q32_9_x, Q23) %>% 
  pivot_longer(., cols=-Q23, names_to=c("variable")) %>% 
  group_by(Q23, variable) %>% 
  filter(value!="NA") %>% 
  filter(Q23!=5) %>% #Filter out R's that identify as "Other" 
  filter(Q23!=6) %>%# Filter out R's that identify as "None of these"
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., cause_var_labels) %>% 
  mutate(label=str_remove_all(label, "Causes - ")) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=Q23))+geom_point()+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")

#### Solutions
on22 %>% 
  select(Q33a_1_x:Q80_6_x) %>% 
  look_for()->solution_var_labels
#Inspect
solution_var_labels
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
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=Housing_Status))+geom_point()+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Policies to address housing cost increase", width=60), x="")

#Solutions by renter/non-renter dummy variable
on22 %>% 
  mutate(renter=fct_relevel(renter, "renter")) %>% 
  select(Q33a_1_x:Q80_6_x, renter) %>% 
  pivot_longer(., cols=-renter, names_to=c("variable")) %>% 
  group_by(renter, variable) %>% 
  filter(value!="NA") %>% 
  filter(renter!="NA") %>%
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  mutate(label=str_remove_all(label, "Support for policy - ")) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=renter))+geom_point()+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Policies to address housing cost increase", width=60), x="")

#Solutions by ideology
mutate(Q23=fct_relevel(Q23, "Liberal")) %>% 
  select(Q33a_1_x:Q80_6_x, Q23) %>% 
  pivot_longer(., cols=-Q23, names_to=c("variable")) %>% 
  group_by(Q23, variable) %>% 
  filter(value!="NA") %>% 
  filter(Q23!=5) %>% #Filter out R's that identify as "Other" 
  filter(Q23!=6) %>%# Filter out R's that identify as "None of these"
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  mutate(label=str_remove_all(label, "Support for policy - ")) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=Q23))+geom_point()+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Policies to address housing cost increase", width=60), x="")
