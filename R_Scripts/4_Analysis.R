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

on22 %>% 
  #Pick the variables working with
  select(Q32_1_x:Q32_9_x, renter) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-renter) %>% 
  group_by(renter, name) %>% 
  filter(!is.na(renter)) %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) +
  ggplot(., aes(x=fct_reorder(name, average), y=average, col=as.factor(renter)))+geom_point()+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")+
  scale_color_discrete(name="", labels=c("Non-renter","Renter"))
  #scale_x_discrete(labels = c("Underinvestment in \npublic affordable housing","Speculation by investors",  "Limited rent control","Excessive foreign \nimmigration to ontario","Urban sprawl","Low interest rates","Municipal red tape","Neighbourhood opposition","Environmental protection"))+


#Causes by self-identified ideology
on22 %>% 
  #Pick the variables working with
  select(Q32_1_x:Q32_9_x, Q23) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Q23) %>% 
  group_by(Q23, name) %>% 
  filter(value!="NA") %>% 
  filter(Q23!=5) %>% #Filter out R's that identify as "Other" 
  filter(Q23!=6) %>%# Filter out R's that identify as "None of these"
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=reorder(name, -average), y=average, col=as.factor(Q23)))+geom_point()+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")+
  scale_x_discrete(labels = c("Underinvestment in \npublic affordable housing","Speculation by investors", "Limited rent control","Excessive foreign \nimmigration to ontario","Urban sprawl","Low interest rates","Municipal red tape","Neighbourhood opposition","Environmental protection"))+
  scale_color_manual(name="", labels=c("Liberal","New Democrat","Progressive Conservative","Green"), values=c("#D71920","#F37021","#1A4782","#3D9B35"))


#Solutions by insider/outsider
on22 %>% 
  #Pick the variables working with
  mutate(Housing_Status=fct_relevel(Housing_Status)) %>% 
  select(Q33a_1_x:Q80_6_x, Housing_Status) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Housing_Status) %>% 
  group_by(Housing_Status, name) %>% 
  filter(value!="NA") %>% 
  filter(Housing_Status!="Other") %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=reorder(name,-average), y=average, col=Housing_Status))+geom_point()+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=Strongly support\n 0=Strongly oppose", title=str_wrap("Solutions to address housing affordability", width=60), x="")+
  scale_x_discrete(labels = c("Increase the non-resident \n speculation tax on foreign buyers", "Expand rent control","Increased public investment\n in affordable housing","Increasing the supply of housing  by 1.5 \nmillion new homes in the next 10 years","Eliminate the land transfer \n tax on home sales","Require developers to build  1 affordable\n home for every 5 new houses or condos","Establish government loans to help\n new home buyers afford a down payment", "Tax on vacant and second homes", "Eliminate density and height \nrestrictions close to transit stations","Abolish municipal rules that \n allow single family homes","Make it easier for individual property\n owners to add housing units", "Weaken heritage designation rules"))

#Solutions by renter/non-renter dummy variable
on22 %>% 
  #Pick the variables working with
  select(Q33a_1_x:Q80_6_x, renter) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-renter) %>% 
  group_by(renter, name) %>% 
  filter(value!="NA") %>% 
  filter(renter!="NA") %>%
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=reorder(name, -average), y=average, col=as.factor(renter)))+geom_point(position=position_dodge(0.2))+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0, position=position_dodge(0.2))+coord_flip() +
  labs(y="1=Strongly support\n 0=Strongly oppose", title=str_wrap("Solutions to address housing affordability", width=60), x="")+
  scale_x_discrete(labels =c("Increase the non-resident \n speculation tax on foreign buyers", "Expand rent control","Increased public investment\n in affordable housing","Require developers to build 1 affordable\n home for every 5 new houses or condos","Increasing the supply of housing by 1.5 \nmillion new homes in the next 10 years","Eliminate the land transfer \n tax on home sales","Establish government loans to help\n new home buyers afford a down payment", "Tax on vacant and second homes", "Abolish municipal rules that \n allow single family homes","Make it easier for individual \n property owners to add housing units","Eliminate density and height \nrestrictions close to transit stations", "Weaken heritage designation rules"))+
  scale_color_discrete(name="", labels=c("Non-renter","Renter"))

on22 %>% 
  #Pick the variables working with
  select(Q33a_1_x:Q80_6_x, Q23) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Q23) %>% 
  group_by(Q23, name) %>% 
  filter(value!="NA") %>% 
  filter(Q23!=5) %>% #Filter out R's that identify as "Other" 
  filter(Q23!=6) %>%# Filter out R's that identify as "None of these"
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=reorder(name, -average), y=average, col=as.factor(Q23)))+geom_point(position=position_dodge(0.25))+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0, position=position_dodge(0.25))+coord_flip() +
  labs(y="1=Strongly support\n 0=Strongly oppose", title=str_wrap("Solutions to address housing affordability", width=60), x="")+
  scale_x_discrete(labels = c("Increase the non-resident \n speculation tax on foreign buyers", "Expand rent control","Increased public investment\n in affordable housing","Require developers to build 1 affordable\n home for every 5 new houses or condos","Increasing the supply of housing by 1.5\n million new homes in the next 10 years", "Tax on vacant and second homes","Eliminate the land transfer \n tax on home sales","Establish government loans to help\n new home buyers afford a down payment", "Make it easier for individual \n property owners to add housing units","Eliminate density and height\n restrictions close to transit stations","Abolish municipal rules that \n allow single family homes", "Weaken heritage designation rules"))+
  scale_color_manual(name="", labels=c("Liberal","New Democrat","Progressive Conservative","Green"), values=c("#D71920","#F37021","#1A4782","#3D9B35"))
