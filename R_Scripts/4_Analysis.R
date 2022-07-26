source("R_Scripts/2_recodes.R")
#Install wlucolrs if necessary
remotes::install_github("sjkiss/wlucolors")
library(wlucolors)
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
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=Housing_Status))+geom_point(position=position_dodge(width = .4)) +xlim(c(0,1))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0, position=position_dodge(width = .4))+
  labs(x="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), col="Housing Situation", y="")

#Causes by Rental Status
on22 %>% 
  mutate(renter=fct_relevel(as_factor(renter), "Renter")) %>% 
  select(Q32_1_x:Q32_9_x, renter) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-renter, names_to=c("variable")) %>% 
  group_by(renter, variable) %>% 
  filter(!is.na(renter)) %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., cause_var_labels) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=as_factor(renter)))+geom_point(position=position_dodge(width = .4))+
  xlim(c(0,1))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0, position=position_dodge(width = .4))+
  labs(x="1=A significant cause\n 0=Not a cause at all", 
       title=str_wrap("Causes of housing cost increase", 
                      width=60), y="", col="Renter/Owner")

#Causes by Partisanship - WIP (need new partianship var)
on22 %>% 
  #Pick the variables working with
  select(Q32_1_x:Q32_9_x, Q23) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Q23,names_to=c("variable")) %>% 
  group_by(Q23, variable) %>% 
  filter(value!="NA") %>% 
  filter(Q23!=5) %>% #Filter out R's that identify as "Other" 
  filter(Q23!=6) %>%# Filter out R's that identify as "None of these"
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., cause_var_labels) %>% 
  ggplot(., aes(y=reorder(label, -average), x=average, col=as.factor(Q23)))+geom_point(position=position_dodge(width = .4))+xlim(c(0,1))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0, position=position_dodge(width = .4))+
  labs(x="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), y="", col="Ideology")
  #scale_x_discrete(labels = c("Underinvestment in \npublic affordable housing","Speculation by investors", "Limited rent control","Excessive foreign \nimmigration to ontario","Urban sprawl","Low interest rates","Municipal red tape","Neighbourhood opposition","Environmental protection"))+
  #scale_color_manual(name="", labels=c("Liberal","New Democrat","Progressive Conservative","Green"), values=c("#D71920","#F37021","#1A4782","#3D9B35"))

names(on22)


#### Solutions ####
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
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=Housing_Status))+geom_point(position=position_dodge(width = .4))+xlim(c(0,1))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0, position=position_dodge(width = .4))+
  labs(x="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Policies to address housing cost increase", width=60), col="Housing Situation", y="")

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
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=renter))+geom_point(position=position_dodge(width = .4))+xlim(c(0,1))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0, position=position_dodge(width = .4))+
  labs(x="1=Strongly Support\n 0=Strongly Oppose", col="Renter/Owner", title=str_wrap("Policies to address housing cost increase", width=60), y="")

#Solutions By Partisanship - WIP
on22 %>% 
  #Pick the variables working with
  select(Q33a_1_x:Q80_6_x, Q23) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Q23, names_to=c("variable")) %>% 
  group_by(Q23, variable) %>% 
  filter(!is.na(value)) %>% 
  filter(Q23!=5) %>% #Filter out R's that identify as "Other" 
  filter(Q23!=6) %>%# Filter out R's that identify as "None of these"
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solutions_var_labels) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=fct_relevel(as_factor(Q23))))+geom_point(position=position_dodge(width = .4))+xlim(c(0,1))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0, position=position_dodge(width = .4))+
  labs(x="1=Strongly Support\n 0=Strongly Oppose", col="Party", title=str_wrap("Support For Policies", width=60), y="Issue")+
  scale_color_manual(values=c("darkblue", "orange", "darkred", "darkgreen"))+theme(legend.position = "bottom")

#Policy tradeoffs by insider/outsider
on22$Q34_1_x
on22 %>% 
  select(Q34_1_x:Q34_5_x) %>% 
  look_for()->tradeoff_var_labels
#Inspect
tradeoff_var_labels
#Note that the variable name is stored in variable and the actual label is stored in label
on22 %>% 
  mutate(Housing_Status=fct_relevel(Housing_Status, "First-Time Homebuyer")) %>% 
  select(Q34_1_x:Q34_5_x, Housing_Status) %>% 
  pivot_longer(., cols=-Housing_Status, names_to=c("variable")) %>% 
  group_by(Housing_Status, variable) %>% 
  filter(value!="NA") %>% 
  filter(Housing_Status!="Other") %>%
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., tradeoff_var_labels) %>% 
  mutate(label=str_remove_all(label, "Trade off support - ")) %>% 
  ggplot(., aes(y=fct_reorder(variable, average), x=average, col=Housing_Status))+geom_point(position=position_dodge(width = .4))+xlim(c(0,1))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0, position=position_dodge(width = .4))+
  labs(x="", title=str_wrap("Trade-off questions", width=60), col="Housing Situation", y="")+
  scale_y_discrete(labels=c("Q34_1_x"= "Increased public investment in affordable housing", "Q34_2_x"="Increased public investment in affordable housing", "Q34_3_x"= "Increased public investment in affordable housing", "Q34_4_x"="Provincial control over local zoning regulations", "Q34_5_x"="Reducing environmental regulations to promote the building of homes"))


