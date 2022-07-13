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
#Get variable labels

on22 %>% 
  #Pick the variables working with
  mutate(Housing_Status=fct_relevel(Housing_Status)) %>% 
  select(Q32_1_x:Q32_9_x, Housing_Status) %>% 
  #This function takes the varibles names of the data propvided and turns them into the variable nmes
  #It makes it unnecessary to use scale_discrete_with the labels as you did
  #The problem with your approach is that if we change the order from descending to ascending then the labels' order
  # doesn't actually change
  #But the variable labels we are working with actually ahave to have variable names
  #So I had to go into the 2_recodes scxript and add a section copy and pasted and modified from the variabvle labels
  #script to set the variable labels
  sjlabelled::label_to_colnames() %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Housing_Status) %>% 
group_by(Housing_Status, name) %>% 
  filter(value!="NA") %>% 
  filter(Housing_Status!="Other") %>%
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=fct_reorder(name, average), y=average, col=Housing_Status))+geom_point()+ylim(c(0,1))+
  geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)+coord_flip() +
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=60), x="")

on22 %>% 
  #Pick the variables working with
  select(Q32_1_x:Q32_9_x, renter) %>% 
  sjlabelled::label_to_colnames() %>% 
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

#Table of Descriptives for Housing Status
kbl(tabyl(on22$Housing_Status, sort= TRUE)) %>% 
  kable_styling()

?tabyl

  kbl(tabyl(on22,Housing_Status, sort= TRUE)) %>% 
  kable_styling()
