source("R_Scripts/2_recodes.R")
#Install wlucolrs if necessary
#remotes::install_github("sjkiss/wlucolors")
library(wlucolors)
theme_set(theme_classic(base_size=16))

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

#Raw Cause Scores

on22 %>% 
  select(Q32_1_x:Q32_9_x) %>% 
  pivot_longer(cols=everything(),names_to="variable") %>% 
  left_join(cause_var_labels, by="variable") %>% 
  group_by(label) %>% 
  summarize(Average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(),se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=Average, y=fct_reorder(label, Average)))+geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)))+
  labs(x="Score (0=Not at all a Cause, 1=A significant Cause)", y="Cause")+
  xlim(c(0,1))+geom_vline(xintercept=0.5, linetype=2)
ggsave(filename=here("Plots", "causes_house_price_increase.png"), width=10, height=4)
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
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=100), x="")+geom_hline(yintercept=0.5, linetype=2)

#Save the above plot out into the "PLots" Subdirectory
#Inspect the graph above to try to discern what it is about and provide a meaningful filename
#No spaces
#Feel free to fiddle with the dimensions (they are in in ches) to get the best graph
ggsave(filename=here("Plots", "causes_by_housing_status.png"), width=10, height=4)

on22 %>% 
  mutate(Renter=Renter) %>% 
  select(Q32_1_x:Q32_9_x, Renter) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Renter, names_to=c("variable")) %>% 
  group_by(Renter, variable) %>% 
  filter(!is.na(Renter)) %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., cause_var_labels) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=Renter))+ylim(c(0,1))+
  geom_pointrange(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), position=position_jitter(width=0.25))+coord_flip()+
  labs(y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=100), x="")+geom_hline(yintercept=0.5, linetype=2)
#scale_x_discrete(labels = c("Underinvestment in \npublic affordable housing","Speculation by investors",  "Limited rent control","Excessive foreign \nimmigration to ontario","Urban sprawl","Low interest rates","Municipal red tape","Neighbourhood opposition","Environmental protection"))+
ggsave(filename=here("Plots", "causes_by_Renter_dichotomous.png"), width=10, height=4)

#Causes by Partisanship
on22 %>% 
  #mutate(Renter=fct_relevel(as_factor(partisanship), "Partisanship")) %>% 
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
  scale_color_manual(values=c("blue", "orange", "darkred", "darkgreen", "black"))+
  labs(color="Partisanship",y="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=100), x="")+geom_hline(yintercept=0.5, linetype=2)
ggsave(filename=here("Plots", "causes_by_provincial_partisanship.png"), width=10, height=4)

names(on22)

#### Solutions ####

#Raw Solutions Scores

on22 %>% 
  select(Q33a_1_x:Q80_6_x) %>% 
  look_for()->solution_var_labels
#Inspect
solution_var_labels$label<-str_remove_all(solution_var_labels$label, "Support for policy - ")
solution_var_labels
#Note that the variable name is stored in variable and the actual label is stored in label

on22 %>% 
  select(Q33a_1_x:Q80_6_x) %>% 
  pivot_longer(cols=everything(),names_to="variable") %>% 
  left_join(solution_var_labels, by="variable") %>% 
  group_by(label) %>% 
  summarize(Average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(),se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=Average, y=fct_reorder(label, Average)))+geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)))+
  labs(x="Score (0=Strongly Oppose, 1=Strongly Support)", title=str_wrap("Solutions To Address Housing Price Increase",60), y="Solution")+xlim(c(0,1))+geom_vline(xintercept=0.5, linetype=2)
ggsave(filename=here("Plots", "solution_house_price_increase.png"), width=10, height=4)
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
 # scale_color_manual(values=c("blue", "orange", "darkred", "darkgreen", "black"))+
  labs(x="Score (0=Strongly Oppose, 1=Strongly Support)", title=str_wrap("Solutions To Address Housing Price Increase",60), y="Solution")  geom_hline(yintercept=0.5, linetype=2)
ggsave(filename=here("Plots", "solutions_by_housing_status.png"), width=10, height=4)

#Solutions by Renter/non-Renter dummy variable
on22 %>% 
  select(Q33a_1_x:Q80_6_x, Renter) %>% 
  pivot_longer(., cols=-Renter, names_to=c("variable")) %>% 
  group_by(Renter, variable) %>% 
  filter(value!="NA") %>% 
  filter(Renter!="NA") %>%
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  mutate(label=str_remove_all(label, "Support for policy - ")) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=Renter))+ylim(c(0,1))+
  geom_pointrange(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), position=position_jitter(width=0.25))+coord_flip()+
  labs(x="Score (0=Strongly Oppose, 1=Strongly Support)", title=str_wrap("Solutions To Address Housing Price Increase",60), y="Solution")
ggsave(filename=here("Plots", "solutions_by_Renter_dichotomous.png"), width=10, height=4)

#Solutiosn By Partisanship
on22 %>% 
  #Pick the variables working with
  select(Q33a_1_x:Q80_6_x, partisanship) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-partisanship, names_to=c("variable")) %>% 
  group_by(partisanship, variable) %>% 
  filter(!is.na(value)) %>% 
  filter(partisanship!="Green") %>% 
  # filter(partisanship!=5) %>% #Filter out R's that identify as "Other" 
  # filter(partisanship!=6) %>%# Filter out R's that identify as "None of these"
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  ggplot(., aes(x=fct_reorder(label, average), y=average, col=partisanship))+ylim(c(0,1))+
  geom_pointrange(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)))+coord_flip()+
  scale_color_manual(values=c("blue", "orange", "darkred", "darkgreen", "black"))+
  labs(x="Score (0=Strongly Oppose, 1=Strongly Support)", title=str_wrap("Solutions To Address Housing Price Increase",60), y="Solution")+geom_hline(yintercept=0.5, linetype=2)
ggsave(filename=here("Plots", "solutions_by_provincial_partisanship.png"), width=10, height=4)

on22 %>% 
  #Pick the variables working with
  select(Q33a_1_x:Q80_6_x, Density) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Density, names_to=c("variable")) %>% 
group_by(Density, variable) %>% 
  summarize(Average=mean(value, na.rm=T), n=n(), sd=sd(value, na.rm=T), se=sd/sqrt(n)) %>% 
left_join(., solution_var_labels) %>% 
mutate(label=str_remove_all(label, "Support for policy - ")) %>% 
  filter(!is.na(Density)) %>% 
  ggplot(., aes(x=Average, y=fct_reorder(label, Average), col=Density))+
  geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)))+
  labs(x="Score (0=Strongly Oppose, 1=Strongly Support)", title=str_wrap("Solutions To Address Housing Price Increase",60), y="Solution")+  
  geom_vline(xintercept=0.5, linetype=2)+
  xlim(c(0,1))

ggsave(filename=here("Plots", "solutions_by_density.png"), width=10, height=4)

# 
# on22 %>% 
#   #Pick the variables working with
#   select(Q33a_1_x:Q80_6_x, Density) %>% 
#   #pivot them longer, except for the Sample variable
#   pivot_longer(., cols=-c(Density), names_to=c("variable")) %>% 
#   group_by(Density, variable) %>% 
#   summarize(Average=mean(value, na.rm=T), n=n(), sd=sd(value, na.rm=T), se=sd/sqrt(n)) %>% 
#   left_join(., solution_var_labels) %>% 
#   mutate(label=str_remove_all(label, "Support for policy - ")) %>% 
#   #filter(Housing_Status!="Other") %>% 
#   filter(!is.na(Density)) %>% 
#   ggplot(., aes(x=Average, y=fct_reorder(label, Average), col=Density))+
#  # facet_grid(~Density)+
#   geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), size=0.5)
# ggsave(filename=here("Plots", "solutions_by_density.png"), width=10, height=4)
# 
# 
solution_var_labels
on22 %>% 
  select(solution_var_labels$variable, cognitive_non_partisan) %>% 
  pivot_longer(., -cognitive_non_partisan, names_to=c("variable")) %>%  
  left_join(., solution_var_labels) %>% 
  group_by(label, cognitive_non_partisan) %>%  
  summarize(Average=mean(value, na.rm=T), n=n(), sd=sd(value, na.rm=T), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=Average, y=fct_reorder(label, Average), col=cognitive_non_partisan))+geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)))

on22 %>% 
  select(Q34_1_x:Q34_5_x) %>% 
  look_for()->trade_off_var_labels

#Get rid of the Trade off support now
trade_off_var_labels$label<-str_remove_all(trade_off_var_labels$label, "Trade off support - ")
## Raw Trade-Offs

trade_off_var_labels
on22 %>% 
  select(Q34_1_x:Q34_5_x) %>% 
  pivot_longer(., cols=everything(), names_to="variable", values_to=c("Support"))  %>% 
group_by(variable) %>% 
  summarize(Average=mean(Support, na.rm=T), n=n(), sd=sd(Support, na.rm=T), se=sd/sqrt(n)) %>% 
   left_join(., trade_off_var_labels) %>% 
  ggplot(., aes(x=Average, y=label)) +
  geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)))+
  labs(x="Score", y="Trade-Off")

on22 %>% 
  mutate(Housing_Status=fct_relevel(Housing_Status, "First-Time Homebuyer")) %>% 
  select(Q34_1_x:Q34_5_x, Housing_Status) %>% 
  pivot_longer(., cols=-Housing_Status, names_to=c("variable")) %>% 
  group_by(Housing_Status, variable) %>% 
  filter(value!="NA") %>% 
  filter(Housing_Status!="Other") %>%
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., trade_off_var_labels) %>% 
  #mutate(label=str_remove_all(label, "Trade off support - ")) %>% 
  ggplot(., aes(y=fct_reorder(variable, average), x=average, col=Housing_Status))+
  geom_pointrange(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)))+
  labs(x="Support for Trade off (0 = Pro-Housing Choice 1=Anti-Housing Choice ", title=str_wrap("Trade-off questions", width=60), col="Housing Situation", y="")+xlim(c(0,1))
#####This line is now superfluous! This is the cool way that you had come up with to add meaningflu 
# variable labels. But I kinda preferred the way of pulling the variable labels
#into the objects solution_var_labels, trade_off_var_labels and then merging them. We do 
#That now in the lines above. 
 # scale_y_discrete(labels=c("Q34_1_x"= "Increased public investment in affordable housing", "Q34_2_x"="Increased public investment in affordable housing", "Q34_3_x"= "Increased public investment in affordable housing", "Q34_4_x"="Provincial control over local zoning regulations", "Q34_5_x"="Reducing environmental regulations to promote the building of homes"))



  
