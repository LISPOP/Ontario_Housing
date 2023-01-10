# source("R_Scripts/2_recodes.R")
source("R_Scripts/3_diagnostics.R")
#source("R_Scripts/1_data_import.R")
names(on22)
#Install wlucolrs if necessary
#remotes::install_github("sjkiss/wlucolors")
library(wlucolors)
theme_set(theme_classic(base_size=24))

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
on22$Housing_Status<-factor(on22$Housing_Status, levels=c("Seeking to purchase", 
                                                          "Not seeking to purchase", 
                                                          "Homeowner", 
                                                          "Other"))
#on22$Housing_Status<-Recode(on22$Housing_Status, "'First-Time Homebuyer'='Renter seeking to purchase';
#'Satisfied Homeowner'='Homeowner';
#'Satisfied Renter'='Renter not seeking to purchase'", 
#       levels=c("Homeowner", "Renter not seeking to purchase", "Renter seeking to purchase"))
#Raw Cause Scores

on22 %>% 
  select(Q32_1_x:Q32_9_x) %>% 
  pivot_longer(cols=everything(),names_to="variable") %>% 
  left_join(cause_var_labels, by="variable") %>% 
  group_by(label) %>% 
  summarize(Average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(),se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=Average, y=fct_reorder(label, Average)))+
  geom_pointrange(size=2,aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)))+
  #scale_x_continuous(breaks=c("0", "0.25", "0.5", "0.75", "1"))+
  scale_y_discrete(labels=function(x) str_wrap(x, width=20))+
  labs(x="0=Not at all a Cause\n1=A significant Cause", y="", title=str_wrap("Causes of House, Rent Price Increases", 25))+
  xlim(c(0,1))+geom_vline(xintercept=0.5, linetype=2)+
  theme(axis.text.y=element_text(size=20))

ggsave(filename=here("Plots", "causes_house_price_increase.png"), width=8, height=8, dpi=300)
   #Now the graph
on22 %>% 
  #Change the Housing STatus variable so that First Time Homebuyers is first
  #mutate(Housing_Status2=fct_relevel(Housing_Status, "First-Time Homebuyer")) %>% 
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
  filter(Housing_Status!="Speculator") %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  #this is the merge
  #It joins what came above in the pipe with the cause_var_labels object
  #Because both have variables `variable` It automatically merges on that variable
  left_join(., cause_var_labels) %>% 
#And graph
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=Housing_Status))+xlim(c(0,1))+
  geom_pointrange(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), position=position_jitter(height=0.25), size = 1)+
  labs(x="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=100), y="")+
  scale_color_brewer(palette="Dark2")+
  geom_vline(xintercept=0.5, linetype=2)+
  guides(col=guide_legend(nrow=3))+
  theme(legend.position = "bottom")+
scale_y_discrete(labels=function(x) str_wrap(x, width=20))
ggsave(filename=here("Plots", "causes_house_price_increase_by_status.png"), width=10, height=10, dpi=300)


#Save the above plot out into the "PLots" Subdirectory
#Inspect the graph above to try to discern what it is about and provide a meaningful filename
#No spaces
#Feel free to fiddle with the dimensions (they are in in ches) to get the best graph
ggsave(filename=here("Plots", "causes_by_housing_status.png"), width=10, height=4)


#### By Community Size
levels(on22$Size)<-c("Rural", "Small", "Medium", "Large", "Toronto/Ottawa")
on22 %>% 
   select(Q32_1_x:Q32_9_x,Size) %>% 
   pivot_longer(., cols=-Size, names_to=c("variable")) %>%
  filter(!is.na(Size)) %>% 
  group_by(Size, variable) %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
   left_join(., cause_var_labels) %>% 
  #And graph
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=Size))+
  xlim(c(0,1))+
  scale_y_discrete(labels=function(x) str_wrap(x, width=20))+
  geom_pointrange(size=1.1,aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), position=position_jitter(height=0.25))+
  labs(x="1=A significant cause\n 0=Not a cause at all", 
       title=str_wrap("Causes of housing cost increase", width=100), 
       y="")+
  geom_vline(xintercept=0.5, linetype=2)+
  guides(col=guide_legend(nrow=2))+
  theme(legend.position = "bottom")+
  scale_color_brewer(palette="Dark2")
 ggsave(filename=here("Plots/cause_by_community_size.png"), width=10, height=8)

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
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=partisanship))+
  xlim(c(0,1))+
  geom_pointrange(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), position=position_jitter(height=0.25))+
  scale_color_manual(values=c("blue", "orange", "darkred", "darkgreen", "black"))+
  labs(color="Partisanship",y="",x="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=100), x="")+
  geom_vline(xintercept=0.5, linetype=2)
ggsave(filename=here("Plots", "causes_by_provincial_partisanship.png"), width=10, height=4)

#Causes by Vote
on22 %>% 
  #mutate(Renter=fct_relevel(as_factor(partisanship), "Partisanship")) %>% 
  #Pick the variables working with
  select(Q32_1_x:Q32_9_x, Vote_Intention_Likely) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Vote_Intention_Likely,names_to=c("variable")) %>% 
  group_by(Vote_Intention_Likely, variable) %>% 
  filter(Vote_Intention_Likely!="Green") %>% 
  filter(Vote_Intention_Likely!="NA") %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., cause_var_labels) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=Vote_Intention_Likely))+
  xlim(c(0,1))+
  geom_pointrange(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), size=1.2,position=position_jitter(height=0.25))+
  scale_color_manual(values=c("blue", "darkred", "orange"))+
  scale_y_discrete(labels=function(x) str_wrap(x, 10)) +
  labs(color="Vote",x="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=100), y="")+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position = "bottom")+guides(col=guide_legend(nrow=2))
ggsave(filename=here("Plots", "causes_by_provincial_vote.png"), width=10, height=10)

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
  ggplot(., aes(x=Average, y=fct_reorder(label, Average)))+geom_pointrange(size=1.2,aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)))+
  labs(x="Score (0=Strongly Oppose\n1=Strongly Support)", 
       title=str_wrap("Solutions To Address Housing Price Increase",30), y="")+
  xlim(c(0,1))+
  scale_y_discrete(labels=function(x) str_wrap(x, 20))+
  geom_vline(xintercept=0.5, linetype=2)

ggsave(filename=here("Plots", "solution_house_price_increase.png"), width=12, height=10)
#Solutions by Community Size
on22 %>% 
  select(Q33a_1_x:Q80_6_x,Size) %>% 
  pivot_longer(., cols=-Size, names_to=c("variable")) %>%
  filter(!is.na(Size)) %>% 
  group_by(Size, variable) %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  #And graph
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=Size))+xlim(c(0,1))+
  geom_pointrange(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), position=position_jitter(height=0.25))+
  labs(x="Score (0=Strongly Oppose, 1=Strongly Support)", title=str_wrap("Solutions To Address Housing Price Increase",60), y="Solution")+xlim(c(0,1))+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position="bottom")+
  guides(col=guide_legend(nrow=2))
ggsave(filename=here("Plots/cause_by_community_size.png"))


#Solutions by insider/outsider
on22 %>% 
  #mutate(Housing_Status=fct_relevel(Housing_Status, "First-Time Homebuyer")) %>% 
  select(Q33a_1_x:Q80_6_x, Housing_Status) %>% 
  pivot_longer(., cols=-Housing_Status, names_to=c("variable")) %>% 
  group_by(Housing_Status, variable) %>% 
  filter(value!="NA") %>% 
  filter(Housing_Status!="Other") %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  mutate(label=str_remove_all(label, "Support for policy - ")) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=Housing_Status))+
  xlim(c(0,1))+
  geom_pointrange(size=1,aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), position=position_jitter(height=0.25), size = 1)+
  scale_color_brewer(palette="Dark2")+
 # scale_color_manual(values=c("blue", "orange", "darkred", "darkgreen", "black"))+
  labs(x="Score (0=Strongly Oppose, 1=Strongly Support)", 
       title="", 
       y="")+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position="bottom")+
  guides(col=guide_legend(ncol=1))+
  scale_y_discrete(labels=function(x) str_wrap(x, width=30))
ggsave(filename=here("Plots", "solutions_by_housing_status.png"), width=12, height=12)

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
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=partisanship))+
  xlim(c(0,1))+geom_point()+
 geom_pointrange(size=1,aes(xmin=average-(1.96*se),
                            xmax=average+(1.96*se)))+
 scale_color_manual(values=c("blue", "orange", "darkred",  "black"))+
  labs(x=str_wrap("Score (0=Strongly Oppose, 1=Strongly Support)", 30), 
       y="")+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position="bottom")+guides(col=guide_legend(ncol=2))+
scale_y_discrete(labels=function(x) str_wrap(x, width=30))
ggsave(filename=here("Plots", "solutions_by_provincial_partisanship.png"), width=14, height=12)


#Solutiosn By Vote
on22 %>% 
  #Pick the variables working with
  select(Q33a_1_x:Q80_6_x, Vote_Intention_Likely) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-Vote_Intention_Likely, names_to=c("variable")) %>% 
  group_by(Vote_Intention_Likely, variable) %>% 
  filter(!is.na(value)) %>% 
  filter(Vote_Intention_Likely!="Green") %>% 
  # filter(partisanship!=5) %>% #Filter out R's that identify as "Other" 
  # filter(partisanship!=6) %>%# Filter out R's that identify as "None of these"
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=Vote_Intention_Likely))+
  xlim(c(0,1))+
  geom_pointrange(size=1,aes(xmin=average-(1.96*se),
                             xmax=average+(1.96*se)),position=position_jitter(height=0.25))+
  scale_color_manual(values=c("blue",  "darkred", "orange"))+
  labs(x=str_wrap("Score (0=Strongly Oppose, 1=Strongly Support)", 30), 
       y="")+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position="bottom")+
  guides(col=guide_legend(ncol=2))+
  scale_y_discrete(labels=function(x) str_wrap(x, width=30))
ggsave(filename=here("Plots", "solutions_by_provincial_vote.png"), width=14, height=12)

### Solutions by Population Size

on22 %>% 
  select(Q33a_1_x:Q80_6_x,Size) %>% 
  pivot_longer(., cols=-Size, names_to=c("variable")) %>%
  filter(!is.na(Size)) %>% 
  group_by(Size, variable) %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  #And graph
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=Size))+
  xlim(c(0,1))+
  scale_y_discrete(labels=function(x) str_wrap(x, width=30))+
  geom_pointrange(size=1.1,aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), position=position_jitter(height=0.25))+
  labs(x="1=A significant cause\n 0=Not a cause at all", 
       title=str_wrap("Solutions to housing cost increase", width=100), 
       y="")+
  geom_vline(xintercept=0.5, linetype=2)+
  guides(col=guide_legend(nrow=2))+
  theme(legend.position = "bottom")
ggsave(filename=here("Plots", "solutions_by_size.png"), width=15, height=12)


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
## Raw Trade-Offs

trade_off_var_labels$label<-str_replace_all(trade_off_var_labels$label, " - ", "\n")

on22 %>% 
  select(Q34_1_x:Q34_5_x) %>% 
  pivot_longer(., cols=everything(), names_to="variable", values_to=c("Support"))  %>% 
group_by(variable) %>% 
  summarize(average=mean(Support, na.rm=T), n=n(), sd=sd(Support, na.rm=T), se=sd/sqrt(n)) %>% 
   left_join(., trade_off_var_labels) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average))+
  geom_pointrange(size=1.2,aes(xmin=average-(1.96*se), xmax=average+(1.96*se)))+
  labs(x="0 = Anti-Housing Choice\n1=Pro-Housing Choice", 
       title="",
       col="Vote Intention", y="")+
  xlim(c(0,1))+
  theme(legend.position="bottom")+
  guides(col=guide_legend(nrow=2, ncol=2))+
  geom_vline(xintercept=0.5, linetype=2)+
  scale_y_discrete(labels=trade_off_var_labels$label)
ggsave(here("Plots", "trade_offs_raw.png"), width=12, height=8)

#Trade-Offs By Housing Status
on22 %>% 
  #mutate(Housing_Status2=fct_relevel(Housing_Status2, "First-Time Homebuyer")) %>% 
  select(Q34_1_x:Q34_5_x, Housing_Status2) %>% 
  pivot_longer(., cols=-Housing_Status2, names_to=c("variable")) %>% 
  group_by(Housing_Status2, variable) %>% 
  filter(value!="NA") %>% 
  filter(Housing_Status2!="Other") %>%
  filter(Housing_Status2!="Speculator") %>% 
 summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., trade_off_var_labels) %>% 
  #mutate(label=str_remove_all(label, "Trade off support - ")) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=Housing_Status2))+
  geom_pointrange(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)))+
  scale_color_brewer(palette="Dark2")+
  labs(x="0 = Anti-Housing Choice\n1=Pro-Housing Choice", 
       title="",
       col="Vote Intention", y="")+
  xlim(c(0,1))+
  geom_vline(xintercept=0.5, linetype=2)+
  scale_y_discrete(labels=trade_off_var_labels$label)
#Trade-Offs by Vote
on22 %>% 
 # mutate(Housing_Status=fct_relevel(Housing_Status, "First-Time Homebuyer")) %>% 
  select(Q34_1_x:Q34_5_x, Vote_Intention_Likely) %>% 
  pivot_longer(., cols=-Vote_Intention_Likely, names_to=c("variable")) %>% 
  group_by(Vote_Intention_Likely, variable) %>% 
  filter(value!="NA") %>% 
  filter(Vote_Intention_Likely!="Green") %>%
  #filter(Housing_Status!="Speculator") %>% 
  summarize(average=mean(value), sd=sd(value), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., trade_off_var_labels) %>% 
  #mutate(label=str_remove_all(label, "Trade off support - ")) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=Vote_Intention_Likely))+
  geom_pointrange(size=1.2,aes(xmin=average-(1.96*se), xmax=average+(1.96*se)))+
  scale_color_manual(values=c("darkblue", "darkred", "orange"))+
  labs(x="0 = Anti-Housing Choice\n1=Pro-Housing Choice", 
       title="",
       col="Vote Intention", y="")+
  xlim(c(0,1))+theme(legend.position="bottom")+
  guides(col=guide_legend(nrow=2, ncol=2))
ggsave(here("Plots", "trade_off_vote.png"), width=10, height=8)

# on22 %>% 
#   select(Vote_Intention_Likely, matches("^Q34_[0-9]$")) %>% 
#   pivot_longer(-Vote_Intention_Likely) %>% 
#   group_by(Vote_Intention_Likely, name) %>% 
#   summarize(avg=mean(value, na.rm=T)) %>% 
#   arrange(name) %>% 
#   View()
#### Causes by Density
on22$CSDTYPE

table(on22$CSDTYPE)
  on22 %>% 
    ggplot(., aes(x=pop_2021))+geom_histogram()+facet_grid(~CSDTYPE)
table(on22$CSDNAME)
on22 %>% 
  select(cause_var_labels$variable, pop_density) %>% 
  pivot_longer(., -pop_density, names_to=c("variable")) %>% 
  left_join(., cause_var_labels) %>% 
  ggplot(., aes(x=log(pop_density), y=value))+geom_point()+
  facet_wrap(~label, scales="free_y")+geom_smooth(method="lm")
#  ggplot(., aes(x=pop_density))+geom_histogram()

  on22 %>% 
    select(solution_var_labels$variable, pop_density) %>% 
    pivot_longer(., -pop_density, names_to=c("variable")) %>% 
    left_join(., solution_var_labels) %>% 
    ggplot(., aes(x=log(pop_density), y=value))+geom_point()+
    facet_wrap(~label, scales="free_y")+geom_smooth(method="lm")
 # ggplot(., aes(x=pop_density))+geom_histogram()
  
#Cause by Ownership Costs
cause_var_labels
on22 %>% 
select(cause_var_labels$variable, median_monthly_mortgage, median_monthly_rent) %>% 
  pivot_longer(., -c(median_monthly_mortgage, median_monthly_rent)) %>% 
  pivot_longer(., c(median_monthly_mortgage, median_monthly_rent), 
               names_to = c("Payment"), values_to=c("Cost")) %>% 
  left_join(., cause_var_labels, by=c("name"="variable")) %>% 
  ggplot(., aes(x=Cost, y=value,  col=Payment))+
  geom_point(size=0.25) +facet_wrap(~label, scales="free_x")+geom_smooth(method="lm")

on22 %>% 
  select(solution_var_labels$variable, median_monthly_mortgage, median_monthly_rent) %>% 
  pivot_longer(., -c(median_monthly_mortgage, median_monthly_rent)) %>% 
  pivot_longer(., c(median_monthly_mortgage, median_monthly_rent), 
               names_to = c("Payment"), values_to=c("Cost")) %>% 
  left_join(., solution_var_labels, by=c("name"="variable")) %>% 
  ggplot(., aes(x=Cost, y=value,  col=Payment))+
  geom_point(size=0.25) +facet_wrap(~label, scales="free_x")+geom_smooth(method="lm")

cause_var_labels
on22 %>% 
  select(cause_var_labels$variable, income_digits) %>% 
  pivot_longer(., -c(income_digits)) %>% 
  #pivot_longer(., c(median_monthly_mortgage, median_monthly_rent), 
        #       names_to = c("Payment"), values_to=c("Cost")) %>% 
  left_join(., cause_var_labels, by=c("name"="variable")) %>% 
  ggplot(., aes(x=income_digits, y=value))+
  geom_point(size=0.25) +facet_wrap(~label, scales="free_x")+geom_smooth(method="lm")

cause_var_labels
on22 %>% 
  select(solution_var_labels$variable, income_digits) %>% 
  pivot_longer(., -c(income_digits)) %>% 
  # pivot_longer(., c(median_monthly_mortgage, median_monthly_rent), 
  #              names_to = c("Payment"), values_to=c("Cost")) %>% 
  left_join(., solution_var_labels, by=c("name"="variable")) %>% 
  ggplot(., aes(x=income_digits, y=value))+
  geom_point(size=0.25) +facet_wrap(~label, scales="free_x")+
  geom_smooth(method="lm")

##MIP Charts
#Causes
on22 %>% 
  select(Q32_1_x:Q32_9_x, MIP_Cost_Housing) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-MIP_Cost_Housing,names_to=c("variable")) %>% 
  group_by(MIP_Cost_Housing, variable) %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., cause_var_labels) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=MIP_Cost_Housing))+
  xlim(c(0,1))+
  geom_pointrange(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), position=position_jitter(height=0.25), size = .75)+
  scale_color_brewer(palette="Dark2")+
  labs(color="Most Important Problem",y="",x="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=100), x="")+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position = "bottom")+
  guides(col=guide_legend(nrow=3, ncol=3))
ggsave(filename=here("Plots", "causes_by_MIP.png"), width=11, height=6)

#Solutions
on22 %>% 
  select(Q33a_1_x:Q80_6_x,MIP_Cost_Housing) %>% 
  pivot_longer(., cols=-MIP_Cost_Housing, names_to=c("variable")) %>%
  group_by(MIP_Cost_Housing, variable) %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=MIP_Cost_Housing))+
  xlim(c(0,1))+
  geom_pointrange(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), position=position_jitter(height=0.25), size=.75)+
  scale_y_discrete(labels=function(x) str_wrap(x, width=30))+
  scale_color_brewer(palette="Dark2")+
  labs(color="Most Important Problem", y="", x="Score (0=Strongly Oppose,\n 1=Strongly Support)", title=str_wrap("Solutions To Address Housing",60))+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position="bottom")+
  guides(col=guide_legend(nrow=3, ncol=3))
ggsave(filename=here("Plots", "solutions_by_MIP.png"), width=10, height=10)

###Spending preferences by vote intention

on22 %>% 
  select(Q16:Q21) %>% 
  var_label()

  on22 %>% 
    #Select what you are looking to work with
    #In this case it is the batch of rescaled cause variables
    select(Q16:Q21) %>% 
    #Use the command look_for() in the labelled library, must be loaded!
    #Store in something meaningful
    look_for()->spending_var_labels
  spending_var_labels
    spending_var_labels$label<-str_remove_all(spending_var_labels$label, "Spending - ")

on22 %>% 
  select(Q16:Q21, Vote_Intention_Likely) %>% 
  pivot_longer(., cols=-Vote_Intention_Likely, names_to=c("variable")) %>% 
  group_by(Vote_Intention_Likely, variable) %>% 
  filter(Vote_Intention_Likely!="Green") %>% 
  filter(Vote_Intention_Likely!="NA") %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n))  %>% 
  left_join(., spending_var_labels) %>% 
  ggplot(., aes(y=label, x=average, col=Vote_Intention_Likely))+
 #THE VALUES HAD NOT BEEN RESCALED TO 0 AND 1
  #SO, SETTING THE X-AXIS LABELS TO 0 AND 1 WAS THROWING A PROBLEM
   #xlim(c(0,1))+
  geom_pointrange(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), size=1.2,position=position_jitter(height=0.25))+
  scale_color_manual(values=c("blue", "darkred", "orange"))+
  scale_y_discrete(labels=function(x) str_wrap(x, 10)) +
  labs(color="Vote",x="1=Spend More\n 0=Spend Less", title=str_wrap("How much should the provincial government spend on...", width=100), y="")+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position = "bottom")+guides(col=guide_legend(nrow=2))

#I think it might be worth putting these variables in a categorical (barplot format)
#The first few lines are identical to above
on22 %>% 
  select(Q16:Q21, Vote_Intention_Likely) %>% 
  pivot_longer(., cols=-Vote_Intention_Likely, names_to=c("variable")) %>% 
#  group_by(Vote_Intention_Likely, variable) %>% 
  filter(Vote_Intention_Likely!="Green") %>% 
  filter(Vote_Intention_Likely!="NA") %>% 
  #Filter out missing values on the spending preference
  filter(!is.na(value)) %>% 
  #convert everything that is labelled to a factor i.e. the spending preference variable
  as_factor() %>% 
 # summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n))  %>% 
  #Join with the spending labels 
  left_join(., spending_var_labels) %>% 
  group_by(Vote_Intention_Likely, label, value) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=n/sum(n)) %>% 
  ggplot(., aes(y=label, 
                fill=value, 
                group=value))+
  geom_col(aes(x=Percent),position="dodge")+
  facet_grid(~Vote_Intention_Likely) +
   labs(fill="Spending Preference", title=str_wrap("How much should the provincial government spend on...", width=100), y="")+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position = "bottom")+
  guides(col=guide_legend(nrow=2))+
  scale_x_continuous(labels=scales::percent)

#### Most Important Problem
#Causes
on22 %>% 
  select(Q32_1_x:Q32_9_x, MIP_top3) %>% 
  #pivot them longer, except for the Sample variable
  pivot_longer(., cols=-MIP_top3,names_to=c("variable")) %>% 
  group_by(MIP_top3, variable) %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., cause_var_labels) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=MIP_top3))+
  xlim(c(0,1))+
  geom_pointrange(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), position=position_jitter(height=0.25), size = .75)+
  scale_color_brewer(palette="Dark2")+
  labs(color="Most Important Problem",y="",x="1=A significant cause\n 0=Not a cause at all", title=str_wrap("Causes of housing cost increase", width=100), x="")+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position = "bottom")+
  guides(col=guide_legend(nrow=2, ncol=2))
ggsave(filename=here("Plots", "causes_by_MIP3.png"), width=11, height=5)

#Solutions
on22 %>% 
  select(Q33a_1_x:Q80_6_x,MIP_top3) %>% 
  pivot_longer(., cols=-MIP_top3, names_to=c("variable")) %>%
  group_by(MIP_top3, variable) %>% 
  summarize(average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(), se=sd/sqrt(n)) %>% 
  left_join(., solution_var_labels) %>% 
  ggplot(., aes(y=fct_reorder(label, average), x=average, col=MIP_top3))+
  xlim(c(0,1))+
  geom_pointrange(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), position=position_jitter(height=0.25), size=.75)+
  scale_y_discrete(labels=function(x) str_wrap(x, width=30))+
  scale_color_brewer(palette="Dark2")+
  labs(color="Most Important Problem", y="", x="Score (0=Strongly Oppose,\n 1=Strongly Support)", title=str_wrap("Solutions To Address Housing",60))+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position="bottom")+
  guides(col=guide_legend(nrow=2, ncol=2))
ggsave(filename=here("Plots", "solutions_by_MIP3.png"), width=10, height=6)

