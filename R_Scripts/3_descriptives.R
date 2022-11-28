library(here)
source(here("R_Scripts/3_diagnostics.R"))
#### This script is for descriptive statistics and tables
#Table of Descriptives for Housing Status
library(janitor)
theme_set(theme_classic())
# Most Important Problem
on22 %>% 
  tabyl(Topic) %>% 
  arrange(., desc(valid_percent)) %>% 
  filter(., valid_percent>0.02) %>% 
  filter(Topic!="None, no issue important / too many to single out")%>%
  filter(Topic!="Don't know / not sure / N/A")%>%
  filter(Topic!="Refused (or spoiled)")%>%
  ggplot(., aes(x=valid_percent*100, y=fct_reorder(Topic, valid_percent)))+
  geom_col()+labs(y="Issue", x="Percent")+
  theme(text=element_text(size=18))
ggsave(here("Plots", "most_important_problem.png"), width=12, height=6)



#Housng Status
#Get var_labels
on22 %>% 
  select(starts_with("Q15")) %>% 
look_for() %>% 
mutate(label=str_remove_all(label, "Best party for issue - "))->best_party_var_labels
best_party_var_labels
on22 %>% 
  select(starts_with("Q15_")) %>% 
  as_factor() %>% 
  pivot_longer(., cols=everything(), names_to=c("variable"), values_to = c("Party")) %>% 
  left_join(., best_party_var_labels) %>% 
 group_by(variable, label, Party) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=round((n/sum(n))*100),1) %>% 
  filter(!is.na(Party)) %>% 
   ggplot(., aes(y=fct_rev(label), fill=fct_rev(Party), x=Percent))+geom_col(position="dodge")+
  scale_fill_manual(values=c("darkgreen", "darkred", "orange", "darkblue"))+
  labs(y="", fill="Party", title="Issue Ownership, 2022 Ontario Election", caption="Source: Ontario 2022 Provincial Election Study\nLaurier Institute for the Study of Public Opinion and Policy")+
  guides(fill=guide_legend(reverse=T))+
  geom_text(aes(y=label, x=Percent, label=Percent), position=position_dodge(0.9), hjust=-0.5)+
  xlim(c(0,50))+
  theme(legend.position="bottom")+theme(text=element_text(size=18))
ggsave(filename=here("Plots", "best_party.png"), width=12, height=8)


#Cross tab 2018 vote by 2021 vote 

#Cross tab vote switching variable by housing status

# Other stuff! Feel free to freelance. 
