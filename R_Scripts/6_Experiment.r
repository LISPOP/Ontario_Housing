source("R_Scripts/4_graphical_analysis.R")
#### Experiment
lookfor(on22, "social")
theme_set(theme_minimal(base_size=16))
on22 %>% 
  select(ends_with('_exp')) %>% 
  var_label()->experimental_variable_labels
experimental_variable_labels
on22 %>%
  rename_with(~ unlist(experimental_variable_labels), ends_with('_exp'))->on22
names(on22)
on22$Experimental_Group
on22 %>% 
  pivot_longer(., cols="6 Storey rental building":"Semi-detached house", 
               names_to="Development", values_to="Development Support") ->on22
on22$Development<-factor(on22$Development, levels=c("6 Storey rental building", 
                                                    "15 Storey rental tower", 
                                                    "6 Storey condominium building", 
                                                    "15 Storey condominium Tower", 
                                                    "Single detached house", 
                                                    "Semi-detached house"))
on22 %>% 
  select(Experimental_Group, Development, `Development Support`) %>% 
  group_by(Experimental_Group, Development) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=Average, y=Development, col=Experimental_Group))+
  #geom_point()+
  xlim(c(0,1))+
  scale_y_discrete(limits=rev) +
  geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), position=position_jitter(height=0.25)) +
  labs(title="Normative Argumentation And Support For Residential Development")+
  geom_vline(xintercept=0.5, linetype=2)
ggsave(filename="Plots/experiment_averages_point.png", width=8,  height=4)

on22 %>% 
  select(Experimental_Group, Development, `Development Support`) %>% 
  group_by(Experimental_Group, Development) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n)) %>% 
  write_csv(., file="Experimental_means.csv")

on22 %>% 
  select(Experimental_Group, Development, `Development Support`) %>% 
  group_by(Experimental_Group, Development) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n))  %>% 
pivot_wider(., names_from="Experimental_Group", values_from=c("Average"), id_cols=c("Development")) %>% 
  mutate(Percent_change=across(Private:Social, ~.x/Control))
