source("R_Scripts/1_data_import.R")
#### Experiment
lookfor(on22, "social")
names(on22)
on22 %>% 
  select(ends_with('_exp')) %>% 
  var_label()->experimental_variable_labels
experimental_variable_labels
on22 %>%
rename_with(~ unlist(experimental_variable_labels), ends_with('_exp'))->on22
names(on22)
on22 %>% 
  pivot_longer(., 
               cols="6 Storey rental building":"Semi-detached house", 
               names_to="Development", 
               values_to="Development Support") ->on22
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

#
names(on22)
table(on22$Development)
on22$Development<-factor(on22$Development, levels=c("Single detached house", 
                                  "Semi-detached house",
                                  "6 Storey rental building", 
                                  "6 Storey condominium building",
                                  "15 Storey rental tower",
                                  "15 Storey condominium Tower"))
m1<-lm(`Development Support` ~ Development+Experimental_Group, data=on22)
summary(m1)

