source("R_Scripts/4_graphical_analysis.R")
#### Experiment
#lookfor(on22, "social")

on22 %>% 
  select(ends_with('_exp')) %>% 
  var_label()->experimental_variable_labels
experimental_variable_labels
on22 %>%
  # This renames the names of the Developmental approval ratings
  # With the type of development
  rename_with(~ unlist(experimental_variable_labels), ends_with('_exp'))->on22
#Check

#on22$Experimental_Group<-Recode(on22$Experimental_Group,as.factor=T, "'Control'='Control' ; 'Private'='Individual' ; 'Public'='Community';'Social'='National'", 
#levels=c("Control" ,"Individual", "Community", "National"))
on22$Experimental_Group<-factor(on22$Experimental_Group, levels=c("Control", "Individual", "Community", "National"))
table(on22$Experimental_Group)
names(on22)

#This sets the data-set up for regressions in on_exp
#This has a dataframe of     columns
#The variable `data` is a data frame of the proper number of observations
#Each row in this data-set corresponds to the data provided for each response in the experinment
on22 %>% 
  pivot_longer(., cols="6 Storey rental building":"Semi-detached house", 
               names_to="Development", values_to="Development Support") %>% 
  nest(-Development)->on_exp
on_exp
#This does the same thing but sets the on22 dataframe up for easy graphing
#Note that the nrow because very large here because we are providing six rows for each respondent.
#Thsu the confidence intervals here are probably not correct. 
on22 %>% 
  pivot_longer(., cols="6 Storey rental building":"Semi-detached house", 
               names_to="Development", values_to="Development Support")->on22
on22$Development<-factor(on22$Development, levels=c("6 Storey rental building", 
                                                    "15 Storey rental tower", 
                                                    "6 Storey condominium building", 
                                                    "15 Storey condominium Tower", 
                                                    "Single detached house", 
                                                   "Semi-detached house"))
on22$Development
on22 %>% 
  mutate(
    #Create dichotomous variable comparing respondent
    # support for rental towers
    # Versus detached houses
    #all others excluded
    rental_tower=case_when(
    str_detect(Development, "rental")~1,
    str_detect(Development, "detached")~0,
    TRUE~NA_integer_,
  ), 
  #Create dichotomous variable comparing respondent
  # support for condo towers
  # Versus detached houses
  #all others excluded
  condo=case_when(
    str_detect(Development, "condo")~1,
    str_detect(Development, "detached")~0,
    TRUE~NA_integer_
  ), 
  #Create dichotomous variable comparing respondent
  # support for 6-story towers
  # Versus detached houses
  #all others excluded
 midsize=case_when(
    str_detect(Development, "6")~1,
    str_detect(Development, "detached")~0,
    TRUE~NA_integer_
  ), 
  )->on22
#### The code below was used for producing graphical analysis prior to January 2025####
on22 %>% 
  select(Experimental_Group, Development, `Development Support`) %>% 
  group_by(Experimental_Group, Development) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=Average, y=fct_reorder(Development, Average, .desc=T), col=Experimental_Group))+
  #geom_point()+
  xlim(c(0,1))+
  scale_y_discrete(limits=rev) +
  geom_pointrange(size=1.2,aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), position=position_jitter(height=0.25)) +
  labs(y="", col="Experimental Group")+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position = "bottom") +
  guides(col=guide_legend(ncol=1))
ggsave(filename="Plots/experiment_averages_point.png", width=10,  height=8)

on22 %>% 
  select(Experimental_Group, Development, `Development Support`) %>% 
  group_by(Experimental_Group, Development) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n)) %>% 
  write_csv(., file="Experimental_means.csv")

on22 %>% 
  select(Experimental_Group, Development, `Development Support`) %>% 
  group_by(Experimental_Group, Development) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), 
            sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n))  %>% 
pivot_wider(., names_from="Experimental_Group", values_from=c("Average"), id_cols=c("Development")) %>% 
  mutate(Percent_change=across(Individual:National, ~.x/Control))


#Estimate Average Support By Homeowners - Ideology

on22 %>% 
  select(Experimental_Group, Development, own_affordable, `Development Support`) %>% 
  group_by(Experimental_Group, Development, own_affordable) %>% 
  filter(!is.na(own_affordable)) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), 
            sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n)) %>% 
  filter(str_detect(own_affordable, "Housing Homeowner")) %>% 
  ggplot(., aes(x=Average, y=Development, col=Experimental_Group))+
  geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), position=position_jitter(height=0.1))+
  facet_wrap(~own_affordable, ncol=2, 
             labeller = labeller(own_affordable = label_wrap_gen(width = 25)))+xlim(c(0,1))+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position="bottom")+
  scale_y_discrete(limits=rev)+labs(y="")+guides(col=guide_legend(ncol=2))
ggsave(filename=here("Plots", "Experiment_development_homeowner_prior_belief.png"), width=12, height=8)
on22 %>% 
  select(Experimental_Group, Development, `Development Support`) %>% 
  group_by(Experimental_Group, Development) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=Average, y=Development, col=Experimental_Group))+
  #geom_point()+
  xlim(c(0,1))+
  scale_y_discrete(limits=rev) +
  geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), position=position_jitter(height=0.25)) +
  labs(y="", x="0=Strongly Oppose, 1=Strongly Support")+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position="bottom")+
  guides(col=guide_legend(ncol=2))
ggsave(filename="Plots/experiment_averages_point.png", width=10,  height=6)

on22 %>% 
  select(Experimental_Group, Development, `Development Support`) %>% 
  group_by(Experimental_Group, Development) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n)) %>% 
  write_csv(., file="Experimental_means.csv")
names(on22)
on22 %>% 
  select(Experimental_Group, Development, `Development Support`) %>% 
  group_by(Experimental_Group, Development) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), 
            sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n))  %>% 
  pivot_wider(., names_from="Experimental_Group", values_from=c("Average"), id_cols=c("Development")) %>% 
  mutate(Percent_change=across(Individual:National, ~.x/Control))


#Estimate Average Support By Homeowners - Ideology

on22 %>% 
  select(Experimental_Group, Development, own_affordable, `Development Support`) %>% 
  group_by(Experimental_Group, Development, own_affordable) %>% 
  filter(!is.na(own_affordable)) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), 
            sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n)) %>% 
  filter(str_detect(own_affordable, "Housing Homeowner")) %>% 
  ggplot(., aes(x=Average, y=Development, col=Experimental_Group))+
  geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), position=position_jitter(height=0.1))+
  facet_wrap(~own_affordable, ncol=2, labeller=labeller(own_affordable=label_wrap_gen(width=22)))+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position="bottom")+
  guides(col=guide_legend(ncol=2))+
  scale_y_discrete(limits=rev)+ labs(y="", x="0=Strongly Oppose, 1=Strongly Support")+
  scale_x_continuous(labels=c("0", "0.25", "0.5", "0.75", "1"))+xlim(c(0,1))
ggsave(filename=here("Plots", "Experiment_development_homeowner_prior_belief.png"), width=10, height=6)


on22 %>% 
  select(Experimental_Group, Development, own_affordable, `Development Support`) %>% 
  group_by(Experimental_Group, Development, own_affordable) %>% 
  filter(!is.na(own_affordable)) %>% 
  summarize(n=n(), Average=mean(`Development Support`, na.rm=T), 
            sd=sd(`Development Support`, na.rm=T), se=sd/sqrt(n)) %>% 
  filter(str_detect(own_affordable, "Non-Homeowner")) %>% 
  ggplot(., aes(x=Average, y=Development, col=Experimental_Group))+
  geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), position=position_jitter(height=0.1))+
  facet_wrap(~own_affordable, ncol=2, labeller=labeller(own_affordable=label_wrap_gen(width=22)))+
  geom_vline(xintercept=0.5, linetype=2)+
  theme(legend.position="bottom")+
  guides(col=guide_legend(ncol=2))+
  scale_y_discrete(limits=rev)+
  labs(y="", x="0=Strongly Oppose, 1=Strongly Support")+
  scale_x_continuous(labels=c("0", "0.25", "0.5", "0.75", "1"))+xlim(c(0,1))
ggsave(filename=here("Plots", "Experiment_development_renter_prior_belief.png"), width=10, height=6)

on22$own_affordable<-relevel(on22$own_affordable,
                             "Pro-Affordable Housing Non-Homeowner")

exp_ols1<-function(x) lm(`Development Support` ~ male+Degree+income_digits+
                             Experimental_Group:own_affordable, data=x)
on22 %>% 
nest(-Development) %>% 
mutate(ols1=map(data, function(x) lm(`Development Support` ~ 
                                       male+Degree+income_digits+Experimental_Group:own_affordable, data=x))) %>% 
  mutate(ols1_tidied=map(ols1, broom::tidy))->exp_models1


  exp_models1 %>% 
    filter(str_detect(Development, "rental")) ->apartment_models
  exp_models1 %>% 
    filter(str_detect(Development, "Condominium")) ->condominium_models
    exp_models1 %>% 
    filter(str_detect(Development, "Single ")) ->single_models

  exp_models1
names(exp_models1$ols1)<-exp_models1$Development

table(on22$own_affordable,on22$Experimental_Group)
# coefs<-c("Experimental_GroupPrivate:own_affordablePro-Affordable Housing Homeowner"=
#            "Private X Pro-Affordable Housing Homeowner", 
#          "Experimental_GroupSocial:own_affordablePro-Affordable Housing Homeowner"=
#            "Social X Pro-Affordable Housing Homeowner",
#          "Experimental_GroupPublic:own_affordablePro-Affordable Housing Homeowner"=
#            "Public X Pro-Affordable Housing Homeowner",
#          "Experimental_GroupPrivate:own_affordableAnti-Affordable Housing Homeowner"=
#            "Private X Anti-Affordable Housing Homeowner", 
#          "Experimental_GroupSocial:own_affordableAnti-Affordable Housing Homeowner"=
#            "Social X Anti-Affordable Housing Homeowner",
#          "Experimental_GroupPublic:own_affordableAnti-Affordable Housing Homeowner"=
#            "Public X Anti-Affordable Housing Homeowner"
#          )
# coefs_renters<-c("Experimental_GroupPrivate:own_affordablePro-Affordable Housing Non-Homeowner"=
#            "Private X Pro-Affordable Housing Non-Homeowner", 
#          "Experimental_GroupSocial:own_affordablePro-Affordable Housing Non-Homeowner"=
#            "Social X Pro-Affordable Housing Non-Homeowner",
#          "Experimental_GroupPublic:own_affordablePro-Affordable Housing Non-Homeowner"=
#            "Public X Pro-Affordable Housing Non-Homeowner",
#          "Experimental_GroupPrivate:own_affordableAnti-Affordable Housing Non-Homeowner"=
#            "Private X Anti-Affordable Housing Non-Homeowner", 
#          "Experimental_GroupSocial:own_affordableAnti-Affordable Housing Non-Homeowner"=
#            "Social X Anti-Affordable Housing Non-Homeowner",
#          "Experimental_GroupPublic:own_affordableAnti-Affordable Housing Non-Homeowner"=
#            "Public X Anti-Affordable Housing Non-Homeowner"
# )


modelsummary(exp_models1$ols1,
             coef_omit=c("!Pro-|Control|Non-Homeowner|Intercept"), stars=T, 
             output="flextable", 
             fmt=2,gof_omit=c("AIC|BIC|F|Log.Lik|Adj.") ) 

#save_as_docx(., path=here("Tables", "experiment_ideology_owners.docx")
modelsummary(exp_models1$ols1,
             coef_omit=c("Housing Homeowner"), stars=T, 
             output="flextable",fmt=2, gof_omit=c("AIC|BIC|F|Log.Lik|Adj.")) 
 # save_as_docx(., path=here("Tables", "experiment_ideology_renters.docx"))

on22 %>% 
  select(Experimental_Group, own_affordable, `Development Support`) %>% 
  group_by(Experimental_Group, own_affordable) %>% 
  summarize(n=n(), avg=mean(`Development Support`, na.rm=T))

