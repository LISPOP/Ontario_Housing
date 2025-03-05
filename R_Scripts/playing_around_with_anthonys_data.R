source("R_Scripts/2_recodes.R")
#Import Anthony's Data
on22$Vote
on22$Renter
library(nnet)
on22$Degree
names(on22)
on22$income_digits
model1<-multinom(Vote_Intention_Likely~age+Degree+male+income_digits+Renter, data=on22)
model2<-multinom(Vote_Intention_Likely~age+Degree+male+income_digits+Housing_Status, data=subset(on22, Housing_Status!="Other"))
model2a<-multinom(Vote~age+Degree+male+income_digits+Housing_Status, data=subset(on22, Housing_Status!="Other"))

on22$Housing_Status
library(modelsummary)
modelsummary(model1)
library(marginaleffects)

predictions(model1, type="probs", variables="Renter", by=c("group", "Renter")) %>%  
  ggplot(., aes(x=estimate, y=Renter, col=group))+
  geom_point()+
  facet_wrap(~fct_relevel(group, "PC", "Liberal", "NDP"), ncol=1)+
  theme_minimal()+
  theme(legend.position="none")+
  scale_color_manual(values=c("darkblue", "darkred", "orange", "darkgreen"))
predictions(model2, type="probs", variables="Housing_Status", by=c("group", "Housing_Status")) %>% 
  ggplot(., aes(x=estimate, y=Housing_Status, col=group))+
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high))+
  facet_wrap(~fct_relevel(group, "PC", "Liberal", "NDP", "Green"), ncol=1)+
  theme_minimal()+
  labs(caption="Covariates include: gender, income, age, degree, likely voters")+
  theme(legend.position="none")+
  scale_color_manual(values=c("darkgreen", "darkred", "orange", "darkblue"))
ggsave(filename="p_vote_housing_status_may_22.png")
predictions(model2a, type="probs", variables="Housing_Status", by=c("group", "Housing_Status")) %>% 
  ggplot(., aes(x=estimate, y=Housing_Status, col=group))+
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high))+
  facet_wrap(~fct_relevel(group, "PC", "Liberal", "NDP", "Green"), ncol=1)+
  theme_minimal()+
  labs(caption="Covariates include: gender, income, age, degree, likely voters")+
  theme(legend.position="none")+
  scale_color_manual(values=c("darkgreen", "darkred", "orange", "darkblue"))

library(janitor)
library(gt)
tabyl(on22$Housing_Status) %>% 
  gt()
table(on22$Renter)

# Load Anthony's Data
library(rio)
library(tidyverse)
library(here)
on23<-import(file=here("Data/Conestoga/HomeownershipPaths_DomenicaMasterMar923.sav"))
library(labelled)
library(car)
lookfor(on23, "vote")
lookfor(on23, "rent") %>% 
  View()
lookfor(on23, "purchase|buy")
lookfor(on23, "purchase")
on23$QID4_2
on23 %>% 
  mutate(Housing_Status=case_when(
    QID15==1~ "Own", 
    QID15==2 & QID4_3 <3~ "Rent Looking to Purchase",
    QID15==2&QID4_3>2~ "Rent Not Looking to Purchase",
    QID15==3&QID4_3 >2 ~ "Rent Not Looking to Purchase",
    QID15==3&QID4_3<3 ~ "Rent Looking to Purchase"
  ))->on23
table(on23$Housing_Status, on23$QID15)
table(on23$Housing_Status, on23$QID4_2)
on23$QID15
on23$Housing_Status<-factor(on23$Housing_Status, 
                            levels=c("Own", "Rent Looking to Purchase", "Rent Not Looking to Purchase"))
lookfor(on23, "education")
on23$QID61
on23$degree<-Recode(on23$QID61, 
                    "1:8='No degree' ; 
                    9:13='Degree'", 
                    as.factor=T, 
                    levels=c("No degree", "Degree"))
lookfor(on23, "gender")
lookfor(on23, "income")
on23$QID67
lookfor(on23, "vote")
on23$QID43
on23$vote<-Recode(on23$QID43, "1='PC' ;
       2='Liberal';
       3='NDP';
       4='Green' ; else=NA", as.factor=T, levels=c("PC", "Liberal", "NDP", "Green"))
library(nnet)
on23$income<-on23$QID67
lookfor(on23, "age")
model3<-multinom(vote~Housing_Status+degree+income+gender+age, data=on23)
library(modelsummary)

modelsummary(model3, shape = term +statistic~ response, stars=T)
predictions(model3, type="probs", variables="Housing_Status", by=c("group", "Housing_Status")) %>% 
  ggplot(., aes(x=estimate, y=Housing_Status, col=group))+
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high))+
  facet_wrap(~fct_relevel(group, "PC", "Liberal", "NDP", "Green"), ncol=1)+
  theme_minimal()+
  theme(legend.position="none")+
  scale_color_manual(values=c("darkgreen", "darkred", "orange", "darkblue"))
ggsave(filename="p_vote_housing_status_feb_23.png", caption="Covariates include: gender, income, age, degree, all voters")
