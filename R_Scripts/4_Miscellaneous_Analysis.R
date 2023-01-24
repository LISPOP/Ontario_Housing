source("R_Scripts/2_recodes.R")
library(broom)
# Regression
on22$own_affordable<-relevel(on22$own_affordable, "Pro-Affordable Housing Non-Homeowner")
table(on22$Homeowner, on22$Housing_Status)
library(modelsummary)
names(on22)
on22$own_affordable
ols1<-function(x) lm(value ~ own_affordable+Degree+male+income_digits+pop_density, data=x)

ols2<-function(x) lm(`Development Support` ~ Experimental_Group+own_affordable+
                       Experimental_Group:own_affordable, data=x)

nrow(on22)
names(on22)
on22$male<-Recode(as.numeric(on22$gender), "1='Male'; 2='Other'", 
                  levels=c("Other", "Male"))
#Regression 1 -  Effect of ideology and Homeownership status on Various policies to addres house price increases
on22 %>% 
  select(solution_var_labels$variable, own_affordable, male, Degree, income_digits, pop_density) %>% 
  pivot_longer(1:12) %>% 
  left_join(., solution_var_labels, by=c("name"="variable")) %>% 
  nest(-label) %>% 
  mutate(m1=map(data, ols1)) %>% 
  mutate(tidy_m1=map(m1, tidy)) ->ols.list
modelsummary(ols.list$m1, stars=T, 
             title="Support for Housing Policy Solutions By Pro-Affordable Housing Homeowners")  
modelsummary(ols.list$m2, stars=T)

on22 %>% 
  select(`Development Support`, Experimental_Group, Development, own_affordable) %>% 
  nest(-Development) %>% 
  mutate(m1=map(data, ols2)) %>% 
  mutate(tidy_m1=map(m1, tidy)) ->model.list
model.list
modelsummary(model.list$m1, stars=T)  


#### Constraint ####

names(on22)

qplot(Ideology, geom="histogram", data=on22)

on22 %>% 
  select(Ideology, Housing_Status, Q33a_1_x:Q80_6_x) %>% 
  pivot_longer(Q33a_1_x:Q80_6_x) %>% 
  ggplot(., aes(x=Ideology, y=value))+
  geom_point()+geom_smooth(method="lm")+
  facet_wrap(~name, nrow=2)

var_label(on22$Q80_3)
