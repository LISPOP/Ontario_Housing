source("R_Scripts/3_diagnostics.R")
look_for(on22, "cause")
library(psych)
#Make the corerlation matrix for cause variables
on22 %>% 
  select(Q32_1_x:Q32_9_x) %>% 
  cor(use="complete.obs")->cause_correlation
#Get Cause var_labels
lookfor(on22, "cause")->cause_var_labels
lookfor(on22, "support for policy")->solution_var_labels

cause_var_labels$label<-str_remove_all(cause_var_labels$label, "Causes - ")
solution_var_labels$label<-str_remove_all(solution_var_labels$label, "Support for")
#Runa scree plot
#
scree(cause_correlation, main="Scree plot of Q32_1_x to Q32_9_x")
look_for(on22, "support")
#Repeat for support variables
on22 %>% 
  select(Q33a_1_x:Q80_6_x)->support_correlation
#Change the names of the variables in the cor matrix
#names(support_correlation)<-solution_var_labels$label
#Renmove the junky support for - bit 
names(support_correlation)<-str_remove_all(names(support_correlation), "Support for policy - ")
#Scree plot
scree(support_correlation, main="Scree plot of Q33a_1_x:Q80_6_x")#This suggests two componoents
#Run pca with varimax, 2 factors
cause_pca<-principal(cause_correlation, nfactors=2, rotate="varimax")
support_pca<-principal(support_correlation, nfactors=3, rotate="varimax")
#Print
print(cause_pca, cut=0.3, sort=T)
print(support_pca, cut=0.3, sort=T)
#install.packages("psychTools")
library(psychTools)
library(knitr)

library(kableExtra)
#Print out cause_pca
fa.sort(cause_pca$loadings[1:9,]) %>% 
  data.frame() %>% 
 # left_join(., cause_var_labels)
mutate_all(., .funs=function(x){
  ifelse(abs(x)< 0.3, NA, x)
}) %>% 
  mutate(Variable=rownames(fa.sort(cause_pca$loadings[1:9,]))) %>% 
  select(Variable,1:2) %>% 
  flextable() %>% 
  save_as_html(., path=here("Tables", "PCA_cause.html"))
#Print out support PCA
library(flextable)
fa.sort(support_pca$loadings[1:12,]) %>% 
  data.frame() %>% 
 # left_join(., solutions_var_label)
  mutate_all(., .funs=function(x){
    ifelse(abs(x)< 0.3, NA, x)
  }) %>% 
  mutate(Variable=rownames(fa.sort(support_pca$loadings[1:12,]))) %>% 
  select(Variable,1:2) %>% 
flextable() %>% 
  save_as_html(., path="PCA_solutions.html")


#on22$government_role<-support_pca$scores[,1]
#on22$YIMBY<-support_pca$scores[,2]
#
biplot(support_pca)

on22 %>% 
  select(Q33a_1_x:Q80_6_x, government_role, YIMBY) %>% 
  cor(., use="complete.obs")
on22 %>% 
  select(government_role, YIMBY, Housing_Status) %>% 
  pivot_longer(-Housing_Status) %>% 
  group_by(Housing_Status, name) %>% 
  summarize(Average=mean(value, na.rm=T))

on22 %>% 
  select(age, government_role, YIMBY) %>% 
  pivot_longer(-1) %>% 
  ggplot(., aes(x=age, y=value))+geom_point()+facet_grid(~name)+geom_smooth()

on22 %>% 
  select(Density, government_role, YIMBY) %>% 
  pivot_longer(-1) %>% 
  group_by(Density,name) %>% 
  summarize(Average=mean(value, na.rm=T))
  ggplot(., aes(x=, y=value))+geom_point()+facet_grid(~name)+geom_smooth()

  
