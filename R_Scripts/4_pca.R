source("R_Scripts/3_diagnostics.R")
 look_for(on22, "cause")
library(psych)
 #May be necessary to install Hmisc
 #install.packages("Hmisc")

#Make the corerlation matrix for cause variables
on22 %>% 
  #Select the cause variables
  select(Q32_1_x:Q32_9_x) %>% 
  #Change the names using the pre-established cause_var_labels
  setNames(., cause_var_labels$label) %>% 
  #Scale each variable so mean is 0 and SD is 1
  mutate(across(.fns=scale)) %>% 
 as.matrix() %>% 
  Hmisc::rcorr() ->cause_correlation


#Runa scree plot
#scree(cause_correlation, main="Scree plot of Q32_1_x to Q32_9_x")
look_for(on22, "support")
#Repeat for support variables
on22 %>% 
  select(Q33a_1_x:Q80_6_x) %>% 
  setNames(., solution_var_labels$label) %>% 
  mutate(across(.fns=scale)) %>% 
  as.matrix() %>% 
  Hmisc::rcorr()->support_correlation

#Show scree plots
scree(cause_correlation$r, main="Causes")
scree(support_correlation$r, main="Solutions")

#Conclusion could do two or three for either #
#Leaning 3 for solutions, 2 for causes

# Do parrallel analysis.
fa.parallel(cause_correlation$r, main="Causes")
fa.parallel(support_correlation$r, main="Solutions")
#Suggests two for solutions; 1 for causes

# Do RMSEA
cause_pca2<-principal(cause_correlation$r, nfactors=2)
cause_pca3<-principal(cause_correlation$r, nfactors=3)
support_pca2<-principal(support_correlation$r, nfactors=2)
support_pca3<-principal(support_correlation$r, nfactors=3)
factor.stats(cause_correlation$r, cause_pca2, n.obs=max(cause_correlation$n))$RMSEA
factor.stats(cause_correlation$r, cause_pca3, n.obs=max(cause_correlation$n))$RMSEA

factor.stats(support_correlation$r, support_pca2, n.obs=max(cause_correlation$n))$RMSEA
factor.stats(support_correlation$r, support_pca3, n.obs=max(cause_correlation$n))$RMSEA

#Run pca with varimax, 2 factors
cause_pca2<-principal(cause_correlation$r, nfactors=2, rotate="varimax")
cause_pca3<-principal(cause_correlation$r, nfactors=3, rotate="varimax")

support_pca2<-principal(support_correlation$r, nfactors=2, rotate=
                          "varimax")
support_pca3<-principal(support_correlation$r, nfactors=3, rotate=
                          "varimax")

# Print
fa.sort(cause_pca2)
fa.sort(cause_pca3)

fa.sort(support_pca2)
fa.sort(support_pca3)
#Get Component scores
on22$state<-support_pca$scores[,1]
on22$market<-support_pca$scores[,2]
# 
# on22 %>% 
#   select(Q33a_1_x:Q80_6_x,state, market) %>% 
#   cor(., use="complete.obs")
# on22 %>% 
#   select(state, market, Housing_Status) %>% 
#   pivot_longer(-Housing_Status) %>% 
#   group_by(Housing_Status, name) %>% 
#   summarize(Average=mean(value, na.rm=T))
# 
# on22 %>% 
#   select(age, state, market) %>% 
#   pivot_longer(-1) %>% 
#   ggplot(., aes(x=age, y=value))+geom_point()+facet_grid(~name)+geom_smooth()
# 
# on22 %>% 
#   select(Density, state, market) %>% 
#   pivot_longer(-1) %>% 
#   group_by(Density,name) %>% 
#   summarize(Average=mean(value, na.rm=T))
#   ggplot(., aes(x=, y=value))+geom_point()+facet_grid(~name)+geom_smooth()
# 
#   
