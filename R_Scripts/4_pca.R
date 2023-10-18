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
 # mutate(across(.fns=scale)) %>% 
 as.matrix() ->cause_scores
cause_scores %>% 
  Hmisc::rcorr() ->cause_correlation


#Runa scree plot
#scree(cause_correlation, main="Scree plot of Q32_1_x to Q32_9_x")
look_for(on22, "support")
#Repeat for support variables
on22 %>% 
  select(Q33a_1_x:Q80_6_x) %>% 
  setNames(., solution_var_labels$label) %>% 
 # mutate(across(.fns=scale)) %>% 
  as.matrix() ->support_scores
support_scores %>% 
  Hmisc::rcorr()->support_correlation
support_correlation
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
print(fa.sort(support_pca2), cut=0.3)
print(fa.sort(support_pca3), cut=0.3)
solution_var_labels
support_pca2$loadings[,1] %>% 
  data.frame() %>% 
  filter(.,.>0.4) %>% 
  rownames()->government
support_pca2$loadings[,2] %>% 
  data.frame() %>% 
  filter(.,.>0.4) %>% 
  rownames()->market

government<-data.frame(label=government, government=rep(1,length(government)))
government 

market<-data.frame(label=market, market=rep(1,length(market)))
market

solution_var_labels
government %>% 
  full_join(., solution_var_labels)->solution_var_labels
solution_var_labels
market %>% 
  full_join(., solution_var_labels)->solution_var_labels
solution_var_labels
solution_var_labels %>% 
  filter(market==1) %>% 
  select(variable)->market_variables
market_variables
print(support_pca2, cut=0.3, sort=T)
solution_var_labels %>% 
  filter(government==1) %>% 
  select(variable)->government_variables
government_variables
keys.list<-list(market=market_variables$variable, government=government_variables$variable)
keys.list
names(support_scores)
solution_var_labels %>% 
  arrange(variable)->solution_var_labels
support_scores<-data.frame(support_scores)
support_scores
solution_var_labels
names(support_scores)
names(support_scores)<-solution_var_labels$variable
names(support_scores)

#Score items
scale_scores<-scoreItems(keys.list, support_scores)
keys.list
on22$Market<-scale_scores$scores[,1]
on22$Government<-scale_scores$scores[,2]

#Calculate averages just with items that load on the PCA above > 0.4
# These are just multi-item averages
market_variables
solution_var_labels
#Find Average of Market variables minus housing supply
on22 %>% 
  #Make Market_Average with rowMeans Function
  mutate(Market_Average=rowMeans(
    #use pick() to select the variables included in the calculation
    pick(
      #Selection all_of() all market_variables minus Q80_3_x wchich is housing supply
      all_of(market_variables$variable)&-Q80_3_x), na.rm=T))->on22
#Repeat for Government Regulation
on22 %>% 
  # select(all_of(government_variables$variable)&-Q80_3_x)
  #remove the increase in housing supply
  mutate(Government_Average=rowMeans(pick(all_of(government_variables$variable)&-Q80_3_x), na.rm=T))->on22
#Is this correlated with ideology?
#

 
on22 %>%
  select(Vote_Intention_Likely, Market, Government) %>%
  pivot_longer(2:3) %>%
  group_by(Vote_Intention_Likely, name) %>%
  summarize(avg=mean(value, na.rm=T)) 
lookfor(on22, "ideology")
on22 %>% 
  select(Ideology, Market, Government) %>% 
  cor(., use="complete.obs")
on22 %>% 
  select(all_of(solution_var_labels$variable)) %>% 
  summary()

on22 %>% 
  select(partisanship, contains("_Average")) %>% 
  pivot_longer(-partisanship) %>% 
  group_by(partisanship, name) %>% 
  summarize(Average=mean(value)) %>% 
  rename(Partisanship=partisanship, Measure=name) %>% 
  filter(Partisanship!="Green") %>% 
  mutate(Measure=fct_recode(Measure, Government="Government_Average", Market="Market_Average")) %>% 
  ggplot(., aes(x=Average, y=Measure, col=Partisanship))+
  geom_point(size=5,position=position_dodge(width=0.2))+xlim(c(0,1))+
  guides(col=guide_legend(ncol=2))+scale_color_manual(values=c("darkblue", "orange", "darkred", "grey"))+
  geom_vline(linetype=2, xintercept=0.5)

# Regression

