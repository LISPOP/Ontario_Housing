source("R_Scripts/2_recodes.R")
lookfor(on22, "feel")
lookfor(on22, "strong")
on22$Q13_1
# on22 %>% 
#   mutate(inparty=case_when(
#     
#   ))
# on22
#### P of being a strong partisan #### 
lookfor(on22, "strongly")
lookfor(on22, "survey")
on22$Q24
on22 %>% 
  select(Q24, Q48_x) %>% 
  group_by(Q48_x) %>%
  summarize(n=n(), pct=n/sum(n)) 
  ggplot(., aes(x=as_factor(Q48_x), fill=as_factor(Q24)))+
  geom_bar(position="dodge")

#### P of not identifying with a Party ####
partisan_surveys<-table(on22$Q48_x, on22$non_partisan)
library(knitr)
  library(xtable)
  library(stargazer)

#### Affective Polarization
  
lookfor(on22, "feel")
on22$Q13_1

# Calculate average like
on22 %>% 
  rowwise() %>% 
  mutate(
    mean_like=mean(c_across(starts_with("Q13")), na.rm=T)
  ) %>% 
  select(starts_with("Q13"), `mean_like`)
# Calculate average like
on22 %>% 
  rowwise() %>% 
  mutate(
    mean_like=mean(c_across(starts_with("Q13")), na.rm=T)
  ) ->on22
#check
mean(c(52, 73, 17, 25))

#Now calculate like-average like scores for each party.

on22 %>% 
  #creating new variables so mutate
mutate(
  #This code comes from https://stackoverflow.com/questions/65182275/r-subtract-the-same-value-from-multiple-columns
  #We are working across the four party like scores
  across(
    #first we select the columns we are working across
    #Then we provide the function we are doing to each, naemmly, subtracting each from the mean_like score we created above
    matches("Q13_[1234]"), ~
      (.-mean_like)^2, na.rm=T, .names="{.col}-mean_like")
  )->on22

on22 %>% 
  select(contains("mean_like"))
on22 %>% 
  select(starts_with("Q13"))
#Sum up each like - mean_like score
on22 %>% 
  mutate(sigma=sum(c_across(matches("Q13_[1234]-mean")), na.rm=T))->on22
#Calculate N
on22 %>% 
  mutate(N=4-sum(
    is.na(
      c_across(matches("Q13_[1234]-mean")))))->on22

#Now take Sigma divide by N
on22 %>% 
  ungroup() %>% 
  mutate(spread=sqrt(sigma/N))->on22
names(on22)
#Does polarization in crease with survey taking
lookfor(on22, "surveys")
on22 %>% 
  group_by(Q48_x) %>% 
  summarize(n=n(), average=mean(spread, na.rm=T), sd=sd(spread, na.rm=T), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=Q48_x, y=average))+geom_point()+geom_errorbar(aes(ymin=average-(1.96*se), ymax=average+(1.96*se)), width=0)

summary(lm(spread~as_factor(Q48_x), data=on22))

on22$Q48_x
on22$Q48_xy<-Recode(on22$Q48_x, "0=0; 1=1; 2:5=3")
summary(lm(spread~as_factor(Q48_xy), data=on22))

d#Create vote shares for partyes

pc<-.408
liberal<-.238
ndp<-.237
green<-.6

#Weighted


#Create weighted like for each respondent
on22$Q13_2
on22$Q13_1_v<-on22$Q12_1*liberal
on22$Q13_2_v<-on22$Q12_2*pc
on22$Q13_3_v<-on22$Q12_3*ndp
on22$Q13_4_v<-on22$Q12_4*green

# Calculate average like
#this creates each respondent's average like score for all the parties using the weighted like scores. 
on22 %>% 
  rowwise() %>% 
  mutate(
    weighted_mean_like=mean(c_across(starts_with("Q13")&ends_with("_v")), na.rm=T)
  ) ->on22
names(on22)
#Now subtract each parties

on22 %>% 
  #creating new variables so mutate
  mutate(
    #This code comes from https://stackoverflow.com/questions/65182275/r-subtract-the-same-value-from-multiple-columns
    #We are working across the four party like scores
    across(
      #first we select the columns we are working across
      #Then we provide the function we are doing to each, naemmly, subtracting each from the mean_like score we created above
      matches("Q13_[1234]"), ~
        (.-weighted_mean_like)^2, na.rm=T, .names="{.col}-weighted_mean_like")
  )->on22
  
#Now multiple the party likes minus the weighted mean likes by the vote share for each party
on22$Q13_1_v
liberal*(on22$Q13_1_v-on22$weighted_mean_like)
pc*(on22$Q13_2_v-on22$weighted_mean_like)
ndp*(on22$Q13_3_v)
names(on22)
#Repeat the mean function, 

on22 %>% 
  #creating new variables so mutate
  mutate(
    #This code comes from https://stackoverflow.com/questions/65182275/r-subtract-the-same-value-from-multiple-columns
    #We are working across the four party like scores
    across(
      #first we select the columns we are working across
      #Then we provide the function we are doing to each, naemmly, subtracting each from the mean_like score we created above
      matches("Q13_[1234]_v"), ~
        (.-mean_like)^2, na.rm=T, .names="{.col}-mean_like")
  )->on22
#Check disribution of party feeling scores

on22 %>% 
  select(num_range("Q13_", 1:4)) %>% 
  pivot_longer(., cols=everything()) %>% 
  ggplot(., aes(x=value))+geom_histogram()+facet_wrap(~name)
?pivot_longer

 
