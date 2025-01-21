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


