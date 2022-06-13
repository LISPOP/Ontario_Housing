#Diagnostics
#show Histogram of age
source("R_Scripts/2_recodes.R")
ggplot(on22, aes(x=age))+geom_histogram()+geom_vline(xintercept=c(18, 95))+
  labs(title="Age Distribution, OPES22")
summary(on22$age)
ggsave(filename=here("Plots","age_distribution.png"))


# Diagnose voting not voting for variables
table(as_factor(on22$Q8), as_factor(on22$Q12_1))
table(as_factor(on22$Q10), as_factor(on22$Q12_1))
#Check average survey response time by votinng_flag variable
# How many respondents have 1 on voting_flag
#Tables can be easily exported as html file using kable() and save_kable()

# (https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Getting_Started)
#How many contradictory voters are there?
table(as_factor(on22$voting_flag))

#Summary by voting_flag group
tapply(on22$Duration__in_seconds_, on22$voting_flag, summary)


#density chart of duration by voting_flag group
on22 %>% 
  select(Duration__in_seconds_, voting_flag) %>% 
  ggplot(., aes(x=Duration__in_seconds_, cols=voting_flag))+
  geom_density()+
  scale_x_log10()+
  facet_grid(cols=vars(voting_flag), labeller=labeller(.cols = label_both))

on22 %>% 
  select(Duration__in_seconds_, voting_flag) %>% 
  ggplot(., aes(x=Duration__in_seconds_, cols=voting_flag))+
  geom_histogram()+
  #scale_x_log10()+
  facet_grid(cols=vars(voting_flag), labeller=labeller(.cols = label_both))

#Less than 100000 seconds?
on22 %>% 
  mutate(time_flag_1_hour=case_when(
    `Duration__in_seconds_`>3600 ~1,
    TRUE ~0
  ))->on22

#Less than a minute?
on22 %>% 
mutate(time_flag_1_minute=case_when(
  `Duration__in_seconds_`<300 ~1,
  TRUE ~0
))->on22
table(on22$time_flag)
table(on22$time_flag, on22$voting_flag)
val_labels(on22$time_flag_1_hour)<-c("Less than 1 hour"=0, "More than 1 hour"=1)
val_labels(on22$time_flag_1_minute)<-c("more than 5 minutes"=0, "less than 5 minutes"=1)

on22 %>% 
  ggplot(., aes(x=Duration__in_seconds_))+geom_histogram()+
  facet_wrap(~as_factor(time_flag_1_hour), scales="free_x")
table(as_factor(on22$time_flag_1_hour))
table(as_factor(on22$time_flag_1_minute))

on22 %>% 
  group_by(time_flag_1_hour) %>% 
  summarize(avg=mean(Duration__in_seconds_))

table(on22$voting_flag, on22$time_flag_1_hour)

library(modelsummary)

  datasummary(as_factor(voting_flag)*(mean)~Duration__in_seconds_, data=on22, output="Tables/contradictory_voters_duration.html")
# 
# on22 %>% 
#   select(psid, Q8, Q10, starts_with("Q12_"), voting_flag) %>% 
#   filter(voting_flag==1) %>% 
#   as_factor() %>% 
#   write_csv("Data/contradictory_voters.csv")
