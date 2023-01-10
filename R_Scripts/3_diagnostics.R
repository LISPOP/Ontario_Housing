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

library(knitr)
on22 %>% 
  group_by(voting_flag) %>% 
  summarize(average=mean(Duration__in_seconds_)) %>% 
kable() %>% 
  kableExtra::save_kable(., file=here("Tables", "contradictory_voters_duration.html"))
#datasummary(as_factor(voting_flag)*(mean)~Duration__in_seconds_, data=on22, output="Tables/contradictory_voters_duration.html")

on22 %>% 
  filter(!is.na(Q42)) %>% 
  ggplot(., aes(x=Q42))+geom_histogram()+facet_wrap(~as.factor(income_digits), scales="free_x", ncol=4)
ggsave(filename=here("Plots", "income_reported_n_digits.png"), width=10, height=3)
  
#Graph of likely voters and all voters vote intention
# on22 %>% 
#   select(Vote_Intention_Likely:Vote_Intention_All) %>% 
#   pivot_longer(., cols=everything()) %>% 
#   group_by(name, value) %>% 
#   summarize(n=n()) %>% 
#   mutate(pct=n/sum(n)) %>% 
#   ggplot(., aes(x=pct, y=value))+geom_col()+facet_grid(~name)

#### Identify straightliners
library(careless)
on22 %>% 
  select(matches("Q32_[0-9]$")) %>% 
  irv(.)->straightlining_Q32
#Assign this variable back into on22
on22$straightlining_Q32<-straightlining_Q32
#Now create a data set of the straightliners
on22 %>%
  #Straightliners have a score of 0 on this variable
  filter(straightlining_Q32==0) %>% 
  #Select responseid and the Q32 variables just for proof of straightlining
  select(ResponseId, matches("Q32_[0-9]$"))->straightliners_Q32
#Write out to csv
write_csv(straightliners_Q32, file="Data/straightliners_Q32.csv")
#### Make table comparison of vote intention and election result 
library(janitor)

tabyl(on22$Vote_Intention_Likely, show_na=T) %>% 
  adorn_pct_formatting() %>% 
  adorn_totals()->sample_vote
sample_vote
#sample_vote<-data.frame(prop.table(table(on22$Vote_Intention_Likely))*100)
library(flextable)
names(sample_vote)<-c("Party" , "Sample n", "Sample Percent", "Percent Certain Voters")
sample_vote %>% 
  left_join(., vote22, by="Party") %>% 
  rename(`Election Percent`="Share") %>% 
  mutate(`Election Percent`=paste(`Election Percent`, "%", sep="")) %>% 
flextable() %>% 
  colformat_double(., digits=0) %>% 
  save_as_docx(path=here("Tables", "sample_share_election_result.docx"))

## Filter out Straightliners
names(on22)
on22 %>% 
  filter(straightlining_Q32!=0)->on22

#### Diagnosting age and agegrps
on22 %>%
  group_by(agegrps) %>% 
 summarize(average=mean(age, na.rm=T))
# This looks OK. 

tab1<-prop.table(table(as_factor(on22$agegrps), on22$Housing_Status), 1)
tab2<-prop.table(table(as_factor(on22$agegrps), as_factor(on22$Q27)), 1)
tab1
tab2
write.table(tab1, file=here("Tables", "agegroups_by_housing_status_row_percent.txt"))
write.table(tab2, file=here("Tables", "agegroups_by_q27_row_percent.txt"))

library(gt)
# tabyl(on22,agegrps, Housing_Status2) %>% 
#   as_factor() %>% 
#   adorn_percentages(denominator="row") %>% 
#   adorn_pct_formatting(digits = 2) %>% 
#   adorn_ns() %>% 
#   gt()
# on22 %>% 
#   select(agegrps, Housing_Status2) %>%
#   filter(Housing_Status2!="Other") %>%
#   as_factor() %>% 
#   tabyl(., agegrps, Housing_Status2, show_na=F) %>% 
#   adorn_percentages(denominator="row") %>% 
#   adorn_pct_formatting(digits = 2) %>% 
#   adorn_ns() %>% 
#   gt()

