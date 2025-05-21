#Diagnostics
#show Histogram of age
#source("R_Scripts/1_data_import.R")

#### Missing Values



ggplot(data.2, aes(x=age))+geom_histogram()+geom_vline(xintercept=c(18, 95))+
  labs(title="Age Distribution, OPES22")
summary(data.2$age)
ggsave(filename=here("Plots","age_distribution.png"))
data.2 %>% 
  filter(age>95)

# Diagnose voting not voting for variables
table(as_factor(data.2$Q8), as_factor(data.2$Q12_1))
table(as_factor(data.2$Q10), as_factor(data.2$Q12_1))
#Check average survey response time by votinng_flag variable
# How many respondents have 1 on voting_flag
#Tables can be easily exported as html file using kable() and save_kable()

# (https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Getting_Started)

#density chart of duration by voting_flag group
# data.2 %>% 
#   select(Duration__in_seconds_, voting_flag) %>% 
#   ggplot(., aes(x=Duration__in_seconds_, cols=voting_flag))+
#   geom_density()+
#   scale_x_log10()+
#   facet_grid(cols=vars(voting_flag), labeller=labeller(.cols = label_both))
# 
# data.2 %>% 
#   select(Duration__in_seconds_, voting_flag) %>% 
#   ggplot(., aes(x=Duration__in_seconds_, cols=voting_flag))+
#   geom_histogram()+
#   #scale_x_log10()+
#   facet_grid(cols=vars(voting_flag), labeller=labeller(.cols = label_both))


#Convert duration into minutes
data.2$duration_minutes<-data.2$Duration__in_seconds_/60
# Check median
median(data.2$duration_minutes)
mean(data.2$duration_minutes)
summary(data.2$duration_minutes)

data.2 %>% 
  ggplot(., aes(x=duration_minutes))+geom_histogram()

#Filter the long takers
data.2 %>% filter(duration_minutes>1000)
#Filter the speeders

data.2$duration_z<-scale(data.2$duration_minutes)

data.2 %>% 
  ggplot(., aes(x=duration_z))+geom_histogram()
data.2 %>% 
  filter(duration_z < -1.95) # No speeders more than less than 1.96 sd fewer 


#Less than 100000 seconds?
data.2 %>% 
  mutate(time_flag_1_hour=case_when(
    `Duration__in_seconds_`>3600 ~1,
    TRUE ~0
  ))->data.2

#Less than a minute?
data.2 %>% 
mutate(time_flag_1_minute=case_when(
  `Duration__in_seconds_`<300 ~1,
  TRUE ~0
))->data.2
# table(data.2$time_flag)
# table(data.2$time_flag, data.2$voting_flag)
# val_labels(data.2$time_flag_1_hour)<-c("Less than 1 hour"=0, "More than 1 hour"=1)
# val_labels(data.2$time_flag_1_minute)<-c("more than 5 minutes"=0, "less than 5 minutes"=1)

data.2 %>% 
  ggplot(., aes(x=Duration__in_seconds_))+geom_histogram()+
  facet_wrap(~as_factor(time_flag_1_hour), scales="free_x")
table(as_factor(data.2$time_flag_1_hour))
table(as_factor(data.2$time_flag_1_minute))

data.2 %>% 
  group_by(time_flag_1_hour) %>% 
  summarize(avg=mean(Duration__in_seconds_))
library(knitr)


#### Identify straightliners
library(careless)
data.2 %>% 
  select(matches("Q32_[0-9]$")) %>% 
  irv(.)->straightlining_Q32
nrow(straightlining_Q32)
#Assign this variable back into data.2
data.2$straightlining_Q32<-straightlining_Q32
data.2 %>% 
  select(matches("Q32_[0-9]$"), straightlining_Q32) %>% 
  filter(is.na(straightlining_Q32))
#Now create a data set of the straightliners
data.2 %>%
  #Straightliners have a score of 0 on this variable
  filter(straightlining_Q32==0) %>% 
  #Select responseid and the Q32 variables just for proof of straightlining
  select(ResponseId, matches("Q32_[0-9]$"))->straightliners_Q32

#data.2 %>% filter(straightlining_Q32==0) %>% view()
data.2 %>% 
  mutate(straightliner=case_when(
    straightlining_Q32==0~1,
    straightlining_Q32!=0~0
  ))->data.2


# Check straightliners on experimental questions
names(data.2)

data.2 %>% 
  select(Q35_1:Q35_6) %>% 
  irv(.)->straightlining_experiment
data.2 %>% 
  mutate(straightlining_experiment=straightlining_experiment)->data.2
data.2 %>% 
  mutate(straightliner_experiment=case_when(
    straightlining_experiment==0~1,
    straightlining_experiment!=0~0
  ))->data.2
table(data.2$straightliner_experiment, useNA = "ifany")
data.2 %>% 
  filter(straightliner_experiment==1) %>% 
  select(Q35_1:Q35_6, straightliner_experiment) %>%  nrow()

data.2 %>% 
  group_by(straightliner_experiment) %>% 
  summarize(avg=mean(duration_minutes, na.rm=T))
#Write out to csv
write_csv(straightliners_Q32, file="Data/straightliners_Q32.csv")
#### Make table comparison of vote intention and election result 
library(janitor)

# tabyl(data.2$Vote_Intention_Likely, show_na=T) %>% 
#   adorn_pct_formatting() %>% 
#   adorn_totals()->sample_vote
# Check dates of likely voters
# data.2 %>% 
#   filter(!is.na(Vote_Intention_Likely)) %>% 
# select(RecordedDate) %>% 
#   summary()
#sample_vote<-data.frame(prop.table(table(data.2$Vote_Intention_Likely))*100)
# library(flextable)
# names(sample_vote)<-c("Party" , "Sample n", "Sample Percent", "Percent Certain Voters")
# sample_vote %>% 
#   left_join(., vote22, by="Party") %>% 
#   rename(`Election Percent`="Share") %>% 
#   mutate(`Election Percent`=paste(`Election Percent`, "%", sep="")) %>% 
# flextable() %>% 
#   colformat_double(., digits=0) %>% 
#   save_as_docx(path=here("Tables", "sample_share_election_result.docx"))

## Filter out Straightliners
names(data.2)
# data.2 %>% 
#   filter(straightlining_Q32!=0)->data.2

#### Diagnosting age and agegrps
# data.2 %>%
#   group_by(agegrps) %>% 
#  summarize(average=mean(age, na.rm=T))
# This looks OK. 
# 
# tab1<-prop.table(table(as_factor(data.2$agegrps), data.2$Housing_Status), 1)
# tab2<-prop.table(table(as_factor(data.2$agegrps), as_factor(data.2$Q27)), 1)
# tab1
# tab2
# write.table(tab1, file=here("Tables", "agegroups_by_housing_status_row_percent.txt"))
# write.table(tab2, file=here("Tables", "agegroups_by_q27_row_percent.txt"))

library(gt)
# tabyl(data.2,agegrps, Housing_Status2) %>% 
#   as_factor() %>% 
#   adorn_percentages(denominator="row") %>% 
#   adorn_pct_formatting(digits = 2) %>% 
#   adorn_ns() %>% 
#   gt()
# data.2 %>% 
#   select(agegrps, Housing_Status2) %>%
#   filter(Housing_Status2!="Other") %>%
#   as_factor() %>% 
#   tabyl(., agegrps, Housing_Status2, show_na=F) %>% 
#   adorn_percentages(denominator="row") %>% 
#   adorn_pct_formatting(digits = 2) %>% 
#   adorn_ns() %>% 
#   gt()

#Check value labels for solutions questions 
#Check value labels for Q33a_1to Q33a_6
data.2 %>% 
  select(Q33a_1:Q33a_6) %>% 
  val_labels()
#It's totally unclear. 
#Check individually
val_labels(data.2$Q33a_1)
val_labels(data.2$Q33a_2)
val_labels(data.2$Q33a_3)
val_labels(data.2$Q33a_4)
val_labels(data.2$Q33a_5)
val_labels(data.2$Q33a_6)
#It seems like there are two sets of value labels
data.2 %>% 
  select(Q33a_1:Q33a_6) %>% 
  summary()
data.2 %>% 
  select(Q80_1:Q80_6) %>% 
  summary()

