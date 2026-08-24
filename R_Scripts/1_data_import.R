# Load Libraries
library(haven)
library(tidyverse)
library(modelsummary)
library(here)
library(labelled)
library(cancensus)
sf::sf_use_s2(TRUE)

#Set cancencus cache and key for a few scripts
#set_cancensus_api_key(key="CensusMapper_e0bb5e9bb16c197f306a580284d35b5b")
#This sets the cancensus cache here in the project directory
set_cancensus_cache_path(cache_path=here("Data/cancensus_cache_statscan_data"))

#Import
on22<-read_dta(file="Data/Housing_02_06_100_Percent_Complete.dta")
names(on22)
#### Postal Code Evaluation
#This modifies postal codes to be properly formatted
on22%>% 
  mutate(postal_code=str_to_upper(str_remove_all(Q47, " ")))->on22
#This provides a count of how many respondents have provided 6-digit postal codes
on22 %>% 
  select(postal_code) %>% 
map_df(., nchar)  %>% 
 count(postal_code)
on22 %>% 
  filter(nchar(postal_code)==7) %>% 
  select(ResponseId, postal_code)

summary(on22$Duration__in_seconds_)
#this code trims a hyphen, trims one postal code at 6 characters,
# filters to try to identify them and shows what is left
# It should show 0 rows

on22 %>% 
  mutate(postal_code=str_remove_all(postal_code, "-")) %>% 
  mutate(postal_code=str_sub(postal_code, 1,6)) %>% 
  filter(nchar(postal_code)==7) %>% 
  select(ResponseId, postal_code)
#Save
on22 %>% 
  mutate(postal_code=str_remove_all(postal_code, "-")) %>% 
  mutate(postal_code=str_sub(postal_code, 1,6))->on22
# Filter out DO variables
on22 %>% 
  select(-contains("_DO_"))->on22
names(on22)
#Filter out v1 variables
on22 %>% 
  select(-matches("^v[0-9]"))->on22
names(on22)

nrow(on22)
# in this script, on22 gets turned into data.2
source("R_Scripts/2_merge_pccf.R")
# Run the diagnostics script
# This looks for straightliners and speeders
source("R_Scripts/2_diagnostics.R")
#Now Run The Missing Values Script
source("R_Scripts/2_missing_values.R")
nrow(data.2)# 1918

# --- Pre-registered straightlining exclusion (OSF: https://osf.io/2ebzk) ---
# Commitment: assess straightlining via intra-rater variability (IRV) on two
# batteries (Q32 and Q35, flagged in 2_diagnostics.R as `straightliner` and
# `straightliner_experiment`, where 1 = IRV == 0), and exclude straightliners
# whose completion time is below the median. We use the conservative "both
# batteries" reading: a respondent is a straightliner only if IRV == 0 on BOTH
# batteries. The median is taken over all respondents at this pre-exclusion
# stage. NA flags (respondent missing a battery) are treated as non-straightliner.
prereg_median_minutes <- median(data.2$duration_minutes, na.rm = TRUE)
data.2 <- data.2 %>%
  mutate(prereg_straightliner_excl = tidyr::replace_na(
    (straightliner == 1 & straightliner_experiment == 1) &
      (duration_minutes < prereg_median_minutes), FALSE)) %>%
  filter(!prereg_straightliner_excl)
nrow(data.2) # after pre-registered straightlining exclusion

# Additional data-quality filter: high item missingness. The extreme-speeder
# `duration_z > -2` filter has been dropped -- after the pre-registered
# straightlining exclusion it removed no additional respondents -- and the
# dissemination-area filter (`!is.na(DA2021)`) is deferred to the geographic
# analysis so it does not shrink the sample for the non-geographic models.
data.2 %>%
  filter(missing_z < 2)->data.2
nrow(data.2) # final analytic N
#This script weights the dataset data.2 after diagnostics and 
# missing values have been excluded
source("R_Scripts/2_weight.R")
# Run the recodes

# This script downloads the statistics canada census data for all Dissemination Areas in Ontario

#This script gets the touch matrices from all Ontario DAs
source("R_Scripts/2_statscan_census_data.R")
#This gets the first order and second dissemination areas 
source("R_Scripts/2_intersect_matrix.R")


#This merges the Dissemination Areas in on22 with the respective statistis 
# gathered in 2_statcsan_census_data.R
on22 %>% 
  #Join respondents to statistics pulled for each DA in on_statscan_da
  left_join(on_statscan_da, by="DA2021") %>% 
  #Join that to the intersecting DAs; note because we have kept the 
  # Underlying structure of Tim's intersection matrix there are multiple rows of 
  # each dissemination area ; but each row contains the averages of the intersecting DAs
  # For each DA 
  # So can only have one row for each DA; so we run distinct() on da.intersect.1
  left_join(., distinct(ungroup(da.intersect.1),DA2021, .keep_all=T)) %>%
  select(-DA2021_intersect) %>% 
  left_join(., distinct(ungroup(da.intersect.2), DA2021, .keep_all=T)) ->on22
names(on22)
on22$Population
#Correlate the a few measures of respondent DA with intersecting DA
on22 %>% 
  select(contains("owned")) %>% 
  rename(`First Order`=2, `First and Second Order`=3) %>% 
  pivot_longer(2:3) %>% 
  ggplot(., aes(x=median_shelter_costs_owned_da, y=value))+
  geom_point()+theme_minimal()+facet_wrap(~fct_relevel(name, "First Order"))+geom_smooth(method="lm")+
  labs(x="Median Shelter Costs (DA)", y="Median Shelter Costs (Averaged Neighbouring DAs)", 
       title="Correlation of Median Shelter Costs of each DA with First and Second Order DAs")
ggsave(here("Plots/median_shelter_costs.png"), width=8, height=5)

on22 %>% 
  select(contains("detached_houses_pct")) %>% 
  rename(`First Order`=2, `First and Second Order`=3) %>% 
  pivot_longer(2:3) %>% 
  ggplot(., aes(x=single_detached_houses_pct_da, y=value))+facet_wrap(~fct_relevel(name, "First Order"))+
  geom_point()+theme_minimal()+geom_smooth(method="lm")+
  labs(x="Percent Single Detached Houses (Percent)", 
       y="Percent Single Detached Houses Neighbouring DAs (Averaged Percent)",
       title="Correlation Between Percent Single-Detached Houses in DAs and Neighbouring DAs")
ggsave(here("Plots/percent_single_detached_houses.png"), width=8, height=5)


#Clean Underscores before names
names(on22)<-str_remove_all(names(on22), "^_")
#names(on22)
#Rename Experimental Group variables
#names(on22)
on22 %>%
  rename("National"=`v7`, "Individual"=`v8`, "Community"=`v9`) ->on22
#on22$National
 on22%>% 
  #rowwise() %>% 
  mutate(experimental_missings=rowSums(select(., National:Community), na.rm=T)) %>% 
   #select(National:Community, experimental_missings)
  mutate(Control=case_when(
    experimental_missings==0~1,
TRUE ~ NA))->on22



# Run the recodes
#names(on22)
#source("R_Scripts/2_recodes.R")


#Look for variables
library(labelled)
# names(on22)


# Spit out Most IMportant problem and hjousing

# look_for(on22, "most important")
# on22 %>% 
#   select(pid, Q3) %>% 
#   write.csv(., file=here("data", "most_important_problem.csv"))
# nrow(on22)
# names(on22)
#clean names for SPSS export

# on22 %>% 
#   select(starts_with("Q32"))

#Check value labels for solutions
val_labels(on22$Q33a_1)
val_labels(on22$Q33a_2)
val_labels(on22$Q33a_3)
val_labels(on22$Q33a_4)
val_labels(on22$Q33a_5)
val_labels(on22$Q33a_6)
# table(as_factor(on22$Q33a_1))
# table(as_factor(on22$Q33a_1), on22$Q33a_1)
#Conclusion in the Q33 set, the #1 corresponded to 0 on the screen; 
# the # 11, corresponded to the 10 on the screen and 12 corresponded to Dont' know
lookfor(on22, "name")

#Most common 
# on22 %>% 
#   count(CSDNAME2021) %>% 
#   arrange(desc(n)) %>% view()
#   write_csv(., file=here("data/most_populous_csds.csv"))

