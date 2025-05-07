#### Missing Values 
#source("R_Scripts/2_recodes.R")
# 1. Make a variable postal_code_good that returns 1 if nchar(postal_code)==6; else return missing N
# 2. make a variable da_good that returns 1 if PRCDA is not missing; else returns missing
# 3. Select those two variables above as well as the variables we are likely to use
# e.g. renter status, provincial party id, experimental group, anything else?
# Select postal_code_good, da_good and a few other key variables: party _ID

on22%>%
  mutate(postal_code_good = ifelse(nchar(postal_code)==6, "1", NA))->on22

on22%>%
  mutate(da_good = ifelse(nchar(DA2021)==8, "1", NA))->on22

# on22 %>% 
#   as_factor() %>% 
#   select(da_good, postal_code_good, Housing_Status, Experimental_Group, Q23) %>% 
#   summarise(across(everything(), ~sum(is.na(.))))
names(on22)
on22%>%
mutate(across(c(Q1,Q2,Q4_1,Q5_1,Q6a,Q6b,Q7,Q8,Q9,Q10,Q11,Q12_1:Q12_7,,Q13_1:Q13_4,Q14_1:Q14_4,Q15_1:Q15_7,Q16:Q21,Q22a_1:Q22a_3,Q24:Q26,Q27, Q31_1:Q31_6,Q32_1:Q32_9,Q33a_1:Q33a_6,Q80_1:Q80_4,Q34_1:Q35_6, Q36,Q38,Q39,Q40,Q41,Q44),~ case_when(is.na(.x) ~ 1, TRUE ~ 0), .names = "{.col}_miss")) %>% 
  mutate(across(c(Q1,Q2,Q4_1,Q5_1,Q6a,Q6b,Q7,Q8,Q9,Q10,Q11,Q12_1:Q12_7,,Q13_1:Q13_4,Q14_1:Q14_4,Q15_1:Q15_7,Q16:Q21,Q22a_1:Q22a_3,Q24:Q26,Q27, Q31_1:Q31_6,Q32_1:Q32_9,Q33a_1:Q33a_6,Q80_1:Q80_4,Q34_1:Q35_6, Q36,Q38,Q39,Q40,Q41,Q44),~ case_when(is.na(.x) == FALSE ~ 1, TRUE ~ 0), .names = "{.col}_seen")) %>%
mutate(sum_missing = rowSums((select(., Q1_miss:Q44_miss)), na.rm = TRUE)) %>%
mutate(sum_seen = rowSums((select(., Q1_seen:Q44_seen)), na.rm = TRUE)) %>%
mutate(missing_pct = (sum_missing / sum_seen) * 100)->on22
on22 %>% 
  ggplot(., aes(x=missing_pct))+geom_histogram()
on22 %>% 
  select(sum_missing, sum_seen, missing_pct) 
  on22 %>% 
    select(sum_missing, sum_seen, missing_pct, duration_minutes) %>% 
  filter(.,missing_pct>100)
  on22 %>% 
    filter(., missing_pct<100) %>% 
    ggplot(., aes(x=missing_pct))+geom_histogram()
  on22 %>%  filter(., missing_pct<100) %>% 
    ggplot(., aes(x=missing_pct, y=duration_minutes))+geom_point()
  
mean(on22$missing_pct)
on22 %>% 
  select(DA2021, postal_code, Housing_Status, Q23) %>% 
  summary()
on22 %>% 
  mutate(sum_NA=rowSums(is.na(.)))->on22
summary(on22$sum_NA)

on22 %>% 
  filter(sum_NA>100) %>% view()
on22 %>% 
  ggplot(., aes(x=Progress))+geom_histogram()
mean(on22$Progress, na.rm=T)
on22$DA
table(is.na(on22$DA2021, useNA="ifany"))
