#### Missing Values 


data.2%>%
  mutate(postal_code_good = ifelse(nchar(postal_code)==6, "1", NA))->data.2

data.2%>%
  mutate(da_good = ifelse(nchar(DA2021)==8, "1", NA))->data.2


data.2%>%
mutate(across(c(Q1,Q2,Q4_1,Q5_1,Q6a,Q6b,Q7,Q8,Q9,Q10,Q11,Q12_1:Q12_7,,Q13_1:Q13_4,Q14_1:Q14_4,Q15_1:Q15_7,Q16:Q21,Q22a_1:Q22a_3,Q24:Q26,Q27, Q31_1:Q31_6,Q32_1:Q32_9,Q33a_1:Q33a_6,Q80_1:Q80_4,Q34_1:Q35_6, Q36,Q38,Q39,Q40,Q41,Q44),~ case_when(is.na(.x) ~ 1, TRUE ~ 0), .names = "{.col}_miss")) %>% 
  mutate(across(c(Q1,Q2,Q4_1,Q5_1,Q6a,Q6b,Q7,Q8,Q9,Q10,Q11,Q12_1:Q12_7,,Q13_1:Q13_4,Q14_1:Q14_4,Q15_1:Q15_7,Q16:Q21,Q22a_1:Q22a_3,Q24:Q26,Q27, Q31_1:Q31_6,Q32_1:Q32_9,Q33a_1:Q33a_6,Q80_1:Q80_4,Q34_1:Q35_6, Q36,Q38,Q39,Q40,Q41,Q44),~ case_when(is.na(.x) == FALSE ~ 1, TRUE ~ 0), .names = "{.col}_seen")) %>%
mutate(sum_missing = rowSums((select(., Q1_miss:Q44_miss)), na.rm = TRUE)) %>%
mutate(sum_seen = rowSums((select(., Q1_seen:Q44_seen)), na.rm = TRUE)) %>%
mutate(missing_pct = (sum_missing / sum_seen) * 100)->data.2
data.2 %>% 
  ggplot(., aes(x=missing_pct))+geom_histogram()
data.2 %>% 
  select(sum_missing, sum_seen, missing_pct) 
  data.2 %>% 
    select(sum_missing, sum_seen, missing_pct, duration_minutes) %>% 
  filter(.,missing_pct>90)

  mean(data.2$missing_pct)
data.2$missing_z<-scale(data.2$missing_pct)
filter(data.2, missing_z>2) %>% 
  select(missing_pct, duration_minutes)
data.2 %>% 
filter(., missing_pct<100) %>% 
  ggplot(., aes(x=missing_pct))+geom_histogram()
  data.2 %>%  filter(., missing_pct<100) %>% 
    ggplot(., aes(x=missing_pct, y=duration_minutes))+geom_point()
  
data.2 %>% 
  ggplot(., aes(x=Progress))+geom_histogram()

# This drops out the missing values variables and puts them in data.missing
data.2 %>% 
  select(ResponseId,ends_with("_seen"), ends_with("_miss"), ends_with("_pct")) ->data.missing

data.2 %>% 
  select(-ends_with("_seen"), -ends_with("_miss"), -ends_with("_pct"))->data.2
