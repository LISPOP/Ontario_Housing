#### Church Op-Ed ####

on22 %>% 
 count(Q80_3_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))
  
on22 %>% 
  count(across(all_of(paste0("Q32_", 1:9, "_y")))) %>% 
  pivot_longer()
  mutate(prop = n/sum(n))


on22 %>% 
  count(Q32_1_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q32_2_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q32_3_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))


on22 %>% 
  count(Q32_4_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q32_5_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q32_6_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q32_7_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q32_8_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))


on22 %>% 
  count(Q32_9_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))


on22 %>% 
  count(Q33a_1_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q33a_2_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q33a_3_y) %>% 
  drop_na() %>% 
  
  mutate(prop = n/sum(n))


on22 %>% 
  count(Q33a_4_y) %>% 
  drop_na() %>% 
  
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q33a_5_y) %>% 
  drop_na() %>% 
  
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q33a_6_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

#### Q80 

on22 %>% 
  count(Q80_1_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q80_2_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q80_3_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q80_4_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))


on22 %>% 
  count(Q80_5_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))

on22 %>% 
  count(Q80_6_y) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n))


on22 %>% 
  mutate(Support_6storey_rent = ifelse(Q35_1 > 6, 1, 0),
         Support_15storey_rent = ifelse(Q35_2 > 6, 1, 0),
         Support_6storey_condo = ifelse(Q35_3 > 6, 1, 0),
         Support_15storey_condo = ifelse(Q35_4 > 6, 1, 0),
         Support_single_detached = ifelse(Q35_5 > 6, 1, 0),
         Support_semi_detatched = ifelse(Q35_6 > 6, 1, 0)
         
         ) %>% 
  summarise(Support_6storey_rent = mean(Support_6storey_rent, na.rm = TRUE),
            Support_15storey_rent = mean(Support_15storey_rent, na.rm = TRUE),
            Support_6storey_condo = mean(Support_6storey_condo, na.rm = TRUE),
            Support_15storey_condo = mean(Support_15storey_condo, na.rm = TRUE),
            Support_single_detached = mean(Support_single_detached, na.rm = TRUE),
            Support_semi_detatched = mean(Support_semi_detatched, na.rm = TRUE))
  