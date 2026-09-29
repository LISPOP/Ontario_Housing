##### Code for Waterloo Op-Ed #####

bouding_box <- read_sf("Data/Regional_Boundary/Regional_Boundary.shp") 

Waterloo <- cities %>% 
  filter(city %in% c("WATERLOO", "KITCHENER", "CAMBRIDGE"))


Waterloo_Long <- Waterloo %>% 
  mutate(across(c(Q32_1, Q32_2, Q32_3, Q32_4, Q32_5, Q32_6, Q32_7, Q32_8, Q32_9), \(x)ifelse(x > 6, 1, 0))) %>% 
  pivot_longer(c(Q32_1, Q32_2, Q32_3, Q32_4, Q32_5, Q32_6, Q32_7, Q32_8, Q32_9), 
               names_to = "Problem", values_to = "value") %>% 
  mutate(Problem = case_match(Problem,
                              "Q32_1" ~ "Speculation",
                              "Q32_2" ~ "Interest",
                              "Q32_3" ~ "Enivronment",
                              "Q32_4" ~ "Municipalities",
                              "Q32_5" ~ "Neigbourhood",
                              "Q32_6" ~ "Sprawl",
                              "Q32_7" ~ "No Affordable Housing",
                              "Q32_8" ~ "No Rent Control",
                              "Q32_9" ~ "Investment"))


problem_df <- Waterloo_Long %>% 
  group_by(Problem) %>% 
  summarise(Mean = mean(value, na.rm = TRUE))

problem_order <- problem_df %>% 
  arrange(-Mean) %>% 
  pull(Problem)

problem_df %>% 
  mutate(Problem = factor(Problem, levels = rev(problem_order))) %>% 
  ggplot(aes(x = Mean, y = Problem)) + 
  geom_point(position = position_dodge(width = 0.6))+
  labs(x = "Percentage that Believe its a problem")
  

Waterloo_Long <- Waterloo %>% 
  mutate(across(c(Q35_1:Q35_6), \(x)ifelse(x > 6, 1, 0))) %>% 
  pivot_longer(c(Q35_1:Q35_6), 
               names_to = "Development", values_to = "value") %>% 
  mutate(Development = case_match(Development,
                              "Q35_1" ~ "6 Storey Rental",
                              "Q35_2" ~ "15 Storey Rental",
                              "Q35_3" ~ "6 Storey Condo",
                              "Q35_4" ~ "15 Storey Condo",
                              "Q35_5" ~ "Sngle Detached",
                              "Q35_6" ~ "Semi-Detached"))


development_df <- Waterloo_Long %>% 
  group_by(Development) %>% 
  summarise(Mean = mean(value, na.rm = TRUE))

development_order <- development_df %>% 
  arrange(-Mean) %>% 
  pull(Development)

development_df %>% 
  mutate(Development = factor(Development, levels = rev(development_order))) %>% 
  ggplot(aes(x = Mean, y = Development)) + 
  labs(x = "Percentage Support") + 
  geom_point(position = position_dodge(width = 0.6)) 

mean(Waterloo$Q33a_4)


Waterloo_long <- Waterloo %>% 
  mutate(across(c(Q33a_4, Q80_2, Q80_3), \(x)ifelse(x > 6, 1, 0))) %>% 
  pivot_longer(c(Q33a_4, Q80_2, Q80_3), 
               names_to = "Solution", values_to = "value") %>% 
  mutate(Solution = case_match(Solution,
                                  "Q33a_4" ~ "Eliminate Rules on Single Family Homes",
                                  "Q80_3" ~ "Increase Supply",
                                  "Q80_2" ~ "Allow more density near transit"))

solution_df <- Waterloo_long %>% 
  group_by(Solution) %>% 
  summarise(Mean = mean(value, na.rm = TRUE))

solution_order <- solution_df %>% 
  arrange(-Mean) %>% 
  pull(Solution)

solution_df %>% 
  mutate(solution_df = factor(Solution, levels = rev(solution_order))) %>% 
  ggplot(aes(x = Mean, y = Solution)) + 
  labs(x = "Percentage Support for each solution") + 
  geom_point(position = position_dodge(width = 0.6)) 
