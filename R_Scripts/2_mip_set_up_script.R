source("R_Scripts/1_data_import.R")
lookfor(on22, "important")
on22 %>%
distinct(Q3) %>% 
  write_csv(file="Data/most_important_problem.csv")

data("ces15phone")

write_csv(data.frame(names(val_labels(ces15phone$CPS15_1))), file="Data/mip_categories.csv")
