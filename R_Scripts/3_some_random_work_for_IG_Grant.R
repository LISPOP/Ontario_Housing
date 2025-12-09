source("R_Scripts/2_recodes.R")
names(on22)
on22 %>% 
  select(matches("Q32_")) 
on22$partisanship
on22 %>% 
select(matches("^Q32_[0-9]_y"), partisanship) %>% 
  var_label()
on22$partisanship
library(crosstable)
crosstable(on22, matches("^Q32_[0-9]_y"), 
           by=partisanship, showNA = "no", total="row",
           percent_pattern = "{n} ({p_col})") %>% 
  as_flextable()
?crosstable
look_for(on22, "policy")
on22 %>% head()
library(crosstable)
crosstable(on22, matches("^Q33a_[0-9]_y|^Q80_._y"), by=partisanship, showNA = "no", 
           percent_pattern = "{n} ({p_col})", total="row") %>% 
  as_flextable()

