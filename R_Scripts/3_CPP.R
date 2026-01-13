source("R_Scripts/2_recodes.R")
library(crosstable)
library(flextable)
library(modelsummary)
library(knitr)
library(kableExtra)

#Select variables for correlation matrix
on22 %>% 
  select(Q33a_1_x:Q80_6_x) ->cor.out #Store in cor.out

#Make a correlation matrix dropping the general upply varible
cor(-cor.out$Q80_3_x,cor.out$Q80_3_x,use="complete.obs")->cor.mat
#convert to data frame
data.frame(correlation=cor.mat) %>% 
  #take the rownames and add them to a column
  rownames_to_column(var="variable") %>% 
  #left_join to solution_var_labels
  left_join(solution_var_labels) %>%
  #Drop a bunch of useless vraibles
  select(-c("pos", "value_labels", "col_type", "missing", "levels", "label_short"))->supply_correlation_matrix

#Print

supply_correlation_matrix %>% 
  #Arrange descending correlation
  arrange(desc(correlation)) %>% 
  #print the table
kable(.,format="html", digits=2) %>% 
  save_kable(., file=here("Tables/supply_correlations.html"))
on22$Q33a_1_x
on22$Q80_3_x
#Inefficient way, dependent variable by dependent variable
mod1<-lm(Q33a_1_x~Q80_3_x, data=on22)
summary(mod1)
#Inefficient way, dependent variable by dependent variable
mod2<-lm(Q33a_2_x~Q80_3_x, data=on22)
summary(mod2)
on22$Q33a_2_x
#Start with the data frame
on22 %>% 
  #Select variables we will need for models, this will change as we go.
  select(Q33a_1_x:Q80_6_x) %>% 
pivot_longer(., cols=-Q80_3_x) %>% 
  nest(-name) %>% 
  mutate(model=map(data, ~lm(value~Q80_3_x,data=.x))) ->model.list
model.list
#modelsummary from modelsummary package
modelsummary(model.list$model, stars=T)        

on22 %>% 
  select(starts_with("Q33a")&ends_with("_y")|starts_with("Q80")&ends_with("_y")) %>% 
crosstable(by="Q80_3_y", percent_pattern="{n} ({p_col}%)") %>% 
filter(variable!="NA") %>% 
  mutate(label=stringr::str_remove_all(label, "Support for policy - ")) %>% 
  rename(`Specific_support`=variable, `General_Not_Support`=4, `General_Support`=5) %>% 
  select(-6) %>% 
  as_flextable() %>% 
  save_as_html(., path=here("Tables/general_specific_support_categorical.html"))

