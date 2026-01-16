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
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~Q80_3_x,data=.x))) ->model.list1

model.list1
#modelsummary from modelsummary package

#Generate the table and use gt:cols_label
# Use the default internal names for the columns
# which are usually `(1)` and `(2)` by default for unnamed lists
model_1 <- modelsummary(model.list1$model, output = "gt", stars=T) |>
  gt::cols_label(
    "(1)" = "More affordable public housing",
    "(2)" = "Taxes for owning multiple houses",
    "(3)" = "Increasing taxes for foreign home-buyers",
    "(4)" = "More non-single housing properties",
    "(5)" = "Require developers to build more affordable housing",
    "(6)" = "Add more properties to existing units",
    "(7)" = "Reduce heritage designation laws",
    "(8)" = "Eliminate density and height restrictions",
    "(9)" = "Government loans for new buyers",
    "(10)" = "Eliminate housing transfer taxes",
    "(11)" = "More rent control"
      )
#output table for model 1
model_1

on22 %>% 
    #Select variables we will need for models, used vote intention here.
  select(Q33a_1_x:Q80_6_x, Vote_Intention_All) %>% 
  pivot_longer(., cols=-Vote_Intention_All) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~Vote_Intention_All,data=.x))) ->model.list2
  
  model.list2
  
  #Generate the table and use gt:cols_label
  # Use the default internal names for the columns
  # which are usually `(1)` and `(2)` by default for unnamed lists
  model_2 <- modelsummary(model.list2$model, output = "gt", stars=T) |>
    gt::cols_label(
      "(1)" = "More affordable public housing",
      "(2)" = "Taxes for owning multiple houses",
      "(3)" = "Increasing taxes for foreign home-buyers",
      "(4)" = "More non-single housing properties",
      "(5)" = "Require developers to build more affordable housing",
      "(6)" = "Add more properties to existing units",
      "(7)" = "Reduce heritage designation laws",
      "(8)" = "Eliminate density and height restrictions",
      "(9)" = "Government loans for new buyers",
      "(10)" = "Eliminate housing transfer taxes",
      "(11)" = "More rent control"
    )
#output table for model 2
model_2       

#HAD SOME ISSUES WITH THE INTERACTION MODEL - ERROR 
#Can't combine `Q33a_1_x` <double> and `Vote_Intention_All` <factor<51a55>>
#Start with the data frame
on22 %>% 
  #Select variables we will need for models, interaction between vote intention and policy.
  select(Q33a_1_x:Q80_6_x, Vote_Intention_All) %>% 
  pivot_longer(., cols=-Q80_3_x) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~Q80_3_x+Vote_Intention_All,data=.x))) ->model.list1

model.list3

on22 %>% 
  select(starts_with("Q33a")&ends_with("_y")|starts_with("Q80")&ends_with("_y")) %>% 
crosstable(by="Q80_3_y", percent_pattern="{n} ({p_col}%)") %>% 
filter(variable!="NA") %>% 
  mutate(label=stringr::str_remove_all(label, "Support for policy - ")) %>% 
  rename(`Specific_support`=variable, `General_Not_Support`=4, `General_Support`=5) %>% 
  select(-6) %>% 
  as_flextable() %>% 
  save_as_html(., path=here("Tables/general_specific_support_categorical.html"))

