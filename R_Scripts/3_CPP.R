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
model_1 <- modelsummary(model.list1$model, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, coef_rename = c("Q80_3_x" = "Housing Supply")) |>
  gt::cols_label(
    "(1)" = "More affordable public housing",
    "(2)" = "Tax multiple/vacant homes",
    "(3)" = "Taxes on foreign homebuyers",
    "(4)" = "Allow more non-single family properties",
    "(5)" = "Require developers to build affordable housing",
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
  pivot_longer(., -c(Q80_3_x,Vote_Intention_All)) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~Vote_Intention_All,data=.x))) ->model.list2
  
  model.list2
  
  #Generate the table and use gt:cols_label
  # Use the default internal names for the columns
  # which are usually `(1)` and `(2)` by default for unnamed lists
  model_2 <- modelsummary(model.list2$model, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, coef_rename = c("Vote_Intention_AllLiberal" = "LIB", "Vote_Intention_AllNDP" = "NDP", "Vote_Intention_AllGreen" = "GRN")) |>
    gt::cols_label(
      "(1)" = "More affordable public housing",
      "(2)" = "Tax multiple/vacant homes",
      "(3)" = "Taxes on foreign homebuyers",
      "(4)" = "Allow more non-single family properties",
      "(5)" = "Require developers to build affordable housing",
      "(6)" = "Add more properties to existing units",
      "(7)" = "Reduce heritage designation laws",
      "(8)" = "Eliminate density and height restrictions",
      "(9)" = "Government loans for new buyers",
      "(10)" = "Eliminate housing transfer taxes",
      "(11)" = "More rent control"
    )
#output table for model 2
model_2       

#Start with the data frame
on22 %>% 
  #Select variables we will need for models, interaction between vote intention and policy.
  select(Q33a_1_x:Q80_6_x, Vote_Intention_All) %>% 
  pivot_longer(., -c(Q80_3_x,Vote_Intention_All)) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~Q80_3_x+Vote_Intention_All,data=.x))) ->model.list3

model_3 <- modelsummary(model.list3$model, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, coef_rename = c("Vote_Intention_AllLiberal" = "LIB", "Vote_Intention_AllNDP" = "NDP", "Vote_Intention_AllGreen" = "GRN", "Q80_3_x" = "Housing Supply")) |>
  gt::cols_label(
    "(1)" = "More affordable public housing",
    "(2)" = "Tax multiple/vacant homes",
    "(3)" = "Taxes on foreign homebuyers",
    "(4)" = "Allow more non-single family properties",
    "(5)" = "Require developers to build affordable housing",
    "(6)" = "Add more properties to existing units",
    "(7)" = "Reduce heritage designation laws",
    "(8)" = "Eliminate density and height restrictions",
    "(9)" = "Government loans for new buyers",
    "(10)" = "Eliminate housing transfer taxes",
    "(11)" = "More rent control"
  )

model_3

on22 %>% 
  #Select variables we will need for models, this will change as we go.
  select(supply_general:supply_demand) %>% 
  pivot_longer(., cols=-supply_general) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~supply_general,data=.x))) ->model.list4

#modelsummary from modelsummary package

#Generate the table and use gt:cols_label
# Use the default internal names for the columns
# which are usually `(1)` and `(2)` by default for unnamed lists
model_4 <- modelsummary(model.list4$model, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2) |>
  gt::cols_label(
    "(1)" = "Supply - Market-based",
    "(2)" = "Supply - Government Regulation ",
    "(3)" = "Supply - Government Investment",
    "(4)" = "Demand"
  )
#output table for model 4
model_4

on22 %>% 
  #Select variables we will need for models, this will change as we go.
  select(supply_general:supply_demand, Vote_Intention_All) %>% 
  pivot_longer(., cols=-c(supply_general,Vote_Intention_All)) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~Vote_Intention_All,data=.x))) ->model.list5

model.list5
#modelsummary from modelsummary package

#Generate the table and use gt:cols_label
# Use the default internal names for the columns
# which are usually `(1)` and `(2)` by default for unnamed lists
model_5 <- modelsummary(model.list5$model, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2) |>
  gt::cols_label(
    "(1)" = "Supply - Market-based",
    "(2)" = "Supply - Government Regulation ",
    "(3)" = "Supply - Government Investment",
    "(4)" = "Demand"
  )
#output table for model 5
model_5

on22 %>% 
  #Select variables we will need for models, this will change as we go.
  select(supply_general:supply_demand, Vote_Intention_All) %>% 
  pivot_longer(., cols=-c(supply_general,Vote_Intention_All)) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~supply_general,data=.x)),
         model1=map(data, ~lm(value~Vote_Intention_All,data=.x)),
         model2=map(data, ~lm(value~supply_general*Vote_Intention_All,data=.x))) ->model.list6

out<-list(model.list6$model[[1]], model.list6$model1[[1]], model.list6$model2[[1]])
modelsummary(out)
#modelsummary from modelsummary package

#Generate the table and use gt:cols_label
# Use the default internal names for the columns
# which are usually `(1)` and `(2)` by default for unnamed lists
model_6 <- modelsummary(model.list6$model1, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2) |>
  gt::cols_label(
    "(1)" = "Supply - Market-based",
    "(2)" = "Supply - Government Regulation ",
    "(3)" = "Supply - Government Investment",
    "(4)" = "Demand"
  )
#output table for model 6
model_6

on22 %>% 
  select(starts_with("Q33a")&ends_with("_y")|starts_with("Q80")&ends_with("_y")) %>% 
crosstable(by="Q80_3_y", percent_pattern="{n} ({p_col}%)") %>% 
filter(variable!="NA") %>% 
  mutate(label=stringr::str_remove_all(label, "Support for policy - ")) %>% 
  rename(`Specific_support`=variable, `General_Not_Support`=4, `General_Support`=5) %>% 
  select(-6) %>% 
  as_flextable() %>% 
  save_as_html(., path=here("Tables/general_specific_support_categorical.html"))

on22$Q24
