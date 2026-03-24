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

###REGRESSION MODELS###

#MODEL 1
#DV Increase Housing Supply
#IVs Solutions (11)
on22 %>% 
  select(Q33a_1_x:Q80_6_x) %>% 
pivot_longer(., cols=-Q80_3_x) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~Q80_3_x,data=.x))) ->model.list1

#Generate the table and use gt:cols_label
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

#MODEL 2
#DV Partisanship
#IVs Solutions (11)
on22 %>% 
  select(Q33a_1_x:Q80_6_x, partisanship) %>% 
  pivot_longer(., -c(Q80_3_x, partisanship)) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~partisanship,data=.x))) ->model.list2
  
  #Generate the table and use gt:cols_label
  model_2 <- modelsummary(model.list2$model, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, coef_rename = c("partisanshipLiberal" = "LIB", "partisanshipNDP" = "NDP", "partisanshipOther" = "OTH", "partisanshipNon-partisan" = "Non-Partisan")) |>
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

#MODEL 3
#DV Partisanship+General Housing Supply
#IVs Solutions (11)
on22 %>% 
  select(Q33a_1_x:Q80_6_x, partisanship) %>% 
  pivot_longer(., -c(Q80_3_x,partisanship)) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~Q80_3_x+partisanship,data=.x))) ->model.list3

model_3 <- modelsummary(model.list3$model, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, coef_rename = c("partisanshipLiberal" = "LIB", "partisanshipNDP" = "NDP", "partisanshipOther" = "OTH", "partisanshipNon-partisan" = "Non-Partisan", "Q80_3_x" = "Housing Supply")) |>
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
#output table for model 3
model_3

#MODEL 4
#DV General Housing Supply
#IVs Categorical Solutions (4)
on22 %>% 
  select(supply_general:supply_demand) %>% 
  pivot_longer(., cols=-supply_general) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~supply_general,data=.x))) ->model.list4

#Generate the table and use gt:cols_label
model_4 <- modelsummary(model.list4$model, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, coef_rename = c("supply_general" = "Housing Supply")) |>
  gt::cols_label(
    "(1)" = "Supply - Market-based",
    "(2)" = "Supply - Government Regulation ",
    "(3)" = "Supply - Government Investment",
    "(4)" = "Demand"
  )
#output table for model 4
model_4

#MODEL 5
#DV Partisanship
#IVs Categorical Solutions (4)

on22 %>% 
  #Select variables we will need for models, this will change as we go.
  select(supply_general:supply_demand, partisanship) %>% 
  pivot_longer(., cols=-c(supply_general,partisanship)) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~partisanship,data=.x))) ->model.list5

#Generate the table and use gt:cols_label
model_5 <- modelsummary(model.list5$model, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, coef_rename = c("partisanshipLiberal" = "LIB", "partisanshipNDP" = "NDP", "partisanshipOther" = "OTH", "partisanshipNon-partisan" = "Non-Partisan")) |>
  gt::cols_label(
    "(1)" = "Supply - Market-based",
    "(2)" = "Supply - Government Regulation ",
    "(3)" = "Supply - Government Investment",
    "(4)" = "Demand"
  )
#output table for model 5
model_5

#MODEL 6
#DV Partisanship x General Housing Supply
#IVs Categorical Solutions (4)
on22 %>% 
  #Select variables we will need for models, this will change as we go.
  select(supply_general:supply_demand, partisanship) %>% 
  pivot_longer(., cols=-c(supply_general,partisanship)) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~supply_general,data=.x)),
         model1=map(data, ~lm(value~partisanship,data=.x)),
         model2=map(data, ~lm(value~supply_general*partisanship,data=.x))) ->model.list6
model.list6

out<-list(
  #Add models of relationship between general and supply market
  model.list6$model[[1]], model.list6$model1[[1]], model.list6$model2[[1]],
  #Add models of relationship between general and supply regulation
          model.list6$model[[2]], model.list6$model1[[2]], model.list6$model2[[2]],
  #Add models of relationship between general and supply government
  model.list6$model[[3]], model.list6$model1[[3]], model.list6$model2[[3]],
  #Add models of relationship between general and demand manipulation
  model.list6$model[[4]], model.list6$model1[[4]], model.list6$model2[[4]]
          )
out
#modelsummary(out)
#modelsummary from modelsummary package

#Generate the table and use gt:cols_label
model_6 <- modelsummary(out, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, 
                        coef_rename = c("partisanshipLiberal" = "LIB", "partisanshipNDP" = "NDP", "partisanshipOther" = "OTH", "partisanshipNon-partisan" = "Non-Partisan", "supply_general" = "Housing Supply")) |>
  tab_spanner(
    label = "Supply - Market-based",
    columns = c(
      2,3,4
    ))|>
  tab_spanner(
    label = "Supply - Govt Regulation",
    columns = c(
      5,6,7
    ))|>
  tab_spanner(
    label = "Supply - Govt Investment",
    columns = c(
      8,9,10
    ))|>
  tab_spanner(
    label = "Demand",
    columns = c(
      11,12,13
    ))
#This needs to be reworked to provide some kind of titling
# check into panels or stub heads for gt tables. 
  # gt::cols_label(
  #   "(1)" = "Model 1",
  #   "(2)" = "Model 2",
  #   "(3)" = "Model 3"
  #)
#output table for model 6
model_6

#MODEL 7
#DV Interest in Politics
#IVs Categorical Solutions (4)

on22 %>% 
  #Select variables we will need for models, this will change as we go.
  select(supply_general:supply_demand, avg_interest) %>% 
  pivot_longer(., cols=-c(supply_general,avg_interest)) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~avg_interest*supply_general,data=.x))) ->model.list7

#Generate the table and use gt:cols_label
model_7 <- modelsummary(model.list7$model, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, coef_rename = c("avg_interest" = "Interest in Politics")) |>
  gt::cols_label(
    "(1)" = "Supply - Market-based",
    "(2)" = "Supply - Government Regulation ",
    "(3)" = "Supply - Government Investment",
    "(4)" = "Demand"
  )
#output table for model 7
model_7

#MODEL 8
#DV Interest in Politics x Partisanship
#IVs Categorical Solutions (4)

on22 %>% 
  #Select variables we will need for models, this will change as we go.
  select(supply_general:supply_demand, partisanship, avg_interest) %>% 
  pivot_longer(., cols=-c(avg_interest,partisanship)) %>% 
  nest(data=-name) %>% 
  mutate(model=map(data, ~lm(value~avg_interest,data=.x)),
         model1=map(data, ~lm(value~partisanship,data=.x)),
         model2=map(data, ~lm(value~avg_interest*partisanship,data=.x))) ->model.list8
model.list8
out<-list(model.list8$model[[1]], model.list8$model1[[1]], model.list8$model2[[1]])
#modelsummary(out)
#modelsummary from modelsummary package

#Generate the table and use gt:cols_label
model_8 <- modelsummary(out, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, coef_rename = c("partisanshipLiberal" = "LIB", "partisanshipNDP" = "NDP", "partisanshipOther" = "OTH", "partisanshipNon-partisan" = "Non-Partisan", "supply_general" = "Housing Supply", "avg_interest" = "Interest in Politics"))|>
  gt::cols_label(
    "(1)" = "Model 1",
    "(2)" = "Model 2",
    "(3)" = "Model 3"
  )
#output table for model 8
model_8


on22 %>% 
  select(starts_with("Q33a")&ends_with("_y")|starts_with("Q80")&ends_with("_y")) %>% 
crosstable(by="Q80_3_y", percent_pattern="{n} ({p_col}%)") %>% 
filter(variable!="NA") %>% 
  mutate(label=stringr::str_remove_all(label, "Support for policy - ")) %>% 
  rename(`Specific_support`=variable, `General_Not_Support`=4, `General_Support`=5) %>% 
  select(-6) %>% 
  as_flextable() %>% 
  save_as_html(., path=here("Tables/general_specific_support_categorical.html"))

#### partisanship x attention

solution_var_labels
on22$Interest
#more non-single-family homes
lookfor(on22, "")
mod_supply_4<-lm(Q33a_4_x~partisanship*avg_interest, data=on22)
summary(mod_supply_4)
mod_supply_6<-lm(Q33a_6_x~partisanship*avg_interest, data=on22)
summary(mod_supply_6)
mod_supply_10<-lm(Q80_4_x~partisanship*avg_interest, data=on22)
summary(mod_supply_10)
mod_supply_11<-lm(Q80_5_x~partisanship*avg_interest, data=on22)
summary(mod_supply_11)
mod_supply_12<-lm(Q80_6_x~partisanship*avg_interest, data=on22)
summary(mod_supply_12)
