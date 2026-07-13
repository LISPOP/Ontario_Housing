source("R_Scripts/2_recodes.R")
library(crosstable)
library(flextable)
library(modelsummary)
library(knitr)
library(kableExtra)
#### Figure 1
#Note that the variable name is stored in variable and the actual label is stored in label
solution_var_labels
#Check Raw Means
solution_var_labels
theme_set(theme_minimal(base_size=16))
on22 %>% 
  select(Q33a_1_x:Q80_6_x) %>% 
  pivot_longer(cols=everything(),names_to="variable") %>% 
  left_join(solution_var_labels, by="variable") %>% 
  group_by(label) %>% 
  summarize(Average=mean(value, na.rm=T), sd=sd(value, na.rm=T), n=n(),se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=Average, y=fct_reorder(label, Average)))+
  geom_pointrange(size=0.2,aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)))+
  labs(x="Score (0=Strongly Oppose\n1=Strongly Support)", 
       title=str_wrap("Solutions To Address Housing Price Increase",30), y="")+
  xlim(c(0,1))+
  # scale_y_discrete(labels=function(x) str_wrap(x, 20))+
  geom_vline(xintercept=0.5, linetype=2)

ggsave(filename=here("Plots", "solution_house_price_increase.png"), width=14, height=8)
#Select variables for correlation matrix
on22 %>% 
  select(Q33a_1_x:Q80_6_x) ->cor.out #Store in cor.out

#Make a correlation matrix dropping the general upply varible
cor(cor.out$Q80_3_x, cor.out %>% select(-Q80_3_x), use="complete.obs") -> cor.mat

#convert to data frame
cor(cor.out$Q80_3_x, cor.out %>% select(-Q80_3_x), use="complete.obs") %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "correlation") %>% 
  left_join(solution_var_labels, by = "variable") %>%
  select(-any_of(c("pos", "value_labels", "col_type", "missing", "levels", "label_short"))) -> supply_correlation_matrix
#Print

supply_correlation_matrix %>% 
  mutate(correlation = as.numeric(correlation)) %>%
  arrange(desc(correlation)) %>%
  kable(., format="html", digits=2) %>% 
  save_kable(., file=here("Tables/supply_correlations.html"))

#Inefficient way, dependent variable by dependent variable
mod1<-lm(Q33a_1_x~Q80_3_x, data=on22)
summary(mod1)
#Inefficient way, dependent variable by dependent variable
mod2<-lm(Q33a_2_x~Q80_3_x, data=on22)
summary(mod2)
on22$Q33a_2_x

###REGRESSION MODELS###

#MODEL 1: Relationship between increasing general housing supply and the other 11
#specific solutions

# Fit a separate regression for each housing-solution outcome, using general housing supply
# (Q80_3_x) as the predictor, then present all models side-by-side in a gt table.
#
# Approach:
# 1) Select solution outcome columns + the supply predictor column.
# 2) Pivot solution outcomes to long format (one row per respondent per solution).
# 3) Nest by solution item name (one tibble per solution).
# 4) Map lm() across nested tibbles to fit one model per solution.
# 5) Use modelsummary() to output a formatted gt regression table and relabel columns.

# Start with the 'on22' dataset (presumably a survey dataframe)
on22 %>% 
  # Keep only the housing-solution outcome variables (a contiguous block of columns)
  # Q33a_1_x through Q80_6_x are interpreted here as the 12 “housing solution” items
  select(Q33a_1_x:Q80_6_x) %>% 
  
  # Reshape from wide to long so that each row becomes:
  # - one observation (respondent) for one solution item
  # - 'name' = which solution item (column name)
  # - 'value' = the respondent’s response to that solution item
  #
  # NOTE: cols = -Q80_3_x means “pivot everything except Q80_3_x”.
  # This implies Q80_3_x is kept as the predictor variable (general supply question).
  pivot_longer(., cols = -Q80_3_x) %>% 
  
  # Group the long data by the solution-item variable name.
  # After nesting:
  # - one row per solution item
  # - 'data' column contains a tibble of all respondent rows for that item
  nest(data = -name) %>% 
  
  # For each nested tibble (i.e., for each solution item),
  # fit a linear model predicting the solution response ('value')
  # from the general housing supply predictor (Q80_3_x).
  #
  # map() returns a list of lm() objects, one per solution item.
  mutate(model = map(data, ~ lm(value ~ Q80_3_x, data = .x))) -> model.list1

# Create a formatted regression table for all models using modelsummary
# - output = "gt" returns a 'gt' table object for further styling
# - stars = TRUE adds significance stars
# - gof_omit removes selected goodness-of-fit rows from the table
# - fmt = 2 formats estimates to 2 decimal places
# - coef_rename makes the predictor label readable in the output
supply_and_solutions <- modelsummary(
  model.list1$model,
  output = "gt",
  stars = TRUE,
  gof_omit = "AIC|BIC|Log|F|R2|RMSE",
  fmt = 2,
  coef_rename = c("Q80_3_x" = "Housing Supply")
) |>
  
  # Replace default model column labels (e.g., "(1)", "(2)", ...)
  # with descriptive names corresponding to each housing solution outcome
  gt::cols_label(
    "(1)"  = "More affordable public housing",
    "(2)"  = "Tax multiple/vacant homes",
    "(3)"  = "Taxes on foreign homebuyers",
    "(4)"  = "Allow more non-single family properties",
    "(5)"  = "Require developers to build affordable housing",
    "(6)"  = "Add more properties to existing units",
    "(7)"  = "Reduce heritage designation laws",
    "(8)"  = "Eliminate density and height restrictions",
    "(9)"  = "Government loans for new buyers",
    "(10)" = "Eliminate housing transfer taxes",
    "(11)" = "More rent control"
  )

# Display the regression table
supply_and_solutions

#MODEL 2: Relationship between partisanship and 11 specific solutions
# This set of models examines how political partisanship is associated with
# support for different housing policy solutions.
#
# A separate linear regression is estimated for each housing solution,
# with partisanship as the key independent variable.
# Coefficients represent differences in support relative to the
# omitted reference category of partisanship.

# Start with the 'on22' survey dataset
on22 %>% 
  # Select all housing-solution outcome variables plus the partisanship variable
  # Q33a_1_x through Q80_6_x represent the set of solution-related survey items
  select(Q33a_1_x:Q80_6_x, partisanship) %>% 
  
  # Reshape the data from wide to long format:
  # - Each row becomes one respondent’s response to one housing solution
  # - 'name' identifies the housing solution item
  # - 'value' contains the respondent’s level of support for that solution
  #
  # Exclude partisanship (and Q80_3_x, if still present) from the pivot so that
  # partisanship remains available as the predictor in the regression models
  pivot_longer(., -c(Q80_3_x, partisanship)) %>% 
  
  # Nest the long data by housing solution item
  # This creates one row per solution, with a list-column ('data') containing
  # all respondent-level observations for that solution
  nest(data = -name) %>% 
  
  # For each nested dataset (i.e., for each housing solution),
  # fit a linear regression model predicting support for that solution ('value')
  # as a function of political partisanship
  #
  # Because 'partisanship' is a factor, lm() estimates coefficients relative
  # to the omitted reference category
  mutate(model = map(data, ~ lm(value ~ partisanship, data = .x))) -> model.list2

  
# Generate a formatted regression table for all partisanship models
# using modelsummary with gt output
partisanship_and_solutions <- modelsummary(
  model.list2$model,
  output   = "gt",
  stars    = TRUE, 
  fmt      = 2,
  
  # Omit selected goodness-of-fit statistics to keep the table concise
  gof_omit = "AIC|BIC|Log|F|R2|RMSE",
  
  # Rename partisanship coefficients for interpretability
  # (relative to the omitted reference category)
  coef_rename = c(
    "partisanshipLiberal"      = "LIB",
    "partisanshipNDP"          = "NDP",
    "partisanshipOther"        = "OTH",
    "partisanshipNon-partisan" = "Non-Partisan"
  )
) |>
  
  # Replace default model column labels with descriptive housing-solution names
  gt::cols_label(
    "(1)"  = "More affordable public housing",
    "(2)"  = "Tax multiple/vacant homes",
    "(3)"  = "Taxes on foreign homebuyers",
    "(4)"  = "Allow more non-single family properties",
    "(5)"  = "Require developers to build affordable housing",
    "(6)"  = "Add more properties to existing units",
    "(7)"  = "Reduce heritage designation laws",
    "(8)"  = "Eliminate density and height restrictions",
    "(9)"  = "Government loans for new buyers",
    "(10)" = "Eliminate housing transfer taxes",
    "(11)" = "More rent control"
  )

# Output the table for Model 2
partisanship_and_solutions


#MODEL 3: Combination of Model 1 and 2 = partisanship+increasing general supply
#and other 11 specific solutions
# These models estimate support for each housing policy solution as a function of
# both general attitudes toward housing supply and political partisanship.
#
# Including both predictors allows us to assess whether partisan differences
# persist once general views on housing supply are held constant.
#
# A separate linear regression is estimated for each housing solution outcome.
# Start with the 'on22' survey dataset
on22 %>% 
  # Select all housing-solution outcome variables plus partisanship
  # Q33a_1_x through Q80_6_x represent the set of housing policy solutions
  select(Q33a_1_x:Q80_6_x, partisanship) %>% 
  
  # Reshape data from wide to long format:
  # - Each row represents one respondent’s response to one housing solution
  # - 'name' identifies which housing solution is being evaluated
  # - 'value' is the respondent’s level of support for that solution
  #
  # Exclude the predictors (Q80_3_x and partisanship) from the pivot so they
  # remain available for use in the regression models
  pivot_longer(., -c(Q80_3_x, partisanship)) %>% 
  
  # Nest the long-format data by housing solution
  # This creates one row per solution, with a list-column ('data')
  # containing all respondent-level observations for that solution
  nest(data = -name) %>% 
  
  # For each nested dataset (i.e., for each housing solution),
  # estimate a linear regression model predicting support for that solution ('value')
  # from:
  #   1) General attitudes toward housing supply (Q80_3_x), and
  #   2) Political partisanship
  #
  # Coefficients on partisanship represent differences relative to the
  # omitted reference category
  mutate(model = map(data, ~ lm(value ~ Q80_3_x + partisanship, data = .x))) -> model.list3

# Generate a formatted regression table summarizing all models
# using modelsummary with gt output
partisan_supply_and_solutions <- modelsummary(
  model.list3$model,
  output   = "gt",
  stars    = TRUE,
  fmt      = 2,
  
  # Omit selected goodness-of-fit statistics to keep the table concise
  gof_omit = "AIC|BIC|Log|F|R2|RMSE",
  
  # Rename coefficients for interpretability
  coef_rename = c(
    "Q80_3_x"                 = "Housing Supply",
    "partisanshipLiberal"     = "LIB",
    "partisanshipNDP"         = "NDP",
    "partisanshipOther"       = "OTH",
    "partisanshipNon-partisan"= "Non-Partisan"
  )
) |>
  
  # Replace generic model column labels with descriptive housing-solution names
  gt::cols_label(
    "(1)"  = "More affordable public housing",
    "(2)"  = "Tax multiple/vacant homes",
    "(3)"  = "Taxes on foreign homebuyers",
    "(4)"  = "Allow more non-single family properties",
    "(5)"  = "Require developers to build affordable housing",
    "(6)"  = "Add more properties to existing units",
    "(7)"  = "Reduce heritage designation laws",
    "(8)"  = "Eliminate density and height restrictions",
    "(9)"  = "Government loans for new buyers",
    "(10)" = "Eliminate housing transfer taxes",
    "(11)" = "More rent control"
  )

# Output the table for Model 3
partisan_supply_and_solutions

# MODEL 4
# Dependent variable (DV): General housing supply attitudes
# Independent variables (IVs): Support for different supply/demand approaches (categorical)

# These models assess whether general attitudes toward housing supply
# are associated with support for different policy approaches to housing,
# including market-based supply expansion, regulatory strategies,
# government investment, and demand-side interventions.
#
# A separate linear regression is estimated for each approach.
on22 %>% 
  # Select the general housing supply attitude variable along with
  # the four categorical solution approach measures
  # (market-based, regulation, government investment, and demand)
  select(supply_general:supply_demand) %>% 
  
  # Reshape the data from wide to long format so that:
  # - Each row represents one respondent’s evaluation of one solution approach
  # - 'name' identifies the specific supply/demand approach
  # - 'value' contains the respondent’s level of support for that approach
  #
  # Exclude 'supply_general' from the pivot so it remains the outcome variable
  pivot_longer(., cols = -supply_general) %>% 
  
  # Nest the long-format data by solution approach
  # This creates one row per approach, with a list-column ('data')
  # containing respondent-level observations
  nest(data = -name) %>% 
  
  # For each nested dataset (i.e., for each solution approach),
  # estimate a linear regression model predicting support for that approach ('value')
  # from general housing supply attitudes ('supply_general')
  mutate(model = map(data, ~ lm(value ~ supply_general, data = .x))) -> model.list4

# Generate a formatted regression table for Model 4 using modelsummary
supply_and_categories <- modelsummary(
  model.list4$model,
  output   = "gt",
  stars    = TRUE,
  fmt      = 2,
  
  # Omit selected goodness-of-fit statistics to keep the table concise
  gof_omit = "AIC|BIC|Log|F|R2|RMSE",
  
  # Rename the predictor for clarity in the output table
  coef_rename = c("supply_general" = "Housing Supply")
) |>
  
  # Replace generic column labels with descriptive solution approach names
  gt::cols_label(
    "(1)" = "Supply – Market-based",
    "(2)" = "Supply – Government Regulation",
    "(3)" = "Supply – Government Investment",
    "(4)" = "Demand"
  )

# Output the table for Model 4
supply_and_categories

# MODEL 5
# Dependent variables (DVs): Support for different housing policy categories
#   (market-based supply, regulation, government investment, demand)
# Independent variable (IV): Political partisanship

on22 %>% 
  # Select the general housing supply category measures and partisanship
  # These variables represent broad policy approach categories rather than
  # individual housing solutions
  select(supply_general:supply_demand, partisanship) %>% 
  
  # Convert the data from wide to long format so that:
  # - Each row represents one respondent’s support for one policy category
  # - 'name' identifies the policy category
  # - 'value' contains the level of support for that category
  #
  # Exclude both supply_general and partisanship from the pivot so that
  # partisanship remains available as the predictor
  pivot_longer(., cols = -c(supply_general, partisanship)) %>% 
  
  # Nest the long-format data by policy category
  # This results in one row per category, with a list-column ('data')
  # containing respondent-level observations
  nest(data = -name) %>% 
  
  # For each policy category, estimate a linear regression model predicting
  # support for that category ('value') as a function of partisanship
  #
  # Because partisanship is categorical, coefficients are interpreted
  # relative to the omitted reference category
  mutate(model = map(data, ~ lm(value ~ partisanship, data = .x))) -> model.list5

# Generate a formatted regression table summarizing Model 5
partisan_and_categories <- modelsummary(
  model.list5$model,
  output   = "gt",
  stars    = TRUE,
  fmt      = 2,
  
  # Omit goodness-of-fit statistics to keep the table concise
  gof_omit = "AIC|BIC|Log|F|R2|RMSE",
  
  # Rename partisanship coefficients for readability
  coef_rename = c(
    "partisanshipLiberal"      = "LIB",
    "partisanshipNDP"          = "NDP",
    "partisanshipOther"        = "OTH",
    "partisanshipNon-partisan" = "Non-Partisan"
  )
) |>
  
  # Replace generic model column labels with descriptive category names
  gt::cols_label(
    "(1)" = "Supply – Market-based",
    "(2)" = "Supply – Government Regulation",
    "(3)" = "Supply – Government Investment",
    "(4)" = "Demand"
  )

# Output the table for Model 5
partisan_and_categories

#MODEL 6
#DV Partisanship x General Housing Supply
#IVs Categorical Solutions (4)
on22 %>%
  # Select the “category” outcomes (e.g., market-based supply, regulation, etc.)
  # plus the predictors we want in the models.
  select(supply_general:supply_demand, partisanship) %>% 
  
  # Convert category columns into long format:
  # - 'name' = which category outcome (e.g., supply_market, supply_regulation, etc.)
  # - 'value' = respondent’s score for that category outcome
  #
  # Keep supply_general and partisanship as predictors by excluding them from pivoting.
  pivot_longer(., cols = -c(supply_general, partisanship)) %>% 
  
  # Create one nested dataframe per category outcome (one row per 'name').
  # Each row now contains a list-column 'data' holding respondent-level rows.
  nest(data = -name) %>% 
  
  # Fit THREE models per category outcome:
  #   model  = value ~ supply_general                (supply-only)
  #   model1 = value ~ partisanship                  (partisanship-only)
  #   model2 = value ~ supply_general * partisanship (interaction: supply effect varies by party)
  mutate(
    model  = map(data, ~ lm(value ~ supply_general, data = .x)),
    model1 = map(data, ~ lm(value ~ partisanship,  data = .x)),
    model2 = map(data, ~ lm(value ~ supply_general * partisanship, data = .x))
  ) -> model.list6

# Inspect the nested structure (one row per category; three model columns)
model.list6


out <- model.list6 %>% 
  select(model, model1, model2) %>% 
  purrr::pmap(function(model, model1, model2) list(model, model1, model2)) %>% 
  purrr::flatten()

out

# Optional: label each of the 12 model columns as “Supply”, “Partisanship”, “Interaction”
col_labs <- setNames(
  rep(c("Supply only", "Partisanship only", "Supply × Partisanship"), 4),
  paste0("(", 1:12, ")")
)

partisanshipxsupply_and_categories <- modelsummary(
  out,
  output = "gt",
  stars  = TRUE,
  fmt    = 2,
  gof_omit = "AIC|BIC|Log|F|R2|RMSE",
  coef_rename = c(
    "supply_general"            = "Housing Supply",
    "partisanshipLiberal"       = "LIB",
    "partisanshipNDP"           = "NDP",
    "partisanshipOther"         = "OTH",
    "partisanshipNon-partisan"  = "Non-Partisan"
  )
) |>
  
  # (Recommended) Add a clear title/subtitle so the table “reads” on its own
  gt::tab_header(
    title = "Model 6: Predicting Support for Housing Policy Categories",
    subtitle = "Each panel shows three specifications: Supply only, Partisanship only, and Supply × Partisanship"
  ) |>
  
  # (Recommended) Label each model column within panels
  gt::cols_label(!!!col_labs) |>
  
  # Panel spanners for the four outcome categories
  gt::tab_spanner(
    label   = "Supply – Market-based",
    columns = c("(1)", "(2)", "(3)")
  ) |>
  gt::tab_spanner(
    label   = "Supply – Govt Regulation",
    columns = c("(4)", "(5)", "(6)")
  ) |>
  gt::tab_spanner(
    label   = "Supply – Govt Investment",
    columns = c("(7)", "(8)", "(9)")
  ) |>
  gt::tab_spanner(
    label   = "Demand",
    columns = c("(10)", "(11)", "(12)")
  ) |>
  
  # Optional: add a source note about interpretation
  gt::tab_source_note(
    source_note = "Partisanship coefficients are relative to the omitted reference category. Interaction terms indicate whether the supply effect differs by partisanship."
  )

# Output
partisanshipxsupply_and_categories

#MODEL 7
#DV Interest in Politics
#IVs Categorical Solutions (4)

on22 %>%
  # Select the outcome variables representing broad housing policy categories
  # (market-based supply, regulation, government investment, and demand),
  # along with the predictors used in this model
  select(supply_general:supply_demand, avg_interest) %>% 
  
  # Reshape the data from wide to long format so that:
  # - Each row represents a respondent’s support for one policy category
  # - 'name' identifies the specific category
  # - 'value' contains the respondent’s level of support
  #
  # Exclude the predictors from pivoting so they remain available in the models
  pivot_longer(., cols = -c(supply_general, avg_interest)) %>% 
  
  # Nest the long-format data by policy category
  # This creates one row per category, with a list-column ('data')
  # containing respondent-level observations
  nest(data = -name) %>% 
  
  # For each category outcome, estimate a linear regression model predicting
  # support for that category ('value') as a function of:
  #   1) General housing supply attitudes (supply_general),
  #   2) Political interest (avg_interest), and
  #   3) Their interaction
  #
  # The interaction term allows the effect of housing supply attitudes
  # to vary by respondents’ level of political interest
  mutate(
    model = map(data, 
                ~ lm(value ~ avg_interest * supply_general, data = .x))
  ) -> model.list7

# Generate a formatted regression table summarizing Model 7
Interestxsupply_and_categories <- modelsummary(
  model.list7$model,
  output   = "gt",
  stars    = TRUE,
  fmt      = 2,
  
  # Omit goodness-of-fit statistics to focus attention on coefficients
  gof_omit = "AIC|BIC|Log|F|R2|RMSE",
  
  # Rename the political interest variable for readability in the table
  coef_rename = c("avg_interest" = "Interest in Politics")
) |>
  
  # Replace generic model column labels with descriptive policy category names
  gt::cols_label(
    "(1)" = "Supply – Market-based",
    "(2)" = "Supply – Government Regulation",
    "(3)" = "Supply – Government Investment",
    "(4)" = "Demand"
  )

# Output the table for Model 7
Interestxsupply_and_categories

#MODEL 8
#DV Interest in Politics x Partisanship
#IVs Categorical Solutions (4)

# This model set compares the explanatory power of political interest,
# partisanship, and their interaction for a single housing policy category.
#
# The interaction model tests whether politically engaged respondents
# translate partisan identities into policy preferences more strongly
# than less politically interested respondents.

on22 %>%
  # Select the category-level outcome variables (supply/demand),
  # along with partisanship and political interest
  select(supply_general:supply_demand, partisanship, avg_interest) %>% 
  
  # Reshape the category outcomes into long format so that:
  # - Each row represents one respondent’s support for one policy category
  # - 'name' identifies the category (e.g., market-based supply, demand, etc.)
  # - 'value' contains the respondent’s level of support
  #
  # Exclude the predictors (avg_interest and partisanship) from pivoting
  # so they remain available for modeling
  pivot_longer(., cols = -c(avg_interest, partisanship)) %>% 
  
  # Nest the long-format data by policy category
  # This produces one row per category with a list-column ('data')
  # holding respondent-level observations
  nest(data = -name) %>% 
  
  # Estimate three models for each policy category:
  #   model  = value ~ avg_interest
  #   model1 = value ~ partisanship
  #   model2 = value ~ avg_interest × partisanship
  #
  # The interaction model tests whether the effect of political interest
  # differs across partisan groups
  mutate(
    model  = map(data, ~ lm(value ~ avg_interest, data = .x)),
    model1 = map(data, ~ lm(value ~ partisanship, data = .x)),
    model2 = map(data, ~ lm(value ~ avg_interest * partisanship, data = .x))
  ) -> model.list8

# Inspect the nested object (one row per category, three models per row)
model.list8


# Extract the three models for the FIRST policy category only
# (e.g., Supply – Market-based)
#
# This table is therefore a focused comparison of:
#   Model 1: Political interest only
#   Model 2: Partisanship only
#   Model 3: Interest × Partisanship
out <- list(
  model.list8$model[[1]],
  model.list8$model1[[1]],
  model.list8$model2[[1]]
)

out

# Generate a formatted regression table for Model 8
interest_partisanship_interaction <- modelsummary(
  out,
  output = "gt",
  stars  = TRUE,
  fmt    = 2,
  
  # Omit goodness-of-fit statistics to keep the table readable
  gof_omit = "AIC|BIC|Log|F|R2|RMSE",
  
  # Rename coefficients for clarity
  coef_rename = c(
    "avg_interest"             = "Interest in Politics",
    "partisanshipLiberal"      = "LIB",
    "partisanshipNDP"          = "NDP",
    "partisanshipOther"        = "OTH",
    "partisanshipNon-partisan" = "Non-Partisan",
    "supply_general"           = "Housing Supply"
  )
) |>
  
  # Label the three columns to reflect the nested model comparison
  gt::cols_label(
    "(1)" = "Interest only",
    "(2)" = "Partisanship only",
    "(3)" = "Interest × Partisanship"
  )

# Output the table for Model 8
interest_partisanship_interaction


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


# MODEL 9
# Dependent variables: Individual housing solution items
# Independent variables: Partisanship, political interest, and their interaction

# Each model estimates whether the effect of political interest varies by partisanship
# for a specific housing solution

on22 %>% 
  select(Q33a_1_x:Q33a_6_x,Q80_1_x: Q80_6_x, partisanship, avg_interest) %>%
  #pivot everything *except* independent variables
  pivot_longer(., cols=c(Q33a_1_x:Q80_6_x)) %>% 
  #nest on the dependent variable name
  # to fit one model per question
  nest(data= -name) %>% 
  mutate(model=map(data, ~lm(value~partisanship*avg_interest, data = .x))) -> model.list9


# Generate a regression table summarizing Model 9
partisanshipxinterest_and_solutions <- modelsummary(
  model.list9$model,
  output = "gt",
  stars  = TRUE,
  fmt    = 2,
  
  # Omit goodness-of-fit statistics for a cleaner presentation
  gof_omit = "AIC|BIC|Log|F|R2|RMSE",
  
  # Rename coefficients for readability
  coef_rename = c(
    "partisanshipLiberal"      = "LIB",
    "partisanshipNDP"          = "NDP",
    "partisanshipOther"        = "OTH",
    "partisanshipNon-partisan" = "Non-Partisan",
    "avg_interest"             = "Interest in Politics"
  )
) |>
  
  # Replace default column labels with descriptive housing solution names
  gt::cols_label(
    "(1)"  = "More affordable public housing",
    "(2)"  = "Taxes for owning multiple houses",
    "(3)"  = "Increasing taxes for foreign home-buyers",
    "(4)"  = "More non-single housing properties",
    "(5)"  = "Require developers to build more affordable housing",
    "(6)"  = "Add more properties to existing units",
    "(7)"  = "Reduce heritage designation laws",
    "(8)"  = "Eliminate density and height restrictions",
    "(9)"  = "Increase housing supply",
    "(10)" = "Government loans for new buyers",
    "(11)" = "Eliminate housing transfer taxes",
    "(12)" = "More rent control"
  )

# Output the table for Model 9
#partisanshipxinterest_and_solutions

# MODEL 10
# Dependent variables: Individual housing solution items
# Independent variables: Renter status, political interest, and their interaction

# Each model tests whether the effect of political interest on support for
# a given housing policy differs between renters and non-renters

on22 %>% 
  select(Q33a_1_x:Q33a_6_x,Q80_1_x: Q80_6_x, Renter, avg_interest) %>%
  #pivot everything *except* independent variables
  pivot_longer(., cols=c(Q33a_1_x:Q80_6_x)) %>% 
  #nest on the dependent variable name
  # to fit one model per question
  nest(data= -name) %>% 
  mutate(model=map(data, ~lm(value~Renter*avg_interest, data = .x))) -> model.list10

# Generate a formatted regression table summarizing all models
# using modelsummary with gt output
renterxinterest_and_solutions <- modelsummary(
  model.list10$model,
  output   = "gt",
  stars    = TRUE,
  fmt      = 2,
  
  # Omit goodness-of-fit statistics for readability
  gof_omit = "AIC|BIC|Log|F|R2|RMSE",
  
  # Rename coefficients for clarity
  coef_rename = c(
    "RenterRenter"   = "Renter",
    "avg_interest"   = "Interest in Politics"
  )
) |>
  
  # Replace generic column labels with descriptive housing solution names
  gt::cols_label(
    "(1)"  = "More affordable public housing",
    "(2)"  = "Taxes for owning multiple houses",
    "(3)"  = "Increasing taxes for foreign homeowners",
    "(4)"  = "More non-single housing properties",
    "(5)"  = "Require developers to build more affordable housing",
    "(6)"  = "Add more properties to existing units",
    "(7)"  = "Reduce heritage designation laws",
    "(8)"  = "Eliminate density and height restrictions",
    "(9)"  = "Increase housing supply",
    "(10)" = "Government loans for new buyers",
    "(11)" = "Eliminate housing transfer taxes",
    "(12)" = "More rent control"
  )


# Output table for Model 10
renterxinterest_and_solutions


#MODEL 11
#DV Interest in Politics X Partisanship
#IVs Gap in Categorical Solutions (3)

mod_supply_25<-lm(supply_gap_market~partisanship*avg_interest, data=on22)
summary(mod_supply_25)
mod_supply_26<-lm(supply_gap_govt~partisanship*avg_interest, data=on22)
summary(mod_supply_26)
mod_supply_27<-lm(supply_gap_demand~partisanship*avg_interest, data=on22)
summary(mod_supply_27)

out<-list(mod_supply_25, mod_supply_26, mod_supply_27)
model_11<-modelsummary(out, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, 
                       coef_rename = c("partisanshipLiberal" = "LIB", "partisanshipNDP" = "NDP", "partisanshipOther" = "OTH", "partisanshipNon-partisan" = "Non-Partisan", "supply_general" = "Housing Supply", "avg_interest" = "Interest in Politics"))|>
  gt::cols_label(
    "(1)" = "Model 1",
    "(2)" = "Model 2",
    "(3)" = "Model 3"
  )
#output table for model 11
model_11

#MODEL 12
#DV Interest in Politics X Renter
#IVs Gap in Categorical Solutions (3)

mod_supply_28<-lm(supply_gap_market~Renter*avg_interest, data=on22)
summary(mod_supply_28)
mod_supply_29<-lm(supply_gap_govt~Renter*avg_interest, data=on22)
summary(mod_supply_29)
mod_supply_30<-lm(supply_gap_demand~Renter*avg_interest, data=on22)
summary(mod_supply_30)

out<-list(mod_supply_28, mod_supply_29, mod_supply_30)
model_12<-modelsummary(out, output = "gt", stars=T, gof_omit = "AIC|BIC|Log|F|R2|RMSE", fmt = 2, 
                       coef_rename = c("partisanshipLiberal" = "LIB", "partisanshipNDP" = "NDP", "partisanshipOther" = "OTH", "partisanshipNon-partisan" = "Non-Partisan", "supply_general" = "Housing Supply", "avg_interest" = "Interest in Politics"))|>
  gt::cols_label(
    "(1)" = "Model 1",
    "(2)" = "Model 2",
    "(3)" = "Model 3"
  )
#output table for model 12
model_12


#### Models for Home Owner Rationality ####
OUTCOME_VARS <- c(paste0("Q33a_", 1:6, "_x"), paste0("Q80_", 1:6, "_x"))
CONTROLS <- c("age", "gender", "Degree")
on22 <- on22 %>% 
  mutate(Degree = relevel(factor(Degree), ref = "No degree"))
Rationaly_models <- list()

for(i in 1:length(OUTCOME_VARS)){
  Rationaly_models[[OUTCOME_VARS[i]]]$Renter <- lm_robust(reformulate(c("Renter", CONTROLS),
                                                                                 response = OUTCOME_VARS[i]),
  data = on22)
  
  Rationaly_models[[OUTCOME_VARS[i]]]$Landlord <- lm_robust(reformulate(c("LandLord", CONTROLS),
                                                                                   response = OUTCOME_VARS[i]),
                                                                       data = on22)
  
  Rationaly_models[[OUTCOME_VARS[i]]]$Partisan <- lm_robust(reformulate(c("partisanship", CONTROLS),
                                                                                 response = OUTCOME_VARS[i]),
  data = on22)
  
  Rationaly_models[[OUTCOME_VARS[i]]]$PartisanxRenter <- lm_robust(reformulate(c("Renter * partisanship", CONTROLS),
                                                                                   response = OUTCOME_VARS[i]),
                                                                       data = on22)
  
  Rationaly_models[[OUTCOME_VARS[i]]]$PartisanxLandlord <- lm_robust(reformulate(c("LandLord * partisanship", CONTROLS),
                                                                                   response = OUTCOME_VARS[i]),
                                                                       data = on22)
}

Rationality_df <- data.frame()
for(i in 1:length(OUTCOME_VARS)){
  
df_r <- tidy(Rationaly_models[[i]]$Renter) %>% 
  mutate(Var = "Renter")

df_l <- tidy(Rationaly_models[[i]]$Landlord) %>% 
  mutate(Var = "Landlord")

df_p <- tidy(Rationaly_models[[i]]$Partisan) %>% 
  mutate(Var = "Partisan")

Rationality_df <- bind_rows(Rationality_df,
                            df_r,
                            df_l,
                            df_p)
  
}


Rationality_df <- Rationality_df %>% 
   filter(term %in% c("RenterRenter", "LandLord", "partisanshipNDP",
                      "partisanshipLiberal", "partisanshipOther")) 
  
Renter_landlord_plot <- Rationality_df %>% 
  filter(term %in% c("RenterRenter", "LandLord")) %>% 
  mutate(term = recode_values(term, "RenterRenter" ~ "Renter Status",
                              "LandLord" ~ "Landlord Status"),
         outcome = recode_values(outcome, "Q33a_1_x" ~ "Increased public investment in affordable housing (1)",
                          "Q33a_2_x" ~ "Introduce a tax on vacant and second homes (2)",
                          "Q33a_3_x" ~ "Increase the non-resident speculation tax on foreign buyers of homes (3)", 
                          "Q33a_4_x" ~ "Abolish municipal rules that only allow single family homes (4)",
                          "Q33a_5_x" ~ "Require developers to build 1 affordable home for every 5 new houses or condominium units (5)",
                          "Q33a_6_x" ~ "Make it easier for individual property owners to add housing units (6)",
                          "Q80_1_x" ~ "Weaken heritage designation rules in municipalities (7)",
                          "Q80_2_x" ~ "Eliminate density and height restrictions close to transit stations (8)",
                          "Q80_3_x" ~ "Increasing the supply of housing by building new homes in the next 10 years (9)",
                          "Q80_4_x" ~ "Establish government loans to help new buyers afford a down payment (10)",
                          "Q80_5_x" ~ "Eliminate the land transfer tax on home sales (11)",
                          "Q80_6_x" ~ "Expand rent control (12)"
         ),
         outcome = factor(outcome, levels = rev(c("Increased public investment in affordable housing (1)",
                                              "Introduce a tax on vacant and second homes (2)",
                                              "Increase the non-resident speculation tax on foreign buyers of homes (3)", 
                                              "Abolish municipal rules that only allow single family homes (4)",
                                              "Require developers to build 1 affordable home for every 5 new houses or condominium units (5)",
                                              "Make it easier for individual property owners to add housing units (6)",
                                              "Weaken heritage designation rules in municipalities (7)",
                                              "Eliminate density and height restrictions close to transit stations (8)",
                                              "Increasing the supply of housing by building new homes in the next 10 years (9)",
                                              "Establish government loans to help new buyers afford a down payment (10)",
                                              "Eliminate the land transfer tax on home sales (11)",
                                              "Expand rent control (12)")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = outcome, col = term)) + 
  geom_point(position = position_dodge(width = 0.6)) +
  geom_linerange(position = position_dodge(width = 0.6)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") + 
  scale_colour_manual(values = c("darkgreen", "orange2")) + 
  guides(colour =  guide_legend(reverse = TRUE)) + 
  theme_bw() + 
  labs(x = "OLS Estimates and 95% Confidence Intervals", y = "Outcome Variable", col = NULL) + 
  theme(legend.position = "bottom")

ggsave("plots/Renter_landlord_plot.png", Renter_landlord_plot, width = 10, height = 6)

#### Model Summary Renter Models ####

Renter_models <- list()
for(i in 1:length(OUTCOME_VARS)){
  Renter_models[[i]] <- Rationaly_models[[i]]$Renter
}
modelsummary(Renter_models, stars = TRUE,
             coef_map = c("RenterRenter" = "Renter Status",
                          "age" = "Age",
                          "gender" = "Man",
                          "DegreeDegree" = "University Degree"
                          ),
             output = "tables/renter_models.html")

#### Model Summary Landlord Models ####

landlord_models <- list()
for(i in 1:length(OUTCOME_VARS)){
  landlord_models[[i]] <- Rationaly_models[[i]]$Landlord
}
modelsummary(landlord_models, stars = TRUE,
             coef_map = c("LandLord" = "Landlord Status",
                          "age" = "Age",
                          "gender" = "Man",
                          "DegreeDegree" = "University Degree"
             ),
             output = "tables/landlord_models.html"
             )

#### Partisanship ####

Partisanship_plot <- Rationality_df %>% 
  filter(Var == "Partisan") %>% 
  mutate(term = recode_values(term, 
                              "partisanshipNDP" ~ "NDP",
                              "partisanshipLiberal" ~ "Liberal",
                              "partisanshipOther" ~ "Other"
                              ),
         outcome = recode_values(outcome, "Q33a_1_x" ~ "Increased public investment in affordable housing (1)",
                                 "Q33a_2_x" ~ "Introduce a tax on vacant and second homes (2)",
                                 "Q33a_3_x" ~ "Increase the non-resident speculation tax on foreign buyers of homes (3)", 
                                 "Q33a_4_x" ~ "Abolish municipal rules that only allow single family homes (4)",
                                 "Q33a_5_x" ~ "Require developers to build 1 affordable home for every 5 new houses or condominium units (5)",
                                 "Q33a_6_x" ~ "Make it easier for individual property owners to add housing units (6)",
                                 "Q80_1_x" ~ "Weaken heritage designation rules in municipalities (7)",
                                 "Q80_2_x" ~ "Eliminate density and height restrictions close to transit stations (8)",
                                 "Q80_3_x" ~ "Increasing the supply of housing by building new homes in the next 10 years (9)",
                                 "Q80_4_x" ~ "Establish government loans to help new buyers afford a down payment (10)",
                                 "Q80_5_x" ~ "Eliminate the land transfer tax on home sales (11)",
                                 "Q80_6_x" ~ "Expand rent control (12)"
         ),
         outcome = factor(outcome, levels = rev(c("Increased public investment in affordable housing (1)",
                                                  "Introduce a tax on vacant and second homes (2)",
                                                  "Increase the non-resident speculation tax on foreign buyers of homes (3)", 
                                                  "Abolish municipal rules that only allow single family homes (4)",
                                                  "Require developers to build 1 affordable home for every 5 new houses or condominium units (5)",
                                                  "Make it easier for individual property owners to add housing units (6)",
                                                  "Weaken heritage designation rules in municipalities (7)",
                                                  "Eliminate density and height restrictions close to transit stations (8)",
                                                  "Increasing the supply of housing by building new homes in the next 10 years (9)",
                                                  "Establish government loans to help new buyers afford a down payment (10)",
                                                  "Eliminate the land transfer tax on home sales (11)",
                                                  "Expand rent control (12)"))),
         term = factor(term, levels = rev(c("Liberal", "NDP", "Other")))
         ) %>% 
  ggplot(aes(x = estimate, y = outcome, xmin = conf.low, xmax = conf.high, col = term)) + 
  geom_point(position = position_dodge(width = 0.8)) + 
  geom_linerange(position = position_dodge(width = 0.8)) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") + 
  scale_colour_manual(values = rev(c("darkred", "orange", "green4"))) + 
  guides(colour =  guide_legend(reverse = TRUE, ncol = 2)) + 
  theme_bw() + 
  labs(x = "OLS Estimates and 95% Confidence Intervals", y = "Outcome Variable", col = "Partisanship (Reference Conservative): ") + 
  theme(legend.position = "bottom")

ggsave("plots/Partisanship_plot.png", Partisanship_plot, width = 10, height = 6)

#### Model Summary for Partisanship Models ####

partisan_models <- list()
for(i in 1:length(OUTCOME_VARS)){
  partisan_models[[i]] <- Rationaly_models[[i]]$Partisan
}

modelsummary(partisan_models, stars = TRUE,
             coef_map = c("partisanshipLiberal" = "Liberal Partisan (Ref. PC)",
                          "partisanshipNDP" = "NDP Partisan",
                          "partisanshipOther" = "Other Partisan",
                          "age" = "Age",
                          "gender" = "Man",
                          "DegreeDegree" = "University Degree"
             ),
             output = "tables/partisanship_models.html")

#### Graph Interaction Models ####

Rationality_interaction <- data.frame()
for(i in 1:length(OUTCOME_VARS)){
df_r <- avg_slopes(Rationaly_models[[i]]$PartisanxRenter, variables = "Renter", by = "partisanship") %>% 
    mutate(Var = "Renter",
           Outcome = OUTCOME_VARS[i])
  
df_l <- avg_slopes(Rationaly_models[[i]]$PartisanxLandlord, variables = "LandLord", by = "partisanship") %>% 
    mutate(Var = "Landlord",
           Outcome = OUTCOME_VARS[i])
  
  
Rationality_interaction <- bind_rows(Rationality_interaction,
                              df_r,
                              df_l)
}


internations_policy_support <- Rationality_interaction %>% 
  as.data.frame() %>% 
  mutate(Outcome = recode_values(Outcome, "Q33a_1_x" ~ "Increased public investment in affordable housing (1)",
                                 "Q33a_2_x" ~ "Introduce a tax on vacant and second homes (2)",
                                 "Q33a_3_x" ~ "Increase the non-resident speculation tax on foreign buyers of homes (3)", 
                                 "Q33a_4_x" ~ "Abolish municipal rules that only allow single family homes (4)",
                                 "Q33a_5_x" ~ "Require developers to build 1 affordable home for every 5 new houses or condominium units (5)",
                                 "Q33a_6_x" ~ "Make it easier for individual property owners to add housing units (6)",
                                 "Q80_1_x" ~ "Weaken heritage designation rules in municipalities (7)",
                                 "Q80_2_x" ~ "Eliminate density and height restrictions close to transit stations (8)",
                                 "Q80_3_x" ~ "Increasing the supply of housing by building new homes in the next 10 years (9)",
                                 "Q80_4_x" ~ "Establish government loans to help new buyers afford a down payment (10)",
                                 "Q80_5_x" ~ "Eliminate the land transfer tax on home sales (11)",
                                 "Q80_6_x" ~ "Expand rent control (12)"
  ),
  Outcome = factor(Outcome, levels = rev(c("Increased public investment in affordable housing (1)",
                                           "Introduce a tax on vacant and second homes (2)",
                                           "Increase the non-resident speculation tax on foreign buyers of homes (3)", 
                                           "Abolish municipal rules that only allow single family homes (4)",
                                           "Require developers to build 1 affordable home for every 5 new houses or condominium units (5)",
                                           "Make it easier for individual property owners to add housing units (6)",
                                           "Weaken heritage designation rules in municipalities (7)",
                                           "Eliminate density and height restrictions close to transit stations (8)",
                                           "Increasing the supply of housing by building new homes in the next 10 years (9)",
                                           "Establish government loans to help new buyers afford a down payment (10)",
                                           "Eliminate the land transfer tax on home sales (11)",
                                           "Expand rent control (12)"))),
  partisanship = factor(partisanship, levels = rev(c("PC", "Liberal", "NDP", "Other")))) %>% 
  ggplot(aes(x = estimate, y = Outcome, xmin = conf.low, xmax = conf.high, col = partisanship)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  facet_wrap(~term) + 
  geom_vline(xintercept = 0, lty = 4, col = "grey40") + 
  scale_colour_manual(values = rev(c("darkblue", "darkred", "orange", "green4"))) + 
  guides(colour =  guide_legend(reverse = TRUE, ncol = 2)) + 
  theme_bw() + 
  labs(x = "Marginal Effect of being a renter or landlord by partisanship \n and 95% confidence intervals", y = "Outcome Variable", col = NULL) + 
  theme(legend.position = "bottom")
  

ggsave("plots/interactions_policy_support.png", internations_policy_support, width = 10, height = 6)

#### Model Summary for Renter x Partisanship ####


renter_partisan_models <- list()
for(i in 1:length(OUTCOME_VARS)){
  renter_partisan_models[[i]] <- Rationaly_models[[i]]$PartisanxRenter
}

modelsummary(renter_partisan_models, stars = TRUE,
             coef_map = c("RenterRenter" = "Renter Status",
                          "partisanshipLiberal" = "Liberal Partisan (Ref. PC)",
                          "partisanshipNDP" = "NDP Partisan",
                          "partisanshipOther" = "Other Partisan",
                          "RenterRenter:partisanshipLiberal" = "Renter x Liberal",
                          "RenterRenter:partisanshipNDP" = "Renter x NDP",
                          "RenterRenter:partisanshipOther" = "Renter x Other Partisan",
                          "age" = "Age",
                          "gender" = "Man",
                          "DegreeDegree" = "University Degree"
             ),
             output = "tables/renter_partisanship_models.html")

#### Model Summary for Landlord x Partisanship ####


landlord_partisan_models <- list()
for(i in 1:length(OUTCOME_VARS)){
  landlord_partisan_models[[i]] <- Rationaly_models[[i]]$PartisanxLandlord
}

modelsummary(landlord_partisan_models, stars = TRUE,
             coef_map = c("LandLord" = "Landlord Status",
                          "partisanshipLiberal" = "Liberal Partisan (Ref. PC)",
                          "partisanshipNDP" = "NDP Partisan",
                          "partisanshipOther" = "Other Partisan",
                          "LandLord:partisanshipLiberal" = "Landlord x Liberal",
                          "LandLord:partisanshipNDP" = "Landlord x NDP",
                          "LandLord:partisanshipOther" = "Landlord x Other Partisan",
                          "age" = "Age",
                          "gender" = "Man",
                          "DegreeDegree" = "University Degree"
             ),
             output = "tables/landlord_partisanship_models.html")
