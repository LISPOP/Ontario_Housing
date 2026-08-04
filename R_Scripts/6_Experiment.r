source("R_Scripts/0_Functions.R")
source("R_Scripts/2_recodes.R")

#### Experiment
names(on22)
# Reorder Development for reporting

DEVELOPMENT_LABELS <- c("single_detached" = "Single Detached \n Houses",
                          "semi_detached" = "Semi-Detached \n Houses",
                          "condo_6_storey" = "6 Storey Condo \n Buildings",
                          "rental_6_storey" = "6 Storey Apartment \n Buildings (Rental)",
                          "condo_15_storey" = "15 Storey Condo \n Buildings",
                          "rental_15_storey" = "15 Storey Apartment \n Buildings (Rental)"
                          )
on22_stacked$Development_Support

#### Mod h1a

REG_VARS <- c("Experimental_Group", "Development")
CONTROLS <- c("age", "male", "income", "Degree")


mod_h1a <- lm_robust(
  reformulate(c(REG_VARS, CONTROLS, "partisanship"), response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  clusters = ResponseId,
  weights = weight) #Clustered by Respondent 

modelsummary(mod_h1a, stars=T)

plot_h1a <- plot_predictions(mod_h1a, by = "partisanship") + 
  theme_bw() +
  labs(x = "Partisan Identity",
       y = "Predicted Support for Housing Developments \n (Standard Errors are Clustered by Respondent)")

 ggsave("plots/h1a.png", height = 4, width = 7, plot_h1a)
 
#### Mod h1b
mod_h1b <- lm_robust(
  reformulate(c(REG_VARS,CONTROLS) ,response = "Development_Support"),
          data = on22_stacked,
          se_type = "CR2", #HC2 SEs are used for experiments 
          clusters = ResponseId,
  weights = weight) #Clustered by Respondent 
modelsummary(mod_h1b, stars=T)

plot_h1b <- tidy(mod_h1b, conf.int = TRUE) %>% 
  filter(term %in% c("Experimental_GroupIndividual", "Experimental_GroupCommunity", "Experimental_GroupNational")) %>% 
  mutate(term = case_match(term,
                           "Experimental_GroupIndividual" ~ "Individual \n Benefits",
                           "Experimental_GroupCommunity" ~ "Community \n Benefits",
                           "Experimental_GroupNational" ~ "Natational \n Benefits"
                           )) %>% 
  ggplot(aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  theme_bw() + 
  geom_vline(xintercept = 0, lty = 4, col = "red") + 
  geom_point() + 
  geom_linerange() +
  labs(x = NULL, y = NULL)

  
ggsave("Plots/h1b.png", plot_h1b, height = 4, width = 7)

on22_stacked <- on22_stacked %>% 
  mutate(Support_development = case_when(Development_Support < 0.5 ~ 0,
                                         Development_Support >= 0.5 ~ 1))
  
mod_h1c_logit <- glm(reformulate(c(REG_VARS,CONTROLS), response = "Support_development"),
                     data = on22_stacked,
                     family = "binomial")

mod_h1c_logit_plot <- plot_predictions(mod_h1c_logit, by = "Development") + theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Predicted Probabilty of Supporting a Development") +
  scale_x_discrete(labels = DEVELOPMENT_LABELS)

ggsave("Plots/h1c_predicted_probabilties.png",
       mod_h1c_logit_plot, height = 4, width = 7)

#### Mod h1c
mod_h1c <- lm_robust(
  reformulate(c(REG_VARS,CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  clusters = ResponseId,
  weights = weight) #Clustered by Respondent 

modelsummary(mod_h1c, stars=T)
#graph_regression(list(main_effect_controls, main_effect), "main_effect")
table(on22_stacked$Development)

plot_predictions(mod_h1c, by = "Development") + theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Predicted Support for Each Development Type") +
  scale_x_discrete(labels = DEVELOPMENT_LABELS)


#### Mod H1d ####

# With Density 
mod_h1d <- lm_robust(
  reformulate(c("Density*Development", REG_VARS[-2],CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  clusters = ResponseId,
  weights = weight) #Clustered by Respondent 
modelsummary(mod_h1d, stars=T)

h1d_plot <- plot_predictions(mod_h1d, by = c("Development", "Density")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Predicted Support for Each Development Type \n by Self-Reported Density") +
  scale_x_discrete(labels = DEVELOPMENT_LABELS) + 
  scale_color_manual(values = c(
    "Urban" = "#0072B2",
    "Suburban" = "#E69F00",
    "Rural" = "#009E73"
  )) + 
  theme(legend.position = "bottom") +
  ylim(0.3, 0.8)

ggsave("Plots/h1d.png", h1d_plot, height = 4, width = 7)

# With % Tower 


built_environment <- "Development * (row_house_pct_da + apartment_in_duplex_pct_da +
                      single_detached_houses_pct_da + semi_detached_house_pct_da + 
                       + apartment_in_building_less_5_pct_da + apartment_in_building_plus_5_pct_da)"

ENVI_VARS <- c("row_house_pct_da", "apartment_in_duplex_pct_da",
                 "single_detached_houses_pct_da", "semi_detached_house_pct_da", 
                 "apartment_in_building_less_5_pct_da", "apartment_in_building_plus_5_pct_da")



on22_stacked <- on22_stacked %>% 
  mutate(pct_towers = apartment_in_building_less_5_pct_da + apartment_in_building_plus_5_pct_da)
table(on22_stacked$pct_towers)
# Plot as predictions 
# mod_h1d_built <- lmer(
#   reformulate(c("built_environment*Development", REG_VARS[-2],CONTROLS, "(1 | DA2021)"), response = "Development_Support"),
#   data = on22_stacked) 

mod_h1b_height <- lmer(
  reformulate(c("Average_Height*Development", REG_VARS[-2],CONTROLS, "(1 | DA2021)", "(1 | ResponseId)"),
              response = "Development_Support"),
  weights = weight,
  data = on22_stacked) 

h1d_grid_height <- datagrid(
  model = mod_h1b_height,
  Average_Height = seq(0, 77, by = 1),
  Development = unique(on22_stacked$Development)
)

h1d_tower_predict <- predictions(mod_h1b_height, h1d_grid_height) %>% 
  as.data.frame()

h1d_plot_height <- h1d_tower_predict %>%
  mutate(Development = case_match(Development, 
                                  "single_detached" ~ "Single Detached \n Houses",
                                  "semi_detached" ~ "Semi-Detached \n Houses",
                                  "condo_6_storey" ~ "6 Storey Condo \n Buildings",
                                  "rental_6_storey" ~ "6 Storey Apartment \n Buildings (Rental)",
                                  "condo_15_storey" ~ "15 Storey Condo \n Buildings",
                                  "rental_15_storey" ~ "15 Storey Apartment \n Buildings (Rental)"),
         Development = factor(Development, levels = c(  "Single Detached \n Houses",
                                                        "Semi-Detached \n Houses",
                                                        "6 Storey Condo \n Buildings", 
                                                        "6 Storey Apartment \n Buildings (Rental)",
                                                        "15 Storey Condo \n Buildings",
                                                        "15 Storey Apartment \n Buildings (Rental)"))) %>% 
  ggplot(aes(x = Average_Height, y = estimate, ymin = conf.low, ymax = conf.high, col = Development, size = Development)) + 
  geom_line() + 
  theme_bw() +
  labs(x = "Percentage of Towers in Respodent's Disemination Area",
       y = "Predicted Support for Each Type of Development",
       col = "Development Type") + 
  theme(legend.position = "bottom") +
  scale_colour_manual(values = c(
    "Single Detached \n Houses" = "#0072B2",         # Blue
    "Semi-Detached \n Houses" = "#56B4E9",           # Light Blue
    "6 Storey Condo \n Buildings" = "#009E73",        # Green
    "6 Storey Apartment \n Buildings (Rental)" = "#F0E442", # Yellow
    "15 Storey Condo \n Buildings" = "#D55E00",       # Orange-Red
    "15 Storey Apartment \n Buildings (Rental)" = "#CC79A7"  # Purple
  )) + 
  scale_size_manual(values = c("Single Detached \n Houses" = 0.75,
                               "Semi-Detached \n Houses" = 0.75,
                               "6 Storey Condo \n Buildings" = 0.5,
                               "6 Storey Apartment \n Buildings (Rental)" = 0.5,
                               "15 Storey Condo \n Buildings" = 0.5,      
                               "15 Storey Apartment \n Buildings (Rental)" = 0.5  )
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 1.5)),
    size = "none"
  )

mod_h1d_tower <- lmer(
  reformulate(c("pct_towers*Development", REG_VARS[-2],CONTROLS, "(1 | DA2021)", "(1 | ResponseId)"),
              response = "Development_Support"),
  weights = weight,
  data = on22_stacked) 

modelsummary(mod_h1d_tower, stars=T)

h1d_grid <- datagrid(
  model = mod_h1d_tower,
  pct_towers = seq(0, 1, length.out = 100),
  Development = unique(on22_stacked$Development)
)

h1d_predict <- predictions(mod_h1d_tower, h1d_grid) %>% 
  as.data.frame()

h1d_plot_tower <- h1d_predict %>%
  mutate(Development = case_match(Development, 
                                  "single_detached" ~ "Single Detached \n Houses",
                                  "semi_detached" ~ "Semi-Detached \n Houses",
                                  "condo_6_storey" ~ "6 Storey Condo \n Buildings",
                                  "rental_6_storey" ~ "6 Storey Apartment \n Buildings (Rental)",
                                  "condo_15_storey" ~ "15 Storey Condo \n Buildings",
                                  "rental_15_storey" ~ "15 Storey Apartment \n Buildings (Rental)"),
         Development = factor(Development, levels = c(  "Single Detached \n Houses",
                                                        "Semi-Detached \n Houses",
                                                        "6 Storey Condo \n Buildings", 
                                                        "6 Storey Apartment \n Buildings (Rental)",
                                                        "15 Storey Condo \n Buildings",
                                                        "15 Storey Apartment \n Buildings (Rental)"))) %>% 
  ggplot(aes(x = pct_towers, y = estimate, ymin = conf.low, ymax = conf.high, col = Development, size = Development)) + 
  geom_line() + 
  theme_bw() +
  labs(x = "Percentage of Towers in Respodent's Disemination Area",
       y = "Predicted Support for Each Type of Development",
       col = "Development Type") + 
  theme(legend.position = "bottom") +
  scale_colour_manual(values = c(
    "Single Detached \n Houses" = "#0072B2",         # Blue
    "Semi-Detached \n Houses" = "#56B4E9",           # Light Blue
    "6 Storey Condo \n Buildings" = "#009E73",        # Green
    "6 Storey Apartment \n Buildings (Rental)" = "#F0E442", # Yellow
    "15 Storey Condo \n Buildings" = "#D55E00",       # Orange-Red
    "15 Storey Apartment \n Buildings (Rental)" = "#CC79A7"  # Purple
  )) + 
  scale_size_manual(values = c("Single Detached \n Houses" = 0.75,
                    "Semi-Detached \n Houses" = 0.75,
                    "6 Storey Condo \n Buildings" = 0.5,
                    "6 Storey Apartment \n Buildings (Rental)" = 0.5,
                    "15 Storey Condo \n Buildings" = 0.5,      
                    "15 Storey Apartment \n Buildings (Rental)" = 0.5  )
                    ) +
  guides(
    colour = guide_legend(override.aes = list(size = 1.5)),
    size = "none"
  )


ggsave("Plots/h1d_tower.png", h1d_plot_tower, width = 7, height = 4)



modelsummary(list("H1a" = mod_h1a,
                  "H1b" = mod_h1b,
                  "H1c (OLS)" = mod_h1c,
                  "H1c (Logistic Regression)" = mod_h1c_logit,
                  "H1d (Population Density)" = mod_h1d,
                  "H1d (Percent Towers)" = mod_h1d_tower),
             stars = TRUE,
             coef_map = c("(Intercept)" = "(Intercept)",
                          "partisanshipLiberal" = "Liberal Partisans \n (Ref. NDP)",
                          "partisanshipOther" = "Green Partisans/Non-partisans",
                          "partisanshipPC" = "PC Partisans",
                          "Experimental_GroupIndividual" = "Individual Benefits",
                          "Experimental_GroupCommunity" = "Community Benefits",
                          "Experimental_GroupNational" = "National Benefits",
                          "Developmentsemi_detached" = "Semi-Detached House \n (Ref. Single Detached House)",
                          "Developmentcondo_6_storey" = "6 Storey Condos",
                          "Developmentrental_6_storey" = "6 Storey Rentals",
                          "Developmentcondo_15_storey" = "15 Storey Condos",
                          "Developmentrental_15_storey" = "15 Storey Rentals",
                          "DensitySuburban" = "Self-Reported Suburban (Ref. Urban)",
                          "DensityRural" = "Rural",
                          "DensitySuburban:Developmentsemi_detached" = "Suburban × Semi-Detached House",
                          "DensityRural:Developmentsemi_detached" = "Rural x Semi-Detached House",
                          "DensitySuburban:Developmentcondo_6_storey" = "Suburban x 6 Storey Condos",
                          "DensityRural:Developmentcondo_6_storey" = "Rural x 6 Storey Condos",
                          "DensitySuburban:Developmentrental_6_storey" = "Suburban x 6 Storey Rentals",
                          "DensityRural:Developmentrental_6_storey" = "Rural x 6 Storey Rentals",
                          "DensitySuburban:Developmentcondo_15_storey" = "Suburban x 15 Storey Condos",
                          "DensityRural:Developmentcondo_15_storey" = "Rural x 15 Storey Condos",
                          "DensitySuburban:Developmentrental_15_storey" = "Suburban x 15 Storey Rentals",
                          "DensityRural:Developmentrental_15_storey" = "Rural x 15 Storey Rental",
                          "pct_towers" = "% Towers in a DA",
                          "pct_towers:Developmentsemi_detached" = "% Towers x Semi-Detached Houses",
                          "pct_towers:Developmentcondo_6_storey" = "% Towers x 6 Storey Condos",
                          "pct_towers:Developmentrental_6_storey" = "% Towers x 6 Storey Rentals",
                          "pct_towers:Developmentcondo_15_storey" = "% Towers x 15 Storey Condos",
                          "pct_towers:Developmentrental_15_storey" = "% Towers x 15 Storey Rentals",
                          "age" = "Age",
                          "maleMale" = "Male",
                          "income" = "Income",
                          "DegreeNo degree" = "No Degree"
             ),
             output = "Tables/temp/h1a-h1c_models.html") 

webshot("Tables/temp/h1a-h1c_models.html", "Tables/h1a-h1c_models.png", vwidth = 1000, vheight = 1000)


#### Neighboring H1e

# should be multi level model 

# second neighbor shows more interesting result halo effect but not significant. 


# on22_stacked <- on22_stacked %>% 
#   mutate(higher = ifelse(pop_density_da_intersect2 > pop_density_da, 1, 0))



on22_stacked <- on22_stacked %>% 
  mutate(higher_density_in1 = ifelse(pop_density_da_intersect1 > pop_density_da, 1, 0),
         higher_density_in2 = ifelse(pop_density_da_intersect2 > pop_density_da, 1, 0))


on22_stacked <- on22_stacked %>% 
   mutate(#pct_towers_intersect1 = apartment_in_building_less_5_pct_da_intersect1 + apartment_in_building_plus_5_pct_da_intersect1,
  #        pct_towers_intersect2 = apartment_in_building_less_5_pct_da_intersect2 + apartment_in_building_plus_5_pct_da_intersect2,
  #   more_towers_in1 = ifelse(pct_towers_intersect1 > pct_towers, 1, 0),
  #   more_towers_in2 = ifelse(pct_towers_intersect2 > pct_towers, 1, 0),
    taller_towers_in1 = ifelse(Average_Height_intersect1 > Average_Height, 1, 0),
    taller_towers_in2 = ifelse(Average_Height_intersect2 > Average_Height, 1, 0))

mod_h1e_density1 <- lmer(reformulate(c("Development*higher_density_in1", "pop_density_da","Experimental_Group", CONTROLS, "(1 | DA2021)", "(1 | ResponseId)"), 
                          response = "Development_Support"),
                weights = weight,
data = on22_stacked
)


mod_h1e_density1_plot <- plot_predictions(mod_h1e_density1, by = c("Development", "higher_density_in1")) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Predicted Support for Each Development Type", col = "Higher Density Disemination Area") +
  scale_x_discrete(labels = DEVELOPMENT_LABELS) + 
  scale_colour_manual(
    values = c("0" = "#D55E00", "1" = "#009E73"),  # Custom colors
    labels = c("0" = "Respodent's DA", "1" = "Neigbouring DA")
  ) +
  ylim(0.4, 0.8) +
  theme(legend.position = "bottom") 

ggsave("Plots/h1e_density1.png", mod_h1e_density1_plot, height = 4, width = 7)

mod_h1e_density2 <- lmer(reformulate(c("Development*higher_density_in2", "pop_density_da","Experimental_Group", CONTROLS, "(1 | DA2021)", "(1 | ResponseId)"), 
                            response = "Development_Support"),
                weights = weight,
                data = on22_stacked
)

mod_h1e_density2_plot <- plot_predictions(mod_h1e_density2, by = c("Development", "higher_density_in2")) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Predicted Support for Each Development Type", col = "Higher Density Disemination Area") +
  scale_x_discrete(labels = DEVELOPMENT_LABELS) + 
  scale_colour_manual(
    values = c("0" = "#D55E00", "1" = "#009E73"),  # Custom colors
    labels = c("0" = "Respodent's DA", "1" = "Next Neigbouring DA")
  ) +
  ylim(0.4, 0.8) +
  theme(legend.position = "bottom") 

ggsave("Plots/h1e_density2.png", mod_h1e_density2_plot, height = 4, width = 7)

#### tower numbers ####

mod_h1e_towers1 <- lmer(reformulate(c("Development*more_towers_in1","pct_towers" ,"Experimental_Group", CONTROLS, "(1 | DA2021)", "(1 | ResponseId)"), 
                          response = "Development_Support"),
                        weights = weight,
              data = on22_stacked
)

mod_h1e_towers1_plot <- plot_predictions(mod_h1e_towers1, by = c("Development", "more_towers_in1")) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Predicted Support for Each Development Type", col = "Disemination Area with More Apartment Towers") +
  scale_x_discrete(labels = DEVELOPMENT_LABELS) + 
  scale_colour_manual(
    values = c("0" = "#D55E00", "1" = "#009E73"),  # Custom colors
    labels = c("0" = "Respodent's DA", "1" = "Neigbouring DA")
  ) +
  ylim(0.4, 0.8) +
  theme(legend.position = "bottom") 

ggsave("Plots/h1e_towers1_plot.png", mod_h1e_towers1_plot, width = 7, height = 4)

mod_h1e_towers2 <- lmer(reformulate(c("Development*more_towers_in2","pct_towers" ,"Experimental_Group", CONTROLS, "(1 | DA2021)", "(1 | ResponseId)"), 
                                  response = "Development_Support"),
                        weights = weight,
                      data = on22_stacked
)

mod_h1e_towers2_plot <- plot_predictions(mod_h1e_towers2, by = c("Development", "more_towers_in2")) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Predicted Support for Each Development Type", col = "Disemination Area with more Apartment Towers") +
  scale_x_discrete(labels = DEVELOPMENT_LABELS) + 
  scale_colour_manual(
    values = c("0" = "#D55E00", "1" = "#009E73"),  # Custom colors
    labels = c("0" = "Respodent's DA", "1" = "Next Neigbouring DA")
  ) +
  ylim(0.4, 0.8) +
  theme(legend.position = "bottom") 

ggsave("Plots/h1e_towers2.png", mod_h1e_towers2_plot, width = 7, height = 4)

#### tower height ####


mod_h1e_height1 <- lmer(reformulate(c("Development*taller_towers_in1","Average_Height" ,"Experimental_Group", CONTROLS, "(1 | DA2021)", "(1 | ResponseId)"), 
                                    response = "Development_Support"),
                        weights = weight,
                        data = on22_stacked
)

DEVELOPMENT_TYPES <- unique(on22_stacked$Development) %>% 
  as.character()
SPATIAL_LAG <- list()

for(i in 1:length(DEVELOPMENT_TYPES)){
 on22_stacked_st <- on22_stacked %>%
   st_as_sf() %>% 
   filter(!st_is_empty(.)) %>% 
   filter(!is.na(Development_Support)) %>% 
   filter(Development == DEVELOPMENT_TYPES[i])


seab<-poly2nb(on22_stacked_st, queen=T)
seaw<-nb2listw(seab, style="W", zero.policy = TRUE)


fit.lag<-lagsarlm(reformulate(c("Average_Height","Experimental_Group"), 
                              response = "Development_Support"),  
                  data = on22_stacked_st, 
                  zero.policy = TRUE,
                  listw = seaw) 

SPATIAL_LAG[[DEVELOPMENT_TYPES[i]]]$model <- fit.lag

fit.lag.effects <- impacts(fit.lag, listw = seaw, R = 999)
fit.lag.effects

SPATIAL_LAG[[DEVELOPMENT_TYPES[i]]]$marginal_effects <- fit.lag.effects

}

mod_h1e_height1_grid <- datagrid(
  model = mod_h1e_height1,
  taller_towers_in1 = c(0, 1),
  Development = unique(on22_stacked$Development)
)

predictions_h1e_height1 <- predictions(mod_h1e_height1, mod_h1e_height1_grid) %>% 
  as.data.frame()

mod_h1e_height1_plot <- predictions_h1e_height1 %>% 
  ggplot(aes(x = Development, y = estimate, ymin = conf.low, ymax = conf.high, col = as.factor(taller_towers_in1))) + 
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Predicted Support for Each Development Type", col = "Disemination Area with Taller Towers") +
  scale_x_discrete(labels = DEVELOPMENT_LABELS) + 
  scale_colour_manual(
    values = c("0" = "#D55E00", "1" = "#009E73"),  # Custom colors
    labels = c("0" = "Respodent's DA", "1" = "Neigbouring DA")
  ) +
  ylim(0.4, 0.8) +
  theme(legend.position = "bottom") 

ggsave("Plots/mod_h1e_height1_plot.png", mod_h1e_height1_plot, width = 7, height = 4)

mod_h1e_height2 <- lmer(reformulate(c("Development*taller_towers_in2","Average_Height" ,"Experimental_Group", CONTROLS, "(1 | DA2021)", "(1 | ResponseId)"), 
                                    response = "Development_Support"),
                        weights = weight,
                        data = on22_stacked
)


mod_h1e_height2_plot <- plot_predictions(mod_h1e_height2, by = c("Development", "taller_towers_in2")) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Predicted Support for Each Development Type", col = "Disemination Area with Taller Towers") +
  scale_x_discrete(labels = DEVELOPMENT_LABELS) + 
  scale_colour_manual(
    values = c("0" = "#D55E00", "1" = "#009E73"),  # Custom colors
    labels = c("0" = "Respodent's DA", "1" = "Next Neigbouring DA")
  ) +
  ylim(0.4, 0.8) +
  theme(legend.position = "bottom") 

ggsave("Plots/mod_h1e_height2_plot.png", mod_h1e_height2_plot, width = 7, height = 4)

modelsummary(list("H1e (Density Neigbouring DAs)" = mod_h1e_density1,
                  "H1e (Density Next Neigbouring DAs)" = mod_h1e_density2,
                  "H1e (% Towers Neigbouring DAs)" = mod_h1e_towers1,
                  "H1e (% Towers Next Neigbouring DAs)" = mod_h1e_density2),
             stars = TRUE,
             coef_map = c("(Intercept)" = "(Intercept)",
                          "higher_density_in1" = "(Next-)Neigbouring DA is Higher",
                          "Developmentsemi_detached:higher_density_in1" = "Semi Detached x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_6_storey:higher_density_in1" = "6 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_6_storey:higher_density_in1" = "6 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_15_storey:higher_density_in1" = "15 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_15_storey:higher_density_in1" = "15 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "higher_density_in2" = "(Next-)Neigbouring DA is Higher",
                          "Developmentsemi_detached:higher_density_in2" = "Semi Detached x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_6_storey:higher_density_in2" = "6 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_6_storey:higher_density_in2" = "6 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_15_storey:higher_density_in2" = "15 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_15_storey:higher_density_in2" = "15 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "more_towers_in1" = "(Next-)Neigbouring DA is Higher",
                          "Developmentsemi_detached:more_towers_in1" = "Semi Detached x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_6_storey:more_towers_in1" = "6 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_6_storey:more_towers_in1" = "6 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_15_storey:more_towers_in1" = "15 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_15_storey:more_towers_in1" = "15 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "more_towers_in2" = "(Next-)Neigbouring DA is Higher",
                          "Developmentsemi_detached:more_towers_in2" = "Semi Detached x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_6_storey:more_towers_in2" = "6 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_6_storey:more_towers_in2" = "6 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_15_storey:more_towers_in2" = "15 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_15_storey:more_towers_in2" = "15 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "partisanshipLiberal" = "Liberal Partisans \n (Ref. NDP)",
                          "partisanshipOther" = "Green Partisans/Non-partisans",
                          "partisanshipPC" = "PC Partisans",
                          "Experimental_GroupIndividual" = "Individual Benefits",
                          "Experimental_GroupCommunity" = "Community Benefits",
                          "Experimental_GroupNational" = "National Benefits",
                          "Developmentsemi_detached" = "Semi-Detached House \n (Ref. Single Detached House)",
                          "Developmentcondo_6_storey" = "6 Storey Condos",
                          "Developmentrental_6_storey" = "6 Storey Rentals",
                          "Developmentcondo_15_storey" = "15 Storey Condos",
                          "Developmentrental_15_storey" = "15 Storey Rentals",
                          "DensitySuburban" = "Self-Reported Suburban (Ref. Urban)",
                          "DensityRural" = "Rural",
                          "DensitySuburban:Developmentsemi_detached" = "Suburban × Semi-Detached House",
                          "DensityRural:Developmentsemi_detached" = "Rural x Semi-Detached House",
                          "DensitySuburban:Developmentcondo_6_storey" = "Suburban x 6 Storey Condos",
                          "DensityRural:Developmentcondo_6_storey" = "Rural x 6 Storey Condos",
                          "DensitySuburban:Developmentrental_6_storey" = "Suburban x 6 Storey Rentals",
                          "DensityRural:Developmentrental_6_storey" = "Rural x 6 Storey Rentals",
                          "DensitySuburban:Developmentcondo_15_storey" = "Suburban x 15 Storey Condos",
                          "DensityRural:Developmentcondo_15_storey" = "Rural x 15 Storey Condos",
                          "DensitySuburban:Developmentrental_15_storey" = "Suburban x 15 Storey Rentals",
                          "DensityRural:Developmentrental_15_storey" = "Rural x 15 Storey Rental",
                          "pct_towers" = "% Towers in a DA",
                          "pct_towers:Developmentsemi_detached" = "% Towers x Semi-Detached Houses",
                          "pct_towers:Developmentcondo_6_storey" = "% Towers x 6 Storey Condos",
                          "pct_towers:Developmentrental_6_storey" = "% Towers x 6 Storey Rentals",
                          "pct_towers:Developmentcondo_15_storey" = "% Towers x 15 Storey Condos",
                          "pct_towers:Developmentrental_15_storey" = "% Towers x 15 Storey Rentals",
                          "age" = "Age",
                          "maleMale" = "Male",
                          "income" = "Income",
                          "DegreeNo degree" = "No Degree"
             ),
             output = "Tables/temp/h1emodels.html") 


webshot("Tables/temp/h1emodels.html", "Tables/h1e_models.png", vwidth = 1000, vheight = 1000)

#### h1f - Main model

mod_h1f_main <- lm_robust(
  reformulate(c("Renter", REG_VARS, CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  weights = weight,
  clusters = ResponseId) #Clustered by Respondent 

modelsummary(mod_h1f_main, stars = TRUE)

#### h1f - renter by development type

mod_h1f_develop <- lm_robust(
  reformulate(c("Renter*Development", REG_VARS[-2],CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  weights = weight,
  clusters = ResponseId) #Clustered by Respondent 

h1f_develop_plot <- plot_predictions(mod_h1f_develop, by = c("Development", "Renter")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Predicted Support for Each Development Type", col = "Disemination Area with more Apartment Towers") +
  scale_x_discrete(labels = DEVELOPMENT_LABELS) + 
  scale_colour_manual(
    values = c("Renter" = "#0072B2", "Other" = "#009E73")
  ) +
  ylim(0.4, 0.8) +
  theme(legend.position = "bottom") 



ggsave("Plots/h2f.png", h1f_develop_plot, height = 4, width = 7)

modelsummary(list("H1f (Pooled Model)" = mod_h1f_main,
                  "H1f (Heterogenous by Development Type)" = mod_h1f_develop),
             stars = TRUE,
             coef_map = c("(Intercept)" = "(Intercept)",
                          "RenterRenter" = "Renter",
                          "RenterRenter:Developmentsemi_detached" = "Renter × Semi-Detached House",
                          "RenterRenter:Developmentcondo_6_storey" = "Renter x 6 Storey Condos",
                          "RenterRenter:Developmentrental_6_storey" = "Renter x 6 Storey Rentals",
                          "RenterRenter:Developmentcondo_15_storey" = "Renter x 15 Storey Condos",
                          "RenterRenter:Developmentrental_15_storey" = "Rental x 15 Storey Rentals",
                          "higher_density_in1" = "(Next-)Neigbouring DA is Higher",
                          "Developmentsemi_detached:higher_density_in1" = "Semi Detached x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_6_storey:higher_density_in1" = "6 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_6_storey:higher_density_in1" = "6 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_15_storey:higher_density_in1" = "15 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_15_storey:higher_density_in1" = "15 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "higher_density_in2" = "(Next-)Neigbouring DA is Higher",
                          "Developmentsemi_detached:higher_density_in2" = "Semi Detached x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_6_storey:higher_density_in2" = "6 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_6_storey:higher_density_in2" = "6 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_15_storey:higher_density_in2" = "15 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_15_storey:higher_density_in2" = "15 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "more_towers_in1" = "(Next-)Neigbouring DA is Higher",
                          "Developmentsemi_detached:more_towers_in1" = "Semi Detached x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_6_storey:more_towers_in1" = "6 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_6_storey:more_towers_in1" = "6 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_15_storey:more_towers_in1" = "15 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_15_storey:more_towers_in1" = "15 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "more_towers_in2" = "(Next-)Neigbouring DA is Higher",
                          "Developmentsemi_detached:more_towers_in2" = "Semi Detached x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_6_storey:more_towers_in2" = "6 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_6_storey:more_towers_in2" = "6 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "Developmentcondo_15_storey:more_towers_in2" = "15 Storey Condos x (Next-)Neigbouring DA is Higher",
                          "Developmentrental_15_storey:more_towers_in2" = "15 Storey Rentals x (Next-)Neigbouring DA is Higher",
                          "partisanshipLiberal" = "Liberal Partisans \n (Ref. NDP)",
                          "partisanshipOther" = "Green Partisans/Non-partisans",
                          "partisanshipPC" = "PC Partisans",
                          "Experimental_GroupIndividual" = "Individual Benefits",
                          "Experimental_GroupCommunity" = "Community Benefits",
                          "Experimental_GroupNational" = "National Benefits",
                          "Developmentsemi_detached" = "Semi-Detached House \n (Ref. Single Detached House)",
                          "Developmentcondo_6_storey" = "6 Storey Condos",
                          "Developmentrental_6_storey" = "6 Storey Rentals",
                          "Developmentcondo_15_storey" = "15 Storey Condos",
                          "Developmentrental_15_storey" = "15 Storey Rentals",
                          "DensitySuburban" = "Self-Reported Suburban (Ref. Urban)",
                          "DensityRural" = "Rural",
                          "DensitySuburban:Developmentsemi_detached" = "Suburban × Semi-Detached House",
                          "DensityRural:Developmentsemi_detached" = "Rural x Semi-Detached House",
                          "DensitySuburban:Developmentcondo_6_storey" = "Suburban x 6 Storey Condos",
                          "DensityRural:Developmentcondo_6_storey" = "Rural x 6 Storey Condos",
                          "DensitySuburban:Developmentrental_6_storey" = "Suburban x 6 Storey Rentals",
                          "DensityRural:Developmentrental_6_storey" = "Rural x 6 Storey Rentals",
                          "DensitySuburban:Developmentcondo_15_storey" = "Suburban x 15 Storey Condos",
                          "DensityRural:Developmentcondo_15_storey" = "Rural x 15 Storey Condos",
                          "DensitySuburban:Developmentrental_15_storey" = "Suburban x 15 Storey Rentals",
                          "DensityRural:Developmentrental_15_storey" = "Rural x 15 Storey Rental",
                          "pct_towers" = "% Towers in a DA",
                          "pct_towers:Developmentsemi_detached" = "% Towers x Semi-Detached Houses",
                          "pct_towers:Developmentcondo_6_storey" = "% Towers x 6 Storey Condos",
                          "pct_towers:Developmentrental_6_storey" = "% Towers x 6 Storey Rentals",
                          "pct_towers:Developmentcondo_15_storey" = "% Towers x 15 Storey Condos",
                          "pct_towers:Developmentrental_15_storey" = "% Towers x 15 Storey Rentals",
                          "age" = "Age",
                          "maleMale" = "Male",
                          "income" = "Income",
                          "DegreeNo degree" = "No Degree"
             ),
             output = "Tables/temp/h1f_models.html") 

webshot("Tables/temp/h1f_models.html", "Tables/h1f_models.png", vwidth = 1000, vheight = 1000)


#### H2 ####

#### H2a 

mod_h2a <- lm_robust(
  reformulate(c("partisanship*Experimental_Group",
                "Development", CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  weights = weight,
  clusters = ResponseId) #Clustered by Respondent 



h2a_plot <- slopes(mod_h2a, variables = "Experimental_Group", by = "partisanship") %>% 
  as.data.frame() %>% 
  mutate(contrast = case_match(contrast, "mean(Community) - mean(Control)" ~ "Community Benefits",
                           "mean(Individual) - mean(Control)" ~ "Individual Benefits",
                           "mean(National) - mean(Control)" ~ "National Benefits")) %>% 
  ggplot(aes(x = partisanship, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_linerange()+
  facet_wrap(~contrast) +
  geom_hline(yintercept = 0, lty = 4, col = "red") +
  labs(y = "Marginal Effect of Treatments by Partisan Identity \n (95% Confidence Intervals are Clustered by Respondent)",
       x = NULL) +
  theme_bw()

ggsave("Plots/h2a.png", h2a_plot, height = 4, width = 7)

#### h2a2

mod_h2a2 <- lm_robust(
  reformulate(c("partisanship*Experimental_Group*Renter",
                "Development", CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  weights = weight,
  clusters = ResponseId) #Clustered by Respondent 


h2a2_plot <- slopes(mod_h2a2, variables = "Experimental_Group", by = c("partisanship", "Renter")) %>% 
  as.data.frame() %>% 
  mutate(contrast = case_match(contrast, "mean(Community) - mean(Control)" ~ "Community Benefits",
                               "mean(Individual) - mean(Control)" ~ "Individual Benefits",
                               "mean(National) - mean(Control)" ~ "National Benefits")) %>% 
  ggplot(aes(x = partisanship, y = estimate, ymin = conf.low, ymax = conf.high, col = Renter)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_linerange(position = position_dodge(width = 0.3))+
  facet_wrap(~contrast) +
  geom_hline(yintercept = 0, lty = 4, col = "red") +
  labs(y = "Marginal Effect of Treatments by Renter Status and Partisan Identity \n (95% Confidence Intervals are Clustered by Respondent)",
       x = NULL) +
  scale_colour_manual(
    values = c("Renter" = "#0072B2", "Other" = "#009E73")
  ) +
  theme_bw()

ggsave("Plots/h2a2.png", h2a2_plot, width = 7, height = 4)


#### H2b 

mod_h2b <- lm_robust(
  reformulate(c("partisanship*Development",
                CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  weights = weight,
  clusters = ResponseId) #Clustered by Respondent 

h2b_plot <- plot_predictions(mod_h2b, condition = c("Development", "partisanship")) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = NULL, y = "Predicted Support for Each Development Type", col = "Partisan Identiy") +
  scale_x_discrete(labels = DEVELOPMENT_LABELS) + 
  scale_colour_manual(
    values = c("NDP" = "#F58220", "Liberal" = "#D71920", "Other" = "darkgrey", "PC" = "#002E6D")
  ) +
  ylim(0.4, 0.8) +
  theme(legend.position = "bottom") 

ggsave("Plots/h2b.png", h2b_plot, height = 4, width = 7)

modelsummary(list("H2a" = mod_h2a,
                  "H2a2" = mod_h2a2,
                  "H2b" = mod_h2b),
             stars = TRUE,
             coef_map = c("(Intercept)" = "(Intercept)",
                          "partisanshipLiberal" = "Liberal Partisans \n (Ref. NDP)",
                          "partisanshipOther" = "Green Partisans/Non-partisans",
                          "partisanshipPC" = "PC Partisans",
                          "Experimental_GroupIndividual" = "Individual Benefits",
                          "Experimental_GroupCommunity" = "Community Benefits",
                          "Experimental_GroupNational" = "National Benefits",
                          "partisanshipLiberal:Experimental_GroupIndividual" = "Liberal x Individual Benefits",
                          "partisanshipOther:Experimental_GroupIndividual" = "Non-Partisan/Green x Individual Benefits",
                          "partisanshipPC:Experimental_GroupIndividual" = "PC x Individual Benefits",
                          "partisanshipLiberal:Experimental_GroupCommunity" = "Liberal x Community Benefits",
                          "partisanshipOther:Experimental_GroupCommunity" = "Non-partisan/Green Partisans x Community Benefits",
                          "partisanshipPC × Experimental_GroupCommunity" = "PC x Community Benefits",
                          "partisanshipLiberal:Experimental_GroupNational" = "Liberal x National Benefits",
                          "partisanshipOther:Experimental_GroupNational" = "Non-partisan/Green Partisans x National Benefits",
                          "partisanshipPC:Experimental_GroupNational" = "PC x National Benefits",
                          "RenterRenter" = "Renter",
                          "partisanshipLiberal:RenterRenter" = "Liberal × Renter",
                          "partisanshipOther:RenterRenter" = "Non-Partisan/Green Partisan x Renter",
                          "partisanshipPC:RenterRenter" = "PC x Renter",
                          "Experimental_GroupIndividual:RenterRenter" = "Renter x Individual Benefits",
                          "Experimental_GroupCommunity:RenterRenter" = "Renter x Community Benefits",
                          "Experimental_GroupNational:RenterRenter" = "Renter x National Benefits",
                          "partisanshipLiberal:Experimental_GroupIndividual:RenterRenter" = "Liberal x Renter x Individual Benefits",
                          "partisanshipOther:Experimental_GroupIndividual:RenterRenter" = "Non-Partisan/Green Partisan x Renter x Individual Benefits",
                          "partisanshipPC:Experimental_GroupIndividual:RenterRenter" = "PC x Renter x Individual Benefits",
                          "partisanshipLiberal:Experimental_GroupCommunity:RenterRenter" = "Liberal x Renter x Community Benefits",
                          "partisanshipOther:Experimental_GroupCommunity:RenterRenter" = "Non-Paritsan/Green Partisan x Renter x Community Benefits",
                          "partisanshipPC:Experimental_GroupCommunity:RenterRenter" = "PC × Renter x Community Benefits",
                          "partisanshipLiberal:Experimental_GroupNational:RenterRenter" = "Liberal x Renter x National Benefits",
                          "partisanshipOther:Experimental_GroupNational:RenterRenter" = "Non-Partisan/Green Partisan x Renter x National Benefits",
                          "partisanshipPC:Experimental_GroupNational:RenterRenter" = "PC x Renter x National Benefits",
                          "Developmentsemi_detached" = "Semi-Detached House \n (Ref. Single Detached House)",
                          "Developmentcondo_6_storey" = "6 Storey Condos",
                          "Developmentrental_6_storey" = "6 Storey Rentals",
                          "Developmentcondo_15_storey" = "15 Storey Condos",
                          "Developmentrental_15_storey" = "15 Storey Rentals",
                          "partisanshipLiberal:Developmentsemi_detached" = "Liberal x Semi-detached Houses",
                          "partisanshipOther:Developmentsemi_detached" = "Non-Partisan/Green Partisan x Semi-detached Houses",
                          "partisanshipPC:Developmentsemi_detached" = "PC x Semi-detached Houses",
                          "partisanshipLiberal:Developmentcondo_6_storey" = "Liberal x 6 Storey Condos",
                          "partisanshipOther:Developmentcondo_6_storey" = "Non-Partisan/Green Partisan x 6 Storey Condos",
                          "partisanshipPC:Developmentcondo_6_storey" = "PC x 6 Storey Condos",
                          "partisanshipLiberal:Developmentrental_6_storey" = "Liberal x 6 Storey Rentals",
                          "partisanshipOther:Developmentrental_6_storey" = "Non-Partisan/Green Party x 6 Storey Rentals",
                          "partisanshipPC:Developmentrental_6_storey" = "PC x 6 Storey Rentals",
                          "partisanshipLiberal:Developmentcondo_15_storey" = "Liberal x 15 Storey Condos",
                          "partisanshipOther:Developmentcondo_15_storey" = "Non-Partisan/Green Party x 15 Storey Condos",
                          "partisanshipPC:Developmentcondo_15_storey" = "PC × 15 Storey Condos",
                          "partisanshipLiberal:Developmentrental_15_storey" = "Liberal x 15 Storey Rentals",
                          "partisanshipOther:Developmentrental_15_storey" = "Non-Partisan/Green Partisan x 15 Storey Rentals",
                          "partisanshipPC:Developmentrental_15_storey" = "PC x 15 Storey Rentals",
                          "DensitySuburban" = "Self-Reported Suburban (Ref. Urban)",
                          "DensityRural" = "Rural",
                          "DensitySuburban:Developmentsemi_detached" = "Suburban × Semi-Detached House",
                          "DensityRural:Developmentsemi_detached" = "Rural x Semi-Detached House",
                          "DensitySuburban:Developmentcondo_6_storey" = "Suburban x 6 Storey Condos",
                          "DensityRural:Developmentcondo_6_storey" = "Rural x 6 Storey Condos",
                          "DensitySuburban:Developmentrental_6_storey" = "Suburban x 6 Storey Rentals",
                          "DensityRural:Developmentrental_6_storey" = "Rural x 6 Storey Rentals",
                          "DensitySuburban:Developmentcondo_15_storey" = "Suburban x 15 Storey Condos",
                          "DensityRural:Developmentcondo_15_storey" = "Rural x 15 Storey Condos",
                          "DensitySuburban:Developmentrental_15_storey" = "Suburban x 15 Storey Rentals",
                          "DensityRural:Developmentrental_15_storey" = "Rural x 15 Storey Rental",
                          "pct_towers" = "% Towers in a DA",
                          "pct_towers:Developmentsemi_detached" = "% Towers x Semi-Detached Houses",
                          "pct_towers:Developmentcondo_6_storey" = "% Towers x 6 Storey Condos",
                          "pct_towers:Developmentrental_6_storey" = "% Towers x 6 Storey Rentals",
                          "pct_towers:Developmentcondo_15_storey" = "% Towers x 15 Storey Condos",
                          "pct_towers:Developmentrental_15_storey" = "% Towers x 15 Storey Rentals",
                          "age" = "Age",
                          "maleMale" = "Male",
                          "income" = "Income",
                          "DegreeNo degree" = "No Degree"
             ),
             output = "Tables/temp/h2_models.html") 

webshot("Tables/temp/h2_models.html", "Tables/h2_models.png", vwidth = 1000, vheight = 1000)

#### DESCRIPTIVE STATISTICS ####

on22 %>% 
  select(age, male, income, Degree, partisanship, Experimental_Group) %>% 
  tbl_summary(by = Experimental_Group,
              label = list(age ~ "Age",
                           male ~ "Gender",
                           income ~ "Income",
                          Degree ~ "Degree Status",
                          partisanship ~ 'Partisan Identity')
              ) %>% 
  as_gt() %>% 
  gtsave("Tables/Descriptives.png")


#### BALANCE TABLE ####

on22 <- on22 %>% 
  drop_na(Experimental_Group, age, male, income, Degree, partisanship)

covars <- covars %>% 
  select(age, male, income, Degree, partisanship)
  
bal_table <- bal.tab(on22$Experimental_Group ~ covars,
                     continuous = "std",
                     binary = "std",
                     disp.means = TRUE,
                     disp.sds = TRUE,
                     disp.v.ratio = TRUE,
                     disp.ks = TRUE,
                     abs = TRUE,
                     pairwise = FALSE, 
                     s.weights = on22$weight)

bal_tab_vars <- c("Age", "Male", "Income", "Respodent has a Degree", "NDP Partisan",
                  "Liberal Party Partisan", "Non Partisan", "PC Party Partisan")

bal_std <- bal_table[[1]]$`All vs. Control`[[1]] %>% 
  rownames_to_column() %>%
  mutate(rowname = factor(bal_tab_vars, levels = bal_tab_vars)) %>% 
  ggplot(aes(x = Diff.Un, y = rowname)) + geom_point() + theme_bw() + 
  geom_vline(xintercept = 0.10, lty = 4) + 
  labs(x = "Standardised Mean Differences (SMD)", y = "") 

ggsave("Plots/bal_std.png", bal_std, width = 7, height = 4)

on22_stacked %>% 
  group_by(Development) %>% 
  summarise(Mean = mean(Support_development, na.rm = TRUE), SD = sd(Support_development, na.rm = TRUE))


#### EXPLORATORY ANALYSES ####

#### Treatment by Renter

mod_renter <- lm_robust(
  reformulate(c("Experimental_Group*Renter",
                "Development", CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  weights = weight,
  clusters = ResponseId) #Clustered by Respondent 

plot_slopes(mod_renter, variables = "Experimental_Group", "Renter") + 
  geom_hline(yintercept = 0, lty = 4, col = "red") +
  theme_bw()

#### Treatment by YIMBY 

mod_yimby <- lm_robust(
  reformulate(c("Experimental_Group*YIMBY",
                "Development", CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  weights = weight,
  clusters = ResponseId) #Clustered by Respondent 

plot_slopes(mod_yimby, variables = "Experimental_Group", "YIMBY") + 
  geom_hline(yintercept = 0, lty = 4, col = "red") +
  theme_bw()

#### Treatment by development ###

lm_robust(
  reformulate(c("Experimental_Group*Development", CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  weights = weight,
  clusters = ResponseId) %>%  #Clustered by Respondent 
plot_predictions(by = c("Development","Experimental_Group"))
#### Heterogeneous Effects by Halo Effect ####
# 
# 
# on22_st <- on22_stacked %>% 
#   filter(!is.na(LAT) & !is.na(LONG)) %>% 
#   st_as_sf(., coords = c("LONG", "LAT"), crs = 4326)
# 
# 
# st_crs(on22_st) # Is EPSG:4326 need to transform
# 
# on22_st <- st_transform(on22_st, crs = 3857)
# 
# touch_matrix <- st_touches(on22_st, sparse = FALSE)
# touch_list <- st_touches(on22_st, sparse = TRUE)
# 
# on22_st[touch_matrix %*% rep(1, nrow(touch_matrix)) > 0, ]
# 
# ggplot() +
#   geom_sf(data = on22_geography, color = "blue", lims_method = "geometry_bbox") #+
#   # geom_sf(data = on22_st[unlist(touch_list), ], color = "red") +
#   # theme_minimal()
# 
# 
# # Second attempt with shape way file
# 
#   
# 
# #This nests the data-set up for regressions in on_exp
# #It was my first stab at doing regressions; it wasn't very good. 
# #This has a dataframe of     columns
# #The variable `data` is a data frame of the proper number of observations
# #Each row in this data-set corresponds to the data provided for each response in the experinment
# on22 %>% 
#   pivot_longer(., cols="rental_6_storey":"semi_detached", 
#                names_to="Development", values_to="Development_Support") %>% 
#   nest(-Development)->on_exp
# on_exp
# 
# 
# on22_stacked %>% 
#   mutate(
#     #Create dichotomous variable comparing respondent
#     # support for rental towers
#     # Versus detached houses
#     #all others excluded
#     rental_tower=case_when(
#     str_detect(Development, "rental")~1,
#     str_detect(Development, "detached")~0,
#     TRUE~NA_integer_,
#   ), 
#   #Create dichotomous variable comparing respondent
#   # support for condo towers
#   # Versus detached houses
#   #all others excluded
#   condo=case_when(
#     str_detect(Development, "condo")~1,
#     str_detect(Development, "detached")~0,
#     TRUE~NA_integer_
#   ), 
#   #Create dichotomous variable comparing respondent
#   # support for 6-Storey towers
#   # Versus detached houses
#   #all others excluded
#  midsize=case_when(
#     str_detect(Development, "6")~1,
#     str_detect(Development, "detached")~0,
#     TRUE~NA_integer_
#   ), 
#   )->on22_stacked
# #### The code below was used for producing graphical analysis prior to January 2025####
# on22_stacked %>% 
#   select(Experimental_Group, Development, `Development_Support`) %>% 
#   group_by(Experimental_Group, Development) %>% 
#   summarize(n=n(), Average=mean(`Development_Support`, na.rm=T), sd=sd(`Development_Support`, na.rm=T), se=sd/sqrt(n)) %>% 
#   ggplot(., aes(x=Average, y=fct_reorder(Development, Average, .desc=T), col=Experimental_Group))+
#   #geom_point()+
#   xlim(c(0,1))+
#   scale_y_discrete(limits=rev) +
#   geom_pointrange(size=1.2,aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), position=position_jitter(height=0.25)) +
#   labs(y="", col="Experimental Group")+
#   geom_vline(xintercept=0.5, linetype=2)+
#   theme(legend.position = "bottom") +
#   guides(col=guide_legend(ncol=1))
# ggsave(filename="Plots/experiment_averages_point.png", width=10,  height=8)
# 
# on22_stacked %>% 
#   select(Experimental_Group, Development, `Development_Support`) %>% 
#   group_by(Experimental_Group, Development) %>% 
#   summarize(n=n(), Average=mean(`Development_Support`, na.rm=T), sd=sd(`Development_Support`, na.rm=T), se=sd/sqrt(n)) %>% 
#   write_csv(., file="Experimental_means.csv")
# 
# on22_stacked %>% 
#   select(Experimental_Group, Development, `Development_Support`) %>% 
#   group_by(Experimental_Group, Development) %>% 
#   summarize(n=n(), Average=mean(`Development_Support`, na.rm=T), 
#             sd=sd(`Development_Support`, na.rm=T), se=sd/sqrt(n))  %>% 
# pivot_wider(., names_from="Experimental_Group", values_from=c("Average"), id_cols=c("Development")) %>% 
#   mutate(Percent_change=across(Individual:National, ~.x/Control))
# 
# 
# #Estimate Average Support By Homeowners - Ideology
# 
# on22_stacked %>% 
#   select(Experimental_Group, Development, own_affordable, `Development_Support`) %>% 
#   group_by(Experimental_Group, Development, own_affordable) %>% 
#   filter(!is.na(own_affordable)) %>% 
#   summarize(n=n(), Average=mean(`Development_Support`, na.rm=T), 
#             sd=sd(`Development_Support`, na.rm=T), se=sd/sqrt(n)) %>% 
#   filter(str_detect(own_affordable, "Housing Homeowner")) %>% 
#   ggplot(., aes(x=Average, y=Development, col=Experimental_Group))+
#   geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), position=position_jitter(height=0.1))+
#   facet_wrap(~own_affordable, ncol=2, 
#              labeller = labeller(own_affordable = label_wrap_gen(width = 25)))+xlim(c(0,1))+
#   geom_vline(xintercept=0.5, linetype=2)+
#   theme(legend.position="bottom")+
#   scale_y_discrete(limits=rev)+labs(y="")+guides(col=guide_legend(ncol=2))
# ggsave(filename=here("Plots", "Experiment_development_homeowner_prior_belief.png"), width=12, height=8)
# on22_stacked %>% 
#   select(Experimental_Group, Development, `Development_Support`) %>% 
#   group_by(Experimental_Group, Development) %>% 
#   summarize(n=n(), Average=mean(`Development_Support`, na.rm=T), sd=sd(`Development_Support`, na.rm=T), se=sd/sqrt(n)) %>% 
#   ggplot(., aes(x=Average, y=Development, col=Experimental_Group))+
#   #geom_point()+
#   xlim(c(0,1))+
#   scale_y_discrete(limits=rev) +
#   geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), position=position_jitter(height=0.25)) +
#   labs(y="", x="0=Strongly Oppose, 1=Strongly Support")+
#   geom_vline(xintercept=0.5, linetype=2)+
#   theme(legend.position="bottom")+
#   guides(col=guide_legend(ncol=2))
# ggsave(filename="Plots/experiment_averages_point.png", width=10,  height=6)
# 
# on22_stacked %>% 
#   select(Experimental_Group, Development, `Development_Support`) %>% 
#   group_by(Experimental_Group, Development) %>% 
#   summarize(n=n(), Average=mean(`Development_Support`, na.rm=T), sd=sd(`Development_Support`, na.rm=T), se=sd/sqrt(n)) %>% 
#   write_csv(., file="Experimental_means.csv")
# names(on22_stacked)
# on22_stacked %>% 
#   select(Experimental_Group, Development, `Development_Support`) %>% 
#   group_by(Experimental_Group, Development) %>% 
#   summarize(n=n(), Average=mean(`Development_Support`, na.rm=T), 
#             sd=sd(`Development_Support`, na.rm=T), se=sd/sqrt(n))  %>% 
#   pivot_wider(., names_from="Experimental_Group", values_from=c("Average"), id_cols=c("Development")) %>% 
#   mutate(Percent_change=across(Individual:National, ~.x/Control))
# 
# 
# #Estimate Average Support By Homeowners - Ideology
# 
# on22_stacked %>% 
#   select(Experimental_Group, Development, own_affordable, `Development_Support`) %>% 
#   group_by(Experimental_Group, Development, own_affordable) %>% 
#   filter(!is.na(own_affordable)) %>% 
#   summarize(n=n(), Average=mean(`Development_Support`, na.rm=T), 
#             sd=sd(`Development_Support`, na.rm=T), se=sd/sqrt(n)) %>% 
#   filter(str_detect(own_affordable, "Housing Homeowner")) %>% 
#   ggplot(., aes(x=Average, y=Development, col=Experimental_Group))+
#   geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), position=position_jitter(height=0.1))+
#   facet_wrap(~own_affordable, ncol=2, labeller=labeller(own_affordable=label_wrap_gen(width=22)))+
#   geom_vline(xintercept=0.5, linetype=2)+
#   theme(legend.position="bottom")+
#   guides(col=guide_legend(ncol=2))+
#   scale_y_discrete(limits=rev)+ labs(y="", x="0=Strongly Oppose, 1=Strongly Support")+
#   scale_x_continuous(labels=c("0", "0.25", "0.5", "0.75", "1"))+xlim(c(0,1))
# ggsave(filename=here("Plots", "Experiment_development_homeowner_prior_belief.png"), width=10, height=6)
# 
# 
# on22_stacked %>% 
#   select(Experimental_Group, Development, own_affordable, `Development_Support`) %>% 
#   group_by(Experimental_Group, Development, own_affordable) %>% 
#   filter(!is.na(own_affordable)) %>% 
#   summarize(n=n(), Average=mean(`Development_Support`, na.rm=T), 
#             sd=sd(`Development_Support`, na.rm=T), se=sd/sqrt(n)) %>% 
#   filter(str_detect(own_affordable, "Non-Homeowner")) %>% 
#   ggplot(., aes(x=Average, y=Development, col=Experimental_Group))+
#   geom_pointrange(aes(xmin=Average-(1.96*se), xmax=Average+(1.96*se)), position=position_jitter(height=0.1))+
#   facet_wrap(~own_affordable, ncol=2, labeller=labeller(own_affordable=label_wrap_gen(width=22)))+
#   geom_vline(xintercept=0.5, linetype=2)+
#   theme(legend.position="bottom")+
#   guides(col=guide_legend(ncol=2))+
#   scale_y_discrete(limits=rev)+
#   labs(y="", x="0=Strongly Oppose, 1=Strongly Support")+
#   scale_x_continuous(labels=c("0", "0.25", "0.5", "0.75", "1"))+xlim(c(0,1))
# ggsave(filename=here("Plots", "Experiment_development_renter_prior_belief.png"), width=10, height=6)
# 
# on22_stacked$own_affordable<-relevel(on22_stacked$own_affordable,
#                              "Pro-Affordable Housing Non-Homeowner")
# # 
# # exp_ols1<-function(x) lm(`Development_Support` ~ male+Degree+income_digits+
# #                              Experimental_Group:own_affordable, data=x)
# # on22 %>% 
# # nest(-Development) %>% 
# # mutate(ols1=map(data, function(x) lm(`Development_Support` ~ 
# #                                        male+Degree+income_digits+Experimental_Group:own_affordable, data=x))) %>% 
# #   mutate(ols1_tidied=map(ols1, broom::tidy))->exp_models1
# # 
# # 
# #   exp_models1 %>% 
# #     filter(str_detect(Development, "rental")) ->apartment_models
# #   exp_models1 %>% 
# #     filter(str_detect(Development, "Condominium")) ->condominium_models
# #     exp_models1 %>% 
# #     filter(str_detect(Development, "Single ")) ->single_models
# # 
# #   exp_models1
# # names(exp_models1$ols1)<-exp_models1$Development
# # 
# # table(on22$own_affordable,on22$Experimental_Group)
# # # coefs<-c("Experimental_GroupPrivate:own_affordablePro-Affordable Housing Homeowner"=
# # #            "Private X Pro-Affordable Housing Homeowner", 
# # #          "Experimental_GroupSocial:own_affordablePro-Affordable Housing Homeowner"=
# # #            "Social X Pro-Affordable Housing Homeowner",
# # #          "Experimental_GroupPublic:own_affordablePro-Affordable Housing Homeowner"=
# # #            "Public X Pro-Affordable Housing Homeowner",
# # #          "Experimental_GroupPrivate:own_affordableAnti-Affordable Housing Homeowner"=
# # #            "Private X Anti-Affordable Housing Homeowner", 
# # #          "Experimental_GroupSocial:own_affordableAnti-Affordable Housing Homeowner"=
# # #            "Social X Anti-Affordable Housing Homeowner",
# # #          "Experimental_GroupPublic:own_affordableAnti-Affordable Housing Homeowner"=
# # #            "Public X Anti-Affordable Housing Homeowner"
# # #          )
# # # coefs_renters<-c("Experimental_GroupPrivate:own_affordablePro-Affordable Housing Non-Homeowner"=
# # #            "Private X Pro-Affordable Housing Non-Homeowner", 
# # #          "Experimental_GroupSocial:own_affordablePro-Affordable Housing Non-Homeowner"=
# # #            "Social X Pro-Affordable Housing Non-Homeowner",
# # #          "Experimental_GroupPublic:own_affordablePro-Affordable Housing Non-Homeowner"=
# # #            "Public X Pro-Affordable Housing Non-Homeowner",
# # #          "Experimental_GroupPrivate:own_affordableAnti-Affordable Housing Non-Homeowner"=
# # #            "Private X Anti-Affordable Housing Non-Homeowner", 
# # #          "Experimental_GroupSocial:own_affordableAnti-Affordable Housing Non-Homeowner"=
# # #            "Social X Anti-Affordable Housing Non-Homeowner",
# # #          "Experimental_GroupPublic:own_affordableAnti-Affordable Housing Non-Homeowner"=
# # #            "Public X Anti-Affordable Housing Non-Homeowner"
# # # )
# # 
# # 
# # modelsummary(exp_models1$ols1,
# #              coef_omit=c("!Pro-|Control|Non-Homeowner|Intercept"), stars=T, 
# #              output="flextable", 
# #              fmt=2,gof_omit=c("AIC|BIC|F|Log.Lik|Adj.") ) 
# # 
# # #save_as_docx(., path=here("Tables", "experiment_ideology_owners.docx")
# # modelsummary(exp_models1$ols1,
# #              coef_omit=c("Housing Homeowner"), stars=T, 
# #              output="flextable",fmt=2, gof_omit=c("AIC|BIC|F|Log.Lik|Adj.")) 
# #  # save_as_docx(., path=here("Tables", "experiment_ideology_renters.docx"))
# # 
# # on22 %>% 
# #   select(Experimental_Group, own_affordable, `Development_Support`) %>% 
# #   group_by(Experimental_Group, own_affordable) %>% 
# #   summarize(n=n(), avg=mean(`Development_Support`, na.rm=T))
# 
