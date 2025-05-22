source("R_Scripts/2_recodes.R")
source("R_Scripts/0_Functions.R")

#### Experiment
names(on22)
# Reorder Development for reporting

on22_stacked$Development

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

plot_predictions(mod_h1a, by = "partisanship")
#### Mod h1b
mod_h1b <- lm_robust(
  reformulate(c(REG_VARS,CONTROLS) ,response = "Development_Support"),
          data = on22_stacked,
          se_type = "CR2", #HC2 SEs are used for experiments 
          clusters = ResponseId,
  weights = weight) #Clustered by Respondent 
modelsummary(mod_h1b, stars=T)

tidy(mod_h1b, conf.int = TRUE) %>% 
  filter(term %in% c("Experimental_GroupIndividual", "Experimental_GroupCommunity", "Experimental_GroupNational")) %>% 
  ggplot(aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  theme_bw() + 
  geom_vline(xintercept = 0, lty = 4, col = "red") + 
  geom_point() + 
  geom_linerange() +
  theme_bw()
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

#### Mod H1d ####

# With Density 
mod_h1d <- lm_robust(
  reformulate(c("Density*Development", REG_VARS[-2],CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  clusters = ResponseId,
  weights = weight) #Clustered by Respondent 
modelsummary(mod_h1d, stars=T)

plot_predictions(mod_h1d, by = c("Development", "Density")) +
  geom_hline(yintercept = 0, lty = 4, col = "red")


# With % Tower 


built_environment <- "Development * (row_house_pct_da + apartment_in_duplex_pct_da +
                      single_detached_houses_pct_da + semi_detached_house_pct_da + 
                       + apartment_in_building_less_5_pct_da + apartment_in_building_plus_5_pct_da)"

ENVI_VARS <- c("row_house_pct_da", "apartment_in_duplex_pct_da",
                 "single_detached_houses_pct_da", "semi_detached_house_pct_da", 
                 "apartment_in_building_less_5_pct_da", "apartment_in_building_plus_5_pct_da")



on22_stacked <- on22_stacked %>% 
  mutate(pct_towers = apartment_in_building_less_5_pct_da + apartment_in_building_plus_5_pct_da)

# Plot as predictions 
# mod_h1d_built <- lmer(
#   reformulate(c("built_environment*Development", REG_VARS[-2],CONTROLS, "(1 | DA2021)"), response = "Development_Support"),
#   data = on22_stacked) 


mod_h1d_tower <- lmer(
  reformulate(c("pct_towers*Development", REG_VARS[-2],CONTROLS, "(1 | DA2021)"),
              response = "Development_Support"),
  weights = weight,
  data = on22_stacked) 

modelsummary(mod_h1d_tower, stars=T)

plot_predictions(mod_h1d_tower, condition = c("Development", "pct_towers")) 



#### Neighboring H1e

# should be multi level model 

# second neighbor shows more interesting result halo effect but not significant. 


# on22_stacked <- on22_stacked %>% 
#   mutate(higher = ifelse(pop_density_da_intersect2 > pop_density_da, 1, 0))



on22_stacked <- on22_stacked %>% 
  mutate(higher_density = ifelse(pop_density_da_intersect1 > pop_density_da, 1, 0))


on22_stacked <- on22_stacked %>% 
  mutate(pct_towers_intersect1 = apartment_in_building_less_5_pct_da_intersect1 + apartment_in_building_plus_5_pct_da_intersect1,
         pct_towers_intersect2 = apartment_in_building_less_5_pct_da_intersect2 + apartment_in_building_plus_5_pct_da_intersect2,
    more_towers_in1 = ifelse(pct_towers_intersect1 > pct_towers, 1, 0),
    more_towers_in2 = ifelse(pct_towers_intersect2 > pct_towers, 1, 0))

mod_h1e <- lmer(reformulate(c("Development*higher_density", "Experimental_Group", CONTROLS, "(1 | DA2021)"), 
                          response = "Development_Support"),
                weights = weight,
data = on22_stacked
)

plot_predictions(mod_h1e, by = c("Development", "higher_density")) + theme_bw()

mod_h1e_towers1 <- lmer(reformulate(c("Development*more_towers_in1", "Experimental_Group", CONTROLS, "(1 | DA2021)"), 
                          response = "Development_Support"),
                        weights = weight,
              data = on22_stacked
)

plot_predictions(mod_h1e_towers1, by = c("Development", "more_towers_in1")) + theme_bw()


mod_h1e_towers2 <- lmer(reformulate(c("Development*more_towers_in2", "Experimental_Group", CONTROLS, "(1 | DA2021)"), 
                                  response = "Development_Support"),
                        weights = weight,
                      data = on22_stacked
)

plot_predictions(mod_h1e_towers2, by = c("Development", "more_towers_in2")) + theme_bw()

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

plot_predictions(mod_h1f_develop, by = c("Development", "Renter"))

#### H2 ####

#### H2a 

mod_h2a <- lm_robust(
  reformulate(c("partisanship*Experimental_Group",
                "Development", CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  weights = weight,
  clusters = ResponseId) #Clustered by Respondent 


plot_slopes(mod_h2a, variables = "Experimental_Group", by = "partisanship") +
  geom_hline(yintercept = 0, lty = 4, col = "red") +
  theme_bw()

#### h2a2

mod_h2a2 <- lm_robust(
  reformulate(c("partisanship*Experimental_Group*Renter",
                "Development", CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  weights = weight,
  clusters = ResponseId) #Clustered by Respondent 


plot_slopes(mod_h2a2, variables = "Experimental_Group", by = c("partisanship", "Renter")) +
  geom_hline(yintercept = 0, lty = 4, col = "red") +
  theme_bw()


#### H2b 

mod_h2b <- lm_robust(
  reformulate(c("partisanship*Development",
                CONTROLS) ,response = "Development_Support"),
  data = on22_stacked,
  se_type = "CR2", #HC2 SEs are used for experiments 
  weights = weight,
  clusters = ResponseId) #Clustered by Respondent 

plot_predictions(mod_h2b, condition = c("Development", "partisanship")) + theme_bw()


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
#   # support for 6-story towers
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
