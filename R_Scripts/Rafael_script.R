
#### POWER ANALYSIS ####

pacman::p_load(pwr)

  
#### Visualize density ####

on22_geography %>% 
  distinct(CFSAUID, .keep_all = TRUE) %>% 
ggplot() +
  geom_sf(aes(fill = pop_density_da), size = 0.1) +  # Fill FSAs by density, outline in black
  scale_fill_viridis_c(option = "turbo", name = "Density") +  # Use a color gradient
  theme_minimal() +  # Clean theme
  labs(title = "Density by FSA", subtitle = "FSAs colored by density levels")
