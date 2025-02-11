#### GRAPH REGRESSION MODELS ####

graph_regression <- function(models, graphname,
                             columns =  c("Experimental_GroupIndividual", "Experimental_GroupCommunity", "Experimental_GroupNational"),
                             Treatments = c("Individual \n Benefits", "Community \n Benefits", "National \n Benefits")){
 
  Treatments <- rep(Treatments, times = 2)
  r_controls <- tidy(models[[1]]) %>% 
    mutate(Controls = "Controls")
  
  r_no_controls <- tidy(models[[2]]) %>% 
    mutate(Controls = "No Controls")
  
  r <- rbind(r_controls, r_no_controls)
 plot <- r %>% 
    filter(term %in% columns) %>% 
    mutate(term = Treatments) %>% 
  ggplot( aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, col = Controls)) +
    geom_point(position = position_dodge(width = 0.5)) + 
    geom_linerange(position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0, col = "grey", lty = 4) + 
    theme_bw() + 
    scale_colour_manual(values = c("black", "grey")) + 
    guides(fill = guide_legend(reverse = TRUE),
          color = guide_legend(reverse = TRUE)) +
    labs(y = NULL, x = NULL) + xlim(c(-0.1, 0.1)) + 
    theme(legend.position = "bottom",
          text = element_text(size = 12))
 
 ggsave(paste0("plots/", graphname, ".png"), plot, height = 3, width = 10)
 
 plot
}
