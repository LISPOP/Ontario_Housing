#Diagnostics
#show Histogram of age

ggplot(on22, aes(x=age))+geom_histogram()+geom_vline(xintercept=c(18, 95))+
  labs(title="Age Distribution, OPES22")
ggsave(filename=here("Plots","age_distribution.png"))
