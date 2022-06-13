#Diagnostics
#show Histogram of age
source("R_Scripts/2_recodes.R")
ggplot(on22, aes(x=age))+geom_histogram()+geom_vline(xintercept=c(18, 95))+
  labs(title="Age Distribution, OPES22")
ggsave(filename=here("Plots","age_distribution.png"))


# Diagnose voting not voting for variables
table(as_factor(on22$Q8), as_factor(on22$Q12_1))
table(as_factor(on22$Q10), as_factor(on22$Q12_1))
#Check average survey response time by votinng_flag variable
# How many respondents have 1 on voting_flag
#Tables can be easily exported as html file using kable() and save_kable()
# (https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Getting_Started)