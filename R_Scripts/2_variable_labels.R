# Variable labels 

on22 %>% 
  select(starts_with("Q31")) %>% 
  var_label()
#Please look in the data dictionary and provide a meaningful, systematic variable label for each one

var_label(on22$Q31_1)<-c("Affordability of $800 per month")