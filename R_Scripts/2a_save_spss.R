#This saves an SPSS file
#It is only to be run when substantial recodes have been conducted
library(here)
source("R_Scripts/2_recodes.R")
names(on22)
write_sav(on22, path=here("Data", paste0("opes22_",Sys.Date(), ".sav")))
write_sav(on22_stacked, path=here("Data", paste0("opes22_stacked",Sys.Date(), ".sav")))

file.copy(here('data', str_extract(list.files(path="data"), "^opes22_.+[0-9].sav?")), to="/Users/skiss/OneDrive - Wilfrid Laurier University/LISPOP/Surveys/Housing_Survey/Data")
