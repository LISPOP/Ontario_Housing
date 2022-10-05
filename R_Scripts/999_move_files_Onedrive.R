#This saves an SPSS file
#It is only to be run when substantial recodes have been conducted
library(here)
source("R_Scripts/1_data_import.R")
names(on22)
write_sav(on22, 
          path=here(
            "Data", paste0("opes22_",Sys.Date(), ".sav")
            )
          )
file.copy(here('Data', str_extract(list.files(path="Data"), "^opes22_2022-.+[0-9].sav?")), to="/Users/skiss/OneDrive - Wilfrid Laurier University/LISPOP/Surveys/Housing_Survey/Data")


#### Move Graphs over to Dropbox####
#Move all graphs over to Results folder
file.copy(here("Plots", list.files("Plots")), to="/Users/skiss/OneDrive - Wilfrid Laurier University/LISPOP/Surveys/Housing_Survey/Plots", overwrite=T)
here("Plots", list.files("Plots"))

#Move CJPH Plots over to CJPH subfolder
# cjph_plots<-here("Plots", list.files("Plots"))
# file.copy(cjph_plots[str_detect(cjph_plots, "cjph")], to="~/Dropbox/Public_Health/CJPH/Plots", overwrite=T)

# #Move CJPH Tables over
# cjph_tables<-here("Tables", list.files("Tables"))
# cjph_tables
# cjph_tables[str_detect(cjph_tables, "cjph")]
# file.copy(cjph_tables[str_detect(cjph_tables, "cjph")], to="~/Dropbox/Public_Health/CJPH/Tables", overwrite=T)
#### Move Recoded Data File ####


#file.copy(here('data', str_extract(list.files(path="Data"), "^opes22.+[0-9].sav?")), to="~/OneDrive - Wilfrid Laurier University/LISPOP/Surveys/Housing_Survey/Data")
file.copy(here("Data", str_extract(list.files(path="Data"), "^opes22.+[0-9].sav?")), to="~/OneDrive - Wilfrid Laurier University/LISPOP/Surveys/Housing_Survey/Data")


            