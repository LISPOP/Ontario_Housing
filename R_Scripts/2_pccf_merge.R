#Read in PCCF+

library(here)
library(data.table)
library(readr)
# This defines column widths for the text file that contains the PCCF
column_widths <- c(6,3,2,4,7,70,3,3,3,1,7,2,4,5,4,1,8,3,1,11,13,1,1,30,1,1,8,8,1,3,1,1) 
#This reads in the fixed-width-file containning the PCCF
pccf_fwf <- read_fwf(here("Data/PCCF/TXT/PCCF_FCCP_V2409_2021.txt"), col_positions = fwf_widths(column_widths))

#Names follow order of PCCF Reference Guide (PCCF_202011-eng.pdf); some names include additional "_" ; "DAuid" renamed to "PRCDDA" to match CIMD nomenclature
column_names <- c("Postal_code", "FSA", "PR", "CDuid", "CSDuid", "CSDname", "CSDtype", "CCScode", "SAC", "SACtype", "Ctname","ER", "DPL", "FED13iud", "POP_CNTR_RA", "POP_CNTR_RA_type", "PRCDDA", "Dissemination_block", "Rep_Pt_Type", "LAT", "LONG", "SLI","PCtype", "Comm_Name", "DMT", "H_DMT", "Birth_Date", "Ret_Date", "PO", "QI", "Source", "POP_CENTR_RA_SIZE_CLASS")
# Name the pccf_fwf with the vector column_names
names(pccf_fwf)<-column_names
#check
head(pccf_fwf)
#Filter pccf_fwf to include single SLIs
pccf_fwf %>% 
  #many postal codes straddle the boundaries of a dissemination block.
  # So, StatsCan picks 1 dissemination block that has the majority of the dwellings 
  # in the postal code and assigns it a 1
  # It is crude, but it means you are then assigning a postal code to the most probable
  # Dissemination block that it belongs to. 
  filter(SLI==1) %>% 
  right_join(., on22, by=c("Postal_code"="postal_code"))->on22


