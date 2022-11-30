# Ontario Provincial Election Survey

This readme document provides an introduction to the Ontario 



## R Code

This repository contains a series of R scripts meant prepare the data for analysis and provide basic diagnostic assessments. The scripts are introduced as follows

1. `1_data_import.R`

This script imports the raw file obtained from Dynata 

2. `2_recodes.R`

This script performs some basic recodes of variables to facilitate data analyses. These variables include a series of composite variables, dichotomous variables and Likert items scaled to run from 0 and 1. At the end of this script the next two scripts are called....

3. `2_value_labels.R` and `2_variable_labels.R`
These scripts modify and value labels and variable labels on new and old variables. The purposes of these scripts are primarily to facilitate the output of useful SPSS and Stata files for collaboration. 

4. `3_diagnostics.R`

This script contains some basic diagnostics on the dataset. Most noteworthy it identified ~100 cases of straightliners in the original data file. *Crucially, it also executes a command that excludes those cases.*

## Preparing for analysis
The scripts from `1_data_import.R` to `3_diagnostics.R` are *nested* so that they run sequentially, in reverse order. Executing `3_diagnostics.R` runs `2_recodes.R` which first runs `1_data_import.R`, then performs its recodes and then calls `2_variable_labels.R` and `2_value_labels.R`

Successfully running `3_diagnostics.R` means the data are ready for any kind of analyses the user wants to run. 

### Adding New Recodes

If users want to create new recoded variables, they should be added in the `2_recodes.R` script. Because `1_data_import.R` is called at the outset of this script, it is easy to quickly see the effects of a recode by just executing the script from top-to-bottom. *It is a self-contained script.* If the user is so inclined, they can add value labels and variable labels int he respective scripts. *It is more helpful to add short, meaningful variable labels that communicate the basics of the survey question than it is to add value labels.*

