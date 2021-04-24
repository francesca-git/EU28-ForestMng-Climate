
###### PREPARATION OF LAND USE DATA TO BE USED AS INPUT IN THE MODEL FOR THE QUANTIFICATION OF THE IMPACTS #####

# Task: Take as input the .csv files with the original areas, tidy them up and match the two models used for 
        # the projection of future land use categories.
# Author: Francesca Rosa
# Date: started in March 2020
# unit of the output data: Mha

setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 
source("./scripts/tidy_areas.R")
source("./scripts/match_areas.R")
source("./scripts/areas_functions.R")

library(pacman)
p_load(dplyr, tidyr, compare, readr, purrr, tibble, stringr, rgdal)  

timber = FALSE
not_rel_wet = FALSE
# timber can be FALSE or TRUE (default = FALSE)
# FALSE = excluding timber plantations from EU forest managements
# TRUE = including timber plantations in EU forest managements for the sensitivity 

 # not_rel_wet can be FALSE or TRUE (default = FALSE)
 # FALSE = excluding not relevant lands and wetlands 
 # TRUE = including not relevant lands and wetlands 

tidy.areas(timber)                          # from the file tidy_areas.R
# Task: Clean the input areas and put them in a suitable structure. 
# The output of this script is a .Rdata file meant to be used as input in match.areas().  
# The results of this function are saved as .Rdata in the folder: /results/areas

match.areas(timber, not_rel_wet)
# Task: Take as input the areas tided up in tidy.areas and match the different datasets 
# The results of this function are saved as .csv data in the folders: 
# /results/areas/notimber or /results/areas/timber

