

setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 

library(pacman)
p_load(dplyr, tidyr, abind, tidyverse)  # dataframe management and string management
select <- dplyr::select
rename <- dplyr::rename


working_folder <- "./user/"

############################ INITIAL SETTINGS ######################################

# IMPORTANT: DO NOT CHANGE THEM 

CI = FALSE # WARNING: this user calculation is available only for the static case, without calculating the confidence intervals, otherwise it would take too long.
BS = FALSE 

source(paste0(working_folder, "scripts/load_parameters.R"))
ecodata_path <- "./data/model_parameters/ecoregions_data/" # path of the directory where the data on the model parameters are stored
parameters = load.parameters(ecodata_path)

# IMPORTANT: this script is meant to calculate the global extinction risk and not the regional.

############################ PREPARATION OF THE AREAS - USER'S AREAS ######################################
  
areas <- read.csv("./user/data/areas.csv")
# Please fill out with your data the .csv file available the path defined here above. 
# The columns of the file are the land use categories, the rows the ecoregions. In each cell, enter the area of the corresponding land use per that specific ecoregion (either in Mha, km2 or m2).
# IMPORTANT: do not change the format, the column's names, the columns' order, the row's names, the rows' order. Just enter the numeric values into the corresponding cells.

Areas_org_new <- areas[, 2:3]
Areas_lu <- areas[, 4:length(areas)]


############################ CALCULATION OF THE GLOBAL IMPACTS AS PDF ######################################

source("./scripts/model/calculate_slost.R")
Slost <- calculate.slost(Total_RemainingNatural_areas = Areas_org_new, Land_use_areas = Areas_lu, param = parameters, CI = FALSE, BS = FALSE)[["Aggr_df"]]

# Arguments:
  # Total_RemainingNatural_areas : dataframe with two columns, one with the total original areas, one with the remaining natural areas. The rows correspond to the 
      # ecoregions and are sorted in alphabetical order according to the ecoregion codes (AA0101, AA0102, etc.).
  # Land_use_areas : dataframe which contains one column per each land use category. The rows correspond to the 
      # ecoregions and are sorted in alphabetical order according to the ecoregion codes (AA0101, AA0102, etc.).
  # param: list containing the parameters of the model: "ratio_eco", "weight_tx", "Sorg_VS", "zvalues".
  # CI can be TRUE or FALSE. TRUE = the CI will be calculated, FALSE = the CI will not be calculated.
# Output: 
  # List with the following elements:
  # Aggr_matrix: not relevant for this calculation 
  # Aggr_df: dataframe with the values of the impacts, where rows = Ecoregions and colums = Land use (same as areas).
  # PerTaxon_matrix: not relevant for this calculation
  # PerTaxon_df: not relevant for this calculation 
  # Aggr2.5_matrix: not relevant for this calculation 
  # Aggr97.5_matrix: not relevant for this calculation 

Slost <- Slost %>% relocate(Eco_code, .before = Annual)

write.csv(Slost, paste0(working_folder, "results/Global_extinction_risk_PDF.csv"), row.names = FALSE)




    