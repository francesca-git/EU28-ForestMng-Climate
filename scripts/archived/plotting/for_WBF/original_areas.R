
# Author: Francesca Rosa
# Date: June 2022

# To be used after the directories have been set in main.R

############### Aggregate the wood volumes of energy plantations imported in EU28 over GLOBIOM regions #############

library(dplyr, warn.conflicts = FALSE)    # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
library(stringr)                          # string management

aggregate.demand.areas <- function(areas_base_path, marginal) {
  
  if (marginal == TRUE) { data_for_im <- read.csv(paste0(areas_base_path, "Import_forest_area_marginal_Mha.csv"), header = TRUE) # import forest areas (marginal approach), Mha
      } else if (marginal == FALSE) {data_for_im <- read.csv(paste0(areas_base_path, "Import_forest_area_average_Mha.csv"), header = TRUE) # import forest areas (average approach), Mha
      }
  data_for_EU <- read.csv(paste0(areas_base_path, "Internal_EU28_managed_forest_area_1000ha.csv"), header = TRUE) 
  data_en_EU <- read.csv(paste0(areas_base_path, "Internal_EU28_perennial_energy_crops_area_1000ha.csv"), header = TRUE) # EU energy plantations, in 1000ha and not Mha
  data_en_im <- read.csv(paste0(areas_base_path, "Import_pellets_plantation_area_Mha.csv"), header = TRUE) # import energy plantations, Mha
  
  data_aggr <- data %>% group_by(Mitigation_scenario, Forest_use, Management_scenario,	Category) %>%
                summarise_if(is.numeric, sum) %>% data.frame()
    
  names(data_aggr)[5:length(data_aggr)] <- as.character(seq(from = 2000, to = 2100, by = 10))
  
  if (marginal == TRUE) {  write.csv(data_aggr, paste0(areas_base_path, "wood_ene-plant-import_mg.csv"), row.names = FALSE)
      } else if (marginal == FALSE) {write.csv(data_aggr, paste0(areas_base_path, "wood_ene-plant-import_av.csv"), row.names = FALSE)}

}


import_forest_av <- read.csv(paste0(areas_base_path, "Import_forest_area_average_Mha.csv"), header = TRUE) # import forest areas (average approach), Mha
      import_forest_mg <- read.csv(paste0(areas_base_path, "Import_forest_area_marginal_Mha.csv"), header = TRUE) # import forest areas (marginal approach), Mha
      import_eplant <- read.csv(paste0(areas_base_path, "Import_pellets_plantation_area_Mha.csv"), header = TRUE) # import energy plantations, Mha
      EU_eplant <- read.csv(paste0(areas_base_path, "Internal_EU28_perennial_energy_crops_area_1000ha.csv"), header = TRUE) # EU energy plantations, in 1000ha and not Mha
      export_forest_av <- read.csv(paste0(areas_base_path, "Export_forest_area_average_Mha.csv"), header = TRUE) # export forest areas (average approach, no timber plantations), Mha
      EU_forest_temp <- read.csv(paste0(areas_base_path, "Internal_EU28_managed_forest_area_1000ha_timber.csv"), header = TRUE) # EU forest areas. These contain the areas of Clear-cut and of Timber plantations to be subtracted from those of Clear-cut.
      export_forest_mg_temp <- read.csv(paste0(areas_base_path, "Export_marginal_timber_Mha.csv"), header = TRUE) # export forest areas (marginal approach, timber plantations), in 1000ha and not Mha
        EU_forest_temp[is.na(EU_forest_temp)] <- 0
        export_forest_mg_temp[is.na(export_forest_mg_temp)] <- 0
      