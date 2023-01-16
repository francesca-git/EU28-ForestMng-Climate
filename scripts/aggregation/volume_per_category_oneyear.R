
# Author: Francesca Rosa
# Date: June 2022

# To be used after the directories have been set in main.R

############### Aggregate the wood volumes of energy plantations imported in EU28 over GLOBIOM regions #############

library(dplyr, warn.conflicts = FALSE)    # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
library(stringr)                          # string management

aggregate.vol.energy.biomass.import <- function(areas_base_path) {
  
 data <- read.csv(paste0(areas_base_path, "ImportWood_energyplantations.csv"), header = TRUE)
  
  data_aggr <- data %>% group_by(Mitigation_scenario, Forest_use, Management_scenario,	Category) %>%
                summarise_if(is.numeric, sum) %>% data.frame()
    
  names(data_aggr)[5:length(data_aggr)] <- as.character(seq(from = 2000, to = 2100, by = 10))
  
  write.csv(data_aggr, paste0(areas_base_path, "wood_ene-plant-import.csv"), row.names = FALSE)

}


############### Prepare the biomass volume harvested internally and imported to be plotted #############

library(dplyr)    # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
library(stringr)                          # string management


#############################################################################################################################################################################

                                                                  # EU WOOD CONSUMED IN THE EU ALL DISAGGREGATED - BARPLOT #

#############################################################################################################################################################################
  
# General task: create a barplot of harvesting need to meet the EU demand in the various climate mitigation scenarios and forest use scenarios with disaggregation also at EU level
# Date: June 2022
# Author: Francesca Rosa

calculate.volume.EP.dis.oneyear <- function(areas_base_path, csv_path, file_label, year) {
 
  # Load the data
    data <- read.csv(paste0(areas_base_path, "Split_volumes.csv"), header = TRUE)
  
  
  # Clean and prepare the data
    data <- data %>% unite("Group", Mitigation_scenario:Forest_use, sep = "_") %>%
              rename(Scenario = Management_scenario)  
    
    names(data)[4:length(data)] <- as.character(seq(from = 2000, to = 2100, by = 10))
  
    data_oneyear <- data %>% select(Group, Scenario, Category, all_of(year)) 
       names(data_oneyear)[length(data_oneyear)] <- "Values"  
       
    data_oneyear <- data_oneyear %>%
                  pivot_wider(names_from = Category, values_from = Values) %>% data.frame %>%
                      mutate(Clear_cut_EU = Clear_cut_EU - Clear_cut_ex, Selection_EU = Selection_EU - Selection_ex) %>%
                        mutate(Selection_EU = case_when(Selection_EU < 0 ~ 0, TRUE ~ as.numeric(Selection_EU))) %>%
                          select(-Clear_cut_ex, -Selection_ex) %>%
                            pivot_longer(cols = contains("_EU") | contains("_im"), names_to = "Category", values_to = "Values")

    # Adding rows for the additional forest management practices needed to be included in the palette but not contributing to this production (so all values will be set to 0)
    if(grepl("Baseline", file_label, fixed = TRUE)) {
     Sel <- data_oneyear %>% filter(Category == "Clear_cut_EU" | Category == "Retention_EU" ) %>%
                  mutate(Category = str_replace(Category,"Clear_cut_EU", "Selection_im"), Category = str_replace(Category, "Retention_EU", "Selective_im")) %>%
                    mutate(Values = 0)

        data_oneyear <- data_oneyear %>% full_join(Sel)
    }
     
    # Rename the categories
         data_oneyear <- data_oneyear %>% mutate(Category = str_replace(Category, "EP_EU", "EU28 - Lignocel. energy crops"), 
                                Category = str_replace(Category, "Clear_cut_EU", "EU28 - Clear cut"),
                                Category = str_replace(Category, "Retention_EU", "EU28 - Retention"),
                                Category = str_replace(Category, "Selection_EU", "EU28 - Selection"),
                                Category = str_replace(Category, "Timber_plant_EU", "EU28 - Timber"),                                
                                Category = str_replace(Category, "Energy_plantations_im", "Import - Energy plantations"), 
                                Category = str_replace(Category, "Pulp_Timber_Plantation_im", "Import - Timber and pulp plantations"),
                                Category = str_replace(Category, "Clear_cut_im", "Import - Clear cut"),
                                Category = str_replace(Category, "Selection_im", "Import - Selection"),
                                Category = str_replace(Category, "Selective_im", "Import - Selective logging"),
                              Category = str_replace(Category, "ReducedImpactLogging_im", "Import - Reduced Impact Logging"))
         
        data_oneyear <- data_oneyear %>% filter(Category != "EU28 - Timber")

        data_oneyear <- data_oneyear %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_")

        write.csv(data_oneyear, paste0(csv_path, "EUdemand_volumes_", year, file_label, "_disaggr.csv"), row.names = FALSE) 

  
}
