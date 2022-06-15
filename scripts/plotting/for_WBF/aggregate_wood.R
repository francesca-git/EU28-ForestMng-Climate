
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
  
  write.csv(data_aggr, paste0(areas_base_path, "wood_ene-plant-import_mg.csv"), row.names = FALSE) 

}



# Author: Francesca Rosa
# Date: June 2022

# To be used after the directories have been set in main.R

############### Prepare the biomass volume harvested internally and imported to be plotted #############

library(dplyr)    # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
library(stringr)                          # string management
source("./scripts/plotting/for_WBF/EU_barplots_WBF.R")

aggregate.plot.vol <- function(areas_base_path, csv_path, plots_path, label_timber, file_label, year) {

  data <- read.csv(paste0(areas_base_path, "EUdemand_split_volumes_mg.csv"), header = TRUE)
  
  data <- data %>% unite("Group", Mitigation_scenario:Forest_use, sep = "_") %>%
            rename(Scenario = Management_scenario)
  
  names(data)[4:length(data)] <- as.character(seq(from = 2000, to = 2100, by = 10))

  ####### EU Forest ########
  
  data_oneyear <- data %>% select(Group, Scenario, Category, all_of(year)) 
  names(data_oneyear)[length(data_oneyear)] <- "Values"  
   
   data_oneyear <- data_oneyear %>% pivot_wider(names_from = Category, values_from = Values) %>% data.frame %>%
              select(Group, Scenario, contains("_EU")) %>%
                pivot_longer(cols = contains("_EU"), names_to = "Category", values_to = "Values") %>% data.frame()
   
   data_oneyear <- data_oneyear %>% mutate(Category = str_replace(Category, "Clear_cut_EU", "Clear_cut"), Category = str_replace(Category, "Retention_EU", "Retention"), 
                          Category = str_replace(Category, "Timber_plant_EU", "Timber"), Category = str_replace(Category, "Selection_EU", "Selection")) 
      
   pal = viridis_pal(option = "magma")(6)
            palette_name = "Viridis-magma"
     
      if(grepl("notimber", label_timber, fixed = TRUE)) {
        data_oneyear <- data_oneyear %>% filter(Category != "Timber")
        data_oneyear$Category <- factor(data_oneyear$Category, levels = c("Clear_cut", "Retention", "Selection", "EP_EU"), labels = c("Clear cut", "Retention", "Selection", "Lignocel. energy crops"))
        pal = pal[-1]  
          } else { 
        data_oneyear$Category <- factor(data_oneyear$Category, levels = c("Timber", "Clear_cut", "Retention", "Selection", "EP_EU"), labels = c("Timber", "Clear cut", "Retention", "Selection", "Lignocellulosic energy crops"))
          }
      
                     
    figure <- plot.EU.barplot(data = data_oneyear, pal = pal, column = "Values", axis_name = "Mm3", file_label = file_label)
      figure
      
    ggsave(paste0(plots_path, "EUForest_volume_Mm3_", year, "_", palette_name, "_", file_label, "_EPnoex.png"), width = 22, height = 15, units = "cm")

    rm(data_oneyear)

  ####### EU Footprint ########
    
  data_oneyear <- data %>% select(Group, Scenario, Category, all_of(year)) 
   names(data_oneyear)[length(data_oneyear)] <- "Values"  

  data_oneyear <- data_oneyear %>% pivot_wider(names_from = Category, values_from = Values) %>% data.frame %>%
              mutate(Forest_net_EU = Clear_cut_EU + Retention_EU + Selection_EU + Timber_plant_EU) %>%
                select(-Clear_cut_EU, -Retention_EU, -Selection_EU, -Timber_plant_EU) %>%
                pivot_longer(cols = contains("_EU") | contains("_im"), names_to = "Category", values_to = "Values") %>% data.frame()
      
    pal = rev(viridis_pal()(7))
      palette_name = "Viridis"
    
       data_oneyear <- data_oneyear %>% mutate(Category = str_replace(Category, "EP_EU", "EU28 Lignocel. energy crops (domestic use)"), 
                              Category = str_replace(Category, "Forest_net_EU", "EU28 Managed forests (domestic use)"),
                              Category = str_replace(Category, "Energy_plantations_im", "Import - Energy plantations"), 
                              Category = str_replace(Category, "Pulp_Timber_Plantation_im", "Import - Timber and pulp plantations"),
                              Category = str_replace(Category, "Clear_cut_im", "Import - Clear cut"),
                              Category = str_replace(Category, "Selection_im", "Import - Selection system"),
                              Category = str_replace(Category, "Selective_im", "Import - Selective logging"))
      
      # set the order of the categories
      data_oneyear$Category <- factor(data_oneyear$Category, levels = c("EU28 Lignocel. energy crops (domestic use)", "EU28 Managed forests (domestic use)", "Import - Energy plantations", "Import - Timber and pulp plantations", "Import - Clear cut",
                                                        "Import - Selection system", "Import - Selective logging"))
                     
     if(grepl("mg", label, fixed = TRUE)) {
        data <- data %>% filter(Category != "Import - Selection system" & Category != "Import - Selective logging")
        #data <- droplevels(data$Category, "Import - Selection system", "Import - Selective logging")
        }
      
      figure <- plot.EU.barplot(data = data_oneyear, pal = pal, column = "Values", axis_name = "Mm3", file_label = file_label)
      figure
      
    ggsave(paste0(plots_path, "EUFootprint_volume_Mm3_", year, "_", palette_name, "_" , file_label, "_EPnoex.png"), width = 27, height = 15, units = "cm")

    rm(data_oneyear)
    
    
  ####### EU Footprint & EU Forest ########
    
  data_oneyear <- data %>% select(Group, Scenario, Category, all_of(year)) 
     names(data_oneyear)[length(data_oneyear)] <- "Values"  
        
      # choose the palette
      pal_FP = rev(viridis_pal()(5))
      palette_name = "Viridis"
      pal_EU = rev(viridis_pal(option = "magma")(7)[3:7])
      palette_name = "Viridis-magma"
      
      Sel <- data_oneyear %>% filter(Category == "Clear_cut_EU" | Category == "Retention_EU" ) %>%
                  mutate(Category = str_replace(Category,"Clear_cut_EU", "Selection_im"), Category = str_replace(Category, "Retention_EU", "Selective_im")) %>%
                    mutate(Values = 0)

        data_oneyear <- data_oneyear %>% full_join(Sel)

      
         data_oneyear <- data_oneyear %>% mutate(Category = str_replace(Category, "EP_EU", "EU28 Lignocel. energy crops"), 
                                Category = str_replace(Category, "Clear_cut_EU", "EU28 Clear cut"),
                                Category = str_replace(Category, "Retention_EU", "EU28 Retention"),
                                Category = str_replace(Category, "Selection_EU", "EU28 Selection"),
                                Category = str_replace(Category, "Timber_plant_EU", "EU28 Timber"),                                
                                Category = str_replace(Category, "Energy_plantations_im", "Import - Energy plantations"), 
                                Category = str_replace(Category, "Pulp_Timber_Plantation_im", "Import - Timber and pulp plantations"),
                                Category = str_replace(Category, "Clear_cut_im", "Import - Clear cut"),
                                Category = str_replace(Category, "Selection_im", "Import - Selection"),
                                Category = str_replace(Category, "Selective_im", "Import - Selective logging"))
        
        # set the order of the categories
         
        data_oneyear <- data_oneyear %>% filter(Category != "EU28 Timber")
        data_oneyear$Category <- factor(data_oneyear$Category, levels = c("Import - Energy plantations", "Import - Timber and pulp plantations", "Import - Clear cut",
                                                        "Import - Selection", "Import - Selective logging",
                                                        "EU28 Lignocel. energy crops", "EU28 Clear cut", "EU28 Retention", "EU28 Selection"))
        pal_EU = pal_EU[-1]
        
        pal = c(pal_FP, pal_EU) 

                       
       # if(grepl("mg", label, fixed = TRUE)) {
       #    data <- data %>% filter(Category != "Import - Selection system" & Category != "Import - Selective logging")
       #    #data <- droplevels(data$Category, "Import - Selection system", "Import - Selective logging")
       #    }
        
        figure <- plot.EU.barplot(data = data_oneyear, pal = pal, column = "Values", axis_name = "Mm3", file_label = file_label)
        figure
        
      ggsave(paste0(plots_path, "EUFootprint&Forest_volume_Mm3_", year, "_", palette_name, "_", file_label, "_EPnoex.png"), width = 27, height = 15, units = "cm")
  
      rm(data_oneyear)    



}
