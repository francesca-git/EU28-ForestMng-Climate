
# Author: Francesca Rosa
# Date: June 2022

# To be used after the directories have been set in main.R

############### Aggregate the wood volumes of energy plantations imported in EU28 over GLOBIOM regions #############

library(dplyr, warn.conflicts = FALSE)    # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
library(stringr)                          # string management

aggregate.vol.energy.biomass.import <- function(areas_base_path, marginal) {
  
  if (marginal == TRUE) { data <- read.csv(paste0(areas_base_path, "ImportWood_energyplantations_mg.csv"), header = TRUE)
      } else if (marginal == FALSE) {data <- read.csv(paste0(areas_base_path, "ImportWood_energyplantations_av.csv"), header = TRUE)}
  
  
  data_aggr <- data %>% group_by(Mitigation_scenario, Forest_use, Management_scenario,	Category) %>%
                summarise_if(is.numeric, sum) %>% data.frame()
    
  names(data_aggr)[5:length(data_aggr)] <- as.character(seq(from = 2000, to = 2100, by = 10))
  
  if (marginal == TRUE) {  write.csv(data_aggr, paste0(areas_base_path, "wood_ene-plant-import_mg.csv"), row.names = FALSE)
      } else if (marginal == FALSE) {write.csv(data_aggr, paste0(areas_base_path, "wood_ene-plant-import_av.csv"), row.names = FALSE)}

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
#source("./scripts/plotting/for_WBF/EU_barplots_WBF.R")

#############################################################################################################################################################################
                                                                                # 1 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 1) EU FOREST WOOD VOLUMES - BARPLOT #

#############################################################################################################################################################################

# General task: create a barplot of EU internal harvesting in the various climate mitigation scenarios and forest use scenarios
# Date: June 2022
# Author: Francesca Rosa


EUForest.volume.barplot.EP.dis <- function(areas_base_path, csv_path, plots_path, label_timber, file_label, year, width_height) {
  
  # Load the data
  if(grepl("mg", file_label, fixed = TRUE)) {
    data <- read.csv(paste0(areas_base_path, "Split_volumes_mg.csv"), header = TRUE)
    }  else if (grepl("av", file_label, fixed = TRUE)) {    data <- read.csv(paste0(areas_base_path, "Split_volumes_av.csv"), header = TRUE)
  }
  
  # Clean and prepare the data
  data <- data %>% unite("Group", Mitigation_scenario:Forest_use, sep = "_") %>%
            rename(Scenario = Management_scenario)
  
  names(data)[4:length(data)] <- as.character(seq(from = 2000, to = 2100, by = 10))
  data_oneyear <- data %>% select(Group, Scenario, Category, all_of(year)) 
  names(data_oneyear)[length(data_oneyear)] <- "Values"  
   
  # Filter the data for the year to be plotted and subtract the imports from the internal production
   data_oneyear <- data_oneyear %>% pivot_wider(names_from = Category, values_from = Values) %>% data.frame %>%
                      mutate(Clear_cut_EU = Clear_cut_EU - Clear_cut_ex, Selection_EU = Selection_EU-Selection_ex) %>%
                        mutate(Selection_EU = case_when(Selection_EU < 0 ~ 0, TRUE ~ as.numeric(Selection_EU))) %>%
                        select(Group, Scenario, contains("_EU")) %>%
                          pivot_longer(cols = contains("_EU"), names_to = "Category", values_to = "Values") %>% data.frame()
   
   data_oneyear <- data_oneyear %>% mutate(Category = str_replace(Category, "Clear_cut_EU", "Clear_cut"), Category = str_replace(Category, "Retention_EU", "Retention"), 
                          Category = str_replace(Category, "Timber_plant_EU", "Timber"), Category = str_replace(Category, "Selection_EU", "Selection")) 
      
   # Choose the palette
   pal = viridis_pal(option = "magma")(6)
            palette_name = "Viridis-magma"
            
  # Delete the rows with timber if the timber feature is turned off
      if(grepl("notimber", label_timber, fixed = TRUE)) {
        data_oneyear <- data_oneyear %>% filter(Category != "Timber")
        data_oneyear$Category <- factor(data_oneyear$Category, levels = c("Clear_cut", "Retention", "Selection", "EP_EU"), labels = c("Clear cut", "Retention", "Selection", "Lignocel. energy crops"))
        pal = pal[-1]  
          } else { 
        data_oneyear$Category <- factor(data_oneyear$Category, levels = c("Timber", "Clear_cut", "Retention", "Selection", "EP_EU"), labels = c("Timber", "Clear cut", "Retention", "Selection", "Lignocellulosic energy crops"))
          }
      
    # Plot      
    figure <- plot.EU.barplot(data = data_oneyear, pal = pal, column = "Values", axis_name = "Mm3", file_label = file_label)
      figure
      
    # Save png
    ggsave(paste0(plots_path, "EUForest_volume_Mm3_", year, "_", palette_name, file_label, "_EPnoex_25.png"), width = width_height[1], height = width_height[2], units = "cm")

}

#############################################################################################################################################################################
                                                                                # 2 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 2) WOOD VOLUMES CONSUMED IN THE EU - BARPLOT #

#############################################################################################################################################################################
  
# General task: create a barplot of harvesting need to meet the EU demand in the various climate mitigation scenarios and forest use scenarios
# Date: June 2022
# Author: Francesca Rosa

EUFootprint.volume.barplot.EP <- function(areas_base_path, csv_path, plots_path, label_timber, file_label, year, width_height) {
 
  # Load the data
  if(grepl("mg", file_label, fixed = TRUE)) {
    data <- read.csv(paste0(areas_base_path, "Split_volumes_mg.csv"), header = TRUE)
    }  else if (grepl("av", file_label, fixed = TRUE)) {    data <- read.csv(paste0(areas_base_path, "Split_volumes_av.csv"), header = TRUE)
  }
  
  # Clean and prepare the data
  data <- data %>% unite("Group", Mitigation_scenario:Forest_use, sep = "_") %>% dplyr::rename(Scenario = Management_scenario)
  
  names(data)[4:length(data)] <- as.character(seq(from = 2000, to = 2100, by = 10))
  data_oneyear <- data %>% select(Group, Scenario, Category, all_of(year)) 
  names(data_oneyear)[length(data_oneyear)] <- "Values"  

  data_oneyear <- data_oneyear %>% pivot_wider(names_from = Category, values_from = Values) %>% data.frame %>%
                    mutate(Clear_cut_EU = Clear_cut_EU - Clear_cut_ex, Selection_EU = Selection_EU - Selection_ex) %>%
                      mutate(Selection_EU = case_when(Selection_EU < 0 ~ 0, TRUE ~ as.numeric(Selection_EU))) %>%
                        mutate(Group = Group, Scenario = Scenario, High_intensity_EU = EP_EU + Clear_cut_EU + Timber_plant_EU + Retention_EU, Low_intensity_EU = Selection_EU,
                             High_intensity_im = Energy_plantations_im + Pulp_Timber_Plantation_im + Clear_cut_im,
                             Low_intensity_im = 0) %>%
                              select(-Clear_cut_EU, -Timber_plant_EU, -EP_EU, -Retention_EU, -Selection_EU, -Energy_plantations_im, -Pulp_Timber_Plantation_im, -Clear_cut_im, -Clear_cut_ex, -Selection_ex)
          
   if(grepl("av", file_label, fixed = TRUE)) {
    data_oneyear <- data_oneyear %>% mutate(Low_intensity_im = Selection_im + Selective_im) %>%
      select(-Selection_im, -Selective_im)
   } 
  
  data_oneyear <- data_oneyear %>% pivot_longer(cols = contains("_EU") | contains("_im"), names_to = "Category", values_to = "Values") %>% data.frame()
  
  data_oneyear <- data_oneyear %>% mutate(Category = str_replace(Category, "High_intensity_EU", "EU28 - High-intensity management"), 
                              Category = str_replace(Category, "Low_intensity_EU", "EU28 - Low-intensity management"),
                              Category = str_replace(Category, "High_intensity_im", "Import - High-intensity management"), 
                              Category = str_replace(Category, "Low_intensity_im", "Import - Low-intensity management"))
      
   # Set the order of the categories
      data_oneyear$Category <- factor(data_oneyear$Category, levels =c("Import - Low-intensity management",  "Import - High-intensity management", 
                                                                       "EU28 - Low-intensity management", "EU28 - High-intensity management"))

  # Choose the palette
      
        pal_im = c("#A6DBA0", "#008837")
        pal_EU = c("#C2A5CF", "#7B3294")
        palette_name = "Brewer_VG"
        pal = c(pal_im, pal_EU) 

        data <- data %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_")
    
  # Plot
    figure <- plot.EU.barplot(data = data_oneyear, pal = pal, column = "Values", axis_name = "Mm3", file_label = file_label)
    figure
      
    ggsave(paste0(plots_path, "EUFootprint_volume_Mm3_", year, "_", palette_name, file_label, "_EPnoex_25.png"), width = width_height[1], height = width_height[2], units = "cm")
    
    write.csv(data_oneyear, paste0(csv_path, "EUdemand_volumes_", year, file_label, ".csv"), row.names = FALSE) 

}    

#############################################################################################################################################################################
                                                                                # 3 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 3) EU WOOD CONSUMED IN THE EU ALL DISAGGREGATED - BARPLOT #

#############################################################################################################################################################################
  
# General task: create a barplot of harvesting need to meet the EU demand in the various climate mitigation scenarios and forest use scenarios with disaggregation also at EU level
# Date: June 2022
# Author: Francesca Rosa


EU.volume.barplot.EP.dis <- function(areas_base_path, csv_path, plots_path, label_timber, file_label, year, width_height) {
 
  # Load the data
   if(grepl("mg", file_label, fixed = TRUE)) {
    data <- read.csv(paste0(areas_base_path, "Split_volumes_mg.csv"), header = TRUE)
    }  else if (grepl("av", file_label, fixed = TRUE)) {    data <- read.csv(paste0(areas_base_path, "Split_volumes_av.csv"), header = TRUE)
  }
  
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
    if(grepl("mg", file_label, fixed = TRUE)) {
     Sel <- data_oneyear %>% filter(Category == "Clear_cut_EU" | Category == "Retention_EU" ) %>%
                  mutate(Category = str_replace(Category,"Clear_cut_EU", "Selection_im"), Category = str_replace(Category, "Retention_EU", "Selective_im")) %>%
                    mutate(Values = 0)

        data_oneyear <- data_oneyear %>% full_join(Sel)
    }
     
    # Rename the categories
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
        
        data_oneyear <- data_oneyear %>% filter(Category != "EU28 Timber")

        data_oneyear$Category <- factor(data_oneyear$Category, levels = c(rev(c("Import - Energy plantations", "Import - Timber and pulp plantations", "Import - Clear cut",
                                                        "Import - Selection", "Import - Selective logging")), rev(c("EU28 Lignocel. energy crops", 
                                                        "EU28 Clear cut", "EU28 Retention", "EU28 Selection"))))

      
      palette_name = "Brewer_VG"
      pal_EU = c("#40004B", "#9970AB", "#C2A5CF", "#E7D4E8")
      pal_im = rev(c("#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B"))
      palette_name = "BuPu"
      pal_EU = c("#9EBCDA", "#8C96C6", "#8C6BB1", "#810F7C")
      pal_im = c("#F7FCB9", "#D9F0A3", "#ADDD8E", "#41AB5D", "#238443")   
            
      pal = c(pal_im, pal_EU) 
      
      data_oneyear <- data_oneyear %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_")

    # Plot
        figure <- plot.EU.barplot(data = data_oneyear, pal = pal, column = "Values", axis_name = "Mm3", file_label = file_label)
        figure
    
    # Save as a png
    
    ggsave(paste0(plots_path, "EUFootprint&Forest_volume_Mm3_", year, "_", palette_name, file_label, "_EPnoex.pdf"), width = width_height[1], height = width_height[2], units = "cm")
    
    write.csv(data_oneyear, paste0(csv_path, "EUdemand_volumes_", year, file_label, "_disaggr.csv"), row.names = FALSE) 

  
}
