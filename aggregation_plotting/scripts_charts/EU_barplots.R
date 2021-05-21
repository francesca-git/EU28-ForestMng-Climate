# Functions:

## plotting function used in all the other functions - plot.EU.barplot

### 1a) EU FOOTPRINT - BARPLOT (energy plantations included) - EUfootprint.barplot.EP
### 1b) IMPACTS OF EU INTERNAL FORESTS - BARPLOT (energy crops excluded, exports included) - EUinternal.barplot.noEPex

### 2) IMPACTS OF EU INTERNAL FORESTS - BARPLOT (energy crops included, exports excluded) - EUinternal.barplot.EPnoex

### 3a) EU FOOTPRINT - BARPLOT (energy plantations excluded, exports excluded) - EUfootprint.barplot.noEP
### 3b) IMPACTS OF EU INTERNAL FORESTS - BARPLOT (energy crops excluded, exports excluded) - EUinternal.barplot.noEPnoex

#############################################################################################################################################################################

# WARNING: The functions defined in this file are called in the file csv_and_plotting.R. The loading/saving paths and the working directory are defined in that file.

#############################################################################################################################################################################


library(ggplot2)
library(colorspace)
library(tidyverse)
library(dplyr)
library(scico)
library(viridis)
library(RColorBrewer)
library(gridExtra)
library(nord)
library(ggpubr)
library(stringr)
select <- dplyr::select
  

################################################### PLOTTING FUNCTION #####################################################################
                    
# This function plots a stacked barplot of European impacts (both footprint and internal), for the two climate scenarios 
# and the multiple forest management scenarios
# the arguments are 
#     data: dataframe with the values for each category 
#     data_top: total value per bar
#     palette_name: name of the palette according to the Brewer color scale

plot.EU.barplot <- function(data, data_top, palette_name) {
  
    #to keep the same order between the scenarios
    data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
    data_top$Scenario <- factor(data_top$Scenario, levels=unique(data_top$Scenario))
    
    # rename the scenarios
    label_scenario <- c("no AFM", "AFM-free", "AFM 25%", "AFM 50%", "AFM 75%", "AFM 100%")  
    
    # rename the groups
    data <- data %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                            Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
    data_top <- data_top %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                             Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
    
    # set the order of the groups
    data$Group <- factor(data$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside"))   
    data_top$Group <- factor(data_top$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside")) 

    # set the maximum value of the y axis according to the max value of the PDF 

      max_data_top <- max(max(data_top$PDFx100), max(data_top$upper95))
      min_data_top <- min(min(data_top$PDFx100), min(data_top$upper95))

      if((round(max_data_top, digits = 3) - max_data_top) < 0.005)
        { ymax_value <- (round(max_data_top, digits = 2) + 0.01)
            } else { ymax_value <- round(max_data_top, digits = 2) }

      if (min_data_top >= 0) {ymin_value = 0}else{
      if((min_data_top - round(min_data_top, digits = 3)) > -0.005)
        { ymin_value <- (round(min_data_top, digits = 2) - 0.01)
      } else { ymin_value <- round(min_data_top, digits = 2) }}

    # ymin_value = 0
    # ymax_value = 0.3

    # plot
    
    figure <-
      ggplot(data)+
      geom_bar(aes(x = Scenario, y = PDFx100, fill = Category), colour = "black",  size = 0.3, width = 0.6, stat = "identity", position = position_stack(reverse = FALSE)) +
      theme_minimal(base_size = 13) + # select the theme of the plot
      theme(legend.position="bottom", 
           legend.text = element_text(size = 10),
           axis.text = element_text(size = 8),
           axis.text.x = element_text(angle = 90),
           axis.title = element_text(size = 10),
           legend.title = element_text(size = 10)) +
      scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      xlab("Scenarios") + ylab("PDF%") +
      scale_fill_brewer(palette = palette_name, direction = -1) +
      #scale_fill_manual(values = pal) +
      ylim(ymin_value, ymax_value) + 
      geom_point(data = data_top, aes(x  = Scenario, y = PDFx100), color = "black", size = 1) +
      theme(panel.border = element_rect(color="grey", fill=NA)) +
      geom_errorbar(data = data_top, aes(x = Scenario, y = PDFx100, ymin = lower95, ymax = upper95), width = 0.05, color = "gray31", size = 0.3) +
      facet_wrap(~ Group) +
      labs(fill = "Land use category") 
    
    return(figure)
    
  }
    


#############################################################################################################################################################################
                                                                                # 1 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 1a) EU FOOTPRINT - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplotS (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: September 2020
# Author: Francesca Rosa

  
  EUfootprint.barplot.EP <- function(csv_path, case_subcase, plots_path, year) {
    
      # folder_slost = folder where the files to be loaded are stored, file_slost = initial part of the name of the file to be loaded, 
      # case_subcase = identification words which selects the file, 
      # plots_path = folder where the plots will be saved

      data <-read.csv(paste0(csv_path, "EUFootprint_", year, case_subcase, "_EP.csv"), header = TRUE)
      data_top <-read.csv(paste0(csv_path, "EUFootprint_", year, case_subcase, "_top_EP.csv"), header = TRUE)
      
      #data <- read.csv(paste0("./plotting/cutoff_timber/EUFootprint_", year, "_mg-det_cutoff_timber.csv"), header = TRUE)
          
      # choose the palette
      palette_name = "YlGn"

      # arrange the data ====
      data <- data %>% mutate(Category = str_replace(Category, "EU28_Energy_crops", "EU28 Energy crops"), 
                              Category = str_replace(Category, "EU28_Forest_net", "EU28 Forests (for internal use)"),
                              Category = str_replace(Category, "Import_Energy_plantations", "Import Energy plantations"), 
                              Category = str_replace(Category, "Import_Forest", "Import Forests"))
      # set the order of the categories
      data$Category <- factor(data$Category, levels = c("EU28 Forests (for internal use)", "Import Forests", "EU28 Energy crops", "Import Energy plantations"))
      
      figure <- plot.EU.barplot(data, data_top, palette_name)
      figure
      # save as pdf
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, case_subcase, "_EP_bottomleg.pdf"), width = 20, height = 16, units = "cm")
      
  }
  


#############################################################################################################################################################################

                                                                  # 1b) EU INTERNAL - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplotS (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: September 2020
# Author: Francesca Rosa

  
  EUinternal.barplot.noEPex <- function(csv_path, case_subcase, plots_path, year) {
  
    data <- read.csv(paste0(csv_path, "EUForest_", year, case_subcase, "_noEPex.csv"), header = TRUE)
    data_top <- read.csv(paste0(csv_path, "EUForest_", year, case_subcase,"_top_noEPex.csv"), header = TRUE)

    palette_name = "Greens" # "#238B45","#74C476", "#BAE4B3", "#EDF8E9"
    #pal = c("#004616", "#238B45","#74C476", "#BAE4B3", "#EDF8E9")
    
    if (grepl("timber", case_subcase, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Timber", "Clear cut", "Other management",  "Retention", "Selection"))
      } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Clear cut", "Other management",  "Retention", "Selection"))
      }
    
    labs = c("Clear cut", "Retention", "Selection", "Other management")
    pal = c("#D7DFEF", "#8CA7D5", "#3872B6", "#2B598E") #oslo
    
    figure <- plot.EU.barplot(data, data_top, palette_name)
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, case_subcase, "_noEPex.pdf"), width = 20, height = 16, units = "cm")
    
  }

#############################################################################################################################################################################
                                                                                # 2 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 2) EU INTERNAL - BARPLOT #

#############################################################################################################################################################################
# General task: plot a figure with four barplotS (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: March 2021
# Author: Francesca Rosa
  
  EUinternal.barplot.EPnoex <- function(csv_path, case_subcase, plots_path, year) {
  
    data <- read.csv(paste0(csv_path, "EUForest_", year, case_subcase, "_EPnoex.csv"), header = TRUE)
    data_top <- read.csv(paste0(csv_path, "EUForest_", year, case_subcase,"_top_EPnoex.csv"), header = TRUE)

    palette_name = "Greens" # "#238B45","#74C476", "#BAE4B3", "#EDF8E9"
    #pal = c("#004616", "#238B45","#74C476", "#BAE4B3", "#EDF8E9")
    
     if (grepl("timber", case_subcase, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Timber", "Clear cut", "Retention", "Selection", "Other management", "Energy crops"))
      } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Clear cut", "Retention", "Selection", "Other management", "Energy crops"))
      }
    
    labs = c("Clear cut", "Retention", "Selection", "Other management", "Energy crops")
    pal = c("#D7DFEF", "#8CA7D5", "#3872B6", "#2B598E", "#152b45") #oslo
    
    figure <- plot.EU.barplot(data, data_top, palette_name)
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, case_subcase, "_EPnoex_bottomleg.pdf"), width = 20, height = 16, units = "cm")
    
  }



#############################################################################################################################################################################
                                                                                # 3 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 3a) EU FOOTPRINT - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplotS (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: March 2021
# Author: Francesca Rosa

  EUfootprint.barplot.noEP <- function(csv_path, case_subcase, plots_path, year) {

      # folder_slost = folder where the files to be loaded are stored, file_slost = initial part of the name of the file to be loaded, 
      # case_subcase = identification words which selects the file, 
      # plots_path = folder where the plots will be saved

      data <-read.csv(paste0(csv_path, "EUFootprint_", year, case_subcase, "_noEP.csv"), header = TRUE)
      data_top <-read.csv(paste0(csv_path, "EUFootprint_", year, case_subcase, "_top_noEP.csv"), header = TRUE)
      
      #data <- read.csv(paste0("./plotting/cutoff_timber/EUFootprint_", year, "_mg-det_cutoff_timber.csv"), header = TRUE)
          
      # choose the palette
      palette_name = "YlGn"

      # arrange the data ====
      data <- data %>% mutate(Category = str_replace(Category, "EU28_Forest_net", "EU28 Forests (for internal use)"),
                              Category = str_replace(Category, "Import_Forest", "Import Forests"))
      
      # set the order of the categories
      data$Category <- factor(data$Category, levels = c("EU28 Forests (for internal use)", "Import Forests"))
      
      figure <- plot.EU.barplot(data, data_top, palette_name)
      figure
      # save as pdf
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, case_subcase, "_noEP.pdf"), width = 20, height = 16, units = "cm")
      
  }
  
  

#############################################################################################################################################################################

                                                                  # 3b) EU INTERNAL - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplotS (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: MArch 2021
# Author: Francesca Rosa

  EUinternal.barplot.noEPnoex <- function(csv_path, case_subcase, plots_path, year) {
  
    data <- read.csv(paste0(csv_path, "EUForest_", year, case_subcase, "_noEPnoex.csv"), header = TRUE)
    data_top <- read.csv(paste0(csv_path, "EUForest_", year, case_subcase,"_top_noEPnoex.csv"), header = TRUE)

    palette_name = "Greens" # "#238B45","#74C476", "#BAE4B3", "#EDF8E9"
    #pal = c("#004616", "#238B45","#74C476", "#BAE4B3", "#EDF8E9")
    
    if (grepl("timber", case_subcase, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Timber", "Clear cut", "Other management",  "Retention", "Selection"))
      } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Clear cut", "Other management",  "Retention", "Selection"))
      }
    
    labs = c("Clear cut", "Retention", "Selection", "Other management")
    pal = c("#D7DFEF", "#8CA7D5", "#3872B6", "#2B598E") #oslo
    
    figure <- plot.EU.barplot(data, data_top, palette_name)
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, case_subcase, "_noEPnoex.pdf"), width = 20, height = 16, units = "cm")
    
  }


