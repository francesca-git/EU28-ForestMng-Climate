# Functions:

## plotting function used in all the other functions - plot.EU.barplot

### 1) IMPACTS OF EU INTERNAL FORESTS - BARPLOT (energy crops included, exports excluded, all forest categories disaggregated) - EUinternal.barplot.EPnoex

### 2) EU FOOTPRINT - BARPLOT (energy plantations and energy crops included, exports from the EU excluded, all forest categories disaggregated) - EU.barplot.EP.all.dis

### 3) EU FOREST AREA AND AREAS WHERE THE BIOMASS IMPORTED TO THE EU IS HARVESTED - BARPLOT (energy plantations and energy crops included, exports from the EU excluded, all forest categories disaggregated) - EU.areas.barplot.EP.dis

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
library(rcartocolor)
select <- dplyr::select
library(cowplot)
  
###########################################################################################################################################
################################################### PLOTTING FUNCTION #####################################################################
###########################################################################################################################################

# This function plots a stacked barplot of European impacts (both footprint and internal), for the two climate scenarios 
# and the multiple forest management scenarios
# the arguments are 
#     data: dataframe with the values for each category 
#     data_top: total value per bar
#     pal: name of the palette according to the Brewer color scale
#     column: name of the column with the values to be plotted (character vector)
#     axis_name: label for the y axis (character vector)

plot.EU.barplot <- function(data, data_top, pal, column, axis_name, file_label) {
  
    if(missing(data_top)){}

    # rename the scenarios

    data_bu <- data
    data <- data_bu
    
    data <- data %>% mutate(Pathway = str_replace(Pathway, "REF", "RCP6.5"), Group = str_replace(Group, "SFM", "Set-aside"),
                            Group = str_replace(Group, "MFM", "Close-to-nature")) 

    
    data <- data %>% mutate(Scenario = str_replace(Scenario, "AFM25", "AFM12.5"), Scenario = str_replace(Scenario, "AFM50", "AFM25"),
                            Scenario = str_replace(Scenario, "AFM75", "AFM37.5"), Scenario = str_replace(Scenario, "AFM100", "AFM50")) 
    
    data <- data %>% filter(Scenario != "AFMfree") # & Scenario != "AFM12.5" & Scenario != "AFM37.5")
    label_scenario <- c("noAFM", "AFM 12.5%", "AFM 25%", "AFM 37.5%", "AFM 50%")  
    #label_scenario <- c("no AFM", "Laissez-faire", "AFM 12.5%", "AFM 25%", "AFM 37.5%", "AFM 50%")  

    #to keep the same order between the scenarios
    data$Scenario <- factor(data$Scenario, levels = unique(data$Scenario))
    
    # set the order of the pathways and the groups
    data$Group <- factor(data$Group, levels = c("Close-to-nature", "Set-aside"))
    data$Pathway <- factor(data$Pathway, levels = c("RCP6.5", "RCP2.6"))
    
    
  # Add the points and the CI if the data loaded are labeled accordingly
    if(grepl("PDF", axis_name, fixed = TRUE)) { # bs means bootstrapping
      
      data_top_bu <- data_top
      data_top <- data_top_bu
      
      data_top <- data_top %>% mutate(Pathway = str_replace(Pathway, "REF", "RCP6.5"), Group = str_replace(Group, "SFM", "Set-aside"),
                                      Group = str_replace(Group, "MFM", "Close-to-nature")) 
    
      data_top <- data_top %>% mutate(Scenario = str_replace(Scenario, "AFM25", "AFM12.5"), Scenario = str_replace(Scenario, "AFM50", "AFM25"),
                                Scenario = str_replace(Scenario, "AFM75", "AFM37.5"), Scenario = str_replace(Scenario, "AFM100", "AFM50"))
      
      data_top <- data_top %>% filter(Scenario != "AFMfree") # & Scenario != "AFM12.5" & Scenario != "AFM37.5")

      data_top$Scenario <- factor(data_top$Scenario, levels=unique(data_top$Scenario))
        
      data_top$Group <- factor(data_top$Group, levels = unique(data$Group))
      data_top$Pathway <- factor(data_top$Pathway, levels = unique(data$Pathway))

     
      }

    # set the maximum value of the y axis

    if(grepl("PDF", axis_name, fixed = TRUE) && grepl("bs", file_label, fixed = TRUE)) {
    
        ymax_value = 4.7e-03
        ymin_value = 0
    
      }

      
    if(grepl("PDF", axis_name, fixed = TRUE) && grepl("static", file_label, fixed = TRUE)) {
  
        ymax_value = 3.5e-03
        ymin_value = 0
        br = c(0, 1e-03, 2e-03, 3e-03)
    
      }

    if(grepl("Mha", axis_name, fixed = TRUE)) {

        ymax_value = 350
        ymin_value = 0

      }

    if(grepl("Mm3", axis_name, fixed = TRUE)) {

        ymax_value = 1700
        ymin_value = 0

      }

    if(grepl("PDF", axis_name, fixed = TRUE) && length(pal) < 5) { # for sensitivity analysis
    
        ymax_value = 6.5e-04
        ymin_value = 0 
    
      }

 
    
    figure <-
      ggplot(data)+
      geom_bar(aes(x = Scenario, y = !! sym(column), fill = Category), size = 0.2, width = 0.45, stat = "identity", position = position_stack(reverse = FALSE)) +
      theme_minimal_hgrid() +
      theme(legend.position = "right", 
           legend.text = element_text(size = 10),
           axis.text = element_text(size = 10),
           axis.text.x = element_text(angle = 90),
           axis.title = element_text(size = 10),
           legend.title = element_text(size = 10),
           legend.key.size = unit(0.6, "cm"),
           strip.background = element_rect(fill = "gray"),
           plot.background = element_rect(fill = "white")) +
      guides(fill = guide_legend(title = "Land use category", title.position = "top")) +
      scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      xlab("Scenarios") + ylab(axis_name) +
      scale_fill_manual(values = pal) +
      ylim(ymin_value, ymax_value) + 
      facet_wrap(Pathway ~ Group, nrow = 1) #horizontal
      
    if(grepl("bs", file_label, fixed = TRUE)) {
    figure +
        geom_errorbar(data = data_top, aes(x = Scenario, y = !! sym(column), ymin = lower95, ymax = upper95), width = 0.02, color = "black", size = 0.05) +
        geom_point(data = data_top, aes(x  = Scenario, y = !! sym(column)), stat = "identity", color = "black", size = 1) #+
        #geom_line(data = data_top, aes(x = Scenario, y = !! sym(column), color = "#41AB5D", size = 0.5))
      }
    
    return(figure)
    
  }
    

#############################################################################################################################################################################

                                                                  # 1) EU INTERNAL - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplots (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: March 2021
# Author: Francesca Rosa
  
  EUinternal.barplot.EPnoex <- function(csv_path, file_label, plots_path, year, width_height) {
  
    data <- read.csv(paste0(csv_path, "EUForest_", year, file_label, "_EPnoex.csv"), header = TRUE)
    data_top <- read.csv(paste0(csv_path, "EUForest_", year, file_label,"_top_EPnoex.csv"), header = TRUE)

    pal = c("#C3D6E8", "#8C96C6", "#8C6BB1", "#530A50")
    palette_name = "BuPu"

    
     if(grepl("timber", file_label, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Selection", "Retention", "Clear_cut", "EP_EU"), labels = c("Timber plantations", "Selection", "Retention", "Clear cut", "Lignocellulosic energy crops"))
        data <- data %>% filter(Category != "Other_management")
        pal = append("#FDAE6B", pal)
        labs = c("Timber plantations", "Selection", "Retention",  "Clear cut", "Lignocel. energy crops")
        } else { 
        data <- data %>% filter(Category != "Timber" & Category != "Other_management")
        data$Category <- factor(data$Category, levels = c("Selection", "Retention", "Clear_cut",  "EP_EU"), labels = c("Selection", "Retention", "Clear cut","Lignocel. energy crops"))
        labs = c("Selection", "Retention",  "Clear cut", "Lignocel. energy crops")

      }
    
    data <- data %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_")
    data_top <- data_top %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_")
  
    figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDF", axis_name = "Extinction risk [PDF]", file_label = file_label)
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, file_label, "_EPnoex.png"), width = width_height[1], height = width_height[2], units = "cm")

  }



#############################################################################################################################################################################

                               # 2) EU FOOTPRINT and EU FOREST - BARPLOT all forest categories disaggregated #

#############################################################################################################################################################################

# General task: create a barplot of footprints of the various climate mitigation scenarios and forest use scenarios with the disaggregation also at EU level
# Date: June 2022
# Author: Francesca Rosa

  
  EU.barplot.EP.all.dis <- function(csv_path, file_label, plots_path, year, width_height) {
      # csv_path = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)
    
      # Choose the palette
      
      palette_name = "BuPu"
      pal_EU = c("#C3D6E8", "#8C96C6", "#8C6BB1", "#530A50")
      pal_im = c("#F7FCB9", "#D9F0A3", "#ADDD8E", "#41AB5D", "#238443")
      
      # Load the data
      # Load the disaggregated data
      data <- read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_EP_disaggr.csv"), header = TRUE)
      # Load the cumulative data 
      data_top <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_top_EP.csv"), header = TRUE)
        
      data <- data %>% filter(Category != "EU28 - Timber plantations")
      
      data$Category <- factor(data$Category, levels = c(rev(c("Import - Energy plantations", "Import - Timber and pulp plantations", "Import - Clear cut",
                                                      "Import - Selection", "Import - Selective logging", "Import - Reduced Impact Logging")), rev(c("EU28 - Lignocel. energy crops", 
                                                      "EU28 - Clear cut", "EU28 - Retention", "EU28 - Selection"))))
      
      if(grepl("SharedEffort", file_label)) {data <- data %>% filter(Category != "Import - Selective logging")}
      if(grepl("Baseline", file_label)) {
          data <- data %>% filter(Category != "Import - Selective logging" & Category != "Import - Reduced Impact Logging" & Category != "Import - Selection")
          pal_im = c("#ADDD8E", "#41AB5D", "#238443")
          }
      if(grepl("LowerIntensity", file_label)) {data <- data %>% filter(Category != "Import - Reduced Impact Logging")}

      data_top <- data_top %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_")
      
      # save the data in a new .csv file

      pal = c(pal_im, pal_EU) 

  
      # Plot
      figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDF", axis_name = "Extinction risk [PDF]", file_label = file_label)
      figure
      
      # Save as png
      ggsave(paste0(plots_path, "EUFootprint&Forest_", year, file_label, "_EP_im-for-all-disaggr.pdf"), width = width_height[1], height = width_height[2], units = "cm")

  }
  


#############################################################################################################################################################################

                                                                  # 3) EU FOREST AND DEMAND AREAS - BARPLOT #

#############################################################################################################################################################################

# General task: create a barplot of areas to meet EU demand in the various climate mitigation scenarios and forest use scenarios with the disaggregation also at EU level
# Date: June 2022
# Author: Francesca Rosa

EU.areas.barplot.EP.dis <- function(aggr_plot_path_areas, label_timber, file_label, plots_path, year, width_height) {
      # aggr_plot_path_areas = path of the .csv files where the data to plot are stored (character vector)
      # label_timber and file_label = identification words which selects the file (character vectors)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)

      # Choose the palette
      palette_name = "BuPu"
      
      pal_EU = c("#C3D6E8", "#8C96C6", "#8C6BB1", "#530A50")
      pal_im = c("#F7FCB9", "#D9F0A3", "#ADDD8E", "#41AB5D", "#238443")

      # Load and prepare the data 
      data <-read.csv(paste0(aggr_plot_path_areas, "areas_EUFootprint_", year,"_", label_timber, "_disaggr.csv"), header = TRUE)

      data <- data %>% pivot_wider(names_from = Category, values_from = Values) %>%
              mutate(EU28_Energy_crops = EP_EU + EP_conv_EU, Energy_plantations_im = EP_conv_im + For_PlantationFuel_im) %>% 
                  select(-EP_conv_EU, -EP_conv_im, -EP_EU, -ForOther_Extensive_EU, -ForOther_Intensive_EU, -For_PlantationFuel_im) %>%
                    pivot_longer(cols = (contains("For") | contains("_im") | contains("EU")), names_to = "Category", values_to = "Values") %>%
                      data.frame()
      
      data <- data %>% mutate(Category = str_replace(Category, "EU28_Energy_crops", "EU28 Lignocel. energy crops"), 
                              Category = str_replace(Category, "For_ClearCut_EU", "EU28 Clear cut"),
                              Category = str_replace(Category, "For_Retention_EU", "EU28 Retention"),
                              Category = str_replace(Category, "For_SelectionSystem_EU", "EU28 Selection"),
                              Category = str_replace(Category, "For_TimberPlant_EU", "EU28 Timber"),
                              Category = str_replace(Category, "Energy_plantations_im", "Import - Energy plantations"), 
                              Category = str_replace(Category, "For_TimberPlant_im", "Import - Timber and pulp plantations"),
                              Category = str_replace(Category, "For_ClearCut_im", "Import - Clear cut"),
                              Category = str_replace(Category, "For_SelectionSystem_im", "Import - Selection"),
                              Category = str_replace(Category, "For_Selective_im", "Import - Selective logging"),
                              Category = str_replace(Category, "For_ReducedImpactLogging_im", "Import - Reduced Impact Logging"))

        data <- data %>% filter(Category != "EU28 Timber")

        data$Category <- factor(data$Category, levels = c(rev(c("Import - Energy plantations", "Import - Timber and pulp plantations", "Import - Clear cut",
                                                        "Import - Selection", "Import - Selective logging", "Import - Reduced Impact Logging")), rev(c("EU28 Lignocel. energy crops", 
                                                        "EU28 Clear cut", "EU28 Retention", "EU28 Selection"))))
        
      if(grepl("SharedEffort", file_label)) {data <- data %>% filter(Category != "Import - Selective logging")}
      if(grepl("Baseline", file_label)) {
          data <- data %>% filter(Category != "Import - Selective logging" & Category != "Import - Reduced Impact Logging" & Category != "Import - Selection")
          pal_im = c("#ADDD8E", "#41AB5D", "#238443")
        }
      if(grepl("LowerIntensity", file_label)) {data <- data %>% filter(Category != "Import - Reduced Impact Logging")}

      pal = c(pal_im, pal_EU) 
  
      data <- data %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_")

      # Plot
      figure <- plot.EU.barplot(data = data, pal = pal, column = "Values", axis_name = "Mha", file_label = file_label)
        figure
        
      ggsave(paste0(plots_path, "EUFootprint&Forest_areas_", year, file_label, "_EPnoex_all-dis.png"), width = width_height[1], height = width_height[2], units = "cm")

  }
      


#############################################################################################################################################################################

                                                                  # 3) HARVEST TO MEET EU FOREST BIOMASS DEMAND - BARPLOT #

#############################################################################################################################################################################

# General task: create the barplot of the biomass volume (Mm3) needed to meet EU demand in the various climate mitigation scenarios and forest use scenarios with the disaggregation also at EU level
# Date: June 2022
# Author: Francesca Rosa

EU.volume.barplot.EP.dis <- function(csv_path, file_label, plots_path, year, width_height) {
      # csv_path = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)
  
      palette_name = "BuPu"

      pal_EU = c("#C3D6E8", "#8C96C6", "#8C6BB1", "#530A50")
      pal_im = c("#F7FCB9", "#D9F0A3", "#ADDD8E", "#41AB5D", "#238443")
      
  data <- read.csv(paste0(csv_path, "EUdemand_volumes_", year, file_label, "_disaggr.csv"), header = TRUE) 
  
  data$Category <- factor(data$Category, levels = c(rev(c("Import - Energy plantations", "Import - Timber and pulp plantations", "Import - Clear cut",
                                                        "Import - Selection", "Import - Selective logging", "Import - Reduced Impact Logging")), rev(c("EU28 - Lignocel. energy crops", 
                                                        "EU28 - Clear cut", "EU28 - Retention", "EU28 - Selection")))) 
  
  if(grepl("SharedEffort", file_label)) {data <- data %>% filter(Category != "Import - Selective logging")}
  if(grepl("Baseline", file_label)) {
          data <- data %>% filter(Category != "Import - Selective logging" & Category != "Import - Reduced Impact Logging" & Category != "Import - Selection")
          pal_im = c("#ADDD8E", "#41AB5D", "#238443")
      }
  if(grepl("LowerIntensity", file_label)) {data <- data %>% filter(Category != "Import - Reduced Impact Logging")}
  
      pal = c(pal_im, pal_EU) 
      
    # Plot
        figure <- plot.EU.barplot(data = data, pal = pal, column = "Values", axis_name = "Mm3", file_label = file_label)
        figure
    
    # Save as a png
    
    ggsave(paste0(plots_path, "EUFootprint&Forest_volume_Mm3_", year, file_label, "_EPnoex.png"), width = width_height[1], height = width_height[2], units = "cm")

    }