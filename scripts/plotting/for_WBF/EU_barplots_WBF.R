# Functions:

## plotting function used in all the other functions - plot.EU.barplot

### 1) IMPACTS OF EU INTERNAL FORESTS - BARPLOT (energy crops included, exports excluded) - EUinternal.barplot.EPnoex

### 2) EU FOOTPRINT - BARPLOT (energy plantations included, imported forest disaggregated) - EUfootprint.barplot.EP.fordis

### 3) EU AREAS - BARPLOT (energy plantations included, imported forest disaggregated) - EUareas.barplot.EP.fordis


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
library(scales)
library(stringr)
library(rcartocolor)
library(ggpattern)
select <- dplyr::select
  

################################################### PLOTTING FUNCTION #####################################################################
                    
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
  
    # data_bu <- data
    # data_top_bu <- data_top
    # data <- data_bu
    # data_top <- data_top_bu

    # Filter the scenarios which will be plotted
    data <- data %>% filter(Scenario == "noAFM" | Scenario == "AFM50") %>%
      unite("Scenario", Group:Scenario, sep = "_") %>%
        filter(Scenario != "RCP2.6_SFM_noAFM" & Scenario != "REF_SFM_noAFM")
      
    # Rename the scenarios
    label_scenario <- rev(c("RCP6.5 - Baseline", "RCP6.5 - Close-to-nature", "RCP6.5 - Set-aside", "RCP2.6 - Baseline", "RCP2.6 - Close-to-nature", "RCP2.6 - Set-aside"))

    data <- data %>% mutate(Scenario = str_replace(Scenario, "REF_MFM_noAFM", "RCP6.5 Baseline"), Scenario = str_replace(Scenario, "REF_MFM_AFM50", "RCP6.5 Close-to-nature"),
                            Scenario = str_replace(Scenario, "REF_SFM_AFM50", "RCP6.5 Set-aside"), Scenario = str_replace(Scenario, "RCP2.6_MFM_noAFM", "RCP2.6 Baseline"), Scenario = str_replace(Scenario, "RCP2.6_MFM_AFM50", "RCP2.6 Close-to-nature"), 
                            Scenario = str_replace(Scenario, "RCP2.6_SFM_AFM50", "RCP2.6 Set-aside")) 
    
    # Set the order of the scenarios
    data$Scenario <- factor(data$Scenario, levels = rev(c("RCP6.5 Baseline", "RCP6.5 Close-to-nature", "RCP6.5 Set-aside", "RCP2.6 Baseline", "RCP2.6 Close-to-nature", "RCP2.6 Set-aside")))
  

    # Add the points and the CI if the data loaded are labeled accordingly
    if(grepl("bs", file_label, fixed = TRUE)) { # bs means bootstrapping

      # Clean and prepare the data with the cumulative results and the ci, as done with the disaggregated values
      
          data_top <- data_top %>% filter(Scenario == "noAFM" | Scenario == "AFM50") %>%
              unite("Scenario", Group:Scenario, sep = "_") %>%
                filter(Scenario != "RCP2.6_SFM_noAFM" & Scenario != "REF_SFM_noAFM")
          
          data_top <- data_top %>% mutate(Scenario = str_replace(Scenario, "REF_MFM_noAFM", "RCP6.5 Baseline"), Scenario = str_replace(Scenario, "REF_MFM_AFM50", "RCP6.5 Close-to-nature"),
                            Scenario = str_replace(Scenario, "REF_SFM_AFM50", "RCP6.5 Set-aside"), Scenario = str_replace(Scenario, "RCP2.6_MFM_noAFM", "RCP2.6 Baseline"), Scenario = str_replace(Scenario, "RCP2.6_MFM_AFM50", "RCP2.6 Close-to-nature"), 
                            Scenario = str_replace(Scenario, "RCP2.6_SFM_AFM50", "RCP2.6 Set-aside")) 

          # Set the order of the scenarios
          data_top$Scenario <- factor(data_top$Scenario, levels = rev(c("RCP6.5 Baseline", "RCP6.5 Close-to-nature", "RCP6.5 Set-aside", "RCP2.6 Baseline", "RCP2.6 Close-to-nature", "RCP2.6 Set-aside")))

          # Define the y limits 
          max_per_scenario <- data %>% filter(!! sym(column) >= 0) %>% group_by(Scenario) %>% summarise(max = sum(!! sym(column)))
          max_per_scenario <- max(max_per_scenario$max, max(data_top$upper95))

          if((round(max_per_scenario, digits = 3) - max_per_scenario) < 0.005)
            { ymax_value <- (round(max_per_scenario, digits = 2) + 0.01)
                } else { ymax_value <- round(max_per_scenario, digits = 2) }

          min_per_scenario <- data %>% filter(!! sym(column) < 0) %>% group_by(Scenario) %>% summarise(min = sum(!! sym(column)))
            min_per_scenario <- min(min_per_scenario$min, min(data_top$lower95))

          if (min_per_scenario >= 0) {ymin_value = 0} else {
            ymin_value <- min_per_scenario }

    }
    
        # Set the y limits
    
    if(grepl("PDF", axis_name, fixed = TRUE)) {
  
      ymax_value = 0.3
      ymin_value = 0
      
    }

    if(grepl("Mha", axis_name, fixed = TRUE)) {
  
      ymax_value = 350
      ymin_value = 0
      
    }
    
    if(grepl("Mm3", axis_name, fixed = TRUE)) {
  
      ymax_value = 1700
      ymin_value = 0
      
    }


    # Plot
      
    figure <-
      ggplot(data) +
      geom_bar(aes(x = Scenario, y = !! sym(column), fill = Category), size = 0.1, width = 0.4, stat = "identity", position = position_stack(reverse = FALSE)) +
      theme_classic() + # select the theme of the plot
      theme(legend.position = "right", 
           legend.text = element_text(size = 12),
           axis.text = element_text(size = 13),
           axis.text.x = element_text(angle = 45, hjust = 1),
           axis.title = element_text(size = 14),
           legend.title = element_text(size = 14),
           legend.key.size = unit(0.6, "cm")) +
      coord_flip() +
      guides(fill = guide_legend(title = "Category", title.position = "top")) +
      scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      xlab("Scenarios") + ylab(axis_name) +
      ylim(ymin_value, ymax_value) + 
      scale_fill_manual(values = pal)

  #   if(length(data) > 3) {
  #   figure +
  #     geom_col_pattern(data = data, aes(x = Scenario, y = !! sym(column), pattern = Intensity),
  #                  pattern_fill = "white", pattern_color = "white", pattern_density = 0.1, pattern_spacing = .01) +
  #         scale_pattern_angle_manual(values = c("none", "stripe"),)
  # }

    # Include the CI if needed
    if(grepl("bs", file_label, fixed = TRUE)) {
    figure +
      ylim(ymin_value, ymax_value) +
      geom_point(data = data_top, aes(x  = Scenario, y = !! sym(column)), color = "black", size = 1) +
      geom_errorbar(data = data_top, aes(x = Scenario, y = !! sym(column), ymin = lower95, ymax = upper95), width = 0.02, color = "black", size = 0.05)
  }

    return(figure)

  }
    

#############################################################################################################################################################################
                                                                                # 1 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 1) EU INTERNAL - BARPLOT #

#############################################################################################################################################################################
# General task: create a barplot of internal impacts of the various climate mitigation scenarios and forest use scenarios
# Date: March 2021
# Author: Francesca Rosa
  
  EUinternal.barplot.EPnoex <- function(csv_path, file_label, plots_path, year, width_height) {
      # csv_path = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)
    
    # Load the data
    
      data <- read.csv(paste0(csv_path, "EUForest_", year, file_label, "_EPnoex.csv"), header = TRUE)
      data_top <- read.csv(paste0(csv_path, "EUForest_", year, file_label,"_top_EPnoex.csv"), header = TRUE)
      
    # Remove "Other_management", since it will not be plotted
      
      data <- data %>% filter(Category != "Other_management")  
      
    # Choose the palette
      
      pal = viridis_pal(option = "magma")(6)
        palette_name = "Viridis-magma"
        
    # Convert the names of the forest management categories to factors and distinguish when timber plantations are considered and when they are not
              
     if(grepl("timber", file_label, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Retention", "Selection", "EP_EU"), labels = c("Timber", "Clear cut", "Retention", "Selection", "Lignocellulosic energy crops"))
        } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Retention", "Selection", "EP_EU"), labels = c("Clear cut", "Retention", "Selection", "Lignocel. energy crops"))
        pal = pal[-1]  
        }
    
    # Plot
    figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name ="global PDF (%)", file_label = file_label)
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, file_label, "_EPnoex_WBF.png"), width = width_height[1], height = width_height[2], units = "cm")

  }



#############################################################################################################################################################################
                                                                                # 2 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 2) EU FOOTPRINT - BARPLOT #

#############################################################################################################################################################################

# General task: create a barplot of footprints of the various climate mitigation scenarios and forest use scenarios
# Date: June 2022
# Author: Francesca Rosa

  
  EUFootprint.barplot.EP <- function(csv_path, file_label, plots_path, year, width_height) {
      # csv_path = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)
    
      # Choose the palette

        pal_im = c("#A6DBA0", "#008837")
        pal_EU = c("#C2A5CF", "#7B3294")
        palette_name = "Brewer_VG"
        pal = c(pal_im, pal_EU) 

      # Load the data
        
          # Load the data
        # Load the disaggregated data
        data <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_EP_im-for-disaggr.csv"), header = TRUE) %>%
              mutate(Category = str_replace(Category, "Import_Energy_plantations", "Energy_plantations_im"))  # Rename one of the category to contain "_im"
        data_EU <- read.csv(paste0(csv_path, "EUForest_", year, file_label, "_EPnoex.csv"), header = TRUE) %>%
                    filter(Category != "Other_management" & Category != "EP_EU")
        # Rename the categories to make them handier
        data_EU <- data_EU %>% mutate(Category = str_replace(Category, "Clear_cut", "Clear_cut_EU"),
                                Category = str_replace(Category, "Retention", "Retention_EU"),
                                Category = str_replace(Category, "Selection", "Selection_EU"),
                                Category = str_replace(Category, "Timber", "Timber_EU"))
        
        # Remove the grouped Category for EU internal impacts
        data <- data %>% filter(Category != "EU28_Forest_net") %>% full_join(data_EU) %>%
          full_join(data_EU)
                      
        # Group the categories according to their intensity-level
        data <- data %>% pivot_wider(names_from = Category, values_from = PDFx100) %>%
          transmute(Group = Group, Scenario = Scenario, High_intensity_EU = EU28_Energy_crops + Clear_cut_EU + Timber_EU + Retention_EU, Low_intensity_EU = Selection_EU,
                    High_intensity_im = Energy_plantations_im + Clear_cut_im + Pulp_Timber_Plantation_im, Low_intensity_im = Selection_im + Selective_im) %>%
              pivot_longer(cols = contains("_EU") | contains("_im"), names_to = "Category", values_to = "PDFx100") %>% 
                mutate(Intensity = 0) %>% mutate(Intensity = case_when(str_detect(Category, "High") ~ 1, str_detect(Category, "Low") ~ 0)) %>% data.frame()
        
          
        # Load the cumulative data 
          data_top <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_top_EP.csv"), header = TRUE)
          # Since "Other_management" should not be plotted, we need to subtract it from the grouped category "EU28_Forest_net"
          # To do it, we first need to load the EU data
          data_other_manag <- read.csv(paste0(csv_path, "EUForest_", year, file_label, "_EPnoex.csv"), header = TRUE)
          # and create a df with only the values for "Other_management"
          data_other_manag <- data_other_manag %>% filter(Category == "Other_management")
          # and rename the column with the values to prevent joining df with columns with the same name
          data_other_manag <- data_other_manag %>% rename(Values = PDFx100)
          # then we join the two df and subtract "Other_management" from data_top
          data_top <- data_top %>% full_join(data_other_manag) %>% 
            mutate(ratio = Values/PDFx100, 
                PDFx100 = PDFx100 - Values)
          data_top <- data_top %>% mutate(lower95 = lower95 - lower95*ratio, upper95 = upper95 - upper95*ratio) %>%
            select(-ratio, -Values, -Category)

        rm(data_other_manag)
      
      # Clean and arrange the data 
      data <- data %>% mutate(Category = str_replace(Category, "High_intensity_EU", "EU28 - High-intensity management"), 
                              Category = str_replace(Category, "Low_intensity_EU", "EU28 - Low-intensity management"),
                              Category = str_replace(Category, "High_intensity_im", "Import - High-intensity management"), 
                              Category = str_replace(Category, "Low_intensity_im", "Import - Low-intensity management"))
      
      # Set the order of the categories
      data$Category <- factor(data$Category, levels = c("Import - Low-intensity management",  "Import - High-intensity management", "EU28 - Low-intensity management", "EU28 - High-intensity management"))
      
      pal = c(pal_im, pal_EU) 

      # Plot
      figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name = "global PDF (%)", file_label = file_label)
      figure
      # Save as png
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP_im-for_WBF_25.png"), width = width_height[1], height = width_height[2], units = "cm")

  }
  



#############################################################################################################################################################################
                                                                                # 3 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 3) EU FOOTPRINT and EU FOREST- BARPLOT #

#############################################################################################################################################################################

# General task: create a barplot of footprints of the various climate mitigation scenarios and forest use scenarios with the disaggregation also at EU level
# Date: June 2022
# Author: Francesca Rosa

  
  EU.barplot.EP.dis <- function(csv_path, file_label, plots_path, year, width_height) {
      # csv_path = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)
    
      # Choose the palette
      
      palette_name = "Brewer_VG"
      pal_EU = c("#40004B", "#9970AB", "#C2A5CF", "#E7D4E8")
      pal_im = rev(c("#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B"))
            
      
      # Load the data
        # Load the disaggregated data
        data <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_EP_im-for-disaggr.csv"), header = TRUE) %>%
              mutate(Category = str_replace(Category, "Import_Energy_plantations", "Energy_plantations_im"))  # Rename one of the category to contain "_im"
        data_EU <- read.csv(paste0(csv_path, "EUForest_", year, file_label, "_EPnoex.csv"), header = TRUE) %>%
                    filter(Category != "Other_management" & Category != "EP_EU")
        # Rename the categories to make them handier
        data_EU <- data_EU %>% mutate(Category = str_replace(Category, "Clear_cut", "Clear_cut_EU"),
                                Category = str_replace(Category, "Retention", "Retention_EU"),
                                Category = str_replace(Category, "Selection", "Selection_EU"),
                                Category = str_replace(Category, "Timber", "Timber_EU"))
        
        # Remove the grouped Category for EU internal impacts
        data <- data %>% filter(Category != "EU28_Forest_net") %>% full_join(data_EU) %>%
          full_join(data_EU)
                      
        # Load the cumulative data and subtract the share of "Other_management" (see previous function)
          data_top <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_top_EP.csv"), header = TRUE)
          data_other_manag <- read.csv(paste0(csv_path, "EUForest_", year, file_label, "_EPnoex.csv"), header = TRUE)
          data_other_manag <- data_other_manag %>% filter(Category == "Other_management")
          data_other_manag <- data_other_manag %>% rename(Values = PDFx100)
          data_top <- data_top %>% full_join(data_other_manag) %>% 
            mutate(ratio = Values/PDFx100, 
                PDFx100 = PDFx100 - Values)
          data_top <- data_top %>% mutate(lower95 = lower95 - lower95*ratio, upper95 = upper95 - upper95*ratio) %>%
            select(-ratio, -Values, -Category)

      rm(data_other_manag)
      
      # Clean and arrange the data 
        data <- data %>% mutate(Category = str_replace(Category, "EU28_Energy_crops", "EU28 Lignocel. energy crops"), 
                                Category = str_replace(Category, "Clear_cut_EU", "EU28 Clear cut"),
                                Category = str_replace(Category, "Retention_EU", "EU28 Retention"),
                                Category = str_replace(Category, "Selection_EU", "EU28 Selection"),
                                Category = str_replace(Category, "Timber_EU", "EU28 Timber"),
                                Category = str_replace(Category, "Energy_plantations_im", "Import - Energy plantations"), 
                                Category = str_replace(Category, "Pulp_Timber_Plantation_im", "Import - Timber and pulp plantations"),
                                Category = str_replace(Category, "Clear_cut_im", "Import - Clear cut"),
                                Category = str_replace(Category, "Selection_im", "Import - Selection"),
                                Category = str_replace(Category, "Selective_im", "Import - Selective logging"))
        
        data <- data %>% filter(Category != "EU28 Timber")
      
        data$Category <- factor(data$Category, levels = c(rev(c("Import - Energy plantations", "Import - Timber and pulp plantations", "Import - Clear cut",
                                                        "Import - Selection", "Import - Selective logging")), rev(c("EU28 Lignocel. energy crops", 
                                                        "EU28 Clear cut", "EU28 Retention", "EU28 Selection"))))
      
      pal = c(rev(pal_im), rev(pal_EU)) 
        
      # Plot
      figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name ="global PDF (%)", file_label = file_label)
      figure
      
      # Save as png
      ggsave(paste0(plots_path, "EUFootprint&Forest_", year, "_", palette_name, file_label, "_EP_im-for-disaggr_WBF_25.png"), width = width_height[1], height = width_height[2], units = "cm")

  }
  

#############################################################################################################################################################################
                                                                                # 5 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 5) EU FOREST AREAS - BARPLOT #

#############################################################################################################################################################################

# General task: create a barplot of EU areas in the various climate mitigation scenarios and forest use scenarios
# Date: June 2022
# Author: Francesca Rosa

  EUForest.areas.barplot.EP.dis <- function(aggr_plot_path_areas, label_timber, file_label, plots_path, year, width_height) {
      # aggr_plot_path_areas = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)

      # Choose the palette
      pal = viridis_pal(option = "magma")(6)
            palette_name = "Viridis-magma"
            
      # Load the data 
      data <-read.csv(paste0(aggr_plot_path_areas, "areas_EUForest_", year,"_", label_timber, "_disaggr.csv"), header = TRUE)

      # Prepare and arrange the data
      data <- data %>% pivot_wider(names_from = Category, values_from = Values) %>%
              mutate(EP_EU = EP_EU + EP_conv_EU, Other_management = ForOther_Extensive_EU + ForOther_Intensive_EU) %>% 
                select(- EP_conv_EU, -ForOther_Extensive_EU, -ForOther_Intensive_EU) %>%
                   pivot_longer(cols = For_ClearCut_EU:Other_management, names_to = "Category", values_to = "Values") %>%
                      data.frame()

      data <- data %>% mutate(Category = str_replace(Category, "For_ClearCut_EU", "Clear_cut"), Category = str_replace(Category, "For_Retention_EU", "Retention"), 
                          Category = str_replace(Category, "For_TimberPlant_EU", "Timber"), Category = str_replace(Category, "For_SelectionSystem_EU", "Selection")) 
      
      # Delete the rows with timber if the timber feature is turned off
      if(grepl("notimber", label_timber, fixed = TRUE)) {
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Clear cut", "Retention", "Selection", "Other management", "Lignocel. energy crops"))
        pal = pal[-1]  
          } else { 
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Timber", "Clear cut", "Retention", "Selection", "Other management", "Lignocellulosic energy crops"))
          }
      
    # Filter out "Other_management" since it will not be plotted
    data <- data %>% filter(Category != "Other management")  
    
    # Plot
    figure <- plot.EU.barplot(data = data, pal = pal, column = "Values", axis_name = "Mha", file_label = file_label)
      figure
      
    ggsave(paste0(plots_path, "EUForest_areas_", year, "_", palette_name, file_label, "_EPnoex_WBF.png"), width = width_height[1], height = width_height[2], units = "cm")

  }
  


#############################################################################################################################################################################
                                                                                # 6 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 6) EU DEMAND AREAS - BARPLOT #

#############################################################################################################################################################################

# General task: create a barplot of areas to meet EU demand in the various climate mitigation scenarios and forest use scenarios with the disaggregation also at EU level
# Date: June 2022
# Author: Francesca Rosa
  
  EUFootprint.areas.barplot.EP <- function(aggr_plot_path_areas, label_timber, file_label, plots_path, year, width_height) {
      # aggr_plot_path_areas = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)

        pal_im = c("#A6DBA0", "#008837")
        pal_EU = c("#C2A5CF", "#7B3294")
        palette_name = "Brewer_VG"
        pal = c(pal_im, pal_EU) 

      # Load the data
      data <-read.csv(paste0(aggr_plot_path_areas, "areas_EUFootprint_", year,"_", label_timber, "_disaggr.csv"), header = TRUE)
      
      # Clean and arrange the data
      
      data <- data %>% pivot_wider(names_from = Category, values_from = Values) %>% data.frame() %>%
        # Group the data to be grouped
        transmute(Group = Group, Scenario = Scenario, High_intensity_EU = EP_EU + EP_conv_EU + For_ClearCut_EU + For_TimberPlant_EU + For_Retention_EU, 
                  Low_intensity_EU = For_SelectionSystem_EU, # + ForOther_Extensive_EU + ForOther_Intensive_EU,
               High_intensity_im = EP_conv_im + For_PlantationFuel_im + For_ClearCut_im, Low_intensity_im = For_SelectionSystem_im + For_Selective_im) %>%
                pivot_longer(cols = contains("_EU") | contains("_im"), names_to = "Category", values_to = "Values") %>% data.frame()
      
      # Clean and arrange the data 
      data <- data %>% mutate(Category = str_replace(Category, "High_intensity_EU", "EU28 - High-intensity management"), 
                              Category = str_replace(Category, "Low_intensity_EU", "EU28 - Low-intensity management"),
                              Category = str_replace(Category, "High_intensity_im", "Import - High-intensity management"), 
                              Category = str_replace(Category, "Low_intensity_im", "Import - Low-intensity management"))
      
     # Set the order of the categories
      data$Category <- factor(data$Category, levels =c("Import - Low-intensity management",  "Import - High-intensity management", 
                                                                       "EU28 - Low-intensity management", "EU28 - High-intensity management"))
    
      # Plot
      figure <- plot.EU.barplot(data = data, pal = pal, column = "Values", axis_name = "Mha", file_label = file_label)
      figure
      
      # Save as png
      ggsave(paste0(plots_path, "EUFootprint_areas_", year, "_", palette_name, "_", label_timber, file_label, "_EP_im-for-disaggr_WBF_25.png"), width = width_height[1], height = width_height[2], units = "cm")

  }
  

#############################################################################################################################################################################
                                                                                # 7 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 7) EU FOREST AND DEMAND AREAS - BARPLOT #

#############################################################################################################################################################################

# General task: create a barplot of areas to meet EU demand in the various climate mitigation scenarios and forest use scenarios with the disaggregation also at EU level
# Date: June 2022
# Author: Francesca Rosa

EU.areas.barplot.EP.dis <- function(aggr_plot_path_areas, label_timber, file_label, plots_path, year, width_height) {
      # aggr_plot_path_areas = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)

      # Choose the palette

      palette_name = "Brewer_VG"
      pal_EU = c("#40004B", "#9970AB", "#C2A5CF", "#E7D4E8")
      pal_im = rev(c("#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B"))
            
      # Load and prepare the data 
      data <-read.csv(paste0(aggr_plot_path_areas, "areas_EUFootprint_", year,"_", label_timber, "_disaggr.csv"), header = TRUE)
      data_top <-read.csv(paste0(aggr_plot_path_areas, "areas_EUFootprint_", year, "_", label_timber, "_top_EP.csv"), header = TRUE)
      
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
                              Category = str_replace(Category, "For_Selective_im", "Import - Selective logging"))

        data <- data %>% filter(Category != "EU28 Timber")

        data$Category <- factor(data$Category, levels = c(rev(c("Import - Energy plantations", "Import - Timber and pulp plantations", "Import - Clear cut",
                                                        "Import - Selection", "Import - Selective logging")), rev(c("EU28 Lignocel. energy crops", 
                                                        "EU28 Clear cut", "EU28 Retention", "EU28 Selection"))))

      pal = c(rev(pal_im), rev(pal_EU)) 
  
      # Plot
      figure <- plot.EU.barplot(data = data, pal = pal, column = "Values", axis_name = "Mha", file_label = file_label)
        figure
        
      ggsave(paste0(plots_path, "EUFootprint&Forest_areas_", year, "_", palette_name, file_label, "_EPnoex_WBF_25.png"), width = width_height[1], height = width_height[2], units = "cm")

  }
      


