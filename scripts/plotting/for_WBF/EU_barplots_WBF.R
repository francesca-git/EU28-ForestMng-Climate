# Functions:

## plotting function used in all the other functions - plot.EU.barplot

### 1) EU FOOTPRINT - BARPLOT (energy plantations included) - EUfootprint.barplot.EP

### 2) IMPACTS OF EU INTERNAL FORESTS - BARPLOT (energy crops included, exports excluded) - EUinternal.barplot.EPnoex

### 3) EU FOOTPRINT - BARPLOT (energy plantations included, imported forest disaggregated) - EUfootprint.barplot.EP.fordis

### 4) EU AREAS - BARPLOT (energy plantations included, imported forest disaggregated) - EUareas.barplot.EP.fordis


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
  
    data_bu <- data
    data <- data_bu

    data <- data %>% filter(Scenario == "noAFM" | Scenario == "AFM75") %>%
      unite("Scenario", Group:Scenario, sep = "_") %>%
        filter(Scenario != "RCP2.6_SFM_noAFM" & Scenario != "RCP2.6_MFM_noAFM" & Scenario != "REF_SFM_noAFM")
      
    # rename the scenarios
    #label_scenario <- c("no AFM (baseline)", "Laissez-faire", "AFM 12.5%", "AFM 25%", "AFM 37.5%", "AFM 50%")  
    label_scenario <- c("Baseline", "RCP6.5 - Close-to-nature", "RCP6.5 - Set-aside", "RCP2.6 - Close-to-nature", "RCP2.6 - Set-aside")

    data <- data %>% mutate(Scenario = str_replace(Scenario, "REF_MFM_noAFM", "Baseline"), Scenario = str_replace(Scenario, "REF_MFM_AFM75", "RCP6.5 Close-to-nature"),
                            Scenario = str_replace(Scenario, "REF_SFM_AFM75", "RCP6.5 Set-aside"), Scenario = str_replace(Scenario, "RCP2.6_MFM_AFM75", "RCP2.6 Close-to-nature"), 
                            Scenario = str_replace(Scenario, "RCP2.6_SFM_AFM75", "RCP2.6 Set-aside")) 
    
    # set the order of the scenarios
    data$Scenario <- factor(data$Scenario, levels = c("Baseline", "RCP6.5 Close-to-nature", "RCP6.5 Set-aside", "RCP2.6 Close-to-nature", "RCP2.6 Set-aside"))

    # set the maximum value of the y axis according to the max value of the PDF 

      #max_data_top <- max(max(data_top$PDFx100), max(data_top$upper95))
    
    #ymax_value = 0.3
    # for EU footprint with fixed legend  
    # ymin_value = -0.02
    # ymax_value = 0.3
    # for EU footprint with fixed legend
    # ymin_value = 0
    # ymax_value = 0.5
    
    ymax_value = 0.35
    ymin_value = 0
      
    # for EU internal forest with fixed legend
    # ymin_value = -0.05
    # ymin_value = 0
    # ymax_value = 0.1

    if(grepl("bs", file_label, fixed = TRUE)) {

          data_top <- data_top %>% filter(Scenario == "noAFM" | Scenario == "AFM100") %>%
              unite("Scenario", Group:Scenario, sep = "_") %>%
                filter(Scenario != "RCP2.6_SFM_noAFM" & Scenario != "RCP2.6_MFM_noAFM" & Scenario != "REF_SFM_noAFM")

          data_top <- data_top %>% mutate(Scenario = str_replace(Scenario, "REF_MFM_noAFM", "Baseline"), Scenario = str_replace(Scenario, "REF_MFM_AFM100", "RCP6.5 Close-to-nature"),
                            Scenario = str_replace(Scenario, "REF_SFM_AFM100", "RCP6.5 Set-aside"), Scenario = str_replace(Scenario, "RCP2.6_MFM_AFM100", "RCP2.6 Close-to-nature"),
                            Scenario = str_replace(Scenario, "RCP2.6_SFM_AFM100", "RCP2.6 Set-aside"))

          # set the order of the scenarios
          data_top$Scenario <- factor(data_top$Scenario, levels = c("Baseline", "RCP6.5 Close-to-nature", "RCP6.5 Set-aside", "RCP2.6 Close-to-nature", "RCP2.6 Set-aside"))

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


    # plot
      
    figure <-
      ggplot(data)+
      geom_bar(aes(x = Scenario, y = !! sym(column), fill = Category), size = 0.1, width = 0.4, stat = "identity", position = position_stack(reverse = FALSE)) +
      theme_classic() + # select the theme of the plot
      #theme_minimal(base_size = 15) + # select the theme of the plot
      theme(legend.position = "right", 
           legend.text = element_text(size = 12),
           axis.text = element_text(size = 12),
           axis.text.x = element_text(angle = 90),
           axis.title = element_text(size = 12),
           legend.title = element_text(size = 13),
           legend.key.size = unit(0.6, "cm")) +
      guides(fill = guide_legend(title = "Land use category", title.position = "top")) +
      scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      xlab("Scenarios") + ylab(axis_name) +
      ylim(ymin_value, ymax_value) + 
      #scale_fill_brewer(palette = palette_name, direction = -1) +
      #facet_wrap(~ Group, ncol = 2) +
      scale_fill_manual(values = pal)
      # facet_grid(. ~ Group) 


  if(grepl("PDF", axis_name, fixed = TRUE)) {
    figure +
      ylim(ymin_value, ymax_value) +
      geom_point(data = data_top, aes(x  = Scenario, y = !! sym(column)), color = "black", size = 1) +
      #theme(panel.border = element_rect(color = "black", fill = NA)) +
      geom_errorbar(data = data_top, aes(x = Scenario, y = !! sym(column), ymin = lower95, ymax = upper95), width = 0.02, color = "black", size = 0.05)
      # guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  }

    return(figure)

  }
    


#############################################################################################################################################################################
                                                                                # 1 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 1) EU FOOTPRINT - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplotS (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: September 2020
# Author: Francesca Rosa

  
  EUfootprint.barplot.EP <- function(csv_path, file_label, plots_path, year) {
      # csv_path = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)

      data <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_EP.csv"), header = TRUE)
      data_top <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_top_EP.csv"), header = TRUE)
      
      #data <- read.csv(paste0("./plotting/cutoff_timber/EUFootprint_", year, "_mg-det_cutoff_timber.csv"), header = TRUE)
          
      # choose the palette
      pal = rev(c("#FFFFCC", "#C2E699", "#78C679", "#238443")) # palette "YlGn" of Brewer palette
      palette_name = "YlGn"
      
      # arrange the data ====
      data <- data %>% mutate(Category = str_replace(Category, "EU28_Energy_crops", "EU28 Lignocel. energy crops (domestic use)"), 
                              Category = str_replace(Category, "EU28_Forest_net", "EU28 Managed forests (domestic use)"),
                              Category = str_replace(Category, "Import_Energy_plantations", "Import - Energy plantations"), 
                              Category = str_replace(Category, "Import_Forest", "Import - Managed forests"))
      # set the order of the categories
      data$Category <- factor(data$Category, levels = c("Import - Energy plantations", "Import - Managed forests", "EU28 Managed forests (domestic use)", "EU28 Lignocel. energy crops (domestic use)"))
      
      figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name ="global PDF (%)", file_label = file_label)
      figure
      # save as pdf
      #ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP_or.pdf"), width = 30, height = 11, units = "cm")
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP_WBF.pdf"), width = 27, height = 15, units = "cm")

  }
  

#############################################################################################################################################################################
                                                                                # 2 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 2) EU INTERNAL - BARPLOT #

#############################################################################################################################################################################
# General task: plot a figure with four barplots (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: March 2021
# Author: Francesca Rosa
  
  EUinternal.barplot.EPnoex <- function(csv_path, file_label, plots_path, year) {
  
    data <- read.csv(paste0(csv_path, "EUForest_", year, file_label, "_EPnoex.csv"), header = TRUE)
    data_top <- read.csv(paste0(csv_path, "EUForest_", year, file_label,"_top_EPnoex.csv"), header = TRUE)

    pal = viridis_pal(option = "magma")(6)
      palette_name = "Viridis-magma"
            
     if(grepl("timber", file_label, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Timber", "Clear cut", "Retention", "Selection", "Other management", "Lignocellulosic energy crops"))
        } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Clear cut", "Retention", "Selection", "Other management", "Lignocel. energy crops"))
                pal = pal[-1]  
        }
      
    
    labs = c("Clear cut", "Retention", "Selection", "Energy crops")
    data <- data %>% filter(Category != "Other management")  

    figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name ="global PDF (%)", file_label = file_label)
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, file_label, "_EPnoex_WBF.png"), width = 22, height = 15, units = "cm")
    #ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, file_label, "_EPnoex_or.pdf"), width = 30, height = 11, units = "cm")

  }



#############################################################################################################################################################################
                                                                                # 3 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 3) EU FOOTPRINT - BARPLOT - DISAGGREGATED #

#############################################################################################################################################################################

# General task: plot a figure with four barplots (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: May 2022
# Author: Francesca Rosa

  
  EUfootprint.barplot.EP.dis <- function(csv_path, file_label, plots_path, year) {
      # csv_path = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)

      data <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_EP_im-for-disaggr.csv"), header = TRUE) %>%
            mutate(Category = str_replace(Category, "Import_Energy_plantations", "Energy_plantations_im"))
      data_bu <- data
      
      data_top <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_top_EP.csv"), header = TRUE)
      
      data_other_manag <- read.csv(paste0(csv_path, "EUForest_", year, file_label, "_EPnoex.csv"), header = TRUE)
      data_other_manag <- data_other_manag %>% filter(Category == "Other_management")
      
      data <- data %>% full_join(data_other_manag) %>% pivot_wider(names_from = Category, values_from = PDFx100) %>% 
        mutate(EU28_Forest_net = EU28_Forest_net - Other_management) %>%
          select(-Other_management) %>%
            pivot_longer(cols = contains("EU") | contains("_im"), names_to = "Category", values_to = "PDFx100") %>% data.frame()
                
      data_other_manag <- data_other_manag %>% rename(Values = PDFx100)
      data_top <- data_top %>% full_join(data_other_manag) %>% 
        mutate(ratio = Values/PDFx100, 
            PDFx100 = PDFx100 - Values)
      data_top <- data_top %>% mutate(lower95 = lower95 - lower95*(1-ratio), upper95 = upper95 - upper95*(1-ratio)) %>%
        select(-ratio, -Values, -Category)

      rm(data_other_manag)
      
      #data <- read.csv(paste0("./plotting/cutoff_timber/EUFootprint_", year, "_mg-det_cutoff_timber.csv"), header = TRUE)
          
      # choose the palette
      pal = carto_pal(7, "Safe")
      palette_name = "Carto-Safe"
      pal = rev(viridis_pal()(7))
      palette_name = "Viridis"
      
      # arrange the data ====
      data <- data %>% mutate(Category = str_replace(Category, "EU28_Energy_crops", "EU28 Lignocel. energy crops (domestic use)"), 
                              Category = str_replace(Category, "EU28_Forest_net", "EU28 Managed forests (domestic use)"),
                              Category = str_replace(Category, "Energy_plantations_im", "Import - Energy plantations"), 
                              Category = str_replace(Category, "Pulp_Timber_Plantation_im", "Import - Timber and pulp plantations"),
                              Category = str_replace(Category, "Clear_cut_im", "Import - Clear cut"),
                              Category = str_replace(Category, "Selection_im", "Import - Selection system"),
                              Category = str_replace(Category, "Selective_im", "Import - Selective logging"))
      
      # set the order of the categories
      data$Category <- factor(data$Category, levels = c("EU28 Lignocel. energy crops (domestic use)", "EU28 Managed forests (domestic use)", "Import - Energy plantations", "Import - Timber and pulp plantations", "Import - Clear cut",
                                                        "Import - Selection system", "Import - Selective logging"))
      
      figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name ="global PDF (%)", file_label = file_label)
      figure
      # save as pdf
      #ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP_or.pdf"), width = 30, height = 11, units = "cm")
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP_im-for-disaggr_WBF.png"), width = 27, height = 15, units = "cm")

  }
  



#############################################################################################################################################################################
                                                                                # 4 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 4) EU AREAS - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplots (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: June 2022
# Author: Francesca Rosa

  
  EUFootprint.areas.barplot.EP.dis <- function(aggr_plot_path_areas, label_timber, file_label, plots_path, year) {
      # aggr_plot_path_areas = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)

      data <-read.csv(paste0(aggr_plot_path_areas, "areas_EUFootprint_", year,"_", label_timber, "_disaggr.csv"), header = TRUE)
      data_top <-read.csv(paste0(aggr_plot_path_areas, "areas_EUFootprint_", year, "_", label_timber, "_top_EP.csv"), header = TRUE)

      # arrange the data ====

      data <- data %>% pivot_wider(names_from = Category, values_from = Values) %>% data.frame() %>%
        transmute(Group = Group, Scenario = Scenario, EU28_Energy_crops = EP_EU + EP_conv_EU, EU28_Forest_net = For_ClearCut_EU + For_Retention_EU + For_TimberPlant_EU + For_SelectionSystem_EU, # + ForOther_Extensive_EU + ForOther_Intensive_EU,
               Import_Energy_plantations = EP_conv_im + For_PlantationFuel_im, Pulp_Timber_Plantation_im = For_TimberPlant_im, Clear_cut_im = For_ClearCut_im, Selection_im = For_SelectionSystem_im, Selective_im = For_Selective_im)
      
      data <- data %>% pivot_longer(cols = EU28_Energy_crops:Selective_im, names_to = "Category", values_to = "Values") %>% data.frame()
      
      data <- data %>% mutate(Category = str_replace(Category, "EU28_Energy_crops", "EU28 Lignocel. energy crops (domestic use)"), 
                              Category = str_replace(Category, "EU28_Forest_net", "EU28 Managed forests (domestic use)"),
                              Category = str_replace(Category, "Import_Energy_plantations", "Import - Energy plantations"), 
                              Category = str_replace(Category, "Pulp_Timber_Plantation_im", "Import - Timber and pulp plantations"),
                              Category = str_replace(Category, "Clear_cut_im", "Import - Clear cut"),
                              Category = str_replace(Category, "Selection_im", "Import - Selection system"),
                              Category = str_replace(Category, "Selective_im", "Import - Selective logging"))
      
      # set the order of the categories
      data$Category <- factor(data$Category, levels = c("EU28 Lignocel. energy crops (domestic use)", "EU28 Managed forests (domestic use)", "Import - Energy plantations", "Import - Timber and pulp plantations", "Import - Clear cut",
                                                        "Import - Selection system", "Import - Selective logging"))
      
      if(grepl("mg", label, fixed = TRUE)) {
        data <- data %>% filter(Category != "Import - Selection system" & Category != "Import - Selective logging")
        #data <- droplevels(data$Category, "Import - Selection system", "Import - Selective logging")
        }
      
      # choose the palette
      pal = carto_pal(7, "Safe")
      palette_name = "Carto-Safe"
      pal = rev(viridis_pal()(7))
      palette_name = "Viridis"
      # pal = viridis_pal(option = "magma")(7)
      # palette_name = "Viridis-magma"
      
      figure <- plot.EU.barplot(data, data_top, pal, "Values", "Mha")
      figure
      # save as pdf
      #ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP_or.pdf"), width = 30, height = 11, units = "cm")
      ggsave(paste0(plots_path, "EUFootprint_areas_", year, "_", palette_name, "_", label_timber, "_", file_label, "_EP_im-for-disaggr_WBF.png"), width = 27, height = 15, units = "cm")

  }
  


  EUForest.areas.barplot.EP.dis <- function(aggr_plot_path_areas, label_timber, file_label, plots_path, year) {
      # aggr_plot_path_areas = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)

      pal = viridis_pal(option = "magma")(6)
            palette_name = "Viridis-magma"
            
      # load the data 
      data <-read.csv(paste0(aggr_plot_path_areas, "areas_EUForest_", year,"_", label_timber, "_disaggr.csv"), header = TRUE)
      data_top <-read.csv(paste0(aggr_plot_path_areas, "areas_EUForest_", year, "_", label_timber, "_top_EP.csv"), header = TRUE)
      
      # prepare them
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
        #pal = rev(c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C", "#003b18"))
          }
      
    data <- data %>% filter(Category != "Other management")  
    figure <- plot.EU.barplot(data, data_top, pal, "Values", "Mha", file_label = file_label)
      figure
      
    ggsave(paste0(plots_path, "EUForest_areas_", year, "_", palette_name, file_label, "_EPnoex_WBF.png"), width = 22, height = 15, units = "cm")

  }
  




