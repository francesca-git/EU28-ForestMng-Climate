# Functions:

## plotting function used in all the other functions - plot.EU.barplot

### 1a) EU FOOTPRINT - BARPLOT (energy plantations included) - EUfootprint.barplot.EP
### 1b) IMPACTS OF EU INTERNAL FORESTS - BARPLOT (energy crops excluded, exports included) - EUinternal.barplot.noEPex

### 2) IMPACTS OF EU INTERNAL FORESTS - BARPLOT (energy crops included, exports excluded) - EUinternal.barplot.EPnoex

### 3a) EU FOOTPRINT - BARPLOT (energy plantations excluded, exports excluded) - EUfootprint.barplot.noEP
### 3b) IMPACTS OF EU INTERNAL FORESTS - BARPLOT (energy crops excluded, exports excluded) - EUinternal.barplot.noEPnoex

### 4) EU FOOTPRINT - BARPLOT (energy plantations included, imported forest disaggregated) - EUfootprint.barplot.EP.fordis

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
  

################################################### PLOTTING FUNCTION #####################################################################
                    
# This function plots a stacked barplot of European impacts (both footprint and internal), for the two climate scenarios 
# and the multiple forest management scenarios
# the arguments are 
#     data: dataframe with the values for each category 
#     data_top: total value per bar
#     palette_name: name of the palette according to the Brewer color scale

plot.EU.barplot <- function(data, data_top, pal) {
  
    
    data <- data %>% filter(Scenario != "AFMfree")
    data_top <- data_top %>% filter(Scenario != "AFMfree")

    # rename the scenarios
    #label_scenario <- c("no AFM (baseline)", "Laissez-faire", "AFM 12.5%", "AFM 25%", "AFM 37.5%", "AFM 50%")  
    label_scenario <- c("Baseline (noAFM)", "AFM 12.5%", "AFM 25%", "AFM 37.5%", "AFM 50%")  

    data_bu <- data
    data <- data_bu
    # rename the groups
    # data <- data %>% unite("Management", Group:Scenario, sep = "!", remove = TRUE)
    # data <- data %>% mutate(Management = str_replace(Management, "RCP2.6_MFM!noAFM", "RCP2.6 - Baseline!noAFM"),
    #                         Management = str_replace(Management, "RCP2.6_SFM!noAFM", "RCP2.6 - Baseline!noAFM"),
    #                         Management = str_replace(Management, "REF_MFM!noAFM", "RCP6.5 (REF) - Baseline!noAFM"),
    #                         Management = str_replace(Management, "REF_SFM!noAFM", "RCP6.5 (REF) - Baseline!noAFM"),
    #                         Management = str_replace(Management, "RCP2.6_MFM!AFMfree", "RCP2.6 - Free model!AFMfree"),
    #                         Management = str_replace(Management, "RCP2.6_SFM!AFMfree", "RCP2.6 - Free model!AFMfree"),
    #                         Management = str_replace(Management, "REF_MFM!AFMfree", "RCP6.5 (REF) - Free model!AFMfree"),
    #                         Management = str_replace(Management, "REF_SFM!AFMfree", "RCP6.5 (REF) - Free model!AFMfree")) %>%
    #                     separate(Management, c("Group", "Scenario"), sep = "!")
    # 
    data <- data %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 - Close-to-nature"), Group = str_replace(Group, "REF_SFM", "RCP6.5 - Set-aside"),
                            Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Close-to-nature"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside")) 

    # data_top <- data_top %>% unite("Management", Group:Scenario, sep = "!", remove = TRUE)
    # data_top <- data_top %>% mutate(Management = str_replace(Management, "RCP2.6_MFM!noAFM", "RCP2.6 - Baseline!noAFM"),
    #                         Management = str_replace(Management, "RCP2.6_SFM!noAFM", "RCP2.6 - Baseline!noAFM"),
    #                         Management = str_replace(Management, "REF_MFM!noAFM", "RCP6.5 (REF) - Baseline!noAFM"),
    #                         Management = str_replace(Management, "REF_SFM!noAFM", "RCP6.5 (REF) - Baseline!noAFM"),
    #                         Management = str_replace(Management, "RCP2.6_MFM!AFMfree", "RCP2.6 - Free model!AFMfree"),
    #                         Management = str_replace(Management, "RCP2.6_SFM!AFMfree", "RCP2.6 - Free model!AFMfree"),
    #                         Management = str_replace(Management, "REF_MFM!AFMfree", "RCP6.5 (REF) - Free model!AFMfree"),
    #                         Management = str_replace(Management, "REF_SFM!AFMfree", "RCP6.5 (REF) - Free model!AFMfree")) %>%
    #                     separate(Management, c("Group", "Scenario"), sep = "!")
    data_top <- data_top %>% mutate(Group = str_replace(Group,  "REF_MFM", "RCP6.5 - Close-to-nature"), Group = str_replace(Group, "REF_SFM", "RCP6.5 - Set-aside"),
                             Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Close-to-nature"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
    
    data <- data %>% mutate(Scenario = str_replace(Scenario, "AFM25", "AFM12.5"), Scenario = str_replace(Scenario, "AFM50", "AFM25"),
                            Scenario = str_replace(Scenario, "AFM75", "AFM37.5"), Scenario = str_replace(Scenario, "AFM100", "AFM50")) 
    data_top <- data_top %>% mutate(Scenario = str_replace(Scenario, "AFM25", "AFM12.5"), Scenario = str_replace(Scenario, "AFM50", "AFM25"),
                            Scenario = str_replace(Scenario, "AFM75", "AFM37.5"), Scenario = str_replace(Scenario, "AFM100", "AFM50"))
    
    #to keep the same order between the scenarios
    data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
    data_top$Scenario <- factor(data_top$Scenario, levels=unique(data_top$Scenario))
    
    # set the order of the groups
    data$Group <- factor(data$Group, levels = c("RCP6.5 - Close-to-nature", "RCP6.5 - Set-aside", "RCP2.6 - Close-to-nature", "RCP2.6 - Set-aside"))
    data_top$Group <- factor(data_top$Group, levels = c("RCP6.5 - Close-to-nature", "RCP6.5 - Set-aside", "RCP2.6 - Close-to-nature", "RCP2.6 - Set-aside"))

    # set the maximum value of the y axis according to the max value of the PDF 

      #max_data_top <- max(max(data_top$PDFx100), max(data_top$upper95))
      max_per_scenario <- data %>% filter(PDFx100 >= 0) %>% group_by(Group, Scenario) %>% summarise(max = sum(PDFx100))
      max_per_scenario <- max(max_per_scenario$max, max(data_top$upper95))
      
      if((round(max_per_scenario, digits = 3) - max_per_scenario) < 0.005)
        { ymax_value <- (round(max_per_scenario, digits = 2) + 0.01)
            } else { ymax_value <- round(max_per_scenario, digits = 2) }

      min_per_scenario <- data %>% filter(PDFx100 < 0) %>% group_by(Group, Scenario) %>% summarise(min = sum(PDFx100))
      min_per_scenario <- min(min_per_scenario$min, min(data_top$lower95))
      
      if (min_per_scenario >= 0) {ymin_value = 0} else {
        ymin_value <- min_per_scenario }

    #ymax_value = 0.3
    # for EU footprint with fixed legend  
    # ymin_value = -0.02
    # ymax_value = 0.3
    # # for EU footprint with fixed legend
    # ymin_value = 0
    # ymax_value = 0.5
      
    # for EU internal forest with fixed legend
    # ymin_value = -0.05
    # ymax_value = 0.1

    # plot
    
    figure <-
      ggplot(data)+
      geom_bar(aes(x = Scenario, y = PDFx100, fill = Category), size = 0.2, width = 0.5, stat = "identity", position = position_stack(reverse = FALSE)) +
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
      xlab("Scenarios") + ylab("global PDF (%)") +
      #scale_fill_brewer(palette = palette_name, direction = -1) +
      scale_fill_manual(values = pal) +
      ylim(ymin_value, ymax_value) + 
      geom_point(data = data_top, aes(x  = Scenario, y = PDFx100), color = "black", size = 1) +
      #theme(panel.border = element_rect(color = "black", fill = NA)) +
      geom_errorbar(data = data_top, aes(x = Scenario, y = PDFx100, ymin = lower95, ymax = upper95), width = 0.02, color = "black", size = 0.05) +
      # guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
      facet_wrap(~ Group, ncol = 2) 
      # facet_grid(. ~ Group) 

    
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
      
      figure <- plot.EU.barplot(data, data_top, pal)
      figure
      # save as pdf
      #ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP_or.pdf"), width = 30, height = 11, units = "cm")
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP.pdf"), width = 27, height = 15, units = "cm")

  }
  


#############################################################################################################################################################################

                                                                  # 1b) EU INTERNAL - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplotS (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: September 2020
# Author: Francesca Rosa

  
  EUinternal.barplot.noEPex <- function(csv_path, file_label, plots_path, year) {
  
    data <- read.csv(paste0(csv_path, "EUForest_", year, file_label, "_noEPex.csv"), header = TRUE)
    data_top <- read.csv(paste0(csv_path, "EUForest_", year, file_label,"_top_noEPex.csv"), header = TRUE)

    pal = rev(c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C")) # Greens palette of Brewer palette
    palette_name = "Greens"

    if (grepl("timber", file_label, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Timber", "Clear cut", "Other management",  "Retention", "Selection"))
        pal = rev(c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C", "#003b18"))
        } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Clear cut", "Other management",  "Retention", "Selection"))
      }
    
    labs = c("Clear cut", "Retention", "Selection", "Other management")

    figure <- plot.EU.barplot(data, data_top, pal)
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, file_label, "_noEPex.pdf"), width = 23, height = 10, units = "cm")
    
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

    pal = rev(c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C")) # Greens palette of Brewer palette
    # pal = c( "#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF")
    #pal = c("#FDE725FF", "#94D840FF", "#3CBC75FF", "#1F968BFF", "2D718EFF", "#404788FF")
    #pal = c("#C2DF23FF", "#51C56AFF", "#1E9B8AFF", "#2D708EFF", "#433E85FF") 
    palette_name = "Greens"
    
     if(grepl("timber", file_label, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Timber", "Clear cut", "Retention", "Selection", "Other management", "Lignocellulosic energy crops"))
                pal = rev(c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C", "#003b18"))
        } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Clear cut", "Retention", "Selection", "Other management", "Lignocel. energy crops"))
      }
    
    labs = c("Clear cut", "Retention", "Selection", "Other management", "Energy crops")

    figure <- plot.EU.barplot(data, data_top, pal)
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, file_label, "_EPnoex.pdf"), width = 22, height = 15, units = "cm")
    #ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, file_label, "_EPnoex_or.pdf"), width = 30, height = 11, units = "cm")

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

  EUfootprint.barplot.noEP <- function(csv_path, file_label, plots_path, year) {

      data <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_noEP.csv"), header = TRUE)
      data_top <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_top_noEP.csv"), header = TRUE)
      
      #data <- read.csv(paste0("./plotting/cutoff_timber/EUFootprint_", year, "_mg-det_cutoff_timber.csv"), header = TRUE)
          
      # choose the palette
      pal = rev(c("#FFFFCC", "#C2E699", "#78C679", "#238443")) # palette "YlGn" of Brewer palette
      palette_name = "YlGn"
      
      # arrange the data ====
      data <- data %>% mutate(Category = str_replace(Category, "EU28_Forest_net", "EU28 Forests (for domestic use)"),
                              Category = str_replace(Category, "Import_Forest", "Import Forests"))
      
      # set the order of the categories
      data$Category <- factor(data$Category, levels = c("EU28 Forests (for domestic use)", "Import Forests"))
      
      figure <- plot.EU.barplot(data, data_top, pal)
      figure
      # save as pdf
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_noEP.pdf"), width = 20, height = 16, units = "cm")
      
  }
  
  

#############################################################################################################################################################################

                                                                  # 3b) EU INTERNAL - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplotS (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: MArch 2021
# Author: Francesca Rosa

  EUinternal.barplot.noEPnoex <- function(csv_path, file_label, plots_path, year) {
  
    data <- read.csv(paste0(csv_path, "EUForest_", year, file_label, "_noEPnoex.csv"), header = TRUE)
    data_top <- read.csv(paste0(csv_path, "EUForest_", year, file_label,"_top_noEPnoex.csv"), header = TRUE)

    pal = rev(c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C")) # Greens palette of Brewer palette
    palette_name = "Greens"

    if (grepl("timber", file_label, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Timber", "Clear cut", "Other management",  "Retention", "Selection"))
        pal = rev(c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C", "#003b18"))
        } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Clear cut", "Other management",  "Retention", "Selection"))
      }
    
    labs = c("Clear cut", "Retention", "Selection", "Other management")

    figure <- plot.EU.barplot(data, data_top, pal)
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, file_label, "_noEPnoex.pdf"), width = 20, height = 16, units = "cm")
    
  }


#############################################################################################################################################################################
                                                                                # 4 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 4) EU FOOTPRINT - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplots (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: May 2022
# Author: Francesca Rosa

  
  EUfootprint.barplot.EP.dis <- function(csv_path, file_label, plots_path, year) {
      # csv_path = path of the .csv files where the data to plot are stored (character vector)
      # file_label = identification words which selects the file (character vector)
      # plots_path = folder where the plots will be saved (character vector)
      # year = year that will be plotted (number)

      data <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_EP_im-for-disaggr.csv"), header = TRUE)
      data_top <-read.csv(paste0(csv_path, "EUFootprint_", year, file_label, "_top_EP.csv"), header = TRUE)
      
      #data <- read.csv(paste0("./plotting/cutoff_timber/EUFootprint_", year, "_mg-det_cutoff_timber.csv"), header = TRUE)
          
      # choose the palette
      pal = carto_pal(7, "Safe")
      palette_name = "Carto-Safe"
      
      # arrange the data ====
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
      
      figure <- plot.EU.barplot(data, data_top, pal)
      figure
      # save as pdf
      #ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP_or.pdf"), width = 30, height = 11, units = "cm")
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP_im-for-disaggr.pdf"), width = 27, height = 15, units = "cm")

  }
  


