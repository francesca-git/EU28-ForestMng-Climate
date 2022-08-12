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
         #label_scenario <- c("no AFM (baseline)", "Laissez-faire", "AFM 12.5%", "AFM 25%", "AFM 37.5%", "AFM 50%")  
          label_scenario <- c("Baseline (noAFM)", "AFM 12.5%", "AFM 25%", "AFM 37.5%", "AFM 50%")  

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

      # Define the y limits 
      
      
        # max_per_scenario <- data %>% filter(!! sym(column) >= 0) %>% group_by(Scenario, Group) %>% summarise(max = sum(!! sym(column)))
        # max_per_scenario <- max(max_per_scenario$max, max(data_top$upper95))
        # 
        # if((round(max_per_scenario, digits = 3) - max_per_scenario) < 0.005)
        #   { ymax_value <- (round(max_per_scenario, digits = 2) + 0.01)
        #       } else { ymax_value <- round(max_per_scenario, digits = 2) }
        # 
        # min_per_scenario <- data %>% filter(!! sym(column) < 0) %>% group_by(Scenario) %>% summarise(min = sum(!! sym(column)))
        #   min_per_scenario <- min(min_per_scenario$min, min(data_top$lower95))
        # 
        # if (min_per_scenario >= 0) {ymin_value = 0} else {
        #   ymin_value <- min_per_scenario }

      }

    # set the maximum value of the y axis 
        
      if(grepl("PDF", axis_name, fixed = TRUE) && grepl("bs", file_label, fixed = TRUE)) {
      
          ymax_value = 0.47
          ymin_value = 0
          
        }
  
      
      if(grepl("PDF", axis_name, fixed = TRUE) && length(pal) < 9) {
      
          ymax_value = 0.1125
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
    
    # plot
    
    figure <-
      ggplot(data)+
      geom_bar(aes(x = Scenario, y = !! sym(column), fill = Category), size = 0.2, width = 0.45, stat = "identity", position = position_stack(reverse = FALSE)) +
      #theme_classic() + # select the theme of the plot
      #theme_minimal(base_size = 15) + # select the theme of the plot
      theme_minimal_hgrid() +
      theme(legend.position = "none", 
           legend.text = element_text(size = 15),
           axis.text = element_text(size = 15),
           axis.text.x = element_text(angle = 90),
           axis.title = element_text(size = 15),
           legend.title = element_text(size = 15),
           legend.key.size = unit(0.6, "cm"),
           strip.background = element_rect(fill = "gray"),
           panel.background = element_rect(fill = "white")) +
      guides(fill = guide_legend(title = "Land use category", title.position = "top")) +
      scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      xlab("Scenarios") + ylab(axis_name) +
      #scale_fill_brewer(palette = palette_name, direction = -1) +
      scale_fill_manual(values = pal) +
      ylim(ymin_value, ymax_value) + 
      #theme(panel.border = element_rect(color = "black", fill = NA)) +
      # guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
      #facet_wrap(Pathway ~ Group, nrow = 1) #horizontal
      facet_wrap(Pathway ~ Group, ncol = 1) #vertical
      #facet_grid(Pathway ~ Group) 

     # Include the CI if needed
    if(grepl("bs", file_label, fixed = TRUE)) {
    figure +
        geom_errorbar(data = data_top, aes(x = Scenario, y = !! sym(column), ymin = lower95, ymax = upper95), width = 0.02, color = "black", size = 0.05) +
        geom_point(data = data_top, aes(x  = Scenario, y = !! sym(column)), color = "black", size = 1)
      }
    
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

      figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name ="global PDF (%)", file_label = file_label)
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

    figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name ="global PDF (%)", file_label = file_label)
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

    #pal = rev(c("#EDF8E9", "#BAE4B3", "#74C476", "#31A354", "#006D2C")) # Greens palette of Brewer palette
    # pal = c( "#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF")
    #pal = c("#FDE725FF", "#94D840FF", "#3CBC75FF", "#1F968BFF", "2D718EFF", "#404788FF")
    #pal = c("#C2DF23FF", "#51C56AFF", "#1E9B8AFF", "#2D708EFF", "#433E85FF")
    pal = rev(c("#40004B", "#9970AB", "#C2A5CF", "#E7D4E8"))
    palette_name = "Purples"
    #pal = rev(c("#762A83", "#9970AB", "#C2A5CF", "#E7D4E8"))
    # pal = rev(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"))
    # palette_name = "Set1"

    # palette_name = "BrBG"
    # pal = (c("#8C510A", "#BF812D", "#DFC27D", "#F6E8C3"))
    # palette_name = "BrBG_rev"
    # pal = (c("#C7EAE5", "#80CDC1", "#35978F", "#01665E"))
    
     if(grepl("timber", file_label, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Selection", "Retention", "Clear_cut", "EP_EU"), labels = c("Timber", "Selection", "Retention", "Clear cut", "Lignocellulosic energy crops"))
                pal = (c("#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#40004B"))
        } else { 
        data <- data %>% filter(Category != "Timber" & Category != "Other_management")
        data$Category <- factor(data$Category, levels = c("Selection", "Retention", "Clear_cut",  "EP_EU"), labels = c("Selection", "Retention", "Clear cut","Lignocel. energy crops"))
      }
    
    labs = c("Selection","Retention",  "Clear cut", "Lignocel. energy crops")

    data <- data %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_")
    data_top <- data_top %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_")
      
    figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name = "global PDF (%)", file_label = file_label)
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, file_label, "_EPnoex_white_test.png"), width = width_height[1], height = width_height[2], units = "cm")
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
      
      figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name ="global PDF (%)", file_label = file_label)
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

    figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name ="global PDF (%)", file_label = file_label)
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
      
      figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name ="global PDF (%)", file_label = file_label)
      figure
      # save as pdf
      #ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP_or.pdf"), width = 30, height = 11, units = "cm")
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, file_label, "_EP_im-for-disaggr.pdf"), width = 27, height = 15, units = "cm")

  }
  

#############################################################################################################################################################################
                                                                                # 3 #
#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 5) EU FOOTPRINT and EU FOREST - BARPLOT all disaggregated #

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
      
      palette_name = "Brewer_VG"
      #palette_name = "BrBG_rev"
      pal_EU = (c("#40004B", "#9970AB", "#C2A5CF", "#E7D4E8"))
      #pal_EU = (c("#8C510A", "#BF812D", "#DFC27D", "#F6E8C3"))
      pal_im = rev(c("#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B"))
      #pal_im = rev(c("#C7EAE5", "#80CDC1", "#35978F", "#01665E", "#003C30"))
      
      # palette_name = "Set1"
      # pal_EU = (c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"))
      # pal_im = (c("#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

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
      
      data <- data %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_")
      data_top <- data_top %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_")
      
      # Plot
      figure <- plot.EU.barplot(data = data, data_top = data_top, pal = pal, column = "PDFx100", axis_name ="global PDF (%)", file_label = file_label)
      figure
      
      # Save as png
      ggsave(paste0(plots_path, "EUFootprint&Forest_", year, "_", palette_name, file_label, "_EP_im-for-all-disaggr_ver_white_test.png"), width = width_height[1], height = width_height[2], units = "cm")

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
      ggsave(paste0(plots_path, "EUFootprint_areas_", year, "_", palette_name, "_", label_timber, file_label, "_EP_im_int-ext.pdf"), width = width_height[1], height = width_height[2], units = "cm")

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
        
      ggsave(paste0(plots_path, "EUFootprint&Forest_areas_", year, "_", palette_name, file_label, "_EPnoex_all-dis.pdf"), width = width_height[1], height = width_height[2], units = "cm")

  }
      
