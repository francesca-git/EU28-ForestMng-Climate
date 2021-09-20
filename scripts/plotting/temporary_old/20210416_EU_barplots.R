
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
      #data <- read.csv(paste0("./plotting/cutoff_timber/EUFootprint_", year, "_mg-det_cutoff_timber.csv"), header = TRUE)
      
          
      # choose the palette
      palette_name = "Terrain 2"
      #palette_name = "bamako"
      
      # arrange the data ====
      data <- data %>% mutate(Category = str_replace(Category, "EU28_Energy_crops", "EU28 Energy crops"), 
                              Category = str_replace(Category, "EU28_Forest_net", "EU28 Forests (for internal use)"),
                              Category = str_replace(Category, "Import_Energy_plantations", "Import Energy plantations"), 
                              Category = str_replace(Category, "Import_Forest", "Import Forests"))
      # set the order of the categories
      data$Category <- factor(data$Category, levels = c("EU28 Energy crops", "Import Energy plantations", "EU28 Forests (for internal use)", "Import Forests"))
      # factorize the forest management scenarios and rename the elements 
      data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
      label_scenario <- c("no AFM", "AFM-free", "AFM 25%", "AFM 50%", "AFM 75%", "AFM 100%")  
      # rename the groups
      data <- data %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                              Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
      # set the order of the groups
      data$Group <- factor(data$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside")) 
      # ====

      figure <-
        ggplot(data, aes(x = Scenario, y = PDFx100, fill = Category))+
        geom_bar(stat='identity', colour = "black", size = 0.3, width = 0.6, position = position_stack(reverse = TRUE))+ # to create a bar plot with a specific width of the bars
        scale_x_discrete(labels = label_scenario) + # assign the names to the labels
        theme_minimal(base_size = 13) + # select the theme of the plot
        theme(legend.position="right", 
             legend.text = element_text(size = 10),
             axis.text = element_text(size = 8),
             axis.text.x = element_text(angle = 90),
             axis.title = element_text(size = 10),
             legend.title = element_blank()) +
        xlab("Scenarios") + ylab("PDF%") +
        scale_fill_discrete_sequential(palette = "Terrain 2", c2 = 20) +
        theme(panel.border = element_rect(color="grey", fill=NA)) +
        facet_wrap(~ Group) 

      figure

      # save as pdf
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, case_subcase, "_EP.pdf"), width = 20, height = 16, units = "cm")
      
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
    
    #to keep the same order between the scenarios
    data_top$Scenario <- factor(data_top$Scenario, levels=unique(data_top$Scenario))
    data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
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
    
      if (grepl("timber", case_subcase, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Timber", "Clear cut", "Other management",  "Retention", "Selection"))
      } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Clear cut", "Other management",  "Retention", "Selection"))
      }
    
    figure <-
      ggplot(data)+
      geom_bar(aes(x = Scenario, y = PDFx100, fill = Category), colour = "black",  size = 0.3, width = 0.6, stat = "identity", position = position_stack(reverse = FALSE)) +
      theme_minimal(base_size = 13) + # select the theme of the plot
      theme(legend.position="right", 
           legend.text = element_text(size = 10),
           axis.text = element_text(size = 8),
           axis.text.x = element_text(angle = 90),
           axis.title = element_text(size = 10),
           legend.title = element_text(size = 10)) +
      scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      xlab("Scenarios") + ylab("PDF%") +
      scale_fill_brewer(palette = palette_name, direction = -1) +
      #scale_fill_manual(values = pal) +
      ylim(-0.02, round(max(data_top$PDFx100), digits = 2)) + 
      geom_point(data = data_top, aes(x  = Scenario, y = PDFx100), color = "black", size = 1) +
      theme(panel.border = element_rect(color="grey", fill=NA)) +
      # geom_errorbar(data = data_top, aes(x = Scenario, y = PDFx100, ymin = lower95, ymax = upper95), width = 0.1, color = "gray60", size = 0.8) +
      facet_wrap(~ Group) +
      labs(fill = "Forest use type") 
    
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

    palette_name = "Oslo" # "#238B45","#74C476", "#BAE4B3", "#EDF8E9"
    #pal = c("#004616", "#238B45","#74C476", "#BAE4B3", "#EDF8E9")
    
    #to keep the same order between the scenarios
    data_top$Scenario <- factor(data_top$Scenario, levels=unique(data_top$Scenario))
    data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
    # rename the scenarios
    label_scenario <- c("no AFM", "AFM-free", "AFM 25%", "AFM 50%", "AFM 75%", "AFM 100%")  
    # rename the groups
    
    data_top <- data_top %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                             Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
   
      data <- data %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                              Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
      # set the order of the groups
      data$Group <- factor(data$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside")) 
      # ====

      scen <- "RCP2.6"
      
      data <- data %>% filter(Group == paste0(scen, " - Multifunctional") | Group == paste0(scen, " - Set-aside"))
      
      data <- data %>% unite("Scenario", Group:Scenario, sep = " - ")  
      
      data <- data %>% mutate(Scenario = str_replace(Scenario, paste0(scen, " - Multifunctional - noAFM"), "Continuation of current practices"),
                      Scenario = str_replace(Scenario, paste0(scen, " - Multifunctional - AFM100"), "Multifunctional"),
                      Scenario = str_replace(Scenario, paste0(scen, " - Set-aside - AFM100"), "Set-aside")) %>%
                        filter(Scenario != paste0(scen, " - Set-aside - noAFM"))
    
    data_top$Group <- factor(data_top$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside"))   
    
      if (grepl("timber", case_subcase, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Timber", "Clear cut", "Retention", "Selection", "Other management", "Energy crops"))
      } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Clear cut", "Retention", "Selection", "Other management", "Energy crops"))
      }
  

    labs = c("Clear cut", "Retention", "Selection", "Other management", "Energy crops")
    pal = c("#D7DFEF", "#8CA7D5", "#3872B6", "#2B598E", "#152b45") #oslo
    
    # set the maximum value of the y axis according to the max value of the PDF 

    if((round(max(data_top$PDFx100), digits = 3) - round(max(data_top$PDFx100), digits = 2)) < 0.005) { ymax_value <- max(data_top$PDFx100) + 0.01
          } else { ymax_value <- max(data_top$PDFx100) }
          
    if((round(max(data_top$PDFx100), digits = 3) - round(max(data_top$PDFx100), digits = 2)) < 0.005) { ymax_value <- max(data_top$PDFx100) + 0.01
    } else { ymax_value <- max(data_top$PDFx100) }

    figure <-
      ggplot(data)+
      geom_bar(aes(x = Scenario, y = PDFx100, fill = Category), colour = "black",  size = 0.3, width = 0.4, stat = "identity", position = position_stack(reverse = FALSE)) +
              theme_minimal(base_size = 13) + # select the theme of the plot
       theme(legend.position="right", 
             legend.text = element_text(size = 10),
             axis.text = element_text(size = 8),
             axis.text.x = element_text(angle = 90),
             axis.title = element_text(size = 10),
           legend.title = element_blank()) +
       theme(legend.key.height=unit(1.8, "cm")) +
      scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      xlab("Scenarios") + ylab("Global potential disappeared fraction of species (PDF) %") +
      scale_fill_manual(values = pal, labels = labs) + #, "darkred")) +
      #scale_fill_manual(values = pal) +
      ylim(0, ymax_value) + 
      coord_flip() +
      geom_point(data = data_top, aes(x  = Scenario, y = PDFx100), color = "black", size = 1) +
      theme(panel.border = element_rect(color="grey", fill=NA)) 
      # geom_errorbar(data = data_top, aes(x = Scenario, y = PDFx100, ymin = lower95, ymax = upper95), width = 0.1, color = "gray60", size = 0.8) +
      facet_wrap(~ Group) +
      labs(fill = "Forest use type") 
    
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, case_subcase, "_EPnoex.pdf"), width = 43, height = 32, units = "cm")
    
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

      # choose the palette
      palette_name = "Terrain 2"
      #palette_name = "bamako"
      
      # arrange the data ====
      data <- data %>% mutate(Category = str_replace(Category, "EU28_Forest_net", "EU28 Forests (for internal use)"),
                              Category = str_replace(Category, "Import_Forest", "Import Forests"))
      # set the order of the categories
      data$Category <- factor(data$Category, levels = c("EU28 Forests (for internal use)", "Import Forests"))
      # factorize the forest management scenarios and rename the elements 
      data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
      label_scenario <- c("no AFM", "AFM-free", "AFM 25%", "AFM 50%", "AFM 75%", "AFM 100%")  
      # rename the groups
      data <- data %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                              Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
      # set the order of the groups
      data$Group <- factor(data$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside")) 
      # ====

      figure <-
        ggplot(data, aes(x = Scenario, y = PDFx100, fill = Category))+
        geom_bar(stat='identity', colour = "black", size = 0.3, width = 0.6, position = position_stack(reverse = TRUE))+ # to create a bar plot with a specific width of the bars
        scale_x_discrete(labels = label_scenario) + # assign the names to the labels
        theme_minimal(base_size = 13) + # select the theme of the plot
        theme(legend.position="right", 
             legend.text = element_text(size = 10),
             axis.text = element_text(size = 8),
             axis.text.x = element_text(angle = 90),
             axis.title = element_text(size = 10),
             legend.title = element_blank()) +
        xlab("Scenarios") + ylab("PDF%") +
        scale_fill_discrete_sequential(palette = "Terrain 2", c2 = 20) +
        #scale_fill_scico_d(palette = palette_name, direction = -1) +
        #scale_fill_manual(values=c("brown1","darkgreen","orange","darkkhaki"))+
        #theme_bw() +
        theme(panel.border = element_rect(color="grey", fill=NA)) +
        facet_wrap(~ Group) 
      
      figure
      
      # plot 

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
  
    data<-read.csv(paste0(csv_path, "EUForest_", year, case_subcase, "_noEPnoex.csv"), header = TRUE)
    data_top<-read.csv(paste0(csv_path, "EUForest_", year, case_subcase,"_top_noEPnoex.csv"), header = TRUE)

    palette_name = "Greens" # "#238B45","#74C476", "#BAE4B3", "#EDF8E9"
    #pal = c("#004616", "#238B45","#74C476", "#BAE4B3", "#EDF8E9")
    
    #to keep the same order between the scenarios
    #data_top$Scenario <- factor(data_top$Scenario, levels=unique(data_top$Scenario))
    data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
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
      if (grepl("timber", case_subcase, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Timber", "Clear cut", "Other management",  "Retention", "Selection"))
      } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Clear cut", "Other management",  "Retention", "Selection"))
      }
    
   # set the maximum value of the y axis according to the max value of the PDF 
    
      if((round(max(data_top$PDFx100), digits = 3) - round(max(data_top$PDFx100), digits = 2)) < 0.005) { ymax_value <- max(data_top$PDFx100) + 0.01
      } else { ymax_value <- max(data_top$PDFx100) }
      
    figure <-
      ggplot(data)+
      geom_bar(aes(x = Scenario, y = PDFx100, fill = Category), colour = "black",  size = 0.3, width = 0.6, stat = "identity", position = position_stack(reverse = FALSE)) +
      theme_minimal(base_size = 13) + # select the theme of the plot
      theme(legend.position="right", 
           legend.text = element_text(size = 10),
           axis.text = element_text(size = 8),
           axis.text.x = element_text(angle = 90),
           axis.title = element_text(size = 10),
           legend.title = element_text(size = 10)) +
      scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      xlab("Scenarios") + ylab("PDF%") +
      #scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      scale_fill_brewer(palette = palette_name, direction = -1) +
      #scale_fill_manual(values = pal) +
      ylim(-0.02, ymax_value) + 
      geom_point(data = data_top, aes(x  = Scenario, y = PDFx100), color = "black", size = 1) +
      theme(panel.border = element_rect(color="grey", fill=NA)) +
      # geom_errorbar(data = data_top, aes(x = Scenario, y = PDFx100, ymin = lower95, ymax = upper95), width = 0.1, color = "gray60", size = 0.8) +
      facet_wrap(~ Group) +
      labs(fill = "Forest use type") 
    
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, case_subcase, "_noEPnoex.pdf"), width = 20, height = 16, units = "cm")
    
  }


