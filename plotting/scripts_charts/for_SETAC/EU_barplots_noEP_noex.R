
### 1) EU FOOTPRINT - BARPLOT
### 2) IMPACTS OF EU INTERNAL FORESTS - BARPLOT


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

                                                                  # 1) EU FOOTPRINT - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplotS (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: September 2020
# Author: Francesca Rosa

  EUfootprint.barplot <- function(csv_path, case_subcase, plots_path, year) {
    
      # folder_slost = folder where the files to be loaded are stored, file_slost = initial part of the name of the file to be loaded, 
      # case_subcase = identification words which selects the file, 
      # plots_path = folder where the plots will be saved

      data <-read.csv(paste0(csv_path, "EUFootprint_", year, case_subcase, "_noEP_noex.csv"), header = TRUE)
      #data <- read.csv(paste0("./plotting/cutoff_timber/EUFootprint_", year, "_mg-det_cutoff_timber.csv"), header = TRUE)
      
          
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
        # , ,
        #ggtitle("Fraction of species loss in 2100 caused by EU forest biomass demand") +
        xlab("Scenarios") + ylab("PDF%") +
        #theme(plot.title = element_text(size = 12, face = "bold.italic")) +
        scale_fill_discrete_sequential(palette = "Terrain 2", c2 = 20) +
        #scale_fill_scico_d(palette = palette_name, direction = -1) +
        #scale_fill_manual(values=c("brown1","darkgreen","orange","darkkhaki"))+
        #theme_bw() +
        theme(panel.border = element_rect(color="grey", fill=NA)) +
        facet_wrap(~ Group) 
      
        #       strip.text.x = element_text(size = 11), axis.title = element_text(size = 11), 
        #       ) + #, legend.text = element_text(size = 8), legend.title = element_text(size = 8)) +
        #labs(fill = "Land-use categ") 
      
      figure
      
      # plot 

      # save as pdf
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, case_subcase, "_noEP_noex.pdf"), width = 20, height = 16, units = "cm")
      
      # 
      # figure <- ggarrange(figure1, figure2, figure3, common.legend = TRUE, legend = "bottom")
      # 
      # figure
      # ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, "_all_taxa.pdf"), width = 20, height = 16, units = "cm")
  }
  
  


#############################################################################################################################################################################

                                                                  # 1) EU INTERNAL - BARPLOT #

#############################################################################################################################################################################

# General task: plot a figure with four barplotS (one for each combination of climate mitigation scenario and forest use scenario) 
# Date: September 2020
# Author: Francesca Rosa

  
  EUinternal.barplot <- function(csv_path, case_subcase, plots_path, year) {
  
    data<-read.csv(paste0(csv_path, "EUForest_", year, case_subcase, "_noEP_noex.csv"), header = TRUE)
    data_top<-read.csv(paste0(csv_path, "EUForest_", year, case_subcase,"_top_noEP_noex.csv"), header = TRUE)

    palette_name = "Greens" # "#238B45","#74C476", "#BAE4B3", "#EDF8E9"
    #pal = c("#004616", "#238B45","#74C476", "#BAE4B3", "#EDF8E9")
    
    #to keep the same order between the scenarios
    #data_top$Scenario <- factor(marginal_CI$Scenario, levels=unique(marginal_CI$Scenario))
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
      #ggtitle("Fraction of species loss in 2100 caused by EU internal forest management") +
      xlab("Scenarios") + ylab("PDF%") +
      #theme(plot.title = element_text(size = 12, face = "bold.italic")) +
      #scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      #scale_fill_manual(values=c("coral","burlywood","cornflowerblue","darkseagreen")) + #, "darkred")) +
      #scale_fill_manual(values=c("brown1","darkgreen","orange","darkkhaki"))+
      scale_fill_brewer(palette = palette_name, direction = -1) +
      #scale_fill_manual(values = pal) +
      ylim(-0.02, ymax_value) + 
      geom_point(data = data_top, aes(x  = Scenario, y = PDFx100), color = "black", size = 1) +
      theme(panel.border = element_rect(color="grey", fill=NA)) +
      # geom_errorbar(data = marginal_CI, aes(x = Scenario, y = PDFx100, ymin = lower95, ymax = upper95), width = 0.1, color = "gray60", size = 0.8) +
      facet_wrap(~ Group) +
      labs(fill = "Forest use type") 
    
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, case_subcase, "_noEP_noex.pdf"), width = 20, height = 16, units = "cm")
    
  }



##################################################### TIME SERIES OF IMPACTS #####################################################

#### EU FOOTPRINT

  data<-read.csv(paste0(csv_path, "EUFootprint_time-series", case_subcase, "_noEP_noex.csv"), header = TRUE)
  
  # factorize the forest management scenarios and rename the elements 
  data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
  
  label_scenario <- c("no AFM", "AFM-free", "AFM 25%", "AFM 50%", "AFM 75%", "AFM 100%")  
  data <- data %>% mutate(Scenario = str_replace(Scenario, "noAFM", "no AFM"), Scenario = str_replace(Scenario, "AFMfree", "AFM-free"),
                          Scenario = str_replace(Scenario, "AFM25", "AFM 25%"), Scenario = str_replace(Scenario, "AFM50", "AFM 50%"),
                          Scenario = str_replace(Scenario, "AFM75", "AFM 75%"), Scenario = str_replace(Scenario, "AFM100", "AFM 100%"))
  # set the order of the Scenarios
  
  # rename the groups
  data <- data %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                          Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
  # set the order of the groups
  data$Group <- factor(data$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside")) 
  
  data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
  
  palette_name = "viridis"
  pal <- c("#DCF3FA", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C")
  pal <- c("#ededed", "#D9D9D9", "#BDBDBD", "#969696", "#636363", "#252525")
  
  #jpg(file = "./plotting/noAF_CI.jpg", width = 900, height = 700)
  figure <-
  ggplot(data, aes(x=Year, y=PDFx100, group = Scenario, col = Scenario, linetype = Scenario)) +
    geom_line(size = 0.8) +
    scale_colour_viridis(discrete = TRUE)+
    #scale_colour_brewer(palette = palette_name) +
    #scale_color_manual(values = pal) +
    #scale_color_discrete_sequential(palette_name, c1 = 50) +
    #scale_color_scico_d(palette = palette_name, direction = -1) +
    theme_minimal() + # select the theme of the plot
    #geom_ribbon(aes(ymin=lower95, ymax=upper95, col = "gray70"), alpha = 0.4)+
    #theme_light(base_size = 16) +
    #geom_vline(xintercept = 2020) + 
    xlim(2020, 2100)+
    xlab("Years") + ylab("PDF%") +
    theme(plot.title = element_text(size = 12, face = "bold.italic")) +
    theme(panel.border = element_rect(color="grey", fill=NA)) +
    facet_wrap(~ Group) +
    theme(legend.position="bottom", legend.text = element_text(size = 11),
          strip.text.x = element_text(size = 11), axis.title = element_text(size = 11), 
          axis.text = element_text(size = 9), 
          #legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(1.5,"cm")) #, legend.text = element_text(size = 8), legend.title = element_text(size = 8)) +
    ggtitle("Fraction of species loss over time caused by EU forest biomass demand")
  
  figure
  
  # plot 
  
  # save as pdf
  ggsave(paste0(plots_path, "EUFootprint_time-series", case_subcase, "_noEP_noex.pdf"), width = 20, height = 16, units = "cm")

  
#### EU FOREST INTERNAL

  data<-read.csv(paste0(csv_path, "EUForest_time-series", case_subcase, "_noEP_noex.csv"), header = TRUE)
  
  # factorize the forest management scenarios and rename the elements 
  data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
  
  label_scenario <- c("no AFM", "AFM-free", "AFM 25%", "AFM 50%", "AFM 75%", "AFM 100%")  
  data <- data %>% mutate(Scenario = str_replace(Scenario, "noAFM", "no AFM"), Scenario = str_replace(Scenario, "AFMfree", "AFM-free"),
                          Scenario = str_replace(Scenario, "AFM25", "AFM 25%"), Scenario = str_replace(Scenario, "AFM50", "AFM 50%"),
                          Scenario = str_replace(Scenario, "AFM75", "AFM 75%"), Scenario = str_replace(Scenario, "AFM100", "AFM 100%"))
  # set the order of the Scenarios
  
  # rename the groups
  data <- data %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                          Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
  # set the order of the groups
  data$Group <- factor(data$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside")) 
  
  data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
  
  palette_name = "viridis"
  pal <- c("#DCF3FA", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C")
  pal <- c("#ededed", "#D9D9D9", "#BDBDBD", "#969696", "#636363", "#252525")
  
  #jpg(file = "./plotting/noAF_CI.jpg", width = 900, height = 700)
  figure <-
  ggplot(data, aes(x=Year, y=PDFx100, group = Scenario, col = Scenario, linetype = Scenario)) +
    geom_line(size = 0.8) +
    scale_colour_viridis(discrete = TRUE)+
    #scale_colour_brewer(palette = palette_name) +
    #scale_color_manual(values = pal) +
    #scale_color_discrete_sequential(palette_name, c1 = 50) +
    #scale_color_scico_d(palette = palette_name, direction = -1) +
    theme_minimal() + # select the theme of the plot
    #geom_ribbon(aes(ymin=lower95, ymax=upper95, col = "gray70"), alpha = 0.4)+
    #theme_light(base_size = 16) +
    #geom_vline(xintercept = 2020) + 
    xlim(2020, 2100)+
    xlab("Years") + ylab("PDF%") +
    theme(plot.title = element_text(size = 12, face = "bold.italic")) +
    theme(panel.border = element_rect(color="grey", fill=NA)) +
    facet_wrap(~ Group) +
    theme(legend.position="bottom", legend.text = element_text(size = 11),
          strip.text.x = element_text(size = 11), axis.title = element_text(size = 11), 
          axis.text = element_text(size = 9), 
          #legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(1.5,"cm")) #, legend.text = element_text(size = 8), legend.title = element_text(size = 8)) +
    ggtitle("Fraction of species loss over time caused by EU forest biomass demand")
  
  figure
  
  # plot 
  
  # save as pdf
  ggsave(paste0(plots_path, "EUForest_time-series", case_subcase, "_noEP_noex.pdf"), width = 20, height = 16, units = "cm")

  
  
  
  