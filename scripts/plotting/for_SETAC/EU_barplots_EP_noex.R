
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
library(extrafont)

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

      data <-read.csv(paste0(csv_path, "EUFootprint_", year, case_subcase, ".csv"), header = TRUE)
      #data <- read.csv(paste0("./plotting/cutoff_timber/EUFootprint_", year, "_mg-det_cutoff_timber.csv"), header = TRUE)
      
      # choose the palette
      palette_name = "Grays"
      #palette_name = "bamako"
      
      # arrange the data ====
      data <- data %>% mutate(Category = str_replace(Category, "EU28_Energy_crops", "EU28 forests and energy crops"), 
                              Category = str_replace(Category, "EU28_Forest_net", "EU28 forests and energy crops"),
                              Category = str_replace(Category, "Import_Energy_plantations",  "Import of energywood"), 
                              Category = str_replace(Category, "Import_Forest", "Import of wood"))
      
      
      data <- data.frame(data %>% group_by(Group, Scenario, Category) %>% summarise_if(is.numeric, sum, na.rm =TRUE))

      # set the order of the categories
      data$Category <- factor(data$Category, levels = c("EU28 forests and energy crops", "Import of energywood", "Import of wood"))

      labs <- c("EU28 forests and energy crops\n(for internal use)", "Import of energywood\nfrom energy plantations", "Import of wood (roundwood, semi-\nfinished products and wood pellets)")
    
      # factorize the forest management scenarios and rename the elements 
      data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
      label_scenario <- c("no AFM", "AFM-free", "AFM 25%", "AFM 50%", "AFM 75%", "AFM 100%")  
      # rename the groups
      data <- data %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                              Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
      # set the order of the groups
      data$Group <- factor(data$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside")) 
      # ====

      scen <- "RCP2.6"
      
      data <- data %>% filter((Group == paste0(scen, " - Multifunctional") | Group == paste0(scen, " - Set-aside")) & (Scenario == "noAFM" | Scenario == "AFM100"))
      
      data <- data %>% unite("Scenario", Group:Scenario, sep = " - ")  
      
      data <- data %>% mutate(Scenario = str_replace(Scenario, paste0(scen, " - Multifunctional - noAFM"), "Continuation of current practices"),
                      Scenario = str_replace(Scenario, paste0(scen, " - Multifunctional - AFM100"), "Multifunctional"),
                      Scenario = str_replace(Scenario, paste0(scen, " - Set-aside - AFM100"), "Set-aside")) %>%
                        filter(Scenario != paste0(scen, " - Set-aside - noAFM"))

      data$Scenario <- factor(data$Scenario, levels = c("Set-aside", "Multifunctional", "Continuation of current practices"))
  
                    
      figure <-
        ggplot(data, aes(x = Scenario, y = PDFx100, fill = Category))+
        geom_bar(stat='identity', colour = "black", size = 0.3, width = 0.4, position = position_stack(reverse = TRUE))+ # to create a bar plot with a specific width of the bars
        #scale_x_discrete(labels = label_scenario) + # assign the names to the labels
         theme_minimal(base_size = 17) + # select the theme of the plot
          theme(legend.position="top",
           legend.text = element_text(size = 24),
           axis.text.y = element_text(size = 24),
           axis.title = element_text(size = 19),
           legend.title = element_blank()) +
        # , ,
        #ggtitle("Fraction of species loss in 2100 caused by EU forest biomass demand") +
        xlab("Scenarios") + ylab("Global potential disappeared fraction of species (PDF) %") +
        coord_flip() +
        #theme(plot.title = element_text(size = 12, face = "bold.italic")) +
        #scale_fill_discrete_sequential(palette = "Terrain 2", c2 = 20) +
        scale_fill_discrete_sequential(palette = palette_name, labels = labs) +
        #scale_fill_brewer(palette = palette_name) +
        #scale_fill_scico_d(palette = palette_name, direction = -1) +
        #scale_fill_manual(values=c("brown1","darkgreen","orange","darkkhaki"))+
        #theme_bw() +
        #facet_wrap(~ Group) +
        #theme(text = element_text(size = 7, family = "Gill Sans MT")) +
        theme(panel.border = element_rect(color="grey", fill=NA))

        #       strip.text.x = element_text(size = 11), axis.title = element_text(size = 11), 
        #       ) + #, legend.text = element_text(size = 8), legend.title = element_text(size = 8)) +
        #labs(fill = "Land-use categ") 
      
      figure
      
      # plot 

      # save as pdf
      ggsave(paste0(plots_path, "EUFootprint_", year, "_", palette_name, case_subcase, "_setac.pdf"), width = 54, height = 32, units = "cm")
      
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
  
    data<-read.csv(paste0(csv_path, "EUForest_", year, case_subcase, "_EP_noex.csv"), header = TRUE)
    data_top<-read.csv(paste0(csv_path, "EUForest_", year, case_subcase,"_top_EP_noex.csv"), header = TRUE)

    palette_name = "Oslo" # "#238B45","#74C476", "#BAE4B3", "#EDF8E9"
    #pal = c("#004616", "#238B45","#74C476", "#BAE4B3", "#EDF8E9")
    
    #to keep the same order between the scenarios
    #data_top$Scenario <- factor(marginal_CI$Scenario, levels=unique(marginal_CI$Scenario))
    data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
    # rename the scenarios
    label_scenario <- c("no AFM", "AFM-free", "AFM 25%", "AFM 50%", "AFM 75%", "AFM 100%")  
    # rename the groups
    
    #data_top <- data_top %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                             #Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
   
      data <- data %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                              Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
      # set the order of the groups
      data$Group <- factor(data$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside")) 
      # ====

      scen <- "RCP2.6"
      
      data <- data %>% filter((Group == paste0(scen, " - Multifunctional") | Group == paste0(scen, " - Set-aside")) & (Scenario == "noAFM" | Scenario == "AFM100"))
      
      data <- data %>% unite("Scenario", Group:Scenario, sep = " - ")  
      
      data <- data %>% mutate(Scenario = str_replace(Scenario, paste0(scen, " - Multifunctional - noAFM"), "Continuation of current practices"),
                      Scenario = str_replace(Scenario, paste0(scen, " - Multifunctional - AFM100"), "Multifunctional"),
                      Scenario = str_replace(Scenario, paste0(scen, " - Set-aside - AFM100"), "Set-aside")) %>%
                        filter(Scenario != paste0(scen, " - Set-aside - noAFM"))
    
    #data_top$Group <- factor(data_top$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside"))   
      if (grepl("timber", case_subcase, fixed = TRUE)) {
        data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Timber", "Clear cut", "Retention", "Selection", "Other management", "Energy crops"))
      } else { 
        data <- data %>% filter(Category != "Timber")
        data$Category <- factor(data$Category, levels = c("Clear_cut", "Retention", "Selection", "Other_management", "EP_EU"), labels = c("Clear cut", "Retention", "Selection", "Other management", "Energy crops"))
      }
    
    # set the maximum value of the y axis according to the max value of the PDF 
    
      if((round(max(data_top$PDFx100), digits = 3) - round(max(data_top$PDFx100), digits = 2)) < 0.005) { ymax_value <- max(data_top$PDFx100) + 0.01
      } else { ymax_value <- max(data_top$PDFx100) }
      
    data$Scenario <- factor(data$Scenario, levels = c("Set-aside", "Multifunctional", "Continuation of current practices"))
      
    labs = c("Clear cut", "Retention", "Selection", "Other management", "Energy crops")
    pal = c("#D7DFEF", "#8CA7D5", "#3872B6", "#2B598E", "#152b45") #oslo

    figure <-
      ggplot(data)+
      geom_bar(aes(x = Scenario, y = PDFx100, fill = Category), colour = "black",  size = 0.3, width = 0.4, stat = "identity", position = position_stack(reverse = FALSE)) +
      theme_minimal(base_size = 17) + # select the theme of the plot
          theme(legend.position="top",
           legend.text = element_text(size = 24),
           axis.text.y = element_text(size = 24),
           axis.title = element_text(size = 19),
           legend.title = element_blank()) +
       theme(legend.key.height=unit(1.8, "cm")) +
      #scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      #ggtitle("Fraction of species loss in 2100 caused by EU internal forest management") +
      xlab("Scenarios") + ylab("Global potential disappeared fraction of species (PDF) %") +
      #theme(plot.title = element_text(size = 12, face = "bold.italic")) +
      #scale_x_discrete(labels = label_scenario) + # assign the names to the labels
      scale_fill_manual(values = pal, labels = labs) + #, "darkred")) +
      #scale_fill_manual(values=c("brown1","darkgreen","orange","darkkhaki"))+
      #scale_fill_brewer(palette = palette_name, direction = -1) +
      #scale_fill_discrete_sequential(palette = "Oslo", c1 = 20) +
      #scale_fill_manual(values = pal) +
      ylim(0, ymax_value) + 
      coord_flip() +
      #geom_point(data = data_top, aes(x  = Scenario, y = PDFx100), color = "black", size = 1) +
      theme(panel.border = element_rect(color="grey", fill=NA)) 
      # geom_errorbar(data = marginal_CI, aes(x = Scenario, y = PDFx100, ymin = lower95, ymax = upper95), width = 0.1, color = "gray60", size = 0.8) +
      #facet_wrap(~ Group) +
      #labs(fill = "Forest use type") 
    
    figure
    
    ggsave(paste0(plots_path, "EUForest_", year, "_", palette_name, case_subcase, "_EP_noex_setac.pdf"), width = 43, height = 32, units = "cm")
    
  }