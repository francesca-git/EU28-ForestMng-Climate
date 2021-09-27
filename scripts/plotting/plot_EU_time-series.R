
setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 

#data<-read.csv("./plotting/charts_examples/Global_development_sel.csv", header = TRUE)

#dev.new()
library(ggplot2)
library(viridis)
library(colorspace)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(scico)


plot.EU.timeseries <- function(csv_path, file_label, plots_path, id, energy_exports) {
  
  if (id == "EUFootprint") {
    if(energy_exports == "EPnoex" | energy_exports == "ex") {
    file_name = paste0("EUFootprint_time-series", file_label, "_EP.csv")
    } else if(energy_exports == "noEPnoex"){ file_name = paste0("EUFootprint_time-series", file_label, "_noEP.csv")
    }
  } else if (id == "EUForest") {
    if(energy_exports == "EPnoex") { file_name = paste0("EUForest_time-series", file_label, "_EPnoex.csv")
    } else if(energy_exports == "ex") { file_name = paste0("EUForest_time-series", file_label, "_ex.csv")
    } else if(energy_exports == "noEPnoex") { file_name = paste0("EUForest_time-series", file_label, "_noEPnoex.csv")
      
    }
  }
  
  data <- read.csv(paste0(csv_path, file_name), header = TRUE)
  
  data <- data %>% filter(Scenario != "AFMfree")
  # factorize the forest management scenarios and rename the elements 
  data$Scenario <- factor(data$Scenario, levels = unique(data$Scenario))
  
  
  label_scenario <- c("no AFM", "AFM 12.5%", "AFM 25%", "AFM 37.5%", "AFM 50%")  
  data <- data %>% mutate(Scenario = str_replace(Scenario, "noAFM", "no AFM"),
                          Scenario = str_replace(Scenario, "AFM25", "AFM 12.5%"), Scenario = str_replace(Scenario, "AFM50", "AFM 25%"),
                          Scenario = str_replace(Scenario, "AFM75", "AFM 37.5%"), Scenario = str_replace(Scenario, "AFM100", "AFM 50%"))
  # set the order of the Scenarios
  
  # rename the groups
  data <- data %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (BAU) - Close-to-naure"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (BAU) - Set-aside"),
                          Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Close-to-naure"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
  # set the order of the groups
  data$Group <- factor(data$Group, levels = c("RCP6.5 (BAU) - Close-to-naure", "RCP6.5 (BAU) - Set-aside", "RCP2.6 - Close-to-naure", "RCP2.6 - Set-aside")) 
  
  data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
  
  palette_name = "viridis"
  pal <- c("#DCF3FA", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C")
  pal <- c("#ededed", "#D9D9D9", "#BDBDBD", "#969696", "#636363", "#252525")
  
  #jpg(file = "./plotting/noAF_CI.jpg", width = 900, height = 700)
  figure <-
  ggplot(data, aes(x = Year, y = PDFx100, group = Scenario, col = Scenario, linetype = Scenario)) +
    geom_line(size = 0.8) +
    scale_colour_viridis(discrete = TRUE)+
    theme_minimal() +
    ylim(0, max(data$PDFx100)) +
    xlim(2020, 2100)+
    xlab("Years") + ylab("PDF%") +
    theme(plot.title = element_text(size = 12, face = "bold.italic")) +
    theme(panel.border = element_rect(color="grey", fill=NA)) +
    facet_wrap(~ Group) +
    theme(legend.position="bottom", legend.text = element_text(size = 11),
          strip.text.x = element_text(size = 11), axis.title = element_text(size = 11), 
          axis.text = element_text(size = 9), 
          legend.key.width = unit(1.5,"cm")) 

  figure
  
  # save as pdf
  ggsave(paste0(plots_path, id, "_time-series", file_label, ".pdf"), width = 10, height = 8)

}