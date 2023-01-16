
### 1) GLOBAL LAND USE IMPACTS AND GLOBAL AREAS - STACKED TIME SERIES
### 2) GLOBAL LAND USE IMPACTS - TIME SERIES



#############################################################################################################################################################################

# WARNING: The functions defined in this file are called in the file csv_and_plotting.R. The loading/saving paths and the working directory are defined in that file as well.

#############################################################################################################################################################################


#############################################################################################################################################################################

                                                                  # 1) GLOBAL LAND USE IMPACTS AND GLOBAL AREAS - STACKED TIME SERIES #

#############################################################################################################################################################################

# General task: plot two stacked time series disaggregated per land use category (one for the areas and one for the impacts) from the corresponding .csv files 
# Date: September 2020
# Author: Francesca Rosa

#dev.new()
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(gridExtra)
library(nord)
library(ggpubr)
library(scico)
library(colorspace)
library(stringr)
select <- dplyr::select


plot.global.time.series <- function(csv_path, file_label, plots_path, label_timber, aggr_plot_path_areas) {
  
  # select the palette
  
    palette_name = "batlow"
    dir = 1
    palette = c("#09143d", "#264b5a", "#4F7B66", "#B1A557", "#E3A65A", "#F8AA88", "#fecace", "#FFE6FF")
    
    
  # load and prepare the the data
    
    data <- read.csv(paste0(csv_path, "global_time-series", file_label, ".csv"), header = TRUE)
    
    data$Category <- factor(data$Category, levels=unique(data$Category))
    data <- data %>% mutate(Group = str_replace(Group, "REF", "RCP6.5 (BAU)"))
    data$Group = factor(data$Group, levels = c("RCP6.5 (BAU)", "RCP2.6"))
  
  # plot the impacts
  
    figure1 <-
    ggplot(data, aes(x=Year, y=PDF, fill = Category)) +
      geom_area(alpha = 0.85, size = 0.2, color = "gray85") +
      scale_fill_manual(values = palette, labels =  c("Afforestation", "Regrowth", "Managed forests", "Energy crops and plantations", "Annual crops", "Pastures", "Permanent crops", "Urban"))+
      xlim(2020, 2100)+
      ggtitle("Biodiversity impacts")+
      labs(y = "PDF %") +
      theme_minimal(base_size = 15) +
      theme(plot.title = element_text(size = 16, face = "italic"),
                    legend.text = element_text(size = 12)) +
      facet_wrap(~Group, ncol = 1)+
      labs(fill = "Land-use category") 
    
   
    rm(data)
    
  # plot the areas (reload the data and adpt the palette)

    data <- read.csv(paste0(aggr_plot_path_areas, "areas_global_time-series_", label_timber, ".csv"), header = TRUE)
    palette = c("#000820", "#09143d", "#264b5a", "#4F7B66", "#B1A557", "#E3A65A", "#F8AA88", "#fecace", "#FFE6FF")

    data$Category <- factor(data$Category, levels=unique(data$Category))
    data <- data %>% mutate(Group = str_replace(Group, "REF", "RCP6.5 (BAU)"))
    data$Group = factor(data$Group, levels = c("RCP6.5 (BAU)", "RCP2.6"))
    
    figure2 <- 
      ggplot(data, aes(x=Year, y=Values, fill = Category)) +
      geom_area(alpha=0.85, size= 0.2, color = "gray85") +
      scale_fill_manual(values = palette, labels = c("Natural", "Afforestation", "Regrowth", "Managed forests", "Energy crops and plantations", "Annual crops", "Pastures", "Permanent crops", "Urban")) +
      xlim(2020, 2100)+
      ggtitle("Land use")+
      labs(y = "Area (Mha)") +
      theme_minimal(base_size = 15) +
      theme(plot.title = element_text(size = 16, face = "italic"),
            legend.text = element_text(size = 12)) +
      facet_wrap(~Group, ncol = 1) +
      labs(fill = "Land-use category") 
    
      
    rm(data)
    
  # put the figures together
    
    figure <- ggarrange(figure2, figure1, common.legend = TRUE, legend = "right")
    
    figure
    
    ggsave(paste0(plots_path, "Global_time-series_", palette_name, file_label, ".pdf"), width = 21, height = 20, units = "cm")

}   


#############################################################################################################################################################################

                                                                  # 2) GLOBAL LAND USE IMPACTS - TIME SERIES #

#############################################################################################################################################################################

# General task: plot the time series of the global impacts with CI from the corresponding .csv files 
# Date: September 2020
# Author: Francesca Rosa



plot.global.time.series.CI <- function(csv_path, file_label, plots_path) {
  
  # load the data
  
    data <- read.csv(paste0(csv_path, "/noAF_global_CI", file_label, ".csv"), header = TRUE)
    data <- data %>% mutate(Group = str_replace(Group, "REF", "RCP6.5 (BAU)"))
    
    data <- data %>% rename(Scenario = Group)

    palette_name = "default"
    
  # plot
    
    figure <-
    ggplot(data, aes(x = Year, y = PDF, linetype = Scenario, fill = Scenario)) +
      geom_line(size = 1.3, aes(color = Scenario)) +
      geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha = 0.1)+
      theme_minimal(base_size = 17) +
      labs(x = "Years", y = "PDF%") + 
      xlim(2020, 2100) + 
      ylim(0, 23)
      theme(legend.position = "right", legend.text = element_text(size = 13)) 
   
    figure
    
    ggsave(paste0(plots_path, "noAF_CI_", palette_name, file_label, ".pdf"), width = 20, height = 17, units = "cm")
    
}

