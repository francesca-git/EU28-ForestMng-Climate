 #############################################################################################################################################################################

                                                                  # GLOBAL LAND USE IMPACTS - TIME SERIES #

#############################################################################################################################################################################

# General task: plot the time series of the global impacts with CI from the corresponding .csv files 
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
select <- dplyr:: select


plot.global.time.series.CI <- function(csv_path, case_subcase, plots_path) {
  

  # load the data
  
    data <- read.csv(paste0(csv_path, "/noAF_global_CI", case_subcase, ".csv"), header = TRUE)
    data <- data %>% mutate(Group = str_replace(Group, "REF", "RCP6.5 (REF)"))
    
    data <- data %>% rename(Scenario = Group)

    palette_name = "default"
    
  # plot
    
    #jpg(file = "./plotting/noAF_CI.jpg", width = 900, height = 700)
    figure <-
    ggplot(data, aes(x=Year, y=PDFx100, color = Scenario, linetype = Scenario, fill = Scenario)) +
      # scale_fill_scico_d(palette = "batlow", end = 0.2, begin = 0.8) +
      # scale_color_scico_d(palette = "batlow", end = 0.2, begin = 0.8) +
      geom_line(size = 1.3) +
      geom_ribbon(aes(ymin=lower95, ymax=upper95), alpha = 0.1)+
      theme_minimal(base_size = 17) +
      labs(x = "Years", y = "PDF%") + #, title = "Species loss over time due to global land use under two climate mitigation scenarios", colour = "Scenario", fill = "Scenario") +
      # theme(plot.title = element_text(size = 12, face = "bold.italic")) +
      # scale_fill_discrete_sequential(palette = palette_name) +
      # #scale_color_discrete_sequential(palette = palette_name) +
      # scale_fill_grey(start = 0.2, end = 0.8) +
      # scale_color_grey(start = 0.2, end = 0.8) +
      # scale_fill_brewer(palette = "Set2") +
      # scale_color_brewer(palette = "Set2") +
      #theme_light(base_size = 16) +
      ylim(0,15)+ xlim(2020, 2100) + #+
      #geom_smooth(method = "loess") 
      theme(legend.position = "right", legend.text = element_text(size = 13)) 
    # legend.text = element_text(size = 16), legend.title = element_text(size = 20), ,
    #         plot.title = element_text(size = 12, face = "italic"), 
    #         axis.text = element_text(size = 14),
    #         axis.title.x = element_text(size = 16), # face = "bold"),
    #         axis.title.y = element_text(size = 16), axis.t)  # face = "bold"))+
      
    figure
    
    ggsave(paste0(plots_path, "noAF_CI_", palette_name, ".pdf"), width = 23, height = 17, units = "cm")
    
    #dev.off()  
    
    #scale_fill_manual(values = c("#E69F00", "#56B4E9"))+
    #scale_colour_manual("black", "black")+
    #scale_colour_viridis(discrete = T) +
    #scale_fill_viridis(discrete = T) +
    #scale_color_manual(values = c( "#E69F00", "#56B4E9"))+
    #scale_color_manual(values = c("#E7B800", "#2E9FDF"))+
    

}