
setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 

#data<-read.csv("./plotting/charts_examples/Global_development_sel.csv", header = TRUE)
data <- read.csv("./aggregation_plotting/results_plots-csv/cutoff/csv/EUFootprint_time-series_cutoff_EP.csv", header = TRUE)

#dev.new()
library(ggplot2)
library(viridis)
library(colorspace)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(scico)


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
ggsave(paste0("./aggregation_plotting/results_plots-csv/cutoff/plots/EUFootprint_time-series.pdf"), width = 10, height = 8)

