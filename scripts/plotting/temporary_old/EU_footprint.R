setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/")

year = "2100"

data <-read.csv(paste0(save_csv_path, "EUFootprint_", year, case, ".csv"), header = TRUE)
#data <- read.csv(paste0("./plotting/cutoff_timber/EUFootprint_", year, "_mg-det_cutoff_timber.csv"), header = TRUE)

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
select <- dplyr:: select


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
  # , ,
  #ggtitle("Fraction of species loss in 2100 caused by EU forest biomass demand") +
  xlab("Scenarios") + ylab("PDF%") +
  #theme(plot.title = element_text(size = 12, face = "bold.italic")) +
  scale_fill_discrete_sequential(palette = palette_name, c2 = 20) +
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
ggsave(paste0(save_plots_path, "EUFootprint_", year, "_", palette_name, case, ".pdf"), width = 20, height = 16, units = "cm")

# 
# figure <- ggarrange(figure1, figure2, figure3, common.legend = TRUE, legend = "bottom")
# 
# figure
# ggsave(paste0(save_plots_path, "EUFootprint_", year, "_", palette_name, "_all_taxa.pdf"), width = 20, height = 16, units = "cm")

