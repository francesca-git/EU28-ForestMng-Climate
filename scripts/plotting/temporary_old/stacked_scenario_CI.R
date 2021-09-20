setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/")

library(ggplot2)
library(RColorBrewer)
require(gridExtra)
library(colorspace)
library(ggpattern)

year = "2100"

data<-read.csv(paste0("./plotting/no_cutoff/data/EUForest_", year, "_mg-det.csv"), header = TRUE)
data_CI<-read.csv(paste0("./plotting/no_cutoff/data/EUForest_", year, "_top_mg_CI.csv"), header = TRUE)
# data<-read.csv(paste0("./plotting/cutoff_timber/EUForest_", year, "_mg-det_cutoff_timber.csv"), header = TRUE)
# data_CI<-read.csv(paste0("./plotting/cutoff_timber/EUForest_", year, "_top_mg_cutoff_timber.csv"), header = TRUE)
 
palette_name = "Greens" # "#238B45","#74C476", "#BAE4B3", "#EDF8E9"
#pal = c("#004616", "#238B45","#74C476", "#BAE4B3", "#EDF8E9")

#to keep the same order between the scenarios
#data_CI$Scenario <- factor(marginal_CI$Scenario, levels=unique(marginal_CI$Scenario))
data$Scenario <- factor(data$Scenario, levels=unique(data$Scenario))
# rename the scenarios
label_scenario <- c("no AFM", "AFM-free", "AFM 25%", "AFM 50%", "AFM 75%", "AFM 100%")  
# rename the groups
data <- data %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                        Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
data_CI <- data_CI %>% mutate(Group = str_replace(Group, "REF_MFM", "RCP6.5 (REF) - Multifunctional"), Group = str_replace(Group, "REF_SFM", "RCP6.5 (REF) - Set-aside"),
                        Group = str_replace(Group, "RCP2.6_MFM", "RCP2.6 - Multifunctional"), Group = str_replace(Group, "RCP2.6_SFM", "RCP2.6 - Set-aside"))
# set the order of the groups
data$Group <- factor(data$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside"))   
data_CI$Group <- factor(data_CI$Group, levels = c("RCP6.5 (REF) - Multifunctional", "RCP6.5 (REF) - Set-aside", "RCP2.6 - Multifunctional", "RCP2.6 - Set-aside"))   
data$Category <- factor(data$Category, levels = c("Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Clear cut", "Other management",  "Retention", "Selection"))
#data$Category <- factor(data$Category, levels = c("Timber", "Clear_cut", "Other_management", "Retention", "Selection"), labels = c("Timber", "Clear cut", "Other management",  "Retention", "Selection"))

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
  ylim(-0.02, 0.04) + 
  geom_point(data = data_CI, aes(x  = Scenario, y = PDFx100), color = "black", size = 1) +
  theme(panel.border = element_rect(color="grey", fill=NA)) +
  # geom_errorbar(data = marginal_CI, aes(x = Scenario, y = PDFx100, ymin = lower95, ymax = upper95), width = 0.1, color = "gray60", size = 0.8) +
  facet_wrap(~ Group) +
  labs(fill = "Forest use type") 

figure

ggsave(paste0("./plotting/no_cutoff/EUForest_", year, "_", palette_name, "_new.pdf"), width = 20, height = 16, units = "cm")
