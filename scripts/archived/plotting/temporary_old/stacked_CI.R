setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
require(gridExtra)

year = c("2050", "2100")
palette = c("#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FEE090", "#FDAE61", "#F46D43","#D73027")
# when areas are plotted, this color code must be added in the first position: "#313695"


# disaggregated

marginal_2050 <- read.csv(paste0("./plotting/data/global_disaggr_", year[1], "_mg-det.csv"), header = TRUE)
marginal_2100 <- read.csv(paste0("./plotting/data/global_disaggr_", year[2], "_mg-det.csv"), header = TRUE)
marginal_2050$Year <- year[1]
marginal_2100$Year <- year[2]
marginal <- marginal_2050 %>% bind_rows(marginal_2100)

# top and CI

marginal_2050_CI <-read.csv(paste0("./plotting/data/global_", year[1], "_top_mg_CI.csv"), header = TRUE)
marginal_2100_CI <-read.csv(paste0("./plotting/data/global_", year[2], "_top_mg_CI.csv"), header = TRUE)
marginal_2050_CI$Year <- year[1]
marginal_2100_CI$Year <- year[2]

marginal_CI <- marginal_2050_CI %>% bind_rows(marginal_2100_CI)

#to keep the same order between the scenarios and categories
marginal$Scenario <- factor(marginal$Scenario, levels=unique(marginal$Scenario))
marginal$Category <- factor(marginal$Category, levels=unique(marginal$Category))

marginal <- marginal %>% 
              separate(Group, into = c("Group", "Forest_use"), sep = "_") %>%
                filter(Scenario == "noAFM", Forest_use == "MFM")

#to keep the same order between the scenarios
marginal_CI$Scenario <- factor(marginal_CI$Scenario, levels=unique(marginal_CI$Scenario))

marginal_CI <- marginal_CI %>% 
  separate(Group, into = c("Group", "Forest_use"), sep = "_") %>%
    filter(Scenario == "noAFM", Forest_use == "MFM")




#jpeg(file = "./plotting/global_CI.jpg", width = 900, height = 700, quality = 100)

figure <-
  ggplot(marginal)+
  geom_bar(aes(x = Group, y = PDFx100, fill = Category), stat = "identity", position = position_stack(reverse = FALSE)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_fill_manual(values = palette)+
  #scale_fill_brewer(palette = "RdYlBu", direction = -1)+
  ylim(0, 15) +
  theme(legend.position="bottom") + theme(legend.title=element_blank())+
  geom_point(data = marginal_CI, aes(x  = Group, y = PDFx100), color = "grey73", size = 2) +
  geom_errorbar(data = marginal_CI, aes(x = Group, y = PDFx100, ymin = lower95, ymax = upper95), width = 0.1, color = "gray73", size = 0.8) +
  facet_wrap(~ Year) 


library(ggpubr)

annotate_figure(figure,
                left = text_grob("PDF%", color = "black", rot = 90, size= 14,  hjust = 0, x = 0.5,),
)

#dev.off()

#to add error bars on the side
# 
# plot2 <- 
#   ggplot(marginal_CI, aes(x = Group, y = PDFx100, group = Group, color = Group)) +
#   theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.y = element_blank()) + #axis.text.x = element_blank()), )+
#   #theme_classic() +
#   geom_point(position = position_dodge2(width = 0.9), aes(y = PDFx100), color = "dimgrey", size = 2) +
#   geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.1, color = "dimgrey", position = position_dodge2(width = 0.9, padding = 0.5), size = 0.8) +
#   #scale_color_brewer(palette = "RdYlBu") +
#   expand_limits(y = c(0, 15)) +
#   facet_wrap(~ Year)# +
#   #theme_transparent()