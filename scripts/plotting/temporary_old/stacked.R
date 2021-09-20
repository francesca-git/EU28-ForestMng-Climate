setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 
# 
# year = "2100"
# 
# marginal_2100<-read.csv(paste0("./plotting/data/global_", year, "_areas.csv"), header = TRUE)
# library(ggplot2)
# library(RColorBrewer)
# 
# #to keep the same order between the scenarios
# marginal_2100$Scenario <- factor(marginal_2100$Scenario, levels=unique(marginal_2100$Scenario))
# #marginal_2100$Category <- factor(marginal_2100$Category, levels=unique(marginal_2100$Category))
# 
# 
# figure <-
#   ggplot(marginal_2100, aes(x = Scenario, y = PDFx100, fill = Category))+
#   geom_bar(stat='identity')+
#   theme(axis.title.x = element_blank(),axis.title.y = element_blank())+
#   theme(legend.position="bottom") + theme(legend.title=element_blank())+
#   scale_fill_brewer(palette = "RdYlBu")+
#   #scale_fill_manual(values=c("brown1","orange","darkkhaki","darkgreen","cornflowerblue","darkorchid","deeppink")) +
#   facet_wrap(~ Group) #+
#   #ylim(0, 0.2)
# 
# library(ggpubr)
# annotate_figure(figure,
#                 left = text_grob("Areas (Mha)", color = "black", rot = 90, size= 14,  hjust = 0, x = 0.5,),
# )
# 

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
require(gridExtra)
library(viridis)
library(ggsci)

year = c("2050", "2100")
palette = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FEE090", "#FDAE61", "#F46D43","#D73027")
  
# disaggregated

marginal_2050 <- read.csv(paste0("./plotting/no_cutoff/data/global_", year[1], "_areas.csv"), header = TRUE)
marginal_2100 <- read.csv(paste0("./plotting/no_cutoff/data/global_", year[2], "_areas.csv"), header = TRUE)
marginal_2050$Year <- year[1]
marginal_2100$Year <- year[2]
marginal <- marginal_2050 %>% bind_rows(marginal_2100)

#to keep the same order between the scenarios and categories
marginal$Scenario <- factor(marginal$Scenario, levels=unique(marginal$Scenario))
marginal$Category <- factor(marginal$Category, levels=unique(marginal$Category))

marginal <- marginal %>% 
  separate(Group, into = c("Group", "Forest_use"), sep = "_") %>%
    filter(Scenario == "noAFM", Forest_use == "MFM")

#jpeg(file = "./plotting/global_CI.jpg", width = 900, height = 700, quality = 100)

figure <-
  ggplot(marginal)+
  geom_bar(aes(x = Group, y = PDFx100, fill = Category), stat = "identity", position = position_stack(reverse = FALSE)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_fill_manual(values = palette) + 
  #scale_fill_viridis(discrete = T)+
  #scale_fill_brewer(palette = "RdYlBu", direction = -1)+
  #ylim(0, 15) +
  theme(legend.position="bottom") + theme(legend.title=element_blank())+
  facet_wrap(~ Year) 


library(ggpubr)

annotate_figure(figure,
                left = text_grob("Areas (Mha)", color = "black", rot = 90, size= 14,  hjust = 0, x = 0.5,),
)

#dev.off()
