
# working directory set with the script set-wd.R available in the main folder

library(tidyverse)
library(dplyr)
library(purrr)


glo_reg_splost <- read.csv("./input_data/VS/Global_Regional_species-lost_LCImpact.csv", header = TRUE, na.strings = c("#VALUE!","#DIV/0!"))

data_longer <- data.frame(data.frame(glo_reg_splost %>% pivot_longer(cols = !contains("Ecoregion"), names_to = "Category", values_to = "Values")) %>%
                separate(Category, c("Scale", "Taxon", "Land_use"), sep = "_") %>%
                  #unite("Category", Taxon:Land_use, sep = "_") %>%
                    pivot_wider(names_from = Scale, values_from = Values))

data_ratio <- data.frame(data_longer %>% group_by(Taxon, Ecoregion) %>%
                summarise(Global = mean(Global, na.rm = TRUE), Regional = mean(Regional,na.rm = TRUE)) %>%
                  mutate(Ratio = Global/Regional))

data_ratio[sapply(data_ratio, is.infinite)] <- NaN

VS <- data.frame(data_ratio %>% select(-Global, -Regional) %>%
        pivot_wider(names_from = Taxon, values_from = Ratio)) %>%
          mutate(Plants = 0.5*Plants) %>%
            rename(VS_plants = Plants, VS_birds = Birds, VS_mammals = Mammals)

write.csv(VS, paste0("./input_data/VS/VS_plats-mamm-birds.csv"), row.names = FALSE)
