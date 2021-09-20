
# working directory set with the script set-wd.R available in the main folder

library(tidyverse)
library(dplyr)
library(purrr)

VS_plants_LU <- read.csv("./data/model_parameters/ecoregions_data/VS/VS_plants_LU.csv", header = TRUE, na.strings = c("#VALUE!","#DIV/0!"))

VS_plants <- VS_plants_LU %>%
              rowwise() %>%
                mutate(VS_plants = mean(c(Annual, Permanent, Pasture, Urban, ForExtensive, ForIntensive), na.rm = T)) %>%
                  select(Eco_code, VS_plants) %>%
                    filter(Eco_code != "World average") %>%
                      mutate(VS_plants = VS_plants*0.5)

VS_plants$VS_plants[VS_plants$VS_plants>1] <- 1

write.csv(VS_plants, paste0("./data/model_parameters/ecoregions_data/VS/VS_plants.csv"), row.names = FALSE) 
