



setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 

library(pacman)
p_load(dplyr, tidyr, abind, tidyverse, stringr)  # dataframe management and string management

p_load(ggplot2, colorspace, scico, viridis, RColorBrewer, gridExtra, nord, ggpubr, sf, sp, rgdal) # plotting

select <- dplyr::select

# Load the data
  data_loaded <- read.csv("./ecoregions_data/z_originals.csv", header = TRUE)
  head(data)

  unique(data_loaded$Organism_group)
#  [1] "zoobenthos"   ""    "birds"        "plants"       "arthropods"   "fish"         "insects"      "nonv plants"  "helminths"    "zooplankton" 
# [11] "mammals"      "fungi"        "redlist spec" "rare species" "vertebrates"  "reptiles"     "molluscs"     "amphibians"  
  
# Clean the data
  data <- data_loaded %>% rename(Realm = ï..Realm) %>%
    filter(Realm == "ter") %>%
      filter(Method != "") %>%
        filter(Method != "both") %>%
          filter(Organism_group != "") %>%
            mutate(Method = str_replace(Method, "ind ", "ind")) %>%
              mutate(Organism_group = str_replace(Organism_group, "nonv plants", "plants"))
  
# Check the distributions
  
  data_ind <- data %>% filter(Method == "ind")
  data_nes <- data %>% filter(Method == "nes")
  data_birds <- data %>% filter(Organism_group == "birds")
  data_mam <- data %>% filter(Organism_group == "mammals")
  data_plants <- data %>% filter(Organism_group == "plants")
  
  mean(pull(data_mam %>% filter(Method == "ind") %>% select(z)))
  # 0.2665217
  
  mean(pull(data_birds %>% filter(Method == "ind") %>% select(z)))
  # 0.2906842
  
  mean(pull(data_plants %>% filter(Method == "ind") %>% select(z)))
  # 0.300354
  
    ggplot(data, aes(x = z, fill = Method)) + 
    geom_histogram(alpha = 0.4, position = 'identity', bins = 40)
  
    data$Organism_group <- factor(data$Organism_group, levels = c("zoobenthos", "molluscs", "redlist spec", "rare species", "vertebrates", 
                                     "arthropods", "fish", "insects", "helminths", "zooplankton", "fungi", 
                                     "reptiles", "amphibians", "plants", "birds", "mammals"))
    
    ggplot(data, aes(x = z, fill = Organism_group)) + 
    geom_histogram(alpha = 0.3, position = 'identity', bins = 30)

    breaks <- seq(-2, 2.3, by = 0.1)
    ggplot(data %>% filter(Organism_group == "plants" | Organism_group == "birds" | Organism_group == "mammals"), 
           aes(x = z, fill = Organism_group, color = Organism_group)) + 
    geom_histogram(alpha = 0.3, position = 'identity', breaks = breaks) +
      facet_wrap(~ Method, ncol = 2)


  
    
    
    