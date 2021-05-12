
###### LOAD, EXPLORE AND PREPARE THE Z VALUES  ###### 

# April 2021
# Author: Francesca Rosa

setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 

library(pacman)
p_load(dplyr, tidyr, abind, tidyverse, stringr)  # dataframe management and string management

p_load(ggplot2, colorspace, scico, viridis, RColorBrewer, gridExtra, nord, ggpubr, sf, sp, rgdal) # plotting

select <- dplyr::select

#### Load the data and check what's in there ####
  data_loaded <- read.csv("./ecoregions_data/z_analysis/z_SAR_data_full.csv", header = TRUE)
  head(data_loaded)

  unique(data_loaded$group)
    # "zoobenthos"   ""    "birds"        "plants"       "arthropods"   "fish"         "insects"      "nonv plants"  "helminths"    "zooplankton" 
    # "mammals"      "fungi"        "redlist spec" "rare species" "vertebrates"  "reptiles"     "molluscs"     "amphibians"  
  unique(data_loaded$system)
    # "terrestrial" "water"   
  unique(data_loaded$habitat)
    # "islands"    "marine"     "non-forest" "streams"    "forest"     ""           "lakes"  
  unique(data_loaded$method)
    # "independent"  ""             "nested"       "both"         "independent " "experiment" 
  nrow(data_loaded %>% filter(method == "")) 
    # 61
  nrow(data_loaded %>% filter(method == "both")) 
    # 11
  nrow(data_loaded %>% filter(method == "experiment")) 
    # 7
  
#### Clean the data ####
  # remove values below 0 and above 1
  data <- data_loaded %>% 
    rename(z = Slope.z.loglog.) %>%
      filter(system == "terrestrial") %>%
        filter(habitat == "forest" | habitat == "non-forest" | habitat == "islands") %>%
          filter(method != "experiment") %>%
            # filter(method != "both") %>%
              filter(z >= 0 & z <= 1) %>%
                mutate(method = str_replace(method, "independent ", "independent")) %>%
                  mutate(method = str_replace(method, "independent", "ind")) %>%
                    mutate(method = str_replace(method, "nested", "nes")) %>%
                      mutate(Organism_group = str_replace(group, "nonv plants", "plants")) %>%
                        select(habitat, group, method, z)
  
  data$group <- factor(data$group, levels = c("zoobenthos", "molluscs", "redlist spec", "rare species", "vertebrates", 
                                     "arthropods", "fish", "insects", "helminths", "zooplankton", "fungi", 
                                     "reptiles", "amphibians", "plants", "birds", "mammals"))
    
#### Check the distributions ####
     
  data_ind <- data %>% filter(method == "ind")
  data_nes <- data %>% filter(method == "nes")
  data_birds <- data %>% filter(group == "birds")
  data_mam <- data %>% filter(group == "mammals")
  data_plants <- data %>% filter(group == "plants")
  
  data_mbp <- data %>% filter(group == "mammals" | group == "birds" | group == "plants")
  
  data_an_p <- data_mbp %>% filter(group == "mammals" | group == "birds" | group == "plants") %>%
        mutate(group = str_replace(group, "mammals", "animal_taxa")) %>%
          mutate(group = str_replace(group, "birds", "animal_taxa"))
  
  mean_habitat_group <- data.frame(data_mbp %>% group_by(group, habitat) %>%
    summarise_if(is.numeric, mean)) %>%
      rename(mean_z = z)
  median_habitat_group <- data.frame(data_mbp %>% group_by(group, habitat) %>%
    summarise_if(is.numeric, median)) %>%
      rename(median_z = z)
  mean_median_hab_group <- mean_habitat_group %>% full_join(median_habitat_group)
  rm(mean_habitat_group, median_habitat_group)
  mean_median_hab_group
  
  mean_habitat <- data.frame(data_mbp %>% group_by(habitat) %>%
    summarise_if(is.numeric, mean))%>%
      rename(mean_z = z) 
  median_habitat <- data.frame(data_mbp %>% group_by(habitat) %>%
    summarise_if(is.numeric, median)) %>%
      rename(median_z = z)
  mean_median_hab <- mean_habitat %>% full_join(median_habitat)
  rm(mean_habitat, median_habitat)
  mean_median_hab
  
  
#### Plot and save the distributions ####
  
  ggplot(data = data_mbp, aes(y = z, x = habitat)) +
    #geom_violin(aes(fill = group), alpha = 0.2) +
      geom_boxplot(aes(fill = group), width = 0.2) +
        geom_jitter(width = 0.05, alpha = 0.3) +
          facet_wrap(~group, ncol = 3) +
            theme(legend.position = "none")
  ggsave(filename = "./ecoregions_data/z_analysis/boxplots_habitat_group.png")

  ggplot(data = data_mbp, aes(y = z, x = habitat)) +
    #geom_violin(aes(fill = group), alpha = 0.2) +
      geom_boxplot(width = 0.2) +
        geom_jitter(width = 0.05, alpha = 0.2) +
            theme(legend.position = "none")
  ggsave(filename = "./ecoregions_data/z_analysis/boxplots_habitat.png")
  
  ggplot(data = data_mbp %>% filter(method == "ind" | method == "nes"), aes(y = z, x = habitat)) +
    #geom_violin(aes(fill = group), alpha = 0.2) +
      geom_boxplot(aes(fill = group), width = 0.2) +
        geom_jitter(width = 0.05, alpha = 0.3) +
          facet_wrap(method ~ group, ncol = 3) +
            theme(legend.position = "none")
  ggsave(filename = "./ecoregions_data/z_analysis/boxplots_habitat_method_group.png")
  
  ggplot(data = data_an_p, aes(y = z, x = habitat)) +
    #geom_violin(aes(fill = group), alpha = 0.2) +
      geom_boxplot(aes(fill = group), width = 0.2) +
        geom_jitter(width = 0.05, alpha = 0.3) +
          facet_wrap(~ group, ncol = 3) +
            theme(legend.position = "none")
  ggsave(filename = "./ecoregions_data/z_analysis/boxplots_habitat_an_pl.png")
    
  ggplot(data, aes(x = z, fill = group)) + 
    geom_histogram(alpha = 0.3, position = 'identity', bins = 30)
    
  ggplot(data_an_p, aes(x = z)) + 
    geom_histogram(alpha = 0.3, position = 'identity', bins = 30) +
      geom_vline(data = mean_median_hab, aes(xintercept = mean_z), size = 0.5, linetype="dashed")+
        geom_vline(data = mean_median_hab, aes(xintercept = median_z), size = 0.5) +
          facet_wrap(~ habitat, ncol = 3) +
            theme(legend.position = "right")
  ggsave(filename = "./ecoregions_data/z_analysis/histograms_habitats.png")

  breaks <- seq(0, 1, by = 0.1)
  ggplot(data %>% filter(group == "plants" | group == "birds" | group == "mammals"), 
         aes(x = z, fill = group, color = group)) + 
  geom_histogram(alpha = 0.3, position = 'identity', breaks = breaks) +
    facet_wrap(~ habitat, ncol = 3)
  ggsave(filename = "./ecoregions_data/z_analysis/histograms_habitat_group.png")

    
    
#### Save the .csv ####
    
    write.csv(data_an_p, file = "./ecoregions_data/z_analysis/z_raw_values.csv", row.names = FALSE)
    

  
    
    
    