# Title: Quantification of areas displaced by the conversion of natural land, annual crops and grasslands to lignocellulosic energy crops or energy plantations
# Author: Francesca Rosa
# Date: 27/09/2021

# The following calculations are made only for the areas obtained with GLOBIOM marginal approach (mg) and without including timber plantations in the EU28 among the AFMs.

# Steps
# Load the .Rdata file created with tidy_areas.R
# 


# Load the .Rdata file created with tidy_areas.R

# use the script main.R to define the wd

load("./data/land_use_data/areas_processed/areas-to-match.RData")

marginal = TRUE 

if(marginal == TRUE) {approach = "_mg"
          }else if(marginal == FALSE) {approach = "_av"}
      

areas <- data.list.steps[["Import_energy_plant"]][[i]] %>% 
            full_join(data.list.steps[["EU_energy_plant"]][[i]]) %>%
              inner_join(ecoregions_in_Globiom) %>%                         # intersects the ecoregions available in Globiom with the ones in the other land use files  
                filter(Management != "AF0") %>%            
                  unite("Scenario", Climate:Management, remove = TRUE) %>%    # creates one single column merging the information about scenarios (climate, management, forest use)
                    arrange(Scenario, Ecoregion)
                      areas[is.na(areas)] <- 0                          # sets to 0 the cells from the EU datasets which do not have corresponding values in ecoregions outside EU
                        areas$Scenario <- as.factor(areas$Scenario)     # keeps the Scenario column as factor
                          areas$Ecoregion <- as.factor(areas$Ecoregion) # keeps the Ecoregion column as factor    
                        
bioenergy <- data.frame(areas %>% mutate(Import = EP_CrpLnd_im + EP_GrsLnd_im + EP_NatLnd_im, 
                              EU = EP_CrpLnd_EU + EP_GrsLnd_EU + EP_NatLnd_EU) %>%
                select(Scenario, Ecoregion, Import, EU) %>%
                  group_by(Scenario) %>%
                    summarise_if(is.numeric, sum))
                        
bioenergy_disaggr <- data.frame(areas %>% 
                      group_by(Scenario) %>%
                        summarise_if(is.numeric, sum))   

write.csv(bioenergy_disaggr, paste0(aggr_plot_path_areas, "Areas_converted-to-bioenergy_2100_Mha.csv"), row.names = FALSE)
                        