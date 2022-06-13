

#############################################################################################################################################################################

# WARNING: The functions defined in this file are called in the file main.R. The loading/saving paths, the libraries and the working directory are defined in that file.

#############################################################################################################################################################################


# General task: calculate the volume of wood harvested from each category of forest management.
# Date: June 2022
# Author: Francesca Rosa


library(rgdal)
library(dplyr)
library(tidyr)
library(compare)
library(readr)
library(purrr)
library(tibble)
library(stringr)

####### Wood and energy crops/plantation production from IIASA

calculate.volume.per.category <- function(csv_path, file_label, areas_base_path) {
    
    EUForest <- read.csv(paste0(areas_base_path, "EUWood_forest.csv"), header = TRUE, check.names = FALSE) # total wood harvested from EU 28 forests (roundwood and energywood >including  fuelwood and logging residues)
    EUEnergy <- read.csv(paste0(areas_base_path, "EUWood_energycrops.csv"), header = TRUE, check.names = FALSE) # energy-wood form energy crops cultivated in the EU
    importForest <- read.csv(paste0(areas_base_path, "ImportWood_forest.csv"), header = TRUE, check.names = FALSE) # total wood imported from forests outside the EU28 (in form of roundwood, semifinished products and wood pellets for energy from industrial byproducts)
    importEnergy <- read.csv(paste0(areas_base_path, "ImportWood_energyplantations.csv"), header = TRUE, check.names = FALSE) # energywood imported exclusively from energy plantations cultivated outside the EU (IP_pellets)
    exportForest <- read.csv(paste0(areas_base_path, "ExportWood_forest.csv"), header = TRUE, check.names = FALSE) # wood harvested in the EU28 and exported to other regions (in form of roundwood or semi-finished products) [wood already included in the “EU28_wood_forest”]
    importByproducts <- read.csv(paste0(areas_base_path, "ImportWood_byproducts.csv"), header = TRUE, check.names = FALSE) # energy wood imported from outside EU in form of pellets obtained as byproducts of the forest industry 
    
    # list containing all the loaded dataframes 
    
    data.list = list("EUForest" = EUForest, "EUEnergy" = EUEnergy, "importForest" = importForest, "importEnergy" = importEnergy, "exportForest" = exportForest, "importByproducts" = importByproducts)
    
    tstep = seq(from = 2000, to = 2100, by = 10)  
    ntstep = length(tstep) #time steps: 2000 -> 2100
    
    # for each element of the list (each dataframe), create a single column (Year) containing all the time values and one column with the Mm3 
      
    list.temp <- lapply(data.list, function(x) pivot_longer(x, cols = 5:(length(x)-1), names_to = "Year", values_to = toString(x[1, length(x)])) %>%
                                                  select(-Category))
    # puts together all the dataframes
    
    df <- reduce(list.temp, full_join)
    
    rm(data.list, list.temp)
    
    df[is.na(df)] <- 0 
    
    # tidies the dataframe (renames some cells, unifies some columns, arranges the df) such that it can be joined to the df with the species loss
    
    df1 <- data.frame(df %>% mutate(Wood_forest_EU = Wood_forest_EU - Wood_forest_ex) %>%                                                             # exports are included in the general EU wood production, this difference keeps them separate 
                  rename(Group = Mitigation_scenario, Management = Management_scenario) %>%
                    mutate(Region = str_replace(Region, "Pacific_Islands", "Pacific_Island")) %>%                                                              
                      unite("Scenario", Group:Management, sep = "_") %>%
                         arrange(Region, Scenario, Year))
    levels(df1$Region) <- sort(levels(df1$Region))
    
    # # loads the PDF dataframe and tidies it
    # 
    # df2 <- read.csv(paste0(csv_path, "slost-globiom", file_label, ".csv"), header = TRUE, check.names = FALSE) # total wood harvested from EU 28 forests (roundwood and energywood >including  fuelwood and logging residues)
    # 
    # df2 <- df2 %>% rename(Region = Globiom_Reg) %>%
    #                 filter(Region != "NN") %>%
    #                   arrange(Region, Scenario, Year) %>%
    #                     mutate(Year = as.character(Year))
    # 
    # df2 = droplevels( df2, "NN")  
    # 
    # # puts together the production and the impacts 
    # # focus on EU
    # 
    # df_fin <- df2 %>% full_join(df1) %>%
    #                     select(Scenario, Region, Year, contains("Wood"), contains("median") & ((starts_with("For_")&(contains("EU")|contains("ex")|contains("im")))|  # selects the columns containing the production of Wood and energy crops/plantations and the columns containing EU internal/imported/exported forest use or energy crops/plantations
    #                                                                                           (starts_with("EP")&((contains("EU")&contains("conv"))|contains("im"))))) %>% 
    #                       mutate(PDF_forest_EU = rowSums(select(., (starts_with("For_") & contains("EU")))), 
    #                              PDF_forest_im = rowSums(select(., (starts_with("For_") & contains("im")))),  
    #                              PDF_forest_ex = rowSums(select(., (starts_with("For_") & contains("ex")))),
    #                              PDF_EP_EU = rowSums(select(., (contains("EP") & contains("EU") & contains("conv")))), 
    #                              PDF_EP_im = rowSums(select(., (contains("EP") & contains("im") & contains("conv"))))) %>%
    #                           select(-(starts_with("For_") & contains("EU")), - (starts_with("For_") & contains("im")), 
    #                              -(contains("EP") & contains("EU") & contains("conv")), -(contains("EP") & contains("im") & contains("conv")), -(starts_with("For_") & contains("ex")))
    # 
    
    # test ====
    
    # r <- sample(1:nrow(df_fin), 1)        # random row
    # test = df_fin[r,]
    # test_check = df2[r,]
    # 
    # if (((test$PDF_EP_im - test_check$EP_conv_im_median) > 1e-14) |
    #     ((test$PDF_EP_EU - test_check$EP_conv_EU_median) > 1e-14) |
    #     ((test$PDF_forest_EU - (test_check$For_ClearCut_EU_median + test_check$For_Retention_EU_median + test_check$For_SelectionSystem_EU_median 
    #                             + test_check$For_TimberPlant_EU_median)) > 1e-14) |
    #     ((test$PDF_forest_ex - test_check$For_ClearCut_ex_median - test_check$For_TimberPlant_ex_median) > 1e-14) |
    #     ((test$PDF_forest_im - (test_check$For_ClearCut_im_median + test_check$For_Plantation_im_median)) > 1e-14)) {stop("ERROR in the aggregation of land use for EU footprint")}
    # 
    # rm(test, test_check)
    # # ====
    # 
    df_aggr <- df1 %>% select(-Region) %>%
      group_by(Scenario, Year) %>%
                summarise_all(sum) %>%
                  filter(Year == "2100") %>%
                    select(-Year) %>%
                      pivot_longer(cols = starts_with("Wood"), names_to = "Category", values_to = "Values")
    
    write.csv(df_aggr, paste0(csv_path, "Mm3_category", file_label, ".csv"), row.names = FALSE)

}







