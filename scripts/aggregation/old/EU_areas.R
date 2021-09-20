
#############################################################################################################################################################################

                                                                  # AREAS CONNECTED TO EU LAND USE OR EU FOOTPRINT #

#############################################################################################################################################################################

# General task: convert the outputs of the model (.Rdata) into .csv files grouping and selecting only the most relevant 
# Date: September 2020
# Author: Francesca Rosa


#############################################################################################################################################################################

# WARNING: run the file load_and_save.R before running this one. load_and_save.R allows you to choose the .Rdata file to load and the location where the .csv files are saved

#############################################################################################################################################################################


setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

# file loaded (.Rdata) using load_and_save.R: results from the script: areas_prepare_for_plotting.R
# Content:
    # data: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                     
library(dplyr, mask.ok = c("select"))     # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
select <- dplyr::select



create.csv.EU.areas <- function(rdata_path_areas, csv_path_areas, case_areas) {

  # Task: Produce four .csv that contain the areas per land/forest use group or category 
        # for a given year selected here below. 
        # (grouping criteria are defined according to the scope of the subsequent analysis, either the whole EU, EU forest biomass footprint or EU internal forests) 


    load(rdata_path_areas)
    
      
    results <- lapply(results, function(x) mutate(x, Regrowth_EU = 0))
    
    
    # focus on 2050 and 2100
    t = c("2050", "2100") 
    year  = t[2]              # select the year
    
    # focus on EU
    
      ################## EU #######################
    
        results_temp <- results[[year]] %>%
          unite("Group", Group:Forest_use, remove = TRUE) %>%
          rename(Scenario = Management) %>% 
          transmute(Group = Group, Scenario = Scenario, 
                    Afforestation = rowSums(dplyr::select(., contains("Afforested") & contains("EU"))),
                    Regrowth = rowSums(dplyr::select(., contains("Regrowth") & contains("EU"))),
                    Annual_crops = Annual_EU, Energy_crops = rowSums(dplyr::select(., contains("EP") & contains("EU"))), 
                    Forest_net = rowSums(dplyr::select(., (starts_with("For_") & contains("EU")))), 
                    Permanent_crops = Permanent_EU, Pasture = Pasture_EU, Urban = Urban_EU)
        
        # change the format (before: one column per each land use class; after the change: one single column with al the values and one with the corresponding Category)
        results_oneyear <- results_temp %>% 
          pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "Values")
        
        write.csv(results_oneyear, paste0(csv_path_areas, "areas_EUlanduse_", year, "_", case_areas, ".csv"), row.names = FALSE)
        # this csv has the following columns: 
        # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), Values (areas)
          
        
    
      ################## EU FOOTPRINT #######################
        
        ######## SUMS OF GROUPED LAND USE CLASSES ########
        
        # group the land use classes in: EU28_Energy_crops, EU_Forest_net, Import_Energy_plantations, Import_Forest
      
          results_temp <- results[[year]] %>%
                            unite("Group", Group:Forest_use, remove = TRUE) %>%
                              rename(Scenario = Management) %>% 
                                transmute(Group = Group, Scenario = Scenario, 
                                          EU_Forest_net = rowSums(select(., (starts_with("For") & contains("EU")))), 
                                          EU_Energy_crops = rowSums(select(., contains("EP") & contains("EU"))) ,
                                          Import_Energy_plantations = rowSums(select(., contains("EP") & contains("im"))), 
                                          Import_Forest = rowSums(select(., starts_with("For_") & contains("im"))))
          
          # change the format (before: one column per each land use class; after the change: one single column with al the values and one with the corresponding Category)
            results_oneyear <- results_temp %>% 
              pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "Values")
          
          write.csv(results_oneyear, paste0(csv_path_areas, "areas_EUFootprint_", year, "_" , case_areas, ".csv"), row.names = FALSE)
          # this csv has the following columns: 
          # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), Values (areas)
                         
                          # test ====
                          
                          test = results_temp[nrow(results_temp),]
                          test_check = results[[year]][nrow(results_temp),]
                          
                          if ((test$EU_Energy_crops != test_check$EP_EU + test_check$EP_conv_EU) |
                              (test$EU_Forest_net != test_check$For_ClearCut_EU + test_check$For_Retention_EU + test_check$For_SelectionSystem_EU + test_check$ForOther_Extensive_EU + test_check$ForOther_Intensive_EU) |
                              (test$Import_Energy_plantations != test_check$EP_conv_im) |
                              (test$Import_Forest != test_check$For_ClearCut_im + test_check$For_PlantationFuel_im)) {stop("ERROR in the aggregation of land use for EU footprint")}
                            
                          # ====
                          
          
        
      ################## EU INTERNAL #######################
          
        
        ######## SUMS OF GROUPED LAND USE CLASSES ########
        # group the land use classes in: Clear_cut, Retention, Selection, Other_management
    
          results_temp <- results[[year]] %>%
                            unite("Group", Group:Forest_use, remove = TRUE) %>%
                              rename(Scenario = Management) %>% 
                                transmute(Group = Group, Scenario = Scenario, 
                                          Clear_cut = rowSums(select(., contains("ClearCut") & (contains("EU")|contains("ex")))),
                                          Retention = rowSums(select(., contains("Retention") & (contains("EU")|contains("ex")))),
                                          Selection = rowSums(select(., contains("Selection") & (contains("EU")|contains("ex")))),
                                          Other_management = rowSums(select(., contains("ForOther") & contains("EU"))))
          
          results_sum <- results_temp %>%
                          transmute(Group = Group, Scenario = Scenario, Sum = rowSums(results_temp[,3:length(results_temp)]))
            
            results_oneyear <- results_temp %>% 
            pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "Values")
        
            results_points <- results_sum %>%
            pivot_longer(cols = 3:(length(results_sum)), names_to = "Category", values_to = "Values")
            
          
          write.csv(results_oneyear, paste0(csv_path_areas, "areas_EUForest_", year, "_" , case_areas, ".csv"), row.names = FALSE)
          write.csv(results_points, paste0(csv_path_areas, "areas_EUForest_", year, "_" , case_areas, "_points.csv"), row.names = FALSE)
          # these csv have the following columns: 
          # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), Values (areas)
             
                          # test ====
                          
                          test = results_temp[nrow(results_temp),]
                          test_check = results[[year]][nrow(results_temp),]
                          test_sum = results_sum[nrow(results_temp),]
                          
                          if ((test$Clear_cut != test_check$For_ClearCut_EU + test_check$For_ClearCut_ex) |
                              (test$Retention != test_check$For_Retention_EU) |
                              (test$Selection != test_check$For_SelectionSystem_EU) |
                              (test$Other_management != test_check$ForOther_Extensive_EU + test_check$ForOther_Intensive_EU) |
                              (test_sum$Sum != test$Clear_cut + test$Retention + test$Selection + test$Other_management)) {stop("ERROR in the aggregation of land use for EU footprint")}
                          
                          # ====
          
      }
    
