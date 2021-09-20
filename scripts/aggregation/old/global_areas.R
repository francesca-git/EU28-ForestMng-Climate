

#############################################################################################################################################################################

                                                                  # GLOBAL LAND USE AREAS #

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



create.csv.global.areas <- function(rdata_path_areas, csv_path_areas, case_areas) {

load(rdata_path_areas)

# Focus on 2050 and 2100
t = c("2050", "2100")
year  = t[2]            # select the year


##################################################### IMPACTS FOR A GIVEN YEAR (GROUPING OF LAND USE CATEGORIES) ##################################################### 

# Task: Produce a .csv that contain the areas per land use group or category 
        # for a given year selected here below. 
        # (grouping criteria are defined according to the scope of the subsequent analysis, either the whole EU, EU forest biomass footprint or EU internal forests) 

    ######## SUMS OF GROUPED LAND USE CLASSES ########

  # group the land use classes in: Afforestation, Annual_crops, Energy_crops_and_plantations, Forest, Pasture, Permanent_crops, Urban
    results_temp <- results[[year]] %>%
              unite("Group", Group:Forest_use, remove = TRUE) %>%
                rename(Scenario = Management) %>% 
                  transmute(Group = Group, Scenario = Scenario, Natural = rowSums(select(., contains("new"))), Afforestation = rowSums(select(., contains("Afforested"))), 
                            Regrowth = rowSums(select(., contains("Regrowth"))), 
                            Forest = rowSums(select(., starts_with("For"))), 
                            Energy_crops_and_plantations = rowSums(select(., contains("EP"))), 
                            Annual_crops = rowSums(select(., contains("Annual"))),
                            Pasture = rowSums(select(., contains("Pasture"))), Permanent_crops = rowSums(select(., contains("Permanent"))),
                            Urban = rowSums(select(., contains("Urban"))))
  # change the format (before: one column per each land use class; after the change: one single column with al the values and one with the corresponding Category)
      results_oneyear <- results_temp %>% 
                        pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "Values")
    
    write.csv(results_oneyear, paste0(csv_path_areas, "areas_global_", year, ".csv"), row.names = FALSE)
    # this csv has the following columns: 
    # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), Values (areas)
                  
                  # test ====
                  
                  test = results_temp[nrow(results_temp),]
                  test_check = results[[year]][nrow(results_temp),]
                  
                      if ((test$Afforestation != test_check$Afforested_EU + test_check$Afforested_RoW) |
                            (test$Regrowth != test_check$Regrowth) |
                            (test$Annual_crops != test_check$Annual_EU + test_check$Annual_RoW) |
                            (test$Energy_crops_and_plantations != test_check$EP_EU + test_check$EP_conv_EU + test_check$EP_RoW + test_check$EP_conv_im) |
                            (abs(test$Forest - (test_check$For_ClearCut_EU +  test_check$For_ClearCut_im + test_check$For_ClearCut_ex + 
                              test_check$For_Retention_EU + test_check$For_SelectionSystem_EU + test_check$For_PlantationFuel_im + test_check$ForOther_Extensive_EU + 
                              test_check$ForOther_Extensive_RoW + test_check$ForOther_Intensive_EU + test_check$ForOther_Intensive_RoW)) > 1e-11) |
                            (test$Pasture != test_check$Pasture_EU + test_check$Pasture_RoW) |
                            (test$Permanent_crops != test_check$Permanent_EU + test_check$Permanent_RoW) |
                            (test$Urban != test_check$Urban_EU + test_check$Urban_RoW)) 
                        {stop("ERROR in the aggregation of land use at global level")}
                  
                  rm(test, test_check)
                  # ====

    rm(results_temp, results_oneyear)


##################################################### TIME SERIES OF IMPACTS ##################################################### 
  
 # TIME SERIES 
                    
  # Task: Produce one .csv that contains the areas (Mha) per scenario, per land use group and per year 
          # (areas of land use categories of the same group are summed, e.g. the areas of all forest land use categories are summed together). 
        
    #for areas:
    #results_median <- results
    
    # Create a df containing all the sums for all the years
    #df = data.frame(matrix(NA, nrow=(nrow(sums[[1]])*length(sums)), ncol=(ncol(sums[[1]])+1)))
    #df = data.frame(matrix(NA, nrow=(nrow(sums[[1]])*length(sums)), ncol=(ncol(sums[[1]])+1)))
    
    df <- results[[1]][1,] %>%
            #unite("Group", Group:Forest_use, remove = TRUE) %>%
              rename(Scenario = Management) %>%
                  select(Group, Scenario) 
    
    for(i in 1:length(tstep)) {
      
      results[[i]]$Year <- toString(tstep[i])
    
      results_temp <- results[[i]] %>%
                        #unite("Group", Group:Forest_use, remove = TRUE) %>%
                          rename(Scenario = Management) %>% 
                            filter(Forest_use == "MFM") %>%
                              select(-Forest_use) %>%
                               transmute(Group = Group, Scenario = Scenario, Year = Year, 
                                        Natural = rowSums(select(., contains("new"))), # for areas
                                        Afforestation = rowSums(select(., contains("Afforested"))), 
                                        Regrowth = rowSums(select(., contains("Regrowth"))), 
                                        Forest = rowSums(select(., starts_with("For"))), 
                                        Energy_crops_and_plantations = rowSums(select(., contains("EP"))), 
                                        Annual_crops = rowSums(select(., contains("Annual"))),
                                        Pasture = rowSums(select(., contains("Pasture"))), Permanent_crops = rowSums(select(., contains("Permanent"))),
                                        Urban = rowSums(select(., contains("Urban"))))
        
      df = df %>% bind_rows(results_temp)                     # bind columns together
      
      # test ====
              
                   test = results_temp %>% filter(Group == "RCP2.6" & Scenario == "noAFM")
                   test_check = results[[i]] %>% filter(Forest_use == "MFM" & Group == "RCP2.6" & Management == "noAFM") 

                       if ((test$Afforestation != test_check$Afforested_EU + test_check$Afforested_RoW) |
                             (test$Regrowth != test_check$Regrowth) |
                             (test$Annual_crops != test_check$Annual_EU + test_check$Annual_RoW) |
                             (abs(test$Energy_crops_and_plantations - (test_check$EP_EU + test_check$EP_conv_EU + test_check$EP_RoW + test_check$EP_conv_im)) > 1e-11) |
                             (abs(test$Forest - (test_check$For_ClearCut_EU +  test_check$For_ClearCut_im + test_check$For_ClearCut_ex +
                               test_check$For_Retention_EU + test_check$For_SelectionSystem_EU + test_check$For_PlantationFuel_im + test_check$ForOther_Extensive_EU +
                               test_check$ForOther_Extensive_RoW + test_check$ForOther_Intensive_EU + test_check$ForOther_Intensive_RoW)) > 1e-11) |
                             (test$Pasture != test_check$Pasture_EU + test_check$Pasture_RoW) |
                             (test$Permanent_crops != test_check$Permanent_EU + test_check$Permanent_RoW) |
                             (test$Urban != test_check$Urban_EU + test_check$Urban_RoW))
                         {stop("ERROR in the aggregation of land use at global level")}
              
                   rm(test, test_check)
                   # ====

                  
    }
    
   
    df <- df[2:nrow(df),]
    df <- df %>% 
      dplyr::filter(Year != "2000" & Year != "2010") %>%
       dplyr::filter(Scenario == "noAFM")
    
    # change the format (before: one column per each land use class; after the change: one single column with al the values and one with the corresponding Category)
    df <- df %>% 
        pivot_longer(cols = 4:(length(df)), names_to = "Category", values_to = "Values")

    write.csv(df, paste0(csv_path_areas, "areas_global_time-series_", case_areas, ".csv"), row.names = FALSE)
    # this csv has the following columns: 
    # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), Values (areas)
     
                   
    rm(df, results_temp) 
    
}   

