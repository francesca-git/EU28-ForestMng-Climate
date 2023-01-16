#############################################################################################################################################################################

                                                                  # IMPACTS OF EU LAND USE #

#############################################################################################################################################################################

# General task: convert the outputs of the model (.Rdata) into .csv files grouping and selecting only the most relevant and those to be plotted
# Date: September 2020
# Author: Francesca Rosa
# This file contains one function, which converts the Rdata to a csv considering i) impacts of all EU land uses, ii) EU forest biomass Footprint, iii) EU internal management.
# Consistently with the approach described in the manuscript, the function create.csv.EU.EPnoex includes the energy plantations imported and EU energy crops (EP), while exports are excluded. 

#############################################################################################################################################################################

# WARNING: The function defined in this file are called in the file csv_and_plotting.R. The loading/saving paths, the libraries and the working directory are defined in that file.

#############################################################################################################################################################################

# 
# Content:
# data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              sums - list of dataframes dataframe containing the sums of the impacts over the ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum, Sum_lower95, Sum_upper95
#              tstep - vector with the time steps
#              results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  
#              results_CI - list of dataframes containing the sums over ecoregions per each year considering only the CI values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              sums_CI - list of dataframes dataframe containing the sums over ecoregions per each year considering only the CI values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum_upper, Sum_lower  

library(dplyr)      # dataframe management
library(tidyr)      # dataframe management
library(abind)      # dataframe management
library(tidyverse)  # dataframe management
select <- dplyr:: select


#############################################################################################################################################################################

                                                                  # EP for EU Footprint and EU internal - No exports #

#############################################################################################################################################################################


create.csv.EU.EPnoex <- function(file_rdata_path, csv_path, file_label, year) {
  
  load(file_rdata_path)  

##################################################### IMPACTS FOR A GIVEN YEAR ##################################################### 


# Task: Produce six .csv that contain the impacts in terms of species lost (PDF) per land/forest use group or category 
        # for a given year selected here below. 
        # (grouping criteria are defined according to the scope of the subsequent analysis, either the whole EU, EU forest biomass footprint or EU internal forest impacts) 


  ################## EU #######################
  
    ######## SUMS OF GROUPED LAND USE CLASSES ########  
  
      # group the land use classes in: Afforestation, Annual_crops, Energy_crops_and_plantations, Forest, Pasture, Permanent_crops, Urban
      # and sum the corresponding impacts for each land use group
    
        results_temp <- results_median[[year]] %>%
          unite("Group", Group:Forest_use, remove = TRUE) %>%     # convert the columns containing the climatic scenarios and the forest use scenarios in a single column called "Group"
            rename(Scenario = Management) %>%                     # rename the column containing the forest management scenarios
              select(Group, Scenario, contains("EU"), (starts_with("For_") & contains("ex"))) %>%
                transmute(Group = Group, Scenario = Scenario, 
                          Afforestation = rowSums(select(., contains("Afforested"))),
                          Regrowth = 0, Forest = rowSums(select(., starts_with("For"))), 
                          Energy_crops_and_plantations = rowSums(select(., contains("EP"))),  
                          Annual_crops = rowSums(select(., contains("Annual"))),
                          Pasture = rowSums(select(., contains("Pasture"))), Permanent_crops = rowSums(select(., contains("Permanent"))),
                          Urban = rowSums(select(., contains("Urban"))))
      
      # change the format (before: one column per each land use class; after the change: one single column with al the values and one with the corresponding Category)
    
        results_oneyear <- results_temp %>% 
          pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "PDF")
        
        write.csv(results_oneyear, paste0(csv_path, "EU_", year, file_label, ".csv"), row.names = FALSE)
        # this csv has the following columns: 
        # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), PDF (impact)
        
    ######## TOTAL SUMS ########
      
      # prepare and save the file with the total sums for the given year 
      
        sums_oneyear = results[[year]] %>% 
          unite("Group", Group:Forest_use, remove = TRUE) %>%
            rename(Scenario = Management) %>% 
              select(Group, Scenario, contains("EU"), (starts_with("For_") & contains("ex"))) %>%
               transmute(Group, Scenario, PDF = rowSums(select(., contains("median"))), lower95 =  rowSums(select(., contains("lower"))), upper95 = rowSums(select(., contains("upper")))) 
      
          write.csv(sums_oneyear, paste0(csv_path, "EU_", year, file_label, "_top.csv"), row.names = FALSE) 
          # this csv has the following columns: 
          # Group (climatic and forest use scenario), Scenario (forest management scenario), PDF (impact), lower95 (lower CI interval), upper95 (upper CI interval)
          # If columns lower95 and upper95 are all 0 it means that CI have not been calculated
      
        rm(sums_oneyear, results_temp)
        
  ################## EU FOOTPRINT #######################
    
  
    ######## SUMS OF GROUPED LAND USE CLASSES ########  
    
        # group the land use classes in: EU28_Energy_crops, EU_Forest_net, Import_Energy_plantations, Import_Forest
    
          results_temp <- results_median[[year]] %>%
                            unite("Group", Group:Forest_use, remove = TRUE) %>%   # convert the columns containing the climatic scenarios and the forest use scenarios in a single column called "Group"
                              rename(Scenario = Management) %>%                   # rename the column containing the forest management scenarios
                                transmute(Group = Group, Scenario = Scenario, EU28_Energy_crops = rowSums(select(., contains("EP") & contains("EU"))), 
                                          EU28_Forest_net = rowSums(select(., (starts_with("For") & contains("EU")))), 
                                          Import_Energy_plantations = rowSums(select(., contains("EP") & contains("im"))), 
                                          Import_Forest = rowSums(select(., starts_with("For_") & contains("im"))))
          
          # change the format (before: one column per each land use class; after the change: one single column with al the values and one with the corresponding Category)
            results_oneyear <- results_temp %>% 
              pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "PDF")
          
          write.csv(results_oneyear, paste0(csv_path, "EUFootprint_", year, file_label, "_EP.csv"), row.names = FALSE)
          # this csv has the following columns: 
          # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), PDF (impact)

  ######## SUMS OF LAND USE CLASSES (IMPORTED FOREST MANAGEMENT DISAGGREGATED) ########

          results_temp_dis <- results_median[[year]] %>%
                            unite("Group", Group:Forest_use, remove = TRUE) %>%   # convert the columns containing the climatic scenarios and the forest use scenarios in a single column called "Group"
                              rename(Scenario = Management) %>%                   # rename the column containing the forest management scenarios
                                transmute(Group = Group, Scenario = Scenario, EU28_Energy_crops = rowSums(select(., contains("EP") & contains("EU"))),
                                          EU28_Forest_net = rowSums(select(., (starts_with("For") & contains("EU")))),
                                          Import_Energy_plantations = rowSums(select(., contains("EP") & contains("im"))),
                                          Clear_cut_im = For_ClearCut_im_median,
                                          Pulp_Timber_Plantation_im = For_Plantation_im_median,
                                          Selection_im = For_SelectionSystem_im_median,
                                          Selective_im = For_Selective_im_median,
                                          ReducedImpactLogging_im = For_ReducedImpactLogging_im_median)

          # change the format (before: one column per each land use class; after the change: one single column with al the values and one with the corresponding Category)
            results_oneyear <- results_temp_dis %>%
              pivot_longer(cols = 3:(length(results_temp_dis)), names_to = "Category", values_to = "PDF")

          write.csv(results_oneyear, paste0(csv_path, "EUFootprint_", year, file_label, "_EP_im-for-disaggr.csv"), row.names = FALSE)
          # this csv has the following columns:
          # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), PDF (impact)
          data_footprint_dis <- results_oneyear # to be used later
          rm(results_temp_dis, results_oneyear)

          
    ######## TOTAL SUMS ########
      
        sums_oneyear = results[[year]] %>% 
          unite("Group", Group:Forest_use, remove = TRUE) %>%
            rename(Scenario = Management) %>% 
              select(Group, Scenario, ((starts_with("For") & contains("EU")) | 
                                         (starts_with("For") & contains("im")) | 
                                         (contains("EP") & contains("EU")) | 
                                         (contains("EP") & contains("im")))) %>%
                transmute(Group, Scenario, PDF = rowSums(select(., contains("median"))), lower95 =  rowSums(select(., contains("lower"))), upper95 = rowSums(select(., contains("upper")))) 
              
          # test ====
          
          if(str_detect(file_label, "noVS") == FALSE) { # the results when the vulnerability scores are not applied are much larger than when they are included in the model. Therefore
            # without this condition the test here below will always fail when the option vulnerability is FALSE.
                  
                  test = results_temp[nrow(results_temp),]
                  test_check = results[[year]][nrow(results_temp),]
                  
                  if ((abs(test$EU28_Energy_crops - (test_check$EP_EU_median + test_check$EP_conv_EU_median)) > 1e-16) |
                      (abs(test$EU28_Forest_net - (test_check$For_ClearCut_EU_median + test_check$For_Retention_EU_median + 
                           test_check$For_SelectionSystem_EU_median + test_check$ForOther_Extensive_EU_median + test_check$ForOther_Intensive_EU_median)) > 1e-16) |
                      (abs(test$Import_Energy_plantations - test_check$EP_conv_im_median) > 1e-16) |
                      (abs(test$Import_Forest - (test_check$For_ClearCut_im_median + test_check$For_Plantation_im_median + test_check$For_SelectionSystem_im_median + test_check$For_Selective_im_median + test_check$For_ReducedImpactLogging_im_median)) > 1e-16)) 
                    {stop("ERROR in the aggregation of land use for EU footprint")}
                    
                  rm(test, test_check)
                  # ====
          }
      # prepare and save the file with the total sums for the given year 

          write.csv(sums_oneyear, paste0(csv_path, "EUFootprint_", year, file_label, "_top_EP.csv"), row.names = FALSE) 
          # this csv has the following columns: 
          # Group (climatic and forest use scenario), Scenario (forest management scenario), PDF (impact), lower95 (lower CI interval), upper95 (upper CI interval)
          # If columns lower95 and upper95 are all 0 it means that CI have not been calculated
                  
          rm(sums_oneyear, results_temp)
            
  ######## TOTAL SUMS DISAGGREGATED ########
        
        
  ################## EU INTERNAL #######################
  
  
    ######## SUMS OF GROUPED LAND USE CLASSES ########  
      
      # group the land use classes in: Clear_cut, Retention, Selection, Other_management
    
        results_temp <- results_median[[year]] %>%
                          unite("Group", Group:Forest_use, remove = TRUE) %>%   # convert the columns containing the climatic scenarios and the forest use scenarios in a single column called "Group"
                            rename(Scenario = Management) %>%                   # rename the column containing the forest management scenarios
                              transmute(Group = Group, Scenario = Scenario, 
                                        Clear_cut = rowSums(select(., contains("ClearCut") & contains("EU"))),
                                        Retention = rowSums(select(., contains("Retention") & contains("EU"))),
                                        Selection = rowSums(select(., contains("Selection") & contains("EU"))),
                                        Timber = rowSums(select(., contains("Timber") & contains("EU"))),
                                        Other_management = rowSums(select(., contains("ForOther") & contains("EU"))),
                                        EP_EU = rowSums(select(., contains("EP") & contains("EU"))))
        
      
          results_oneyear <- results_temp %>% 
          pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "PDF")
          
          data_eu_dis <- results_oneyear %>% data.frame() # to be used later
          
          write.csv(results_oneyear, paste0(csv_path, "EUForest_", year, file_label, "_EPnoex.csv"), row.names = FALSE)
          # this csv has the following columns: 
          # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), PDF (impact)
        
    ######## TOTAL SUMS ########
      
      # prepare and save the file with the total sums for the given year 
      
        sums_oneyear = results[[year]] %>% 
          unite("Group", Group:Forest_use, remove = TRUE) %>%
            rename(Scenario = Management) %>% 
              select(Group, Scenario, (((contains("ClearCut")| contains("Retention")| contains("Selection") | contains("Timber") | contains("EP")) & (contains("EU"))) | (contains("ForOther") & contains("EU")))) %>%
                transmute(Group, Scenario, PDF = rowSums(select(., contains("median"))), lower95 =  rowSums(select(., contains("lower"))), upper95 = rowSums(select(., contains("upper")))) 
      
          write.csv(sums_oneyear, paste0(csv_path, "EUForest_", year, file_label, "_top_EPnoex.csv"), row.names = FALSE) 
          # this csv has the following columns: 
          # Group (climatic and forest use scenario), Scenario (forest management scenario), PDF (impact), lower95 (lower CI interval), upper95 (upper CI interval)
          # If columns lower95 and upper95 are all 0 it means that CI have not been calculated
          
          # test ====

          if(str_detect(file_label, "noVS") == FALSE) { # the results when the vulnerability scores are not applied are much larger than when they are included in the model. Therefore
            # without this condition the test here below will always fail when the option vulnerability is FALSE.
                    
                    test = results_temp[nrow(results_temp),]
                    test_check = results[[year]][nrow(results_temp),]
                    test_sum = sums_oneyear[nrow(results_temp),]
                    
                    if ((abs(test$Clear_cut - test_check$For_ClearCut_EU_median) > 1e-16) |
                        (abs(test$Retention - (test_check$For_Retention_EU_median)) > 1e-16) |
                        (abs(test$Selection - test_check$For_SelectionSystem_EU_median) > 1e-16) |
                        (abs(test$Timber - test_check$For_TimberPlant_EU_median) > 1e-16) |
                        (abs(test$Other_management - (test_check$ForOther_Extensive_EU_median + test_check$ForOther_Intensive_EU_median)) > 1e-16) |
                        (abs(test_sum$PDF -(test$Clear_cut + test$Retention + test$Selection + test$Other_management + test$EP_EU)) > 1e-16)) 
                      {stop("ERROR in the aggregation of land use for EU Internal")}
                    
                    rm(test, test_check)
                    # ====
          }
          
        rm(results_temp, results_oneyear)

        
  ################## EU FOOTPRINT - ALL CATEGORIES DISAGGREGATED #######################
  
        data <- data_footprint_dis %>%
              mutate(Category = str_replace(Category, "Import_Energy_plantations", "Energy_plantations_im"))  # Rename one of the category to contain "_im"
        data_EU <- data_eu_dis %>%
                    filter(Category != "Other_management" & Category != "EP_EU")
        # Rename the categories to make them handier
        data_EU <- data_EU %>% mutate(Category = str_replace(Category, "Clear_cut", "Clear_cut_EU"),
                                Category = str_replace(Category, "Retention", "Retention_EU"),
                                Category = str_replace(Category, "Selection", "Selection_EU"),
                                Category = str_replace(Category, "Timber", "Timber_EU"))
        
        # Remove the grouped Category for EU internal impacts
        data <- data %>% filter(Category != "EU28_Forest_net") %>% full_join(data_EU) %>%
          full_join(data_EU)
                      
        # Clean and arrange the data 
        data <- data %>% mutate(Category = str_replace(Category, "EU28_Energy_crops", "EU28 - Lignocel. energy crops"), 
                                Category = str_replace(Category, "Clear_cut_EU", "EU28 - Clear cut"),
                                Category = str_replace(Category, "Retention_EU", "EU28 - Retention"),
                                Category = str_replace(Category, "Selection_EU", "EU28 - Selection"),
                                Category = str_replace(Category, "Timber_EU", "EU28 - Timber plantations"),
                                Category = str_replace(Category, "Energy_plantations_im", "Import - Energy plantations"), 
                                Category = str_replace(Category, "Pulp_Timber_Plantation_im", "Import - Timber and pulp plantations"),
                                Category = str_replace(Category, "Clear_cut_im", "Import - Clear cut"),
                                Category = str_replace(Category, "Selection_im", "Import - Selection"),
                                Category = str_replace(Category, "Selective_im", "Import - Selective logging"),
                                Category = str_replace(Category, "ReducedImpactLogging_im", "Import - Reduced Impact Logging"))
        
        data <- data %>% filter(Category != "EU28 - Timber plantations")
        
        data <- data %>% separate(col = Group, into = c("Pathway", "Group"), sep = "_") %>% data.frame()
        
              # save the data in a new .csv file
        write.csv(data, paste0(csv_path, "EUFootprint_", year, file_label, "_EP_disaggr.csv"), row.names = FALSE)

        rm(data_footprint_dis, data_eu_dis, data, data_EU)
        
##################################################### TIME SERIES OF IMPACTS ##################################################### 
  
  # TIME SERIES (WITH CI)
                    
  # Task: Produce two .csv that contain the impacts in terms of species lost (PDF) per forest use group or category 
          # (forest categories are grouped/summed according to the scope, either EU footprint or EU internal impacts) 
          # for all years 

    
    #sums = sums_CI 
  
  # Create a df containing all the sums for all the years
  #df = data.frame(matrix(NA, nrow=(nrow(sums[[1]])*length(sums)), ncol=(ncol(sums[[1]])+1)))
  
  
  ################## EU FOOTPRINT #######################
  
  
    df <- sums[[1]][1,] %>%
      rename(PDF = Sum_median, upper95 = Sum_upper95, lower95 = Sum_lower95) %>%
        dplyr::select(-Forest_use, -Management)
    
      for(i in 1:length(tstep)) {
      
        results[[i]]$Year <- toString(tstep[i])
        
          temp <- results[[i]] %>% unite("Group", Group:Forest_use, remove = TRUE) %>%
            rename(Scenario = Management) %>% 
              transmute(Group = Group, Scenario = Scenario, Year = Year, 
                    PDF = rowSums(dplyr::select(., contains("median")& ((contains("EP") & contains("EU"))|(starts_with("For") & contains("EU"))| 
                                                                       (contains("EP") & contains("im"))|(starts_with("For_") & contains("im"))))),
                    upper95 = rowSums(dplyr::select(., contains("upper")& ((contains("EP") & contains("EU"))|(starts_with("For") & contains("EU"))| 
                                                                      (contains("EP") & contains("im"))|(starts_with("For_") & contains("im"))))),
                    lower95 = rowSums(dplyr::select(., contains("lower")& ((contains("EP") & contains("EU"))|(starts_with("For") & contains("EU"))|
                                                                      (contains("EP") & contains("im"))|(starts_with("For_") & contains("im"))))))
          
         # test ====
          
        if(str_detect(file_label, "noVS") == FALSE) { # the results when the vulnerability scores are not applied are much larger than when they are included in the model. Therefore
            # without this condition the test here below will always fail when the option vulnerability is FALSE.
                  
         test = temp[nrow(temp),]
         test_check = results[[i]][nrow(temp),]
         
         if (abs(test$PDF - (test_check$EP_EU_median + test_check$EP_conv_EU_median +
              test_check$For_ClearCut_EU_median + test_check$For_Retention_EU_median +
              test_check$For_SelectionSystem_EU_median + test_check$For_TimberPlant_EU_median + test_check$ForOther_Extensive_EU_median + 
              test_check$ForOther_Intensive_EU_median + test_check$EP_conv_im_median + 
              test_check$For_ClearCut_im_median + test_check$For_Plantation_im_median + test_check$For_SelectionSystem_im_median + test_check$For_Selective_im_median + test_check$For_ReducedImpactLogging_im_median)) > 1e-15) 
           {stop("ERROR in EU Footprint")}
         
         rm(test, test_check)
         # ====
         
        }
          
        df = df %>% bind_rows(temp)                     # bind columns together
      }
      
      df <- df[2:nrow(df),]
      df <- df %>% 
              dplyr::filter(Year != "2000" & Year != "2010") %>%
                arrange(Year)
      
      write.csv(df, paste0(csv_path, "EUFootprint_time-series", file_label, "_EP.csv"), row.names = FALSE) 
      # this csv has the following columns: 
      # Group (climatic and forest use scenario), PDF (impact), lower95 (lower CI interval), upper95 (upper CI interval), Scenario (forest management scenario), Year

      rm(df, temp)
    
  
  ################## EU INTERNAL #######################
  
    df <- sums[[1]][1,] %>%
      rename(PDF = Sum_median, upper95 = Sum_upper95, lower95 = Sum_lower95) %>%
        select(-Forest_use, -Management)
    
        for(i in 1:length(tstep)) {
        
          results[[i]]$Year <- toString(tstep[i])
          
          temp <- results[[i]] %>% unite("Group", Group:Forest_use, remove = TRUE) %>%
            rename(Scenario = Management) %>% 
              transmute(Group = Group, Scenario = Scenario, Year = Year,
                    PDF = rowSums(dplyr::select(., contains("median") & 
                                                       (((contains("ClearCut")| contains("Retention")| contains("Selection") 
                                                        | contains("Timber") | contains("EP")) & (contains("EU")))
                                                        | (contains("ForOther") & contains("EU"))))),
                    lower95 = rowSums(dplyr::select(., contains("lower") &   
                                                        (((contains("ClearCut")| contains("Retention")| contains("Selection") 
                                                        | contains("Timber") | contains("EP")) & (contains("EU")))
                                                        | (contains("ForOther") & contains("EU"))))),
                    upper95 = rowSums(dplyr::select(., contains("upper") &   (((contains("ClearCut")| contains("Retention")| contains("Selection") 
                                                        | contains("Timber") | contains("EP")) & (contains("EU")))
                                                        | (contains("ForOther") & contains("EU"))))))
      

        
        df = df %>% bind_rows(temp)                     # bind columns together
      }
  
      df <- df[2:nrow(df),]
      df <- df %>% 
        dplyr::filter(Year != "2000" & Year != "2010") %>%
        arrange(Year)
      
      write.csv(df, paste0(csv_path, "EUForest_time-series", file_label, "_EPnoex.csv"), row.names = FALSE) 
      # this csv has the following columns: 
      # Group (climatic and forest use scenario), PDF (impact), lower95 (lower CI interval), upper95 (upper CI interval), Scenario (forest management scenario), Year
      
      rm(df, temp)
  
}

