#############################################################################################################################################################################

                                                                  # IMPACTS OF EU LAND USE #

#############################################################################################################################################################################

# General task: convert the outputs of the model (.Rdata) into .csv files grouping and selecting only the most relevant 
# Date: September 2020
# Author: Francesca Rosa


#############################################################################################################################################################################

# WARNING: The functions defined in this file are called in the file csv_and_plotting.R. The loading/saving paths, the libraries and the working directory are defined in that file.

#############################################################################################################################################################################


# file loaded (.Rdata) using load_and_save.R: results from the script: slost_aggregate.R
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


create.csv.EUEPnoex <- function(CI, rdata_path, csv_path, case_subcase) {
  
  load(rdata_path)  


##################################################### IMPACTS FOR A GIVEN YEAR ##################################################### 


# Task: Produce six .csv that contain the impacts in terms of species lost (PDF) per land/forest use group or category 
        # for a given year selected here below. 
        # (grouping criteria are defined according to the scope of the subsequent analysis, either the whole EU, EU forest biomass footprint or EU internal forest impacts) 


# focus on 2050 and 2100
t = c("2050", "2100") 
year  = t[2]              # select the year

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
          pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "PDFx100")
        
        write.csv(results_oneyear, paste0(csv_path, "EU_", year, case_subcase, ".csv"), row.names = FALSE)
        # this csv has the following columns: 
        # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), PDFx100 (impact)
        
    ######## TOTAL SUMS ########
      
      # prepare and save the file with the total sums for the given year 
      
        sums_oneyear = results[[year]] %>% 
          unite("Group", Group:Forest_use, remove = TRUE) %>%
            rename(Scenario = Management) %>% 
              select(Group, Scenario, contains("EU"), (starts_with("For_") & contains("ex"))) %>%
               transmute(Group, Scenario, PDFx100 = rowSums(select(., contains("median"))), lower95 =  rowSums(select(., contains("lower"))), upper95 = rowSums(select(., contains("upper")))) 
      
        if (CI == TRUE) { write.csv(sums_oneyear, paste0(csv_path, "EU_", year, case_subcase, "_top_CI.csv"), row.names = FALSE) 
          } else if (CI == FALSE) { write.csv(sums_oneyear, paste0(csv_path, "EU_", year, case_subcase, "_top.csv"), row.names = FALSE) }
          # this csv has the following columns: 
          # Group (climatic and forest use scenario), Scenario (forest management scenario), PDFx100 (impact), lower95 (lower CI interval), upper95 (upper CI interval)
          # If CI == FALSE, the columns lower95 and upper95 are all 0
      
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
              pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "PDFx100")
          
          write.csv(results_oneyear, paste0(csv_path, "EUFootprint_", year, case_subcase, ".csv"), row.names = FALSE)
          # this csv has the following columns: 
          # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), PDFx100 (impact)
        
    ######## TOTAL SUMS ########
      
      
        sums_oneyear = results[[year]] %>% 
          unite("Group", Group:Forest_use, remove = TRUE) %>%
            rename(Scenario = Management) %>% 
              select(Group, Scenario, ((starts_with("For") & contains("EU")) | 
                                         (starts_with("For") & contains("im")) | 
                                         (contains("EP") & contains("EU")) | 
                                         (contains("EP") & contains("im")))) %>%
                transmute(Group, Scenario, PDFx100 = rowSums(select(., contains("median"))), lower95 =  rowSums(select(., contains("lower"))), upper95 = rowSums(select(., contains("upper")))) 
      
                  # test ====
                  
                  test = results_temp[nrow(results_temp),]
                  test_check = results[[year]][nrow(results_temp),]
                  
                  if ((abs(test$EU28_Energy_crops - (test_check$EP_EU_median + test_check$EP_conv_EU_median)) > 1e-16) |
                      (abs(test$EU28_Forest_net - (test_check$For_ClearCut_EU_median + test_check$For_Retention_EU_median + 
                           test_check$For_SelectionSystem_EU_median + test_check$ForOther_Extensive_EU_median + test_check$ForOther_Intensive_EU_median)) > 1e-16) |
                      (abs(test$Import_Energy_plantations - test_check$EP_conv_im_median) > 1e-16) |
                      (abs(test$Import_Forest - (test_check$For_ClearCut_im_median + test_check$For_Plantation_im_median)) > 1e-16)) 
                    {stop("ERROR in the aggregation of land use for EU footprint")}
                    
                  rm(test, test_check)
                  # ====
      
      # prepare and save the file with the total sums for the given year 

        if (CI == TRUE) { write.csv(sums_oneyear, paste0(csv_path, "EUFootprint_", year, case_subcase, "_top_CI.csv"), row.names = FALSE) 
                } else if (CI == FALSE) { write.csv(sums_oneyear, paste0(csv_path, "EUFootprint_", year, case_subcase, "_top.csv"), row.names = FALSE) }
                # this csv has the following columns: 
                # Group (climatic and forest use scenario), Scenario (forest management scenario), PDFx100 (impact), lower95 (lower CI interval), upper95 (upper CI interval)
                # If CI == FALSE, the columns lower95 and upper95 are all 0
                  
            rm(sums_oneyear, results_temp)
        
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
          pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "PDFx100")
      
          write.csv(results_oneyear, paste0(csv_path, "EUForest_", year, case_subcase, "_EP_noex.csv"), row.names = FALSE)
          # this csv has the following columns: 
          # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), PDFx100 (impact)
        
    ######## TOTAL SUMS ########
      
      # prepare and save the file with the total sums for the given year 
      
        sums_oneyear = results[[year]] %>% 
          unite("Group", Group:Forest_use, remove = TRUE) %>%
            rename(Scenario = Management) %>% 
              select(Group, Scenario, (((contains("ClearCut")| contains("Retention")| contains("Selection") |  contains("Timber") | contains("EP")) & (contains("EU"))) | (contains("ForOther") & contains("EU")))) %>%
                transmute(Group, Scenario, PDFx100 = rowSums(select(., contains("median"))), lower95 =  rowSums(select(., contains("lower"))), upper95 = rowSums(select(., contains("upper")))) 
      
          if (CI == TRUE) { write.csv(sums_oneyear, paste0(csv_path, "EUForest_", year, case_subcase, "_top_CI_EP_noex.csv"), row.names = FALSE) 
            } else if (CI == FALSE) { write.csv(sums_oneyear, paste0(csv_path, "EUForest_", year, case_subcase, "_top_EP_noex.csv"), row.names = FALSE) }
          # this csv has the following columns: 
          # Group (climatic and forest use scenario), Scenario (forest management scenario), PDFx100 (impact), lower95 (lower CI interval), upper95 (upper CI interval)
          # If CI == FALSE, the columns lower95 and upper95 are all 0
           
                    # test ====
                    
                    test = results_temp[nrow(results_temp),]
                    test_check = results[[year]][nrow(results_temp),]
                    test_sum = sums_oneyear[nrow(results_temp),]
                    
                    if ((abs(test$Clear_cut - test_check$For_ClearCut_EU_median) > 1e-16) |
                        (abs(test$Retention - (test_check$For_Retention_EU_median)) > 1e-16) |
                        (abs(test$Selection - test_check$For_SelectionSystem_EU_median) > 1e-16) |
                        (abs(test$Timber - test_check$For_TimberPlant_EU_median) > 1e-16) |
                        (abs(test$Other_management - (test_check$ForOther_Extensive_EU_median + test_check$ForOther_Intensive_EU_median)) > 1e-16) |
                        (abs(test_sum$PDFx100 -(test$Clear_cut + test$Retention + test$Selection + test$Other_management + test$EP_EU)) > 1e-16)) 
                      {stop("ERROR in the aggregation of land use for EU Internal")}
                    
                    rm(test, test_check)
                    # ====
                    
        rm(sums_oneyear, results_temp)

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
      rename(PDFx100 = Sum_median, upper95 = Sum_upper95, lower95 = Sum_lower95) %>%
        dplyr::select(-Forest_use, -Management)
    
      for(i in 1:length(tstep)) {
      
        results[[i]]$Year <- toString(tstep[i])
        
          temp <- results[[i]] %>% unite("Group", Group:Forest_use, remove = TRUE) %>%
            rename(Scenario = Management) %>% 
              transmute(Group = Group, Scenario = Scenario, Year = Year, 
                    PDFx100 = rowSums(dplyr::select(., contains("median")& ((contains("EP") & contains("EU"))|(starts_with("For") & contains("EU"))| 
                                                                       (contains("EP") & contains("im"))|(starts_with("For_") & contains("im"))))),
                    upper95 = rowSums(dplyr::select(., contains("upper")& ((contains("EP") & contains("EU"))|(starts_with("For") & contains("EU"))| 
                                                                      (contains("EP") & contains("im"))|(starts_with("For_") & contains("im"))))),
                    lower95 = rowSums(dplyr::select(., contains("lower")& ((contains("EP") & contains("EU"))|(starts_with("For") & contains("EU"))|
                                                                      (contains("EP") & contains("im"))|(starts_with("For_") & contains("im"))))))
          
         # test ====
       
         test = temp[nrow(temp),]
         test_check = results[[i]][nrow(temp),]
         
         if (abs(test$PDFx100 - (test_check$EP_EU_median + test_check$EP_conv_EU_median +
              test_check$For_ClearCut_EU_median + test_check$For_Retention_EU_median +
              test_check$For_SelectionSystem_EU_median + test_check$ForOther_Extensive_EU_median + 
              test_check$ForOther_Intensive_EU_median + test_check$EP_conv_im_median + 
              test_check$For_ClearCut_im_median + test_check$For_Plantation_im_median)) > 1e-15) 
           {stop("ERROR in EU Footprint")}
         
         rm(test, test_check)
         # ====
         
      
        df = df %>% bind_rows(temp)                     # bind columns together
      }
      
      df <- df[2:nrow(df),]
      df <- df %>% 
              dplyr::filter(Year != "2000" & Year != "2010") %>%
                arrange(Year)
      
       if (CI == TRUE) { write.csv(df, paste0(csv_path, "EUFootprint_time-series", case_subcase, "_CI.csv"), row.names = FALSE)
                } else if (CI == FALSE) { write.csv(df, paste0(csv_path, "EUFootprint_time-series", case_subcase, ".csv"), row.names = FALSE) }
      # this csv has the following columns: 
      # Group (climatic and forest use scenario), PDFx100 (impact), lower95 (lower CI interval), upper95 (upper CI interval), Scenario (forest management scenario), Year

      rm(df, temp)
    
  
  ################## EU INTERNAL #######################
  
    df <- sums[[1]][1,] %>%
      rename(PDFx100 = Sum_median, upper95 = Sum_upper95, lower95 = Sum_lower95) %>%
        select(-Forest_use, -Management)
    
        for(i in 1:length(tstep)) {
        
          results[[i]]$Year <- toString(tstep[i])
          
          temp <- results[[i]] %>% unite("Group", Group:Forest_use, remove = TRUE) %>%
            rename(Scenario = Management) %>% 
              transmute(Group = Group, Scenario = Scenario, Year = Year,
                    PDFx100 = rowSums(dplyr::select(., contains("median") &  ((contains("ClearCut") & (contains("EU")|contains("ex")))|
                                                                                (contains("Retention") & (contains("EU")|contains("ex")))|
                                                                                (contains("Selection") & (contains("EU")|contains("ex")))|
                                                                                (contains("Timber") & (contains("EU")|contains("ex")))|
                                                                                (contains("ForOther") & contains("EU"))))),
                    lower95 = rowSums(dplyr::select(., contains("lower") &  ((contains("ClearCut") & (contains("EU")|contains("ex")))|
                                                                                (contains("Retention") & (contains("EU")|contains("ex")))|
                                                                                (contains("Selection") & (contains("EU")|contains("ex")))|
                                                                                (contains("Timber") & (contains("EU")|contains("ex")))|
                                                                                (contains("ForOther") & contains("EU"))))),
                    upper95 = rowSums(dplyr::select(., contains("upper") &  ((contains("ClearCut") & (contains("EU")|contains("ex")))|
                                                                                (contains("Retention") & (contains("EU")|contains("ex")))|
                                                                                (contains("Selection") & (contains("EU")|contains("ex")))|
                                                                                (contains("Timber") & (contains("EU")|contains("ex")))|
                                                                                (contains("ForOther") & contains("EU"))))))
      
                                           
        # # test ====
        # test = temp[nrow(temp),]
        # test_check = results[[i]][nrow(temp),]
        # 
        # if (abs(test$PDFx100 - (test_check$For_ClearCut_EU_median + test_check$For_ClearCut_ex_median + 
        #                         test_check$For_Retention_EU_median + test_check$For_SelectionSystem_EU_median +
        #                         test_check$ForOther_Extensive_EU_median + test_check$ForOther_Intensive_EU_median)) > 1e-16)
        # {stop("ERROR in EU Forest")}
        # 
        # rm(test, test_check)
        # 
        # # ====
        
        df = df %>% bind_rows(temp)                     # bind columns together
      }
  
      df <- df[2:nrow(df),]
      df <- df %>% 
        dplyr::filter(Year != "2000" & Year != "2010") %>%
        arrange(Year)
      
      if (CI == TRUE) { write.csv(df, paste0(csv_path, "EUForest_time-series", case_subcase, "_CI.csv"), row.names = FALSE) 
                } else if (CI == FALSE) { write.csv(df, paste0(csv_path, "EUForest_time-series", case_subcase, ".csv"), row.names = FALSE) }
      # this csv has the following columns: 
      # Group (climatic and forest use scenario), PDFx100 (impact), lower95 (lower CI interval), upper95 (upper CI interval), Scenario (forest management scenario), Year
      
      rm(df, temp)
  
}