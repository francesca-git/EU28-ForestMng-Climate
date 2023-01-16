

# EU IMPACT TIME SERIES, BOTH EU FOOTPRINT AND INTERNAL IMPACTS

# Task: Produce six .csv that contain the mean values of species lost (PDF) per land/forest use category grouping of the forest categories according to the scope, either all EU, EU forest biomass footprint or EU internal forest impacts) 
        # for a given year selected here below. 

# Date: September 2020
# Author: Francesca Rosa

# WARNING: run the file load_and_save.R before running this one. load_and_save.R allows you to choose the .Rdata file to load and the location where the .csv files are saved

# if (missing(save_csv_path)) {
#  stop("Open, check and edit the file load_and_save.R first!")
# }

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

# Rdata file loaded using load_and_save.R
#load("./plotting/no_cutoff/sums_mg-det_CI.RData")   

# Content:
# results from the script: slost_aggregate.R
# data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              sums - list of dataframes dataframe containing the sums of the impacts over the ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum, Sum_lower95, Sum_upper95
#              tstep - vector with the time steps
#              results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  
#              results_CI - list of dataframes containing the sums over ecoregions per each year considering only the CI values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              sums_CI - list of dataframes dataframe containing the sums over ecoregions per each year considering only the CI values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum_upper, Sum_lower  


library(dplyr)    # dataframe management
library(tidyr)      # dataframe management
library(abind)      # dataframe management
library(ggplot2)

# focus on 2050 and 2100
t = c("2050", "2100") 
year  = t[2]              # select the year

# focus on EU

################## EU #######################

  # sums of aggregated land use classes ==== 
  # aggregate the land use classes in: Afforestation, Annual_crops, Energy_crops_and_plantations, Forest, Pasture, Permanent_crops, Urban
  results_temp <- results_median[[year]] %>%
    unite("Group", Group:Forest_use, remove = TRUE) %>%
      rename(Scenario = Management) %>% 
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
  
  write.csv(results_oneyear, paste0(save_csv_path, "EU_", year, case, ".csv"), row.names = FALSE)

  #total sums ====
  # prepare and save the file with the total sums for the given year 
  
  sums_oneyear = results[[year]] %>% 
    unite("Group", Group:Forest_use, remove = TRUE) %>%
      rename(Scenario = Management) %>% 
        select(Group, Scenario, contains("EU"), (starts_with("For_") & contains("ex"))) %>%
         transmute(Group, Scenario, PDFx100 = rowSums(select(., contains("median"))), lower95 =  rowSums(select(., contains("lower"))), upper95 = rowSums(select(., contains("upper")))) 

  write.csv(sums_oneyear, paste0(save_csv_path, "EU_", year, case, "_top_CI.csv"), row.names = FALSE)
  

################## EU FOOTPRINT #######################
  
  
  # sums of aggregated land use classes  
  # aggregate the land use classes in: EU28_Energy_crops, EU_Forest_net, Import_Energy_plantations, Import_Forest
  
    results_temp <- results_median[[year]] %>%
                      unite("Group", Group:Forest_use, remove = TRUE) %>%
                        rename(Scenario = Management) %>% 
                          transmute(Group = Group, Scenario = Scenario, EU28_Energy_crops = rowSums(select(., contains("EP") & contains("EU"))), 
                                    EU28_Forest_net = rowSums(select(., (starts_with("For") & contains("EU")))), 
                                    Import_Energy_plantations = rowSums(select(., contains("EP") & contains("im"))), 
                                    Import_Forest = rowSums(select(., starts_with("For_") & contains("im"))))
    
    # change the format (before: one column per each land use class; after the change: one single column with al the values and one with the corresponding Category)
      results_oneyear <- results_temp %>% 
        pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "PDFx100")
    
    write.csv(results_oneyear, paste0(save_csv_path, "EUFootprint_", year, case, ".csv"), row.names = FALSE)
   
    #total sums ====
    # prepare and save the file with the total sums for the given year 
    
    sums_oneyear = results[[year]] %>% 
      unite("Group", Group:Forest_use, remove = TRUE) %>%
        rename(Scenario = Management) %>% 
          select(Group, Scenario, ((starts_with("For") & contains("EU")) | 
                                     (starts_with("For") & contains("im")) | 
                                     (contains("EP") & contains("EU")) | 
                                     (contains("EP") & contains("im")))) %>%
            transmute(Group, Scenario, PDFx100 = rowSums(select(., contains("median"))), lower95 =  rowSums(select(., contains("lower"))), upper95 = rowSums(select(., contains("upper")))) 
    
    write.csv(sums_oneyear, paste0(save_csv_path, "EUFootprint_", year, case, "_top_CI.csv"), row.names = FALSE)

    
    # test ====
    
    test = results_temp[nrow(results_temp),]
    test_check = results[[year]][nrow(results_temp),]
    
    if ((test$EU28_Energy_crops != test_check$EP_EU_median + test_check$EP_conv_EU_median) |
        (test$EU28_Forest_net != test_check$For_ClearCut_EU_median + test_check$For_Retention_EU_median + test_check$For_SelectionSystem_EU_median + test_check$ForOther_Extensive_EU_median + test_check$ForOther_Intensive_EU_median) |
        (test$Import_Energy_plantations != test_check$EP_conv_im_median) |
        (test$Import_Forest != test_check$For_ClearCut_im_median + test_check$For_PlantationFuel_im_median)) {stop("ERROR in the aggregation of land use for EU footprint")}
      
    rm(test, test_check)
    # ====
    
    
################## EU INTERNAL #######################
    
    
  # sums of aggregated land use classes  
  # aggregate the land use classes in: Clear_cut, Retention, Selection, Other_management

  results_temp <- results_median[[year]] %>%
                    unite("Group", Group:Forest_use, remove = TRUE) %>%
                      rename(Scenario = Management) %>% 
                        transmute(Group = Group, Scenario = Scenario, 
                                  Clear_cut = rowSums(select(., contains("ClearCut") & (contains("EU")|contains("ex")))),
                                  Retention = rowSums(select(., contains("Retention") & (contains("EU")|contains("ex")))),
                                  Selection = rowSums(select(., contains("Selection") & (contains("EU")|contains("ex")))),
                                  Timber = rowSums(select(., contains("Timber") & (contains("EU")|contains("ex")))),
                                  Other_management = rowSums(select(., contains("ForOther") & contains("EU"))))
  

    results_oneyear <- results_temp %>% 
    pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "PDFx100")

    write.csv(results_oneyear, paste0(save_csv_path, "EUForest_", year, case, ".csv"), row.names = FALSE)
    #total sums ====
    # prepare and save the file with the total sums for the given year 
    
    sums_oneyear = results[[year]] %>% 
      unite("Group", Group:Forest_use, remove = TRUE) %>%
        rename(Scenario = Management) %>% 
          select(Group, Scenario, (((contains("ClearCut")| contains("Retention")| contains("Selection") |  contains("Timber")) & (contains("EU") | contains("ex"))) | (contains("ForOther") & contains("EU")))) %>%
            transmute(Group, Scenario, PDFx100 = rowSums(select(., contains("median"))), lower95 =  rowSums(select(., contains("lower"))), upper95 = rowSums(select(., contains("upper")))) 
    
    write.csv(sums_oneyear, paste0(save_csv_path, "EUForest_", year, case, "_top_CI.csv"), row.names = FALSE)

  # test ====
  
  test = results_temp[nrow(results_temp),]
  test_check = results[[year]][nrow(results_temp),]
  test_sum = sums_oneyear[nrow(results_temp),]
  
  if ((test$Clear_cut != test_check$For_ClearCut_EU_median + test_check$For_ClearCut_ex_median) |
      (test$Retention != test_check$For_Retention_EU_median) |
      (test$Selection != test_check$For_SelectionSystem_EU_median) |
      (test$Other_management != test_check$ForOther_Extensive_EU_median + test_check$ForOther_Intensive_EU_median) |
      (abs(test_sum$PDFx100 -(test$Clear_cut + test$Retention + test$Selection + test$Other_management)) > 1e-16)) {stop("ERROR in the aggregation of land use for EU footprint")}
  
  rm(test, test_check)
  # ====
  
  

