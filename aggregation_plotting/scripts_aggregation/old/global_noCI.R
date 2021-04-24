
# GLOBAL IMPACTS 

# Task: Produce two .csv that contain the impacts in terms of species lost (PDF)
        # 1) per scenario and summed over all land use categories for a given year selected here below 
        # 2) per scenario and per land use group for a given year selected here below (impacts of land use categories of the same group are summed, e.g. the impacts of all forest land use categories are summed together). 
        
# Date: September 2020
# Author: Francesca Rosa

# WARNING: run the file load_and_save.R before running this one. load_and_save.R allows you to choose the .Rdata file to load and the location where the .csv files are saved

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

# Rdata file loaded using load_and_save.R
# load("./plotting/no_cutoff/sums_mg-det.RData")   

# Content:
# results from the script: slost_aggregate.R
                        # data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                        #              sums - list of dataframes dataframe containing the sums over ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum
                        #              tstep - vector with the time steps
                        #              results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                        #              sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  


library(dplyr)    # dataframe management
library(tidyr)      # dataframe management
library(abind)      # dataframe management
library(ggplot2)


sums = sums_median              # select median values 
results = results_median

# Focus on 2050 and 2100
t = c("2050", "2100")
year  = t[1]            # select the year


################## 1) TOTAL SUMS #######################

  # prepare and save the file with the total sums and the CI for a given year
    
    sums_oneyear = sums[[year]] %>% 
      unite("Group", Group:Forest_use, remove = TRUE) %>%
        rename(Scenario = Management, PDFx100 = Sum) 
        
    write.csv(sums_oneyear, paste0(save_csv_path, "global_", year, case, "_top.csv"), row.names = FALSE)
    
    
################## 2) SUMS PER LAND USE GROUP #######################
    
  # aggregate the land use classes in: Afforestation, Annual_crops, Energy_crops_and_plantations, Forest, Pasture, Permanent_crops, Urban
    results_temp <- results[[year]] %>%
              unite("Group", Group:Forest_use, remove = TRUE) %>%
                rename(Scenario = Management) %>% 
                  transmute(Group = Group, Scenario = Scenario, Afforestation = rowSums(select(., contains("Afforested"))), 
                          Regrowth = rowSums(select(., contains("Regrowth"))), 
                          Forest = rowSums(select(., starts_with("For"))), 
                          Energy_crops_and_plantations = rowSums(select(., contains("EP"))), 
                          Annual_crops = rowSums(select(., contains("Annual"))),
                          Pasture = rowSums(select(., contains("Pasture"))), Permanent_crops = rowSums(select(., contains("Permanent"))),
                          Urban = rowSums(select(., contains("Urban"))))
  # change the format (before: one column per each land use class; after the change: one single column with al the values and one with the corresponding Category)
      results_oneyear <- results_temp %>% 
                        pivot_longer(cols = 3:(length(results_temp)), names_to = "Category", values_to = "PDFx100")
    
    write.csv(results_oneyear, paste0(save_csv_path, "global_", year, case, ".csv"), row.names = FALSE)


# test ====

test = results_temp[nrow(results_temp),]
test_check = results[[year]][nrow(results_temp),]

    if ((test$Afforestation != test_check$Afforested_EU_median + test_check$Afforested_RoW_median) |
    (test$Regrowth != test_check$Regrowth_median) |
    (test$Annual_crops != test_check$Annual_EU_median + test_check$Annual_RoW_median) |
    ((abs(test$Energy_crops_and_plantations - (test_check$EP_EU_median + test_check$EP_conv_EU_median + test_check$EP_RoW_median + test_check$EP_conv_im_median))) > 1e-11 ) |
    (test$Forest != test_check$For_ClearCut_EU_median +  test_check$For_ClearCut_im_median + test_check$For_ClearCut_ex_median + 
      test_check$For_Retention_EU_median + test_check$For_SelectionSystem_EU_median + test_check$For_PlantationFuel_im_median + test_check$ForOther_Extensive_EU_median + 
      test_check$ForOther_Extensive_RoW_median + test_check$ForOther_Intensive_EU_median + test_check$ForOther_Intensive_RoW_median) |
    (test$Pasture != test_check$Pasture_EU_median + test_check$Pasture_RoW_median) |
    (test$Permanent_crops != test_check$Permanent_EU_median + test_check$Permanent_RoW_median) |
    (test$Urban != test_check$Urban_EU_median + test_check$Urban_RoW_median)) {stop("ERROR in the aggregation of land use at global level")}

# ====

