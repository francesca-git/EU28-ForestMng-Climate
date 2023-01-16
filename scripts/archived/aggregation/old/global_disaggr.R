
# GLOBAL DISAGGREGATED IMPACTS 

# Task: Produce a .csv that contains the mean values of species lost (PDF) per ecoregion and per land use category (no aggregation) 
        # for a given year selected here below. 
# Date: September 2020
# Author: Francesca Rosa

# WARNING: run the file load_and_save.R before running this one. load_and_save.R allows you to choose the .Rdata file to load and the location where the .csv files are saved

# if (missing(save_csv_path)) {
#  stop("Open, check and edit the file load_and_save.R first!")
# }

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

# Rdata file loaded using load_and_save.R
#load("./plotting/no_cutoff/sums_mg-det.RData")   

# Content:
# results from the script: slost_aggregate.R
# data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              sums - list of dataframes dataframe containing the sums of the impacts over ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum
#              tstep - vector with the time steps
#              results_median - list of dataframes containing the sums of the impacts over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              sums_median - list of dataframes dataframe containing the sums of the impacts over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  

library(dplyr)    # dataframe management
library(tidyr)      # dataframe management
library(abind)      # dataframe management
library(ggplot2)


sums = sums_median              # select median values 
results = results_median

# Focus on 2050 and 2100
t = c("2050", "2100")
year  = t[1]            # select the year

results_temp <- results[[year]] %>%
  unite("Group", Group:Forest_use, remove = TRUE) %>%
  rename(Scenario = Management) 
  
  write.csv(results_temp, paste0(save_csv_path, "global_tot-disaggr_", year, case, ".csv"), row.names = FALSE)
  
  
  
  