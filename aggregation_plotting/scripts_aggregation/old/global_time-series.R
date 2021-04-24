
# TIME SERIES OF GLOBAL IMPACTS 

# Task: Produce one .csv that contains the impacts in terms of species lost (PDF) 
        # per scenario and per land use group (impacts of land use categories of the same group are summed, e.g. the impacts of all forest land use categories are summed together). 
        
# Date: September 2020
# Author: Francesca Rosa

# WARNING: run the file load_and_save.R before running this one. load_and_save.R allows you to choose the .Rdata file to load and the location where the .csv files are saved

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

# Rdata file loaded using load_and_save.R
# load("./plotting/sums_mg-det_CI.RData")

# Content:

#load("./plotting/cutoff_timber/sums_mg-det_cutoff_timber.RData")   
# results from the script: slost_aggregate.R
                # data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                #              sums - list of dataframes dataframe containing the sums over ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum, Sum_lower95, Sum_upper95
                #              tstep - vector with the time steps
                #              results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                #              sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  
                #              results_CI - list of dataframes containing the sums over ecoregions per each year considering only the CI values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                #              sums_CI - list of dataframes dataframe containing the sums over ecoregions per each year considering only the CI values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum_upper, Sum_lower  

#load("./plotting/areas.RData")   # results from the script: slost_aggregate.R
# data loaded for areas: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              

library(dplyr)    # dataframe management
library(tidyr)      # dataframe management
library(tidyverse)
library(abind)      # dataframe management
library(ggplot2)

#for areas:
#results_median <- results

# Create a df containing all the sums for all the years
#df = data.frame(matrix(NA, nrow=(nrow(sums[[1]])*length(sums)), ncol=(ncol(sums[[1]])+1)))
#df = data.frame(matrix(NA, nrow=(nrow(sums[[1]])*length(sums)), ncol=(ncol(sums[[1]])+1)))

df <- results_median[[1]][1,] %>%
        #unite("Group", Group:Forest_use, remove = TRUE) %>%
          rename(Scenario = Management) %>%
              select(Group, Scenario) 

for(i in 1:length(tstep)) {
  
  results_median[[i]]$Year <- toString(tstep[i])

  temp <- results_median[[i]] %>%
                    #unite("Group", Group:Forest_use, remove = TRUE) %>%
                      rename(Scenario = Management) %>% 
                        filter(Forest_use == "MFM") %>%
                          select(-Forest_use) %>%
                           transmute(Group = Group, Scenario = Scenario, Year = Year, 
                                    #Natural = rowSums(select(., contains("new"))), # for areas
                                    Afforestation = rowSums(select(., contains("Afforested"))), 
                                    Regrowth = rowSums(select(., contains("Regrowth"))), 
                                    Forest = rowSums(select(., starts_with("For"))), 
                                    Energy_crops_and_plantations = rowSums(select(., contains("EP"))), 
                                    Annual_crops = rowSums(select(., contains("Annual"))),
                                    Pasture = rowSums(select(., contains("Pasture"))), Permanent_crops = rowSums(select(., contains("Permanent"))),
                                    Urban = rowSums(select(., contains("Urban"))))
    
  df = df %>% bind_rows(temp)                     # bind columns together
  
}

 
df <- df[2:nrow(df),]
df <- df %>% 
  dplyr::filter(Year != "2000" & Year != "2010") %>%
   dplyr::filter(Scenario == "noAFM")

# change the format (before: one column per each land use class; after the change: one single column with al the values and one with the corresponding Category)
df <- df %>% 
    pivot_longer(cols = 4:(length(df)), names_to = "Category", values_to = "PDFx100")
    #pivot_longer(cols = 4:(length(df)), names_to = "Category", values_to = "Area")

write.csv(df, paste0(save_csv_path, "global_time-series", case, ".csv"), row.names = FALSE)
#write.csv(df, paste0("./plotting/data/global_time-series_areas.csv"), row.names = FALSE)

rm(df) 

  
  
 
