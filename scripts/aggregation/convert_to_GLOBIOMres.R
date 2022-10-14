

#############################################################################################################################################################################

# WARNING: The functions defined in this file are called in the file csv_and_plotting.R. The loading/saving paths, the libraries and the working directory are defined in that file.

#############################################################################################################################################################################


# General task: convert the results of the model (at ecoregion resolution) to the resolution of the GLOBIOM regions 
# Date: April 2021
# Author: Francesca Rosa

source("./scripts/data_preparation/areas_functions.R")

library(dplyr)    # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
#library(ggplot2)                         # plot
library(stringr)                          # string management


convert.to.GLOBIOMres <- function(results_path, csv_path, areas_base_path, file_label) {

  # load the files (each file corresponds to one year)
  temp = list.files(path = paste0(results_path) , pattern = paste0("*", file_label, ".csv"), full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
  myfiles = lapply(temp, read.csv)            # read the files, create a list where in each element is loaded one of the file as df
  rm(temp)
  
  tstep = seq(from = 2000, to = 2100, by = 10)    # time steps
  
  names(myfiles) = sapply(tstep, FUN = toString)  # rename each element of the list with the corresponding time step
  
  # select the median values and put the dataframes in a list (each element correspond to one year)
  results <- lapply(myfiles, function(x) select(x, Scenario, Ecoregion, Year, contains("median"))) 
  results_in <- lapply(results, function(x) data.frame(x))
  
  # convert the list into a single dataframe
  df <- reduce(results_in, full_join) %>%
          mutate(Year = as.character(Year)) 
  
  # classify according to GLIOBIOM region
  Globiom_eco_org <- read.csv(paste0(areas_base_path, "GLOBIOM_Ecoregion.csv"), header = TRUE)
  # rearrange data in Globiom_eco
    Globiom_eco_org <- filter(Globiom_eco_org, Ecoregion!="Lake", Ecoregion != "Rock and Ice") 
    Globiom_eco_org = droplevels(Globiom_eco_org, "Lake", "Rock and Ice")  #erase these two names from the list of factors
    
  df_regions <- join_regions(df, Globiom_eco_org)

  df_regions <- df_regions %>% 
                  group_by(Scenario, Globiom_Reg, Year) %>%       # groups by scenario, globiom region and year -> no ecoregion level anymore
                    summarise_if(is.numeric, sum, na.rm =TRUE) %>%
                      arrange(Scenario, Globiom_Reg) %>%
                        select(-Share) 
  
  df_regions <- data.frame(df_regions)
  
  ## test
    # test to check that the sum over all ecoregions per scenario and year remains the same after the reallocation to Globiom regions
    test_df <- data.frame(df %>% group_by(Scenario, Year) %>%
                    summarise_if(is.numeric, sum, na.rm = TRUE))
    test_df_regions <- data.frame(df_regions %>% group_by(Scenario, Year) %>%
                    summarise_if(is.numeric, sum, na.rm = TRUE))
    if (all.equal(test_df, test_df_regions) == FALSE) {stop("Error in the aggregation of impacts at Globiom level")}
    
    # test to check that 
    test_year = sample(tstep[8:11], 1)
    test_scenario = sample(unique(df$Scenario), 1)
    print(paste0("Test on year ", test_year, " and on scenario ", test_scenario))
    test1 = df_regions %>% filter(Year == "2100" & Scenario == test_scenario) %>% select(-Scenario, -Globiom_Reg, -Year)
    test2 = results_in[[11]] %>% filter(Year == "2100" & Scenario == test_scenario) %>% select(-Scenario, -Ecoregion, -Year) 
    if (abs(sum(test1, na.rm = TRUE) - sum(test2, na.rm = TRUE)) > 1e-8) {stop("Error in the aggregation of impacts at Globiom level")}

  rm(test_df, test_df_regions, test_year, test_scenario, test1, test2)  
  
  write.csv(df_regions, paste0(csv_path, "slost-globiom", file_label, ".csv"), row.names = FALSE)

}






