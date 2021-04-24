

# EU IMPACT, BOTH EU FOOTPRINT AND INTERNAL IMPACTS, IN GIVEN YEARS

# Task: Produce two .csv that contain the impacts in terms of species lost (PDF) per forest use group or category 
        # (forest categories are grouped/summed according to the scope, either EU footprint or EU internal impacts) 
        # for all years 

# Date: September 2020
# Author: Francesca Rosa

# WARNING: run the file load_and_save.R before running this one. load_and_save.R allows you to choose the .Rdata file to load and the location where the .csv files are saved

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
library(tidyverse)
library(abind)      # dataframe management
library(ggplot2)

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
        test_check$For_ClearCut_im_median + test_check$For_PlantationFuel_im_median)) > 1e-16) 
     {stop("ERROR in EU Footprint")}
   
   rm(test, test_check)
   # ====
   

  df = df %>% bind_rows(temp)                     # bind columns together
}

df <- df[2:nrow(df),]
df <- df %>% 
        dplyr::filter(Year != "2000" & Year != "2010") %>%
          arrange(Year)


write.csv(df, paste0(save_csv_path, "EUFootprint_time-series", case, "_CI.csv"), row.names = FALSE)

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
                                                                        (contains("ForOther") & contains("EU"))))),
            lower95 = rowSums(dplyr::select(., contains("lower") &  ((contains("ClearCut") & (contains("EU")|contains("ex")))|
                                                                        (contains("Retention") & (contains("EU")|contains("ex")))|
                                                                        (contains("Selection") & (contains("EU")|contains("ex")))|
                                                                        (contains("ForOther") & contains("EU"))))),
            upper95 = rowSums(dplyr::select(., contains("upper") &  ((contains("ClearCut") & (contains("EU")|contains("ex")))|
                                                                        (contains("Retention") & (contains("EU")|contains("ex")))|
                                                                        (contains("Selection") & (contains("EU")|contains("ex")))|
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


write.csv(df, paste0(save_csv_path, "EUForest_ClearCut-noex_time-series", case, "_CI.csv"), row.names = FALSE)

rm(df, temp)


