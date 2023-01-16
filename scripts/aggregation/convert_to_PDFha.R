
#############################################################################################################################################################################

# WARNING: The functions defined in this file are called in the file csv_and_plotting.R. The loading/saving paths, the libraries and the working directory are defined in that file.

#############################################################################################################################################################################

# General task: calculate the results (PDF) per ha 
# Date: April 2021
# Author: Francesca Rosa


library(dplyr)                            # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
library(stringr)                          # string management

convert.to.PDFha <- function(results_path, areas_processed_path, csv_path, file_label) {
  
  temp = list.files(path = paste0(results_path) , pattern = paste0("*", file_label, ".csv"), full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
      myfiles_1 = lapply(temp, read.csv)            # read the files, create a list where in each element is loaded one of the file as df
      rm(temp)
      
  temp = list.files(path = paste0(areas_processed_path, "disaggregated/") , pattern = paste0("*.csv"), full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
      myfiles_2 = lapply(temp, read.csv)            # read the files, create a list where in each element is loaded one of the file as df
      rm(temp)
      
  tstep = seq(from = 2000, to = 2100, by = 10)    # time steps
  for_management = c("noAFM", "AFMfree", "AFM25", "AFM50", "AFM75", "AFM100") # new names for management scenarios
  
  names(myfiles_1) = sapply(tstep, FUN = toString)  # rename each element of the list with the corresponding time step
  names(myfiles_2) = sapply(tstep, FUN = toString)  # rename each element of the list with the corresponding time step
  
  # select only median values
  PDF <- lapply(myfiles_1, function(x) select(x, Scenario, Ecoregion, Year, contains("median"))) 
  
  # remove columns that do not have a corresponding value in PDF
  Areas <- lapply(myfiles_2, function(x) select(x, -contains("new"), -contains("org"))) 
  
  for (i in 1:length(Areas)) {Areas[[i]]$Year = as.character(tstep[i])}
  
  # convert the elements of the list to a single dataframe                
  df_PDF <- reduce(PDF, full_join)
  df_Areas <- reduce(Areas, full_join)
  df_Areas <- df_Areas %>% select(Scenario:Ecoregion, Year, (3:length(df_Areas) - 1)) %>%
                rename(For_Plantation_im = For_PlantationFuel_im) %>%
                  mutate(For_Plantation_im = For_Plantation_im + For_TimberPlant_im) %>%
                    select(-For_TimberPlant_im)
  df_Areas$Year <- as.integer(df_Areas$Year)
  
  Ecoregions <- df_PDF %>% select(Year, Ecoregion) %>% distinct()
  Ecoregions$Ecoregion <- as.factor(Ecoregions$Ecoregion)
  Scenarios <- unique(df_Areas$Scenario)
  nscenarios <- length(Scenarios)
  
  df_Areas_new <- data.frame(matrix(NA, nrow = 0, ncol = 0)) 
  # the current df_Areas dataframe contains only the ecoregions considered in GLOBIOM, therefore we need to add 
  # those which are in the df_PDF and not in df_Areas.
  
      for(sc in 1:nscenarios) {
        
        print(sc)
        Areas_temp <- df_Areas %>% filter(Scenario == toString(Scenarios[sc])) %>%
            right_join(Ecoregions) %>%
              mutate(Ecoregion = as.character(Ecoregion)) %>%
                arrange(Ecoregion) %>%
                  mutate(Ecoregion = as.factor(Ecoregion)) %>%
                    mutate(Scenario = toString(Scenarios[sc]))

        df_Areas_new <- df_Areas_new %>% bind_rows(Areas_temp)
           
         } 

  df_Areas <- df_Areas_new
  rm(df_Areas_new)
  df_PDF <- df_PDF %>% arrange(Scenario, Ecoregion, Year)
  df_Areas <- df_Areas %>% arrange(Scenario, Ecoregion, Year) 
  
  # ecoreg_in_Globiom <- unique(df_Areas$Ecoregion)
  
  # df_PDF <- df_PDF %>% filter(Ecoregion %in% ecoreg_in_Globiom)
  
  ratio <- df_PDF
  ratio <- bind_cols(ratio[1:3], ratio[4:length(ratio)]/df_Areas[4:length(df_Areas)]) %>%
              replace(is.na(.), 0) 
  
          # test ====
                    row_test <- sample(8:nrow(ratio), 1)
                    test = ratio[row_test,]
                    test_check1 = df_PDF[row_test,]
                    test_check2 = df_Areas[row_test,]
                    test_check3 <- test_check1[4:length(test_check1)]/test_check2[4:length(test_check2)]
                           test_check3[is.na(test_check3)] <- 0

                    if ((abs(test$Annual_EU_median - test_check3$Annual_EU_median) > 1e-16) |
                          (abs(test$Permanent_EU_median - test_check3$Permanent_EU_median) > 1e-16) |
                          (abs(test$Pasture_EU_median -  test_check3$Pasture_EU_median) > 1e-16) |
                          (abs(test$Urban_EU_median - test_check3$Urban_EU_median) > 1e-16) |
                          (abs(test$Annual_RoW_median - test_check3$Annual_RoW_median) > 1e-16) |
                          (abs(test$Permanent_RoW_median - test_check3$Permanent_RoW_median) > 1e-16) |
                          (abs(test$Pasture_RoW_median - test_check3$Pasture_RoW_median) > 1e-16) |
                          (abs(test$Urban_RoW_median - test_check3$Urban_RoW_median) > 1e-16) |
                          (abs(test$For_ClearCut_EU_median - test_check3$For_ClearCut_EU_median) > 1e-16) |
                          (abs(test$For_ClearCut_im_median - test_check3$For_ClearCut_im_median) > 1e-16) |
                          (abs(test$For_ClearCut_ex_median - test_check3$For_ClearCut_ex_median) > 1e-16) |
                          (abs(test$For_Plantation_im_median - test_check3$For_Plantation_im_median) > 1e-16) |
                          (abs(test$For_Retention_EU_median - test_check3$For_Retention_EU_median) > 1e-16) &
                          (abs(test$For_TimberPlant_EU_median - test_check3$For_TimberPlant_EU_median) > 1e-16) |
                          (abs(test$For_TimberPlant_ex_median - test_check3$For_TimberPlant_ex_median) > 1e-16) |
                          (abs(test$ForOther_Extensive_EU_median - test_check3$ForOther_Extensive_EU_median) > 1e-16) |
                          (abs(test$ForOther_Extensive_RoW_median - test_check3$ForOther_Extensive_RoW_median) > 1e-16) |
                          (abs(test$ForOther_Intensive_EU_median - test_check3$ForOther_Intensive_EU_median) > 1e-16) |
                          (abs(test$ForOther_Intensive_RoW_median - test_check3$ForOther_Intensive_RoW_median) > 1e-16) |
                          (abs(test$For_Selective_im_median - test_check3$For_Selective_im_median) > 1e-16) |
                          (abs(test$For_ReducedImpactLogging_im_median - test_check3$For_ReducedImpactLogging_im_median) > 1e-16) |
                          (abs(test$For_SelectionSystem_EU_median - test_check3$For_SelectionSystem_EU_median) > 1e-16) |
                          (abs(test$EP_EU_median - test_check3$EP_EU_median) > 1e-16) |
                          (abs(test$EP_conv_EU_median - test_check3$EP_conv_EU_median) > 1e-16) |
                          (abs(test$EP_RoW_median - test_check3$EP_RoW_median) > 1e-16) |
                          (abs(test$EP_conv_im_median - test_check3$EP_conv_im_median) > 1e-16) |
                          (abs(test$Afforested_EU_median - test_check3$Afforested_EU_median) > 1e-16) |
                          (abs(test$Afforested_RoW_median - test_check3$Afforested_RoW_median) > 1e-16) |                        
                          (abs(test$Regrowth_median - test_check3$Regrowth_median) > 1e-16))
                           
                    {stop("ERROR in the calculation of PDF/ha")}

                    rm(test, test_check1, test_check2, test_check3)
          # ====
  
  write.csv(ratio, paste0(csv_path, "PDF_ha.csv"), row.names = FALSE)
   
  ratio_tot <- df_PDF
  df_PDF_sum <- data.frame(df_PDF %>% select(-Ecoregion) %>%
                group_by(Scenario, Year) %>% 
                  summarise_all(sum, na.rm = TRUE))
  
  df_Areas_sum <- data.frame(df_Areas %>% select(-Ecoregion) %>%
                group_by(Scenario, Year) %>% 
                  summarise_all(sum, na.rm = TRUE))
  
  ratio_tot <- data.frame(bind_cols(df_PDF_sum[1:3], df_PDF_sum[4:length(df_PDF_sum)]/df_Areas_sum[4:length(df_Areas_sum)]) %>%
              replace(is.na(.), 0)) 
                    
  write.csv(ratio_tot, paste0(csv_path, "PDF_ha_tot.csv"), row.names = FALSE)

}
  
  

