

library(dplyr)    # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
library(stringr)                          # string management

convert.PDF.ha <- function(folder_slost, folder_areas, csv_path, case_subcase) {
  
  temp = list.files(path = paste0(folder_slost) , pattern = paste0("*", case_subcase, ".csv"), full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
      myfiles_1 = lapply(temp, read.csv)            # read the files, create a list where in each element is loaded one of the file as df
      rm(temp)
      
  temp = list.files(path = paste0(folder_areas) , pattern = paste0("*.csv"), full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
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
  
  df_Areas_new <- data.frame(matrix(NA, nrow = 0, ncol = 0)) 
  # the current df_Areas dataframe contains only the ecoregions considered in GLOBIOM, therefore we need to add 
  # those which are in the df_PDF and not in df_Areas.
  
  Ecoregions <- df_PDF %>% select(Ecoregion) 
  Scenarios <- unique(df_PDF$Scenario)
  nscenarios <- length(Scenarios)
  
      for(sc in 1:nscenarios) {
                                                                     
        Areas_temp <- df_Areas %>% filter(Scenario == toString(Scenarios[sc])) %>%
            full_join(Ecoregions) %>%
              mutate(Ecoregion = as.character(Ecoregion)) %>%
                arrange(Ecoregion) %>%
                  mutate(Ecoregion = as.factor(Ecoregion)) %>%
                    mutate(Scenario = toString(Scenarios[sc]))

        df_Areas_new <- df_Areas_new %>% bind_rows(Areas_temp)
           
         } 

  df_PDF <- df_PDF %>% arrange(Scenario, Ecoregion, Year)
  df_Areas <- df_Areas %>% arrange(Scenario, Ecoregion, Year) 
  
  ecoreg_in_Globiom <- unique(df_Areas$Ecoregion)
  
  df_PDF <- df_PDF %>% filter(Ecoregion %in% ecoreg_in_Globiom)
  
  ratio <- bind_cols(ratio[1:3], ratio[4:length(ratio)]/df_Areas[4:length(df_Areas)]) %>%
              replace(is.na(.), 0) 
  
          # test ====
                    row_test <- sample(8:nrow(ratio), 1)
                    test = ratio[row_test,]
                    test_check1 = df_PDF[row_test,]
                    test_check2 = df_Areas[row_test,]
                    
                    if ((!is.na(abs(test$For_ClearCut_EU_median - (test_check1$For_ClearCut_EU_median/test_check2$For_ClearCut_EU))) &
                      (abs(test$For_ClearCut_EU_median - (test_check1$For_ClearCut_EU_median/test_check2$For_ClearCut_EU)) > 1e-16)) |
                        (!is.na(abs(test$For_ClearCut_ex_median - (test_check1$For_ClearCut_ex_median/test_check2$For_ClearCut_ex))) &
                         (abs(test$For_ClearCut_ex_median - (test_check1$For_ClearCut_ex_median/test_check2$For_ClearCut_ex)) > 1e-16)) |
                        (!is.na(abs(test$For_ClearCut_im_median - (test_check1$For_ClearCut_im_median/test_check2$For_ClearCut_im))) &
                         (abs(test$For_ClearCut_im_median - (test_check1$For_ClearCut_im_median/test_check2$For_ClearCut_im)) > 1e-16)) |
                        (!is.na(abs(test$For_Retention_EU_median - (test_check1$For_Retention_EU_median/test_check2$For_Retention_EU))) &
                         (abs(test$For_Retention_EU_median - (test_check1$For_Retention_EU_median/test_check2$For_Retention_EU)) > 1e-16))|
                        (!is.na(abs(test$For_PlantationFuel_im_median - (test_check1$For_PlantationFuel_im_median/test_check2$For_PlantationFuel_im))) & 
                         (abs(test$For_Plantation_im_median - (test_check1$For_Plantation_im_median/test_check2$For_Plantation_im)) > 1e-16)) |
                        (!is.na(abs(test$For_SelectionSystem_EU_median - (test_check1$For_SelectionSystem_EU_median/test_check2$For_SelectionSystem_EU))) &
                         (abs(test$For_SelectionSystem_EU_median - (test_check1$For_SelectionSystem_EU_median/test_check2$For_SelectionSystem_EU)) > 1e-16)) |
                        (!is.na(abs(test$EP_EU_median - (test_check1$EP_EU_median/test_check2$EP_EU))) & 
                         (abs(test$EP_EU_median - (test_check1$EP_EU_median/test_check2$EP_EU)) > 1e-16))|
                        (!is.na(abs(test$EP_conv_EU_median - (test_check1$EP_conv_EU_median/test_check2$EP_conv_EU))) &
                         (abs(test$EP_conv_EU_median - (test_check1$EP_conv_EU_median/test_check2$EP_conv_EU)) > 1e-16))) 
                    {stop("ERROR in the calculation of PDF/ha")}
              
          # ====
  
  write.csv(ratio, paste0(csv_path, "PDF-ha.csv"), row.names = FALSE)

}
  
  

