
# General task: convert the outputs of the model (.Rdata) into .csv files grouping and selecting only the most relevant 
# Date: September 2020
# Author: Francesca Rosa

# 1) Impacts for all scenarios for a give year or as time series (both grouped and disaggregated impacts)
# 2) Total impacts under the noAF scenario

#############################################################################################################################################################################

# WARNING: The functions defined in this file are called in the file csv_and_plotting.R. The loading/saving paths, the libraries and the working directory are defined in that file.

#############################################################################################################################################################################



#############################################################################################################################################################################

                                                                  # 1) IMPACTS OF GLOBAL LAND USE #

#############################################################################################################################################################################


  
  create.csv.global <- function(CI, file_rdata_path, csv_path, file_label) {
    
    load(file_rdata_path)  
  
  ##################################################### IMPACTS FOR TWO GIVEN YEARS (GROUPING OF LAND USE CATEGORIES) ##################################################### 
  
  # GLOBAL IMPACTS 
  
  # Task: Produce six .csv that contain the impacts in terms of species lost (PDF) (three for 2050 and three for 2100)
          # 1) per scenario and summed over all land use categories 
          # 2) per scenario and per land use group (impacts of land use categories of the same group are summed, e.g. the impacts of all forest land use categories are summed together). 
          # 3) per scenario and per all land use categories (no grouping)
  
  # Date: September 2020
  # Author: Francesca Rosa
  
  
  # Focus on 2050 and 2100
  t = c("2050", "2100")
  
  for (i in 1:length(t)) {
    
    year  = t[i]            # select the year
    
  ################## 1) TOTAL SUMS #######################
  
    # prepare and save the file with the total sums for the given year 
        
        sums_oneyear = sums[[year]] %>% 
          unite("Group", Group:Forest_use, remove = TRUE) %>%
            rename(Scenario = Management, PDFx100 = Sum_median, lower95 = Sum_lower95, upper95 = Sum_upper95) 
            
          if (CI == TRUE) { write.csv(sums_oneyear, paste0(csv_path, "global_", year, file_label, "_top_CI.csv"), row.names = FALSE) 
            } else if (CI == FALSE) { write.csv(sums_oneyear, paste0(csv_path, "global_", year, file_label, "_top.csv"), row.names = FALSE) }
            # this csv has the following columns: 
            # Group (climatic and forest use scenario), Scenario (forest management scenario), PDFx100 (impact), lower95 (lower CI interval), upper95 (upper CI interval)
            # If CI == FALSE, the columns lower95 and upper95 are all 0
        
  
        
  ################## 2) SUMS PER LAND USE GROUP #######################
        
        
      # aggregate the land use classes in: Afforestation, Annual_crops, Energy_crops_and_plantations, Forest, Pasture, Permanent_crops, Urban
        results_temp <- results_median[[year]] %>%
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
        
                # test ====
                  
                    test = results_temp[nrow(results_temp),]
                    test_check = results[[year]][nrow(results_temp),]
                  
                        if ((abs((test$Afforestation - (test_check$Afforested_EU_median + test_check$Afforested_RoW_median))) > 1e-15) |
                            (abs((test$Regrowth - test_check$Regrowth_median)) > 1e-15) |
                        (abs(test$Annual_crops - (test_check$Annual_EU_median + test_check$Annual_RoW_median)) > 1e-15) |
                        (abs(test$Energy_crops_and_plantations - (test_check$EP_EU_median + test_check$EP_conv_EU_median + test_check$EP_RoW_median + test_check$EP_conv_im_median)) > 1e-14) |
                        (abs(test$Forest - (test_check$For_ClearCut_EU_median +  test_check$For_ClearCut_im_median + test_check$For_ClearCut_ex_median +
                          test_check$For_Retention_EU_median + test_check$For_SelectionSystem_EU_median + test_check$For_Plantation_im_median + test_check$ForOther_Extensive_EU_median +
                          test_check$ForOther_Extensive_RoW_median + test_check$ForOther_Intensive_EU_median + test_check$ForOther_Intensive_RoW_median)) > 1e-15) |
                        (abs(test$Pasture - (test_check$Pasture_EU_median + test_check$Pasture_RoW_median)) > 1e-15) |
                        (abs(test$Permanent_crops - (test_check$Permanent_EU_median + test_check$Permanent_RoW_median)) > 1e-15)  |
                        (abs(test$Urban - (test_check$Urban_EU_median + test_check$Urban_RoW_median)) > 1e-15 )) 
                          {stop("ERROR in the aggregation of land use at global level")}
                  
                    # ====
        
        write.csv(results_oneyear, paste0(csv_path, "global_", year, file_label, ".csv"), row.names = FALSE)
        # this csv has the following columns: 
        # Group (climatic and forest use scenario), Scenario (forest management scenario), Category (land use category), PDFx100 (impact)
    
        rm(results_temp, results_oneyear)
    
  ################## 3) NO GROUPING, DISAGGREGATED RESULTS #######################
  
    
          results_temp <- results_median[[year]] %>%
                        unite("Group", Group:Forest_use, remove = TRUE) %>%
                          rename(Scenario = Management) 
          
          write.csv(results_temp, paste0(csv_path, "global_tot-disaggr_", year, file_label, ".csv"), row.names = FALSE)
          
          rm(results_temp)
  
              
          }
  
  
  
  ##################################################### TIME SERIES OF IMPACTS ##################################################### 
    
    # TIME SERIES (WITH CI)
                      
    # Task: Produce one .csv that contains the impacts in terms of species lost (PDF) 
          # per scenario and per land use group (impacts of land use categories of the same group are summed, e.g. the impacts of all forest land use categories are summed together). 
          
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
        rm(temp)    
      }
      
       
      df <- df[2:nrow(df),]
      df <- df %>% 
        dplyr::filter(Year != "2000" & Year != "2010") %>%
         dplyr::filter(Scenario == "noAFM")
      
      # change the format (before: one column per each land use class; after the change: one single column with al the values and one with the corresponding Category)
      df <- df %>% 
          pivot_longer(cols = 4:(length(df)), names_to = "Category", values_to = "PDFx100")
          #pivot_longer(cols = 4:(length(df)), names_to = "Category", values_to = "Area")
      
      write.csv(df, paste0(csv_path, "global_time-series", file_label, ".csv"), row.names = FALSE)
      #write.csv(df, paste0("./plotting/data/global_time-series_areas.csv"), row.names = FALSE)
      
      rm(df) 
      
  }
  
  
  
#############################################################################################################################################################################

                                                 # 2) IMPACTS OF GLOBAL LAND USE (NO ALTERNATIVE FOREST MANAGEMENT SCENARIOS) #
  
#############################################################################################################################################################################
  
  create.csv.noAF <- function(file_rdata_path, csv_path, file_label) {
    
    load(file_rdata_path)  
    
  #sums = sums_CI 
  
  # Create a df containing all the sums for all the years
  #df = data.frame(matrix(NA, nrow=(nrow(sums[[1]])*length(sums)), ncol=(ncol(sums[[1]])+1)))
  df <- sums[[1]][1,]
  
  for(i in 1:length(tstep)) {
    
    sums[[i]]$Year <- toString(tstep[i])
    df = df %>% bind_rows(sums[[i]])                     # bind columns together
  }
  
  df <- df[2:nrow(df),]
  df <- df %>% 
          rename(PDFx100 = Sum_median, upper95 = Sum_upper95, lower95 = Sum_lower95) %>%
            dplyr::filter(Year != "2000" & Year != "2010") %>%
              filter((Group == "REF" | Group == "RCP2.6") & (Forest_use == "MFM") & (Management == "noAFM")) %>% 
                dplyr::select(-Forest_use, -Management) %>%
                  arrange(Year)
  
  
  write.csv(df, paste0(csv_path, "noAF_global_CI", file_label, ".csv"), row.names = FALSE)
  
  }
  
  
  
  
