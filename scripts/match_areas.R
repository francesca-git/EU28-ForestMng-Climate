
# Title: Match the areas
# Task: Take as input the areas tided up in tidy_areas.R and match the different datasets 
# Author: Francesca Rosa
# Date: started in March 2020
# unit of the output data: Mha


setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation")
source("./scripts/areas_functions.R")      # R file containing the function used in this script
#options(warn=0)

#install.packages("rgdal")
#install.packages("tidyverse")
#install.packages("compare")
library(rgdal)
library(dplyr)
library(tidyr)
library(compare)
library(readr)
library(purrr)
library(tibble)
library(stringr)

timber = "ON" # "OFF" = excluding timber plantations from EU forest managements, "ON" = including timber plantations in EU forest managements for the sensitivity analysis

if (timber == "OFF") { load("./results/areas/areas-to-match.RData") # all areas corresponding to timber plantations are 0 Mha
} else if(timber == "ON") {load("./results/areas/areas-to-match_timber.RData") } # Here we consider that part of the areas covered by clear cut in EU are allocated to Timber plantations.
                                                                      # therefore, after all the allocation processes, the column containing data on Timber plantations is subtracted from Clear cut  

# the file uploaded here above contains several elements:  
  # - data.list.steps - list whose elements are the dataframes containing the areas of the different land use types (Mha), considering both the marginal and average approach: "Import_forest_av", "Import_forest_mg",  
      # "Import_energy_plant", "EU_forest", "EU_energy_plant", "Export_forest_av", "Export_forest_mg", "Broad_land_use", "Forest_intensity"
  # - ecoregions_in_Globiom - dataframe containing the codes of the ecoregions included in the GLOBIOM model
  # - EU_ecoregions - dataframe containing the codes of EU ecoregions 
  # - Globiom_eco_EU - dataframe containing the codes of EU ecoregions, the name of the corresponding Globiom region ("EU") and the share of these ecoregions set to 1, in order to use it later with the Neg_allocation function
  # - Globiom_eco_org - dataframe containing the codes of all ecoregions, the name of the corresponding Globiom regiona and the share of these ecoregions per Globiom region 
  # - urban - dataframe with the extension of urban areas (Mha) per ecoregion 

  ntstep <- length(data.list.steps[[1]]) # number of time steps
  tstep <- seq(from = 2000, to = 2100, by = 10) # years corresponding to each time step

#################################################################################
### PREPARATION OF THE AREAS SUCH THAT THEY CAN BE USED AS INPUT FOR THE MODEL 
#################################################################################                        
  
  
  
            ################################################## CREATE THE EMPTY LISTS TO BE FILLED BY THE AREAS ##################################################

  
    # vectors of names useful to apply the functions for the allocation =============
      Forests = c("Extensive", "Intensive", "Regrowth")
      EPconverted_EU <- c("EP_CrpLnd_EU", "EP_GrsLnd_EU", "EP_NatLnd_EU")
      EPtoconvert_EU = c("CrpLnd_EU", "GrsLnd_EU", "NatLnd_EU")
      EPconverted_RoW <- c("EP_CrpLnd_im", "EP_GrsLnd_im", "EP_NatLnd_im")
      EPtoconvert_RoW = c("CrpLnd_RoW", "GrsLnd_RoW", "NatLnd_RoW")
 
      areas.av.in <- list()
      areas.mg.in <- list()
      areas.av.fin <- list()
      areas.mg.fin <- list()
      areas.av <- list()
      areas.mg <- list()
      areas.av.fin.full <- list()
      areas.mg.fin.full <- list()
      
      

      
  for (i in 1:ntstep) {

    print(paste0("Time step: ", tstep[i]))
    
     # Comments on the following steps are in the section of code on the average approach
    
            ################################################## 1) CREATE A SINGLE DATAFRAME ##################################################

   # Create a single dataframe which combines all the input data
    
      areas_mg <- data.list.steps[["Import_forest_mg"]][[i]] %>% 
              full_join(data.list.steps[["Import_energy_plant"]][[i]]) %>% 
                full_join(data.list.steps[["EU_forest"]][[i]]) %>% 
                  full_join(data.list.steps[["EU_energy_plant"]][[i]]) %>%
                    full_join(data.list.steps[["Export_forest_mg"]][[i]]) %>%
                      full_join(data.list.steps[["Broad_land_use"]][[i]]) %>%
                        inner_join(ecoregions_in_Globiom) %>%
                          unite("Scenario", Climate:Management, remove = TRUE) %>% 
                            arrange(Scenario, Ecoregion)
                              areas_mg[is.na(areas_mg)] <- 0
                                areas_mg$Scenario <- as.factor(areas_mg$Scenario) 
                                  areas_mg$Ecoregion <- as.factor(areas_mg$Ecoregion) 
                                                  
      areas_av <- data.list.steps[["Import_forest_av"]][[i]] %>% 
                    full_join(data.list.steps[["Import_energy_plant"]][[i]]) %>% 
                      full_join(data.list.steps[["EU_forest"]][[i]]) %>% 
                        full_join(data.list.steps[["EU_energy_plant"]][[i]]) %>%
                          full_join(data.list.steps[["Export_forest_av"]][[i]]) %>%
                            full_join(data.list.steps[["Broad_land_use"]][[i]]) %>%
                              inner_join(ecoregions_in_Globiom) %>%                         # intersects the ecoregions available in Globiom with the ones in the other land use files  
                                unite("Scenario", Climate:Management, remove = TRUE) %>%    # creates one single column merging the information about scenarios (climate, management, forest use)
                                  arrange(Scenario, Ecoregion)
                                    areas_av[is.na(areas_av)] <- 0                          # sets to 0 the cells from the EU datasets which do not have corresponding values in ecoregions outside EU
                                      areas_av$Scenario <- as.factor(areas_av$Scenario)     # keeps the Scenario column as factor
                                        areas_av$Ecoregion <- as.factor(areas_av$Ecoregion) # keeps the Ecoregion column as factor    
                                        
                                        
            ################################################## 2) START MATCHING THE DATASETS ##################################################
       

    #2) Subtract the subset areas of the submodel from the base model, remove of Wetlands and Not relevant lands, aggregate columns belonging to the same land use category, add urban 
                                        
      areas.mg_in <- areas_mg %>%
                      mutate(CrpLnd_EU = CrpLnd_EU - EP_CrpLnd_EU, CrpLnd_RoW = CrpLnd_RoW - EP_CrpLnd_im, 
                             GrsLnd_EU = GrsLnd_EU - EP_GrsLnd_EU, GrsLnd_RoW = GrsLnd_RoW - EP_GrsLnd_im,
                             NatLnd_EU = NatLnd_EU - EP_NatLnd_EU, NatLnd_RoW = NatLnd_RoW - EP_NatLnd_im) %>%
                        # mutate(MngFor = MngFor - Clear_cut_im - Clear_cut_EU - Selection_EU - Retention_EU - Plantation_im) %>%
                        mutate(MngFor = MngFor - Clear_cut_im - Plantation_im) %>%
                          select(-WetLnd, -NotRel) %>%
                            inner_join(urban) %>%
                              mutate(sum_EUfor = Clear_cut_EU + Selection_EU + Retention_EU)
                                areas.mg_in$Ecoregion <- as.factor(areas.mg_in$Ecoregion)
                                
      areas.av_in <- areas_av %>%
                      mutate(CrpLnd_EU = CrpLnd_EU - EP_CrpLnd_EU, CrpLnd_RoW = CrpLnd_RoW - EP_CrpLnd_im,            # broad land use classes
                             GrsLnd_EU = GrsLnd_EU - EP_GrsLnd_EU, GrsLnd_RoW = GrsLnd_RoW - EP_GrsLnd_im,            # these subtractions produce negative values in some ecoregions
                             NatLnd_EU = NatLnd_EU - EP_NatLnd_EU, NatLnd_RoW = NatLnd_RoW - EP_NatLnd_im) %>%
                        # mutate(MngFor = MngFor - Clear_cut_im - Clear_cut_EU - Selection_im - Selection_EU - Selective_im - Retention_EU - Plantation_im) %>%
                        mutate(MngFor = MngFor - Clear_cut_im - Selection_im - Selective_im - Plantation_im) %>%      # these subtractions and those occurring in the Subset_allocation function produce negative values in some ecoregions
                          select(-WetLnd, -NotRel) %>%
                            inner_join(urban) %>%
                              mutate(sum_EUfor = Clear_cut_EU + Selection_EU + Retention_EU)
                                areas.av_in$Ecoregion <- as.factor(areas.av_in$Ecoregion)
      
      
          # test =====
            # Check CrpLnd, GrsLnd and NatLnd in EU ====
            
              scenario_test = "RCP_MFM_AF100"
              ecoregion_test = "PA0401"                                            
              
              test <- areas.mg_in %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              test_check <- areas_mg %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              
              if((test$CrpLnd_EU != test_check$CrpLnd_EU - test_check$EP_CrpLnd_EU) | (test$GrsLnd_EU != test_check$GrsLnd_EU - test_check$EP_GrsLnd_EU) 
                 | (test$NatLnd_EU != test_check$NatLnd_EU - test_check$EP_NatLnd_EU) | (test$sum_EUfor != test_check$Clear_cut_EU + test_check$Selection_EU + test_check$Retention_EU)) {
                stop("ERROR in section 2 - marginal - energy plantations or forests in EU")
              }
              
              rm(test, test_check)
              
              test <- areas.av_in %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              test_check <- areas_av %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              
              if((test$CrpLnd_EU != test_check$CrpLnd_EU - test_check$EP_CrpLnd_EU) | (test$GrsLnd_EU != test_check$GrsLnd_EU - test_check$EP_GrsLnd_EU) 
                 | (test$NatLnd_EU != test_check$NatLnd_EU - test_check$EP_NatLnd_EU) | (test$sum_EUfor != test_check$Clear_cut_EU + test_check$Selection_EU + test_check$Retention_EU)) {
                stop("ERROR in section 2 - average - energy plantations or forests in EU")
              }
              
              rm(test, test_check)
              
            # Check CrpLnd, GrsLnd and NatLnd outside EU =====
            
              scenario_test = "RCP_SFM_AF100"
              ecoregion_test = "IM0143"                                            
              
              test <- areas.mg_in %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              test_check <- areas_mg %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              
              if((test$CrpLnd_RoW != test_check$CrpLnd_RoW - test_check$EP_CrpLnd_im) | (test$GrsLnd_RoW != test_check$GrsLnd_RoW - test_check$EP_GrsLnd_im) | (test$NatLnd_RoW != test_check$NatLnd_RoW - test_check$EP_NatLnd_im)) {
                stop("ERROR in section 2 - marginal - energy plantation import")
              }
              
              rm(test, test_check)
              
              test <- areas.av_in %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              test_check <- areas_av %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              
              if((test$CrpLnd_RoW != test_check$CrpLnd_RoW - test_check$EP_CrpLnd_im) | (test$GrsLnd_RoW != test_check$GrsLnd_RoW - test_check$EP_GrsLnd_im) | (test$NatLnd_RoW != test_check$NatLnd_RoW - test_check$EP_NatLnd_im)) {
                stop("ERROR in section 2 - average - energy plantation import")
              }
            
            # Check MngFor ====
            
              test <- areas.mg_in %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              test_check <- areas_mg %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              
              if(abs(test$MngFor - (test_check$MngFor - test_check$Clear_cut_im - test_check$Plantation_im)) > 1e-10) {
                stop("ERROR in section 2 - marginal - managed forests")
              }
              
              rm(test, test_check)
              
              test <- areas.av_in %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              test_check <- areas_av %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
              
              if(abs(test$MngFor - (test_check$MngFor - test_check$Clear_cut_im - test_check$Plantation_im - test_check$Selection_im - test_check$Selective_im)) > 1e-10) {
                stop("ERROR in section 2 - average - managed forests")
              }
              
              rm(test, test_check)
              
            
          # ========                                                                                   
                                

      
      # Grasslands in GLOBIOM areas do not include pastures and rangelands, which are accounted in NatLnd. Therefore, part of NatLnd is allocated to GrsLnd according to FAO data ====
        
        # marginal 
        areas.mg_test <- areas.mg_in
        areas.mg_in[areas.mg_in$NatLnd_EU > 0, "GrsLnd_EU"] <- areas.mg_in[areas.mg_in$NatLnd_EU > 0, "GrsLnd_EU"] + areas.mg_in[areas.mg_in$NatLnd_EU > 0, "NatLnd_EU"]*0.51
        areas.mg_in[areas.mg_in$NatLnd_EU > 0, "NatLnd_EU"] <- areas.mg_in[areas.mg_in$NatLnd_EU > 0, "NatLnd_EU"] - areas.mg_in[areas.mg_in$NatLnd_EU > 0, "NatLnd_EU"]*0.51
        areas.mg_in[areas.mg_in$NatLnd_RoW > 0, "GrsLnd_RoW"] <- areas.mg_in[areas.mg_in$NatLnd_RoW > 0, "GrsLnd_RoW"] + areas.mg_in[areas.mg_in$NatLnd_RoW > 0, "NatLnd_RoW"]*0.51
        areas.mg_in[areas.mg_in$NatLnd_RoW > 0, "NatLnd_RoW"] <- areas.mg_in[areas.mg_in$NatLnd_RoW > 0, "NatLnd_RoW"] - areas.mg_in[areas.mg_in$NatLnd_RoW > 0, "NatLnd_RoW"]*0.51
        
        #average
        areas.av_test <- areas.av_in
        areas.av_in[areas.av_in$NatLnd_EU > 0, "GrsLnd_EU"] <- areas.av_in[areas.av_in$NatLnd_EU > 0, "GrsLnd_EU"] + areas.av_in[areas.av_in$NatLnd_EU > 0, "NatLnd_EU"]*0.51
        areas.av_in[areas.av_in$NatLnd_EU > 0, "NatLnd_EU"] <- areas.av_in[areas.av_in$NatLnd_EU > 0, "NatLnd_EU"] - areas.av_in[areas.av_in$NatLnd_EU > 0, "NatLnd_EU"]*0.51
        areas.av_in[areas.av_in$NatLnd_RoW > 0, "GrsLnd_RoW"] <- areas.av_in[areas.av_in$NatLnd_RoW > 0, "GrsLnd_RoW"] + areas.av_in[areas.av_in$NatLnd_RoW > 0, "NatLnd_RoW"]*0.51
        areas.av_in[areas.av_in$NatLnd_RoW > 0, "NatLnd_RoW"] <- areas.av_in[areas.av_in$NatLnd_RoW > 0, "NatLnd_RoW"] - areas.av_in[areas.av_in$NatLnd_RoW > 0, "NatLnd_RoW"]*0.51
        
      areas.mg.in[[toString(tstep[i])]] <- areas.mg_in
      areas.av.in[[toString(tstep[i])]] <- areas.av_in
          
                                                     
          ################################################## 3) MAKE THE ALLOCATION ##################################################

    #3) Make all necessary allocations
      
        # MG - Main allocation processes ====== 
          areas.mg_in = Subset_allocation(areas.mg_in, areas.mg.in[["2000"]], "MngFor", "sum_EUfor", "PriFor", c("NaN", "MFM", "noAF"))                                                                                                   
            areas_neg_alloc = Neg_allocation(areas.mg_in, Globiom_eco_org, "MngFor")
              areas_for_alloc = For_allocation(areas_neg_alloc, Globiom_eco_org, "MngFor", data.list.steps[["Forest_intensity"]][[i]], Forests)
                areas_EP_alloc_RoW = EP_allocation(areas_for_alloc, Globiom_eco_org, EPtoconvert_RoW, EPconverted_RoW)
                  areas_EP_alloc_EU = EP_allocation(areas_EP_alloc_RoW, Globiom_eco_org, EPtoconvert_EU, EPconverted_EU)
                    areas_EP_alloc_EU[is.na(areas_EP_alloc_EU)] <- 0
                      areas_post_alloc <- separate_region(areas_EP_alloc_EU, Globiom_eco_org, "Urban", "EU") 
                        areas.mg.fin[[toString(tstep[i])]] <- areas_post_alloc
          
          areas.mg.fin[[toString(tstep[i])]] <- areas.mg.fin[[toString(tstep[i])]] %>%
                                                  arrange(Scenario, Ecoregion)
          
          areas.mg_in_full <- areas.mg.fin[[toString(tstep[i])]] %>% 
            mutate(Clear_cut_EU = Clear_cut_EU - Timber_plant_EU - Clear_cut_EU_ex, Timber_plant_EU = Timber_plant_EU - Timber_plant_EU_ex)
          
          # test ====
            test1 = colSums(areas.mg_in[3:length(areas.mg_in)])
            test1a = sum(test1)
            
            # Check allocation of negative MngFor ====
            test2 = colSums(areas_neg_alloc[3:length(areas_neg_alloc)])
            test2a = sum(test2)
              if(round(test1a-test2a, 3) != 0) {stop("ERROR - marginal - allocation of negative values of MngFor")}
              if(all.equal(test1, test2) != TRUE) {stop("ERROR - marginal - allocation of negative values of MngFor")}
              
            # Check allocation of exceeding MngFor ====
            test3 = colSums(areas_for_alloc[3:length(areas_for_alloc)])
            test3a = sum(test3)
            if((test3a-test2a) > 1) {stop("ERROR - marginal - allocation of exceeding MngFor")}
            if((all.equal(test2[1:(length(test2)-3)], test3[1:(length(test3)-8)], tolerance = 1e-03) != TRUE) 
               | (all.equal(test2[(length(test2)-1):length(test2)], test3[(length(test3)-7):(length(test3)-6)], tolerance = 1e-03) != TRUE)) {stop("ERROR - marginal - allocation of exceeding Mngfor, other land uses")}
            if(abs((test2["MngFor"] - sum(test3[(length(test3)-5):length(test3)]))) > 1) {stop("ERROR - marginal - allocation of exceeding Mngfor")}
            
            # Check allocation of energy plantations outside EU ====
            test4 = colSums(areas_EP_alloc_RoW[3:length(areas_EP_alloc_RoW)])
            test4a = sum(test4)
            if(round(test4a-test3a, 3) != 0) {stop("ERROR - marginal - allocation of Crp, Grs or Nat RoW")}
            if(abs(sum(test3["CrpLnd_RoW"], test3["GrsLnd_RoW"], test3["NatLnd_RoW"]) - sum(test4["CrpLnd_RoW"], test4["GrsLnd_RoW"], test4["NatLnd_RoW"])) > 1e-04) {stop("ERROR - marginal - allocation of Crp, Grs or Nat - RoW")}
            
            # Check allocation of energy plantations inside EU ====
            test5 = colSums(areas_EP_alloc_EU[3:length(areas_EP_alloc_EU)])
            test5a = sum(test5)
            if(round(test4a-test5a, 3) != 0) {stop("ERROR - marginal - allocation of Crp, Grs or Nat - EU")}
            if(abs(sum(test5["CrpLnd_EU"], test5["GrsLnd_EU"], test5["NatLnd_EU"]) - sum(test4["CrpLnd_EU"], test4["GrsLnd_EU"], test4["NatLnd_EU"])) > 1e-04) {stop("ERROR - marginal - allocation of Crp, Grs or Nat - EU")}
            
            # Check separation of Urban areas ====
              test6 = areas_EP_alloc_EU
              test7 = areas_post_alloc
                if ((sum(test6$Urban) - sum(test7$Urban_EU + test7$Urban_RoW)) > 1e-6) {stop("ERROR - marginal - separation of Urban areas")}
              
            rm(test1, test2, test3, test4, test5, test1a, test2a, test3a, test4a, test5a, test6, test7)
            
            
            
            
          # ====
          
          rm(areas_neg_alloc, areas_for_alloc, areas_EP_alloc_RoW, areas_EP_alloc_EU)
        
        # AV - Main allocation processes ====== 
          areas.av_in = Subset_allocation(areas.av_in, areas.av.in[["2000"]], "MngFor", "sum_EUfor", "PriFor", c("NaN", "MFM", "noAF"))                       # allocation of difference between sum_EUfor in 2000 and sum_EUfor in the ith year to PriFor for the scenario set-aside regular subtraction of the sum of forest use from MngFor for the other scenarios                                                                                                  
            areas_neg_alloc = Neg_allocation(areas.av_in, Globiom_eco_org, "MngFor")                                                                          # allocation of negative values of MngFor to positive MngFor areas within the same Globiom Region
              areas_for_alloc = For_allocation(areas_neg_alloc, Globiom_eco_org, "MngFor", data.list.steps[["Forest_intensity"]][[i]], Forests)               # allocation of the remaning MngFor area to two classes according to the share at Globiom leve: forest intensive and forest extensive
                areas_EP_alloc_RoW = EP_allocation(areas_for_alloc, Globiom_eco_org, EPtoconvert_RoW, EPconverted_RoW)                                        # allocation of negative values in CrpLnd, GrsLnd and NatLnd to the remaining positive ones within the same ecoregion or according to the share at Globiom level
                  areas_EP_alloc_EU = EP_allocation(areas_EP_alloc_RoW, Globiom_eco_org, EPtoconvert_EU, EPconverted_EU)
                    areas_EP_alloc_EU[is.na(areas_EP_alloc_EU)] <- 0                                                                                          # sets to 0 the NaN values that pop up in the intersections between rows outside EU and columns of EU values
                      areas_post_alloc <- separate_region(areas_EP_alloc_EU, Globiom_eco_org, "Urban", "EU")                                                      # separates Urban areas in EU and outside EU
                        areas.av.fin[[toString(tstep[i])]] <- areas_post_alloc
          
          areas.av.fin[[toString(tstep[i])]] <- areas.av.fin[[toString(tstep[i])]] %>%
                                                    arrange(Scenario, Ecoregion)
          
          areas.av_in_full<- areas.av.fin[[toString(tstep[i])]] %>%                                                                        # subtract areas exported outside EU from EU forested areas
            mutate(Clear_cut_EU = Clear_cut_EU - Clear_cut_EU_ex, Selection_EU = Selection_EU - Selection_EU_ex)                             
          
            # test ====
            test1 = colSums(areas.av_in[3:length(areas.av_in)])
            test1a = sum(test1)
            
            # Check allocation of negative MngFor ====
            test2 = colSums(areas_neg_alloc[3:length(areas_neg_alloc)])
            test2a = sum(test2)
            if(round(test1a-test2a, 3) != 0) {stop("ERROR - marginal - allocation of negative values of MngFor")}
            if(all.equal(test1, test2) != TRUE) {stop("ERROR - marginal - allocation of negative values of MngFor")}
            
            # Check allocation of exceeding MngFor ====
            test3 = colSums(areas_for_alloc[3:length(areas_for_alloc)])
            test3a = sum(test3)
            if((test3a-test2a) > 1) {stop("ERROR - marginal - allocation of exceeding MngFor")}
            if((all.equal(test2[1:(length(test2)-3)], test3[1:(length(test3)-8)], tolerance = 1e-03) != TRUE) 
               | (all.equal(test2[(length(test2)-1):length(test2)], test3[(length(test3)-7):(length(test3)-6)], tolerance = 1e-03) != TRUE)) {stop("ERROR - marginal - allocation of exceeding Mngfor, other land uses")}
            if(abs((test2["MngFor"] - sum(test3[(length(test3)-5):length(test3)]))) > 1) {stop("ERROR - marginal - allocation of exceeding Mngfor")}
            
            # Check allocation of energy plantations outside EU ====
            test4 = colSums(areas_EP_alloc_RoW[3:length(areas_EP_alloc_RoW)])
            test4a = sum(test4)
            if(round(test4a-test3a, 3) != 0) {stop("ERROR - marginal - allocation of Crp, Grs or Nat RoW")}
            if(abs(sum(test3["CrpLnd_RoW"], test3["GrsLnd_RoW"], test3["NatLnd_RoW"]) - sum(test4["CrpLnd_RoW"], test4["GrsLnd_RoW"], test4["NatLnd_RoW"])) > 1e-04) {stop("ERROR - marginal - allocation of Crp, Grs or Nat - RoW")}
            
            # Check allocation of energy plantations inside EU ====
            test5 = colSums(areas_EP_alloc_EU[3:length(areas_EP_alloc_EU)])
            test5a = sum(test5)
            if(round(test4a-test5a, 3) != 0) {stop("ERROR - marginal - allocation of Crp, Grs or Nat - EU")}
            if(abs(sum(test5["CrpLnd_EU"], test5["GrsLnd_EU"], test5["NatLnd_EU"]) - sum(test4["CrpLnd_EU"], test4["GrsLnd_EU"], test4["NatLnd_EU"])) > 1e-04) {stop("ERROR - marginal - allocation of Crp, Grs or Nat - EU")}
            
            # Check separation of Urban areas ====
              test6 = areas_EP_alloc_EU
              test7 = areas_post_alloc
                if ((sum(test6$Urban) - sum(test7$Urban_EU + test7$Urban_RoW)) > 1e-6) {stop("ERROR - average - separation of Urban areas")}
            
            rm(test1, test2, test3, test4, test5, test1a, test2a, test3a, test4a, test5a, test6, test7)
            
          # ====
          
          rm(areas_neg_alloc, areas_for_alloc, areas_EP_alloc_RoW, areas_EP_alloc_EU, areas_post_alloc)
      
        # MG - Allocation of negative values in forest use classes to the positive areas in the same forest category in the ecoregions belonging to the EU region =========
            
          areas.mg_fin_full_EU <- areas.mg_in_full %>%                                 
                                    inner_join(EU_ecoregions)                                                 # extract the rows belonging to the EU region
          
          areas.mg_fin_full_RoW <- setdiff(areas.mg_in_full, areas.mg_fin_full_EU)   # put the remaning rows in another df, such that after the allocation they can be binded again
          
          areas.mg_fin_full_EU = Neg_allocation(areas.mg_fin_full_EU, Globiom_eco_EU, "Selection_EU") # allocate negative values in Selection (the allocation is done such as all the ecoregions belonging to EU do not have any share outside EU -> the Globiom share is set to 1)
          areas.mg_fin_full_EU = Neg_allocation(areas.mg_fin_full_EU, Globiom_eco_EU, "Clear_cut_EU") # allocate negative values in Clear cut (the allocation is done such as all the ecoregions belonging to EU do not have any share outside EU -> the Globiom share is set to 1)
          areas.mg_fin_full_EU = Neg_allocation(areas.mg_fin_full_EU, Globiom_eco_EU, "Timber_plant_EU") # allocate negative values in Timber plantations (the allocation is done such as all the ecoregions belonging to EU do not have any share outside EU -> the Globiom share is set to 1)

          areas.mg_fin_full = areas.mg_fin_full_RoW %>%                # the EU and RoW ecoregions are binded together again
                                bind_rows(areas.mg_fin_full_EU) %>%
                                  arrange(Scenario, Ecoregion)
          
          rm(areas.mg_fin_full_EU, areas.mg_fin_full_RoW)
          
            # test ====
              test = colSums(areas.mg_fin_full[3:length(areas.mg_fin_full)])
              test_sum = sum(test)
              
              # Check allocation of negative forest use ====
              test_check = colSums(areas.mg_in_full[3:length(areas.mg_in_full)])
              test_check_sum = sum(test_check)
              if(round(test_sum - test_check_sum, 3) != 0) {stop("ERROR - marginal - allocation of negative values of forest use")}
              if(all.equal(test, test_check) != TRUE) {stop("ERROR - marginal - allocation of negative values of forest use")}
              
              rm(test, test_sum, test_check, test_check_sum)
            # ========

        # AV - Allocation of negative values in forest use classes to the positive areas in the same forest category in the ecoregions belonging to the EU region ===========
          # The subtraction of exported areas from internal EU forest area produces negative values in some ecoregions. 
        
          areas.av_fin_full_EU <- areas.av_in_full %>%                                 
                                    inner_join(EU_ecoregions)                             # extract the rows belonging to the EU region
          areas.av_fin_full_RoW <- setdiff(areas.av_in_full, areas.av_fin_full_EU)       # put the remaning rows in another df, such that after the allocation they can be binded again
    
          areas.av_fin_full_EU = Neg_allocation(areas.av_fin_full_EU, Globiom_eco_EU, "Selection_EU") # allocate negative values in Selection (the allocation is done such as all the ecoregions belonging to EU do not have any share outside EU -> the Globiom share is set to 1)
          areas.av_fin_full_EU = Neg_allocation(areas.av_fin_full_EU, Globiom_eco_EU, "Clear_cut_EU") # allocate negative values in Clear cut (the allocation is done such as all the ecoregions belonging to EU do not have any share outside EU -> the Globiom share is set to 1)
          
          areas.av_fin_full = areas.av_fin_full_RoW %>%                # EU and RoW ecoregions are binded together again
                                    bind_rows(areas.av_fin_full_EU) %>%
                                      arrange(Scenario, Ecoregion)
          rm(areas.av_fin_full_EU, areas.av_fin_full_RoW)
          
            # test ====
              test = colSums(areas.av_fin_full[3:length(areas.av_fin_full)])
              test_sum = sum(test)
              
              # Check allocation of negative forest use ====
              test_check = colSums(areas.av_in_full[3:length(areas.av_in_full)])
              test_check_sum = sum(test_check)
              if(round(test_sum - test_check_sum, 3) != 0) {stop("ERROR - marginal - allocation of negative values of forest use")}
              if(all.equal(test, test_check) != TRUE) {stop("ERROR - marginal - allocation of negative values of forest use")}
              
              rm(test, test_sum, test_check, test_check_sum)
            # ========
         
          ################################################## 4) ARRANGE THE COLUMNS ##################################################
              
    #4) Arrange the columns to make them suitable for the model 
          
        # Disaggregated land use classes ======

          # Annual_EU	Permanent_EU	Pasture_EU	Annual_RoW	Permanent_RoW	Pasture_RoW	Urban	For_ClearCut_EU	For_ClearCut_im	For_ClearCut_ex	
          # For_Retention_EU	For_PlantationFuel_im	For_SelectionSystem_EU	EP_EU	EP_conv_EU	EP_RoW	EP_conv_im	Afforested	Regrowth	
          # ForOther_Extensive_EU	ForOther_Extensive	ForOther_Intensive_EU	ForOther_Intensive
          
          areas.mg_fin_full <- areas.mg_fin_full %>%
            transmute(Scenario = Scenario, Ecoregion = Ecoregion, A_org = rowSums(.[3:length(areas.mg_fin_full)]), A_new = NatLnd_EU + NatLnd_RoW + PriFor, 
                      Annual_EU = CrpLnd_EU, Permanent_EU = OagLnd_EU, Pasture_EU = GrsLnd_EU, Urban_EU = Urban_EU, 
                      Annual_RoW = CrpLnd_RoW, Permanent_RoW = OagLnd_RoW, Pasture_RoW = GrsLnd_RoW, Urban_RoW = Urban_RoW,
                      For_ClearCut_EU = Clear_cut_EU, For_ClearCut_im = Clear_cut_im, For_ClearCut_ex = Clear_cut_EU_ex, For_Retention_EU = Retention_EU, 
                      For_PlantationFuel_im = Plantation_im/2, For_TimberPlant_EU = Timber_plant_EU, For_TimberPlant_ex = Timber_plant_EU_ex, For_TimberPlant_im = Plantation_im/2,
                      For_SelectionSystem_EU = Selection_EU, For_Selective_im = 0,
                      EP_EU = Ene_Plant_EU, EP_conv_EU = EP_CrpLnd_EU+EP_GrsLnd_EU+EP_NatLnd_EU, EP_RoW = Ene_Plant_RoW, EP_conv_im = EP_CrpLnd_im+EP_GrsLnd_im+EP_NatLnd_im, 
                      Afforested_EU = AfrLnd_EU, Afforested_RoW = AfrLnd_RoW, Regrowth = For_Regrowth_EU + For_Regrowth, ForOther_Extensive_EU = For_Extensive_EU, ForOther_Extensive_RoW = For_Extensive, 
                      ForOther_Intensive_EU = For_Intensive_EU, ForOther_Intensive_RoW = For_Intensive) #26 # we consider a single For_Regrowth, because Regrowth in EU is 0
              
          areas.mg_fin_full$Ecoregion <- as.factor(areas.mg_fin_full$Ecoregion) 
          areas.mg.fin.full[[toString(tstep[i])]] <- areas.mg_fin_full

          areas.av_fin_full <- areas.av_fin_full %>%
            transmute(Scenario = Scenario, Ecoregion = Ecoregion, A_org = rowSums(.[3:length(areas.av_fin_full)]), A_new = NatLnd_EU + NatLnd_RoW + PriFor, 
                      Annual_EU = CrpLnd_EU, Permanent_EU = OagLnd_EU, Pasture_EU = GrsLnd_EU, Urban_EU = Urban_EU, 
                      Annual_RoW = CrpLnd_RoW, Permanent_RoW = OagLnd_RoW, Pasture_RoW = GrsLnd_RoW, Urban_RoW = Urban_RoW,
                      For_ClearCut_EU = Clear_cut_EU, For_ClearCut_im = Clear_cut_im, For_ClearCut_ex = Clear_cut_EU_ex, For_Retention_EU = Retention_EU, For_PlantationFuel_im = Plantation_im, 
                      For_SelectionSystem_EU = Selection_EU, For_Selection_im = Selection_im, For_Selection_ex = Selection_EU_ex, For_Selective_im = Selective_im,
                      EP_EU = Ene_Plant_EU, EP_conv_EU = EP_CrpLnd_EU+EP_GrsLnd_EU+EP_NatLnd_EU, EP_RoW = Ene_Plant_RoW, EP_conv_im = EP_CrpLnd_im+EP_GrsLnd_im+EP_NatLnd_im, 
                      Afforested_EU = AfrLnd_EU, Afforested_RoW = AfrLnd_RoW, Regrowth = For_Regrowth_EU + For_Regrowth, ForOther_Extensive_EU = For_Extensive_EU, ForOther_Extensive_RoW = For_Extensive, 
                      ForOther_Intensive_EU = For_Intensive_EU, ForOther_Intensive_RoW = For_Intensive) #29

          areas.av_fin_full$Ecoregion <- as.factor(areas.av_fin_full$Ecoregion)
          areas.av.fin.full[[toString(tstep[i])]] <- areas.av_fin_full
      
        # Grouped land use classes ======
          
          areas.mg_remap = areas.mg_fin_full %>%
                              transmute(Scenario = Scenario, Ecoregion = Ecoregion, A_org = A_org, A_new = A_new, Annual = Annual_EU + Annual_RoW, 
                                        Permanent = Permanent_EU + Permanent_RoW + EP_EU + EP_conv_EU, Pasture = Pasture_EU + Pasture_RoW, Urban = Urban_EU + Urban_RoW, 
                                        For_ClearCut = For_ClearCut_EU + For_ClearCut_im + For_ClearCut_ex,
                                        For_Retention_EU = For_Retention_EU, For_PlantationFuel = For_PlantationFuel_im + EP_RoW +  EP_conv_im, 
                                        For_TimberPlant = For_TimberPlant_EU + For_TimberPlant_ex + For_TimberPlant_im,
                                        For_SelectionSystem_EU = For_SelectionSystem_EU, 
                                        For_Selective_im = 0,
                                        Secondary = Afforested_EU + Afforested_RoW + Regrowth, 
                                        ForOther_Extensive = ForOther_Extensive_EU + ForOther_Extensive_RoW, ForOther_Intensive = ForOther_Intensive_EU + ForOther_Intensive_RoW) #23
                            
          areas_mg <- areas.mg_remap
     
          areas.av_remap = areas.av_fin_full %>%
                              transmute(Scenario = Scenario, Ecoregion = Ecoregion, A_org = A_org, A_new = A_new, Annual = Annual_EU + Annual_RoW, 
                                        Permanent = Permanent_EU + Permanent_RoW + EP_EU + EP_conv_EU, Pasture = Pasture_EU + Pasture_RoW, Urban = Urban_EU + Urban_RoW, 
                                        For_ClearCut = For_ClearCut_EU + For_ClearCut_im + For_ClearCut_ex,
                                        For_Retention_EU = For_Retention_EU, For_PlantationFuel = For_PlantationFuel_im + EP_RoW +  EP_conv_im, 
                                        For_SelectionSystem = For_SelectionSystem_EU + For_Selection_im + For_Selection_ex, For_Selective_im = For_Selective_im, Secondary = Afforested_EU + Afforested_RoW + Regrowth, 
                                        ForOther_Extensive = ForOther_Extensive_EU + ForOther_Extensive_RoW, ForOther_Intensive = ForOther_Intensive_EU + ForOther_Intensive_RoW) #26
                            
          areas_av <- areas.av_remap
          
          
        # Fraction of land use classes ======

            # "fr_Annual_EU"               "fr_Permanent_EU"            "fr_Pasture_EU"              "fr_Urban_EU"               
            # "fr_Annual_RoW"              "fr_Permanent_RoW"           "fr_Pasture_RoW"             "fr_Urban_RoW"               "fr_For_ClearCut_EU"         "fr_For_ClearCut_im"        
            # "fr_For_ClearCut_ex"         "fr_For_Retention_EU"        "fr_For_PlantationFuel_im"   "fr_For_PlantationTimber_im" "fr_For_TimberPlant_EU"      "fr_For_TimberPlant_ex"     
            # "fr_For_SelectionSystem_EU"  "fr_For_Selective_im"        "fr_EP_EU"                   "fr_EP_conv_EU"              "fr_EP_RoW"                  "fr_EP_conv_im"             
            # "fr_Afforested_EU"           "fr_Afforested_RoW"          "fr_Regrowth"                "fr_ForOther_Extensive_EU"   "fr_ForOther_Extensive_RoW"  "fr_ForOther_Intensive_EU"  
            # "fr_ForOther_Intensive_RoW" 
                  
            fr_areas_mg <- areas.mg_fin_full %>%
                           full_join(areas.mg_remap) %>%
            transmute(Scenario = Scenario, Ecoregion = Ecoregion, 
                      fr_Annual_EU = Annual_EU/Annual, fr_Permanent_EU = Permanent_EU/Permanent,  fr_Pasture_EU = Pasture_EU/Pasture, fr_Urban_EU = Urban_EU/Urban, 
                      fr_Annual_RoW = Annual_RoW/Annual, fr_Permanent_RoW = Permanent_RoW/Permanent, fr_Pasture_RoW = Pasture_RoW/Pasture, fr_Urban_RoW = Urban_RoW/Urban,
                      fr_For_ClearCut_EU = For_ClearCut_EU/For_ClearCut, fr_For_ClearCut_im = For_ClearCut_im/For_ClearCut, fr_For_ClearCut_ex = For_ClearCut_ex/For_ClearCut,
                      fr_For_Retention_EU = For_Retention_EU/For_Retention_EU, 
                      fr_For_PlantationFuel_im = For_PlantationFuel_im/For_PlantationFuel, fr_For_PlantationTimber_im = For_TimberPlant_im/For_TimberPlant,
                      fr_For_TimberPlant_EU = For_TimberPlant_EU/For_TimberPlant, fr_For_TimberPlant_ex = For_TimberPlant_ex/For_TimberPlant,
                      fr_For_SelectionSystem_EU = For_SelectionSystem_EU/For_SelectionSystem_EU, 
                      fr_For_Selective_im = 1,
                      fr_EP_EU = EP_EU/Permanent, fr_EP_conv_EU = EP_conv_EU/Permanent, fr_EP_RoW = EP_RoW/For_PlantationFuel, fr_EP_conv_im = EP_conv_im/For_PlantationFuel, 
                      fr_Afforested_EU = Afforested_EU/Secondary, fr_Afforested_RoW = Afforested_RoW/Secondary, fr_Regrowth = Regrowth/Secondary, 
                      fr_ForOther_Extensive_EU = ForOther_Extensive_EU/ForOther_Extensive, fr_ForOther_Extensive_RoW = ForOther_Extensive_RoW/ForOther_Extensive, 
                      fr_ForOther_Intensive_EU = ForOther_Intensive_EU/ForOther_Intensive, fr_ForOther_Intensive_RoW = ForOther_Intensive_RoW/ForOther_Intensive) #26 # we consider a single For_Regrowth, because Regrowth in EU is 0
              
          fr_areas_mg$Ecoregion <- as.factor(fr_areas_mg$Ecoregion) 
          fr_areas_mg[is.na(fr_areas_mg)] <- 0

        ## test ========
          test_row <- sample(1:nrow(fr_areas_mg),1)
          test <- fr_areas_mg[test_row,]
          test_check <- areas.mg_remap[test_row,]
          
          if((test_check$Annual > 0 & abs(1 - (test$fr_Annual_EU + test$fr_Annual_RoW )) >  1e-15) | 
             (test_check$Permanent > 0 & abs(1 - (test$fr_Permanent_EU + test$fr_Permanent_RoW + test$fr_EP_EU + test$fr_EP_conv_EU)) > 1e-15) | 
             (test_check$Pasture > 0 & abs(1 - (test$fr_Pasture_EU + test$fr_Pasture_RoW )) > 1e-15) |
             (test_check$Urban > 0 & abs(1 - (test$fr_Urban_EU + test$fr_Urban_RoW)) > 1e-15) |
             (test_check$For_ClearCut > 0 & abs(1 - (test$fr_For_ClearCut_EU + test$fr_For_ClearCut_im + test$fr_For_ClearCut_ex)) > 1e-15) |
             (test_check$For_Retention_EU > 0 & abs(1 - (test$fr_For_Retention_EU)) > 1e-15) |
             (test_check$For_PlantationFuel > 0 & abs(1 - (test$fr_For_PlantationFuel_im + test$fr_EP_RoW + test$fr_EP_conv_im)) > 1e-15) |
             (test_check$For_SelectionSystem_EU > 0 & abs(1 - (test$fr_For_SelectionSystem_EU)) > 1e-15) |
             (test_check$For_TimberPlant > 0 & abs(1 - (test$fr_For_TimberPlant_EU + test$fr_For_TimberPlant_ex + test$fr_For_PlantationTimber_im)) > 1e-15) |
             (test_check$Secondary > 0 & abs(1 - (test$fr_Afforested_EU + test$fr_Afforested_RoW + test$fr_Regrowth)) > 1e-15) |
             (test_check$ForOther_Extensive > 0 & abs(1 - (test$fr_ForOther_Extensive_EU + test$fr_ForOther_Extensive_RoW)) > 1e-15) | 
             (test_check$ForOther_Intensive > 0 & abs(1 - (test$fr_ForOther_Intensive_EU + test$fr_ForOther_Intensive_RoW)) > 1e-15) )
          {stop("ERROR - marginal - calculation of fractions")}
          
          rm(test, test_check)
          
          # ========
          
          if (timber == "OFF") {
            # areas.mg_fin_full <- areas.mg_fin_full %>% select(-For_TimberPlant_EU, -For_TimberPlant_ex)
            # areas.mg_remap <- areas.mg_remap %>% select(-For_TimberPlant)
            # write.csv(areas.mg_fin_full, paste0("./results/areas/MG/disaggregated/areas_disaggr_mg_", tstep[i], ".csv"), row.names = FALSE)
            # write.csv(areas.av_fin_full, paste0("./results/areas/AV/disaggregated/areas_disaggr_av_", tstep[i], ".csv"), row.names = FALSE)
            write.csv(areas.mg_remap, paste0("./results/areas/notimber/mg/aggregated/areas_aggr_mg_", tstep[i], ".csv"), row.names = FALSE)
            # write.csv(areas.av_remap, paste0("./results/areas/AV/aggregated/areas_aggr_av_", tstep[i], ".csv"), row.names = FALSE)
            write.csv(fr_areas_mg, paste0("./results/areas/notimber/mg/fraction/fr_areas_disaggr_mg_", tstep[i], ".csv"), row.names = FALSE)

          } else if (timber == "ON"){
            # write.csv(areas.mg_fin_full, paste0("./results/areas/MG/disaggregated/areas_disaggr_mg_", tstep[i], "_timber.csv"), row.names = FALSE)
            # write.csv(areas.av_fin_full, paste0("./results/areas/AV/disaggregated/areas_disaggr_av_", tstep[i], "_timber.csv"), row.names = FALSE)
            write.csv(areas.mg_remap, paste0("./results/areas/timber/mg/aggregated/areas_aggr_mg_", tstep[i], "_timber.csv"), row.names = FALSE)
            # write.csv(areas.av_remap, paste0("./results/areas/AV/aggregated/areas_aggr_av_", tstep[i], "_timber.csv"), row.names = FALSE)
            write.csv(fr_areas_mg, paste0("./results/areas/timber/fraction/mg/fr_areas_disaggr_mg_", tstep[i], "_timber.csv"), row.names = FALSE)

          }
        
        rm(areas.av_in, areas.av_fin_full, areas.av_remap, areas.mg_in, areas.mg_fin_full, areas.mg_remap)
        
        # ======  
        
      }
  
  
  rm(list = ls())

  