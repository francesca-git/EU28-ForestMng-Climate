setwd("/home/frrosa/R/Forest_management/") # folder where this calculation takes places

library("triangle") # triangula distribution -> for the distribution of the z values
library(dplyr)      # dataframe management
library(tidyr)      # dataframe management
library(abind)      # dataframe management
library(tidyverse)
library(parallel)
library(doParallel)
library(foreach)
library(arsenal)
select <- dplyr::select
source("./parameters_calculation.R")
source("./CF_functions.R")
source("./calculate_slost.R")
source("./allocate_impacts.R")

############################# DEFINE THE SCENARIOS/CONDITIONS UNDER WHICH PERFORM THE ANALYSIS ############################# 
  
  # Calculation of the impacts
    Approach = "MG"         # "AV" or "MG". AV: average (imports and exports involve all types of forest management); MG: marginal (imports and exports involve only intensive forest use, Plantation and Clear cut)
    Disaggregated = "ON"             # "ON" or "OFF". "ON": the results are dis-aggregated according to the land use classes provided by Fulvio; "OFF": the impacts are aggregated according to Chaudhary 2015 (broad land use classes) and Chaudhary 2016 (forest use intensities) papers 
    Timber = FALSE  # TRUE (part of clear cut areas have been allocated to Timber plantations). !! This option is valid only if approach == "MG"
    
  # Calculation of the response ratio
    CI = TRUE                            # TRUE (confidence intervals are calculated) or FALSE (confidence intervals are not calculated)
    cutoff = TRUE # TRUE (all raw RR > 1 are set to 1) or FALSE
    case = "cutoff" # cutoff cutoff_timber nocutoff nocutoff_timber
    vulnerability = "ON"
    

############################# INITIAL SETTINGS ############################# 
  
     necoregions = 804                      
     Ecoregions = read.csv("./ecoregions_data/Ecoregions_description.csv", header = TRUE) # Ecoregions specifications from Chaudhary 2015
     Ecoregions <- Ecoregions %>% dplyr::select(Eco_code) %>% mutate(Eco_code = as.factor(Eco_code)) 
     nscenarios = 24
     
     if(Timber == FALSE) {path_areas = "./areas/notimber/"} else if (Timber == TRUE) {path_areas = "./areas/timber/"}

     # create a vector with the names of the land use types as in the array with the response ratio
     
     lu_to_assess = name.landuse() # this is a character vector
     nlfuse = length(lu_to_assess) # number of land use types
     
############################# LOAD THE AREAS ############################# 
    
    # Select the files to load according to the approach
    
      if (Approach == "AV") {
        temp = list.files(path = paste0(path_areas, "av/aggregated/"), pattern="*.csv", full.names = TRUE)
        fr_temp = list.files(path = paste0(path_areas, "av/fraction/"), pattern="*.csv", full.names = TRUE)
        
      } else if(Approach == "MG") {
        temp = list.files(path = paste0(path_areas, "mg/aggregated/"), pattern="*.csv", full.names = TRUE)
        fr_temp = list.files(path = paste0(path_areas, "mg/fraction/"), pattern="*.csv", full.names = TRUE)
      }
    
      myfiles = lapply(temp, read.csv)
      myfiles_fr = lapply(fr_temp, read.csv)
      #ratio_eco - land use names
      #[1] "Annual"               "Permanent"            "Pasture"              "Urban"                "PlantationNonTimber"  "ClearCut"             "Retention"            "PlantationFuel"       "SlashAndBurn"         "SelectionSystem"      "SelectiveLogging"    
      #[12] "Agroforestry"         "Plantation"           "ReducedImpactLogging" "Afforested"           "ForExtensive"         "ForIntensive"
      

############################# CALCULATE THE IMPACTS ############################# 
        
  tstep = seq(from = 2000, to = 2100, by = 10)  
  ntstep = length(tstep) #time steps: 2000 -> 2100

  ptm <- proc.time()
  # Parallel for-loop
    # Packages need to be passed on to each core
    
    # Setup parallel computation (setting up a cluster that needs to be stopped later)
    start.time = Sys.time()
    
    UseCores = round(detectCores() / 5 * 4)  # Number of cores (parallel "workers") to be used
    cl = makeCluster(UseCores)
    registerDoParallel(cl)
    
    
    ################### LOOPS OVER THE YEARS ################### 
    
    
    # Parallel for-loop
    # Packages need to be passed on to each core
    results = foreach(t = 1:ntstep, .packages=c('dplyr', 'tidyr', 'abind', 'tidyverse', 
                                                'fitdistrplus', 'truncdist', 'foreach', 
                                                'MASS', 'triangle')) %dopar% {
                                                  
      print("in the foreach function")
      #t = 11
      #AREAS 
      print(paste0("time step (start): ", tstep[t]))
      Areas <- myfiles[[t]]
      fr_Areas <- myfiles_fr[[t]]
      
      Areas <- Areas %>% rename(Eco_code = Ecoregion)
      fr_Areas <- fr_Areas %>% rename(Eco_code = Ecoregion)
      # Original natural areas and new natural areas, other land use areas
      # extent (m2) of the orginal natural habitat per ecoregion (A_org) and extent of the current/new natural habitat per ecoregion (A_new) -> A_new = remaining natural area 
      # dataframe with land use areas other than A_org and A_new 

      Scenarios = unique(Areas$Scenario)
  
      Slost_fin <- data.frame(matrix(NA, nrow = necoregions*nscenarios, ncol = 3+3*nlfuse))
      
      names(Slost_fin) = c("Scenario", "Eco_code", "Year", paste(lu_to_assess, "median", sep = "_"), paste(lu_to_assess, "lower95", sep = "_"), paste(lu_to_assess, "upper95", sep = "_"))
      
      Slost_fin$Scenario = rep(Scenarios, each = necoregions)
      Slost_fin$Eco_code = rep(Ecoregions$Eco_code, nscenarios)
      Slost_fin$Year = tstep[t]
      
      Slost_fin_plants <- Slost_fin
      Slost_fin_birds <- Slost_fin
      Slost_fin_mammals <- Slost_fin
      
      fr_Areas_new <- data.frame(matrix(NA, nrow = 0, ncol = 0))

      scenario_in = 0:(nscenarios-1)
      
      ptm <- proc.time()
      
        
      ################### LOOPS OVER THE SCENARIOS ################### 
      
      
      for (sc in scenario_in) { #scenario_in) {
        #sc = 0
        
        ##### PREPARE THE AREAS #####
        
          print(paste0("scenario (start): ", Scenarios[sc+1]))
  
          Areas_temp <- Areas %>% filter(Scenario == toString(Scenarios[sc+1])) %>%
                          dplyr::select(-Scenario) %>%
                            full_join(Ecoregions) %>%
                              mutate(Eco_code = as.character(Eco_code)) %>%
                                arrange(Eco_code) %>%
                                  mutate(Eco_code = as.factor(Eco_code))
          
          Areas_lu <- Areas_temp[4:length(Areas_temp)]
          rownames(Areas_lu) <- Ecoregions$Eco_code
          
          Areas_org_new <-Areas_temp[2:3]
          rownames(Areas_org_new) <- Ecoregions$Eco_code
        
        
        ##### COMPUTE THE TOTAL IMPACTS AND THE IMPACTS PER TAXONOMIC GROUP #####

          Slost <- calculate.slost(Total_RemainingNatural_areas = Areas_org_new, Land_use_areas = Areas_lu, CI = FALSE, vulnerability = vulnerability, cutoff = cutoff)
          # Arguments:
          # 1) Total_RemainingNatural_areas : dataframe with two columns, one with the total original areas, one with the remaining natural areas. The rows correspond to the 
          # ecoregions and are sorted in alphabetical order according to the ecoregion codes (AA0101, AA0102, etc.)
          # 2) Land_use_areas : dataframe which contains one column per each land use category. The rows correspond to the 
          # ecoregions and are sorted in alphabetical order according to the ecoregion codes (AA0101, AA0102, etc.)
          # 3) CI can be TRUE or FALSE. TRUE = the CI will be calculated, FALSE = the CI will not be calculated. WARNING: at this step, CI must be FALSE,
          # because here this function is used only to compute the median values. The calculation of the CI is computed at a later step (next if loop)
          # 5) vulnerability: "ON" or "OFF" 
          
          # result = list("Aggr_matrix"= Slost_aggr_median, "Aggr_df"= Slost_aggr_df, "PerTaxon_matrix"= Slost_taxa_matrix, "Pertaxon_df"= Slost_taxa_df,
          #              "Aggr2.5_matrix"= Slost_aggr_2.5, "Aggr97.5_matrix"= Slost_aggr_97.5)
          
          Slost_aggr_matrix <- Slost[["Aggr_matrix"]]
          Slost_taxa_matrix <- Slost[["PerTaxon_matrix"]]
          Slost_aggr2.5_matrix <- Slost[["Aggr2.5_matrix"]]
          Slost_aggr97.5_matrix <- Slost[["Aggr97.5_matrix"]]
          
          rm(Slost)
          
          if (CI == TRUE) {
            Slost_CI <- calculate.slost(Total_RemainingNatural_areas = Areas_org_new, Land_use_areas = Areas_lu, CI = TRUE, vulnerability = vulnerability, cutoff = cutoff)
            Slost_aggr2.5_matrix <- Slost_CI[["Aggr2.5_matrix"]]
            Slost_aggr97.5_matrix <- Slost_CI[["Aggr97.5_matrix"]]
            rm(Slost_CI)
          }
          
          Slost_fin[((necoregions*sc)+1):(necoregions*(sc+1)),4:(3+nlfuse)] = Slost_aggr_matrix
          Slost_fin[((necoregions*sc)+1):(necoregions*(sc+1)),(3+nlfuse+1):(3+2*nlfuse)] = Slost_aggr2.5_matrix
          Slost_fin[((necoregions*sc)+1):(necoregions*(sc+1)),(3+2*nlfuse+1):(3+3*nlfuse)] = Slost_aggr97.5_matrix
          
          Slost_fin_plants[((necoregions*sc)+1):(necoregions*(sc+1)),4:(3+nlfuse)] = Slost_taxa_matrix[["Plants"]]
          Slost_fin_birds[((necoregions*sc)+1):(necoregions*(sc+1)),4:(3+nlfuse)] = Slost_taxa_matrix[["Birds"]]
          Slost_fin_mammals[((necoregions*sc)+1):(necoregions*(sc+1)),4:(3+nlfuse)] = Slost_taxa_matrix[["Mammals"]]
        
        fr_temp <- fr_Areas %>% filter(Scenario == toString(Scenarios[sc+1])) %>%
              full_join(Ecoregions) %>%
                mutate(Eco_code = as.character(Eco_code)) %>%
                  arrange(Eco_code) %>%
                    mutate(Eco_code = as.factor(Eco_code)) %>%
                      mutate(Scenario = toString(Scenarios[sc+1]))
        
        fr_Areas_new <- fr_Areas_new %>% bind_rows(fr_temp)
        
        print(paste0("scenario (end): ", Scenarios[sc+1]))
        
        rm(Areas_temp, Areas_lu, Areas_org_new)
      }
      
      
      # end <- proc.time()
      # time = end - start
      
      proc.time() - ptm      
      #print(paste0("Time: ", time))
      
      
############################# ALLOCATE THE IMPACTS TO THE DISAGGREGATED LAND USE CATEGORIES #############################
      
      print(paste0("time step (end - before allocation): ", tstep[t]))
      
      Slost_fin_disaggr <- allocate.impacts(Slost_fin, fr_Areas_new)
      Slost_fin_disaggr_plants <-  allocate.impacts(Slost_fin_plants, fr_Areas_new)      
      Slost_fin_disaggr_birds <-   allocate.impacts(Slost_fin_birds, fr_Areas_new)     
      Slost_fin_disaggr_mammals <- allocate.impacts(Slost_fin_mammals, fr_Areas_new)      
      
      print(paste0("time step (end - after allocation): ", tstep[t]))
      
      rm(fr_Areas_new)
############################# SAVE THE RESULTS ############################# 
      

        if (Approach == "AV" && vulnerability == "OFF") {
          write.csv(Slost_fin, paste0("./species-lost/av/Slost_av_", tstep[t], "noV.csv"), row.names = FALSE)
        } else if (Approach == "AV" && vulnerability == "ON") {
          write.csv(Slost_fin, paste0("./species-lost/av/Slost_av_", tstep[t], ".csv"), row.names = FALSE)
        } else if(Approach == "MG" && vulnerability == "OFF") {
          write.csv(Slost_fin, paste0("./species-lost/Slost_mg_", tstep[t], "noV.csv"), row.names = FALSE)
        } else if (Approach == "MG" && vulnerability == "ON") {
          write.csv(Slost_fin_disaggr, paste0("./species-lost/Slost_mg_", tstep[t],  "_", case, ".csv"), row.names = FALSE)
          write.csv(Slost_fin_disaggr_plants, paste0("./species-lost/Slost_mg_", tstep[t], "_", case, "_plants.csv"), row.names = FALSE)
          write.csv(Slost_fin_disaggr_birds, paste0("./species-lost/Slost_mg_", tstep[t],  "_", case, "_birds.csv"), row.names = FALSE)
          write.csv(Slost_fin_disaggr_mammals, paste0("./species-lost/Slost_mg_", tstep[t],  "_", case, "_mammals.csv"), row.names = FALSE)
        }
      
      print(paste0("time step (end): ", tstep[t]))
      ptm <- proc.time()
      
      
    }
    
    # Stop the cluster
    stopCluster(cl)
  
  t = Sys.time()-start.time
  print(paste('Total time:', round(t, 3), units(t)))
  
  ptm <- proc.time()
  
  
