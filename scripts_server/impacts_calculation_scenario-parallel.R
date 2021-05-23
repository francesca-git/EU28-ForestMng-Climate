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
    vulnerability = TRUE
    BS = TRUE
    

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
      

  
      
############################# CALCULATION OR LOADING OF THE INPUT PARAMETERS ############################# 
      
        ptm <- proc.time()
      
      # Setting values used in the calculation
      print(paste0("CI : ", CI))
      
      if (CI == FALSE) { uncertainties = FALSE 
                         n = 2
      } else if (CI == TRUE){ uncertainties = TRUE
            distribution = "Weibull"
              if (BS == TRUE) {n = 10000
              } else if (BS == FALSE) {n = 1000}
                }
      
      # The parameters are calculated using the functions in the file parameters_calculation.R and in bootstrapping.R and then saved to be loaded here. 
      
      # response ratios (all .Rdata which are loaded here below contain a 4-dimension array called rr_ecoregion. 1dim: ecoregions;
      # 2dim: land use categories; 3dim: taxa; 4dim: either twice the median of the raw values or the monte carlo simulations or 
      # the bootstrsapped medians)
      
      if (BS == TRUE) {   # if bootstrapping is TRUE, the response ratios come from the bootstrapping process (boostrapping.R)
        load("./rr_z/rr_ecoregion_bs.Rdata") 
      } else if (BS == FALSE & uncertainties == TRUE) {  # if BS == FALSE and uncertainties == TRUE, the response ratios come from the monte carlo simulations
        load("./rr_z/rr_ecoregion_mc.Rdata")
      } else if (BS == FALSE & uncertainties == FALSE) { # if BS == FALSE and uncertainties == FALSE, the response ratios are the medians of the raw values
        load("./rr_z/rr_ecoregion_static.Rdata")
      }
      
      ratio_eco <- rr_ecoregion
      rm(rr_ecoregion)
      
      # calculate.RR(uncertainties = uncertainties, distribution = distribution, n = n, cutoff = cutoff)        # four dimension array containing the response ratio per ecoregion (1st dimensio), land use category (2nd dimension) and taxa (3rd dimension).
      # The fourth dimension contains either two equal values corresponding to the median of the raw response ratios or the n simulations generated by the given distribution
      
      if (dim(ratio_eco)[4] != n) {stop("Problem with the dimension of the array containing the response ratios (function: calculate.slost)")}
      
      # Weights (which include the vulnerability scores) and original number of species
      weight_tx = prepare.Sorg.VS.weighting(vulnerability = vulnerability)[["Weights"]]    # dataframe containing the weighting factors (rows: ecoregions, columns: weights per taxonomic group)
      Sorg_VS = prepare.Sorg.VS.weighting(vulnerability = vulnerability)[["Sorg_VS"]]      #
      
      # z values (all .Rdata which are loaded here below contain a 2-dimension array called zvalues. 1dim: ecoregions;
      # 2dim: either the mean of the raw values or the monte carlo simulations or the bootstrsapped medians)
      
      if(BS == TRUE) {
        load("./rr_z/zvalues_ecoregion_bs.Rdata")  # load the 
      } else if(BS == FALSE) {
        load("./rr_z/zvalues.Rdata")
      }
      
      parameters = list("ratio_eco" = ratio_eco, "weight_tx" = weight_tx, "Sorg_VS" = Sorg_VS, "zvalues" = zvalues)
      
      rm(ratio_eco, weight_tx, Sorg_VS, zvalues)
        
        proc.time() - ptm      
        
    
          
############################# CALCULATE THE IMPACTS ############################# 
        
  tsteps = c(2020, 2050, 2100) # seq(from = 2000, to = 2100, by = 10)  
  ntsteps = length(tsteps) #time steps: 2000 -> 2100

  # Parallel for-loop
  # Packages need to be passed on to each core
  
  # Setup parallel computation (setting up a cluster that needs to be stopped later)
  
  
  # !!!!!!!!!!!!!!!!!!!!!!!
  
  # UseCores = 5 # round(detectCores() / 2)  # Number of cores (parallel "workers") to be used
  # cl = makeCluster(UseCores)
  # registerDoParallel(cl)
  # 
  initial.time = Sys.time()
  # 
  
    ################### LOOPS OVER THE YEARS ################### 
    
  tstep = 2
  tstep_areas = 6
  #  for (tstep in 3:3) {#ntsteps) {
      #tstep = 3
      #test_matrix <- matrix(1, nrow = necoregions, ncol = 17)
      ptm_year <- Sys.time()
      
      
      ############  PREPARATION OF AREAS ############
          
        print(paste0("time step (start): ", tsteps[tstep]))
        Areas <- myfiles[[tstep_areas]]
        fr_Areas <- myfiles_fr[[tstep_areas]]
        
        Areas <- Areas %>% rename(Eco_code = Ecoregion)
        fr_Areas <- fr_Areas %>% rename(Eco_code = Ecoregion)
        # Original natural areas and new natural areas, other land use areas
        # extent (m2) of the orginal natural habitat per ecoregion (A_org) and extent of the current/new natural habitat per ecoregion (A_new) -> A_new = remaining natural area 
        # dataframe with land use areas other than A_org and A_new 
        
        Scenarios = unique(Areas$Scenario)
        
        scenario_in = 1:nscenarios
        
        
        ################### PARALLEL LOOPS OVER THE SCENARIOS - FRACTION OF AREAS ################### 
        
          start.time = Sys.time()
          
          fr_Areas_new <- data.frame(matrix(NA, nrow = 0, ncol = 0))
  
          for(sc in 1:nscenarios) {
                                                                         
            fr_temp <- fr_Areas %>% filter(Scenario == toString(Scenarios[sc])) %>%
                full_join(Ecoregions) %>%
                  mutate(Eco_code = as.character(Eco_code)) %>%
                    arrange(Eco_code) %>%
                      mutate(Eco_code = as.factor(Eco_code)) %>%
                        mutate(Scenario = toString(Scenarios[sc]))
  
            fr_Areas_new <- fr_Areas_new %>% bind_rows(fr_temp)
               
             } 
  
            t = Sys.time()-start.time
            print(paste('Time - preparation of the fractions of areas:', round(t, 3), units(t)))
            
        
      ############ CALCULATION OF SPECIES LOSS ############
        
        ## Preparation of the dataframes to be filled up
        
          Slost_fin <- data.frame(matrix(NA, nrow = necoregions*nscenarios, ncol = 3+3*nlfuse))
          
          names(Slost_fin) = c("Scenario", "Eco_code", "Year", paste(lu_to_assess, "median", sep = "_"), paste(lu_to_assess, "lower95", sep = "_"), paste(lu_to_assess, "upper95", sep = "_"))
          
          Slost_fin$Scenario = rep(Scenarios, each = necoregions)
          Slost_fin$Eco_code = rep(Ecoregions$Eco_code, nscenarios)
          Slost_fin$Year = tsteps[tstep]
          
          Slost_fin_plants <- Slost_fin
          Slost_fin_birds <- Slost_fin
          Slost_fin_mammals <- Slost_fin

       
        ################### PARALLEL LOOPS OVER THE SCENARIOS - SLOST ################### 
      
        # Parallel for-loop
        # Packages need to be passed on to each core
        # scenario_in
      
        start.time = Sys.time()
          
        results <- list()
        
        for(sc in 1:nscenarios) {
            
        # !!!!!!!!!!!!!!!!!
          
        #results = foreach(sc = 1:2, .packages=c('dplyr', 'tidyr', 'abind', 'tidyverse', 
         #                                        'fitdistrplus', 'truncdist', 'foreach', 
          #                                       'MASS', 'triangle')) %dopar% {
                                                   
                  ##### PREPARE THE AREAS #####
                  
                    print(paste0("scenario (start): ", Scenarios[sc]))
            
                    Areas_temp <- Areas %>% filter(Scenario == toString(Scenarios[sc])) %>%
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
          
                    if (BS == FALSE) {
                    Slost <- calculate.slost(Total_RemainingNatural_areas = Areas_org_new, Land_use_areas = Areas_lu, param = parameters, CI = FALSE, BS)
                      Slost_aggr_matrix <- Slost[["Aggr_matrix"]]
                      Slost_taxa_matrix <- Slost[["PerTaxon_matrix"]]
                      Slost_aggr2.5_matrix <- Slost[["Aggr2.5_matrix"]]
                      Slost_aggr97.5_matrix <- Slost[["Aggr97.5_matrix"]]
                      rm(Slost)
                    }
                    # Arguments:
                    # 1) Total_RemainingNatural_areas : dataframe with two columns, one with the total original areas, one with the remaining natural areas. The rows correspond to the
                    # ecoregions and are sorted in alphabetical order according to the ecoregion codes (AA0101, AA0102, etc.)
                    # 2) Land_use_areas : dataframe which contains one column per each land use category. The rows correspond to the
                    # ecoregions and are sorted in alphabetical order according to the ecoregion codes (AA0101, AA0102, etc.)
                    # 3) CI can be TRUE or FALSE. TRUE = the CI will be calculated, FALSE = the CI will not be calculated. WARNING: at this step, CI must be FALSE,
                    # because here this function is used only to compute the median values. The calculation of the CI is computed at a later step (next if loop)
                    # 5) vulnerability: TRUE or FALSE
                    # 6) cutoff: TRUE or FALSE
                    # 7) BS : TRUE or FALSE
                    # Output:
                    # list("Aggr_matrix"= Slost_aggr_median, "Aggr_df"= Slost_aggr_df, "PerTaxon_matrix"= Slost_taxa_matrix, "Pertaxon_df"= Slost_taxa_df,
                    #      "Aggr2.5_matrix"= Slost_aggr_2.5, "Aggr97.5_matrix"= Slost_aggr_97.5)

                    if (CI == TRUE) {
                      Slost_CI <- calculate.slost(Total_RemainingNatural_areas = Areas_org_new, Land_use_areas = Areas_lu, param = parameters, CI = TRUE, BS)
                      if (BS == TRUE) {
                        Slost_aggr_matrix <- Slost_CI[["Aggr_matrix"]]
                        Slost_taxa_matrix <- Slost_CI[["PerTaxon_matrix"]]
                      }
                      Slost_aggr2.5_matrix <- Slost_CI[["Aggr2.5_matrix"]]
                      Slost_aggr97.5_matrix <- Slost_CI[["Aggr97.5_matrix"]]
                      rm(Slost_CI)
                    }

                    
                    # Slost_aggr_matrix <- test_matrix
                    # Slost_aggr2.5_matrix <- test_matrix
                    # Slost_aggr97.5_matrix <- test_matrix
                    # Slost_taxa_matrix <- list("Plants" = test_matrix, "Birds" =  test_matrix, "Mammals" = test_matrix)
                    # 
                    Slost_list <- list("Total_impacts" = Slost_aggr_matrix, "Total_impacts_2.5q" = Slost_aggr2.5_matrix, "Total_impacts_97.5q" = Slost_aggr97.5_matrix,
                                       "Plants" = Slost_taxa_matrix[["Plants"]], "Birds" = Slost_taxa_matrix[["Birds"]], "Mammals" = Slost_taxa_matrix[["Mammals"]])
                    
                    results[[sc]] <- Slost_list
                    
                    
                    }
        
        
          t = Sys.time() - start.time
          print(paste('Time - calculation of species loss over all scenarios:', round(t, 3), units(t)))
      
      
          ################### FOR LOOPS OVER THE SCENARIOS TO FILL THE FINAL DATA FRAMES ################### 
        
          start.time = Sys.time()
        
            for(sc in 1:nscenarios) {
              
              Slost_fin[((necoregions*(sc-1))+1):(necoregions*(sc)), 4:(3+nlfuse)] = results[[sc]][["Total_impacts"]] # Slost_aggr_matrix # 
              Slost_fin[((necoregions*(sc-1))+1):(necoregions*(sc)), (3+nlfuse+1):(3+2*nlfuse)] = results[[sc]][["Total_impacts_2.5q"]] # Slost_aggr2.5_matrix # 
              Slost_fin[((necoregions*(sc-1))+1):(necoregions*(sc)), (3+2*nlfuse+1):(3+3*nlfuse)] = results[[sc]][["Total_impacts_97.5q"]] # Slost_aggr97.5_matrix # 
              
              Slost_fin_plants[((necoregions*(sc-1))+1):(necoregions*(sc)), 4:(3+nlfuse)] = results[[sc]][["Plants"]] # Slost_taxa_matrix[["Plants"]] # 
              Slost_fin_birds[((necoregions*(sc-1))+1):(necoregions*(sc)), 4:(3+nlfuse)] = results[[sc]][["Birds"]] # Slost_taxa_matrix[["Birds"]] # 
              Slost_fin_mammals[((necoregions*(sc-1))+1):(necoregions*(sc)), 4:(3+nlfuse)] = results[[sc]][["Mammals"]] # Slost_taxa_matrix[["Mammals"]] # 
            
            }
          
          rm(results)
          
          t = Sys.time()-start.time
          print(paste('Time - compilation of Slost_fin:', round(t, 3), units(t))) 
          
      
          ################### ALLOCATE THE IMPACTS TO THE DISAGGREGATED LAND USE CATEGORIES ################### 
      
          start.time = Sys.time()
            
            Slost_fin_disaggr <- allocate.impacts(Slost_fin, fr_Areas_new)
            Slost_fin_disaggr_plants <-  allocate.impacts(Slost_fin_plants, fr_Areas_new)      
            Slost_fin_disaggr_birds <-   allocate.impacts(Slost_fin_birds, fr_Areas_new)     
            Slost_fin_disaggr_mammals <- allocate.impacts(Slost_fin_mammals, fr_Areas_new)      
            
            t = Sys.time()-start.time
            print(paste('Time - allocation of areas:', round(t, 3), units(t))) 
    
            rm(fr_Areas_new)
        
      
############################# SAVE THE RESULTS ############################# 
      

        if (Approach == "AV" && vulnerability == FALSE) {
          write.csv(Slost_fin, paste0("./species-lost/av/Slost_av_", tsteps[tstep], "noV.csv"), row.names = FALSE)
        } else if (Approach == "AV" && vulnerability == TRUE) {
          write.csv(Slost_fin, paste0("./species-lost/av/Slost_av_", tsteps[tstep], ".csv"), row.names = FALSE)
        } else if(Approach == "MG" && vulnerability == FALSE) {
          write.csv(Slost_fin, paste0("./species-lost/Slost_mg_", tsteps[tstep], "noV.csv"), row.names = FALSE)
        } else if (Approach == "MG" && vulnerability == TRUE) {
          write.csv(Slost_fin_disaggr, paste0("./species-lost/Slost_mg_", tsteps[tstep],  "_", case, ".csv"), row.names = FALSE)
          write.csv(Slost_fin_disaggr_plants, paste0("./species-lost/Slost_mg_", tsteps[tstep], "_", case, "_plants.csv"), row.names = FALSE)
          write.csv(Slost_fin_disaggr_birds, paste0("./species-lost/Slost_mg_", tsteps[tstep],  "_", case, "_birds.csv"), row.names = FALSE)
          write.csv(Slost_fin_disaggr_mammals, paste0("./species-lost/Slost_mg_", tsteps[tstep],  "_", case, "_mammals.csv"), row.names = FALSE)
        }
      
      print(paste0("time step (end): ", tsteps[tstep]))
      
      ptm_endyear <- Sys.time() - ptm_year     
      
      print(paste0("Time (one year): ", round(ptm_endyear, 3), units(ptm_endyear)))
      

    #}
  
  run_time = Sys.time() - initial.time
  print(paste('Total time:', round(run_time, 3), units(run_time)))
  
      
      # Stop the cluster
      # stopCluster(cl)
      
    
  

  
