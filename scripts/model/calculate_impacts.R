source("./scripts/model/parameters_calculation.R")
source("./scripts/model/CF_functions.R")
source("./scripts/model/calculate_slost.R")
source("./scripts/model/allocate_impacts.R")

if(!require("pacman")){install.packages("pacman")};require("pacman")
p_load(MASS, dplyr, tidyr, abind, tidyverse, stringr, foreach, fitdistrplus, truncdist)  # dataframe management and string management
select <- dplyr::select


# calculate.impacts <- function(cutoff, CI, BS, marginal, label, areas_processed_path, results_path, ecodata_path, vulnerability) {
#   
#   if(missing(cutoff)) {cutoff = TRUE}
#   if(missing(CI)) {CI = FALSE}
#   if(missing(BS)) {BS = TRUE}
#   if(missing(marginal)) {marginal = TRUE}
    
############################# INITIAL SETTINGS ############################# 
  
     necoregions = 804                      
     Ecoregions = read.csv(paste0(ecodata_path, "Ecoregions_description.csv"), header = TRUE) # Ecoregions specifications from Chaudhary et al. (2015)
     Ecoregions <- Ecoregions %>% dplyr::select(Eco_code) %>% mutate(Eco_code = as.factor(Eco_code)) # convert the eco codes of the ecoregions to a factor
     
     # create a vector with the names of the land use types as in the array with the response ratio
     lu_to_assess = name.landuse() # function in allocate_impact.R which returns a character vector with the land use names 
     nlfuse = length(lu_to_assess) # number of land use types
     
############################# LOAD THE AREAS ############################# 
  
     temp = list.files(path = paste0(areas_processed_path, "aggregated/"), pattern="*.csv", full.names = TRUE)
     fr_temp = list.files(path = paste0(areas_processed_path, "fraction"), pattern="*.csv", full.names = TRUE)
        
     myfiles = lapply(temp, read.csv)
     myfiles_fr = lapply(fr_temp, read.csv)
      #ratio_eco - land use names
      #[1] "Annual"               "Permanent"            "Pasture"              "Urban"                "PlantationNonTimber"  "ClearCut"             "Retention"            "PlantationFuel"       "SlashAndBurn"         "SelectionSystem"      "SelectiveLogging"    
      #[12] "Agroforestry"         "Plantation"           "ReducedImpactLogging" "Afforested"           "ForExtensive"         "ForIntensive"
    rm(temp, fr_temp)    

  
      
############################# CALCULATION OR LOADING OF THE INPUT PARAMETERS ############################# 
      
      ptm <- proc.time()
      
      # Setting values used in the calculation
      print(paste0("CI : ", CI))
      
      if (CI == FALSE) { n = 2
      } else if (CI == TRUE){ 
            distribution = "Weibull"                      # confidence intervals calculated with montecarlo simulation and a Weibull distribution for the response ratio
              if (BS == TRUE) {n = 10000                  # if BS == TRUE, 10000 bootstrapped values of response ratios and z values are computed, 
              } else if (BS == FALSE) {n = 1000}          # if BS == FALSE, 1000 values are produced with the montecarlo simulatio procedure
                }
      
      # The parameters are calculated using the functions in the file parameters_calculation.R and in bootstrapping.R and then saved to be loaded here. 
      # The parameters are first saved and then called here because the bootstrapping procedure takes a lot of time, so it has already been performed.
      
      # LOADING OF THE RESPONSE RATIOS 
      # all .Rdata loaded here below contain a 4-dimension array called rr_ecoregion. 1dim: ecoregions; 2dim: land use categories; 
      # 3dim: taxa; 4dim: either two columns both containing the median of the raw values (static option) or 1000 columns with the monte carlo simulations (mc) or 
      # 10000 columns with the bootstrapped values (bs).
      
      # We first load the array with the response ratios in the static option, as this has to be loaded regardless of the value assigned to CI.  
      # This because the static results are the actual results (and the CI are used to quantify the uncertainties of the these results) 
      # and because they are needed to compute the CI in a consistent way when the bootstrapping is performed. 
      # More information are provided in the scientific publication to which this code refers and in the part of this script
      # where the CI are assigned to the final arrays. 
      
      if (cutoff == TRUE) { load(paste0(ecodata_path, "rr_z/rr_ecoregion_static.Rdata"))
        } else if (cutoff == FALSE) { load(paste0(ecodata_path, "rr_z/rr_ecoregion_static_nocutoff.Rdata"))}
      
      ratio_eco_static <- rr_ecoregion
########################################################################################################################################################################     
    #ratio_eco_static[,"SelectiveLogging",,] <- ratio_eco_static [,"ReducedImpactLogging",,]
########################################################################################################################################################################     


        if (CI == TRUE) {
          if(cutoff == TRUE){
            if (BS == TRUE) {   # the response ratios come from the bootstrapping process (boostrapping.R)
              load(paste0(ecodata_path, "rr_z/rr_ecoregion_bs.Rdata")) 
            } else if (BS == FALSE) {  # the response ratios come from the monte carlo simulations
              load(paste0(ecodata_path, "rr_z/rr_ecoregion_mc.Rdata"))
          }} else if (cutoff == FALSE) {
            if(BS == TRUE) { # the response ratios come from the bootstrapping process (boostrapping.R)
            load(paste0(ecodata_path, "rr_z/rr_ecoregion_bs_nocutoff.Rdata")) 
            } else if (BS == FALSE) {  # the response ratios come from the monte carlo simulations
            load(paste0(ecodata_path, "rr_z/rr_ecoregion_mc_nocutoff.Rdata"))
            }}
          
          ratio_eco <- rr_ecoregion

          }  
      
      rm(rr_ecoregion)
      
      # calculate.RR(uncertainties = uncertainties, distribution = distribution, n = n, cutoff = cutoff)        # four dimension array containing the response ratio per ecoregion (1st dimensio), land use category (2nd dimension) and taxa (3rd dimension).
      # The fourth dimension contains either two equal values corresponding to the median of the raw response ratios or the n simulations generated by the given distribution
      
      if (dim(ratio_eco_static)[4] != 2) {stop("Problem with the dimension of the array containing the response ratios (function: calculate.slost)")}
      
      if (CI == TRUE) {      if (dim(ratio_eco)[4] != n) {stop("Problem with the dimension of the array containing the response ratios (function: calculate.slost)")}}
      
      
      # Weights (which include the vulnerability scores) and original number of species 
      # this function (prepare.Sorg.VS.weighting) is defined in parameters_calculation.R)
      weight_tx = prepare.Sorg.VS.weighting(vulnerability = vulnerability, ecodata_path)[["Weights"]]    # dataframe containing the weighting factors (rows: ecoregions, columns: weights per taxonomic group)
      Sorg_VS = prepare.Sorg.VS.weighting(vulnerability = vulnerability, ecodata_path)[["Sorg_VS"]]      # dataframe containing the original number of species per ecoregion
      
      # Z VALUES
      # all .Rdata which are loaded here below contain a 2-dimension array called zvalues. 1dim: ecoregions;
      # 2dim: either the mean of the raw values (static option) or 1000 monte carlo simulations (mc) or 10000 bootstrapped medians (bs)

      # As for the static values of the response ratios, the static values of z are loaded regardless of the value of CI.   

      load(paste0(ecodata_path, "rr_z/zvalues_static.Rdata"))
      zvalues_static <- zvalues

      if(CI == TRUE & BS == TRUE) { # this loop will load another zvalues
        load(paste0(ecodata_path, "rr_z/zvalues_ecoregion_bs.Rdata")) 
        } else if (CI == TRUE & BS == FALSE) {
        load(paste0(ecodata_path, "rr_z/zvalues_ecoregion_mc.Rdata"))
        } 
      
      # This is the list which will be used as argument in the function which compute the static results. 
      parameters_static = list("ratio_eco" = ratio_eco_static, "weight_tx" = weight_tx, "Sorg_VS" = Sorg_VS, "zvalues" = zvalues_static)
      
      if(CI == TRUE) {
       # This is the list which will be used as argument in the function which compute the confidence intervals.
        parameters = list("ratio_eco" = ratio_eco, "weight_tx" = weight_tx, "Sorg_VS" = Sorg_VS, "zvalues" = zvalues)
        rm(ratio_eco)
      }
      
      rm(ratio_eco_static, weight_tx, Sorg_VS, zvalues)
      
      
      proc.time() - ptm      
        
    
          
############################# CALCULATION OF THE IMPACTS ############################# 
        
  # tsteps = c(2030, 2040, 2060, 2070, 2080, 2090) # seq(from = 2000, to = 2100, by = 10)  
  # ntsteps = length(tsteps) #time steps: 2000 -> 2100
  # tstep_areas = c(4,5,7,8,9,10)
  
  tsteps = seq(from = 2000, to = 2100, by = 10)  # temporal steps
 
  ntsteps = length(tsteps)
  tstep_areas = seq(from = 1, to = 11, by = 1)
 
  initial.time = Sys.time()
  
  ################### LOOPS OVER THE YEARS ################### 

    for (tstep in 1:ntsteps) { 
      ptm_year <- Sys.time()
      
      
        ################### PREPARATION OF AREAS ################### 
          
        print(paste0("time step (start): ", tsteps[tstep]))
        Areas <- myfiles[[tstep_areas[tstep]]]
        fr_Areas <- myfiles_fr[[tstep_areas[tstep]]]
        
        Areas <- Areas %>% rename(Eco_code = Ecoregion)         # Original natural areas, new natural areas, other land use areas
        fr_Areas <- fr_Areas %>% rename(Eco_code = Ecoregion)   # Given each land use group (e.g. Annual crop or permanent crop), there may be multiple 
        # land use sub-groups  (e.g., Annual crop in EU and Annual crop outside EU), which belong to the same ecoregion and have the same response ratio. 
        # fr_Areas is the share of each sub-group per land use. It is used to disaggregate the aggregated land use classes into the sub-classes.
        
        Scenarios = unique(Areas$Scenario)
        
        nscenarios = length(Scenarios)
        
        ################### PARALLEL LOOPS OVER THE SCENARIOS - FRACTION OF AREAS ################### 
        
          start.time = Sys.time()
          
          fr_Areas_new <- data.frame(matrix(NA, nrow = 0, ncol = 0)) 
          
          # Areas and fr_Areas come from the GLOBIOM model, however, GLOBIOM does not contain all the 
          # ecoregion available in the species loss model. Therefore, these gaps are filled here below and 
          # the ecoregion not considered in GLOBIOM are labeled as "NA".
  
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
            
        ################### PREPARE THE AREAS ################### 
        
          print(paste0("scenario (start): ", Scenarios[sc]))
          
          # Same procedure as for fr_Areas (the missing ecoregions in GLOBIOM are added here and labelled as "NA")
  
          Areas_temp <- Areas %>% filter(Scenario == toString(Scenarios[sc])) %>%
                          dplyr::select(-Scenario) %>%
                            full_join(Ecoregions) %>%
                              mutate(Eco_code = as.character(Eco_code)) %>%
                                arrange(Eco_code) %>%
                                  mutate(Eco_code = as.factor(Eco_code))
          
          Areas_org_new <- Areas_temp[2:3]     # extent of the orginal natural habitat per ecoregion (A_org) and extent of the current/new natural habitat per ecoregion (A_new) -> A_new = remaining natural area 
          rownames(Areas_org_new) <- Ecoregions$Eco_code
          Areas_lu <- Areas_temp[4:length(Areas_temp)] # dataframe with land use areas other than A_org and A_new 
          rownames(Areas_lu) <- Ecoregions$Eco_code
          
        ################### COMPUTE THE TOTAL IMPACTS AND THE IMPACTS PER TAXONOMIC GROUP ################### 
          
          # STATIC computation of the impacts (no confidence intervals)

          Slost <- calculate.slost(Total_RemainingNatural_areas = Areas_org_new, Land_use_areas = Areas_lu, param = parameters_static, CI = FALSE, BS) # function in calculate_slost.R
                    
          print("After Slost static")

            Slost_aggr_matrix <- Slost[["Aggr_matrix"]]             # general results
            Slost_taxa_matrix <- Slost[["PerTaxon_matrix"]]         # results per taxonomic group
            Slost_aggr2.5_matrix <- Slost[["Aggr2.5_matrix"]]       # 2.5% confidence intervals (in this case it will be all 0s)
            Slost_aggr97.5_matrix <- Slost[["Aggr97.5_matrix"]]     # 97.5% confidence intervals (in this case it will be all 0s)
                      
          # Arguments:
          # 1) Total_RemainingNatural_areas : dataframe with two columns, one with the total original areas, one with the remaining natural areas. The rows correspond to the
          # ecoregions and are sorted in alphabetical order according to the ecoregion codes (AA0101, AA0102, etc.)
          # 2) Land_use_areas : dataframe which contains one column per each land use category. The rows correspond to the
          # ecoregions and are sorted in alphabetical order according to the ecoregion codes (AA0101, AA0102, etc.)
          # 3) param = list with four elements: "ratio_eco" "weight_tx" "Sorg_VS"   "zvalues" 
          # 4) CI can be TRUE or FALSE. TRUE = the CI will be calculated, FALSE = the CI will not be calculated. WARNING: at this step, CI must be FALSE,
          # because here this function is used only to compute the median values. The calculation of the CI is computed at a later step (next if loop)
          # 5) vulnerability: TRUE or FALSE
          # 6) cutoff: TRUE or FALSE
          # 7) BS : TRUE or FALSE
          # Output:
          # list("Aggr_matrix"= Slost_aggr_median, "Aggr_df"= Slost_aggr_df, "PerTaxon_matrix"= Slost_taxa_matrix, "Pertaxon_df"= Slost_taxa_df,
          #      "Aggr2.5_matrix"= Slost_aggr_2.5, "Aggr97.5_matrix"= Slost_aggr_97.5)

          # CONFIDENCE INTERVALS (the confidence intervals are calculated only for the main results and not for each species group)
          
          if (CI == TRUE) {  # the function is the same used right above to compute the static results but in this case CI == TRUE and it computes the confidence intervals
            Slost_CI <- calculate.slost(Total_RemainingNatural_areas = Areas_org_new, Land_use_areas = Areas_lu, param = parameters, CI = TRUE, BS) # function in calculate_slost.R
        
            Slost_aggr2.5_matrix <- Slost_CI[["Aggr2.5_matrix"]]
            Slost_aggr97.5_matrix <- Slost_CI[["Aggr97.5_matrix"]]
            if(BS == TRUE) {
              Slost_aggr2.5_matrix <- 2*Slost[["Aggr_matrix"]] - Slost_CI[["Aggr97.5_matrix"]]
              Slost_aggr97.5_matrix <- 2*Slost[["Aggr_matrix"]] - Slost_CI[["Aggr2.5_matrix"]]
              
            # test
              test_index = sample(1:ncol(Slost_CI[["Aggr2.5_matrix"]]), 1)
              annual_crops_res <- 2*Slost[["Aggr_matrix"]][ , test_index] -Slost_CI[["Aggr97.5_matrix"]][ , test_index]
              annual_crops_test <- Slost_aggr2.5_matrix[ , test_index]
                if(all.equal(annual_crops_res, annual_crops_test) != TRUE){stop("Error in the calculation of the confidence intervals with bootstrapping (lower95)")}
              annual_crops_res <- 2*Slost[["Aggr_matrix"]][ , test_index] - Slost_CI[["Aggr2.5_matrix"]][ , test_index]
              annual_crops_test <- Slost_aggr97.5_matrix[ , test_index]
                if(all.equal(annual_crops_res, annual_crops_test) != TRUE){stop("Error in the calculation of the confidence intervals with bootstrapping (upper95)")}
              
              rm(annual_crops_res, annual_crops_test)
            }
          
          rm(Slost_CI, Slost)
              
          }
            

          # Slost_aggr_matrix <- test_matrix
          # Slost_aggr2.5_matrix <- test_matrix
          # Slost_aggr97.5_matrix <- test_matrix
          # Slost_taxa_matrix <- list("Plants" = test_matrix, "Birds" =  test_matrix, "Mammals" = test_matrix)
          # 
          Slost_list <- list("Total_impacts" = Slost_aggr_matrix, "Total_impacts_2.5q" = Slost_aggr2.5_matrix, "Total_impacts_97.5q" = Slost_aggr97.5_matrix,
                             "Plants" = Slost_taxa_matrix[["Plants"]], "Birds" = Slost_taxa_matrix[["Birds"]], "Mammals" = Slost_taxa_matrix[["Mammals"]])
          
          results[[sc]] <- Slost_list
          
          rm(Slost_list)
          
          }


          t = Sys.time() - start.time
          print(paste('Time - calculation of species loss over all scenarios:', round(t, 3), units(t)))
    
    
        ################### FOR LOOPS OVER THE SCENARIOS TO FILL OUT THE FINAL DATA FRAMES ################### 
      
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
          
          rm(t, start.time)
  
          rm(fr_Areas_new, Slost_fin, Slost_fin_plants, Slost_fin_birds, Slost_fin_mammals)
      
      
        ############################# SAVE THE RESULTS ############################# 
 
          write.csv(Slost_fin_disaggr, paste0(results_path, "/Slost_", tsteps[tstep],  "_", label, ".csv"), row.names = FALSE)
          write.csv(Slost_fin_disaggr_plants, paste0(results_path, "/Slost_", tsteps[tstep], "_", label, "_plants.csv"), row.names = FALSE)
          write.csv(Slost_fin_disaggr_birds, paste0(results_path, "/Slost_", tsteps[tstep],  "_", label, "_birds.csv"), row.names = FALSE)
          write.csv(Slost_fin_disaggr_mammals, paste0(results_path, "/Slost_", tsteps[tstep],  "_", label, "_mammals.csv"), row.names = FALSE)
        
      print(paste0("time step (end): ", tsteps[tstep]))
      
      ptm_endyear <- Sys.time() - ptm_year     
      
      print(paste0("Time (one year): ", round(ptm_endyear, 3), units(ptm_endyear)))
      
      
    }
  
  run_time = Sys.time() - initial.time
  print(paste('Total time:', round(run_time, 3), units(run_time)))
  
  rm(ptm_endyear, ptm_year, run_time)

      
#}
  

  
