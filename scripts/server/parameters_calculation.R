setwd("/home/frrosa/R/Forest_management/") # folder where this calculation takes places

library("triangle")                       # triangula distribution -> for the distribution of the z values
library(dplyr)                            # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(foreach)
library(fitdistrplus)
library(truncdist)
source("./CF_functions.R")                # source of all functions used here
source("./distributions.R")
select <- dplyr::select


  ############################# LOAD THE DATA ############################# 
  
  load.data <- function() {
    
      #Ecoregions
      
      Ecoregions_tot = read.csv("./ecoregions_data/Ecoregions_description.csv", header = TRUE) # Ecoregions specifications from Chaudhary 2015
      Ecoregions_tot$Eco_code <- as.factor(Ecoregions_tot$Eco_code)   # conversion of Ecoregions to factor (the inner_join usually converts the joining columns to characters)
      Ecoregions = Ecoregions_tot[,c("Eco_code", "Biome_ID", "Realm", "Ecoregion_name", "Assigned_habitat_type", "Ecoregion_Area_km2", "Global_area_share")]
    
      necoregions = length(Ecoregions$Eco_code)   #number of ecoregions
      CF_local_raw_blu = read.csv("./ecoregions_data/CF_local.csv", header = TRUE) # Local CF from SI of Chaudhary 2015, blu = broad land use (annual, permanent, pasture, urban)

      forest_management <- read.csv("./ecoregions_data/CF_local_forest-use.csv", header = TRUE, col.names = c("Taxa_Used", "Land_use_type", "Ratio", "Local_CF")) # Local CF and response ratio from SI of Chaudhary 2016
      
      result <- list("Ecoregions" = Ecoregions, "Ecoregion_number" = necoregions, "Ecoregions_Sorg_VS" = Ecoregions_tot,
                     "CF_local_raw_blu" = CF_local_raw_blu, "forest_management" = forest_management)
      return(result)
      
      
    }
  
  
  ############################# PREPARE THE Z VALUES ############################# 
  
  # output: zvalues (array)
  prepare.zvalues <- function(distribution, n) {  # distribution can be "OFF" (no simulations are generated for the z values) 
                                                  # or contain a character string which defines the distribution to use for the simulations of the response ratios.
                                                  # The default value is "OFF". n instead is the number of simulations that will be generated if distribution != "OFF".
    # task: 
    # assign the z values to all ecoregion 
    # generate simulations from a triangular distribution if distribution == "ON"
    # output: array whose rows are the ecoregions and whose columns are the z values
    
    if(missing(n)) {n = 1000}
        if (distribution == "OFF") {simulations = 2} else if (distribution != "OFF") {simulations = n}
    
      # load the data on the ecoregions
        data_loaded <- load.data()
        Ecoregions <- data_loaded[["Ecoregions"]]
        necoregions <- data_loaded[["Ecoregion_number"]]
      
      # from Supplementary materials (Table S2) of De Baan (2013), Use in Life Cycle Assessment: Global Characterization Factors Based on Regional and Global Potential Species Extinction
      
        z = c(0.258,0.344,0.211)          #z for island, forest and nonforest
        z_lower = c(0.242, 0.307, 0.185)
        z_upper = c(0.282, 0.384, 0.247)
      
        zvalues <- data.frame(matrix(NA, nrow=necoregions, ncol=simulations + 1))     # create an empty dataframe to store the value of z for each ecoregion 
        names(zvalues)[1] = "Eco_code"
        zvalues <- zvalues %>% mutate(Eco_code = Ecoregions$Eco_code)                 # add a column with the ecoregion codes
        z_2d_sim <- array(data = NA, dim = c(length(z), simulations), dimnames = list(c("island", "forest", "non-forest"), c(1:simulations)))     # create an empty array to store the result of the simulatio
        
      # if distribution == "ON", generate 1000 simulations using a triangula distribution function
      
        for (i in 1:length(z)) {
          if (distribution == "OFF") {
            z_2d_sim[i,] = z[i]
          } else {
            z_2d_sim[i,] = rtriangle(n = simulations, a = z_lower[i], b = z_upper[i], c = z[i])
          }
        }
        
      # assign the corresponding simulation result to each ecoregion according to the habitat type (islands, forests, not forests)
      
        Ecoregions[,"z_values"] <- NA     # add a column to Ecoregions, to store the z values
        
        for (i in 1:nrow(Ecoregions)) {
          
          if (Ecoregions$Assigned_habitat_type[i] == "island") {
            Ecoregions$z_values[i] = z[1]
            zvalues[i,2:length(zvalues)] = z_2d_sim[1,]
          } else if (Ecoregions$Assigned_habitat_type[i] == "forest") {
            Ecoregions$z_values[i] = z[2]
            zvalues[i,2:length(zvalues)] = z_2d_sim[2,]
          } else if (Ecoregions$Assigned_habitat_type[i] == "non-forest") {
            Ecoregions$z_values[i] = z[3]
            zvalues[i,2:length(zvalues)] = z_2d_sim[3,]
          }
        }
        
        zvalues <- array(zvalues[2:length(zvalues)], dimnames = list(Ecoregions$Eco_code, 1:simulations))   # convert of the dataframe to an array (this format is needed to use the functions when species lost are computed)
        
        rm(z_2d_sim, z_lower, z_upper)
        
        return(zvalues)
    }
   
  
  ############################# WEIGHTING ############################# 

  # output: result (list("Weights" = weight_tx, "Sorg_VS" = Sorg_VS))
  prepare.Sorg.VS.weighting <- function(vulnerability) { # Vulnerability can be "ON" (Sorg is multiplied by the vulnerability scores and the weighting 
    # used in the aggregation over taxonimic groups takes into account the world VS) or "OFF". Default is "ON".
    
    # task:         
    # calculate the weighting factors used to covert regional impacts into global impacts 
    # using the original number of species (Sorg), the vulnerability scores (VS) and the total number of species (Sorg_tot)
    # output: dataframe containing the weighting factors (rows: ecoregions, columns: weights per taxonomic group)
            # dataframe containing the product between the original number of species and the vulnerability scores (rows: ecoregions, columns: product per taxonomic group) 
    
    if(missing(vulnerability)) {vulnerability = "ON"}
    
    # load the ecoregion data
    data_loaded <- load.data()
    Ecoregions_tot <- data_loaded[["Ecoregions_Sorg_VS"]]
    
    # select the columns with the values corresponding to Sorg and VS
    Sorg = Ecoregions_tot[,c("Sorg_plants", "Sorg_birds", "Sorg_mammals")] #number of species per each taxa in the natural habitat 
    Sorg = data.matrix(Sorg)
    rownames(Sorg) = Ecoregions_tot$Eco_code
    VS = Ecoregions_tot[,c("VS_plants", "VS_birds", "VS_mammals")]         #vulnerabitliy scores
    VS <- data.matrix(VS)
    rownames(VS) = Ecoregions_tot$Eco_code
    
    # total number of species used for the final weighting  
    Sorg_tot_pl = 321212  # total number of plant species (from LC-Impact report)
    Sorg_tot_b = 10104    # total number of bird species (from LC-Impact report)
    Sorg_tot_m = 5490    # total number of mammal species (from LC-Impact report)
    
    if (vulnerability == "ON") {
      
      VS_world_pl = 1.00
      VS_world_b = 0.29
      VS_world_m = 0.44
      
      Sorg_VS = Sorg*VS
      colnames(Sorg_VS) = c("Plants", "Birds", "Mammals")
      
      weight_tx <- Sorg_VS
      weight_tx[,"Plants"] <- (0.5/(Sorg_tot_pl*VS_world_pl))
      weight_tx[,"Birds"] <- (0.5/((dim(Sorg)[2]-1)*Sorg_tot_b*VS_world_b))
      weight_tx[,"Mammals"] <- (0.5/((dim(Sorg)[2]-1)*Sorg_tot_m*VS_world_m))
      
    } else if (vulnerability == "OFF") {
      
      Sorg_VS = Sorg
      colnames(Sorg_VS) = c("Plants", "Birds", "Mammals")
      weight_tx <- Sorg_VS
      weight_tx[,"Plants"] <- (0.5/Sorg_tot_pl)
      weight_tx[,"Birds"] <- (0.5/((dim(Sorg)[2]-1)*Sorg_tot_b))
      weight_tx[,"Mammals"] <- (0.5/((dim(Sorg)[2]-1)*Sorg_tot_m))
    }
    
    result = list("Weights" = weight_tx, "Sorg_VS" = Sorg_VS)
    return(result)
  }
  
    
  ############################# CALCULATE CF ############################# 

  # output: ratio_eco (array)
  calculate.RR <- function(distribution, n, cutoff) { # distribution can be "OFF" (no simulations are generated for the z values) 
                                                      # or contain a character string which defines the distribution to use for the simulations of the response ratios.
                                                      # The default value is "OFF". Available character strings for fitting the response ratios: "lognormal" or "Weibull" or "OFF" (OFF means that only the mode of the data will be calculated, therefore later it will not be possible to ) - default: "OFF", # cutoff for sensitivity (all RR > 1 are set to 1): TRUE or FALSE, default: FALSE
                                                      # n instead is the number of simulations that will be generated if distribution != "OFF".
                                                      # cutoff can be "TRUE" (response ratios larger than 1 are excluded when the median value is calculated) or "FALSE", default is "FALSE".
    # task: 
    # compile a four dimension array containing the response ratio per ecoregion, land use category and taxa.
    # The fourth dimension contains either two equal values corresponding to the median of the raw response ratios or the n simulations generated by teh given distribution
    
    if (missing(distribution)) {distribution = "OFF"}
    if (missing(n)) {n = 1000}
    if (missing(cutoff)) { cutoff = TRUE}
    if (distribution == "OFF") {simulations = 2} else if (distribution != "OFF") {simulations = n}
  
    ########### LOAD THE DATA AND PREPARE THE CF ###########
    
      # load data
      
      data_loaded <- load.data()
      CF_local_raw_blu = data_loaded[["CF_local_raw_blu"]]      # dataframe: Local CF for broad land use categories (blu)
      forest_management = data_loaded[["forest_management"]]    # dataframe: Local CF and response ratio for forest management catogories
      Ecoregions = data_loaded[["Ecoregions"]]                  # dataframe: Ecoregion information
      necoregions = data_loaded[["Ecoregion_number"]]           # number: number of ecoregions
      
      # CF LOCAL- Broad land use classes 
        # blu = broad land use (annual, permanent, pasture, urban)
        
        land_use_type = unique(CF_local_raw_blu$Land_use_type)  # list of land use types 
        land_use_ID = unique(CF_local_raw_blu$Land_use_ID)      # list of land use ID 
        nlanduse = length(land_use_ID)                          # number of land use types 
        
        
        taxa = unique(CF_local_raw_blu$Taxa_Used) #list of taxa
        ntaxa = length(taxa)                      #numbert of taxa 
        
        # land use types, land use ID and taxa: ("Annual", "Permanent", "Pasture", "Urban"), ("5.1", "5.2", "4.1", "7.1"), ("Plants", "Birds", "Mammals")
        
        biomes = c(1:14, "Artificial")  # unique(CF_local_raw$Biome_ID). Only 13% of raw local CF assigned to Urban areas were also assigned to a biome, of which only two biomes have enough data to calculate the mean. Therefore, I created an additional "biome" class, called "Artificial", where to store all raw data for urban areas, regardless of the biome.
        nbiome = length(biomes)         # number of biomes
        
        CFloc_blu = CFcalc(CF_local_raw_blu, taxa, land_use_type, biomes)	# Local CF (source file: CF_function.R)
        
        CFloc_3d_blu = CFloc_blu[[1]]       # 3d array containing local CF: median of CF per biome, land use and taxa (if less than 5 data points are available, data are aggregated by land use and taxa or only by land use)
        CFloc_group_blu = CFloc_blu[[2]]    # list of grouped CF (raw data are grouped by biome, land use type and taxa, if less than 5 data points are available, data are grouped by land use and taxa or only by land use)
        CFloc_group_in_blu = CFloc_blu[[3]] # list of indexes to define each element of CFloc_group_blu. E.g., CFloc_group_in_blu[[1]] = c(1,1,1), because CFloc_group_blu[[1]] contains the grouped local CF corresponding to the first element of the vectors biomes, land_use_type and taxa
        
      #CF LOCAL - Forest use classes  
      
        forest_management$Biome_ID <- "Global" 
        
        f_use = unique(forest_management$Land_use_type)
        f_biomes = "Global"
        nfuse = length(f_use)
        
        #forest uses: PlantationNonTimber, ClearCut, Retention, PlantationFuel, SlashAndBurn, SelectionSystem, SelectiveLogging, Agroforestry, Plantation, ReducedImpactLogging
        
        CFloc_forest = CFcalc_group(forest_management, taxa, f_use, f_biomes) 
        
        CFloc_group_for = CFloc_forest[[2]]       # List. Each element contains the local CF grouped by forest use category and taxa
        
        ##### CHANGE THE FOREST USE FOR THE EXTENSIVE AND INTENSIVE ####
        
        f_use_ex = f_use[c(3,6,7)] # Retention SelectionSystem SelectiveLogging 
        f_use_in = f_use[c(2,4,9)] # ClearCut PlantationFuel PlantationTimber Plantation NonTimber
        f_use_pl = f_use[c(1,4,9)] # PlantationNonTimber PlantationFuel PlantationTimber
        
        CFloc_group_for_aggr = list()
        names_for = c("Extensive", "Intensive", "Plantations")
        f_use_aggr = list(f_use_ex, f_use_in, f_use_pl)
        names(f_use_aggr) = names_for
        
        for (j in 1:length(names_for)) {
          for (k in 1:ntaxa) {
            CFloc_group_for_aggr[[paste(names_for[j], taxa[k], f_biomes, sep = "_")]] = unname(unlist(CFloc_group_for[paste(f_use_aggr[[names_for[j]]], taxa[k], f_biomes, sep = "_")]))
          }
        }
      
        # this are the combinations of forest use and taxa for which there are not enough data -> an aggregation is needed
        
          CFloc_group_for[["SelectionSystem_Birds_Global"]] = CFloc_group_for_aggr[["Extensive_Birds_Global"]]
          CFloc_group_for[["ClearCut_Mammals_Global"]] = CFloc_group_for_aggr[["Intensive_Mammals_Global"]]
          CFloc_group_for[["PlantationFuel_Mammals_Global"]] = CFloc_group_for_aggr[["Plantations_Mammals_Global"]]
          CFloc_group_for[["SelectionSystem_Mammals_Global"]] = CFloc_group_for_aggr[["Extensive_Mammals_Global"]]
          CFloc_group_for[["Plantation_Mammals_Global"]] = CFloc_group_for_aggr[["Plantations_Mammals_Global"]]
          CFloc_group_for[["ReducedImpactLogging_Mammals_Global"]] = CFloc_group_for_aggr[["Extensive_Mammals_Global"]]
        
        # afforested CF come from recovery times 
          if (distribution == "OFF") {
            CFloc_group_for_afforested = 1-0.957
          } else {
            param_afforested = lnorm_parametrized(c(0.932, 1))                                                                # this are the distribution paramenters for the response ratio RR (0.932 = RR after one year of recovery, 1 = ideal final state)
            CFloc_group_for_afforested = 1 - rlnorm(1000, meanlog = param_afforested$meanlog, sdlog = param_afforested$sdlog) # CF = 1-RR
          }
        # there is no distinction between taxa, because we took the most-conservative scenario
        
          CFloc_group_for[["Afforested_Plants_Global"]] = CFloc_group_for_afforested
          CFloc_group_for[["Afforested_Birds_Global"]] = CFloc_group_for_afforested
          CFloc_group_for[["Afforested_Mammals_Global"]] = CFloc_group_for_afforested
          
        # creation of elements in the list also for extensive and intensive forest use
        
          CFloc_group_for[["ForExtensive_Plants_Global"]] = CFloc_group_for_aggr[["Extensive_Plants_Global"]]
          CFloc_group_for[["ForExtensive_Birds_Global"]] = CFloc_group_for_aggr[["Extensive_Birds_Global"]]
          CFloc_group_for[["ForExtensive_Mammals_Global"]] = CFloc_group_for_aggr[["Extensive_Mammals_Global"]]
          
          CFloc_group_for[["ForIntensive_Plants_Global"]] = CFloc_group_for_aggr[["Intensive_Plants_Global"]]
          CFloc_group_for[["ForIntensive_Birds_Global"]] = CFloc_group_for_aggr[["Intensive_Birds_Global"]]
          CFloc_group_for[["ForIntensive_Mammals_Global"]] = CFloc_group_for_aggr[["Intensive_Mammals_Global"]]
        
        # the indices must be updated too
        
          CFloc_group_in_for = CFloc_forest[[3]]                              # indices referring to the biome (only one because there is no biome differentiation), forest use and taxa
          CFloc_group_in_for[[length(CFloc_group_in_for) + 1]] = c(1,11,1)    # the second element starts from 11 because there are 10 previous forest use classes 
          CFloc_group_in_for[[length(CFloc_group_in_for) + 1]] = c(1,11,2)
          CFloc_group_in_for[[length(CFloc_group_in_for) + 1]] = c(1,11,3)
          
          CFloc_group_in_for[[length(CFloc_group_in_for) + 1]] = c(1,12,1)
          CFloc_group_in_for[[length(CFloc_group_in_for) + 1]] = c(1,12,2)
          CFloc_group_in_for[[length(CFloc_group_in_for) + 1]] = c(1,12,3)
          
          CFloc_group_in_for[[length(CFloc_group_in_for) + 1]] = c(1,13,1)
          CFloc_group_in_for[[length(CFloc_group_in_for) + 1]] = c(1,13,2)
          CFloc_group_in_for[[length(CFloc_group_in_for) + 1]] = c(1,13,3)
        
          f_use <- c(as.character(f_use),"Afforested",  "ForExtensive", "ForIntensive")
          nfuse = length(f_use)
        #"PlantationNonTimber"  "ClearCut"             "Retention"            "PlantationFuel"       "SlashAndBurn"         "SelectionSystem"      "SelectiveLogging"     "Agroforestry"         "Plantation"          
        # "ReducedImpactLogging" "Afforested"           "ForExtensive"         "ForIntensive"
        
          rm(names_for, f_use_ex, f_use_in, f_use_pl)
    
        
        
    ########### MEDIAN CALCULATION WHEN DISTRIBUTION IS OFF AND MONTE-CARLO SIMULATION WHEN DISTRIBUTION IS ONE OF THE AVAILABLE ONES ###########

        
      #f_use_new <- c("ClearCut", "Retention", "PlantationFuel", "SelectionSystem", "SelectiveLogging", "Afforested", "ForExtensive", "ForIntensive")
    
      #CF LOCAL - broad land use classes
      
        ratio_blu <- lapply(CFloc_group_blu, function(x) 1-x) #from CF to ratio
        # ratio_blu <- lapply(ratio_blu, function(x) sapply(x, function(x) if (x == 0) {x = 0.4*10^(-8)} else {x = x}))
        ratio_blu <- lapply(ratio_blu, function(x) sapply(x, function(x) if (x == 0) {x = NA} else {x = x}))
        ratio_blu <- lapply(ratio_blu, function(x) x[!is.na(x)])
        
        ratio_sim_blu <- array(data = NA, dim= c(nbiome, nlanduse, ntaxa, simulations), dimnames = list(biomes,land_use_type, taxa, c(1:simulations)))
        
        
      #CF LOCAL - forest use classes
      
      #removes 0 from CFloc_group_for
      
        ratio_for <- lapply(CFloc_group_for, function(x) 1-x) #from CF to ratio
        ratio_for <- lapply(ratio_for, function(x) sapply(x, function(x) if (x == 0) {x = NA} else {x = x}))
        ratio_for <- lapply(ratio_for, function(x) x[!is.na(x)])
        
      #nfuse = length(f_use_new)
      
      # ratio_for <- ratio_for_full[-c(1:3, 13:15, 22:30)]
        ratio_group_in_for <- CFloc_group_in_for
        
        ratio_sim_for <- array(data = NA, dim= c(1, nfuse, ntaxa, simulations), dimnames = list(f_biomes, f_use, taxa, c(1:simulations)))
        
      # Simulations 
      
        if (distribution == "OFF") {
          
          if(cutoff == TRUE) {
            ratio_blu <- lapply(ratio_blu, function(x) sapply(x, function(x) if (x > 1) {x = 1} else {x = x}))
            ratio_for <- lapply(ratio_for, function(x) sapply(x, function(x) if (x > 1) {x = 1} else {x = x}))
          }
          
          for (counter in 1:length(ratio_blu)) {
            set.seed(counter)
            ratio_sim_blu[CFloc_group_in_blu[[counter]][1],CFloc_group_in_blu[[counter]][2],CFloc_group_in_blu[[counter]][3],] = 
              c(median(ratio_blu[[counter]]),median(ratio_blu[[counter]]))
            
          }
          
          #if (sum(abs(sort(c(ratio_sim_blu[,,,1]))-sort(unlist(lapply(ratio_blu, function(x) median(x)))))) > 1e-16) {stop("ERROR in median calculation (distribution 'OFF')")}
          
          for (counter in 1:length(ratio_for)) {
            set.seed(counter)
            ratio_sim_for[ratio_group_in_for[[counter]][1],ratio_group_in_for[[counter]][2],ratio_group_in_for[[counter]][3],] = 
              c(median(ratio_for[[counter]]), median(ratio_for[[counter]]))      
          }
          
          if (sum(abs(sort(c(ratio_sim_for[1,,,1]))-sort(unlist(lapply(ratio_for, function(x) median(x)))))) > 1e-16) {stop("ERROR in median calculation (disrtibution 'OFF')")}
          
        } else  if (distribution == "lognormal") {
          
          for (counter in 1:length(ratio_blu)) {
            set.seed(counter)
            ratio_sim_blu[CFloc_group_in_blu[[counter]][1],CFloc_group_in_blu[[counter]][2],CFloc_group_in_blu[[counter]][3],] = 
              rtrunc(simulations, spec="lnorm", a=0, b=max(ratio_blu[[counter]]), meanlog = mean(log(ratio_blu[[counter]])), sdlog = sd(log(ratio_blu[[counter]])))      
          }
          
          for (counter in 1:length(ratio_for)) {
            set.seed(counter)
            ratio_sim_for[ratio_group_in_for[[counter]][1],ratio_group_in_for[[counter]][2],ratio_group_in_for[[counter]][3],] = 
              rtrunc(simulations, spec="lnorm", a=0, b=max(ratio_for[[counter]]), meanlog = mean(log(ratio_for[[counter]])), sdlog = sd(log(ratio_for[[counter]])))            
          }
          
        } else if (distribution == "Weibull") {
          
          for (counter in 1:length(ratio_blu)) {
            set.seed(counter)
            fw = fitdist(ratio_blu[[counter]], "weibull")
            shape_w = fw$estimate[1] 
            scale_w = fw$estimate[2]
            ratio_sim_blu[CFloc_group_in_blu[[counter]][1],CFloc_group_in_blu[[counter]][2],CFloc_group_in_blu[[counter]][3],] = 
              #rtrunc(simulations, spec="weibull", a=0, b=max(ratio_blu[[counter]]), shape = shape_w, scale = scale_w)
              rtrunc(simulations, spec="weibull", a=0, b=1, shape = shape_w, scale = scale_w)
            
          }
          
          rm(fw, shape_w, scale_w)
          for (counter in 1:length(ratio_for)) {
            set.seed(counter)
            fw = fitdist(ratio_for[[counter]], "weibull")
            shape_w = fw$estimate[1] 
            scale_w = fw$estimate[2]
            ratio_sim_for[ratio_group_in_for[[counter]][1],ratio_group_in_for[[counter]][2],ratio_group_in_for[[counter]][3],] = 
              #rtrunc(simulations, spec="weibull", a=0, b=max(ratio_for[[counter]]), shape = shape_w, scale = scale_w)
              rtrunc(simulations, spec="weibull", a=0, b=1, shape = shape_w, scale = scale_w)
            
          }
          
          rm(fw, shape_w, scale_w)
          
        }
      
        ratio_eco_blu <- array(data = NA, dim = c(necoregions,nlanduse,ntaxa,simulations), dimnames = list(unique(Ecoregions$Eco_code),land_use_type,taxa,c(1:simulations)))
        for (i in 1:necoregions) {
          pos = which(dimnames(ratio_sim_blu)[[1]] == Ecoregions$Biome_ID[i])
          ratio_eco_blu[i,,,] = ratio_sim_blu[pos,,,]
        }
        
        Eco_code <- Ecoregions %>% dplyr::select(Eco_code)                     # create a dataframe with only one column containing the codes of the ecoregions
        Ecoregions_global <- Eco_code %>% mutate(Biome_ID = "Global")
        ratio_eco_for <- array(data = NA, dim = c(necoregions,nfuse,ntaxa,simulations), dimnames = list(unique(Ecoregions$Eco_code),f_use,taxa,c(1:simulations)))
        
        for (i in 1:necoregions) {
          pos = which(dimnames(ratio_sim_for)[[1]] == Ecoregions_global$Biome_ID[i])
          ratio_eco_for[i,,,] = ratio_sim_for[pos,,,]
        }
        
        rm(pos)
      
      #Merging the two array
      
      ratio_eco <- abind(ratio_eco_blu, ratio_eco_for, along = 2)
      ratio_eco_backup = ratio_eco
      
    return(ratio_eco)    
    
    }      
    
  

  #### data have been saved until this point ====
  
  #return(list("ratio_eco" = ratio_eco, "zvalues" = zvalues, "weight_taxa" = weight_tx, "Sorg_VS" = Sorg_VS))


