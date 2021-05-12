setwd("/home/frrosa/R/Forest_management/") # folder where this calculation takes places

library("triangle")                       # triangulaR distribution -> for the distribution of the z values
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
      CF_local_landuse = read.csv("./ecoregions_data/CF_local.csv", header = TRUE) # Local CF from SI of Chaudhary 2015, blu = broad land use (annual, permanent, pasture, urban)

      CF_local_forestmng <- read.csv("./ecoregions_data/CF_local_forest-use.csv", header = TRUE, col.names = c("Taxa_Used", "Land_use_type", "Ratio", "Local_CF")) # Local CF and response ratio from SI of Chaudhary 2016
      
      z_rawvalues <- read.csv("./ecoregions_data/z_raw_values.csv", header = TRUE)
      
      result <- list("Ecoregions" = Ecoregions, "Ecoregion_number" = necoregions, "Ecoregions_Sorg_VS" = Ecoregions_tot,
                     "CF_local_landuse" = CF_local_landuse, "CF_local_forestmng" = CF_local_forestmng, "z_rawvalues" = z_rawvalues)
      return(result)
      
      
    }
  
  
  ############################# PREPARE THE Z VALUES ############################# 
  
  # output: zvalues (array)
  prepare.zvalues <- function(uncertainties) {  # uncertainties can be FALSE (no simulations are generated for the z values) 
                                                  # or contain a character string which defines the uncertainties to use for the simulations of the response ratios.
                                                  # The default value is FALSE. n instead is the number of simulations that will be generated if uncertainties != FALSE.
    # task: 
    # assign the z values to all ecoregion 
    # generate simulations from a triangular distribution if uncertainties == TRUE
    # output: array whose rows are the ecoregions and whose columns are the z values
    
    n = 1000
    if (uncertainties == FALSE) {simulations = 2} else if (uncertainties != FALSE) {simulations = n}
    
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
        
      # if uncertainties == TRUE, generate 1000 simulations using a triangular distribution function
      
        for (i in 1:length(z)) {
          if (uncertainties == FALSE) {
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
  prepare.Sorg.VS.weighting <- function(vulnerability) { # Vulnerability can be TRUE (Sorg is multiplied by the vulnerability scores and the weighting 
    # used in the aggregation over taxonimic groups takes into account the world VS) or FALSE. Default is TRUE.
    
    # task:         
    # calculate the weighting factors used to covert regional impacts into global impacts 
    # using the original number of species (Sorg), the vulnerability scores (VS) and the total number of species (Sorg_tot)
    # output: dataframe containing the weighting factors (rows: ecoregions, columns: weights per taxonomic group)
            # dataframe containing the product between the original number of species and the vulnerability scores (rows: ecoregions, columns: product per taxonomic group) 
    
    if(missing(vulnerability)) {vulnerability = TRUE}
    
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
    
    if (vulnerability == TRUE) {
      
      VS_world_pl = 1.00
      VS_world_b = 0.29
      VS_world_m = 0.44
      
      Sorg_VS = Sorg*VS
      colnames(Sorg_VS) = c("Plants", "Birds", "Mammals")
      
      weight_tx <- Sorg_VS
      weight_tx[,"Plants"] <- (0.5/(Sorg_tot_pl*VS_world_pl))
      weight_tx[,"Birds"] <- (0.5/((dim(Sorg)[2]-1)*Sorg_tot_b*VS_world_b))
      weight_tx[,"Mammals"] <- (0.5/((dim(Sorg)[2]-1)*Sorg_tot_m*VS_world_m))
      
    } else if (vulnerability == FALSE) {
      
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
  
    
  ############################# PREPARE RR ############################# 

  # output: (array)
  prepare.RR <- function(uncertainties, cutoff) { # uncertainties can be FALSE (no simulations are generated for the z values) 
                                                      # or contain a character string which defines the distribution to use for the simulations of the response ratios.
                                                      # The default value is FALSE. Available character strings for fitting the response ratios: "lognormal" or "Weibull" or FALSE (OFF means that only the mode of the data will be calculated, therefore later it will not be possible to ) - default: FALSE, # cutoff for sensitivity (all RR > 1 are set to 1): TRUE or FALSE, default: FALSE
                                                      # n instead is the number of simulations that will be generated if uncertainties != FALSE.
                                                      # cutoff can be "TRUE" (response ratios larger than 1 are excluded when the median value is calculated) or "FALSE", default is "FALSE".
    # output:
    # rr_land_use = list containing the grouped response ratios. Order of grouping: biome, land use, taxon. This means that the first element will be
    # e.g. Annual_Plants_1 (land-use_taxon_biome), the second Annual_Birds_1, the third Annual_Mammals_1, the fourth Permanent_Plants_1 etc.
    # rr_forest_mng = list containing the grouped response ratios. Same order as rr_land_use (though here there is only one biome, called "Global")
    # dim = vector containing the number of: biomes, ecoregions, land use types, forest management categories, taxa, simulations (this order must be kept like 
    # this). It is called dim because these numbers will define the dimension of the array built in this function. 
    # names_dim = list with the names of the dimensions: biomes, land use types, forest management categories, taxa, simulations.
    # indices_land_use = list where each element contains three numbers indicating which biome, land use and taxa are in the rr_land_use list. E.g., 
    # the first element is 111, which refer to Annual_Plants_1 of rr_land_use (Annual = first land use in the names_dim list, Plants = first taxa in
    # the names_dim list, 1 = first biome in the names_dim list).
    # indices_forest_mng = same as indices_land_use but for rr_forest_mng. 
    
  
    ########### LOAD THE DATA AND PREPARE THE CF ###########
    
      # load data
      
      data_loaded <- load.data()
      CF_local_landuse = data_loaded[["CF_local_landuse"]]      # dataframe: Local CF for broad land use categories (blu)
      CF_local_forestmng = data_loaded[["CF_local_forestmng"]]    # dataframe: Local CF and response ratio for forest management catogories
      Ecoregions = data_loaded[["Ecoregions"]]                  # dataframe: Ecoregion information
      necoregions = data_loaded[["Ecoregion_number"]]           # number: number of ecoregions
      
      # CF LOCAL- Broad land use classes 
        # blu = broad land use (annual, permanent, pasture, urban)
        
        land_use_types = unique(CF_local_landuse$Land_use_type)  # list of land use types 
        land_use_ID = unique(CF_local_landuse$Land_use_ID)      # list of land use ID 
        nlanduse = length(land_use_ID)                          # number of land use types 
        
        taxa = unique(CF_local_landuse$Taxa_Used) #list of taxa
        ntaxa = length(taxa)                      #numbert of taxa 
        
        # land use types, land use ID and taxa: ("Annual", "Permanent", "Pasture", "Urban"), ("5.1", "5.2", "4.1", "7.1"), ("Plants", "Birds", "Mammals")
        
        biomes = c(1:14, "Artificial")  # unique(CF_local_raw$Biome_ID). Only 13% of raw local CF assigned to Urban areas were also assigned to a biome, of which only two biomes have enough data to calculate the mean. Therefore, I created an additional "biome" class, called "Artificial", where to store all raw data for urban areas, regardless of the biome.
        nbiome = length(biomes)         # number of biomes
        
        CFloc_landuse = CFcalc(CF_local_landuse, taxa, land_use_types, biomes)	# Local CF (source file: CF_function.R)
        
        CFloc_group_landuse = CFloc_landuse[[2]]    # list of grouped CF (raw data are grouped by biome, land use type and taxa, if less than 5 data points are available, data are grouped by land use and taxa or only by land use)
        indices_land_use = CFloc_landuse[[3]] # list of indexes to define each element of CFloc_group_landuse. E.g., indices_land_use[[1]] = c(1,1,1), because CFloc_group_landuse[[1]] contains the grouped local CF corresponding to the first element of the vectors biomes, land_use_types and taxa
        
      #CF LOCAL - Forest use classes  
      
        CF_local_forestmng$Biome_ID <- "Global" 
        
        forest_mng_categories = unique(CF_local_forestmng$Land_use_type)
        f_biomes = "Global"
        nfuse = length(forest_mng_categories)
        
        #forest uses: PlantationNonTimber, ClearCut, Retention, PlantationFuel, SlashAndBurn, SelectionSystem, SelectiveLogging, Agroforestry, Plantation, ReducedImpactLogging
        
        CFloc_forest = CFcalc_group(CF_local_forestmng, taxa, forest_mng_categories, f_biomes) 
        
        CFloc_group_for = CFloc_forest[[2]]       # List. Each element contains the local CF grouped by forest use category and taxa
        
        ##### CHANGE THE FOREST USE FOR THE EXTENSIVE AND INTENSIVE ####
        
        forest_mng_categories_ex = forest_mng_categories[c(3,6,7)] # Retention SelectionSystem SelectiveLogging 
        forest_mng_categories_in = forest_mng_categories[c(1,2,4,9)] # ClearCut PlantationFuel PlantationTimber PlantationNonTimber
        forest_mng_categories_pl = forest_mng_categories[c(1,4,9)] # PlantationNonTimber PlantationFuel PlantationTimber
        
        CFloc_group_for_aggr = list()
        names_for = c("Extensive", "Intensive", "Plantations")
        forest_mng_categories_aggr = list(forest_mng_categories_ex, forest_mng_categories_in, forest_mng_categories_pl)
        names(forest_mng_categories_aggr) = names_for
        
        for (j in 1:length(names_for)) {
          for (k in 1:ntaxa) {
            CFloc_group_for_aggr[[paste(names_for[j], taxa[k], f_biomes, sep = "_")]] = unname(unlist(CFloc_group_for[paste(forest_mng_categories_aggr[[names_for[j]]], taxa[k], f_biomes, sep = "_")]))
          }
        }
      
        # this are the combinations of forest use and taxa for which there are not enough data -> an aggregation is needed
        
          CFloc_group_for[["SelectionSystem_Birds_Global"]] = CFloc_group_for_aggr[["Extensive_Birds_Global"]]
          #CFloc_group_for[["ClearCut_Mammals_Global"]] = CFloc_group_for_aggr[["Intensive_Mammals_Global"]]
          CFloc_group_for[["PlantationFuel_Mammals_Global"]] = CFloc_group_for_aggr[["Intensive_Mammals_Global"]]
          CFloc_group_for[["SelectionSystem_Mammals_Global"]] = CFloc_group_for_aggr[["Extensive_Mammals_Global"]]
          CFloc_group_for[["Plantation_Mammals_Global"]] = CFloc_group_for_aggr[["Intensive_Mammals_Global"]]
          CFloc_group_for[["ReducedImpactLogging_Mammals_Global"]] = CFloc_group_for_aggr[["Extensive_Mammals_Global"]]
        
        # afforested CF come from recovery times 
          if (uncertainties == FALSE) {
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
        
          indices_forest_mng = CFloc_forest[[3]]                              # indices referring to the biome (only one because there is no biome differentiation), forest use and taxa
          indices_forest_mng[[length(indices_forest_mng) + 1]] = c(1,11,1)    # the second element starts from 11 because there are 10 previous forest use classes 
          indices_forest_mng[[length(indices_forest_mng) + 1]] = c(1,11,2)
          indices_forest_mng[[length(indices_forest_mng) + 1]] = c(1,11,3)
          
          indices_forest_mng[[length(indices_forest_mng) + 1]] = c(1,12,1)
          indices_forest_mng[[length(indices_forest_mng) + 1]] = c(1,12,2)
          indices_forest_mng[[length(indices_forest_mng) + 1]] = c(1,12,3)
          
          indices_forest_mng[[length(indices_forest_mng) + 1]] = c(1,13,1)
          indices_forest_mng[[length(indices_forest_mng) + 1]] = c(1,13,2)
          indices_forest_mng[[length(indices_forest_mng) + 1]] = c(1,13,3)
        
          forest_mng_categories <- c(as.character(forest_mng_categories),"Afforested",  "ForExtensive", "ForIntensive")
          nfuse = length(forest_mng_categories)
        #"PlantationNonTimber"  "ClearCut"             "Retention"            "PlantationFuel"       "SlashAndBurn"         "SelectionSystem"      "SelectiveLogging"     "Agroforestry"         "Plantation"          
        # "ReducedImpactLogging" "Afforested"           "ForExtensive"         "ForIntensive"
        
          rm(names_for, forest_mng_categories_ex, forest_mng_categories_in, forest_mng_categories_pl)
    
      # Conversion of CF to response ration (RR = 1 -CF)
      
        # broad land use classes
          
          rr_land_use <- lapply(CFloc_group_landuse, function(x) 1-x) #from CF to ratio
          rr_land_use <- lapply(rr_land_use, function(x) sapply(x, function(x) if (x == 0) {x = NA} else {x = x}))
          rr_land_use <- lapply(rr_land_use, function(x) x[!is.na(x)])
        
        #forest use classes
      
        # removes 0 from CFloc_group_for
      
          rr_forest_mng <- lapply(CFloc_group_for, function(x) 1-x) #from CF to ratio
          rr_forest_mng <- lapply(rr_forest_mng, function(x) sapply(x, function(x) if (x == 0) {x = NA} else {x = x}))
          rr_forest_mng <- lapply(rr_forest_mng, function(x) x[!is.na(x)])
          
       
          # if (uncertainties == FALSE) { 
          #   
          #   if(cutoff == TRUE) {
          #     rr_land_use <- lapply(rr_land_use, function(x) sapply(x, function(x) if (x > 1) {x = NA} else {x = x}))
          #     rr_forest_mng <- lapply(rr_forest_mng, function(x) sapply(x, function(x) if (x > 1) {x = NA} else {x = x}))
          #   }
          #   
          # }
          # 
          
          dim <- c(nbiome, necoregions, nlanduse, nfuse, ntaxa)  
          biomes = append(biomes, f_biomes)   # the last element (f_biome) is "Global", so it is not a biome
          names_dim <- list(biomes = biomes, land_use_types = land_use_types, forest_mng_categories = forest_mng_categories, taxa = taxa)
          
        results <- list("rr_land_use" = rr_land_use, "rr_forest_mng" = rr_forest_mng, "dim" = dim, "names_dim" = names_dim, "indices_land_use" = indices_land_use, 
                        "indices_forest_mng" = indices_forest_mng, "Ecoregions" = Ecoregions)
        
        return(results)

  }


  ############################# CALCULATE RR (WITH AND WITHOUT MONTE CARLO SIMULATIONS) ############################# 

  calculate.RR <- function(uncertainties, distribution, cutoff) {
  
  ########### MEDIAN CALCULATION OF RR WHEN UNCERTAINTIES IS OFF AND MONTE-CARLO SIMULATION WHEN UNCERTAINTIES IS ONE OF THE AVAILABLE ONES ###########
  
  # task: 
  # compile a four dimension array containing the response ratio per ecoregion, land use category and taxa.
  # The fourth dimension contains either two equal values corresponding to the median of the raw response ratios or the n simulations generated by the given distribution
  
    if (missing(uncertainties)) {uncertainties = FALSE}
    if (missing(distribution) & uncertainties == FALSE) {distribution = FALSE
    } else if (missing(distribution) & uncertainties == TRUE) {distribution = "Weibull"}
    if (missing(cutoff)) {cutoff = TRUE}
    if (uncertainties != FALSE) {n = 1000} else if (uncertainties == FALSE) {n = 2}
    simulations = n
    
    rr <- prepare.RR(uncertainties, cutoff)
    
    # this function provides
    # rr_land_use = list containing the grouped response ratios. Order of grouping: biome, land use, taxon. This means that the first element will be
    # e.g. Annual_Plants_1 (land-use_taxon_biome), the second Annual_Birds_1, the third Annual_Mammals_1, the fourth Permanent_Plants_1 etc.
    # rr_forest_mng = list containing the grouped response ratios. Same order as rr_land_use (though here there is only one biome, called "Global")
    # dim = vector containing the number of: biomes, ecoregions, land use types, forest management categories, taxa, simulations (this order must be kept like 
    # this). It is called dim because these numbers will define the dimension of the array built in this function. 
    # names_dim = list with the names of the dimensions: biomes, land use types, forest management categories, taxa, simulations.
    # indices_land_use = list where each element contains three numbers indicating which biome, land use and taxa are in the rr_land_use list. E.g., 
    # the first element is 111, which refer to Annual_Plants_1 of rr_land_use (Annual = first land use in the names_dim list, Plants = first taxa in
    # the names_dim list, 1 = first biome in the names_dim list).
    # indices_forest_mng = same as indices_land_use but for rr_forest_mng. 

        rr_land_use <- rr[["rr_land_use"]]
        rr_forest_mng <- rr[["rr_forest_mng"]]
        dim <- rr[["dim"]]
        names_dim <- rr[["names_dim"]]
        indices_land_use <- rr[["indices_land_use"]]
        indices_forest_mng <- rr[["indices_forest_mng"]]
        Ecoregions <- rr[["Ecoregions"]]
    
        nbiome = dim[1]
        necoregions = dim[2]
        nlanduse = dim[3]
        nfuse = dim[4]
        ntaxa = dim[5]
        simulations = n
        
        biomes = names_dim[["biomes"]]
        ecoregions = names_dim[["ecoregions"]]
        land_use_types = names_dim[["land_use_types"]]
        forest_mng_categories = names_dim[["forest_mng_categories"]]
        taxa = names_dim[["taxa"]]
        
        
        # Apply cutoff if needed (convert to 1 all response ratio above 1)
        
        if (uncertainties == FALSE) {
          
          if(cutoff == TRUE) {
            rr_land_use <- lapply(rr_land_use, function(x) sapply(x, function(x) if (x > 1) {x = 1} else {x = x}))
            rr_forest_mng <- lapply(rr_forest_mng, function(x) sapply(x, function(x) if (x > 1) {x = 1} else {x = x}))
          }
          
        }
        
        
        rr_land_use_sim <- array(data = NA, dim= c(nbiome, nlanduse, ntaxa, simulations), dimnames = list(biomes[1:(length(biomes)-1)], land_use_types, taxa, c(1:simulations)))
        rr_forest_mng_sim <- array(data = NA, dim= c(1, nfuse, ntaxa, simulations), dimnames = list(biomes[length(biomes)], forest_mng_categories, taxa, c(1:simulations)))
        
        
      # Simulations 

        if (uncertainties == FALSE) { # no simulations
          
          for (counter in 1:length(rr_land_use)) {
            set.seed(counter)
            rr_land_use_sim[indices_land_use[[counter]][1],indices_land_use[[counter]][2],indices_land_use[[counter]][3],] = 
              c(median(rr_land_use[[counter]]),median(rr_land_use[[counter]]))
            
          }
          
          #if (sum(abs(sort(c(rr_land_use_sim[,,,1]))-sort(unlist(lapply(rr_land_use, function(x) median(x)))))) > 1e-16) {stop("ERROR in median calculation (uncertainties 'OFF')")}
          
          for (counter in 1:length(rr_forest_mng)) {
            set.seed(counter)
            rr_forest_mng_sim[indices_forest_mng[[counter]][1],indices_forest_mng[[counter]][2],indices_forest_mng[[counter]][3],] = 
              c(median(rr_forest_mng[[counter]]), median(rr_forest_mng[[counter]]))      
          }
          
          if (sum(abs(sort(c(rr_forest_mng_sim[1,,,1]))-sort(unlist(lapply(rr_forest_mng, function(x) median(x)))))) > 1e-16) {stop("ERROR in median calculation (uncertainties 'OFF')")}
          
        } else  if (distribution == "lognormal") {
          
          for (counter in 1:length(rr_land_use)) {
            set.seed(counter)
            rr_land_use_sim[indices_land_use[[counter]][1],indices_land_use[[counter]][2],indices_land_use[[counter]][3],] = 
              rtrunc(simulations, spec="lnorm", a=0, b=max(rr_land_use[[counter]]), meanlog = mean(log(rr_land_use[[counter]])), sdlog = sd(log(rr_land_use[[counter]])))      
          }
          
          for (counter in 1:length(rr_forest_mng)) {
            set.seed(counter)
            rr_forest_mng_sim[indices_forest_mng[[counter]][1],indices_forest_mng[[counter]][2],indices_forest_mng[[counter]][3],] = 
              rtrunc(simulations, spec="lnorm", a=0, b=max(rr_forest_mng[[counter]]), meanlog = mean(log(rr_forest_mng[[counter]])), sdlog = sd(log(rr_forest_mng[[counter]])))            
          }
          
        } else if (distribution == "Weibull") {
          
          for (counter in 1:length(rr_land_use)) {
            set.seed(counter)
            fw = fitdist(rr_land_use[[counter]], "weibull")
            shape_w = fw$estimate[1] 
            scale_w = fw$estimate[2]
            rr_land_use_sim[indices_land_use[[counter]][1],indices_land_use[[counter]][2],indices_land_use[[counter]][3],] = 
              #rtrunc(simulations, spec="weibull", a=0, b=max(rr_land_use[[counter]]), shape = shape_w, scale = scale_w)
              rtrunc(simulations, spec="weibull", a=0, b=1, shape = shape_w, scale = scale_w)
            
          }
          
          rm(fw, shape_w, scale_w)
          for (counter in 1:length(rr_forest_mng)) {
            set.seed(counter)
            fw = fitdist(rr_forest_mng[[counter]], "weibull")
            shape_w = fw$estimate[1] 
            scale_w = fw$estimate[2]
            rr_forest_mng_sim[indices_forest_mng[[counter]][1],indices_forest_mng[[counter]][2],indices_forest_mng[[counter]][3],] = 
              #rtrunc(simulations, spec="weibull", a=0, b=max(rr_forest_mng[[counter]]), shape = shape_w, scale = scale_w)
              rtrunc(simulations, spec="weibull", a=0, b=1, shape = shape_w, scale = scale_w)
            
          }
          
          rm(fw, shape_w, scale_w)
          
        }
      
        rr_landuse_ecoregion <- array(data = NA, dim = c(necoregions, nlanduse, ntaxa, simulations), dimnames = list(unique(Ecoregions$Eco_code), land_use_types, taxa, c(1:simulations)))
        for (i in 1:necoregions) {
          pos = which(dimnames(rr_land_use_sim)[[1]] == Ecoregions$Biome_ID[i])
          rr_landuse_ecoregion[i,,,] = rr_land_use_sim[pos,,,]
        }
        
        Eco_code <- Ecoregions %>% dplyr::select(Eco_code)                     # create a dataframe with only one column containing the codes of the ecoregions
        Ecoregions_global <- Eco_code %>% mutate(Biome_ID = "Global")
        rr_forest_ecoregion <- array(data = NA, dim = c(necoregions, nfuse, ntaxa, simulations), dimnames = list(unique(Ecoregions$Eco_code), forest_mng_categories, taxa, c(1:simulations)))
        
        for (i in 1:necoregions) {
          pos = which(dimnames(rr_forest_mng_sim)[[1]] == Ecoregions_global$Biome_ID[i])
          rr_forest_ecoregion[i,,,] = rr_forest_mng_sim[pos,,,]
        }
        
        rm(pos)
      
      #Merging the two array
      
      rr_ecoregion <- abind(rr_landuse_ecoregion, rr_forest_ecoregion, along = 2)
      rr_ecoregion_backup = rr_ecoregion
      
    return(rr_ecoregion)    
    
    }      
    
  

  #### data have been saved until this point ====
  
  #return(list("ratio_eco" = ratio_eco, "zvalues" = zvalues, "weight_taxa" = weight_tx, "Sorg_VS" = Sorg_VS))


