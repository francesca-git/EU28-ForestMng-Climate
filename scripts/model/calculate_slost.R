source("./scripts/model/CF_functions.R")
source("./scripts/model/parameters_calculation.R")


if(!require("pacman")){install.packages("pacman")};require("pacman")
p_load(MASS, dplyr, tidyr, abind, tidyverse, stringr, foreach, fitdistrplus, truncdist)  # dataframe management and string management
select <- dplyr::select
    

  calculate.slost <- function(Total_RemainingNatural_areas, Land_use_areas, param, CI, BS) { 
    # Total_RemainingNatural_areas : dataframe with two columns, one with the total original areas, one with the remaining natural areas. The rows correspond to the 
      # ecoregions and are sorted in alphabetical order according to the ecoregion codes (AA0101, AA0102, etc.)
    # Land_use_areas : dataframe which contains one column per each land use category. The rows correspond to the 
      # ecoregions and are sorted in alphabetical order according to the ecoregion codes (AA0101, AA0102, etc.)
    # CI can be TRUE or FALSE. TRUE = the CI will be calculated, FALSE = the CI will not be calculated
    # vulnerability: TRUE or FALSE 
    

    if((CI != FALSE & CI != TRUE) | missing(CI)) {stop("Select one option for the calculation of the confidence intervals (FALSE or TRUE)")}
    
    ratio_eco = param[["ratio_eco"]]
    weight_tx = param[["weight_tx"]]
    Sorg_VS = param[["Sorg_VS"]]
    zvalues = param[["zvalues"]]
        
    necoregions = dim(ratio_eco)[1]                       # number of ecoregions = length of the first dimension of the array ratio_eco 
    Ecoregions = dimnames(ratio_eco)[[1]]
    Ecoregions = data.frame(Ecoregions)
    names(Ecoregions) ="Eco_code"
  
      
    ############################# CALCULATION OF THE IMPACTS ############################# 
      
     #### SPECIES LOSS PER TAXON AND ECOREGION ####
      
      # specieslost_sim calculates the affinity (h), the suitable areas (sum(hi*Ai)) and the species lost (no allocation, no weighting, 
      # no aggregation, as in eq. 11.3 of LCImpact method for land stress (Chaudhary, 2016), but multiplied also by the vulnerability scores)

        system.time(  
        Slost_h_a_suit <- specieslost_sim(Sorg_VS, Total_RemainingNatural_areas$A_org, Total_RemainingNatural_areas$A_new, Land_use_areas, zvalues, ratio_eco, n = n)
        )

        h = Slost_h_a_suit[[1]]       # list of 3d array. The lenght of the list is equal to the number of n. Each array contains the affinity values per ecoregion (1st dim), land use category (2nd dim) and per taxon (3rd dim) 
                                      # Ecoregions are in alphabetical order according to the ecoregion code, AA0101, AA0102, etc. Taxa are "Plants"  "Birds"   "Mammals".
                                      # If CI are not calculated and number of n = 2, the list contains only two equal dataframes.
        taxa <- unlist(dimnames(h[[1]])[3])   # taxa considered
        
        Slost = Slost_h_a_suit[[3]]   # A single dataframe containing the impacts per taxonomin group (columns) and per ecoregion (rows, in alphabetical order according to the ecoregion code, AA0101, AA0102, etc).
                                      # The number of columns is the number of taxonomic groups multiplied by the number of n

        rm(Total_RemainingNatural_areas, Slost_h_a_suit)
        
      #### ALLOCATION FACTOR FOR LAND USE CATEGORIES ####
        
      # allocation_sim calculates the allocation factors used to allocate the impacts to the land use categories
        system.time(
        a_factor <- allocation_sim(h, Land_use_areas, n)    # list of 3d array. The lenght of the list is equal to the number of n. Each array contains the allocation values per ecoregion (1st dim), land use category (2nd dim) and per taxon (3rd dim) 
        )                                                           # Ecoregions are in alphabetical order according to the ecoregion code, AA0101, AA0102, etc. Taxa are "Plants"  "Birds"   "Mammals".
        
      # remove what is not needed anymore
        rm(h, Land_use_areas)      
      
      #### ALLOCATION AND WEIGHTING ####
        
      # prepare the structure of the arrays which have to be filled out with the results of the calculation
        Slost_lf <- array(NA, dim = c(dim(a_factor[[1]]), n), dimnames = list(dimnames(a_factor[[1]])[[1]],dimnames(a_factor[[1]])[[2]],dimnames(a_factor[[1]])[[3]], 1:n))
        Slost_tx_w <- Slost
        
      # test_weight <- apply(Slost, MARGIN = 3, function(x) x*weight_tx)
        
      # multiply Slost by the allocation factor and the weights
        
          ptm <- proc.time()
        for (n in 1:n) {
          
          Slost_tx_w[,,n] <- Slost[,,n]*weight_tx[,]
          
          for (i in 1:dim(a_factor[[n]])[2]) {
            
            Slost_lf[,i,,n] <- Slost_tx_w[,,n]*a_factor[[n]][,i,]
            
          }
        }
          proc.time() - ptm      
      

       #### RESULTS PER SPECIES GROUP ####
          
        Slost_taxa_df <- list()       # list to be filled out with the results per taxa (each element of the list will be a dataframe with the impacts per ecoregion and land use category)
        Slost_taxa_matrix <- list()   # list to be filled out with the results per taxa (each element of the list will be a matrix with the impacts per ecoregion and land use category)
        
        ptm <- proc.time()
        
        if (CI == TRUE) { # the CI are calculated only for the species-groups-aggregated results
          Slost_pertaxon <- 0 # apply(Slost_lf, c(1,2,3), mean, na.rm = TRUE)                   # compute the mean of the Slost computed using the bootstrapped values of rr adn z
          Slost_pertaxon2.5 <- 0 # apply(Slost_lf, c(1,2,3), function(x) quantile(x, 0.025, na.rm = TRUE)[[1]])
          Slost_pertaxon97.5 <- 0 # apply(Slost_lf, c(1,2,3), function(x) quantile(x, 0.975, na.rm = TRUE)[[1]])
        } else if (CI == FALSE) {
          Slost_pertaxon <- apply(Slost_lf, c(1,2,3), median, na.rm = TRUE)                 # compute the median of the impacts
          Slost_pertaxon2.5 <- 0
          Slost_pertaxon97.5 <- 0
        }
          
        Slost_df <- data.frame(Slost_pertaxon)                                            # convert the matrix to a dataframe
          proc.time() - ptm      
        
          ptm <- proc.time()
        for (i in 1:length(taxa)) {
          temp <- Slost_df %>% dplyr::select(contains(toString(taxa[i])))                   # select the columns for taxon i
          Slost_taxa_matrix[[taxa[i]]] <- data.matrix(temp)                                 # fill out the matrix
          rownames(Slost_taxa_matrix[[taxa[i]]]) <- rownames(temp)                          # give names to the rows of the matrix
          Slost_taxa_df[[taxa[i]]] <- temp %>%                                              # save it in the list
                          add_column(Ecoregions, .before = names(temp)[1])                  # create a column with the codes of the ecoregions
            } 
          proc.time() - ptm      
        
        rm(temp, Slost_df)
        
        
       #### AGGREGATED RESULTS ####
          ptm <- proc.time()     
        if (CI == TRUE) {
          Slost_aggr_fin <- apply(Slost_pertaxon, MARGIN = c(1,2), sum, na.rm = TRUE)
          Slost_aggr_2.5 <- apply(Slost_pertaxon2.5, MARGIN = c(1,2), sum, na.rm = TRUE)
          Slost_aggr_97.5 <- apply(Slost_pertaxon97.5, MARGIN = c(1,2), sum, na.rm = TRUE)
        } else if (CI == FALSE) {
        # sum the impacts 
          Slost_aggr <- apply(Slost_lf, MARGIN = c(1,2,4), sum, na.rm = TRUE)
          Slost_aggr_fin <- apply(Slost_aggr, c(1,2), median)                      # compute the median of the impacts
        }
          proc.time() - ptm     
        
          ptm <- proc.time()
        if (CI == FALSE) {
          Slost_aggr_2.5 <- 0
          Slost_aggr_97.5 <- 0
        } else if(CI == TRUE) {
          Slost_aggr_2.5 <- apply(Slost_aggr, c(1,2), function(x) quantile(x, 0.025)[[1]])
          Slost_aggr_97.5 <- apply(Slost_aggr, c(1,2), function(x) quantile(x, 0.975)[[1]])
        }
          proc.time() - ptm      
        

        Slost_aggr_df <- data.frame(Slost_aggr_fin) %>%
                      add_column(Ecoregions, .before = names(Slost_aggr_fin)[1])
        
        result = list("Aggr_matrix"= Slost_aggr_fin, "Aggr_df"= Slost_aggr_df, "PerTaxon_matrix"= Slost_taxa_matrix, "Pertaxon_df"= Slost_taxa_df,
                        "Aggr2.5_matrix"= Slost_aggr_2.5, "Aggr97.5_matrix"= Slost_aggr_97.5)
        
          
          return(result)
        
      }
  
  # test_sum <- sum(Slost_aggr_fin)
  # test_2.5 <- sum(Slost_aggr_2.5)
  # test_97.5 <- sum(Slost_aggr_97.5)
  # 
  # test_sum_EU <- sum(Slost_aggr_fin)
  # test_2.5_EU <- sum(Slost_aggr_2.5)
  # test_97.5_EU <- sum(Slost_aggr_97.5)
  # 
  # test_sum_EUf <- sum(Slost_aggr_fin)
  # test_2.5_EUf <- sum(Slost_aggr_2.5)
  # test_97.5_EUf <- sum(Slost_aggr_97.5)
  # 
  # 
  
  # Slost_aggr_sort <- Slost_aggr
  
  # ptm <- proc.time()
  # for(i in 1:necoregions) {
  #   for (j in 1:dim(Slost_aggr)[2]) {
  #     if ((length(which(is.na(Slost_aggr[i,j,])))) == 0) {
  #       Slost_aggr_sort[i,j,] <- sort(Slost_aggr[i,j,])
  #     } #else {Slost_aggr_sort[i,j,] = Slost_aggr[i,j,]}
  #   }
  # }
  # proc.time() - ptm      
  
