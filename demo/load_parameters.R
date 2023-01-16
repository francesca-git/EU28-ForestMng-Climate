
# Function to load the parameters used in calculate.lost

load.parameters <- function(ecodata_path) {

     necoregions = 804                      
      
############################# CALCULATION OR LOADING OF THE INPUT PARAMETERS ############################# 
      
      ptm <- proc.time()
      n = 2
     
      # The parameters have been calculated using the functions in the file parameters_calculation.R and in bootstrapping.R and then saved to be loaded here. 

      # LOADING OF THE RESPONSE RATIOS 
      # all .Rdata loaded here below contain a 4-dimension array called rr_ecoregion. 1dim: ecoregions; 2dim: land use categories; 
      # 3dim: taxa; 4dim: two columns both containing the median of the raw values (static option).
      
  
      load(paste0(ecodata_path, "rr_z/rr_ecoregion_static.Rdata"))
      ratio_eco_static <- rr_ecoregion
      rm(rr_ecoregion)
      
      if (dim(ratio_eco_static)[4] != 2) {stop("Problem with the dimension of the array containing the response ratios (function: calculate.slost)")}
      
      
      # Weights (which include the vulnerability scores) and original number of species 
      # this function (prepare.Sorg.VS.weighting) is defined in parameters_calculation.R)
      source("./scripts/model/parameters_calculation.R")
      weight_tx = prepare.Sorg.VS.weighting(vulnerability = TRUE, ecodata_path)[["Weights"]]    # dataframe containing the weighting factors (rows: ecoregions, columns: weights per taxonomic group)
      Sorg_VS = prepare.Sorg.VS.weighting(vulnerability = TRUE, ecodata_path)[["Sorg_VS"]]      # dataframe containing the original number of species per ecoregion
      
      # Z VALUES
      # all .Rdata which are loaded here below contain a 2-dimension array called zvalues. 1dim: ecoregions;
      # 2dim: either the mean of the raw values (static option) or 1000 monte carlo simulations (mc) or 10000 bootstrapped medians (bs)

      # As for the static values of the response ratios, the static values of z are loaded regardless of the value of CI.   

      load(paste0(ecodata_path, "rr_z/zvalues_static.Rdata"))
      zvalues_static <- zvalues

      # This is the list which will be used as argument in the function which compute the static results. 
      parameters_static = list("ratio_eco" = ratio_eco_static, "weight_tx" = weight_tx, "Sorg_VS" = Sorg_VS, "zvalues" = zvalues_static)
      
      return(parameters_static)
      
}