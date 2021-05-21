setwd("/home/frrosa/R/Forest_management/") # folder where this calculation takes places

library("triangle") # triangular distribution -> for the distribution of the z values
library(dplyr)      # dataframe management
library(tidyr)      # dataframe management
library(abind)      # dataframe management
library(tidyverse)
library(parallel)
library(doParallel)
library(foreach)
library(arsenal)
library(boot)
library(kernelboot)
library(ggplot2)
select <- dplyr::select
source("./parameters_calculation.R")

#### Statistic to bootstrap ####

foo <- function(data, indices){
  dt <- data[indices]
  # print(median(dt))
  # print((indices))
  # print("End of this bootstrapping loop")
  median(dt, na.rm = TRUE)
} 

foo_mean <-  function(data, indices){
  dt <- data[indices]
  # print(median(dt))
  # print((indices))
  # print("End of this bootstrapping loop")
  mean(dt, na.rm = TRUE)
}

############################# CALCULATE RR (WITH BOOTSTRAPPING) ############################# 

calculate.RR.bs <- function(uncertainties, n, cutoff) {
  
  # task: 
  # compile a four dimension array containing the response ratio per ecoregion, land use category and taxa.
  # The fourth dimension contains the bootsrtapped medians 
  # uncertainties -> whether to calculate them or not (TRUE or FALSE)
  # n = number of replicates in the bootstrapping process
  # cutoff <- whether to cut off the rr above 1 and covert them to 1 or not (TRUE or FALSE)
  
  if (missing(uncertainties)) {uncertainties = TRUE}
  if (missing(cutoff)) {cutoff = TRUE}
  if (missing(n)) {n = 10000}

  check_mean_median = FALSE
  
  rr <- prepare.RR(uncertainties, n, cutoff)
  
  # this function provides
  # rr_land_use = list containing the grouped response ratios. Order of grouping: biome, land use, taxon. This means that the first element will be
  # e.g. Annual_Plants_1 (land-use_taxon_biome), the second Annual_Birds_1, the third Annual_Mammals_1, the fourth Permanent_Plants_1 etc.
  # rr_forest_mng = list containing the grouped response ratios. Same order as rr_land_use (though here there is only one biome, called "Global")
  # dim = vector containing the number of: biomes, ecoregions, land use types, forest management categories, taxa (this order must be kept like 
  # this). It is called dim because these numbers will define the dimension of the array built in this function. 
  # names_dim = list with the names of the dimensions: biomes, land use types, forest management categories, taxa.
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

  biomes = names_dim[["biomes"]]
  ecoregions = names_dim[["ecoregions"]]
  land_use_types = names_dim[["land_use_types"]]
  forest_mng_categories = names_dim[["forest_mng_categories"]]
  taxa = names_dim[["taxa"]]
  

  # Apply cutoff if needed (convert to 1 all response ratio above 1)
  
    if(cutoff == TRUE) {
      rr_land_use <- lapply(rr_land_use, function(x) sapply(x, function(x) if (x > 1) {x = 1} else {x = x}))
      rr_forest_mng <- lapply(rr_forest_mng, function(x) sapply(x, function(x) if (x > 1) {x = 1} else {x = x}))
    }
  
  
  rr_land_use_bs <- array(data = NA, dim= c(nbiome, nlanduse, ntaxa, n), dimnames = list(biomes[1:(length(biomes)-1)], land_use_types, taxa, c(1:n)))
  rr_forest_mng_bs <- array(data = NA, dim= c(1, nfuse, ntaxa, n), dimnames = list(biomes[length(biomes)], forest_mng_categories, taxa, c(1:n)))
  
  number_data_group <- sort(sapply(rr_land_use, length))
  number_data_group_forest <- sort(sapply(rr_forest_mng, length))

  # for each combination of biome, land use and taxon, n median values will be bootstrapped
  
  
  ##### LAND USE #####
  
  ptm <- proc.time()
  
  set.seed(1)
  
  for(counter in 1:length(rr_land_use)) {  
    
    # check that the list element from where the data are sources is correct
    names_in <- names(rr_land_use[counter])             # land use, taxonomic group and biome from where the grouped data are sourced
    names_fin <- paste0(dimnames(rr_land_use_bs)[[2]][indices_land_use[[counter]][2]], "_", dimnames(rr_land_use_bs)[[3]][indices_land_use[[counter]][3]], 
                        "_", dimnames(rr_land_use_bs)[[1]][indices_land_use[[counter]][1]])            # land use, taxonomic group and biome where the results are stored
    print(names_in)
    print(names_fin) 
    if(names_in != names_fin) {stop("Error in the sourcing of raw data from the list (land use categories)")}
    
    # Prepare the input data for the bootstrapping
    data_vector <- rr_land_use[[counter]]
    # Create the empty vector where the bootstrapped values will be stored
    temp_res <- vector("numeric", length = n)

    # Bootstrapping
        res_boot = boot(data = data_vector, statistic = foo, R = n)
        temp_res <- res_boot$t
        
        rr_land_use_bs[indices_land_use[[counter]][1], indices_land_use[[counter]][2], indices_land_use[[counter]][3], ] <- temp_res
        
    rm(res_boot, temp_res)
        
    print(paste0("Land use, ", counter))
      
    }
    
  proc.time() - ptm 
  
  save(rr_land_use_bs, file = "./rr_z/rr_land_use_bs.Rdata")
  
  
  ##### FOREST MANAGEMENT #####
  
  ptm <- proc.time()
  
  for(counter in 1:length(rr_forest_mng)) {  
  
  # check that the list element from where the data are sources is correct
  names_in <- names(rr_forest_mng[counter])             # land use, taxonomic group and biome from where the grouped data are sourced
  names_fin <- paste0(dimnames(rr_forest_mng_bs)[[2]][indices_forest_mng[[counter]][2]], "_", dimnames(rr_forest_mng_bs)[[3]][indices_forest_mng[[counter]][3]], 
                      "_", dimnames(rr_forest_mng_bs)[[1]][indices_forest_mng[[counter]][1]])            # land use, taxonomic group and biome where the results are stored
  print(names_in)
  print(names_fin) 
  if(names_in != names_fin) {stop("Error in the sourcing of raw data from the list (forest management categories)")}
  
  # Prepare the input data for the bootstrapping
  data_vector <- rr_forest_mng[[counter]]
  # Create the empty vector where the bootstrapped values will be stored
  temp_res <- vector("numeric", length = n)
  
  # Bootstrapping
    res_boot = boot(data = data_vector, statistic = foo, R = n)
    temp_res <- res_boot$t
    
    rr_forest_mng_bs[indices_forest_mng[[counter]][1], indices_forest_mng[[counter]][2], indices_forest_mng[[counter]][3], ] = temp_res 
  
  rm(res_boot, temp_res)
  
  print(paste0("Forest management, ", counter))
  
  }
  
  proc.time() - ptm  
  
  save(rr_forest_mng_bs, file = "./rr_z/rr_forest_mng_bs.Rdata")
  

  rr_landuse_ecoregion <- array(data = NA, dim = c(necoregions, nlanduse, ntaxa, n), dimnames = list(unique(Ecoregions$Eco_code), land_use_types, taxa, c(1:n)))
  for (i in 1:necoregions) {
    pos = which(dimnames(rr_land_use_bs)[[1]] == Ecoregions$Biome_ID[i])
    rr_landuse_ecoregion[i,,,] = rr_land_use_bs[pos,,,]
  }
  
  Eco_code <- Ecoregions %>% dplyr::select(Eco_code)                     # create a dataframe with only one column containing the codes of the ecoregions
  Ecoregions_global <- Eco_code %>% mutate(Biome_ID = "Global")
  rr_forest_ecoregion <- array(data = NA, dim = c(necoregions, nfuse, ntaxa, n), dimnames = list(unique(Ecoregions$Eco_code), forest_mng_categories, taxa, c(1:n)))
  
  for (i in 1:necoregions) {
    pos = which(dimnames(rr_forest_mng_bs)[[1]] == Ecoregions_global$Biome_ID[i])
    rr_forest_ecoregion[i,,,] = rr_forest_mng_bs[pos,,,]
  }
  
  rm(pos)
  
  #Merging the two array
  
  rr_ecoregion <- abind(rr_landuse_ecoregion, rr_forest_ecoregion, along = 2)
  rr_ecoregion_backup = rr_ecoregion
  
  save(rr_ecoregion, file = "./rr_z/rr_ecoregion_bs.Rdata")
  
  return(rr_ecoregion)   
  

  # mean and median
  
  if(check_mean_median == TRUE) {
    
    rr_land_use_stat <- data.frame(matrix(NA, nrow = length(rr_land_use), ncol = 3))
    colnames(rr_land_use_stat) <- c("categories", "mean", "median")
    rr_land_use_stat$categories = names(rr_land_use)
    rr_land_use_stat$mean <- sapply(rr_land_use, mean, na.rm = TRUE)
    rr_land_use_stat$median <- sapply(rr_land_use, median, na.rm = TRUE)
    
    rr_land_use_stat$diff_mean_median <- rr_land_use_stat$mean - rr_land_use_stat$median
    rr_land_use_stat <- rr_land_use_stat %>% 
      mutate(diff_mean_median = mean - median) %>%
      mutate(sign = case_when(diff_mean_median >= 0 ~ "positive", diff_mean_median < 0 ~ "negative"))
    
    
    rr_forest_mng_stat <- data.frame(matrix(NA, nrow = length(rr_forest_mng), ncol = 3))
    colnames(rr_forest_mng_stat) <- c("categories", "mean", "median")
    rr_forest_mng_stat$categories = names(rr_forest_mng)
    rr_forest_mng_stat$mean <- sapply(rr_forest_mng, mean, na.rm = TRUE)
    rr_forest_mng_stat$median <- sapply(rr_forest_mng, median, na.rm = TRUE)
    
    rr_forest_mng_stat$diff_mean_median <- rr_forest_mng_stat$mean - rr_forest_mng_stat$median
    rr_forest_mng_stat <- rr_forest_mng_stat %>% 
      mutate(diff_mean_median = mean - median) %>%
      mutate(sign = case_when(diff_mean_median >= 0 ~ "positive", diff_mean_median < 0 ~ "negative"))
    
    figure <- ggplot(data = rr_land_use_stat, aes(x = categories, y = diff_mean_median)) +
      geom_bar(stat = "identity", aes(fill = sign), width = 0.7) +
      labs(y = "difference between mean and median", 
           title = "Difference between mean and median of response ratios for each biome, land use type and taxonomic group") +
      theme_minimal(base_size = 8) +
      theme(legend.position = "none", 
            axis.text.x = element_text(size = 6, angle = 90)) 
    figure
    ggsave("./plots/diff_mean_median_landuse.png", width= 25, height = 10, units = "cm")
    
    figure <- ggplot(data = rr_forest_mng_stat, aes(x = categories, y = diff_mean_median)) +
      geom_bar(stat = "identity", aes(fill = sign), width = 0.7) +
      labs(y = "difference between mean and median", 
           title = "Difference between mean and median of response ratios for each forest use category and taxonomic group") +
      theme_minimal(base_size = 8) +
      theme(legend.position = "none", 
            axis.text.x = element_text(size = 6, angle = 90))
    
    figure
    ggsave("./plots/diff_mean_median_forestmng.png", width= 25, height = 10, units = "cm")
    
  }
  
  
}
  
 
prepare.zvalues.bs <- function(n) {  
  
  # tasks: 
  # bootstrap the z values per habitat type and per taxon type (plant or animal (birds and mammals))
  # assign the z values to all ecoregion 
  # output: array whose rows are the ecoregions and whose columns are the z values
  # n = number of replicates in the bootstrapping process
  
  # load the data on the ecoregions
  data_loaded <- load.data()
  Ecoregions <- data_loaded[["Ecoregions"]]
  necoregions <- data_loaded[["Ecoregion_number"]]
  z_rawvalues <- data_loaded[["z_rawvalues"]]
  
  habitats <- unique(z_rawvalues$habitat)
  nhabitats <- length(habitats)
  
  z_rawvalues_list <- list("island" = z_rawvalues %>% filter(habitat == habitats[1]) %>% select(z), 
                           "forest"=  z_rawvalues %>% filter(habitat == habitats[2]) %>% select(z),
                           "non-forest" =  z_rawvalues %>% filter(habitat == habitats[3]) %>% select(z))
  
  
  # raw values of z from Drakare et al. 2005, "The imprint of the geographical, evolutionary and ecological context on species–area relationships", Ecology Letters.
  # not all the information was available in the paper, therefore part of the data were asked to the corresponding author
  
  # array to fill up with bootstrapped results
  z_bs <- matrix(data = NA, nrow = nhabitats, ncol = n, dimnames = list(c("island", "forest", "non-forest"), as.character(c(1:n))))     # create an empty array to store the result of the bootstrapping
  
  # final array which will be filled up with the values used as input for the calculation of the impacts
  zvalues <- data.frame(matrix(NA, nrow = necoregions, ncol = n + 1))     # create an empty dataframe to store the value of z for each ecoregion 
  names(zvalues)[1] = "Eco_code"
  names(zvalues)[2:length(zvalues)] = as.character(c(1:n))
  zvalues <- zvalues %>% mutate(Eco_code = Ecoregions$Eco_code)                 # add a column with the ecoregion codes

  set.seed(2)
  
  for (counter in 1:nhabitats) {
    
    data_vector <- pull(z_rawvalues_list[[counter]])
    temp_res <- vector("numeric", length = n)

        res_boot = boot(data = data_vector, statistic = foo, R = n)
        temp_res <- res_boot$t
    
    z_bs[counter,] <- temp_res
    
    rm(res_boot, temp_res)
  }
  
  save(z_bs, file = "./rr_z/z_bs.Rdata")
  # load("./ecoregions_data/z_bs.Rdata")
  # assign the corresponding simulation result to each ecoregion according to the habitat type (islands, forests, not forests)
  
  Ecoregions[,"z_values"] <- NA     # add a column to Ecoregions, to store the z values
  
  for (i in 1:nrow(Ecoregions)) {
    
    if (Ecoregions$Assigned_habitat_type[i] == "island") {
      zvalues[i,2:length(zvalues)] = z_bs[1,]
    } else if (Ecoregions$Assigned_habitat_type[i] == "forest") {
      zvalues[i,2:length(zvalues)] = z_bs[2,]
    } else if (Ecoregions$Assigned_habitat_type[i] == "non-forest") {
      zvalues[i,2:length(zvalues)] = z_bs[3,]
    }
  }
  
  zvalues <- array(zvalues[2:length(zvalues)], dimnames = list(Ecoregions$Eco_code, 1:n))   # convert of the dataframe to an array (this format is needed to use the functions when species lost are computed)
  
  rm(z_bs, z_lower, z_upper)
  
  save(zvalues, file = "./rr_z/zvalues_ecoregion_bs.Rdata")
  
  return(zvalues)
}


# 
# 
# for (i in 1:n) {
#   res_boot <- kernelboot(data = data_vector, statistic = function(data) median(data, na.rm = TRUE), R = n)
#   #print(paste0("i: ", i))
#   #plot(res_boot)
#   result[i] = mean(res_boot$boot.samples)
# }



# foo <- function(data, indices){
#   dt <- data[sample(indices, round(0.9*length(indices))),]
#   print(mean(dt))
#   #print((indices))
#   mean(dt, na.rm = TRUE)
# }
