
library(MASS)
library(dplyr)    

# h
     
  

    affinity_CF = function(x, y) { #x = CFloc and y = z
      
      if (missing(y)) {
        
        y = mean(z)
        
      }
      
      
      result = (1-x)^(1/y)
      
      return(result)
      
    }
    
  
    affinity = function(x, y) { #x = CFloc and y = z
      
      if (missing(y)) {
        
        y = mean(z)
        
      }
      
      
      result = (x)^(1/y)
      
      return(result)
      
    }
    
      
    #how to use it: h = apply(CFloc_ecoreg, MARGIN = c(2,3), FUN = affinity, y = Ecoregions$z_values)   
    
    
    # Slost 
    
    specieslost = function(Sorg, Aorg, Anew, Aij, zvalues, CFloc) {
      
      
      h = apply(CFloc_ecoreg, MARGIN = c(2,3), FUN = affinity, y = Ecoregions$z_values) 
      
      necoregions = length(Aorg)
      ntaxa = ncol(Sorg)
      
      for (i in 1:necoregions) {
        for (k in 1:ntaxa) {
          
          a_suit[i,k] = sum(h[i,,k]*Aij[i,])
          
          Slost = Sorg[i,k]*(1-((Anew[i] + a_suit[i,k])/Aorg[i])^zvalues[i]) 
          
        }
      }
      
      result <-list("Slost" = Slost, "a_suit" = a_suit) 
      
      return(result)
      
    }
    

    #WITH SIMULATIONS  
    
    # h 
    
    affinity_sim = function(res_ratio, zvalues, n) { #res_ratio = 4d array of local response ratio (4° dim = simulations), zvalues = 2d array of z values (nrows = necoregions, 2° dim = simulations), n = number of simulations
      
      result = lapply(1:n, function(t) {
                        apply(res_ratio[,,,t], MARGIN = c(2,3), FUN = affinity, y = zvalues[,t]) 
                       }
                     ) #list of h matrices
     
      return(result)
      
    }
    
      
    # product of hgij and Aij starting from h and Aij
    
    h_A_product = function (h, Aij) { #h = one matrix from the list h containing all the simulation results, Aij = 2d dataframe of Aij
        
        temp1 <- lapply(1:dim(h)[3], function(x) {h[,,x]}) #create a list which has a number of elements equal to the dimension along which we want to multiply (in this case the taxa)
        
        temp2 <- lapply(temp1, function(x,y) {x*y}, y = data.matrix(Aij)) # multiply the dataframe Aij (after converting it to a matrix) with each element of the list (so Aij is multiplied with each element of temp1[i,j,])
        
        a_suit_prod <- array(sapply(temp2, function(x){x}), dim = dim(h)) # convert the list into an array 
        
        a_suit <- apply(a_suit_prod, MARGIN = c(1,3), FUN = sum)# sum oover the land use classes, matrix necoregions x ntaxa
        
        return(a_suit)
      
      }

   
    # product of of hgij and Aij starting from CFloc, zvalues, Aij
    
    suitablearea = function(res_ratio, zvalues, Aij, n) { #res_ratio = 4d array of local response ratio (4° dim = simulations), 
      #zvalues = 2d array of z values (nrows = necoregions, 2° dim = simulations), Aij = 2d dataframe of Aij (area shares),
      # n = number of simulations
      
      h = affinity_sim(res_ratio,zvalues,n)
      
      #lapply(1:n, function(sim) h_A_product(h[[sim]], arg3)) 
      
      a_suit_sim <- vector(mode = "list", length = simulations) # list of suitable areas
      
      for (i in 1:n) {
        
        a_suit_sim[[i]] = h_A_product(h[[i]], Aij)  
        dimnames(a_suit_sim[[i]]) = list(dimnames(res_ratio)[[1]], dimnames(res_ratio)[[3]])
        
      }
      
      result <- list("h" = h, "a_suit_sim" = a_suit_sim)
      return(result)
    }
    
    
    # 
    # suitablearea = function(res_ratio, zvalues, Aij, n) { #res_ratio = 4d array of local response ratio (4° dim = simulations), 
    #   #zvalues = 2d array of z values (nrows = necoregions, 2° dim = simulations), Aij = 2d dataframe of Aij (area shares),
    #   # n = number of simulations
    #   res_ratio = ratio_eco
    #   Aij = Areas_lf[((necoregions*sc)+1):(necoregions*(sc+1)),]
    #   n = simulations
    #   
    #   h = affinity_sim(res_ratio,zvalues,n)
    #   
    #   #lapply(1:n, function(sim) h_A_product(h[[sim]], arg3)) 
    #   
    #   a_suit_sim <- vector(mode = "list", length = simulations) # list of suitable areas
    #   
    #   ptm <- proc.time()
    #   
    #   for (i in 1:n) {
    #     a_suit_sim[[i]] = h_A_product(h[[i]], Aij)  
    #     dimnames(a_suit_sim[[i]]) = list(dimnames(res_ratio)[[1]], dimnames(res_ratio)[[3]])
    #     
    #   }
    #   proc.time() - ptm      
    #   
    #   result <- list("h" = h, "a_suit_sim" = a_suit_sim)
    #   return(result)
    # }
    
    
    
    specieslost_sim = function(Sorg, Aorg, Anew, Aij, zvalues, res_ratio, n) { # Sorg, Aij = 2d dataframe (Areas per land use), Anew = 1d array of remaining natural areas, Aorg = 1d array of original natural areas,
      #  zvalues = 2d array of z values (nrows = necoregions, 2° dim = simulations), n = number of simulations 
      # res_ratio = 4d array (biomes x land use x taxa x simulations)
      res_ratio = ratio_eco
      Aij = Areas_lf[((necoregions*sc)+1):(necoregions*(sc+1)),]
      n = simulations
      Aorg = A_org_new[((necoregions*sc)+1):(necoregions*(sc+1)),]$A_org
      Anew = A_org_new[((necoregions*sc)+1):(necoregions*(sc+1)),]$A_new
      
      #ptm <- proc.time()
      
      suit_area <- suitablearea(res_ratio, zvalues, Aij, n)
      h <- suit_area[[1]] # calculation of h 
      a_suit <- suit_area[[2]] # calculation of the product between h and Aij, a_suit = list of matrices (ecoregions x taxa)
      
      #proc.time() - ptm      
      
      #z_list <- lapply(1:n, function(x) {z[,x]}) # creation of a list where each element is a column of z (therefore a simulation for each column)
      
      #Slost <- vector(mode = "list", length = n) # creation of a list for the species lost: each element of the list will be a 2d matrix, with the species lost per ecoregion and per taxa
      
      # Anew = A_org_new_temp$A_new
      # Aorg = A_org_new_temp$A_org
      
      Slost <- array(NA, dim = c(dim(a_suit[[1]]),n), dimnames = list(rownames(a_suit[[1]]), colnames(a_suit[[1]]), 1:n))
      #ptm <- proc.time()
      
      for (i in 1:n) {
        
        areas_fr_temp = a_suit_fraction(Aorg, Anew, a_suit[[i]])
        areas_fr_temp = 1-(areas_fr_temp^zvalues[,i])
        areas_fr_temp = Sorg*areas_fr_temp

        #Slost[,,i] = Sorg*(1-(areas_fr_temp^zvalues[,i]))
        Slost[,,i] = areas_fr_temp
        
        #Slost[,,i] = data.matrix((Sorg * (1-((Anew+a_suit[[i]])/Aorg)^zvalues[,i])))
        #Slost[[i]] = (Sorg * (1-((Anew+a_suit[[i]])/Aorg)^z_list[[i]]))
        #dimnames(Slost[[i]]) = list(dimnames(a_suit[[i]])[[1]], dimnames(a_suit[[i]])[[2]])
        
      }
      
      #proc.time() - ptm      
      
      result <- list("h" = h, "a_suit" = a_suit, "Slost" = Slost)
      
      return(result)
      
    }   
    
    
    
    a_suit_fraction = function(Aorg, Anew, a_suit_2d) { # Anew = 1d array of remaining natural areas, Aorg = 1d array of original natural areas,
      # a_suit = 2d array from the list containing all the 2d arrays from simulations with the suitable area (not natural) per ecoregion and taxa 
      
      result = ((Anew+a_suit_2d)/Aorg)
      return(result)
      # result is a 2d array containing the fraction of suitable area for a given simulation 
      
    }
    
    # species lost 
    
  
    
    #Regional CFs

    allocation = function(h, Aij) { #h = one matrix from the list h containing all the simulation results, Aij = 2d dataframe of Aij

      temp1 <- lapply(1:dim(h)[3], function(x) {h[,,x]}) #create a list from h which has a number of elements equal to the dimension along which we want to multiply (in this case the taxa)
      
      temp2 <- lapply(temp1, function(x,y) {(1-x)*y}, y = data.matrix(Aij)) # multiply the dataframe Aij (after converting it to a matrix) with each element of the list (so Aij is multiplied with each element of temp1[i,j,])
      
      a_suit_prod <- array(sapply(temp2, function(x){x}), dim = dim(h)) # convert the list into an array 
      a_suit_prod_t <- apply(a_suit_prod, MARGIN = c(3,2), FUN = t)
      
      a_suit_sum <- apply(a_suit_prod, MARGIN = c(1,3), FUN = sum)# sum over the land use classes, matrix necoregions x ntaxa
      
      allocation_factor = array(NA, dim = dim(a_suit_prod), dimnames = dimnames(h))
      
      #ptm <- proc.time()
      
      for(i in 1:dim(a_suit_prod)[2]){
        allocation_factor[,i,] <- a_suit_prod[,i,]/a_suit_sum
      }
      #proc.time() - ptm      
      
      return(allocation_factor)
     
    }
    

    allocation_sim = function(h, Aij, n) { #h = list of 1000 matrices eco x land use x taxa, Aij = 2d dataframe of Aij, n = number of simulations
      
  
      allocation_factor_sim <- vector(mode = "list", length = simulations) # list of allocation factors
      ptm <- proc.time()
      
      for (i in 1:n) {
        
        allocation_factor_sim[[i]] = allocation(h[[i]], Aij)  
        dimnames(allocation_factor_sim[[i]]) = dimnames(h[[i]])
        
      }
      proc.time() - ptm      
      
      return(allocation_factor_sim)
      
    }
      
      
      
    #CF LOCAL
    
    CFcalc = function(raw_data, taxa, lu_type, biomes) { #raw_data must contain at least the following columns: "Biomes_ID", "Land_use_ID","Land_use_type","Taxa_Used", "Local_CF"
      
      
      ntaxa = length(taxa) #numbert of taxa 
      nlanduse = length(lu_type) #number of land use 
      nbiome = length(biomes) #numbert of biomes 
      
      # definition of arrays to be filled
      
      #2D array to store the averages of CFlocal per land use and taxa
      
      CF_lu_tx <- array(1, dim=c(nlanduse,ntaxa), dimnames = list(lu_type, taxa))
      
      #1D array to store the averages per taxa
      
      CF_lu <- array(1, dim = nlanduse, dimnames = list(lu_type)) 
      
      #3D array to store CFloc per biome, land use and taxa
      
      CFloc <- array(data = NA, dim = c(nbiome,nlanduse,ntaxa), dimnames = list(biomes,lu_type, taxa))
      
      
      # LOCAL CF - calculation of the averages per land use and taxa
      
      #selection of all rows in the raw_data with a specific land use ID and taxa and then mean of the values (land use ID and taxa are passed by the i and j indexes from respectively the vectors land_use_ID and taxa)
      for (j in 1:nlanduse) {
        for (k in 1:ntaxa) {
          CF_lu_tx[j,k] = mean(raw_data[which(raw_data$Land_use_type == lu_type[j] & raw_data$Taxa_Used == taxa[k]), "Local_CF"])
        }
      }
      
      #LOCAL CF - averages per land use
      
      #same procedure: selection of those CFloc corresponding to a given taxa in the df raw_data
      for (j in 1:nlanduse) {
        CF_lu[k] = mean(raw_data[which(raw_data$Land_use_type == lu_type[j]), "Local_CF"])
      }
      
      #LOCAL CF 
      
      nCFloc = nlanduse*ntaxa*nbiome #number of CFloc needed
      
      raw_data_group = list()  #list to store the data per biome, land use and taxa   
      raw_data_group_in = vector(mode = "list", length = nbiome*nlanduse*ntaxa) #list to store the indexes 
      
      #parameters to store/analyse the CF for which there are enough data, so averages over land uses or taxa are not needed
      nCFloc_complete = 0
      whichCFloc_complete = list(1) 
      nCFloc_raw_complete	= vector(mode = "integer", length = 1)
      CFloc_raw_complete = list(1)
      pos_complete = list(1)
      
      #calculation 
      
      for (i in 1:nbiome) {
        for (j in 1:nlanduse) {
          for (k in 1:ntaxa) {
            
            #print(paste0("biome: ", i, "; land use: ", land_use_type[j], "; taxa: ", taxa[k]))
            
            if (length(which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))) <= 5) {
              #print("biome x land use x taxa < 5")
              
              if (length(which((raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))) <= 5) {
                #print("land use x taxa < 5")
                
                #CFloc[i,j,k] = CF_tx[dimnames(CFloc)[[3]][k]]
                
                CFloc[i,j,k] = CF_lu[dimnames(CFloc)[[2]][j]]
                
                raw_data_group_in[[(ntaxa*(j-1) + k)+(ntaxa*nlanduse)*(i-1)]]= c(i,j,k) 
                #raw_data_group[[(ntaxa*(j-1) + k)+(ntaxa*nlanduse)*(i-1)]] = raw_data[which(raw_data$Land_use_type == lu_type[j]), "Local_CF"]
                raw_data_group[[paste0(lu_type[j],"_",taxa[k],"_",biomes[i])]] = raw_data[which(raw_data$Land_use_type == lu_type[j]), "Local_CF"]
                
                
              } else {
                #print("biome x land use x taxa < 5, but land use x taxa > 5")
                CFloc[i,j,k] = CF_lu_tx[dimnames(CFloc)[[2]][j],dimnames(CFloc)[[3]][k]]		
                
                raw_data_group_in[[(ntaxa*(j-1) + k)+(ntaxa*nlanduse)*(i-1)]] = c(i,j,k) 
                raw_data_group[[paste0(lu_type[j],"_",taxa[k],"_",biomes[i])]] = raw_data[which(raw_data$Land_use_type == lu_type[j] & raw_data$Taxa_Used == taxa[k]), "Local_CF"]
                
              }
              
            } else {
              
              #print("biome x land use x taxa > 5")
              
              CFloc[i,j,k] = median(raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))])
              
              raw_data_group_in[[(ntaxa*(j-1) + k)+(ntaxa*nlanduse)*(i-1)]] = c(i,j,k) 
              #raw_data_group[[(ntaxa*(j-1) + k)+(ntaxa*nlanduse)*(i-1)]] = raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))]
              raw_data_group[[paste0(lu_type[j],"_",taxa[k],"_",biomes[i])]] = raw_data$Local_CF[which((raw_data$Biome_ID == biomes[i]) & (raw_data$Land_use_type == lu_type[j]) & (raw_data$Taxa_Used == taxa[k]))]
              
              
              nCFloc_complete = nCFloc_complete + 1
              whichCFloc_complete[[nCFloc_complete]] = which(CFloc == CFloc[i,j,k], arr.ind = TRUE)
              nCFloc_raw_complete[nCFloc_complete]	= length(raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))])
              CFloc_raw_complete[[nCFloc_complete]] = raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))]
              pos_complete[[nCFloc_complete]] = c(i,j,k)
              
            }
            
            
          }
        }
      }
      
      
      result = vector(mode = "list", length = 5)
      result[[1]] = CFloc
      result[[2]] = raw_data_group
      result[[3]] = raw_data_group_in
      result[[4]] = CF_lu_tx
      result[[5]] = CF_lu
      result[[6]] = nCFloc_complete
      result[[7]] = CFloc_raw_complete
      result[[8]] = pos_complete
      
      
      return(result)
      
    }
    
    
    
    #############################

    CFcalc_group = function(raw_data, taxa, lu_type, biomes) { #raw_data must contain at least the following columns: "Biomes_ID", "Land_use_ID","Land_use_type","Taxa_Used", "Local_CF"
      
      
      ntaxa = length(taxa) #numbert of taxa 
      nlanduse = length(lu_type) #number of land use 
      nbiome = length(biomes) #numbert of biomes 
      
      # definition of arrays to be filled
      
      #2D array to store the averages of CFlocal per land use and taxa
      
      CF_lu_tx <- array(1, dim=c(nlanduse,ntaxa), dimnames = list(lu_type, taxa))
      
      #1D array to store the averages per taxa
      
      CF_lu <- array(1, dim = nlanduse, dimnames = list(lu_type)) 
      
      #3D array to store CFloc per biome, land use and taxa
      
      CFloc <- array(data = NA, dim = c(nbiome,nlanduse,ntaxa), dimnames = list(biomes,lu_type, taxa))
      
      
      # LOCAL CF - calculation of the averages per land use and taxa
      
      #selection of all rows in the raw_data with a specific land use ID and taxa and then mean of the values (land use ID and taxa are passed by the i and j indexes from respectively the vectors land_use_ID and taxa)
      for (j in 1:nlanduse) {
        for (k in 1:ntaxa) {
          CF_lu_tx[j,k] = mean(raw_data[which(raw_data$Land_use_type == lu_type[j] & raw_data$Taxa_Used == taxa[k]), "Local_CF"])
        }
      }
      
      # #LOCAL CF - averages per taxa
      # 
      # #same procedure: selection of those CFloc corresponding to a given taxa in the df raw_data
      # for (k in 1:ntaxa) {
      #   CF_tx[k] = mean(raw_data[which(raw_data$Taxa_Used == taxa[k]), "Local_CF"])
      # }
      # 
      
      #LOCAL CF - averages per land use
      
      #same procedure: selection of those CFloc corresponding to a given land use in the df raw_data
      for (j in 1:nlanduse) {
        CF_lu[j] = mean(raw_data[which(raw_data$Land_use_type == lu_type[j]), "Local_CF"])
      }
      
      
      #LOCAL CF 
      
      nCFloc = nlanduse*ntaxa*nbiome #number of CFloc needed
      
      raw_data_group = list()  #list to store the data per biome, land use and taxa   
      raw_data_group_in = vector(mode = "list", length = nbiome*nlanduse*ntaxa) #list to store the indexes 
      
      #parameters to store/analyse the CF for which there are enough data, so averages over land uses or taxa are not needed
      nCFloc_complete = 0
      whichCFloc_complete = list(1) 
      nCFloc_raw_complete	= vector(mode = "integer", length = 1)
      CFloc_raw_complete = list(1)
      pos_complete = list(1)
      
      #calculation 
      
      for (i in 1:nbiome) {
        for (j in 1:nlanduse) {
          for (k in 1:ntaxa) {
          
            
            #print(paste0("biome: ", i, "; land use: ", land_use_type[j], "; taxa: ", taxa[k]))
            
            # if (length(which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))) <= 5) {
            #   #print("biome x land use x taxa < 5")
            # 
            #   if (length(which((raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))) <= 5) {
            #     #print("land use x taxa < 5")
            # 
            #     CFloc[i,j,k] = CF_lu[dimnames(CFloc)[[2]][j]]
            # 
            #     raw_data_group_in[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]]= c(i,k,j)
            #     raw_data_group[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]] = raw_data$Local_CF[which((raw_data$Biome_ID == biomes[i]) & (raw_data$Land_use_type == lu_type[j]) & (raw_data$Taxa_Used == taxa[k]))]
            # 
            #   } else {
            #     #print("biome x land use x taxa < 5, but land use x taxa > 5")
            #     
            #     CFloc[i,j,k] = CF_lu_tx[dimnames(CFloc)[[2]][j],dimnames(CFloc)[[3]][k]]
            # 
            #     raw_data_group_in[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]] = c(i,k,j)
            #     raw_data_group[[paste0(lu_type[j],"_",taxa[k],"_",biomes[i])]] = raw_data$Local_CF[which((raw_data$Biome_ID == biomes[i]) & (raw_data$Land_use_type == lu_type[j]) & (raw_data$Taxa_Used == taxa[k]))]
            # 
            #   }
            #   
            # } else {
            #   
            #print("biome x land use x taxa > 5")
            
            CFloc[i,j,k] = median(raw_data$Local_CF[which((raw_data$Biome_ID == biomes[i]) & (raw_data$Land_use_type == lu_type[j]) & (raw_data$Taxa_Used == taxa[k]))])
            
            raw_data_group_in[[(ntaxa*(j-1) + k)+(nlanduse*ntaxa)*(i-1)]] = c(i,j,k) 
            raw_data_group[[paste0(lu_type[j],"_",taxa[k],"_",biomes[i])]] = raw_data$Local_CF[which((raw_data$Biome_ID == biomes[i]) & (raw_data$Land_use_type == lu_type[j]) & (raw_data$Taxa_Used == taxa[k]))]
            
            nCFloc_complete = nCFloc_complete + 1
            whichCFloc_complete[[nCFloc_complete]] = which(CFloc == CFloc[i,j,k], arr.ind = TRUE)
            nCFloc_raw_complete[nCFloc_complete]	= length(raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))])
            CFloc_raw_complete[[nCFloc_complete]] = raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))]
            pos_complete[[nCFloc_complete]] = c(i,j,k)
            
            #}
            
            
          }
        }
      }
      
      
      result = vector(mode = "list", length = 5)
      result[[1]] = CFloc
      result[[2]] = raw_data_group
      result[[3]] = raw_data_group_in
      result[[4]] = CF_lu_tx
      result[[5]] = CF_lu
      result[[6]] = nCFloc_complete
      result[[7]] = CFloc_raw_complete
      result[[8]] = pos_complete
      
      
      return(result)
      
    }
    
    
        
    CFcalc_for = function(raw_data, taxa, lu_type, biomes) { #raw_data must contain at least the following columns: "Biomes_ID", "Land_use_ID","Land_use_type","Taxa_Used", "Local_CF"
      
      
      ntaxa = length(taxa) #numbert of taxa 
      nlanduse = length(lu_type) #number of land use 
      nbiome = length(biomes) #numbert of biomes 
      
      # definition of arrays to be filled
      
      #2D array to store the averages of CFlocal per land use and taxa
      
      CF_lu_tx <- array(1, dim=c(nlanduse,ntaxa), dimnames = list(lu_type, taxa))
      
      #1D array to store the averages per taxa
      
      CF_lu <- array(1, dim = nlanduse, dimnames = list(lu_type)) 
      
      #3D array to store CFloc per biome, land use and taxa
      
      CFloc <- array(data = NA, dim = c(nbiome,nlanduse,ntaxa), dimnames = list(biomes,lu_type, taxa))
      
      
      # LOCAL CF - calculation of the averages per land use and taxa
      
      #selection of all rows in the raw_data with a specific land use ID and taxa and then mean of the values (land use ID and taxa are passed by the i and j indexes from respectively the vectors land_use_ID and taxa)
      for (j in 1:nlanduse) {
        for (k in 1:ntaxa) {
          CF_lu_tx[j,k] = mean(raw_data[which(raw_data$Land_use_type == lu_type[j] & raw_data$Taxa_Used == taxa[k]), "Local_CF"])
        }
      }
      
      # #LOCAL CF - averages per taxa
      # 
      # #same procedure: selection of those CFloc corresponding to a given taxa in the df raw_data
      # for (k in 1:ntaxa) {
      #   CF_tx[k] = mean(raw_data[which(raw_data$Taxa_Used == taxa[k]), "Local_CF"])
      # }
      # 
      
      #LOCAL CF - averages per land use

      #same procedure: selection of those CFloc corresponding to a given land use in the df raw_data
      for (j in 1:nlanduse) {
        CF_lu[j] = mean(raw_data[which(raw_data$Land_use_type == lu_type[j]), "Local_CF"])
      }

      
      #LOCAL CF 
      
      nCFloc = nlanduse*ntaxa*nbiome #number of CFloc needed
      
      raw_data_group = list()  #list to store the data per biome, land use and taxa   
      raw_data_group_in = vector(mode = "list", length = nbiome*nlanduse*ntaxa) #list to store the indexes 
      
      #parameters to store/analyse the CF for which there are enough data, so averages over land uses or taxa are not needed
      nCFloc_complete = 0
      whichCFloc_complete = list(1) 
      nCFloc_raw_complete	= vector(mode = "integer", length = 1)
      CFloc_raw_complete = list(1)
      pos_complete = list(1)
      
      #calculation 
      
      for (i in 1:nbiome) {
        for (k in 1:ntaxa) {
          for (j in 1:nlanduse) {
            
            #print(paste0("biome: ", i, "; land use: ", land_use_type[j], "; taxa: ", taxa[k]))
            
            # if (length(which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))) <= 5) {
            #   #print("biome x land use x taxa < 5")
            # 
            #   if (length(which((raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))) <= 5) {
            #     #print("land use x taxa < 5")
            # 
            #     CFloc[i,j,k] = CF_lu[dimnames(CFloc)[[2]][j]]
            # 
            #     raw_data_group_in[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]]= c(i,k,j)
            #     raw_data_group[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]] = raw_data$Local_CF[which((raw_data$Biome_ID == biomes[i]) & (raw_data$Land_use_type == lu_type[j]) & (raw_data$Taxa_Used == taxa[k]))]
            # 
            #   } else {
            #     #print("biome x land use x taxa < 5, but land use x taxa > 5")
            #     
            #     CFloc[i,j,k] = CF_lu_tx[dimnames(CFloc)[[2]][j],dimnames(CFloc)[[3]][k]]
            # 
            #     raw_data_group_in[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]] = c(i,k,j)
            #     raw_data_group[[paste0(lu_type[j],"_",taxa[k],"_",biomes[i])]] = raw_data$Local_CF[which((raw_data$Biome_ID == biomes[i]) & (raw_data$Land_use_type == lu_type[j]) & (raw_data$Taxa_Used == taxa[k]))]
            # 
            #   }
            #   
            # } else {
            #   
              #print("biome x land use x taxa > 5")
              
              CFloc[i,j,k] = median(raw_data$Local_CF[which((raw_data$Biome_ID == biomes[i]) & (raw_data$Land_use_type == lu_type[j]) & (raw_data$Taxa_Used == taxa[k]))])
              
              raw_data_group_in[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]] = c(i,k,j) 
              raw_data_group[[paste0(lu_type[j],"_",taxa[k],"_",biomes[i])]] = raw_data$Local_CF[which((raw_data$Biome_ID == biomes[i]) & (raw_data$Land_use_type == lu_type[j]) & (raw_data$Taxa_Used == taxa[k]))]
              
              nCFloc_complete = nCFloc_complete + 1
              whichCFloc_complete[[nCFloc_complete]] = which(CFloc == CFloc[i,j,k], arr.ind = TRUE)
              nCFloc_raw_complete[nCFloc_complete]	= length(raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))])
              CFloc_raw_complete[[nCFloc_complete]] = raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))]
              pos_complete[[nCFloc_complete]] = c(i,j,k)
              
            #}
            
            
          }
        }
      }
      
      
      result = vector(mode = "list", length = 5)
      result[[1]] = CFloc
      result[[2]] = raw_data_group
      result[[3]] = raw_data_group_in
      result[[4]] = CF_lu_tx
      result[[5]] = CF_lu
      result[[6]] = nCFloc_complete
      result[[7]] = CFloc_raw_complete
      result[[8]] = pos_complete
      
      
      return(result)
      
    }
    
    
    
    
    # 
    # 
    # CFcalc_for = function(raw_data, taxa, lu_type, biomes) { #raw_data must contain at least the following columns: "Biomes_ID", "Land_use_ID","Land_use_type","Taxa_Used", "Local_CF"
    #   
    #   ntaxa = length(taxa) #numbert of taxa 
    #   nlanduse = length(lu_type) #number of land use 
    #   nbiome = length(biomes) #numbert of biomes 
    #   
    #   # definition of arrays to be filled
    #   
    #   #2D array to store the averages of CFlocal per land use and taxa
    #   
    #   CF_lu_tx <- array(1, dim=c(nlanduse,ntaxa), dimnames = list(lu_type, taxa))
    #   
    #   #1D array to store the averages per taxa
    #   
    #   CF_tx <- array(1, dim = ntaxa, dimnames = list(taxa)) 
    #   
    #   #3D array to store CFloc per biome, land use and taxa
    #   
    #   CFloc <- array(data = NA, dim = c(nbiome,nlanduse,ntaxa), dimnames = list(biomes,lu_type, taxa))
    #   
    #   # LOCAL CF - calculation of the averages per land use and taxa
    #   
    #   #selection of all rows in the raw_data with a specific land use ID and taxa and then mean of the values (land use ID and taxa are passed by the i and j indexes from respectively the vectors land_use_ID and taxa)
    #   for (j in 1:nlanduse) {
    #     for (k in 1:ntaxa) {
    #       CF_lu_tx[j,k] = mean(raw_data[which(raw_data$Land_use_type == lu_type[j] & raw_data$Taxa_Used == taxa[k]), "Local_CF"])
    #     }
    #   }
    #   
    #   #LOCAL CF - averages per taxa
    #   
    #   #same procedure: selection of those CFloc corresponding to a given taxa in the df raw_data
    #   for (k in 1:ntaxa) {
    #     CF_tx[k] = mean(raw_data[which(raw_data$Taxa_Used == taxa[k]), "Local_CF"])
    #   }
    #   
    #   #LOCAL CF 
    #   
    #   nCFloc = nlanduse*ntaxa*nbiome #number of CFloc needed
    #   
    #   raw_data_group = vector(mode = "list", length = nbiome*nlanduse*ntaxa)  #list to store the data per biome, land use and taxa   
    #   raw_data_group_in = vector(mode = "list", length = nbiome*nlanduse*ntaxa) #list to store the indexes 
    #   
    #   #parameters to store/analyse the CF for which there are enough data, so averages over land uses or taxa are not needed
    #   nCFloc_complete = 0
    #   whichCFloc_complete = list(1) 
    #   nCFloc_raw_complete	= vector(mode = "integer", length = 1)
    #   CFloc_raw_complete = list(1)
    #   pos_complete = list(1)
    #   
    #   #calculation 
    #   
    #   for (i in 1:nbiome) {
    #     for (k in 1:ntaxa) {
    #       for (j in 1:nlanduse){
    #         
    #         #print(paste0("biome: ", i, "; land use: ", land_use_type[j], "; taxa: ", taxa[k]))
    #         
    #         if (length(which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))) < 5) {
    #           #print("biome x land use x taxa < 5")
    #           
    #           if (length(which((raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))) < 5) {
    #             #print("land use x taxa < 5")
    #             
    #             CFloc[i,j,k] = CF_tx[dimnames(CFloc)[[3]][k]]
    #             
    #             raw_data_group_in[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]]= c(i,k,j) 
    #             raw_data_group[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]] = raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))]
    #             
    #           } else {
    #             #print("biome x land use x taxa < 5, but land use x taxa > 5")
    #             CFloc[i,j,k] = CF_lu_tx[dimnames(CFloc)[[2]][j],dimnames(CFloc)[[3]][k]]		
    #             
    #             raw_data_group_in[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]]= c(i,k,j) 
    #             raw_data_group[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]] = raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))]
    #             
    #           }
    #           
    #         } else {
    #           
    #           #print("biome x land use x taxa > 5")
    #           
    #           CFloc[i,j,k] = median(raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))])
    #           
    #           nCFloc_complete = nCFloc_complete + 1
    #           whichCFloc_complete[[nCFloc_complete]] = which(CFloc == CFloc[i,j,k], arr.ind = TRUE)
    #           nCFloc_raw_complete[nCFloc_complete]	= length(raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))])
    #           CFloc_raw_complete[[nCFloc_complete]] = raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))]
    #           pos_complete[[nCFloc_complete]] = c(i,j,k)
    #           
    #           raw_data_group_in[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]]= c(i,k,j) 
    #           raw_data_group[[(nlanduse*(k-1) + j)+(ntaxa*nlanduse)*(i-1)]] = raw_data$Local_CF[which((raw_data$Biome_ID == dimnames(CFloc)[[1]][i]) & (raw_data$Land_use_type == dimnames(CFloc)[[2]][j]) & (raw_data$Taxa_Used == dimnames(CFloc)[[3]][k]))]
    #           
    #           
    #         }
    #         
    # 
    #       }
    #     }
    #   }
    #   
    #   
    #   result = vector(mode = "list", length = 5)
    #   result[[1]] = CFloc
    #   result[[2]] = raw_data_group
    #   result[[3]] = raw_data_group_in
    #   result[[4]] = CF_lu_tx
    #   result[[5]] = CF_tx
    #   result[[6]] = nCFloc_complete
    #   result[[7]] = CFloc_raw_complete
    #   result[[8]] = pos_complete
    #   
    #   
    #   return(result)
    #   
    # }
    # 
    # 
    # specieslost_sim = function(Sorg, Aorg, Anew, Aij, zvalues, res_ratio, n) { # Sorg, Aij = 2d dataframe (Areas per land use), Anew = 1d array of remaining natural areas, Aorg = 1d array of original natural areas,
    #   # zvalues = 2d array of z values (nrows = necoregions, 2° dim = simulations), n = number of simulations 
    #  # res_ratio = 4d array (biomes x land use x taxa x simulations)
    # system.time(
    #   h <- suitablearea(res_ratio, zvalues, Aij, n)[[1]]) # calculation of h 
    # system.time(
    #   a_suit <- suitablearea(res_ratio, zvalues, Aij, n)[[2]]) # calculation of the product between h and Aij, a_suit = list of matrices (ecoregions x taxa)
    # 
    # #z_list <- lapply(1:n, function(x) {z[,x]}) # creation of a list where each element is a column of z (therefore a simulation for each column)
    # 
    # #Slost <- vector(mode = "list", length = n) # creation of a list for the species lost: each element of the list will be a 2d matrix, with the species lost per ecoregion and per taxa
    # 
    # # Anew = A_org_new_temp$A_new
    # # Aorg = A_org_new_temp$A_org
    # 
    # Slost <- array(NA, dim = c(dim(a_suit[[1]]),n), dimnames = list(rownames(a_suit[[1]]), colnames(a_suit[[1]]), 1:n))
    # 
    # for (i in 1:n) {
    #   
    #   areas_fr_temp = a_suit_fraction(Aorg, Anew, a_suit[[i]])
    #   # areas_fr_temp = 1-(areas_fr_temp^zvalues[,i])
    #   # areas_fr_temp = Sorg*areas_fr_temp
    #   # 
    #   Slost[,,i] = areas_fr_temp
    #   
    #   #Slost[,,i] = data.matrix((Sorg * (1-((Anew+a_suit[[i]])/Aorg)^zvalues[,i])))
    #   #Slost[[i]] = (Sorg * (1-((Anew+a_suit[[i]])/Aorg)^z_list[[i]]))
    #   #dimnames(Slost[[i]]) = list(dimnames(a_suit[[i]])[[1]], dimnames(a_suit[[i]])[[2]])
    #   
    # }
    # 
    # result <- list("h" = h, "a_suit" = a_suit, "Slost" = Slost)
    # 
    # return(result)
    # 
    # }   
