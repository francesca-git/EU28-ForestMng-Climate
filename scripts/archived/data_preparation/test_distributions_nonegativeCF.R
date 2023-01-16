
setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 

#load("./ratio.RData")
source("./CF_functions.R")
library("fitdistrplus")
library("tidyverse")
# functions ====
list_subsetting <- function(list, nelements) {
  for (i in 1:length(list)) {
    if(length(list[[i]]) < nelements) {
      names(list)[i] = "not_relevant"
    } 
  }
  list <- list[names(list) != "not_relevant"]
  return(list)
}

fromCFtoRR = function(data)  {
  data <- lapply(data, function(x) 1-x) #from CF to ratio
  data <- lapply(data, function(x) sapply(x, function(x) if (x == 0) {x = 1e-18} else {x = x}))
  data <- lapply(data, function(x) sapply(x, function(x) if (x > 1) {x = 1} else {x = x})) 
  data <- lapply(data, function(x) x[!is.na(x)])
  
  return(data)
}

plot_fromgroups <- function(input_data) {
  
  for (i in 1:length(input_data)) {
    
    title <- names(input_data)[i]
    
    data <- input_data[[i]]
    
    if(length(data) < 5) {
     
      plot.new()
      title(main = paste0(title, " - less than 5 dp"))
      plot.new()
      title(main = "Q-Q plot - less than 5 dp")
       
    } else{
    
    plot_distr(data, title)
    }
  }
  
}

chi2test <- function(measured, expected, data_breaks) {
  chi2_res <- sum((measured-expected)^2/expected)
  chi2_pvalue <- 1-pchisq(q = chi2_res, df = length(data_breaks)-2)
  results = list("chi2" = chi2_res, "pvalue" = chi2_pvalue)
  return(results)
}


plot_distr <- function(data, title) {
  fw <- fitdist(data, "weibull")
  fg <- fitdist(data, "gamma")
  fln <- fitdist(data, "lnorm")
  fn <- fitdist(data, "norm")
  
  ndata = length(data)
  measured <- hist(data, plot = F)$counts
  data_breaks <- hist(data, plot = F)$breaks
  expected_ln <- diff(plnorm(q = data_breaks, meanlog = fln$estimate[1], sdlog = fln$estimate[2]))*ndata
  expected_w <- diff(pweibull(q = data_breaks, shape = fw$estimate[1], scale = fw$estimate[2]))*ndata
  expected_g <- diff(pgamma(q = data_breaks, shape = fg$estimate[1], rate = fg$estimate[2]))*ndata
  
  pvalue_ln = chi2test(measured = measured, expected = expected_ln, data_breaks = data_breaks)[2]
  pvalue_w = chi2test(measured = measured, expected = expected_w, data_breaks = data_breaks)[2]
  pvalue_g = chi2test(measured = measured, expected = expected_g, data_breaks = data_breaks)[2]
  
  print(paste0("p value lnorm: ", pvalue_ln))
  print(paste0("p value weibull: ", pvalue_w))
  print(paste0("p value gamma: ", pvalue_g))
  
  plot.legend <- c(paste0("Weibull - p: ", pvalue_w), paste0("lognormal - p: ", pvalue_ln), paste0("gamma - p: ", pvalue_g))
  denscomp(list(fw, fln, fg), legendtext = plot.legend, main = paste0(title, " - dp: ", length(data)))
  qqcomp(list(fw, fln, fg), legendtext = plot.legend)
  # cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
  # ppcomp(list(fw, fln, fg), legendtext = plot.legend)
}
# ====

# FOREST USE ====

CF_local_raw_forest <- read.csv("./ecoregions_data/CF_local_forest-use.csv", header = TRUE, col.names = c("Taxa_Used", "Land_use_type", "Ratio", "Local_CF"))
CF_local_raw_forest$Biome_ID <- "Global" 
fu_types <- unique(CF_local_raw_forest$Land_use_type)
taxa <- unique(CF_local_raw_forest$Taxa_Used)
biomes <- "Global"

CF_local_forest <- CFcalc_group(CF_local_raw_forest, taxa, fu_types, biomes)[[2]]
ratio_forest = fromCFtoRR(CF_local_forest) # none of these values is 0

ratio_forest_birds <- names(ratio_forest) %>% 
                        str_detect('Birds') %>%
                          keep(ratio_forest, .)
ratio_forest_plants <- names(ratio_forest) %>% 
                        str_detect('Plants') %>%
                          keep(ratio_forest, .)
ratio_forest_mamm <- names(ratio_forest) %>% 
                      str_detect('Mammals') %>%
                        keep(ratio_forest, .)

# per forest use
CF_local_raw_forest$Taxa_Used <- "All"
CF_local_forestuse <- CFcalc_group(CF_local_raw_forest, "All", fu_types, biomes)[[2]]
ratio_forestuse = fromCFtoRR(CF_local_forestuse)

# LAND USE ====

CF_local_raw <- read.csv("./ecoregions_data/CF_local_complete.csv", header = TRUE)
lu_types_complete <- unique(CF_local_raw$Land_use_type)
taxa <- unique(CF_local_raw$Taxa_Used)
biomes <- unique(CF_local_raw$Biome_ID)
CF_local <- CFcalc_group(CF_local_raw, taxa, lu_types_complete, biomes)[[2]]

# GLOBAL - per land use ====
CF_local_raw_lu <- CF_local_raw 
CF_local_raw_lu$Biome_ID <- "Global"
CF_local_raw_lu$Taxa_Used <- "All"
CFloc_group_blu = CFcalc_group(CF_local_raw_lu, "All", lu_types_complete, "Global")[[2]]
ratio_blu_global = fromCFtoRR(CFloc_group_blu)

# GLOBAL - per taxa and land use ====

CF_local_raw_global <- CF_local_raw
CF_local_raw_global$Biome_ID <- "Global"
CFloc_global = CFcalc_group(CF_local_raw_global, taxa, lu_types_complete, "Global")[[2]]
ratio_global = fromCFtoRR(CFloc_global)


# More than 20 data points per biome-taxa-landuse combination ====

CF_local_raw_complete <- CF_local_raw
CFloc_complete = CFcalc(CF_local_raw_complete, taxa, lu_types_complete, biomes)[[7]]
CFloc_complete_in = CFcalc(CF_local_raw_complete, taxa, lu_types_complete, biomes)[[8]]

for (i in 1:length(CFloc_complete)) {
  if(length(CFloc_complete[[i]]) >= 20) {
    names(CFloc_complete)[i] = paste0(biomes[CFloc_complete_in[[i]][1]], "_", lu_types_complete[CFloc_complete_in[[i]][2]], "_", taxa[CFloc_complete_in[[i]][3]])
  } else {
    names(CFloc_complete)[i] = "not_relevant"
  }
}

CFloc_morethan20 <- CFloc_complete[names(CFloc_complete) != "not_relevant"]
ratio_morethan20 = fromCFtoRR(CFloc_morethan20)





# PLOTTING ====
dev.new()
par(mfrow = c(3, 4))


data <- ratio_blu_global[1:6]

plot_fromgroups(data)




# NAMES ====
# [1] "PlantationNonTimber_Plants_Global"   "PlantationNonTimber_Birds_Global"    "PlantationNonTimber_Mammals_Global" 
# [4] "ClearCut_Plants_Global"              "ClearCut_Birds_Global"               "ClearCut_Mammals_Global"            
# [7] "Retention_Plants_Global"             "Retention_Birds_Global"              "Retention_Mammals_Global"           
# [10] "PlantationFuel_Plants_Global"        "PlantationFuel_Birds_Global"         "PlantationFuel_Mammals_Global"      
# [13] "SlashAndBurn_Plants_Global"          "SlashAndBurn_Birds_Global"           "SlashAndBurn_Mammals_Global"        
# [16] "SelectionSystem_Plants_Global"       "SelectionSystem_Birds_Global"        "SelectionSystem_Mammals_Global"     
# [19] "SelectiveLogging_Plants_Global"      "SelectiveLogging_Birds_Global"       "SelectiveLogging_Mammals_Global"    
# [22] "Agroforestry_Plants_Global"          "Agroforestry_Birds_Global"           "Agroforestry_Mammals_Global"        
# [25] "Plantation_Plants_Global"            "Plantation_Birds_Global"             "Plantation_Mammals_Global"          
# [28] "ReducedImpactLogging_Plants_Global"  "ReducedImpactLogging_Birds_Global"   "ReducedImpactLogging_Mammals_Global"
# [31] "Afforested_Plants_Global"            "Afforested_Birds_Global"             "Afforested_Mammals_Global"          
# [34] "ForExtensive_Plants_Global"          "ForExtensive_Birds_Global"           "ForExtensive_Mammals_Global"        
# [37] "ForIntensive_Plants_Global"          "ForIntensive_Birds_Global"           "ForIntensive_Mammals_Global"   


# names(ratio_blu_global)
# [1] "Annual_All_Global"           "Permanent_All_Global"        "Pasture_All_Global"          "Forest_extensive_All_Global"
# [5] "Forest_intensive_All_Global" "Urban_All_Global


# 
# names(ratio_global)
# [1] "Annual_Plants_Global"               "Permanent_Plants_Global"            "Pasture_Plants_Global"              "Forest_extensive_Plants_Global"    
# [5] "Forest_intensive_Plants_Global"     "Urban_Plants_Global"                "Annual_Birds_Global"                "Permanent_Birds_Global"            
# [9] "Pasture_Birds_Global"               "Forest_extensive_Birds_Global"      "Forest_intensive_Birds_Global"      "Urban_Birds_Global"                
# [13] "Annual_Mammals_Global"              "Permanent_Mammals_Global"           "Pasture_Mammals_Global"             "Forest_extensive_Mammals_Global"   
# [17] "Forest_intensive_Mammals_Global"    "Urban_Mammals_Global"               "Annual_Amphibians_Global"           "Permanent_Amphibians_Global"       
# [21] "Pasture_Amphibians_Global"          "Forest_extensive_Amphibians_Global" "Forest_intensive_Amphibians_Global" "Urban_Amphibians_Global"           
# [25] "Annual_Reptiles_Global"             "Permanent_Reptiles_Global"          "Pasture_Reptiles_Global"            "Forest_extensive_Reptiles_Global"  
# [29] "Forest_intensive_Reptiles_Global"   "Urban_Reptiles_Global" 



# [1] "Annual_Plants_1"              "Annual_Birds_1"               "Annual_Mammals_1"             "Permanent_Plants_1"          
# [5] "Permanent_Birds_1"            "Permanent_Mammals_1"          "Pasture_Plants_1"             "Pasture_Birds_1"             
# [9] "Pasture_Mammals_1"            "Urban_Plants_1"               "Urban_Birds_1"                "Urban_Mammals_1"             
# [13] "Annual_Plants_2"              "Annual_Birds_2"               "Annual_Mammals_2"             "Permanent_Plants_2"          
# [17] "Permanent_Birds_2"            "Permanent_Mammals_2"          "Pasture_Plants_2"             "Pasture_Birds_2"             
# [21] "Pasture_Mammals_2"            "Urban_Plants_2"               "Urban_Birds_2"                "Urban_Mammals_2"             
# [25] "Annual_Plants_3"              "Annual_Birds_3"               "Annual_Mammals_3"             "Permanent_Plants_3"          
# [29] "Permanent_Birds_3"            "Permanent_Mammals_3"          "Pasture_Plants_3"             "Pasture_Birds_3"             
# [33] "Pasture_Mammals_3"            "Urban_Plants_3"               "Urban_Birds_3"                "Urban_Mammals_3"             
# [37] "Annual_Plants_4"              "Annual_Birds_4"               "Annual_Mammals_4"             "Permanent_Plants_4"          
# [41] "Permanent_Birds_4"            "Permanent_Mammals_4"          "Pasture_Plants_4"             "Pasture_Birds_4"             
# [45] "Pasture_Mammals_4"            "Urban_Plants_4"               "Urban_Birds_4"                "Urban_Mammals_4"             
# [49] "Annual_Plants_5"              "Annual_Birds_5"               "Annual_Mammals_5"             "Permanent_Plants_5"          
# [53] "Permanent_Birds_5"            "Permanent_Mammals_5"          "Pasture_Plants_5"             "Pasture_Birds_5"             
# [57] "Pasture_Mammals_5"            "Urban_Plants_5"               "Urban_Birds_5"                "Urban_Mammals_5"             
# [61] "Annual_Plants_6"              "Annual_Birds_6"               "Annual_Mammals_6"             "Permanent_Plants_6"          
# [65] "Permanent_Birds_6"            "Permanent_Mammals_6"          "Pasture_Plants_6"             "Pasture_Birds_6"             
# [69] "Pasture_Mammals_6"            "Urban_Plants_6"               "Urban_Birds_6"                "Urban_Mammals_6"             
# [73] "Annual_Plants_7"              "Annual_Birds_7"               "Annual_Mammals_7"             "Permanent_Plants_7"          
# [77] "Permanent_Birds_7"            "Permanent_Mammals_7"          "Pasture_Plants_7"             "Pasture_Birds_7"             
# [81] "Pasture_Mammals_7"            "Urban_Plants_7"               "Urban_Birds_7"                "Urban_Mammals_7"             
# [85] "Annual_Plants_8"              "Annual_Birds_8"               "Annual_Mammals_8"             "Permanent_Plants_8"          
# [89] "Permanent_Birds_8"            "Permanent_Mammals_8"          "Pasture_Plants_8"             "Pasture_Birds_8"             
# [93] "Pasture_Mammals_8"            "Urban_Plants_8"               "Urban_Birds_8"                "Urban_Mammals_8"             
# [97] "Annual_Plants_9"              "Annual_Birds_9"               "Annual_Mammals_9"             "Permanent_Plants_9"          
# [101] "Permanent_Birds_9"            "Permanent_Mammals_9"          "Pasture_Plants_9"             "Pasture_Birds_9"             
# [105] "Pasture_Mammals_9"            "Urban_Plants_9"               "Urban_Birds_9"                "Urban_Mammals_9"             
# [109] "Annual_Plants_10"             "Annual_Birds_10"              "Annual_Mammals_10"            "Permanent_Plants_10"         
# [113] "Permanent_Birds_10"           "Permanent_Mammals_10"         "Pasture_Plants_10"            "Pasture_Birds_10"            
# [117] "Pasture_Mammals_10"           "Urban_Plants_10"              "Urban_Birds_10"               "Urban_Mammals_10"            
# [121] "Annual_Plants_11"             "Annual_Birds_11"              "Annual_Mammals_11"            "Permanent_Plants_11"         
# [125] "Permanent_Birds_11"           "Permanent_Mammals_11"         "Pasture_Plants_11"            "Pasture_Birds_11"            
# [129] "Pasture_Mammals_11"           "Urban_Plants_11"              "Urban_Birds_11"               "Urban_Mammals_11"            
# [133] "Annual_Plants_12"             "Annual_Birds_12"              "Annual_Mammals_12"            "Permanent_Plants_12"         
# [137] "Permanent_Birds_12"           "Permanent_Mammals_12"         "Pasture_Plants_12"            "Pasture_Birds_12"            
# [141] "Pasture_Mammals_12"           "Urban_Plants_12"              "Urban_Birds_12"               "Urban_Mammals_12"            
# [145] "Annual_Plants_13"             "Annual_Birds_13"              "Annual_Mammals_13"            "Permanent_Plants_13"         
# [149] "Permanent_Birds_13"           "Permanent_Mammals_13"         "Pasture_Plants_13"            "Pasture_Birds_13"            
# [153] "Pasture_Mammals_13"           "Urban_Plants_13"              "Urban_Birds_13"               "Urban_Mammals_13"            
# [157] "Annual_Plants_14"             "Annual_Birds_14"              "Annual_Mammals_14"            "Permanent_Plants_14"         
# [161] "Permanent_Birds_14"           "Permanent_Mammals_14"         "Pasture_Plants_14"            "Pasture_Birds_14"            
# [165] "Pasture_Mammals_14"           "Urban_Plants_14"              "Urban_Birds_14"               "Urban_Mammals_14"            
# [169] "Annual_Plants_Artificial"     "Annual_Birds_Artificial"      "Annual_Mammals_Artificial"    "Permanent_Plants_Artificial" 
# [173] "Permanent_Birds_Artificial"   "Permanent_Mammals_Artificial" "Pasture_Plants_Artificial"    "Pasture_Birds_Artificial"    
# [177] "Pasture_Mammals_Artificial"   "Urban_Plants_Artificial"      "Urban_Birds_Artificial"       "Urban_Mammals_Artificial"  
# ====