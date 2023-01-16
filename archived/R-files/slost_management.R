setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

library("triangle") # triangula distribution -> for the distribution of the z values
library(dplyr)      # dataframe management
library(tidyr)      # dataframe management
library(abind)      # dataframe management
library(ggplot2)

temp = list.files(pattern="*.csv")
myfiles_av = lapply(temp, read.csv)

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Fulvio_areas/Slost/MG") 

temp = list.files(pattern="*.csv")
myfiles_mg = lapply(temp, read.csv)

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Fulvio_areas/Slost")

# Retention -> Retention_EU
# Plantation -> Plantation_im

tstep = seq(from = 2000, to = 2100, by = 10)  

# av SelectiveLogging -> SelectiveLogging_im
# 
# mg Selection -> Selection_EU

  t = 11
  
  Slost_org = list(myfiles_av[[t]],myfiles_mg[[t]])
  approach = c("AV", "MG")
  
  Slost_aggr_tot = list()
  
  for (n in 1:2) { 

  Slost = Slost_org[[n]]
  
  Slost_scenarios <- Slost %>% 
                    #unite("Scenario", Climate:Management, remove = TRUE) %>%
                      select(-Ecoregion, -Year, -contains("upper95"), -contains("lower95")) %>%
                        group_by(Scenario) %>%
                          summarise_if(is.numeric, sum, na.rm = TRUE) 
  
  Slost_aggr <- Slost_scenarios %>%
                  select(Scenario)
  
  Slost_temp = Slost_scenarios %>%
                  select(Scenario, contains("_im"))
  Slost_aggr$Import = rowSums(Slost_temp[,2:length(Slost_temp)])
  
  Slost_temp =  Slost_scenarios %>%
                  select(Scenario, contains("_ex"))
  Slost_aggr$Export = rowSums(Slost_temp[,2:length(Slost_temp)])
  
  Slost_aggr$Net_import = Slost_aggr$Import - Slost_aggr$Export
  
  Slost_temp = Slost_scenarios %>%
                  select(Scenario, ((contains("_EU")&-contains(("EP_EU"))|contains("_ex")|contains("Extensive")|contains("Intensive")))) 
  Slost_aggr$For_EU = rowSums(Slost_temp[,2:length(Slost_temp)])
  
  Slost_aggr$For_RoW = Slost_scenarios$ForExtensive_median + Slost_scenarios$ForIntensive_median
  
  Slost_aggr$For_EUandImport = Slost_aggr$For_EU + Slost_aggr$Import
  
  Slost_aggr$Other_LU = Slost_scenarios$Annual_median + Slost_scenarios$Permanent_median + Slost_scenarios$Pasture_median + Slost_scenarios$Urban_median + Slost_scenarios$EP_EU_median + Slost_scenarios$EP_RoW_median
  
  Slost_aggr$Afforested = Slost_scenarios$Afforested_median
  
  Slost_aggr <- Slost_aggr %>% select(-Import, -Export)
  
  Slost_aggr <- Slost_aggr %>% separate(Scenario, c("Climate", "Forest_use", "Management"), "_") %>%
                arrange(Climate, Forest_use, Management)
  Slost_aggr$Total <- rowSums(Slost_scenarios[,2:length(Slost_scenarios)])

  Slost_aggr <- data.frame(Slost_aggr)
  
  write.csv(Slost_aggr, paste0("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Fulvio_areas/Slost_", approach[n] ,"_", tstep[t], "_aggr.csv"), row.names = FALSE)
  
  Slost_aggr_tot[[n]] = Slost_aggr
  
  }
  
  points_col = rainbow(24)

  dev.new()
  par(mfrow = c(4, 6))
  j = 0
  
  n = 2
  
  for (k in 1:4) {
    for (i in 4:(length(Slost_aggr_tot[[n]])-1)) {
      j = j +1
      plot(factor(Slost_aggr_tot[[n]][(6*(k-1)+1):(6*k),3]), Slost_aggr_tot[[n]][(6*(k-1)+1):(6*k),i],  
           xlab = "Management scenario", ylab = "PDF", pch = 16, col = points_col[j])
           title(main = paste0(approach[n], ", ", Slost_aggr_tot[[n]]$Climate[(6*(k-1)+1)], " - ", Slost_aggr_tot[[n]]$Forest_use[(6*(k-1)+1)], " - ", names(Slost_aggr_tot[[n]][i])))
    }
    
    
  }

  
  

  
  dev.new()
  par(mfrow = c(2, 2))

  
  for (k in 1:4) {
    
      plot(factor(Slost_aggr_tot[[n]][(6*(k-1)+1):(6*k),3]), Slost_aggr_tot[[n]][(6*(k-1)+1):(6*k),length(Slost_aggr_tot[[n]])],  
           xlab = "Management scenario", ylab = "PDF", pch = 16)
      title(main = paste0("Total ", approach[n], ", ", Slost_aggr_tot[[n]]$Climate[(6*(k-1)+1)], " - ", Slost_aggr_tot[[n]]$Forest_use[(6*(k-1)+1)]))
    
    
  }
  
  
  

# for (t in 1:length(tstep)) {
#   
#   temp <- myfiles_av[[t]] %>%
#     rename_at(vars(contains("Retention")), list(~ c("Retention_EU_median", "Retention_EU_lower95.", "Retention_EU_upper95."))) %>%
#     rename_at(vars(contains("Plantation")), list(~ c("PlantationFuel_im_median", "PlantationFuel_im_lower95.", "PlantationFuel_im_upper95."))) %>%
#     rename_at(vars(contains("Selective")), list(~ c("Selective_im_median", "Selective_im_lower95.", "Selective_im_upper95."))) 
#   
#   myfiles_av[[t]] <- temp
#   write.csv(myfiles_av[[t]], paste0("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Fulvio_areas/Slost_av_", tstep[t], "_V_full.csv"), row.names = FALSE)
#   
#   temp <- myfiles_mg[[t]] %>%
#     rename_at(vars(contains("Retention")), list(~ c("Retention_EU_median", "Retention_EU_lower95.", "Retention_EU_upper95."))) %>%
#     rename_at(vars(contains("Plantation")), list(~ c("PlantationFuel_im_median", "PlantationFuel_im_lower95.", "PlantationFuel_im_upper95."))) %>%
#     rename_at(vars(contains("Selection")), list(~ c("Selection_EU_median", "Selection_EU_lower95.", "Selection_EU_upper95."))) 
#   
#   myfiles_mg[[t]] <- temp
#   write.csv(myfiles_mg[[t]], paste0("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Fulvio_areas/Slost_mg_", tstep[t], "_V_full.csv"), row.names = FALSE)
#   
# }

                  