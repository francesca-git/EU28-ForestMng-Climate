# Title: Comparison between the non-linear approach and the linear approach (with CFs)
# Author: Francesca Rosa
# Date: 23/09/2021


# This file contains many unsuccessful tentatives of finding a way to compare the linear and the non-linear approach

# Ratio between species loss and areas

year = "2100"

data_areas <- read.csv(paste0(areas_processed_path, "/disaggregated/areas_disaggr_mg_", year[1], ".csv"))
  
data_areas_complete <- data_areas

data_areas <- data_areas %>% select(-A_org, -A_new)

Ecoregion_list <- unique(data_areas$Ecoregion)
  
results_path <- "./results/species-lost_cutoff_static_mg_VS0.5"
data_species <- read.csv(paste0(results_path, "/", result_files, year[1], file_label, ".csv"))

data_species <- data_species %>% select(Scenario, Ecoregion, contains("median")) 

col_old <- colnames(data_species)
col_new <- gsub(pattern = ".median", replacement = "", x  = col_old)
colnames(data_species) <- col_new
rm(col_old, col_new)

data_species <- data_species %>% add_column(For_PlantationFuel_im = 0, .before = "For_TimberPlant_EU") %>%
                  add_column(For_TimberPlant_im = 0, .before = "For_SelectionSystem_EU") %>%
                    mutate(For_PlantationFuel_im = For_Plantation_im/2, For_TimberPlant_im = For_Plantation_im/2) %>%
                      select(-For_Plantation_im) %>%
                        filter(Ecoregion %in% Ecoregion_list)



all.equal(names(data_areas), names(data_species))


ratio <- data_species

ratio[,3:(length(ratio))] <- data_species[,3:(length(ratio))]/data_areas[,3:(length(ratio))]

ratio[is.na(ratio)] <- 0

# example with one ecoregion
# Ecoregion = PA1209, Iberian sclerophyllous and semi-deciduous forests 

ratio_ex <- ratio %>% filter(Ecoregion == "PA1209")

data_species_ex <- data_species %>% filter(Ecoregion == "PA1209")

data_areas_ex <- data_areas %>% filter(Ecoregion == "PA1209")

plot(x = data_areas_ex$For_ClearCut_EU, y = data_species_ex$For_ClearCut_EU)

plot(x = data_areas$For_ClearCut_EU, y = data_species$For_ClearCut_EU)

plot(x = data_areas$Annual_EU, y = data_species$Annual_EU)

plot(x = data_areas_ex$Annual_EU, y = data_species_ex$Annual_EU)

plot(x = data_areas_ex$Permanent_EU, y = data_species_ex$Permanent_EU)


data_withCFs <- read.csv("./additional_analyses/comparison_other-model-versions/results/LCImpact/Slost_mg_2100_LCImpact.csv")

col_old <- colnames(data_withCFs)
col_new <- gsub(pattern = ".median", replacement = "", x  = col_old)
colnames(data_withCFs) <- col_new

rm(col_old, col_new)

data_withCFs <- data_withCFs %>%
              filter(Ecoregion %in% Ecoregion_list)

data_withCFs_ex <- data_withCFs %>% filter(Ecoregion == "PA1209")

plot(x = data_areas$For_ClearCut_EU, y = data_withCFs$For_ClearCut_EU)

plot(x = data_areas$Annual_EU, y = data_withCFs$Annual_EU)

plot(x = data_areas_ex$Annual_EU, y = data_withCFs_ex$Annual_EU)

plot(x = data_areas_ex$For_ClearCut_EU, y = data_withCFs_ex$For_ClearCut_EU)

plot(x = data_areas_ex$Permanent_EU, y = data_species_ex$Permanent_EU)



# Species loss vs Anew


data_species_tot <- data_species %>% mutate(Sum = rowSums(select(., contains("_") | contains("Regrowth")))) %>%
                      select(Scenario, Ecoregion, Sum) %>% 
                        inner_join(data_areas_complete %>% select(Scenario, Ecoregion, A_org, A_new)) %>%
                            mutate(A_artif = A_org - A_new) %>%
                              select(Scenario, Ecoregion, A_artif, A_new, Sum)
  
data_withCFs_tot <- data_withCFs %>% mutate(Sum = rowSums(select(., contains("_") | contains("Regrowth"))))%>%
                      select(Scenario, Ecoregion, Sum) %>% 
                        inner_join(data_areas_complete %>% select(Scenario, Ecoregion, A_org, A_new)) %>%
                            mutate(A_artif = A_org - A_new)  %>%
                              select(Scenario, Ecoregion, A_artif, A_new, Sum)

  
ecoreg <- "IM0102" # Borneo lowland rain forest


data_species_tot_ex <- data_species_tot %>% filter(Ecoregion == ecoreg)

data_withCFs_tot_ex <- data_withCFs_tot %>% filter (Ecoregion == ecoreg)

plot(x = data_species_tot$A_new, y = data_species_tot$Sum, ylim = c(0,0.0035), xlab = "Remaining natural area", ylab = "Log of Species loss (non linear)")
plot(x = data_withCFs_tot$A_new, y = data_withCFs_tot$Sum, ylim = c(0,0.0035), xlab = "Remaining natural area", ylab = "Log of Species loss (CFs)")

plot(x = data_species_tot$A_new, y = log(data_species_tot$Sum), ylim = c(-20,-5), xlab = "Remaining natural area", ylab = "Log of Species loss (non linear)")
plot(x = data_withCFs_tot$A_new, y = log(data_withCFs_tot$Sum), ylim = c(-20,-5), xlab = "Remaining natural area", ylab = "Log of Species loss (CFs)")

plot(x = data_species_tot$A_new, y = data_species_tot$Sum, log = "y", xlab = "Remaining natural area", ylab = "Log of Species loss (non linear)")
plot(x = data_withCFs_tot$A_new, y = data_withCFs_tot$Sum, log = "y", xlab = "Remaining natural area", ylab = "Log of Species loss (CFs)")

plot(x = data_species_tot$A_new, y = data_species_tot$Sum, ylim = c(0,0.0035), xlab = "Remaining natural area", ylab = "Species loss", col = 2, pch = 1)
points(x = data_species_tot$A_new, y = data_withCFs_tot$Sum, col = 5, pch = 1)
legend(legend = c("non linear", "linear (CFs)"), x = "topright",
       col = c(2, 5), pch = 1)

plot(x = data_species_tot$A_new, y = log(data_species_tot$Sum), ylim = c(-20,-5), xlab = "Remaining natural area", ylab = "Log of Species loss", col = 2, pch = 1)
points(x = data_species_tot$A_new, y = log(data_withCFs_tot$Sum), col = 5, pch = 1)
legend(legend = c("non linear", "linear (CFs)"), x = "topright",
       col = c(2, 5), pch = 1)

plot(x = data_species_tot$A_new, y = data_species_tot$Sum, log = "y", xlab = "Remaining natural area", ylab = "Species loss", col = 2, pch = 1)
points(x = data_species_tot$A_new, y = data_withCFs_tot$Sum, col = 5, pch = 1)
legend(legend = c("non linear", "linear (CFs)"), x = "topright",
       col = c(2, 5), pch = 1)


























