setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF")
source("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/areas_allocation.R")

#install.packages("rgdal")
#install.packages("tidyverse")
#install.packages("compare")
#install.packages("compareDF")
library(rgdal)
library(dplyr)
library(tidyr)
#library(compare)
library(compareDF)

####### AREAS 

#Initial areas
import_forest_av <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Import_forest_area_average_Mha.csv", header = TRUE)
import_forest_mg <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Import_forest_area_marginal_Mha.csv", header = TRUE)
import_eplant <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Import_pellets_plantation_area_Mha.csv", header = TRUE)
EU_forest <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Internal_EU28_managed_forest_area_1000ha.csv", header = TRUE) #### in 1000ha and not Mha
EU_eplant <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Internal_EU28_perennial_energy_crops_area_1000ha.csv", header = TRUE) #### in 1000ha and not Mha
export_forest_av <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Export_forest_area_average_Mha.csv", header = TRUE)
export_forest_mg <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Export_forest_area_marginal_Mha.csv", header = TRUE)
broad_land_use_ref <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Land_use_area_ref_Mha.csv", header = TRUE)
broad_land_use_rcp <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Land_use_area_rcp_Mha.csv", header = TRUE)
forest_intensity_ref <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Forest_Intensity_ref_Mha.csv", header = TRUE)
forest_intensity_rcp <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Forest_Intensity_rcp_Mha.csv", header = TRUE)
Globiom_eco_org <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/GLOBIOM_Ecoregion.csv", header = TRUE)
#biome_realm <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Biomes_realms.csv", header = TRUE)
urban <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Chaudhary_2015_Areas.csv", header = TRUE, 
                  col.names = c("Ecoregion", "A_org", "A_new", "A_annual", "A_permanent", "A_pasture", "Urban", "A_ext_forest", "A_int_forest", "p_annual", "p_permanent", "p_pasture", "p_urban", "p_ext_forest", "p_int_forest"))
urban = urban[,c("Ecoregion", "Urban")] 







final_areas = list(
  "areas_av" = read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Fulvio_areas/Areas/AV/Areas_full/areas_full_av_2100.csv", header = TRUE),
  "areas_mg" = read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Fulvio_areas/Areas/MG/Areas_full/areas_full_mg_2100.csv", header = TRUE)
  )

old_areas = list(
  "areas_av_old" = read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Fulvio_areas/Old/20200510_Areas/20200510_AV_Areas/Areas/Areas_full/areas_full_av_2100.csv", header = TRUE),
  "areas_mg_old" = read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Fulvio_areas/Old/20200510_Areas/20200510_MG_Areas/Areas/Areas_full/areas_full_mg_2100.csv", header = TRUE)
  )

names(final_areas[[1]])
names(final_areas[[2]])
names(old_areas[[1]])
names(old_areas[[2]])

#calculate the total by row and column

final_areas[[1]] <- final_areas[[1]] %>%
                      mutate(For_RoW = For_Extensive + For_Intensive) %>%
                        select(-For_Extensive, - For_Intensive)

old_areas[[1]] <- old_areas[[1]] %>%
                      mutate(For_RoW = A_ClearCut_RoW + A_Retention_RoW + A_Plantation_RoW + A_Selection_RoW + A_Selective_RoW) %>%
                        select(-A_ClearCut_RoW, -A_Retention_RoW, -A_Plantation_RoW, -A_Selection_RoW, -A_Selective_RoW)
names(old_areas[[1]]) = names(final_areas[[1]])

summary(final_areas[[1]])
summary(old_areas[[1]])

nrow(final_areas[[1]])
nrow(old_areas[[1]])


sum_fin_scenarios = final_areas[[1]] %>%
                      group_by(Scenario) %>%
                        summarise_if(is.numeric, sum)

EU_fin_scenarios = sum_fin_scenarios  %>%
                    

sum_old_scenarios = old_areas[[1]] %>%
                      group_by(Scenario) %>%
                        summarise_if(is.numeric, sum)

sum_fin_eco = final_areas[[1]] %>%
                      group_by(Ecoregion) %>%
                        summarise_if(is.numeric, sum)

sum_old_eco = old_areas[[1]] %>%
                      group_by(Ecoregion) %>%
                        summarise_if(is.numeric, sum)

diff_eco = sum_fin_eco - sum_old_eco

which((sum_fin_eco-sum_old_eco)!= 0)

compare_eco = compare_df(sum_old_eco, sum_fin_eco)

col_sum_fin = colSums(final_areas[[1]][3:length(final_areas[[1]])])
col_sum_old = colSums(old_areas[[1]][3:length(old_areas[[1]])])

# colSums(final_areas[[1]][3:length(final_areas[[1]])])
# A_org            A_new           Annual        Permanent          Pasture            Urban      ClearCut_EU      ClearCut_im      ClearCut_ex     Retention_EU    Plantation_im 
# 250906.99684     107143.99370      22547.43387      14630.32285      34978.22139       3891.09412       1391.45237       1516.52548        653.34096        257.30458        377.73653 
# Selection_EU     Selection_im     Selection_ex     Selective_im      EnePlant_EU EnePlant_conv_EU     EnePlant_RoW EnePlant_conv_im       Afforested          For_RoW 
# 520.38219       1281.19565         43.56512        336.53749        298.57135        234.79934       6928.86570        137.76047      16187.05305      37550.84063 
# colSums(old_areas[[1]][3:length(old_areas[[1]])])
# A_org            A_new           Annual        Permanent          Pasture            Urban      ClearCut_EU      ClearCut_im      ClearCut_ex     Retention_EU    Plantation_im 
# 251604.20988      87731.40560      22547.43388      14630.32285      34978.22140       3891.09412       1391.45237       1516.52548        653.34096        257.30458        377.73653 
# Selection_EU     Selection_im     Selection_ex     Selective_im      EnePlant_EU EnePlant_conv_EU     EnePlant_RoW EnePlant_conv_im       Afforested          For_RoW 
# 520.38219       1281.19565         43.56512        336.53749        298.57135        234.79934       6928.86571        137.76047      16187.05305      57660.64174 

test1 = col_sum_fin["A_org"] - col_sum_fin["A_new"] - col_sum_fin["For_RoW"]
test2 = col_sum_old["A_org"] - col_sum_old["A_new"] - col_sum_old["For_RoW"]
# These two numbers are equal, and also all the columns have the same sum, expect the three used to calculate test1 and test2. The difference between the sum of A_org in the two cases is given by the difference
# between the old and the new values of A_new and "For_RoW". A_org old is larger than A_org final 

compare_av = compare_df(old_areas[[1]][,], final_areas[[1]])
