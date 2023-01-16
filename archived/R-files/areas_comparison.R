setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 
library(raster)  
library("viridis")  
library("wesanderson")
library(dplyr)
library(plot.matrix)

ref_df_alleco <- read.csv("C:/Users/Rosa/Documents/PhD_project/Recalculation_CF/input-data/modified/Chaudhary_2015_Areas_test_Mha.csv")

ref_df_alleco <- ref_df_alleco %>%  select(-contains("p_"))
                 

df <- read.csv("./areas/AV/aggregated/areas_aggr_av_2000.csv") 

df <- df %>% filter(Scenario == "REF_MFM_noAF") %>%
                select(-Scenario) %>%
                 mutate(ExtForest = Retention_EU + Selection + Selective_im + For_Extensive, Intforest = ClearCut + Plantation + For_Intensive) %>%
                  select(-Retention_EU, - Selection, - Selective_im, - For_Extensive, -ClearCut, - Plantation, - For_Intensive, - Afforested) %>%
                    rename(Eco_code = Ecoregion)

Ecoregions <- data.frame(df$Eco_code)
names(Ecoregions) = "Eco_code"
ref_df <- ref_df_alleco %>% inner_join(Ecoregions)

ref_df_remeco <- setdiff(ref_df_alleco, ref_df)

df_colsum <- colSums(df[2:length(df)])
ref_df_colsum <- colSums(ref_df[2:length(df)])
ref_df_alleco_colsum <- colSums(ref_df_alleco[2:length(df)])
ref_df_remeco_colsum <- colSums(ref_df_remeco[2:length(df)])
  
diff_rel <- sqrt(((ref_df-df)/ref_df)^2)
#diff_rel <- (((ref_df-df)/ref_df))

diff_rel$Eco_code <- df$Eco_code
diff_rel[diff_rel == Inf] <- NA
#diff_rel[diff_rel == -Inf] <- NA
diff_rel[diff_rel == "NaN"] <- NA

diff_rel2D <- data.matrix(diff_rel[2:length(diff_rel)])

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(diff_rel2D, border = NA,col = wes_palette("Zissou1", 100, type = "continuous"))

diff_rel2D[diff_rel2D > 1000] <- NA

plot(diff_rel2D, border = NA, col = wes_palette("Zissou1", 100, type = "continuous"))

diff_rel2D[diff_rel2D > 10] <- NA

plot(diff_rel2D, border = NA, col = wes_palette("Zissou1", 100, type = "continuous"))

plot(diff_rel2D[,c(2)], border = NA, col = wes_palette("Zissou1", 100, type = "continuous"))


# 
# ref_df_alleco <- read.csv("C:/Users/Rosa/Documents/PhD_project/Recalculation_CF/input-data/modified/Chaudhary_2015_Areas_test_Mha.csv")
# 
# ref_df_alleco <- ref_df_alleco %>%  
#                   select(-contains("p_")) %>%
#                     transmute(Scenario = "REF_MFM_noAF", Eco_code = Eco_code, A_org = A_org, A_new = A_new, Annual = Annual, Permanent = Permanent, Pasture = Pasture, Urban = Urban, Clearcut = 0, Retention = 0, PlantationFuel = 0, SelectionSystem = 0, SelectiveLogging = 0, Afforested = 0, ForExtensive = ExtForest, ForIntensive = IntForest)
# 
# ref_df <- ref_df_alleco %>% inner_join(Ecoregions)
# ref_df <- ref_df %>% rename(Ecoregion = Eco_code )
# write.csv(ref_df, "C:/Users/Rosa/Documents/PhD_project/Recalculation_CF/input-data/modified/Areas_2015_Mha.csv",row.names = FALSE)
