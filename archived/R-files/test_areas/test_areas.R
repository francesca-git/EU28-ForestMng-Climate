setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF")
source("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/areas_allocation.R")

#install.packages("rgdal")
#install.packages("tidyverse")
#install.packages("compare")
library(rgdal)
library(dplyr)
library(tidyr)
library(compare)

####### AREAS from Fulvio

#read the data

EU_forest <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Internal_EU28_managed_forest_area_1000ha.csv", header = TRUE) #### in 1000ha and not Mha
export_forest_av <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Export_forest_area_average_Mha.csv", header = TRUE)
export_forest_mg <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/Export_forest_area_marginal_Mha.csv", header = TRUE)
Globiom_eco_org <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/grouped_land_use_files/GLOBIOM_Ecoregion.csv", header = TRUE)


#from 1000ha to Mha  
EU_forest <- EU_forest %>%
  mutate_if(is.numeric, function(x) x/1000)

string = c("Selection_EU_ex",  "Clear_cut_EU_ex", "Selection_EU", "Clear_cut_EU")
replacement = c("Selection_EU_ex_2100",  "Clear_cut_EU_ex_2100", "Selection_EU_2100", "Clear_cut_EU_2100")
compare = list()

for (i in 1:2) {

compare_temp_ex <- export_forest_av %>%
                        filter(Ecoregion!="Lake", Ecoregion != "Rock and Ice") %>%
                          filter(Category == string[i]) %>%  
                            rename(Climate = Mitigation_scenario, Management = Management_scenario) %>%
                              unite("Scenario", Climate:Management, remove = TRUE) %>% # create one single column merging the information about scenarios (climate, 
                                arrange(Scenario, Ecoregion) 
  
  names(compare_temp_ex)[names(compare_temp_ex) == "X2100"] <- replacement[i]
                                 
compare_temp_EU <- EU_forest %>%
                    filter(Ecoregion!="Lake", Ecoregion != "Rock and Ice") %>%
                      filter(Category == string[[i+2]]) %>%
                        rename(Climate = Mitigation_scenario, Management = Management_scenario) %>%
                          unite("Scenario", Climate:Management, remove = TRUE) %>% # create one single column merging the information about scenarios (climate, 
                            arrange(Scenario, Ecoregion)

names(compare_temp_EU)[names(compare_temp_EU) == "X2100"] <- replacement[i+2]


compare_temp <- compare_temp_EU %>%
                select(Scenario, Ecoregion, contains("_2100")) %>%
                  bind_cols(compare_temp_ex) %>%
                    select(Scenario, Ecoregion, contains("_2100"))

compare_temp[, "diff"] = compare_temp[,(length(compare_temp)-1)] - compare_temp[,length(compare_temp)]

compare[[i]] = compare_temp

#negative_diff <- compare_S %>% filter(diff < 0 )

}

# compare[[1]] = data.frame(compare[[1]])
# compare[[2]] = data.frame(compare[[2]])
# 
# compare_tot <- compare[[1]] %>%
#                 bind_cols(compare[[2]])




Globiom_eco_org <- filter(Globiom_eco_org, Ecoregion!="Lake", Ecoregion != "Rock and Ice") 
EU_forest <- filter(EU_forest, Ecoregion!="Lake", Ecoregion != "Rock and Ice") 
export_forest_av <- filter(export_forest_av, Ecoregion!="Lake", Ecoregion != "Rock and Ice") 
export_forest_mg <- filter(export_forest_mg, Ecoregion!="Lake", Ecoregion != "Rock and Ice") 

Globiom_eco_org = droplevels(Globiom_eco_org, "Lake", "Rock and Ice")  #erase these two names from the list of factors
EU_forest = droplevels(EU_forest, "Lake", "Rock and Ice")  #erase these two names from the list of factors
export_forest_av = droplevels(export_forest_av, "Lake", "Rock and Ice")  #erase these two names from the list of factors
export_forest_mg = droplevels(export_forest_mg, "Lake", "Rock and Ice")  #erase these two names from the list of factors


eco_updated <- Globiom_eco_org %>%
                select(Ecoregion) %>%
                  distinct

tstep = seq(from = 2000, to = 2100, by = 10)  
ntstep = length(tstep) #time steps: 2000 -> 2100

fu_data = list(EU_forest, export_forest_av, export_forest_mg)
data.list = fu_data

data.list.steps <- vector(mode = "list", length = length(data.list))

  for (k in 1:length(data.list)) {
    
    #for each elment of data.list: 
    
    temp_data = data.list[[k]]
    temp_data = arrange(temp_data, Category)
    temp_data[temp_data<0] <- 0
    #vector with the list of land use categories
    
    temp.list = list()
    i = 11
    #for (i in 1:ntstep) {
      
      #for each time step:
      
      temp <- dplyr::select(temp_data, paste0("X",tstep[i]))   #select the column of the i-th time step
      df_i_temp <- dplyr::select(temp_data, -starts_with("X2"))  #eliminate all the other columns of the time serie  
      df_i_temp <- bind_cols(df_i_temp,temp)    #bind together temp and df_i_temp
      df_i = pivot_wider(df_i_temp, names_from = Category, values_from = paste0("X",tstep[i]))  #create a dataframe where each column (besides Ecoregion, Management etc.)                                                                                                       #corresponds to a land use class
      df_i = data.frame(df_i)
      df_i = droplevels(df_i, "Lake", "Rock and Ice")  #erase these two names from the list of factors
      temp.list <- df_i   #put df_i in the i-th element of the temporary list used then to pass the values to the final list
      
   # }
     
    data.list.steps[[k]] <- temp.list %>%
      rename(Climate = Mitigation_scenario, Management = Management_scenario) %>%
        inner_join(eco_updated)%>%
          unite("Scenario", Climate:Management, remove = TRUE) # create one single column merging the information about scenarios (climate, 
    data.list.steps[[k]]$Ecoregion <- as.factor(data.list.steps[[k]]$Ecoregion) 
    
    
  }

final_areas = list(
                "areas_av" = read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Fulvio_areas/Areas/AV/Areas_full/areas_full_av_2100.csv", header = TRUE),
                "areas_mg" = read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/Fulvio_areas/Areas/MG/Areas_full/areas_full_mg_2100.csv", header = TRUE)
                )

eco_EU <- EU_forest %>%
            select(Ecoregion) %>%
              distinct

EU_net = list()

for (i in 1:2) {

EU_net[[i]] <- data.list.steps[[1]] %>%
                select(-Retention_EU) %>%
                  arrange(Scenario, Ecoregion) %>%
                    mutate(Clear_cut_EU_net = Clear_cut_EU - data.list.steps[[i+1]]$Clear_cut_EU_ex) 
                      if (i == 1) {
                        EU_net[[i]] <- EU_net[[i]] %>%
                      mutate(Selection_EU_net = Selection_EU - data.list.steps[[i+1]]$Selection_EU_ex) 
                      }

final_areas[[i]] <- final_areas[[i]] %>%
                      inner_join(eco_EU) %>%
                        arrange(Scenario, Ecoregion)
                      if (i == 1) {
                        final_areas[[i]] <- final_areas[[i]] %>% select(Scenario, Ecoregion, ClearCut_EU, ClearCut_ex, Selection_EU, Selection_ex)
                      } else {
                        final_areas[[i]] <- final_areas[[i]] %>% select(Scenario, Ecoregion, ClearCut_EU, ClearCut_ex)
                      }

                    final_areas[[i]]$Ecoregion <- as.factor(final_areas[[i]]$Ecoregion) 

}

compare_areas_av = EU_net[[1]] %>%
                    select(Scenario, Ecoregion, Clear_cut_EU_net, Selection_EU_net) %>%
                      mutate(Clear_cut_EU_fin = final_areas[[1]]$ClearCut_EU, Clear_cut_ex = data.list.steps[[2]]$Clear_cut_EU_ex, Clear_cut_ex_fin = final_areas[[1]]$ClearCut_ex) %>%
                        mutate(Selection_EU_fin = final_areas[[1]]$Selection_EU, Selection_ex = data.list.steps[[2]]$Selection_EU_ex, Selection_ex_fin = final_areas[[1]]$Selection_ex) 


compare_S <- data.frame(compare[[1]])

compare_tot_S <- compare_S %>%
                  bind_cols(compare_areas_av) %>%
                    select(Scenario, Ecoregion, diff, Selection_EU_fin, Selection_EU_ex_2100, Selection_ex_fin) %>%
                      rename(Selection_EU_net = diff) %>%
                        mutate(diff_EU = Selection_EU_net - Selection_EU_fin,  diff_ex = Selection_EU_ex_2100 - Selection_ex_fin)


test_EU_S = sum(sqrt((compare_tot_S$diff_EU)^2))

test_ex_S = sum(sqrt((compare_tot_S$diff_ex)^2))

compare_CC <- data.frame(compare[[2]])

compare_tot_CC <- compare[[2]] %>%
                  bind_cols(compare_areas_av) %>%
                    select(Scenario, Ecoregion, diff, Clear_cut_EU_fin, Clear_cut_EU_ex_2100, Clear_cut_ex_fin) %>%
                      rename(Clear_cut_EU_net = diff) %>%
                        mutate(diff_EU = Clear_cut_EU_net-Clear_cut_EU_fin,  diff_ex = Clear_cut_ex_fin - Clear_cut_EU_ex_2100)

test_EU_CC = sum(sqrt((compare_tot_CC$diff_EU)^2))

test_ex_CC = sum(sqrt((compare_tot_CC$diff_ex)^2))

# 
# export_forest_av_S <- export_forest_av %>%
#   filter(Ecoregion!="Lake", Ecoregion != "Rock and Ice") %>%
#   filter(Category == "CLear_Cut_EU_ex") %>%  
#   rename(Climate = Mitigation_scenario, Management = Management_scenario) %>%
#   unite("Scenario", Climate:Management, remove = TRUE) %>% # create one single column merging the information about scenarios (climate, 
#   arrange(Scenario, Ecoregion) %>%
#   rename(Selection_ex_2100 = X2100)
# 
# EU_forest_S <- EU_forest %>%
#   filter(Ecoregion!="Lake", Ecoregion != "Rock and Ice") %>%
#   filter(Category == "Clear_Cut_EU") %>%
#   rename(Climate = Mitigation_scenario, Management = Management_scenario) %>%
#   unite("Scenario", Climate:Management, remove = TRUE) %>% # create one single column merging the information about scenarios (climate, 
#   arrange(Scenario, Ecoregion)
# 
# 
# 
# compare_S <- EU_forest_S %>%
#   rename(Selection_EU_2100 = X2100) %>%
#   select(Scenario, Ecoregion, Selection_EU_2100) %>%
#   mutate(Selection_ex_2100 = export_forest_av_S$Selection_ex_2100)
# 
# compare_S <- compare_S %>%
#   mutate(diff = Selection_EU_2100-Selection_ex_2100)
# 
# negative_diff <- compare_S %>% filter(diff < 0 )
# 

#share_eco_Glreg[("Globiom_Reg" == "ANZ")|("Scenario" == "RCP_MFM_AF0"),]
#share_eco_Glreg[(Globiom_Reg == "ANZ"),]
#sum(share_eco_Glreg[(share_eco_Glreg$Globiom_Reg == "ANZ")&(share_eco_Glreg$Scenario == "RCP_MFM_AF0"), "MngFor"])
