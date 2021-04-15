
# Title: Tidy the areas
# Task: Clean the input areas and put them in a suitable structure. The output of this script is a .Rdata file meant to be used as input in match_areas.R.
# Author: Francesca Rosa
# Date: started in March 2020
  
setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation")
  source("./scripts/areas_functions.R")
  #options(warn=0)
  
  #install.packages("rgdal")
  #install.packages("tidyverse")
  #install.packages("compare")
  library(rgdal)
  library(dplyr)
  library(tidyr)
  library(compare)
  library(readr)
  library(purrr)
  library(tibble)
  library(stringr)

                      ######################################### DECIDE WHAT THE SCRIPT SHOULD DO ###############################################

  
  read_the_data = TRUE
  clean_arrange_data = TRUE
  put_data_together = TRUE
  timber = "ON"  # default = "OFF". "OFF" = excluding timber plantations from EU forest managements, 
                  #"ON" = including timber plantations in EU forest managements for the sensitivity analysis
  
  
                      ################################################## READ THE DATA ##################################################

  if (read_the_data == TRUE) {
    
    import_forest_av <- read.csv("./grouped_land_use_files/Import_forest_area_average_Mha.csv", header = TRUE) # import forest areas (average approach), Mha
    import_forest_mg <- read.csv("./grouped_land_use_files/Import_forest_area_marginal_Mha.csv", header = TRUE) # import forest areas (marginal approach), Mha
    import_eplant <- read.csv("./grouped_land_use_files/Import_pellets_plantation_area_Mha.csv", header = TRUE) # import energy plantations, Mha
    EU_eplant <- read.csv("./grouped_land_use_files/Internal_EU28_perennial_energy_crops_area_1000ha.csv", header = TRUE) # EU energy plantations, in 1000ha and not Mha
    export_forest_av <- read.csv("./grouped_land_use_files/Export_forest_area_average_Mha.csv", header = TRUE) # export forest areas (average approach, no timber plantations), Mha
    EU_forest_temp <- read.csv("./grouped_land_use_files/Internal_EU28_managed_forest_area_1000ha_timber.csv", header = TRUE) # EU forest areas (timber plantations)
    export_forest_mg_temp <- read.csv("./grouped_land_use_files/Export_marginal_timber_Mha.csv", header = TRUE) # export forest areas (marginal approach, timber plantations), in 1000ha and not Mha
      EU_forest_temp[is.na(EU_forest_temp)] <- 0
      export_forest_mg_temp[is.na(export_forest_mg_temp)] <- 0
    
    # EU forest: with or without timber plantations (according to what is indicated at the beginning of the script with "timber")
      # including timber plantations in EU forest managements
        if (timber == "ON") {  
          EU_forest <- EU_forest_temp # keep the file as it is because it already includes timber plantations
          export_forest_mg <- export_forest_mg_temp # keep the file as it is because it already includes timber plantations  
          rm(EU_forest_temp, export_forest_mg_temp)
      # excluding timber plantations from EU forest managements
          # areas of timber plantations from EU_forest_temp and export_forest_mg will be replaced by 0 values
          # this way, the dataframe with and without timber plantations will have the same structure, number or rows and columns, but 0 Mha will be allocated in the dataframe where timber plantations msut be excluded
        } else if (timber == "OFF") {
            temp <- EU_forest_temp %>% filter(Category == "Timber_plant_EU") %>% # create a dataframe with 0 Mha as values for timber plantations 
                               mutate_if(is.numeric, ~(.*0))
            temp_notimb <- filter(EU_forest_temp, Category != "Timber_plant_EU") # filter the rows with other forest management type than timber plantations
            EU_forest <- bind_rows(temp_notimb, temp) # filter the rows which do not contains areas of timber plantations and bind them to the temporary dataframe with 0 values for timber plantations
            rm(temp)
            temp_notimb_ex <- read.csv("./grouped_land_use_files/Export_forest_area_marginal_Mha.csv", header = TRUE) # load the file with the export data without timber plantations
            temp <- export_forest_mg_temp %>% filter(Category == "Timber_plant_EU_ex") # filter the rows with timber plantations form export_forest_mg_temp
            temp <- temp %>% mutate_if(is.numeric, ~(.*0)) # convert the values to 0
            export_forest_mg <- bind_rows(temp_notimb_ex, temp) # bind the loaded file with the rows containing 0 as values for timber plantations
            rm(temp, EU_forest_temp, export_forest_mg_temp, temp_notimb_ex, temp_notimb)
        }

    broad_land_use_ref <- read.csv("./grouped_land_use_files/Land_use_area_ref_Mha.csv", header = TRUE) # Broad land use categories (annual crops, pastures, etc.) in the reference climatic mitigation scenario (RCP6.5), Mha
    broad_land_use_rcp <- read.csv("./grouped_land_use_files/Land_use_area_rcp_Mha.csv", header = TRUE) # Broad land use categories (annual crops, pastures, etc.) in the more ambitious climatic mitigation scenario (RCP2.6), Mha 
    forest_intensity_ref <- read.csv("./grouped_land_use_files/Forest_Intensity_ref_Mha.csv", header = TRUE) # Areas of Regrowth, Intensive forest management or Extensive forest management in the GLOBIOM regions for the reference climatic mitigation scenario (RCP6.5), Mha 
    forest_intensity_rcp <- read.csv("./grouped_land_use_files/Forest_Intensity_rcp_Mha.csv", header = TRUE) # Areas of Regrowth, Intensive forest management or Extensive forest management in the GLOBIOM regions for the more ambitious climatic mitigation scenario (RCP2.6), Mha 
    Globiom_eco_org <- read.csv("./grouped_land_use_files/GLOBIOM_Ecoregion.csv", header = TRUE) # mapping between GLOBIOM and the ecoregions, with the share of each ecoregion in each GLOBIOM region
    urban <- read.csv("./grouped_land_use_files/Chaudhary_2015_Areas.csv", header = TRUE,  # Urban areas
                      col.names = c("Ecoregion", "A_org", "A_new", "A_annual", "A_permanent", "A_pasture", "Urban", "A_ext_forest", "A_int_forest", "p_annual", "p_permanent", "p_pasture", "p_urban", "p_ext_forest", "p_int_forest"))
    urban = urban[,c("Ecoregion", "Urban")] 
  
  }
 
                        ################################################## CLEAN AND ARRANGE THE DATA ##################################################
  
    if (clean_arrange_data == TRUE) {
    
    # set the same unit for all the dataframes
    
      # from m2 to Mha
      urban <- urban %>%
          mutate_if(is.numeric, function(x) x/10000000000)
      
      # from 1000ha to Mha  
      EU_forest <- EU_forest %>%
          mutate_if(is.numeric, function(x) x/1000)
      
      EU_eplant <- EU_eplant %>%
          mutate_if(is.numeric, function(x) x/1000)
      
    # rearrange data in Globiom_eco
      
      Globiom_eco_org <- filter(Globiom_eco_org, Ecoregion!="Lake", Ecoregion != "Rock and Ice") 
      Globiom_eco_org = droplevels(Globiom_eco_org, "Lake", "Rock and Ice")  #erase these two names from the list of factors
      ecoregions_in_Globiom = unique(Globiom_eco_org$Ecoregion)
      ecoregions_in_Globiom <- data.frame(ecoregions_in_Globiom)
      names(ecoregions_in_Globiom) = "Ecoregion"
      ecoregion_areas = unique(broad_land_use_rcp$Ecoregion) 
      ecoregion_areas = data.frame(ecoregion_areas)
      names(ecoregion_areas) = list("Ecoregion")
      
      # ecoregions in EU 
      Globiom_eco_EU <- Globiom_eco_org %>%
                          inner_join(ecoregion_areas) %>%
                            filter(Globiom_Reg == "EU") %>%
                              mutate(Share = 1)             # the share is set to 1 because it does not matter how the ecoregions are shared outside EU (this is needed when the negative values for forest use in EU must be allocated to the positive)
      EU_ecoregions = unique(Globiom_eco_EU$Ecoregion)
      EU_ecoregions = data.frame(EU_ecoregions)
      names(EU_ecoregions) = list("Ecoregion")
      
    # define factors for the dataframes of broad land use, in order to make them ready to be bound together, and then bind them 
      
      broad_land_use_ref$Climate = factor("REF")
      broad_land_use_rcp$Climate = factor("RCP")
      
      forest_intensity_ref$Climate = factor("REF")
      forest_intensity_rcp$Climate = factor("RCP")
      
      # merge the two climate scenarios for broad land use and forest intensity 
      
      broad_land_use <-  bind_rows(broad_land_use_ref,broad_land_use_rcp)
      forest_intensity <- bind_rows(forest_intensity_ref,forest_intensity_rcp) #Extensive Intensive Regrowth
      
      broad_land_use$Climate <- as.factor(broad_land_use$Climate)
      forest_intensity$Climate <- as.factor(forest_intensity$Climate)
      forest_intensity[is.na(forest_intensity)] <- 0
      
    # useful definitions
      
      tstep = seq(from = 2000, to = 2100, by = 10)  
      ntstep = length(tstep) #time steps: 2000 -> 2100
      FUscenarios = c("MFM", "SFM") #Forest use 
      Cscenarios = c("REF", "RCP") #Climate/mitigation scenarios
      Mscenarios = c("noAF", "AF0", "AF25", "AF50", "AF75", "AF100") #Management scenario
      nscenarios = length(Mscenarios)*length(Cscenarios)*length(FUscenarios)
    
    # removal of rows that do not contain an ecoregion code: Lake and Rock and Ice
      
      broad_land_use <- filter(broad_land_use, Ecoregion!="Lake", Ecoregion != "Rock and Ice")
    
  }
  
                          ################################################## PUT THE DATA TOGETHER ##################################################
  
    if(put_data_together == TRUE) {
    
    # definition of different lists to re-arrange data into one single list
    
    data = list(import_forest_av, import_forest_mg, import_eplant, EU_forest, EU_eplant, export_forest_av, export_forest_mg, broad_land_use, forest_intensity)
    fu_data = list(import_forest_av, import_forest_mg, import_eplant, EU_forest, EU_eplant, export_forest_av, export_forest_mg)
    lu_data = list(broad_land_use, forest_intensity)
    
    # list of forest management data and removal of rows with Lake and Rock and Ice
    fu.list <- lapply(fu_data, function(x) filter(x, Ecoregion!="Lake", Ecoregion != "Rock and Ice") %>%
                                              rename(Climate = Mitigation_scenario, Management = Management_scenario))
    
    data.list <- append(fu.list,lu_data)
    
    data.list.steps <- vector(mode = "list", length = length(data.list))
    
  # for cycle over the dataframes in data.list to create a new list. It contains a number of elements equal to data.list. Each element is a sub-list, 
    # whose elements are dataframes, one for each time steps, with columns corresponding to the land use classes.  
  
    for (k in 1:length(data.list)) {
    
      #for each element of data.list: 
      
        temp_data = data.list[[k]]                # take element k of data.list
        temp_data = arrange(temp_data, Category)  # sort according to the Column Category (columns with the land use classes)
        temp_data[temp_data < 0] <- 0               # set to 0 all negative data (in some categories there are negative values of a very small order of magnitude, which are artifacts of the model)
        #vector with the list of land use categories
     
        temp.list = list()
    
      for (i in 1:ntstep) {
    
        #for each time step:
   
            temp <- dplyr::select(temp_data, paste0("X",tstep[i]))   #select the column of the i-th time step
            df_i_temp <- dplyr::select(temp_data, -starts_with("X2"))  #eliminate all the other columns of the time serie  
            df_i_temp <- bind_cols(df_i_temp,temp)    #bind together temp and df_i_temp
            df_i = pivot_wider(df_i_temp, names_from = Category, values_from = paste0("X",tstep[i]))  #create a dataframe where each column (besides Ecoregion, Management etc.)                                                                                                       #corresponds to a land use class
            df_i = data.frame(df_i)
            df_i = droplevels(df_i, "Lake", "Rock and Ice")  #erase these two names from the list of factors
            temp.list[[toString(tstep[i])]] <- df_i   #put df_i in the i-th element of the temporary list used then to pass the values to the final list
          
      }
        
      #two level list. First level: same as data.list (Import_forest_av, Import_forest_mg, Import_energy_plant, etc.). Second level: time steps.
      
      data.list.steps[[k]] <- temp.list
    
    
    }
  
      
    names(data.list.steps) = c("Import_forest_av", "Import_forest_mg", "Import_energy_plant", "EU_forest", "EU_energy_plant", "Export_forest_av", "Export_forest_mg", "Broad_land_use", "Forest_intensity")
  
    #cleaning the dataframe
    
    data.list.steps[["Broad_land_use"]] <- lapply(data.list.steps[["Broad_land_use"]], function(x)
                                             mutate(x, MngFor = MngFor_EU + MngFor_RoW,
                                                     NotRel = NotRel_EU + NotRel_RoW, PriFor = PriFor_EU + PriFor_RoW, WetLnd = WetLnd_EU + WetLnd_RoW) %>%
                                                select(-MngFor_EU, -MngFor_RoW, -NotRel_EU, -NotRel_RoW, -PriFor_EU, -PriFor_RoW, -WetLnd_EU, -WetLnd_RoW))
       
                                          
    }
  
                            ################################################## SAVE THE DATA TO A .RDATA FILE ##################################################

    
  if (timber == "OFF") { save(data.list.steps, ecoregions_in_Globiom, EU_ecoregions, Globiom_eco_EU, Globiom_eco_org, urban, file = "./results/areas/areas-to-match.RData") 
    } else if (timber == "ON") { save(data.list.steps, ecoregions_in_Globiom, EU_ecoregions, Globiom_eco_EU, Globiom_eco_org, urban, file = "./results/areas/areas-to-match_timber.RData") }
  
  rm(list = ls())
  
  