
####### COMPUTATION OF SPECIES LOSS USING DIFFERENT VERSIONS OF THE MODEL #######

# The tests have been performed by using the CF available in Chaudhary et al. (2015), Chaudhary et al. (2016) and
# Chaudhary & Brooks (2018). The CF have been multiplied by the areas from GLOBIOM. As not all the versions have the same 
# land use classification, several assumptions have been made. To match the land use classification of the GLOBIOM model
# to Chaudhary et al. (2015) and Chaudhary et al. (2016) land use classification, the managed forest areas from GLOBIOM 
# have been grouped in intensive management (plantations, clear-cut) and extensive management (selection systems, selective logging,
# retention). For Chaudhary & Brooks (2018), more assumptions were needed and the detailed description of the matching process
# is provided in the file CF_Chaudhary_2018_input_with-description.
# The option with the inclusion of timber plantation as replacement for part of EU clear-cut is not included. 
 
#especially for Chaudhary & Brooks 

# working directory set with the script set-wd.R available in the main folder

library(dplyr)      # dataframe management
library(tidyr)      # dataframe management
library(abind)      # dataframe management
library(tidyverse)
library(parallel)
library(doParallel)
library(foreach)
select <- dplyr::select

input_data <- "2018"

########################## FUNCTION #################################

allocate.impacts <- function(df, fraction_of_areas) {
  # this function takes as input the impacts (as species loss) calculated with no distinction of 
  # sub-groups when the RR is the same (e.g. Annual_EU and Annual_RoW in the same ecoregion)
  # and multiplies them by the fraction of areas allocated for each sub-group.
  
  df_disaggr <- df %>% 
    full_join(fraction_of_areas, by = c("Scenario", "Ecoregion")) %>%
    transmute(Scenario = Scenario, Ecoregion = Ecoregion, Year = Year,
              Annual_EU_median = fr_Annual_EU*Annual_median, Permanent_EU_median = fr_Permanent_EU*Permanent_median, Pasture_EU_median = fr_Pasture_EU*Pasture_median, 
              Urban_EU_median = fr_Urban_EU*Urban_median, Annual_RoW_median = fr_Annual_RoW*Annual_median, Permanent_RoW_median = fr_Permanent_RoW*Permanent_median, 
              Pasture_RoW_median = fr_Pasture_RoW*Pasture_median, Urban_RoW_median = fr_Urban_RoW*Urban_median,
              For_ClearCut_EU_median = fr_For_ClearCut_EU*ClearCut_median, For_ClearCut_im_median = fr_For_ClearCut_im*ClearCut_median, For_ClearCut_ex_median = fr_For_ClearCut_ex*ClearCut_median,
              For_Retention_EU_median = fr_For_Retention_EU*Retention_median, 
              For_Plantation_im_median = fr_For_PlantationFuel_im*PlantationFuel_median + fr_For_PlantationTimber_im*Plantation_median,
              For_TimberPlant_EU_median = fr_For_TimberPlant_EU*Plantation_median, For_TimberPlant_ex_median = fr_For_TimberPlant_ex*Plantation_median, 
              For_SelectionSystem_EU_median = fr_For_SelectionSystem_EU*SelectionSystem_median,
              For_Selective_im_median = fr_For_Selective_im*SelectiveLogging_median,
              EP_EU_median = fr_EP_EU*Permanent_median, EP_conv_EU_median = fr_EP_conv_EU*Permanent_median, EP_RoW_median = fr_EP_RoW*PlantationFuel_median, EP_conv_im_median = fr_EP_conv_im*PlantationFuel_median, 
              Afforested_EU_median = fr_Afforested_EU*Afforested_median, Afforested_RoW_median = fr_Afforested_RoW*Afforested_median, Regrowth_median = fr_Regrowth*Afforested_median, 
              ForOther_Extensive_EU_median = fr_ForOther_Extensive_EU*ForExtensive_median, ForOther_Extensive_RoW_median = fr_ForOther_Extensive_RoW*ForExtensive_median, 
              ForOther_Intensive_EU_median = fr_ForOther_Intensive_EU*ForIntensive_median, ForOther_Intensive_RoW_median = fr_ForOther_Intensive_RoW*ForIntensive_median)
  
  test1 <- df_disaggr %>% mutate(sum_test1 = rowSums(select(., -Scenario, -Ecoregion, -Year), na.rm = TRUE)) %>%
    select(Scenario, Ecoregion, sum_test1)
  test2 <- df %>% mutate(sum_test2 = rowSums(select(., -Scenario, -Ecoregion, -Year), na.rm = TRUE)) %>%
    select(Scenario, Ecoregion, sum_test2)
  compare_df <- full_join(test1, test2, by = c("Scenario", "Ecoregion")) %>%
    mutate(diff = abs(sum_test1 - sum_test2))
  if(max(compare_df$diff, na.rm = TRUE) > 1e-16) {stop("ERROR in the allocation of disaggregated areas")}
  rm(test1, test2, compare_df)

  
  return(df_disaggr)
  
}


########################## LOAD THE DATA #################################

########################## VERIFY THE CLASS OF THE INPUT DATA - ALL COLUMNS EXCEPT THE FIST ONE MUST BE NUMERIC #################################

  if (input_data == "2016") {
  data <- read.csv("./comparison_other_model_verions/input_data/CF_LCImpact_2016_input.csv")
  end_name = "LCImpact"
  constant = 40
  } else if (input_data == "2015") {
    data <- read.csv("./comparison_other_model_verions/input_data/CF_Chaudhary_2015_input.csv")
    end_name = "Chaudhary2015"
    constant = 1
  } else if (input_data == "2018") {
    data <- read.csv("./comparison_other_model_verions/input_data/CF_Chaudhary_2018_input.csv")
    end_name = "Chaudhary2018"
    constant = 1
  }


########################## ADAPT THE FACTORS TO THE LAND USE DATA #################################

# replicate the CF in order to have them for each scenario

scenarios <- read.csv("./comparison_other_model_verions/input_data/Scenarios.csv")
scenarios = unlist(unique(scenarios))
nscenarios <- length(scenarios)

Ecoregions <- read.csv("./comparison_other_model_verions/input_data/Ecoregion_codes.csv")
Ecoregions_df <- unique(Ecoregions)
Ecoregions <- unlist(Ecoregions_df)
necoregions <- length(Ecoregions)

data <- data %>% full_join(Ecoregions_df)

########################## MULTIPLY THE CF AND THE AREAS #################################

temp = list.files(path = "./comparison_other_model_verions/input_data/areas/notimber/mg/aggregated/", pattern="*.csv", full.names = TRUE)
myfiles = lapply(temp, read.csv)
fr_temp = list.files(path = "./comparison_other_model_verions/input_data/areas/notimber/mg/fraction/", pattern="*.csv", full.names = TRUE)
myfiles_fr = lapply(fr_temp, read.csv)

rm(temp, fr_temp)

tstep = seq(from = 2000, to = 2100, by = 10)  


# Parallel for-loop
# Packages need to be passed on to each core

# Setup parallel computation (setting up a cluster that needs to be stopped later)
start.time = Sys.time()

UseCores = round(detectCores() / 5 * 2)  # Number of cores (parallel "workers") to be used
cl = makeCluster(UseCores)
registerDoParallel(cl)


################### LOOPS OVER THE YEARS ################### 

# Parallel for-loop
# Packages need to be passed on to each core

  results = foreach(t = 1:length(tstep), .packages=c('dplyr', 'tidyr', 'abind', 'tidyverse')) %dopar% {
   
    areas <- myfiles[[t]] %>%
                mutate_if(is.numeric, ~.*10000000000)
    fr_Areas <- myfiles_fr[[t]]
    
    nlandusecat <- length(areas %>% select(-Scenario, -Ecoregion, -A_org, -A_new))
    names_landusecat <- names(data %>% select(-Ecoregion))
    
    Slost_fin <- data.frame(matrix(NA, nrow = necoregions*nscenarios, ncol = 3+nlandusecat))
    
    names(Slost_fin) = c("Scenario", "Ecoregion", "Year", names_landusecat)
    
    Slost_fin$Scenario = rep(scenarios, each = necoregions)
    Slost_fin$Ecoregion = rep(Ecoregions, nscenarios)
    Slost_fin$Year = tstep[t]
    
    fr_Areas_new <- data.frame(matrix(NA, nrow = 0, ncol = 0))
    
    for (sc in 0:(nscenarios-1)) {

      print(paste0("scenario (start): ", scenarios[sc+1]))
    
      # Add the ecoregions which are not included in the GLOBIOM data
        areas_temp <- areas %>% filter(Scenario == toString(scenarios[sc+1])) %>%
                    select(-Scenario) %>%
                      full_join(Ecoregions_df) %>%
                        mutate(Ecoregion = as.character(Ecoregion)) %>%
                          arrange(Ecoregion) %>%
                            mutate(Ecoregion = as.factor(Ecoregion))
        
        matrix_areas <- data.matrix(areas_temp %>% select(-Ecoregion, -A_org, -A_new))
        # convert all elements of data to numeric (except the column Ecoregion)
        
        data_temp <- data %>% select(-Ecoregion) 
        indx <- sapply(data_temp, is.factor)
        data_temp[indx] <- lapply(data_temp[indx], function(x) as.numeric(as.character(x)))
        matrix_CF <- data.matrix(data %>% select(-Ecoregion))
        
        impacts <- matrix_areas*matrix_CF
        
      # test ==========
        
        col_in = sample(1:ncol(impacts), 1)
        row_in = sample(1:nrow(impacts), 1)
          
          while (is.na(impacts[row_in, col_in])) {
            col_in = sample(1:ncol(impacts), 1)
            row_in = sample(1:nrow(impacts), 1)
          }
      
        if(abs(impacts[row_in,col_in] - matrix_areas[row_in,col_in]*matrix_CF[row_in,col_in]) > 1e-16) {stop("ERROR in the product between areas and CFs")}
        #rm(col_in, row_in)
        
      # ======
        
        Slost_fin[((necoregions*sc)+1):(necoregions*(sc+1)), 4:(3+nlandusecat)] = impacts/constant
        
        # Add the ecoregions which are not included in the GLOBIOM data
          fr_temp <- fr_Areas %>% filter(Scenario == toString(scenarios[sc+1])) %>%
                  full_join(Ecoregions_df, by = "Ecoregion") %>%
                    mutate(Ecoregion = as.character(Ecoregion)) %>%
                      arrange(Ecoregion) %>%
                        mutate(Ecoregion = as.factor(Ecoregion)) %>%
                          mutate(Scenario = toString(scenarios[sc+1]))
          
          fr_Areas_new <- fr_Areas_new %>% bind_rows(fr_temp)
          
          rm(fr_temp, impacts, areas_temp, matrix_CF, matrix_areas)
        
    }
    
    Slost_fin_disaggr <- allocate.impacts(Slost_fin, fr_Areas_new)
    
    
    write.csv(Slost_fin_disaggr, paste0("./comparison_other_model_verions/results_", end_name,"/Slost_mg_", tstep[t], "_", end_name, ".csv"), row.names = FALSE)
    
    
  }
  
  # Stop the cluster
  stopCluster(cl)
  
  
  
  