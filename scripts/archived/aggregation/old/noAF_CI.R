
#############################################################################################################################################################################

                                                                  # IMPACTS OF GLOBAL LAND USE (NO ALTERNATIVE FOREST MANAGEMENT SCENARIOS) #

#############################################################################################################################################################################

#############################################################################################################################################################################
# General task: convert the outputs of the model (.Rdata) into a .csv file with the total land use impacts over time considering only the two climate mitigation scenarios
# Date: September 2020
# Author: Francesca Rosa


#############################################################################################################################################################################

# WARNING: run the file load_and_save.R before running this one. load_and_save.R allows you to choose the .Rdata file to load and the location where the .csv files are saved

#############################################################################################################################################################################

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

# file loaded (.Rdata) using load_and_save.R: results from the script: slost_aggregate.R
# Content:
# data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              sums - list of dataframes dataframe containing the sums of the impacts over the ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum, Sum_lower95, Sum_upper95
#              tstep - vector with the time steps
#              results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  
#              results_CI - list of dataframes containing the sums over ecoregions per each year considering only the CI values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#              sums_CI - list of dataframes dataframe containing the sums over ecoregions per each year considering only the CI values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum_upper, Sum_lower  

library(dplyr)      # dataframe management
library(tidyr)      # dataframe management
library(abind)      # dataframe management
library(tidyverse)  # dataframe management
select <- dplyr:: select


create.csv.noAF <- function(rdata_path, csv_path, case_subcase) {
  
  load(rdata_path)  
  
#sums = sums_CI 

# Create a df containing all the sums for all the years
#df = data.frame(matrix(NA, nrow=(nrow(sums[[1]])*length(sums)), ncol=(ncol(sums[[1]])+1)))
df <- sums[[1]][1,]

for(i in 1:length(tstep)) {
  
  sums[[i]]$Year <- toString(tstep[i])
  df = df %>% bind_rows(sums[[i]])                     # bind columns together
}

df <- df[2:nrow(df),]
df <- df %>% 
        rename(PDFx100 = Sum_median, upper95 = Sum_upper95, lower95 = Sum_lower95) %>%
          dplyr::filter(Year != "2000" & Year != "2010") %>%
            filter((Group == "REF" | Group == "RCP2.6") & (Forest_use == "MFM") & (Management == "noAFM")) %>% 
              dplyr::select(-Forest_use, -Management) %>%
                arrange(Year)


write.csv(df, paste0(csv_path, "noAF_global_CI", case_subcase, ".csv"), row.names = FALSE)

}




