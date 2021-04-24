
# Load the file and set the path for saving the output
# March 2021
# Author: Francesca Rosa


setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 


type_of_data <- "impacts" # "impacts" or "areas"
subfolder <- "LCImpact/"  # subfolder from where the files must be loaded or to where the files must be saved
taxon <- "LCImpact"
case <- paste0("_", taxon)  # e.g. nocutoff_plants
end_slost <- "_LCImpact" # or e.g. nocutoff_notimber_taxa (end of the names of the folders in /calculation/results/slost/)
mainfolder <- "./plotting/results/"

save_csv_path_areas <- paste0(mainfolder, subfolder,"csv/areas/")      # path used to save csv with data on areas (generated using the .Rdata)
save_plots_path <- paste0(mainfolder, subfolder,"plots/")  # path used to save the plots (generated using the .csv data)
#load directly slost
open_folder_slost = paste0("./results/slost/species-lost", end_slost, "/mg/") 
file_slost = "Slost_"


  if (type_of_data == "areas") {
    
      file <- "areas.Rdata"
      # results from the script: areas_prepare_for_plotting.R
                          # data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                          # sums - list of dataframes dataframe containing the sums over ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum
                          # tstep - vector with the time steps
                          # results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                          # sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  
  }
  
  if (type_of_data == "impacts") {
    
      file <- paste0("sums", case, ".Rdata")
      # results from the script: slost_aggregate.R
                          # data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                          # sums - list of dataframes dataframe containing the sums over ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum, Sum_lower95, Sum_upper95
                          # tstep - vector with the time steps
                          # results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                          # sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  
                          # results_CI - list of dataframes containing the sums over ecoregions per each year considering only the CI values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                          # sums_CI - list of dataframes dataframe containing the sums over ecoregions per each year considering only the CI values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum_upper, Sum_lower  
      
  }

################# PATHS FOR SAVING #######################

save_csv_path <- paste0(mainfolder, subfolder,"csv/")      # path used to save csv (generated using the .Rdata)

################# LOADING OF .RDATA FILE #######################

load_path <- paste0(mainfolder, subfolder, file)              # complete path used to load the file

load(load_path)  

rm(mainfolder, load_path, subfolder, type_of_data)


















# load_path <- paste0(mainfolder, subfolder, file)         # complete path used to load the file
# 
# load(load_path)   # results from the script: slost_aggregate.R
#                     # data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#                     # sums - list of dataframes dataframe containing the sums over ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum
#                     # tstep - vector with the time steps
#                     # results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
#                     # sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  
