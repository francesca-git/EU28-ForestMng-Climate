
# Load the file and set the path for saving the output
# March 2021
# Author: Francesca Rosa

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

library(pacman)
p_load(dplyr, tidyr, abind, tidyverse, stringr)  # dataframe management and string management

p_load(ggplot2, colorspace, scico, viridis, RColorBrewer, gridExtra, nord, ggpubr, sf, sp, rgdal) # plotting

select <- dplyr::select

source("./plotting/scripts_prepareplotting/slost_prepare_for_plotting.R")                                # functions to convert the results of the model in a single .Rdata file
source(".//plotting/scripts_prepareplotting/global_Rdata-to-csv.R")                # functions to convert the .Rdata file to several .csv for impacts of global land use
source("./plotting/scripts_prepareplotting/areas_Rdata-to-csv.R")                  # functions to convert the .Rdata file to several .csv for areas
source("./plotting/scripts_charts/global_time-series.R")                                # functions to plot the impacts of global land use 

source("./plotting/scripts_prepareplotting/EU_Rdata-to-csv_.R")                     # functions to convert the .Rdata file to several .csv for EU impacts 
source("./plotting/scripts_charts/EU_barplots.R")                                       # functions to plot the impacts of EU
source("./plotting/scripts_charts/map_PDF.R")                                           # functions to plot the impacts on a map


############################ IDENTIFY THE PATH ##############################



# identify the results to access 
  type_of_data <- "impacts"       # "impacts" or "areas"

  case <- "cutoff"              # cutoff, nocutoff, cutoff_timber, nocutoff_timber, LCImpact
  subcase <- ""                   # e.g. plants, mammals or birds (end of the csv files)
  case_areas <- "notimber"        # "notimber" or "timber" 
   
  if(subcase == "") {
      case_subcase <- paste0("_", case)
      } else {case_subcase <- paste0("_", case, "_", subcase)}

  CI <- FALSE # TRUE or FALSE
  
  year = "2100"
  
  
############################ RESULTS DIRECTLY FROM THE MODEL ##############################

# access the results obtained directly from the model
  
  folder_slost = paste0("./results/slost/species-lost_", case, "/") 
  file_slost = "Slost_mg_"     # in the call of the file the year and the subcase are added
  
  

############################ ACCESS THE PLOTTING FOLDER ##############################

  # calculation -> PLOTTING -> RESULTS -> SUBFOLDER, whose name marks may refer to different types of results (with cutoff, without cutoff, with timber, without timber, etc)
  # -> CSV or PLOTS or .Rdata files 
  
    mainfolder <- "./plotting/results_plots-csv/"
    subfolder <- paste0(case, "/")  # subfolder from where the files must be loaded or to where the files must be saved
  
    plots_path <- paste0(mainfolder, subfolder,"plots/")  # path used to save the plots (generated using the .csv data)
      
    file_rdata <- paste0("sums", case_subcase, ".Rdata")
    file_rdata_areas <- paste0("areas_", case_areas, ".Rdata")

    rdata_path <- paste0(mainfolder, subfolder, file_rdata)
    rdata_path_areas <- paste0(mainfolder, "areas/", case_areas, "/", file_rdata_areas)
    
    csv_path <- paste0(mainfolder, subfolder,"csv/")      # path used to save csv (generated using the .Rdata)
    csv_path_areas <-  paste0(mainfolder, "areas/", case_areas, "/")     # path used to save csv with data on areas (generated using the .Rdata)
    
  

############################ PREPARE THE FILES FOR PLOTTING ##############################

      
  ################# PREPARE DATA FOR PLOTTING (model results -> aggregated .Rdata file) #######################
  
    prepare.for.plotting(folder_slost, rdata_path, case_subcase) # slost_prepare_for_plotting.R
      
      ################# PREPARE DATA FOR PLOTTING (aggregated .Rdata file -> .csv) #################
      # General task: convert the outputs of the model (.Rdata) into .csv files grouping and selecting only the most relevant 
          
      ######## EU ##########
          create.csv.EU(CI, rdata_path, csv_path, case_subcase) # EU_Rdata-to-csv.R
  
      ######## GLOBAL IMPACTS (DISAGGREGATED LAND USE CATEGORIES) ##########

          create.csv.global(CI, rdata_path, csv_path, case_subcase) # global_Rdata-to-csv.R
  
      ######## GLOBAL IMPACTS (CI) ##########

          create.csv.noAF(rdata_path, csv_path, case_subcase) # global.R
  
  
    
############################ PLOT ##############################

    
      ################# GLOBAL TIME SERIES (AGGREGATED, WITH CI) #######################
          
          plot.global.time.series.CI(csv_path, case_subcase, plots_path) # global_time-series.R --> noAF
      
      ################# GLOBAL TIME SERIES AND AREAS (DISAGGREGATED) #######################
  
          plot.global.time.series(csv_path, case_subcase, plots_path, case_areas)  # global_time-series.R
    
      ################# MAP OF GLOBAL/EU IMPACTS #######################
          id = "EUFootprint"
          plot.map(folder_slost, file_slost, case_subcase, plots_path, id) # map_PDF.R
         # id = can be "EUFootprint", "EUForest" or "Global"
      
      ################# BARPLOT OF EU FOOTPRINT  #######################
      
          EUfootprint.barplot(csv_path, case_subcase, plots_path, year) # EU_barplots.R

      ################# BARPLOT OF EU INTERNAL FOREST IMPACTS #######################
          
          EUinternal.barplot(csv_path, case_subcase, plots_path, year) # EU_barplots.R


############################ AREAS ##############################
    
      ################# PREPARE DATA FOR PLOTTING (model results -> aggregated .Rdata file) #######################
      
      
      
      ################# PREPARE DATA FOR PLOTTING (aggregated .Rdata file -> .csv files for EU) #######################
      
          create.csv.EU.areas(rdata_path_areas, csv_path_areas, case_areas, year) # EU_areas.R
          
      ################# PREPARE DATA FOR PLOTTING (aggregated .Rdata file -> .csv files for global impacts #######################

          create.csv.global.areas(rdata_path_areas, csv_path_areas, case_areas, year)   # global_areas.R   

            
#"areas.Rdata"
        # results from the script: areas_prepare_for_plotting.R
                            # data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                            # sums - list of dataframes dataframe containing the sums over ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum
                            # tstep - vector with the time steps
                            # results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                            # sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  


#file_rdata <- paste0("sums", case_subcase, ".Rdata")
        # results from the script: slost_aggregate.R
                            # data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                            # sums - list of dataframes dataframe containing the sums over ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum, Sum_lower95, Sum_upper95
                            # tstep - vector with the time steps
                            # results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                            # sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  
                            # results_CI - list of dataframes containing the sums over ecoregions per each year considering only the CI values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                            # sums_CI - list of dataframes dataframe containing the sums over ecoregions per each year considering only the CI values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum_upper, Sum_lower  
        

