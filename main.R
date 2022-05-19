
###### SET THE PATH FOR ALL OPERATIONS, LOAD THE DATA, AGGREGATE THE RESULTS, PLOT THE RESULTS ###### 

# Load the file and set the path for saving the output
# March 2021
# Author: Francesca Rosa
####################
############################ SET WORKING DIRECTORY ################################

setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 

############################ LOAD LIBRARIES ################################

library(pacman)
p_load(dplyr, tidyr, abind, tidyverse, stringr)  # dataframe management and string management

p_load(ggplot2, colorspace, scico, viridis, RColorBrewer, gridExtra, nord, ggpubr, sf, sp, rgdal, rcartocolor) # plotting

p_load(dplyr, tidyr, compare, readr, purrr, tibble, stringr, rgdal)  


############### TO CHECK #################

library("triangle") # triangular distribution -> for the distribution of the z values
library(parallel)
library(doParallel)
library(foreach)
library(arsenal)
library(MASS)

select <- dplyr::select


############################ INITIAL SETTINGS ######################################

  # General settings for the calculation of the impacts
    marginal = TRUE         # TRUE or FALSE. Default IS TRUE. TRUE: the script loads the areas modelled by GLOBIOM with a marginal approach (imports and exports involve only intensive forest use, Plantation and Clear cut). FALSE: the script loads the areas modelled by GLOBIOM with an average approach (imports and exports involve all types of forest management). 
    timber = FALSE          # TRUE or FALSE. FALSE = default setting which means that timber plantations are not included in the management practices considered part of EU clear cut areas have been allocated to Timber plantations). WARNING: This option is valid only if approach == "MG"
    CI = FALSE              # TRUE (confidence intervals are calculated) or FALSE (confidence intervals are not calculated)

  # Settings to define which .Rdata file to load for the response ratios and the z values 
    cutoff = TRUE           # TRUE (all raw RR > 1 are set to 1) or FALSE
    vulnerability = FALSE    # TRUE (global impacts are quantified) or FALSE. Default = TRUE. 
    BS = TRUE               # If CI = TRUE and BS = TRUE, confidence intervals are quantified with bootstrapping. If CI = TRUE and BS = FALSE, confidence intervals are quantified with MonteCarlo simulation. 
                            # if CI = FALSE, no confidence intervals are quantified so it does not matter which value is assigned to BS.
    
    subcase = "mammals" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)

############################# SETTING THE LOCATION OF THE DIRECTORIES NEEDED TO RUN THE MODEL, AGGREGATE THE RESULTS AND PLOT THEM  ############################# 

  source("./scripts/set_directories.R")

    
    
############################ PREPARATION OF THE AREAS ######################################

   # Do you have your own areas? If yes, go to the next section "Preparation of the areas - user's areas" and DO NOT RUN THE NEXT LINES OF CODE. 
    
   # Are the folders data/land_use_data/areas_processed/notimber and data/land_use_data/areas_processed/timber and the relative sub-folders already 
   # containing the .csv files with the processed areas? 
   # If yes, DO NOT RUN THE NEXT LINES OF CODE and move directly to the section "Calculation of species loss (PDF)". 
   # if no, then run the next lines of code. 
    
    NR_W_included = FALSE # FALSE: default setting - the matching process will remove not relevant land and wetland from the land use areas, TRUE: the matching process will not remove
    # not relevant land and wetland from the land use areas. The defaul is FALSE becauseand not relevant lands have no matching land use classes in the biodiversity model
    # The option TRUE is used to produce the .csv files (available in data/land_use_data/areas_processed/NotRel-Wet) needed to plot the maps with the future land use according to the GLOBIOM model.
    source("./scripts/data_preparation/do_tidy_match.R")
    tidy.match.areas(timber, marginal, NR_W_included, areas_base_path, areas_processed_path)

############################ PREPARATION OF THE AREAS - USER'S AREAS ######################################
  
  # If you want to use your own areas to do the calculation, fill out the .csv file available at /data/land_use_data/areas_user/aggergated with your data. 
  # The columns of the file are the land use categories, the rows the ecoregions. In each cell, enter the area of the corresponding land use per that specific ecoregion (either in Mha,km2 or m2).
  # Then run the following line of code:

    areas_processed_path <- areas_processed_path_user
    

############################ CALCULATION OF SPECIES LOSS (PDF) ####################################

  # calculate.impacts.R
  
    source("./scripts/model/parameters_calculation.R")
    source("./scripts/model/CF_functions.R")
    source("./scripts/model/calculate_slost.R")
    source("./scripts/model/allocate_impacts.R")
    
    
    if (dir.exists(results_path) == TRUE) {file.rename(results_path, paste0(results_path, "_", format(as.Date(Sys.Date()), "%d.%m.%Y") ))} # check if the directory already exists. If it does, rename the older folder appending today's date to its name
    dir.create(results_path) # create the directory where the files will be saved
    #dir.create(paste0(results_path, "/by_species-group")) # create the directory where the files with the species-group-specific results will be saved
    
    source("./scripts/model/calculate_impacts.R") # calculate.impacts(cutoff, CI, BS, marginal, label, areas_processed_path, results_path, ecodata_path, vulnerability)   
    
    
    
############################ AGGREGATE THE RESULTS BOTH FOR ANALYSIS AND FOR PLOTTING ##############################
    
    
############################ AREAS ##############################

    year = "2100" # select the year. WARNING: use a character string containing the year (e.g., "2100")
 
      ################# AGGREGATE DATA (model results -> aggregated .Rdata file) #######################
         if (dir.exists(aggr_plot_path_areas) == TRUE) {file.rename(aggr_plot_path_areas, paste0(aggr_plot_path_areas, "_", format(as.Date(Sys.Date()), "%d.%m.%Y") ))} # check if the directory already exists. If it does, rename the older folder appending today's date to its name
          dir.create(aggr_plot_path_areas) # create the directory

          source("./scripts/aggregation/aggregate_areas.R")         # functions to convert the .csv files produced by the matching procedure and used as input in the 
          # biodiversity model to an aggregated .Rdata file (aggregation over land use)
          aggregate.areas(areas_processed_path, file_rdata_path_areas)   # aggregate_areas.R
      
      ################# PREPARE DATA FOR PLOTTING (aggregated .Rdata file -> .csv files for EU) #######################
          
          source("./scripts/aggregation/areas_Rdata-to-csv.R")         # functions to convert the .Rdata file to several .csv for areas
          create.csv.EU.areas(file_rdata_path_areas, aggr_plot_path_areas, label_timber, year) # areas_Rdata-to-csv.R
          
      ################# PREPARE DATA FOR PLOTTING (aggregated .Rdata file -> .csv files for global impacts #######################
          
          source("./scripts/plotting/map_areas.R")                       # functions to plot the impacts of global land use 
          create.csv.global.areas(file_rdata_path_areas, aggr_plot_path_areas, label_timber, year)   # areas_Rdata-to-csv.R
    
    

    
if (dir.exists(aggr_plot_path) == TRUE) {file.rename(aggr_plot_path, paste0(aggr_plot_path, "_", format(as.Date(Sys.Date()), "%d.%m.%Y") ))} # check if the directory already exists. If it does, rename the older folder appending today's date to its name
      dir.create(aggr_plot_path) # create the directory
      
      dir.create(plots_path) # create the director
      dir.create(csv_path)
      
  ################# AGGREGATE DATA OVER THE ECOREGIONSAND/OR LAND USES #######################
      
    # General task: convert .csv model results to an aggregated .Rdata file
    
    source("./scripts/aggregation/aggregate_results.R")          # functions to convert the results of the model in a single .Rdata file

    aggregate.results(results_path, file_rdata_path, file_label)     # aggregate_results.R - this function takes the results of the model calculation for a specific calculation setting
                                                         # defined by the code words at the beginning of this file (e.g. applying a cutoff and including timber plantations 
                                                         #, bootstrapping approach for the CI) and aggregate them as sum over the ecoregions or as total sums. 

  ################# PREPARE THE FILES TO BE PLOTTED #################
    
    # General task: convert the aggregated .Rdata files to aggregated .csv file, grouping and selecting only the most relevant 
     
    year = "2100" # select the year. WARNING: use a character string containing the year (e.g., "2100")
  
    energy_exports = "EPnoex" # "EPex" or "noEPnoex"
    
      ######## EU ##########
      source("./scripts/aggregation/EU_Rdata-to-csv.R")            # functions to convert the .Rdata file to several .csv for EU impacts 
    
        if (energy_exports == "ex") { create.csv.EU.ex(file_rdata_path, csv_path, file_label, year) # EU_Rdata-to-csv.R
          }else if(energy_exports == "EPnoex") { create.csv.EU.EPnoex(file_rdata_path, csv_path, file_label, year)
            }else if(energy_exports == "noEPnoex") {create.csv.EU.noEPnoex(file_rdata_path, csv_path, file_label, year)}
      
      ######## GLOBAL IMPACTS - DISAGGREGATED LAND USE CATEGORIES (PDF) ##########
    
          source("./scripts/aggregation/global_Rdata-to-csv.R")        # functions to convert the .Rdata file to several .csv for impacts of global land us
          create.csv.global(CI, file_rdata_path, csv_path, file_label, year) # global_Rdata-to-csv.R
  
      ######## GLOBAL IMPACTS - AGGREGATED LAND USE CATEGORIES AND CI (PDF) ##########
          
          create.csv.noAF(file_rdata_path, csv_path, file_label) # global_Rdata-to-csv.R
    
      ################# CALCULATE IMPACTS PER VOLUME OF WOOD (PDF*year/Mm3) #################
          
          source("./scripts/aggregation/convert_to_GLOBIOMres.R")
          convert.to.GLOBIOMres(results_path, csv_path, areas_base_path, file_label)  # convert_to_GLOBIOMres.R  --> aggregate the impacts using the resolution of Globiom regions
          
          source("./scripts/aggregation/impacts_per_volume.R")
          calculate.impacts.pervolume(csv_path, file_label, areas_base_path)         # impacts_per_volume.R --> calculate the impacts per volume of wood in each Globiom region

      ################# CALCULATE IMPACTS PER HECTARE (PDF/HA) #################
          
          source("./scripts/aggregation/convert_to_PDFha.R")
          convert.to.PDFha(results_path, areas_processed_path, csv_path, file_label)    # convert_to_PDFha.R --> calculate the impacts per hectar (ecoregion resolution)
          
          
          
          
          
############################ PLOT ##############################
          
    # Select the year to be plotted and if the exports are included or not 
          
    year = "2100" # select the year. WARNING: use a character string containing the year (e.g., "2100")
    energy_exports = "EPnoex" # "EPex" or "noEPnoex" or "EPnoex-dis" ("EPnoex-dis is an additional option to plot the barplot of the EU footprint with a disaggregation of the forest management typed for imported wood biomass)

      ################# GLOBAL TIME SERIES AND AREAS (DISAGGREGATED) #######################
          
          source("./scripts/plotting/plot_global_time-series.R")              # functions to plot the impacts of global land use 
          plot.global.time.series(csv_path, file_label, plots_path, label_timber, aggr_plot_path_areas)  # plot_global_time-series.R
    
      ################# BARPLOT OF EU FOOTPRINT  #######################
          
          source("./scripts/plotting/EU_barplots.R")                     # functions to plot the impacts of EU
          
          if (energy_exports == "ex") { EUfootprint.barplot.EP(csv_path, file_label, plots_path, year) # EU_barplots.R
            }else if(energy_exports == "EPnoex") { EUfootprint.barplot.EP(csv_path, file_label, plots_path, year)
              }else if(energy_exports == "noEPnoex") { EUfootprint.barplot.noEP(csv_path, file_label, plots_path, year)
                }else if(energy_exports == "EPnoex-dis") { EUfootprint.barplot.EP.dis(csv_path, file_label, plots_path, year)}
  
      ################# BARPLOT OF EU INTERNAL FOREST IMPACTS #######################
          
          if (energy_exports == "ex") { EUinternal.barplot.noEPex(csv_path, file_label, plots_path, year) # EU_barplots.R
            }else if(energy_exports == "EPnoex") { EUinternal.barplot.EPnoex(csv_path, file_label, plots_path, year)
            }else if(energy_exports == "noEPnoex") { EUinternal.barplot.noEPnoex(csv_path, file_label, plots_path, year)}
          
          
      ################# GLOBAL TIME SERIES (AGGREGATED, WITH CI) #######################
          
          plot.global.time.series.CI(csv_path, file_label, plots_path) # plot_global_time-series.R --> noAF
      
      ################# EUROPEAN TIME SERIES (AGGREGATED) #######################
          
          source("./scripts/plotting/plot_EU_time-series.R")            # function to plot the time series of EU internal impacts and footprint 
          id = "EUForest"
          plot.EU.timeseries(csv_path, file_label, plots_path, id, energy_exports)  # plot_EU_time-series.R 
          
          id = "EUFootprint"
          plot.EU.timeseries(csv_path, file_label, plots_path, id, energy_exports)  # plot_EU_time-series.R 
          
          
          
          # WARNING: THE FUNCTIONS TO PLOT THE MAPS DO NOT WORK IF THEY ARE CALLED HERE. THEREFORE, THEY HAVE TO BE RUN MANUALLY, AFTER 
          # SETTING THE id AND THE TYPE OF MAP, AS SHOWN HERE BELOW.

      ################# MAP OF GLOBAL/EU IMPACTS #######################
          
          id = "EUFootprint" # options: "EUForest", "EUFootprint" or "Global"
          map = "PDF" # "PDF" 
          graph = "B-50" # "B-50" or "B-25-50", to be selected when id == "EUFootprint". "B-50" = the map will plot the following scenarios for 2100: Baseline, Multifunctional100% and Set-Aside100%
                          # "B-25-50" = = the map will plot the following scenarios for 2100: Baseline, Multifunctional50%, Multifunctional100%, SetAside50% and Set-Aside100%
          
          source("./scripts/plotting/map_PDF.R")                        
          # Plots produced with this function: Global - 2020 vs 2100 RCP2.6 vs 2100 REF, EUFootprint - 2100 + RCP2.6 for MFM and SFM100; EUForest - 2100 + RCP2.6 vs REF for Baseline, MFM25%, MFM100%, SFM25%, SFM100% 

          source("./scripts/plotting/map_PDF_multi.R")                   
          # This other function is meant to be tailored according to what it is needed to be plotted.
          
      ################# MAP OF GLOBAL IMPACTS PER UNIT OF VOLUME #######################
          
          id = "EUFootprint"  # options: only "EUFootprint" 
          source("./scripts/plotting/map_PDF-Mm3.R")                     # functions to plot the impacts on a map

      ################# MAP OF GLOBAL/EU IMPACTS PER MHA #######################
          id = "EUFootprint"
          map = "PDFha" # "PDFha"
          source("./scripts/plotting/map_PDF_multi.R")                   
          
      ################# MAP OF GLOBAL LAND USE #######################
  
          source("./scripts/plotting/map_areas.R")

        #"areas.Rdata"
        # results from the script: areas_prepare_for_plotting.R
                            # data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                            # sums - list of dataframes dataframe containing the sums over ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum
                            # tstep - vector with the time steps
                            # results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                            # sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  


        #file_rdata <- paste0("sums", file_label, ".Rdata")
        # results from the script: slost_aggregate.R
                            # data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                            # sums - list of dataframes dataframe containing the sums over ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum, Sum_lower95, Sum_upper95
                            # tstep - vector with the time steps
                            # results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                            # sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  
                            # results_CI - list of dataframes containing the sums over ecoregions per each year considering only the CI values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                            # sums_CI - list of dataframes dataframe containing the sums over ecoregions per each year considering only the CI values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum_upper, Sum_lower  
        

