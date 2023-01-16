# March 2021
# Author: Francesca Rosa, ETH Zürich


############################################################################
############################ SET WORKING DIRECTORY #########################
############################################################################

setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 


############################################################################
############################ LOAD LIBRARIES ################################
############################################################################

library(pacman)
p_load(dplyr, tidyr, abind, tidyverse, stringr, compare, readr, purrr, tibble, stringr, rgdal, foreach, arsenal, MASS)  # dataframe management and string management

p_load(ggplot2, colorspace, scico, viridis, RColorBrewer, gridExtra, nord, ggpubr, sf, sp, rgdal, rcartocolor) # plotting

select <- dplyr::select
rename <- dplyr::rename

####################################################################################
############################ INITIAL SETTINGS ######################################
####################################################################################

  # General settings for the calculation of the impacts
    baseline = TRUE         # TRUE or FALSE. Default IS TRUE. TRUE: the script loads the areas modeled in GLOBIOM applying the baseline scenario (in the EU up to 138 Mha are under forest managemnt and imports and exports involve only intensive forest use, Plantation and Clear cut). FALSE: another option is selected.
    shared_effort = FALSE     # TRUE or FALSE. TRUE: the script loads the areas modelled by GLOBIOM with a shared-effort approach (in the EU up to 160 Mha are under forest management and imports come partially from low-intensity forest management, namely Reduced Impact Logging). FALSE: another option is selected.
    lower_intensity = FALSE  # TRUE or FALSE. TRUE: the script loads the areas modelled by GLOBIOM with a lower-intensity approach (same as baseline, but part of the imports and the exports are harvested from low-intensity forestry, though for the imports much less area is covered by low-intensity management than the shared-effort approach and the management is selective logging instead of Reduced impact logging). FALSE: another option is selected.
                             # WARNING: the lower-intensity approach was tested but the results were not included in the published article.
    
    timber = FALSE          # TRUE or FALSE. FALSE = default setting which means that timber plantations are not included in the management practices considered part of EU clear cut areas have been allocated to Timber plantations). WARNING: This option is valid only for the Baseline scenario (when baseline == TRUE)
    CI = FALSE              # TRUE (confidence intervals are calculated) or FALSE (confidence intervals are not calculated)
                            # WARNING: the calculation with the CI takes lot of time and space (might not be suitable for a laptop)
    
  # Settings to define which .Rdata file to load for the response ratios and the z values 
    cutoff = FALSE           # TRUE (all raw RR > 1 are set to 1) or FALSE. WARNING: this option was a test and the results were not included in the final manuscript.
    vulnerability = TRUE     # TRUE (global impacts are quantified) or FALSE. Default = TRUE. The code must run with this option in his default value (TRUE)
    
    # if CI == TRUE, then one should decide how to calculate the CI (not relevant if CI == FALSE)
    BS = TRUE               # If CI = TRUE and BS = TRUE, confidence intervals are quantified with bootstrapping. If CI = TRUE and BS = FALSE, confidence intervals are quantified with MonteCarlo simulation. 
                            # if CI = FALSE, no confidence intervals are quantified so it does not matter which value is assigned to BS.

    subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
    # WARNING: The extinction risk is calculated for all the species groups aggregated and for each individually by default. 
    #          This option matters only when it comes to aggregating and plotting the results. 
    
    
############################# SETTING THE LOCATION OF THE DIRECTORIES NEEDED TO RUN THE MODEL, AGGREGATE THE RESULTS AND PLOT THEM  ############################# 

  # This script creates variables storing the relevant paths (e.g., aggr_plot_path is the path where aggregated results and plots are saved)
  source("./scripts/set_directories.R")
    
    
############################################################################################
############################ PREPARATION OF THE AREAS ######################################
############################################################################################

   # Do you have your own areas? If yes, go to the next section "Preparation of the areas - user's areas" and DO NOT RUN THE NEXT LINES OF CODE. 
    
   # Are the folders data/land_use_data/areas_processed/notimber and data/land_use_data/areas_processed/timber and the relative sub-folders already 
   # containing the .csv files with the processed areas? 
   # If yes, DO NOT RUN THE NEXT LINES OF CODE and move directly to the section "Calculation of species loss (PDF)". 
   # if no, then run the next lines of code. 
    
    NR_W_included = FALSE # FALSE: default setting - the matching process will remove not relevant land and wetland from the land use areas, TRUE: the matching process will not remove
    # not relevant land and wetland from the land use areas. The defaul is FALSE becausea not relevant land and wetland have no matching land use classes in the biodiversity model
    # The option TRUE is used to produce the .csv files (available in data/land_use_data/areas_processed/NotRel-Wet) to plot the maps with the future land use according to the GLOBIOM model.
    source("./scripts/data_preparation/do_tidy_match.R")
    
    tidy.match.areas(timber, label_approach, NR_W_included, areas_base_path, areas_processed_path)
    # label_approach = "Baseline", "SharedEffort" or "LowerIntensity". The string is defined in set_directories.R and based on the value of baselie, shared_effort and lower_intensity
  

###################################################################################################
############################ CALCULATION OF THE EXTINCTION RISK (PDF) #############################
###################################################################################################
    
  # calculate.impacts.R
  
    source("./scripts/model/parameters_calculation.R")
    source("./scripts/model/model_functions.R")
    source("./scripts/model/calculate_slost.R")
    source("./scripts/model/allocate_impacts.R")
    
    
    if (dir.exists(results_path) == TRUE) {file.rename(results_path, paste0(results_path, "_", format(as.Date(Sys.Date()), "%d.%m.%Y") ))} # check if the directory already exists. If it does, rename the older folder appending today's date to its name
    dir.create(results_path) # create the directory where the files will be saved
    #dir.create(paste0(results_path, "/by_species-group")) # create the directory where the files with the species-group-specific results will be saved
    
    source("./scripts/model/calculate_impacts.R") 
    # this script calculated the extinction risk and produces .csv files saved in 
    # ./results/, in the folder named after the scenario selected
    
    
# WARNING: In the original files, in the results and in the aggregated values (see below), the labeling of the scenarios is slightly different
#          compared to the acronym used in the manuscript. 
#          manuscript                 files used as input and output in the previous steps and in the aggregation (here below)
#          RCP2.6                     RCP
#          RCP6.5                     REF
#          CFM                        MFM (multifunctional forest management)
#          12.5% or SFM 12.5%         AFM25 (the difference in % is due to the fact that the % in the scenarios listed in the 
                                            # original files were referred to the EU suitable area, which is 1/2 than the area
                                            # currently under forest management, to which the figures and results in the manuscript are referred)
#          CFM 25% or SFM 25%         AFM50
#          CFM 37.5% or SFM 37.5%     AFM75
#          CFM 50% or SFM 50%         AFM100

    
    
####################################################################################################################
############################ AGGREGATE THE RESULTS FOR ANALYSIS AND FOR PLOTTING ##############################
####################################################################################################################
    
# This part of the script aggregates the results (e.g. over ecoregion or over land use or over sub-scenario)
    
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
          
############################# SPECIES GLOBAL EXTINCTION RISK ##############################
          
# If a folder with the aggregation results for a given scenario is already present, then add the today's date to the name of the folder and create a new one for the new results
if (dir.exists(aggr_plot_path) == TRUE) {file.rename(aggr_plot_path, paste0(aggr_plot_path, "_", format(as.Date(Sys.Date()), "%d.%m.%Y") ))} # check if the directory already exists. If it does, rename the older folder appending today's date to its name
      dir.create(aggr_plot_path) # create the directory
      
      dir.create(plots_path) # create the directory
      dir.create(csv_path)
      
      
  ################# AGGREGATE DATA OVER THE ECOREGIONS AND/OR LAND USES #######################
      
    # General task: convert .csv model results to an aggregated .Rdata file
    
    source("./scripts/aggregation/aggregate_results.R")          # functions to convert the results of the model in a single .Rdata file

    aggregate.results(results_path, file_rdata_path, file_label)     # aggregate_results.R - this function takes the results of the model calculation for a specific calculation setting
                                                         # defined by the code words at the beginning of this file (e.g. applying a cutoff and including timber plantations 
                                                         #, bootstrapping approach for the CI) and aggregate them as sum over the ecoregions or as total sums. 

  ################# PREPARE THE FILES TO BE PLOTTED #################
    
    # General task: convert the aggregated .Rdata files to aggregated .csv file, grouping and selecting only the most relevant. 
    # Most of the following scripts and function were prepared to be used with the results for a single year, which can be selected.
    
    year = "2100" # select the year. WARNING: use a character string containing the year (e.g., "2100")
  
    energy_exports = "EPnoex" # it means that in the analysis the energy crops and plantations are included, while the exports are excluded,
                              # consistently with the footprint approach
    
      ######## EU ##########
    
      source("./scripts/aggregation/EU_Rdata-to-csv.R") # functions to convert the .Rdata file to several .csv for EU impacts 
    
      create.csv.EU.EPnoex(file_rdata_path, csv_path, file_label, year)

      ################# CALCULATE VOLUME OF WOOD HARVESTED (Mm3) #################
          source("./scripts/aggregation/volume_per_category_oneyear.R")
          calculate.volume.EP.dis.oneyear(areas_base_path, csv_path, file_label, year) # calculate the volume (Mm3) of roundwood equivalent which is harvested in each forestry category in a given year to meet the EU forest biomass demand 
          # Script which creates the .csv file paste0(csv_path, "EUdemand_volumes_", year, file_label, "_disaggr.csv"), then used in the plotting function (see below) to obtain Figure S13
          # The .csv file produced in this script is used as input in the script impacts_per_volume_oneyear.R and the relative function calculate.impacts.pervolume.oneyear
          
      ################# CONVERT THE IMPACTS TO THE GLOBIOM REGION RESOLUTION [PDF] #################
          source("./scripts/aggregation/convert_to_GLOBIOMres.R")
          convert.to.GLOBIOMres(results_path, csv_path, areas_base_path, file_label)  # convert_to_GLOBIOMres.R  --> aggregate the impacts using the resolution of Globiom regions
          # This script produces a .csv file (paste0("slost-globiom", file_label, ".csv")).
          
      ################# CALCULATE IMPACTS PER VOLUME OF WOOD (PDF*year/Mm3) #################

          source("./scripts/aggregation/impacts_per_volume_oneyear.R")
          calculate.impacts.pervolume.oneyear(csv_path, year, file_label) # calculates the PDF * year / Mm3 per scenario and for the following grouped categories: EU28_forests, 	Import_forests,	Import_energy,	Imports_tot
          # This script produces the .csv file paste0(csv_path, "PDF_Mm3_", year, file_label, ".csv"), used to fill table S15.1
          # As input, it needs paste0(csv_path, "EUdemand_volumes_", year, file_label, "_disaggr.csv") produced with calculate.volume.EP.dis.oneyear and 
          # (paste0(csv_path, "EU_Footprint_", year, file_label, "_EP_disaggregated.csv") produced with create.csv.EU.EPnoex.
          
          # WARNING: only available for the Baseline scenario. 
          source("./scripts/aggregation/impacts_per_volume.R")
          calculate.impacts.pervolume(csv_path, file_label, areas_base_path)         # impacts_per_volume.R --> calculate the impacts per volume of wood in each Globiom region and in each ecoregion
          # WARNING: this script takes as input the .csv produced through the function convert.to.GLOBIOMres (paste0("slost-globiom", file_label, ".csv")).
          # The script produced two .csv files (paste0(csv_path, "PDF_Mm3_globiomregions_", file_label, ".csv") and paste0(csv_path, "PDF_Mm3", file_label, ".csv")).
          # The second one was used to fill out Table S18.1.
     
      ################# CALCULATE VOLUME OF WOOD PER CATEGORY FOR ALL YEARS (Mm3) #################
          # WARNING: only available for the Baseline scenario. It produced the .csv file paste0("Mm3_category", file_label, ".csv").
          # The .csv produced through this script was not used/published in the manuscript.
          source("./scripts/aggregation/volume_per_category.R")
          calculate.volume.per.category(csv_path, file_label, areas_base_path)    

      ################# CALCULATE IMPACTS PER HECTARE (PDF/HA) #################
          # WARNING: only available for the Baseline scenario. Additionally, the .csv produced through this script ("PDF_ha.csv" and "PDF_ha_tot.csv")) were not used/published in the manuscript.
          source("./scripts/aggregation/convert_to_PDFha.R")
          convert.to.PDFha(results_path, areas_processed_path, csv_path, file_label)    # convert_to_PDFha.R --> calculate the impacts per hectare (ecoregion resolution)
          

################################################################
############################ PLOT ##############################
################################################################
          
    # Select the year to be plotted and if the exports are included or not 
    # Most of the following scripts and function were prepared to be used with the results for a single year, which can be selected.
          
    year = "2100" # select the year. WARNING: use a character string containing the year (e.g., "2100")


      ################# BARPLOT OF EU FOOTPRINT  #######################
          
          source("./scripts/plotting/EU_barplots.R")                  
          library(cowplot)
          if(CI == TRUE) { width_height = c(30, 30) 
          } else if(CI == FALSE) { width_height = c(32, 9)}
          EU.barplot.EP.all.dis(csv_path, file_label, plots_path, year, width_height)
          
      ################# BARPLOT OF EU INTERNAL FOREST IMPACTS #######################
          
          source("./scripts/plotting/EU_barplots.R")                     
          width_height = c(32, 9)
          EUinternal.barplot.EPnoex(csv_path, file_label, plots_path, year, width_height)

      ################# BARPLOT OF EU DEMAND AREAS  #######################
          
          source("./scripts/plotting/EU_barplots.R")                     
          width_height = c(32, 9)
          EU.areas.barplot.EP.dis(aggr_plot_path_areas, label_timber, file_label, plots_path, year, width_height)

      ################# BARPLOT OF EU DEMAND HARVEST  #######################
         
          source("./scripts/plotting/EU_barplots.R")                  
          width_height = c(32, 9)
          EU.volume.barplot.EP.dis(csv_path, file_label, plots_path, year, width_height)

      ################# EUROPEAN TIME SERIES (AGGREGATED) #######################
          # Plotting function used to plot the Figures in S14
          source("./scripts/plotting/plot_EU_time-series.R")            # function to plot the time series of EU internal impacts and footprint 
          id = "EUForest"
          plot.EU.timeseries(csv_path, file_label, plots_path, id)  # plot_EU_time-series.R 
          
          id = "EUFootprint"
          plot.EU.timeseries(csv_path, file_label, plots_path, id)  # plot_EU_time-series.R 
          
      # Other plots:
      ###### To plot the maps, go to the file scripts/plotting/plot_all_maps.R #####
      ###### To plot the ranges of impacts per GLOBIOM region (Figure S18.1 and S18.2), go to the file scripts/plotting/impacts_per_volume_ranges.R #####
          
          
          