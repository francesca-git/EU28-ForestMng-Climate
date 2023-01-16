
setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 
#setwd("/home/frrosa/R/forest-management/") 

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
rename <- dplyr::rename

############################ INITIAL SETTINGS ######################################

  # General settings for the calculation of the impacts
    baseline = TRUE         # TRUE or FALSE. Default IS TRUE. TRUE: the script loads the areas modeled in GLOBIOM applying the baseline scenario (in the EU up to 138 Mha are under forest managemnt and imports and exports involve only intensive forest use, Plantation and Clear cut). FALSE: another option is selected.
    shared_effort = FALSE   # TRUE or FALSE. TRUE: the script loads the areas modelled by GLOBIOM with a shared-effort approach (in the EU up to 160 Mha are under forest management and imports come partially from low-intensity forest management). FALSE: another option is selected.
    lower_intensity = FALSE # TRUE or FALSE. TRUE: the script loads the areas modelled by GLOBIOM with a lower-intensity approach (same as baseline, but part of the imports and the exports are harvested from low-intensity management, though for the imports much less area is covered by low-intensity management than the shared-effort approach). FALSE: another option is selected.
                            # NB: the lower-intensity approach was tested but the results were not included in the published article.
    
    timber = FALSE          # TRUE or FALSE. FALSE = default setting which means that timber plantations are not included in the management practices considered part of EU clear cut areas have been allocated to Timber plantations). WARNING: This option is valid only if approach == "MG"
    CI = FALSE              # TRUE (confidence intervals are calculated) or FALSE (confidence intervals are not calculated)

  # Settings to define which .Rdata file to load for the response ratios and the z values 
    cutoff = TRUE           # TRUE (all raw RR > 1 are set to 1) or FALSE
    vulnerability = TRUE    # TRUE (global impacts are quantified) or FALSE. Default = TRUE. 
    BS = TRUE               # If CI = TRUE and BS = TRUE, confidence intervals are quantified with bootstrapping. If CI = TRUE and BS = FALSE, confidence intervals are quantified with MonteCarlo simulation. 
                            # if CI = FALSE, no confidence intervals are quantified so it does not matter which value is assigned to BS.

    subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)

############################# SETTING THE LOCATION OF THE DIRECTORIES NEEDED TO RUN THE MODEL, AGGREGATE THE RESULTS AND PLOT THEM  ############################# 

  source("./scripts/set_directories.R")
  

########## PLOTS FOR THE WORLD BIODIVERSITY FORUM ###########

year = "2100" # select the year. WARNING: use a character string containing the year (e.g., "2100")

source("./scripts/plotting/for_WBF/EU_barplots_WBF.R")
source("./scripts/plotting/plot_biomass-harvested.R")
source("./scripts/plotting/EU_barplots.R")                     # functions to plot the impacts of EU

          width_height = c(9, 27)
          EUFootprint.areas.barplot.EP(aggr_plot_path_areas, label_timber, file_label, plots_path, year, width_height)
          EUFootprint.volume.barplot.EP(areas_base_path, csv_path, plots_path, label_timber, file_label, year, width_height)  

          EU.areas.barplot.EP.dis(aggr_plot_path_areas, label_timber, file_label, plots_path, year, width_height)
          EU.volume.barplot.EP.dis(areas_base_path, csv_path, plots_path, label_timber, file_label, year, width_height)  
    
          EU.barplot.EP.dis(csv_path, file_label, plots_path, year, width_height)
          EUFootprint.barplot.EP(csv_path, file_label, plots_path, year, width_height)
          
            
          id = "EUFootprint" # options: "EUForest", "EUFootprint" or "Global"
          map = "PDF" # "PDF" 
          graph = "B-50" # "B-50" or "B-25-50", to be selected when id == "EUFootprint". "B-50" = the map will plot the following scenarios for 2100: Baseline, Multifunctional100% and Set-Aside100%
                          # "B-25-50" = = the map will plot the following scenarios for 2100: Baseline, Multifunctional50%, Multifunctional100%, SetAside50% and Set-Aside100%
          
          source("./scripts/plotting/for_WBF/map_PDF_multi.R")                   
          # This other function is meant to be tailored according to what it is needed to be plotted.
          