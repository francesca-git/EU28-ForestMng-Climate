setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 

library("triangle") # triangular distribution -> for the distribution of the z values
library(parallel)
library(doParallel)
library(foreach)
library(arsenal)
library(MASS)

select <- dplyr::select
rename <- dplyr::rename

############################ PDF ######################################

 ################################ BASELINE ##########################################

  # General settings for the calculation of the impacts
    baseline = TRUE         # TRUE or FALSE. Default IS TRUE. TRUE: the script loads the areas modeled in GLOBIOM applying the baseline scenario (in the EU up to 138 Mha are under forest managemnt and imports and exports involve only intensive forest use, Plantation and Clear cut). FALSE: another option is selected.
    shared_effort = FALSE   # TRUE or FALSE. TRUE: the script loads the areas modelled by GLOBIOM with a shared-effort approach (in the EU up to 160 Mha are under forest management and imports come partially from low-intensity forest management). FALSE: another option is selected.
    lower_intensity = FALSE # TRUE or FALSE. TRUE: the script loads the areas modelled by GLOBIOM with a lower-intensity approach (same as baseline, but part of the imports and the exports are harvested from low-intensity management, though for the imports much less area is covered by low-intensity management than the shared-effort approach). FALSE: another option is selected.
                            # NB: the lower-intensity approach was tested but the results were not included in the published article.
    
    timber = FALSE          # TRUE or FALSE. FALSE = default setting which means that timber plantations are not included in the management practices considered part of EU clear cut areas have been allocated to Timber plantations). WARNING: This option is valid only if approach == "MG"
    CI = TRUE              # TRUE (confidence intervals are calculated) or FALSE (confidence intervals are not calculated)

  # Settings to define which .Rdata file to load for the response ratios and the z values 
    cutoff = TRUE           # TRUE (all raw RR > 1 are set to 1) or FALSE
    vulnerability = TRUE    # TRUE (global impacts are quantified) or FALSE. Default = TRUE. 
    BS = TRUE               # If CI = TRUE and BS = TRUE, confidence intervals are quantified with bootstrapping. If CI = TRUE and BS = FALSE, confidence intervals are quantified with MonteCarlo simulation. 
                            # if CI = FALSE, no confidence intervals are quantified so it does not matter which value is assigned to BS.
    source("./scripts/plotting/plot_map.R")

    ############################ FOOTPRINT ######################################
    
        id = "EUFootprint" # options: "EUForest", "EUFootprint" or "Global"
        map = "PDF" # "PDF" 
        graph = "B-50" # "B-50" or "B-25-50", to be selected when id == "EUFootprint". "B-50" = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature100% and Set-Aside100%
                              # "B-25-50" = = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature50%, Closer-to-nature100%, SetAside50% and Set-Aside100%
        ratio = FALSE
        difference = FALSE
        
        subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
          source("./scripts/set_directories.R")
        plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
        # This other function is meant to be tailored according to what it is needed to be plotted.

        subcase = "mammals" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
          source("./scripts/set_directories.R")
        plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
    
        subcase = "birds" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
          source("./scripts/set_directories.R")
        plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
        
        subcase = "plants" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
          source("./scripts/set_directories.R")
        plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
        
     ############################ EU INTERNAL ######################################
       
        id = "EUForest" # options: "EUForest", "EUFootprint" or "Global"
        map = "PDF" # "PDF" 
        graph = "B-50" # "B-50" or "B-25-50", to be selected when id == "EUFootprint". "B-50" = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature100% and Set-Aside100%
                              # "B-25-50" = = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature50%, Closer-to-nature100%, SetAside50% and Set-Aside100%
        ratio = FALSE
        difference = FALSE
        
        subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
          source("./scripts/set_directories.R")
        plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
        # This other function is meant to be tailored according to what it is needed to be plotted.
        
        subcase = "mammals" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
          source("./scripts/set_directories.R")
        plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
    
        subcase = "birds" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
          source("./scripts/set_directories.R")
        plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
        
        subcase = "plants" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
          source("./scripts/set_directories.R")
        plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
    
        
 ################################ SHARED-EFFORT ##########################################
      
  # General settings for the calculation of the impacts
    baseline = FALSE         # TRUE or FALSE. Default IS TRUE. TRUE: the script loads the areas modeled in GLOBIOM applying the baseline scenario (in the EU up to 138 Mha are under forest managemnt and imports and exports involve only intensive forest use, Plantation and Clear cut). FALSE: another option is selected.
    shared_effort = TRUE   # TRUE or FALSE. TRUE: the script loads the areas modelled by GLOBIOM with a shared-effort approach (in the EU up to 160 Mha are under forest management and imports come partially from low-intensity forest management). FALSE: another option is selected.
    lower_intensity = FALSE # TRUE or FALSE. TRUE: the script loads the areas modelled by GLOBIOM with a lower-intensity approach (same as baseline, but part of the imports and the exports are harvested from low-intensity management, though for the imports much less area is covered by low-intensity management than the shared-effort approach). FALSE: another option is selected.
                            # NB: the lower-intensity approach was tested but the results were not included in the published article.
    
    timber = FALSE          # TRUE or FALSE. FALSE = default setting which means that timber plantations are not included in the management practices considered part of EU clear cut areas have been allocated to Timber plantations). WARNING: This option is valid only if approach == "MG"
    CI = FALSE              # TRUE (confidence intervals are calculated) or FALSE (confidence intervals are not calculated)

  # Settings to define which .Rdata file to load for the response ratios and the z values 
    cutoff = TRUE           # TRUE (all raw RR > 1 are set to 1) or FALSE
    vulnerability = TRUE    # TRUE (global impacts are quantified) or FALSE. Default = TRUE. 
    BS = TRUE               # If CI = TRUE and BS = TRUE, confidence intervals are quantified with bootstrapping. If CI = TRUE and BS = FALSE, confidence intervals are quantified with MonteCarlo simulation. 
                            # if CI = FALSE, no confidence intervals are quantified so it does not matter which value is assigned to BS.
  
    source("./scripts/plotting/plot_map.R") 
      
      ############################ FOOTPRINT ######################################
  
      id = "EUFootprint" # options: "EUForest", "EUFootprint" or "Global"
      map = "PDF" # "PDF" 
      graph = "B-50" # "B-50" or "B-25-50", to be selected when id == "EUFootprint". "B-50" = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature100% and Set-Aside100%
                            # "B-25-50" = = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature50%, Closer-to-nature100%, SetAside50% and Set-Aside100%
      ratio = FALSE
      difference = FALSE
      
      subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
        source("./scripts/set_directories.R")
      plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
      # This other function is meant to be tailored according to what it is needed to be plotted.
      
      subcase = "mammals" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
      plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
  
      subcase = "birds" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
        source("./scripts/set_directories.R")
      plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
      
      subcase = "plants" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
        source("./scripts/set_directories.R")
      plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
      
      ############################ EU INTERNAL ######################################
     
      id = "EUForest" # options: "EUForest", "EUFootprint" or "Global"
      map = "PDF" # "PDF" 
      graph = "B-50" # "B-50" or "B-25-50", to be selected when id == "EUFootprint". "B-50" = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature100% and Set-Aside100%
                            # "B-25-50" = = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature50%, Closer-to-nature100%, SetAside50% and Set-Aside100%
      ratio = FALSE
      difference = FALSE
      
      subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
        source("./scripts/set_directories.R")
      plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
      # This other function is meant to be tailored according to what it is needed to be plotted.
      
      subcase = "mammals" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
        source("./scripts/set_directories.R")
      plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
  
      subcase = "birds" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
        source("./scripts/set_directories.R")
      plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
      
      subcase = "plants" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
        source("./scripts/set_directories.R")
      plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
  
      
############################ RATIO AND DIFFERENCE ######################################
       
      source("./scripts/plotting/plot_map.R")

  ############################ BASELINE ######################################
    
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

    id = "EUFootprint" # options: "EUForest", "EUFootprint" or "Global"
    map = "PDF" # "PDF" 
    graph = "B-50" # "B-50" or "B-25-50", to be selected when id == "EUFootprint". "B-50" = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature100% and Set-Aside100%
                          # "B-25-50" = = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature50%, Closer-to-nature100%, SetAside50% and Set-Aside100%
    ratio = TRUE
    difference = FALSE
    
    subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
      source("./scripts/set_directories.R")
    plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
    # This other function is meant to be tailored according to what it is needed to be plotted.
    
    ratio = FALSE
    difference = TRUE
    
    subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
      source("./scripts/set_directories.R")
    plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
    # This other function is meant to be tailored according to what it is needed to be plotted.
    
    id = "EUForest" # options: "EUForest", "EUFootprint" or "Global"
    map = "PDF" # "PDF" 
    graph = "B-25-50" # "B-50" or "B-25-50", to be selected when id == "EUFootprint". "B-50" = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature100% and Set-Aside100%
               
    ratio = TRUE
    difference = FALSE
    
    subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
      source("./scripts/set_directories.R")
    plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
    # This other function is meant to be tailored according to what it is needed to be plotted.
    
    ratio = FALSE
    difference = TRUE
    
    subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
      source("./scripts/set_directories.R")
    plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
    # This other function is meant to be tailored according to what it is needed to be plotted.
    
    ############################ SHARED-EFFORT ######################################
    
    # General settings for the calculation of the impacts
      baseline = FALSE         # TRUE or FALSE. Default IS TRUE. TRUE: the script loads the areas modeled in GLOBIOM applying the baseline scenario (in the EU up to 138 Mha are under forest managemnt and imports and exports involve only intensive forest use, Plantation and Clear cut). FALSE: another option is selected.
      shared_effort = TRUE   # TRUE or FALSE. TRUE: the script loads the areas modelled by GLOBIOM with a shared-effort approach (in the EU up to 160 Mha are under forest management and imports come partially from low-intensity forest management). FALSE: another option is selected.
      lower_intensity = FALSE # TRUE or FALSE. TRUE: the script loads the areas modelled by GLOBIOM with a lower-intensity approach (same as baseline, but part of the imports and the exports are harvested from low-intensity management, though for the imports much less area is covered by low-intensity management than the shared-effort approach). FALSE: another option is selected.
                              # NB: the lower-intensity approach was tested but the results were not included in the published article.
      
      timber = FALSE          # TRUE or FALSE. FALSE = default setting which means that timber plantations are not included in the management practices considered part of EU clear cut areas have been allocated to Timber plantations). WARNING: This option is valid only if approach == "MG"
      CI = FALSE              # TRUE (confidence intervals are calculated) or FALSE (confidence intervals are not calculated)
  
    # Settings to define which .Rdata file to load for the response ratios and the z values 
      cutoff = TRUE           # TRUE (all raw RR > 1 are set to 1) or FALSE
      vulnerability = TRUE    # TRUE (global impacts are quantified) or FALSE. Default = TRUE. 
      BS = TRUE               # If CI = TRUE and BS = TRUE, confidence intervals are quantified with bootstrapping. If CI = TRUE and BS = FALSE, confidence intervals are quantified with MonteCarlo simulation. 
                              # if CI = FALSE, no confidence intervals are quantified so it does not matter which value is assigned to BS.
    
    id = "EUFootprint" # options: "EUForest", "EUFootprint" or "Global"
    map = "PDF" # "PDF" 
    graph = "B-50" # "B-50" or "B-25-50", to be selected when id == "EUFootprint". "B-50" = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature100% and Set-Aside100%
                          # "B-25-50" = = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature50%, Closer-to-nature100%, SetAside50% and Set-Aside100%
    ratio = TRUE
    difference = FALSE
    
    subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
      source("./scripts/set_directories.R")
    plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
    # This other function is meant to be tailored according to what it is needed to be plotted.
    
    ratio = FALSE
    difference = TRUE
    
    subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
      source("./scripts/set_directories.R")
    plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
    # This other function is meant to be tailored according to what it is needed to be plotted.
    
    id = "EUForest" # options: "EUForest", "EUFootprint" or "Global"
    map = "PDF" # "PDF" 
    graph = "B-25-50" # "B-50" or "B-25-50", to be selected when id == "EUFootprint". "B-50" = the map will plot the following scenarios for 2100: Baseline, Closer-to-nature100% and Set-Aside100%
               
    ratio = TRUE
    difference = FALSE
    
    subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
      source("./scripts/set_directories.R")
    plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
    # This other function is meant to be tailored according to what it is needed to be plotted.
    
    ratio = FALSE
    difference = TRUE
    
    subcase = "" # available options: "" = all species groups aggregated, "mammals" = results for mammals (aggregated), "birds" = results for birds (aggregated), "plants" = results for plants (aggregated)
      source("./scripts/set_directories.R")
    plot.map(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference)
    # This other function is meant to be tailored according to what it is needed to be plotted.
    