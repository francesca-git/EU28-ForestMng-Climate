

    if (cutoff == TRUE) { label_cutoff = "cutoff"
      } else if (cutoff == FALSE) {label_cutoff = "nocutoff"} 

    if (timber == FALSE) {label_timber = "notimber"
      } else if (timber == TRUE) {label_timber = "timber"}
    
    if (CI == FALSE) {label_CI = "static"
      } else if (CI == TRUE) {if (BS == FALSE) {label_CI = "mc"
      } else if (BS == TRUE) {label_CI = "bs"}}
    
        
    if (marginal == TRUE) { label_approach = "mg"
      } else if (marginal == FALSE) {label_approach = "av"}
    
    if (label_timber == "notimber") { 
      label = paste0(label_cutoff, "_", label_CI, "_", label_approach)  
    } else if (label_timber != "") { 
      label = paste0(label_cutoff, "_", label_timber, "_", label_CI, "_", label_approach) 
    } 

    if(vulnerability == FALSE) {
      label = paste0(label, "_noVS")
    }

############################# SETTING THE LOCATION OF THE DIRECTORIES NEEDED TO RUN THE MODEL  ############################# 

    ### MODEL PARAMETERS ###
    ecodata_path <- "./data/model_parameters/ecoregions_data/" # path of the directory where the data on the model parameters are stored

    ### LAND USE AREAS ###
    areas_path = "./data/land_use_data/"
    areas_base_path = paste0(areas_path, "areas_base/")
    if(lowintensity_imports == TRUE) {areas_base_path = paste0(areas_path, "areas_base/alternative/")}
    areas_processed_path =  paste0(areas_path, "areas_processed/", label_timber, "/", label_approach, "/")
    
    areas_processed_path_user =  paste0("./data/land-use/areas_user/")

    ### RESULT DIRECTORY ###
    results_path <- paste0("./results/species-lost_", label) # set the name of the directory where the files will be saved 

############################ SETTING THE LOCATION OF THE DIRECTORIES NEEDED FOR AGGREGATION AND PLOTTING ##############################

    energy_exports <- "EPnoex"    # "ex" (Footprint includes energy plantations, Internal EU does not include energy crops but includes exports) 
                                  # "EPnoex" (Footprint includes energy plantations and Internal EU includes energy crop and exports)  
                                  # "noEPnoex" (Footprint does not include energy plantations, Internal EU does not include nor energy crop nor exports)   
    
   
############################ RESULTS DIRECTLY FROM THE MODEL ##############################
    
  if(vulnerability == TRUE) {
    if(subcase == "") {
      file_label <- paste0("_", label)
    } else {file_label <- paste0("_", label, "_", subcase)}
  } else if(vulnerability == FALSE) {
     if(subcase == "") {
      file_label <- paste0("_", label)
    } else {file_label <- paste0("_", label, "_", subcase)}
  }

# access the results obtained directly from the model
    
   result_files = "Slost_"     # in the call of the file the year and the subcase are added
  
############################ ACCESS THE FOLDER WHERE RESULTS ARE AGGREGATED AND PLOTTED ##############################

  # calculation -> PLOTTING -> RESULTS -> SUBFOLDER, whose name marks may refer to different types of results (with cutoff, without cutoff, with timber, without timber, etc)
  # -> CSV or PLOTS or .Rdata files 

      aggr_plot_path <- paste0( "./aggregation_plotting/", label, "/") # set the name of the directory where the aggregated files will be saved 
      
      plots_path <- paste0(aggr_plot_path, "plots/")  # path used to save the plots (generated using the .csv data)
      csv_path <- paste0(aggr_plot_path,"csv/")      # path used to save csv (generated using the .Rdata)
  
      file_rdata_name <- paste0("sums_", label, ".Rdata")
      if(subcase != "") {file_rdata_name <- paste0("sums_", label, "_", subcase, ".Rdata")}

      file_rdata_path <- paste0(aggr_plot_path, file_rdata_name)

      aggr_plot_path_areas <- paste0( "./aggregation_plotting/areas/", label_timber, "/", label_approach, "/") # set the name of the directory where the aggregated files will be saved 
      
      file_rdata_path_areas <- paste0(aggr_plot_path_areas, "areas.Rdata")
        
      file_rdata_name_areas <- paste0("areas_", label_timber, ".Rdata")
      
    
      
      
      
      
    