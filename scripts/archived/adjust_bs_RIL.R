
temp = list.files(path = paste0(results_path) , pattern = paste0("*", file_label, ".csv"), full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
    myfiles = lapply(temp, read.csv)            # read the files, create a list where in each element is loaded one of the file as df
    rm(temp)
    

    tstep = seq(from = 2000, to = 2100, by = 10)    # time steps
    
    names(myfiles) = sapply(tstep, FUN = toString)  # rename each element of the list with the corresponding time step
    
    for_management = c("noAFM", "AFMfree", "AFM25", "AFM50", "AFM75", "AFM100")                                                                                     # new names for management scenarios
    # forests = c("ClearCut_EU_median","ClearCut_im_median", "ClearCut_ex_median", "Retention_EU_median", "PlantationFuel_im_median", "SelectionSystem_EU_median")    # columns to be renamed to be handled easier
        
      # Prepare the data to start aggregating or tidying up
        results_in <- lapply(myfiles, function(x) data.frame(x)) 
    
      # Sum species lost over the ecoregions, such that there is a single value per Scenario and land use. The result is a list of dataframes, one for each year.
    
        # rename columns and factors 
        ## !! the order of the operations in the next block must remain the same                                        
        
              results_in <- lapply(results_in, function(x) add_column(x, "For_ReducedImpactLogging_im_median" = 0, .after = "For_Selective_im_median") %>%
                               add_column("For_ReducedImpactLogging_im_lower95" = 0, .after = "For_Selective_im_lower95") %>%
                               add_column("For_ReducedImpactLogging_im_upper95" = 0, .after = "For_Selective_im_upper95") %>%
                               mutate(For_ReducedImpactLogging_im_median = For_Selective_im_median) %>%
                              mutate(For_ReducedImpactLogging_im_lower95 = For_Selective_im_lower95) %>%
                               mutate(For_ReducedImpactLogging_im_upper95 = For_Selective_im_upper95) )
        
        for(i in 1:length(tstep)) {
          write.csv(results_in[[i]], paste0(results_path, "/Slost_", tstep[i], file_label, ".csv"), row.names = FALSE)
        }
 
                             
                             
# For_ReducedImpactLogging_im_median
# For_ReducedImpactLogging_im_lower95
# For_ReducedImpactLogging_im_upper95
        For_TimberPlant_ex_median
        
              results_in <- lapply(results_in, function(x) add_column(x, "For_ReducedImpactLogging_im_median" = 0, .after = "For_Selective_im_median") %>%
                               add_column("For_ReducedImpactLogging_im_lower95" = 0, .after = "For_Selective_im_lower95") %>%
                               add_column("For_ReducedImpactLogging_im_upper95" = 0, .after = "For_Selective_im_upper95") %>%
                               mutate(For_ReducedImpactLogging_im_median = For_Selective_im_median) %>%
                              mutate(For_ReducedImpactLogging_im_lower95 = For_Selective_im_lower95) %>%
                               mutate(For_ReducedImpactLogging_im_upper95 = For_Selective_im_upper95) %>%
                              mutate(For_Selective_im_median = For_TimberPlant_ex_median) %>%
                              mutate(For_Selective_im_lower95 = For_TimberPlant_ex_lower95) %>%
                               mutate(For_Selective_im_upper95 = For_TimberPlant_ex_upper95)) 
              
              