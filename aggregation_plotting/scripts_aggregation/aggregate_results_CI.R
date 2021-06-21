# Francesca Rosa
# 04/06/2021
# Merging of the files with the results calculated from the real data and the files with the bootstrapped CI.


  aggregate.results.CI <- function(folder_slost, rdata_path, case_subcase) {
      # folder_slost = folder with the output of the model (character vector)
      # rdata_path = folder where the output of the function will be saved (character vector)
      # case_subcase = key identifying to which subgroup the results belong (character vector), e.g. whether the cutoff was applied or a specific taxa is considered 
      # output: .Rdata file with the results
    
    temp = list.files(path = paste0(folder_slost, "nobs/") , pattern = paste0("*", case_subcase, ".csv"), full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
    myfiles = lapply(temp, read.csv)            # read the files, create a list where in each element is loaded one of the file as df
    rm(temp)
    
    temp = list.files(path = paste0(folder_slost, "bs/") , pattern = paste0("*", case_subcase, ".csv"), full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
    fnames_new <- sub(".csv", "_CI.csv", temp, fixed = TRUE)
    file.rename(temp, fnames_new)
    temp <- fnames_new
    myfiles_CI = lapply(temp, read.csv)            # read the files, create a list where in each element is loaded one of the file as df
    rm(temp)
    
    tstep = seq(from = 2000, to = 2100, by = 10)    # time steps
    
    names(myfiles) = sapply(tstep, FUN = toString)  # rename each element of the list with the corresponding time step
    names(myfiles_CI) = sapply(tstep, FUN = toString)  # rename each element of the list with the corresponding time step

    for_management = c("noAFM", "AFMfree", "AFM25", "AFM50", "AFM75", "AFM100")                                                                                     # new names for management scenarios
    # forests = c("ClearCut_EU_median","ClearCut_im_median", "ClearCut_ex_median", "Retention_EU_median", "PlantationFuel_im_median", "SelectionSystem_EU_median")    # columns to be renamed to be handled easier
        
      # Join medians and CI
        results_median <- lapply(myfiles, function(x) select(x, Scenario, Ecoregion, Year, contains("median"))) 
        results_CI <- lapply(myfiles_CI, function(x) select(x, contains("lower95") | contains("upper95")))
        results_in <- mapply(c, results_median, results_CI, SIMPLIFY = FALSE)
        results_in <- lapply(results_in, function(x) data.frame(x))
        
        for(i in length(results_in)) {
          results_fin <- results_in[[i]]
          results_fin[,32:(32+27)] <- 2*results_fin[, 4:31] - results_fin[,(32+28):87]
          annual_crops_fin <- 2*results_in[[i]]$Annual_RoW_median - results_in[[i]]$Annual_RoW_upper95
          annual_crops_test <- results_fin$Annual_RoW_lower95
              if(all.equal(annual_crops_fin, annual_crops_test) != TRUE){stop("Error in the calculation of the CI (lower95)")}
          results_fin[,(32+28):87] <- 2*results_fin[, 4:31] - results_fin[,32:(32+27)]
          annual_crops_fin <- 2*results_in[[i]]$Annual_RoW_median - results_in[[i]]$Annual_RoW_upper95
          annual_crops_test <- results_fin$Annual_RoW_lower95
              if(all.equal(annual_crops_fin, annual_crops_test) != TRUE){stop("Error in the calculation of the CI (upper95)")}
        }
       
        
      # Sum species lost over the ecoregions, such that there is a single value per Scenario and land use. The result is a list of dataframes, one for each year.
    
        # rename columns and factors 
        ## !! the order of the operations in the next block must remain the same                                        
        
        results_in <- lapply(results_in, function(x) separate(x, Scenario, into = c("Group", "Forest_use", "Management"), sep = "_") %>%                                       # separate the column Scenario into three columns 
                                                       mutate(Management = str_replace(Management,"noAF", "noAFM"), Management = str_replace(Management,"AF0", "AFMfree"),  # rename the factors in the column with management information
                                                              Management = str_replace(Management,"AF25", "AFM25"), Management = str_replace(Management,"AF50", "AFM50"),     
                                                              Management = str_replace(Management,"AF75", "AFM75"), Management = str_replace(Management,"AF100", "AFM100"),
                                                              Group = str_replace(Group, "RCP", "RCP2.6")) %>%                                                              # rename the rcp scenario 
                                                          unite("Scenario", Group:Management, sep = "_"))                                                                   # re-merge the columns describing the scenario to keep it as it was initially
              
        # compute the sum over the ecoregions 
        results = lapply(results_in, function(x)  select(x, -Ecoregion, -Year) %>%                               
                                                  group_by(Scenario) %>%                                      
                                                    summarise_if(is.numeric, sum, na.rm = TRUE) %>%                                                          # sum
                                                      mutate_if(is.numeric, ~(.*100)) %>%                                                                    # convert to percentage
                                                        separate(Scenario, into = c("Group", "Forest_use", "Management"), sep = "_") %>%                     # separate the column Scenario
                                                          mutate(Group = factor(Group), Forest_use = factor(Forest_use), Management = factor(Management)) %>% # keep the factor feature
                                                            arrange(match(Management, for_management), Group, Forest_use)  )                                 # arrange the data according column Management, as displaied in for_management  
        results = lapply(results, function(x) data.frame(x))
        
        # sum over land uses 
        sums <- lapply(results, function(x) mutate(x, Sum_median = rowSums(select(x, contains("median")), na.rm = TRUE)) %>%            # sum the columns, except the ones describing the scenario                       
                                              mutate(Sum_lower95 =  rowSums(select(x, contains("lower95")), na.rm = TRUE)) %>%
                                                mutate(Sum_upper95 = rowSums(select(x, contains("upper95")), na.rm = TRUE))  %>%
                                                  select(Group, Forest_use, Management, Sum_median, Sum_lower95, Sum_upper95))       # select only the useful columns
        sums = lapply(sums, function(x) data.frame(x))
  
        # # # test ====
        # if (abs(rowSums(results[[1]][1, 4:28]) - sums[[1]][1,4]) > 1e-16) {stop("ERROR in the calculation of the sums")}
        # if (length(results[[1]]) > 28) {
        #   if((abs(rowSums(results[[1]][1, 29:53]) - sums[[1]][1,5]) > 1e-16) |
        #         (abs(rowSums(results[[1]][1, 54:78]) - sums[[1]][1,6] > 1e-16))) {stop("ERROR in the calculation of the sums")}
        #   }
  
        # test ====
        y <- sample(1:11, 1)                        # random year
        r <- sample(1:nrow(results[[1]]), 1)        # random row
        if ((rowSums(results[[y]][r, 4:31], na.rm = TRUE) - sums[[y]][r,4]) > 1e-16) {stop("ERROR in the calculation of the sums (median)")}
          if(length(results[[y]]) > 31) { 
            if ((rowSums(results[[y]][r, 32:59], na.rm = TRUE) - sums[[y]][r,5] > 1e-16) |
                (rowSums(results[[y]][r, 60:87], na.rm = TRUE) - sums[[y]][r,6] > 1e-16))  {stop("ERROR in the calculation of the sums (CI)")}
          }
  
        # ====
          
        results_median <- lapply(results, function(x)  select(x, -contains("upper95"), -contains("lower95")))             # focus on median values
        sums_median <- lapply(results_median, function(x) mutate(x, Sum = rowSums(x[4:length(x)], na.rm = TRUE)) %>%                    # sum the columns, except the ones describing the scenario
                                       select(Group, Forest_use, Management, Sum))                                         # select only the interesting columns
    
        results_CI <- lapply(results, function(x)  select(x, -contains("median")))                                        # focus on CI values
        sums_CI <- lapply(results_CI, function(x) mutate(x, Sum_upper = rowSums(select(x, contains("upper95")), na.rm = TRUE)) %>%      # sum the columns for the upper CI, except the ones describing the scenario
                                                      mutate(Sum_lower = rowSums(select(x, contains("lower95")), na.rm = TRUE)) %>%
                                                         select(Group, Forest_use, Management, Sum_upper, Sum_lower))     # select only the interesting columns
    
        rm(myfiles, results_in, for_management)
        
        save(results, results_CI, results_median, sums, sums_CI, sums_median, tstep, file = paste0(rdata_path)) # this data can then be read by the scripts used to aggregate the land uses
     
  }
      