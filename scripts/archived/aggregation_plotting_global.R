# Aggregation and plotting of global results not used in the manuscript

 ################# PREPARE DATA FOR PLOTTING (aggregated .Rdata file -> .csv files for global impacts #######################
          
          create.csv.global.areas(file_rdata_path_areas, aggr_plot_path_areas, label_timber, year)   # areas_Rdata-to-csv.R
    
######## GLOBAL IMPACTS - DISAGGREGATED LAND USE CATEGORIES (PDF) ##########
    
          source("./scripts/aggregation/global_Rdata-to-csv.R")        # functions to convert the .Rdata file to several .csv for impacts of global land us
          create.csv.global(CI, file_rdata_path, csv_path, file_label, year) # global_Rdata-to-csv.R
  
######## GLOBAL IMPACTS - AGGREGATED LAND USE CATEGORIES AND CI (PDF) ##########
          
          create.csv.noAF(file_rdata_path, csv_path, file_label) # global_Rdata-to-csv.R
    
 ################# GLOBAL TIME SERIES AND AREAS (DISAGGREGATED) #######################
          
          source("./scripts/plotting/plot_global_time-series.R")            
          plot.global.time.series(csv_path, file_label, plots_path, label_timber, aggr_plot_path_areas)  # plot_global_time-series.R

################# GLOBAL TIME SERIES (AGGREGATED, WITH CI) #######################
          
          plot.global.time.series.CI(csv_path, file_label, plots_path) # plot_global_time-series.R --> noAF
      