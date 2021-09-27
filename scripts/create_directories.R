
    if (dir.exists(aggr_plot_path) == TRUE) {file.rename(aggr_plot_path, paste0(aggr_plot_path, "_", format(as.Date(Sys.Date()), "%d.%m.%Y") ))} # check if the directory already exists. If it does, rename the older folder appending today's date to its name
      dir.create(aggr_plot_path) # create the directory
      
      dir.create(plots_path) # create the directories for plots and for csv files
      dir.create(csv_path)
      
      if (dir.exists(aggr_plot_path_areas) == TRUE) {file.rename(aggr_plot_path_areas, paste0(aggr_plot_path_areas, "_", format(as.Date(Sys.Date()), "%d.%m.%Y") ))} # check if the directory already exists. If it does, rename the older folder appending today's date to its name
      dir.create(aggr_plot_path_areas) # create the directory

