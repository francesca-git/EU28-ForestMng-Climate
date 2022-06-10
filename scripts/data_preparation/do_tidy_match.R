
###### PREPARATION OF LAND USE DATA TO BE USED AS INPUT IN THE MODEL FOR THE QUANTIFICATION OF THE IMPACTS #####

# Task: Take as input the .csv files with the original areas, tidy them up and match the two models used for 
        # the projection of future land use categories.
        # To do this, this script uses the functions tidy_areas.Rdata and then match_areas.Rdata
# Author: Francesca Rosa
# Date: started in March 2020
# unit of the output data: Mha

# working directory set with the script set-wd.R available in the main folder

tidy.match.areas <- function(timber, marginal, not_rel_wet, areas_base_path, areas_processed_path) {
        
        source("./scripts/data_preparation/tidy_areas.R")
        source("./scripts/data_preparation/match_areas.R")
        source("./scripts/data_preparation/areas_functions.R")
        
        # ARGUMENTS OF THE FUNCTIONS 
        # timber can be FALSE or TRUE (default = FALSE)
                # FALSE = excluding timber plantations from EU forest managements
                # TRUE = including timber plantations in EU forest managements for the sensitivity 
        # marginal
        # not_rel_wet can be FALSE or TRUE (default = FALSE)
                # FALSE = excluding not relevant lands and wetlands 
                # TRUE = including not relevant lands and wetlands 
                # The default setting is false because wetlands and not relevant lands have
                # no matching land use classes in the biodiversity model
                # The option TRUE is used to produce the .csv files (available in data/land_use_data/areas_processed/NotRel-Wet) 
                # needed to plot the maps with the future land use according to the GLOBIOM model.
        # areas_base_path: directory path where the original areas .csv files are stored (e.g., "User/Document/Project/data/land_use_data/data_raw/")
        # areas_processed_path: directory path where the .csv files output of tidy.areas have been stored after they have been cleaned (e.g., "User/Document/Project/data/land_use_data/processed_data/")

        tidy.areas(timber, areas_base_path)                          # from the file tidy_areas.R
        # Task: Clean the input areas and put them in a suitable structure. 
        # The output of this script is a .Rdata file meant to be used as input in match.areas().  
        # The results of this function are saved as .Rdata in the folder: /results/areas
        
        print("Areas have been tidied up")
        

        match.areas(timber, marginal, not_rel_wet, areas_processed_path)
        # Task: Take as input the areas tided up in tidy.areas and match the different datasets 
        # The results of this function are saved as .csv data in the folders: 
        # /data/land_use_data/areas_processed/notimber or /data/land_use_data/areas_processed/timber
        
        print("Areas have been matched")

        
   }