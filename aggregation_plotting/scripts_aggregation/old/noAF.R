
## to edit: working directory (setwd()), the path to load the data (load()), destination of final .csv (last row, write.csv())

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

load("./plotting/sums_mg-det.RData")   # results from the script: slost_aggregate.R
                                # data loaded: results - list of dataframes containing the sums over ecoregions per each year (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                                #              sums - list of dataframes dataframe containing the sums over ecoregions per each year (aggregated over the land uses), columns: Group, Forest_use, Management, Sum
                                #              tstep - vector with the time steps
                                #              results_median - list of dataframes containing the sums over ecoregions per each year considering only the median values (disaggregated per land use), columns in each df: Group, Forest_use, Management, all the land uses
                                #              sums_median - list of dataframes dataframe containing the sums over ecoregions per each year considering only the median values (aggregated over the land uses), columns: Group, Forest_use, Management, Sum  
                                

library(dplyr)    # dataframe management
library(tidyr)      # dataframe management
library(abind)      # dataframe management
library(ggplot2)

sums = sums_median

# Create a df containing all the sums for all the years
sums_tot = data.frame(sums[[1]][1:3])      # starting point: df with Group, Forest_use, Management  

  for(i in 1:length(tstep)) {
    names(sums[[i]])[names(sums[[i]]) == "Sum"] <- toString(tstep[i])   # rename the columns containing the sums in each element of the list with the corresponding year, such that they can be differenciated once joined 
    sums_tot = sums_tot %>% bind_cols(sums[[i]][4])                     # bind columns together
  }

  # Focus on the noAF scenario to plot it separetely
  noAF <- sums_tot %>% filter((Group == "REF" | Group == "RCP2.6") & (Forest_use == "MFM") & (Management == "noAFM")) %>%
            dplyr::select(-Forest_use, -Management) %>%
              pivot_longer(cols = 2:(length(sums_tot)-2), names_to = "Year", values_to = "PDFx100") %>%    # put all years and all values in the same columns (it considers length(sums_tot)-2 because two columns have been removed)
                arrange(Year) %>%
                  filter(Year != "2000" & Year != "2010")
  
  write.csv(noAF, paste0("./plotting/data/noAF_global_mg-det.csv"), row.names = FALSE)
  