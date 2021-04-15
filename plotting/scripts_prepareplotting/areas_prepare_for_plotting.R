
## to edit: working directory (setwd()), the path to load the data (list.files()), destination of the RData file (last row, save.image())

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

library(dplyr, warn.conflicts = FALSE)    # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
#library(ggplot2)                          # plot
library(stringr)                          # string management
select <- dplyr::select

temp = list.files(path = "./results/areas/MG/disaggregated/", pattern="*.csv", full.names = TRUE)   # save as list the paths of all .csv files in the selected folder

myfiles = lapply(temp, read.csv)            # read the files, create a list where in each element is loaded one of the file as df

tstep = seq(from = 2000, to = 2100, by = 10)    # time steps

names(myfiles) = sapply(tstep, FUN = toString)  # rename each element of the list with the corresponding time step

for_management = c("noAFM", "AFMfree", "AFM25", "AFM50", "AFM75", "AFM100")                                                                                     # new names for management scenarios
#forests = c("ClearCut_EU","ClearCut_im", "ClearCut_ex", "Retention_EU", "Plantation_im", "Selection_EU")    # columns to be renamed to be handled easier


# Sum species lost over the ecoregions, such that there is a single value per Scenario and land use. The result is a list of dataframes, one for each year.

# rename columns and factors 
## !! the order of the operations in the next block must remain the same

results_in <- myfiles 
# results_in = lapply(results_in, function(x) rename_at(x, vars(starts_with("For")), list(~ str_replace(., "For", "ForOther_"))) %>%                              # rename the columns corresponding to generic forest management such that all of them start with "ForOther"
#                                        rename_at(vars(all_of(forests)), ~ paste("For", forests, sep = "_")))                                                   # rename the columns corresponding to forest management intensities such that all of them start with "For" (this makes them easier to select later)

results_in <- lapply(results_in, function(x) separate(x, Scenario, into = c("Group", "Forest_use", "Management"), sep = "_") %>%                                       # separate the column Scenario into three columns 
                       mutate(Management = str_replace(Management,"noAF", "noAFM"), Management = str_replace(Management,"AF0", "AFMfree"),  # rename the factors in the column with management information
                              Management = str_replace(Management,"AF25", "AFM25"), Management = str_replace(Management,"AF50", "AFM50"),     
                              Management = str_replace(Management,"AF75", "AFM75"), Management = str_replace(Management,"AF100", "AFM100"),
                              Group = str_replace(Group, "RCP", "RCP2.6")) %>%                                                              # rename the rcp scenario 
                       unite("Scenario", Group:Management, sep = "_"))                                                                   # re-merge the columns describing the scenario to keep it as it was initially


# compute the sum over the ecoregions 
results = lapply(results_in, function(x)  select(x, -Ecoregion) %>%                               
                   group_by(Scenario) %>%                                      
                    summarise_if(is.numeric, sum, na.rm = TRUE) %>%                                                          # sum
                      separate(Scenario, into = c("Group", "Forest_use", "Management"), sep = "_") %>%                     # separate the column Scenario
                        mutate(Group = factor(Group), Forest_use = factor(Forest_use), Management = factor(Management)) %>% # keep the factor feature
                          arrange(match(Management, for_management), Group, Forest_use)  )                                 # arrange the data according column Management, as displaied in for_management  


rm(myfiles, temp, results_in, for_management)

save.image("./plotting/areas.RData") # this data can then be read by the scripts used to aggregate the land uses
