
## to edit: working directory (setwd()), the path to load the data (list.files()), destination of the RData file (last row, save.image())

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 
source("./areas_allocation.R")

library(dplyr)    # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
#library(ggplot2)                         # plot
library(stringr)                          # string management

temp = list.files(path = "./species-lost_nocutoff/mg/", pattern="*.csv", full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
myfiles_median = lapply(temp, read.csv)            # read the files, create a list where in each element is loaded one of the file as df
rm(temp)
temp = list.files(path = "./species-lost_CI/mg/", pattern="*.csv", full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
myfiles_CI = lapply(temp, read.csv)
rm(temp)

tstep = seq(from = 2000, to = 2100, by = 10)    # time steps

names(myfiles_median) = sapply(tstep, FUN = toString)  # rename each element of the list with the corresponding time step
names(myfiles_CI) = sapply(tstep, FUN = toString)  # rename each element of the list with the corresponding time step


for_management = c("noAFM", "AFMfree", "AFM25", "AFM50", "AFM75", "AFM100")                                                                                     # new names for management scenarios
# forests = c("ClearCut_EU_median","ClearCut_im_median", "ClearCut_ex_median", "Retention_EU_median", "PlantationFuel_im_median", "SelectionSystem_EU_median")    # columns to be renamed to be handled easier

# Join medians and CI
results_median <- lapply(myfiles_median, function(x) select(x, Scenario, Ecoregion, Year, contains("median"))) 
results_CI <- lapply(myfiles_CI, function(x) select(x, contains("lower95") | contains("upper95")))
results_in <- mapply(c, results_median, results_CI, SIMPLIFY = FALSE)
results_in <- lapply(results_in, function(x) data.frame(x))

# Sum species lost over the ecoregions to have the results for GLOBIOM regions. 

# rename columns and factors 
## !! the order of the operations in the next block must remain the same                                        

results_in <- lapply(results_in, function(x) separate(x, Scenario, into = c("Group", "Forest_use", "Management"), sep = "_") %>%                                       # separate the column Scenario into three columns 
                       mutate(Management = str_replace(Management,"noAF", "noAFM"), Management = str_replace(Management,"AF0", "AFMfree"),  # rename the factors in the column with management information
                              Management = str_replace(Management,"AF25", "AFM25"), Management = str_replace(Management,"AF50", "AFM50"),     
                              Management = str_replace(Management,"AF75", "AFM75"), Management = str_replace(Management,"AF100", "AFM100"),
                              Group = str_replace(Group, "RCP", "RCP2.6")) %>%                                                              # rename the rcp scenario 
                       unite("Scenario", Group:Management, sep = "_"))

df <- reduce(results_in, full_join) %>%
        mutate(Year = as.character(Year))

# classify according to GLIOBIOM region

Globiom_eco_org <- read.csv("./grouped_land_use_files/GLOBIOM_Ecoregion.csv", header = TRUE)
# rearrange data in Globiom_eco
  Globiom_eco_org <- filter(Globiom_eco_org, Ecoregion!="Lake", Ecoregion != "Rock and Ice") 
  Globiom_eco_org = droplevels(Globiom_eco_org, "Lake", "Rock and Ice")  #erase these two names from the list of factors
  
df_regions <- join_regions(df, Globiom_eco_org)
df_EU <- dF_regions %>% select(Scenario, Ecoregion, Year, ClearCut_EU_median, ClearCut_EU_ex)

df_regions <- df_regions %>% 
                group_by(Scenario, Globiom_Reg, Year) %>%       # groups by scenario, globiom region and year -> no ecoregion level anymore
                  summarise_if(is.numeric, sum, na.rm =TRUE) %>%
                    arrange(Scenario, Globiom_Reg) %>%
                      select(-Share) %>%
                        mutate_if(is.numeric, ~(.*100))         # convert to percentage
  

df_regions <- data.frame(df_regions)

write.csv(df_regions, paste0("./plotting/no_cutoff/slost-globiom_mg-det.csv"), row.names = FALSE)








