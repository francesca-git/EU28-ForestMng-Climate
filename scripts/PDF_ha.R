
## to edit: working directory (setwd()), the path to load the data (list.files()), destination of the RData file (last row, save.image())

setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

library(dplyr)    # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
#library(ggplot2)                          # plot
library(stringr)                          # string management

temp = list.files(path = "./species-lost_nocutoff/mg/", pattern="*.csv", full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
myfiles_1 = lapply(temp, read.csv)            # read the files, create a list where in each element is loaded one of the file as df
rm(temp)
temp = list.files(path = "./areas/MG/disaggregated/", pattern="*.csv", full.names = TRUE)   # save as list the paths of all .csv files in the selected folder
myfiles_2 = lapply(temp, read.csv)
rm(temp)

tstep = seq(from = 2000, to = 2100, by = 10)    # time steps
for_management = c("noAFM", "AFMfree", "AFM25", "AFM50", "AFM75", "AFM100") # new names for management scenarios

names(myfiles_1) = sapply(tstep, FUN = toString)  # rename each element of the list with the corresponding time step
names(myfiles_2) = sapply(tstep, FUN = toString)  # rename each element of the list with the corresponding time step

# select only median values
PDF <- lapply(myfiles_1, function(x) select(x, Scenario, Ecoregion, Year, contains("median"))) 

# remove columns that do not have a corresponding value in PDF
Areas <- lapply(myfiles_2, function(x) select(x, -contains("new"), -contains("org"))) 


# rename columns and factors 

PDF <- lapply(PDF, function(x) separate(x, Scenario, into = c("Group", "Forest_use", "Management"), sep = "_") %>%                                       # separate the column Scenario into three columns 
                       mutate(Management = str_replace(Management,"noAF", "noAFM"), Management = str_replace(Management,"AF0", "AFMfree"),  # rename the factors in the column with management information
                              Management = str_replace(Management,"AF25", "AFM25"), Management = str_replace(Management,"AF50", "AFM50"),     
                              Management = str_replace(Management,"AF75", "AFM75"), Management = str_replace(Management,"AF100", "AFM100"),
                              Group = str_replace(Group, "RCP", "RCP2.6")) %>%                                                              # rename the rcp scenario 
                       unite("Scenario", Group:Management, sep = "_"))                                                                   # re-merge the columns describing the scenario to keep it as it was initially

Areas <- lapply(Areas, function(x) separate(x, Scenario, into = c("Group", "Forest_use", "Management"), sep = "_") %>%                                       # separate the column Scenario into three columns 
                  mutate(Management = str_replace(Management,"noAF", "noAFM"), Management = str_replace(Management,"AF0", "AFMfree"),  # rename the factors in the column with management information
                         Management = str_replace(Management,"AF25", "AFM25"), Management = str_replace(Management,"AF50", "AFM50"),     
                         Management = str_replace(Management,"AF75", "AFM75"), Management = str_replace(Management,"AF100", "AFM100"),
                         Group = str_replace(Group, "RCP", "RCP2.6")) %>%                                                              # rename the rcp scenario 
                  unite("Scenario", Group:Management, sep = "_"))                        # re-merge the columns describing the scenario to keep it as it was initially

# add a column containing the Year     

for (i in 1:length(Areas)) {Areas[[i]]$Year = as.character(tstep[i])}

# convert the elements of the list to a single dataframe                
df_PDF <- reduce(PDF, full_join)
df_Areas <- reduce(Areas, full_join)
df_Areas <- df_Areas %>% select(Scenario:Ecoregion, Year, (3:length(df_Areas) - 1))


df_PDF <- df_PDF %>% arrange(Scenario, Ecoregion)
df_Areas <- df_Areas %>% arrange(Scenario, Ecoregion) 


ratio <- df_PDF

ratio <- bind_cols(ratio[1:3],ratio[4:length(ratio)]/df_Areas[4:length(df_Areas)]) %>%
            replace(is.na(.), 0) 

        # test ====
        
                  test = ratio[8204,]
                  test_check1 = df_PDF[8204,]
                  test_check2 = df_Areas[8204,]
                  
                  if ((abs(test$For_ClearCut_EU_median -(test_check1$For_ClearCut_EU_median/test_check2$For_ClearCut_EU)) > 1e-16) |
                  (abs(test$For_ClearCut_ex_median -(test_check1$For_ClearCut_ex_median/test_check2$For_ClearCut_ex)) > 1e-16) |
                  (abs(test$For_ClearCut_im_median -(test_check1$For_ClearCut_im_median/test_check2$For_ClearCut_im)) > 1e-16) |
                  #(abs(test$For_Retention_EU_median -(test_check1$For_Retention_EU_median/test_check2$For_Retention_EU)) > 1e-16) |
                  #(abs(test$For_PlantationFuel_im_median-(test_check1$For_PlantationFuel_im_median/test_check2$For_PlantationFuel_im)) > 1e-16) |
                  (abs(test$For_SelectionSystem_EU_median-(test_check1$For_SelectionSystem_EU_median/test_check2$For_SelectionSystem_EU)) > 1e-16) |
                  (abs(test$EP_EU_median-(test_check1$EP_EU_median/test_check2$EP_EU)) > 1e-16) |
                  (abs(test$EP_conv_EU_median-(test_check1$EP_conv_EU_median/test_check2$EP_conv_EU)) > 1e-16)) {stop("ERROR in the calculation of PDF/ha")}
        
        # ====

write.csv(ratio, paste0("./plotting/no_cutoff/data/PDF-ha_mg-det.csv"), row.names = FALSE)


  
  

