
setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation")
#options(warn=0)

#install.packages("rgdal")
#install.packages("tidyverse")
#install.packages("compare")
library(rgdal)
library(dplyr)
library(tidyr)
library(compare)
library(readr)
library(purrr)
library(tibble)
library(stringr)

####### Wood and energy crops/plantation production from Fulvio

EUForest <- read.csv("./grouped_land_use_files/EUWood_forest.csv", header = TRUE, check.names = FALSE) # total wood harvested from EU 28 forests (roundwood and energywood >including  fuelwood and logging residues)
EUEnergy <- read.csv("./grouped_land_use_files/EUWood_energycrops.csv", header = TRUE, check.names = FALSE) # energy-wood form energy crops cultivated in the EU
importForest <- read.csv("./grouped_land_use_files/ImportWood_forest.csv", header = TRUE, check.names = FALSE) # total wood imported from forests outside the EU28 (in form of roundwood, semifinished products and wood pellets for energy from industrial byproducts)
importEnergy <- read.csv("./grouped_land_use_files/ImportWood_energyplantations.csv", header = TRUE, check.names = FALSE) # energywood imported exclusively from energy plantations cultivated outside the EU (IP_pellets)
exportForest <- read.csv("./grouped_land_use_files/ExportWood_forest.csv", header = TRUE, check.names = FALSE) # wood harvested in the EU28 and exported to other regions (in form of roundwood or semi-finished products) [wood already included in the “EU28_wood_forest”]
importByproducts <- read.csv("./grouped_land_use_files/ImportWood_byproducts.csv", header = TRUE, check.names = FALSE) # energy wood imported from outside EU in form of pellets obtained as byproducts of the forest industry 

# list containing all the loaded dataframes 

data.list = list("EUForest" = EUForest, "EUEnergy" = EUEnergy, "importForest" = importForest, "importEnergy" = importEnergy, "exportForest" = exportForest, "importByproducts" = importByproducts)

tstep = seq(from = 2000, to = 2100, by = 10)  
ntstep = length(tstep) #time steps: 2000 -> 2100

# for each element of the list (each dataframe), creates a single column (Year) containing all the time values and one column with the Mm3 
  
list.temp <- lapply(data.list, function(x) pivot_longer(x, cols = 5:(length(x)-1), names_to = "Year", values_to = toString(x[1, length(x)])) %>%
                                              select(-Category))
# puts together all the dataframes

df <- reduce(list.temp, full_join)

rm(data.list, list.temp)

df[is.na(df)] <- 0 

# tidies the dataframe (renames some cells, unifies some columns, arranges the df) such that it can be joined to the df with the species loss

df1 <- df %>% mutate(Wood_forest_EU = Wood_forest_EU - Wood_forest_ex) %>% # exports are included in the general EU wood production, this difference keeps them separate 
              rename(Group = Mitigation_scenario, Management = Management_scenario) %>%
                mutate(Management = str_replace(Management,"noAF", "noAFM"), Management = str_replace(Management,"AF0", "AFMfree"),  # rename the factors in the column with management information
                       Management = str_replace(Management,"AF25", "AFM25"), Management = str_replace(Management,"AF50", "AFM50"),     
                       Management = str_replace(Management,"AF75", "AFM75"), Management = str_replace(Management,"AF100", "AFM100"),
                       Group = str_replace(Group, "RCP", "RCP2.6"), # rename the rcp scenario 
                       Region = str_replace(Region, "Pacific_Islands", "Pacific_Island")) %>%                                                              
                  unite("Scenario", Group:Management, sep = "_") %>%
                     arrange(Region, Scenario, Year)
levels(df1$Region) <- sort(levels(df1$Region))

# loads the PDF dataframe and tidies it

df2 <- read.csv("./plotting/no_cutoff/slost-globiom_mg-det.csv", header = TRUE, check.names = FALSE) # total wood harvested from EU 28 forests (roundwood and energywood >including  fuelwood and logging residues)

df2 <- df2 %>% rename(Region = Globiom_Reg) %>%
                filter(Region != "NN") %>%
                  arrange(Region, Scenario, Year) %>%
                    mutate(Year = as.character(Year))

df2 = droplevels( df2, "NN")  


# puts together the production and the impacts 
# focus on EU

df_fin <- df2 %>% full_join(df1) %>%
                    select(Scenario, Region, Year, contains("Wood"), contains("median")& ((starts_with("For_")&(contains("EU")|contains("ex")|contains("im")))|  # selects the columns containing the production of Wood and energy crops/plantations and the columns containing EU internal/imported/exported forest use or energy crops/plantations
                                                                                          (starts_with("EP")&((contains("EU")&contains("conv"))|contains("im"))))) %>% 
                      mutate(PDF_forest_EU = rowSums(select(., (starts_with("For_") & contains("EU")))), 
                             PDF_forest_im = rowSums(select(., (starts_with("For_") & contains("im")))),
                             PDF_EP_EU = rowSums(select(., (contains("EP") & contains("EU") & contains("conv")))), 
                             PDF_EP_im = rowSums(select(., (contains("EP") & contains("im") & contains("conv")))),
                             PDF_forest_ex = rowSums(select(., (starts_with("For_") & contains("ex"))))) %>%
                      select(-(starts_with("For_") & contains("EU")), - (starts_with("For_") & contains("im")), 
                             -(contains("EP") & contains("EU") & contains("conv")), -(contains("EP") & contains("im") & contains("conv")), -(starts_with("For_") & contains("ex")))


# test ====

test = df_fin[nrow(df_fin),]
test_check = df2[nrow(df_fin),]

if ((test$PDF_forest_EU != test_check$EP_conv_EU_median) |
    (test$PDF_EP_EU != test_check$For_ClearCut_EU_median + test_check$For_Retention_EU_median + test_check$For_SelectionSystem_EU_median) |
    (test$PDF_EP_im != test_check$EP_conv_im_median) |
    (test$PDF_forest_im != test_check$For_ClearCut_im_median + test_check$For_PlantationFuel_im_median) |
    (test$PDF_forest_ex != test_check$For_ClearCut_ex_media)) {stop("ERROR in the aggregation of land use for EU footprint")}

rm(test, test_check)
# ====

# computes PDF/Mm3 per globiom region

df_ratio <- df_fin %>% transmute(Scenario = Scenario, Region = Region, Year = Year, 
                                 forest_EU_noex = PDF_forest_EU/Wood_forest_EU,
                                 forest_im = PDF_forest_im/Wood_forest_im,
                                 EP_EU = PDF_EP_EU/Wood_EP_EU,
                                 EP_im = PDF_EP_im/Wood_EP_im,
                                 forest_ex = PDF_forest_ex/Wood_forest_ex,
                                 forest_EU_ex = (PDF_forest_EU + PDF_forest_ex)/(Wood_forest_ex+Wood_forest_EU))
                                 

df_ratio[df_ratio == "NaN"] <- NA
df_ratio[sapply(df_ratio, is.infinite)] <- NA

df_ratio_clean <- df_ratio %>%
                    replace(is.na(.), 0) 

write.csv(df_ratio, "./plotting/no_cutoff/data/PDF_Mm3_globiomregions.csv", row.names = FALSE)

# computes PDF/Mm3 summing up the values over the globiom regions

df_ratio_sum <- df_fin %>%
                  group_by(Scenario, Year) %>%
                    summarise_if(is.numeric, sum, na.rm = TRUE) 

df_ratio_sum <- data.frame(df_ratio_sum)
df_ratio_sum <- df_ratio_sum %>%
                      transmute(Scenario = Scenario, Year = Year, 
                                forest_EU_noex = PDF_forest_EU/Wood_forest_EU,
                                forest_im = PDF_forest_im/Wood_forest_im,
                                EP_EU = PDF_EP_EU/Wood_EP_EU,
                                EP_im = PDF_EP_im/Wood_EP_im,
                                forest_ex = PDF_forest_ex/Wood_forest_ex,
                                forest_EU_ex = (PDF_forest_EU + PDF_forest_ex)/(Wood_forest_ex+Wood_forest_EU))


write.csv(df_ratio_sum, "./plotting/no_cutoff/data/PDF_Mm3.csv", row.names = FALSE)

  







