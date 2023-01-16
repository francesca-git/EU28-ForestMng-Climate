
setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 

library(pacman)
p_load(dplyr, tidyr, abind, tidyverse, stringr)  # dataframe management and string management

p_load(ggplot2, colorspace, scico, viridis, RColorBrewer, nord, sf, sp, ggalluvial) # plotting

select <- dplyr::select

df <- read.csv("./grouped_land_use_files/EU_country_2020_noAF.csv")
df <- df %>% rename(Wood_Mm3 = X2020) %>%
        mutate(Year = 2020)

gg <- ggplot(data = df %>% filter(Country != "EU28" & Category == "Wood_forest_im"), aes(y = Wood_Mm3, axis1 = Category, axis2 = Country)) +
        geom_alluvium(aes(fill = Category)) + 
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) 

gg