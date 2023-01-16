# 04/09/2020 
# Francesca Rosa
# Maps of biodiversity impacts per ecoregion (ALTERFOR project)

library(rgdal)
library(raster)
library(dplyr)                            # dataframe management
library(tidyr)                            # dataframe management
library(abind)                            # dataframe management
library(tidyverse)                        # dataframe management
library(ggplot2)                          # plot
library(stringr)                          # string management


# functions

df_to_shp = function(sf, df, key) {
        
        # merge on common variable, here called 'key'
        m <- merge(sf, df, by = key)
        
        return(m)
        
}




setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 


# The map is obtained by merging the dataframe which contains the PDF per ecoregion and the original ecoregion shapefile

#shp <- readOGR("./ecoregions_data/WWF_Ecoregions/wwf_terr_ecos.shp")
shp <- readOGR(dsn = "./ecoregions_data/WWF_Ecoregions",
                          layer = "wwf_terr_ecos")
year = 2100
subset = "RCP2.6_SFM_AFMfree"

df <- read.csv(paste0("./plotting/no_cutoff/data/PDF-ha_mg-det.csv"))
 
# Filters the columns and the rows that will be put on maps: scenario RCP2.6_SFM_AF100

# -> try including and excluding other management

df <- df %>% mutate(Year = as.character(Year)) %>%
              filter(Scenario == subset & Year == "2100") %>% 
                mutate_if(is.numeric, ~.*100) %>%
                 mutate(PDF = rowSums(dplyr::select(., (starts_with("For_") & contains("median"))))) %>%
                  dplyr::select(Ecoregion, PDF) %>%
                    dplyr::rename(eco_code = Ecoregion) 
 
new_shp <- df_to_shp(shp, df, "eco_code")
pdf(file = paste0("./plotting/no_cutoff/PDF-ha_EUForest_test.pdf"), width = 14, height = 7)

spplot(new_shp, "PDF", col = "transparent")
dev.off()

writeOGR(new_shp, ".", "./ecoregions_data/WWF_Ecoregions/new_map", driver="ESRI Shapefile") 


geo.proj <- proj4string(new_shp)
#create SpatialPoints object for plots
pts <- SpatialPoints(cbind(lons,lats), proj4string = CRS(geo.proj))


#creates object that assigns each plot index to an ecoregion
plot.ecocodes <- over(pts, wwf.ecoregions)[[var.extract]]

plot.ecocodes

library(broom)
map <- tidy(new_shp, region = "PDF")

plot <- ggplot() + geom_polygon(data = new_shp@data, aes(x = coordinates(new_shp)[,1], y = coordinates(new_shp)[,2], fill = new_shp@data$PDF))

