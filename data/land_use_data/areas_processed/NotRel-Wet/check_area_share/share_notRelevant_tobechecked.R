setwd("C:/Users/Rosa/Documents/GitHub/forest-management") 

library(ggplot2)
library(sf)
library(sp)
library(dplyr)
library(tidyverse)
library(rgdal)
library(viridis)
library(RColorBrewer)
library(stringr)
library(colorspace)

title = "Share of wetlands in each ecoregion"
path <- "Wetlands_2000_REF"

tot <- read.csv("./excluded_lands/Aorg_mg_2000_REF.csv")

land <- read.csv(paste0("./excluded_lands/", path, ".csv")) 
land <- land %>% mutate(Area = EU + RoW) %>%
                      select(Ecoregion, Area)

check <- inner_join(tot, land)

check <- check %>% mutate(Share = Area/(A_org + Area))


write.csv(check, paste0("./excluded_lands/share_", path, ".csv"), row.names = F)


shp_original <- st_read(dsn = "./ecoregions_data/WWF_Ecoregions", layer = "wwf_terr_ecos")
shp <- shp_original 
shp <- shp %>% filter(Shape_Area > 0.1) %>%
                rename(Ecoregion = eco_code)

df <- left_join(shp, check)


pal <- c("#F1F1F1", "#E1EDC9", "#C0E7BB", "#98DEB6", "#6BD1B9", "#3CC2BE", "#1CAEC3", "#3797C3", "#5B7BBD", "#775BAF", "#833993", "#80146E")

legend = "%"

pdf(file = paste0("./excluded_lands/", path, "_shares.pdf"), width = 9, height = 7)

figure <- ggplot() +
            geom_sf(data = shp, fill = "transparent", colour = NA) +
              geom_sf(data = df, aes(fill = Share), colour = NA) + 
                scale_fill_gradientn(colors = pal, na.value = "white") +
                labs(fill = "", x = "", y = "", title = title) +
                theme(plot.title = element_text(size = 10, face = "bold.italic")) +
                labs(fill = legend) +
                theme(strip.background = element_rect(color=NULL, fill="white", size=1.5, linetype="solid")) +
                theme(panel.background = element_blank(),
                      panel.border = element_rect(colour = "grey", fill = "transparent"),
                      plot.title = element_text(hjust = 0.5))

figure

dev.off()
