  



approach = "marginal"

data_in <- read.csv(paste0(areas_base_path, "analysis/imports_", approach, "_REF_SFM100_Mha.csv"), header = TRUE)

# Clear_cut Plantation Selection Selective

year = "X2100"

category = "Plantation"

data <- data_in %>% 
          select(Mitigation_scenario, Forest_use, Management_scenario, Ecoregion, contains(year), Category) %>%
            rename("Values" = toString(year)) %>%
              rename("eco_code" = Ecoregion)

data$Category <- as.factor(data$Category)

shp_original <- st_read(dsn = "./scripts/plotting/maps_shapefiles/WWF_Ecoregions", layer = "wwf_terr_ecos")
shp <- shp_original 
shp <- shp %>% filter(Shape_Area > 0.1)


df <- left_join(shp, data, by = "eco_code")

df <- df %>% filter(Category != "NA")


legend = "Areas per ecoregion (Mha)"
pal <- c("#fafafa", "#f0f9e8", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081", "#1E4069", "#011959")


png(file = paste0("aggregation_plotting/areas/notimber/imports_", approach, "_RCP2.6_SFM100_", year, ".png"), width = 14, height = 14, res = 600, units = "in")


figure <- ggplot() +
              geom_sf(data = shp, fill = "transparent", colour = "gray45", lwd = 0.2) +
                geom_sf(data = df, aes(fill = Values), colour = NA) + 
                  scale_fill_gradientn(colors = pal, na.value = "white") + 
                    labs(fill = legend) +
                      theme(text = element_text(size = 23), 
                          axis.text = element_blank(),
                          legend.title = element_text(size = 27),
                          legend.text = element_text(size = 23),
                          legend.key.size = unit(2, "cm"), legend.key.width = unit(1,"cm"),
                          legend.position = "right") +
                      theme(strip.background = element_rect(color = NULL, fill = "white", size = 1.5, linetype = "solid"),
                          strip.text = element_text(size = 23)) +
                      theme(panel.background = element_blank(),
                          panel.border = element_rect(colour = "grey", fill = "transparent", size = 0.5),
                          plot.title = element_text(hjust = 0.5)) +
                          facet_wrap( ~ Category, ncol = 2) 

figure

  
dev.off()




