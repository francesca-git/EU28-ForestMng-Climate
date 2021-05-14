
map <- st_read(dsn = "./aggregation_plotting/Map_EU/ref-nuts-2016-01m.shp/NUTS_RG_01M_2016_3035_LEVL_0.shp", layer = "NUTS_RG_01M_2016_3035_LEVL_0")

not_in_EU28 <- c("AL", "ME", "MK", "RS", "TR", "CH", "IS", "LI", "NO")

map <- map %>% filter(!NUTS_ID %in% not_in_EU28)

plot(map)

unique(map$NUTS_ID)


shp_original <- st_read(dsn = "./aggregation_plotting/WWF_Ecoregions", layer = "wwf_terr_ecos")
  shp <- shp_original 
  shp <- shp %>% filter(Shape_Area > 0.1)
  
testchenonfunzioneramai = st_intersection(shp, map)
