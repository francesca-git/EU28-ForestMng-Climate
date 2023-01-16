setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 


#library(globiomvis)
library(ggplot2)
library(sf)
library(sp)
library(dplyr)
library(rgdal)
library(viridis)
library(RColorBrewer)
source("./areas_allocation.R")

#https://iiasa.github.io/globiomvis/articles/example2.html


shp_original <- st_read(dsn = "./ecoregions_data/WWF_Ecoregions", layer = "wwf_terr_ecos")

shp <- shp_original 
shp <- shp %>% filter(Shape_Area > 0.1)

Zoom = F
region = "global"
year = c(2100)
map = "PDF" #options: "Areas" or "PDF" or "PDF-ha"
id = "Global" #Options for Areas: "Annual", "Pasture", "For" (forests), "EP" (energy crops and plantations) - For PDF: "Global", "EUFootprint, "EUForest" 
focus = "global"
scenario = "REF-RCP"
#subset = c("REF_MFM_AFMfree", "REF_MFM_AFM100", "REF_SFM_AFMfree", "REF_SFM_AFM100", "RCP2.6_MFM_AFMfree", "RCP2.6_MFM_AFM100", "RCP2.6_SFM_AFMfree", "RCP2.6_SFM_AFM100")
#subset = c("REF_MFM_AFM25", "REF_MFM_AFM100", "REF_SFM_AFM25", "REF_SFM_AFM100", "RCP2.6_MFM_noAFM", "RCP2.6_MFM_AFM100", "RCP2.6_SFM_AFM25", "RCP2.6_SFM_AFM100")
subset = c("REF_MFM_noAFM", "RCP2.6_MFM_noAFM")
#subset = c("REF_MFM_noAFM")

  if (map == "PDF") {
    title = paste0("PDF of global species - Impacts of ", focus)
    legend = "PDF (%)"
    data <- read.csv(paste0("./species-lost_nocutoff/mg/Slost_mg_", year[1], "_V_disaggr.csv"))
  
    if (length(year) > 1) {
      data_temp <- read.csv(paste0("./species-lost_nocutoff/mg/Slost_mg_", year[2], "_V_disaggr.csv"))
      data <- data %>% full_join(data_temp)
    }
  
  } else if (map == "Areas") {
    
    title = paste0("Share of area covered by ", focus)
    legend = "Area covered (%)"
    data <- read.csv(paste0("./areas/MG/disaggregated/areas_disaggr_mg_", year[1], ".csv"))
    data$Year = year[1]
    
    if (length(year) > 1) {
      data_temp <- read.csv(paste0("./areas/MG/disaggregated/areas_disaggr_mg_", year[2], ".csv"))
      data_temp$Year = year[2]
      data <- data %>% full_join(data_temp)
    }
    
  } else if (map == "PDF-ha") {
    
    title = paste0("PDF per hectare of global species - Impacts of ", focus)
    legend = "PDF per hectare"
    data_temp <- read.csv(paste0("./plotting/no_cutoff/data/PDF-ha_mg-det.csv"))
    data <- data_temp %>% filter(Year == year[1]) %>%
                            filter(Ecoregion != "PA1203") # this ecoregion corresponds to Canary Islands and has PDF/ha one order of magnitude higher than the others -> since we want to plot EU28 values, we remove them

    if (length(year) > 1) {
      data <-  data_temp %>% filter(Year == year[1] | Year == year[2])
    }
  }



if (map == "PDF" | map == "Areas") {
  
  data <- data %>% separate( Scenario, into = c("Group", "Forest_use", "Management"), sep = "_") %>%                                       # separate the column Scenario into three columns 
              mutate(Management = str_replace(Management,"noAF", "noAFM"), Management = str_replace(Management,"AF0", "AFMfree"),  # rename the factors in the column with management information
                Management = str_replace(Management,"AF25", "AFM25"), Management = str_replace(Management,"AF50", "AFM50"),     
                Management = str_replace(Management,"AF75", "AFM75"), Management = str_replace(Management,"AF100", "AFM100"),
                Group = str_replace(Group, "RCP", "RCP2.6")) %>%                                                              # rename the rcp scenario 
                  unite("Scenario", Group:Management, sep = "_")                                                                   # re-merge the columns describing the scenario to keep it as it was initially

}

data <- data %>% dplyr::filter(Scenario %in% subset) %>%
            mutate(Year = as.character(Year))

  if (map == "PDF" | map == "PDF-ha") {
    
    data <- data %>%  
      mutate_if(is.numeric, ~.*100) %>%
        mutate(Global = rowSums(dplyr::select(., contains("median"))),
                EUFootprint = rowSums(dplyr::select(., contains("median")& ((contains("EP") & contains("EU"))|(starts_with("For") & contains("EU"))|(contains("EP") & contains("im"))|(starts_with("For_") & contains("im"))))),
                  EUForest = rowSums(dplyr::select(., contains("median")& ((starts_with("For_") & (contains("EU")|contains("ex"))) | (contains("ForOther") & contains("EU")))))) %>%
          dplyr::select(Scenario, Ecoregion, Year, contains(id)) %>%
            dplyr::rename_at(vars(all_of(id)), ~ "Values") %>%
              dplyr::rename(eco_code = Ecoregion) 
    
    
   
  } else if (map == "Areas") {
    
    data <- data %>%
      mutate(Values = rowSums(select(., contains(id)))/A_org*100) %>%
        dplyr::select(Scenario, Ecoregion, Year, Values) %>%
          dplyr::rename(eco_code = Ecoregion) 
  } 

write.csv(data, paste0("./plotting/no_cutoff/global_PDF_2100_noAF.csv"), row.names = FALSE)

df <- left_join(shp, data)

  if (Zoom == TRUE) {
    
  Globiom_eco_org <- read.csv("./grouped_land_use_files/GLOBIOM_Ecoregion.csv", header = TRUE)
    # rearrange data in Globiom_eco
    Globiom_eco_org <- filter(Globiom_eco_org, Ecoregion!="Lake", Ecoregion != "Rock and Ice") %>%
                        dplyr::rename(eco_code = Ecoregion) 
    Globiom_eco_org = droplevels(Globiom_eco_org, "Lake", "Rock and Ice")  #erase these two names from the list of factors
  
  df <- join_regions(df, Globiom_eco_org) %>%
          filter(Globiom_Reg == region)
  }
  
  df <- df %>% filter(Scenario != "NA")

ptm <- proc.time()

if (length(year) > 1) {
  
  pdf(file = paste0("./plotting/no_cutoff/", map, "_", scenario, "_", region, "_", id,".pdf"), width = 14, height = 7)

  } else{
    
  pdf(file = paste0("./plotting/no_cutoff/", map, "-", id,"_", scenario, "_", year[1], "_", region, ".pdf"), width = 14, height = 14)
  
  }

figure <- ggplot() +
            geom_sf(data = shp, fill = "transparent", colour = NA) +
              geom_sf(data = df, aes(fill = Values), colour = NA) + 
                #scale_fill_viridis() +
                #scale_fill_gradientn(colours = viridis_pal()(20), limits=c(0, 100)) +
                scale_fill_viridis(option = "magma", na.value = "grey50") +
                #scale_fill_distiller(palette = "Blues") +      
                labs(fill = "", x = "", y = "", title = title) +
                #facet_wrap(~Scenario, nrow = 4, ncol = 2) +
                labs(fill = legend) +
                #coord_sf(xlim=c(-15, 45), ylim=c(30, 75)) +
                theme(panel.background = element_blank(),
                      panel.border = element_rect(colour = "black", fill = "transparent"),
                      plot.title = element_text(hjust = 0.5))

if(length(year) == 1) {figure <- figure + facet_wrap(vars(Scenario), nrow = 4, ncol = 2) } 

if(length(year) > 1 ) {figure <- figure + facet_grid(vars(Scenario, Year), labeller = "label_both")}

if (region != "global") {figure <- figure + coord_sf(xlim=c(-15, 45), ylim=c(30, 75)) }

figure

dev.off()



# world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
proc.time() - ptm
 hist(exercise2, freq = F)
 curve(dgamma(x, shape = alpha, rate = beta), col="red", lwd=2, add = T)
 