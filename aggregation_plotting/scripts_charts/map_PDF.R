

#############################################################################################################################################################################

                                                                  # MAPS OF IMPACTS #

#############################################################################################################################################################################

# General task: plot the impact maps according to the selected id (id refer to the impacts of either the global land use, the EU Footprint, the internal EU forests)
# Date: September 2020
# Author: Francesca Rosa



#############################################################################################################################################################################

# WARNING: The functions defined in this file are called in the file csv_and_plotting.R. The loading/saving paths and the working directory are defined in that file as well.

#############################################################################################################################################################################


#library(globiomvis) ====
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
library(scico)
library(gridExtra)
library(ggpubr)
select <- dplyr::select

#https://iiasa.github.io/globiomvis/articles/example2.html
# ====


plot.map <- function(folder_slost, file_slost, case_subcase, plots_path, id, energy_exports, map)  { 
  
  if(missing(map)){map = "PDF"}
  # folder_slost = folder where the files to be loaded are stored, file_slost = initial part of the name of the file to be loaded, 
  # case_subcase = identification words which selects the file, 
  # plots_path = folder where the plots will be saved
  # id = can be "EUFootprint", "EUForest" or "Global"
  
  shp_original <- st_read(dsn = "./aggregation_plotting/WWF_Ecoregions", layer = "wwf_terr_ecos")
  shp <- shp_original 
  shp <- shp %>% filter(Shape_Area > 0.1)
  
  #test = "PuRd"
  #test = "Purple-Yellow"
  #pal <- c("#F1F1F1", "#E1EDC9", "#C0E7BB", "#98DEB6", "#6BD1B9", "#3CC2BE", "#1CAEC3", "#3797C3", "#5B7BBD", "#775BAF", "#833993", "#80146E")

  
  #id = "EUFootprint"
  
  if(id == "Global"){
      region = "global"
      year = c(2020, 2100)
      focus = "global land use"
      climate = "REF-RCP"
      Zoom = F
      group = c("RCP6.5-REF", "RCP2.6")
      scenario = c("Multifunctional")
      level = "Baseline"
      title = "Global loss of species - Impacts of global land use in 2020 and 2100"
  
  } else if (id == "EUFootprint") {
      print(id)
      region = "global"
      year = c(2100)
      focus = "European forest biomass"
      climate = "RCP"
      Zoom = F
      group = c("RCP2.6")
      scenario = c("Multifunctional", "Set-aside")
      level = c("Free", "100%")
      title = "Global loss of species - Impacts of EU forest biomass demand in 2100 (RCP2.6)"
  
  } else if (id == "EUForest"){
      region = "Europe"
      year = c(2100)
      focus = "European internal forest"
      climate = "REF-RCP"
      Zoom = FALSE
      group = c("RCP6.5-REF","RCP2.6")
      scenario = c("Multifunctional", "Set-aside")
      level = c("Baseline","25%", "100%")
      title = "Global loss of species - Impacts of EU internal forest management in 2100"
  
  } else {stop("Define id")}
  
  # Load the data ====
  
  if(map == "PDF") {
      legend = "PDF (%)"
      data <- read.csv(paste0(folder_slost, file_slost, year[1], case_subcase, ".csv"))
    
      if (length(year) > 1) {
        data_temp <- read.csv(paste0(folder_slost, file_slost, year[2], case_subcase, ".csv"))
        data <- data %>% full_join(data_temp)
      }
  }
  
  if(map == "PDFha") {
      legend = "PDF(%)/ha (logarithmic scale)"
      data <- read.csv(paste0(csv_path, "PDF-ha.csv"), header = TRUE)
      
        if(id == "Global") {data <- data %>% filter(Year == "2100" | Year == "2020")
        }else if (id != "Global")  {data <- data %>% filter(Year == "2100")}
  }
  
  #====
      
  # Rename elements ====
    data <- data %>% separate( Scenario, into = c("Group", "Scenario", "Level"), sep = "_") %>%                                       # separate the column Scenario into three columns 
                mutate(Level = str_replace(Level,"noAF", "Baseline"), Level = str_replace(Level,"AF0", "Free"),  # rename the factors in the column with Level information
                  Level = str_replace(Level,"AF25", "25%"), Level = str_replace(Level,"AF50", "50%"),     
                  Level = str_replace(Level,"AF75", "75%"), Level = str_replace(Level,"AF100", "100%"),
                  Group = str_replace(Group, "RCP", "RCP2.6"),  Group = str_replace(Group, "REF", "RCP6.5-REF"), Scenario = str_replace(Scenario,"MFM","Multifunctional"), Scenario = str_replace(Scenario, "SFM", "Set-aside")) #%>%                                                              # rename the rcp scenario 
                    #unite("Scenario", Group:Level, sep = "_")                                                                   # re-merge the columns describing the scenario to keep it as it was initially
  #====
      
  # Filter the elements according to what we want to plot ====
    data <- data %>% dplyr::filter(Group %in% group) %>%
                      dplyr::filter(Scenario %in% scenario) %>%
                        dplyr::filter(Level %in% level) %>%
                mutate(Year = as.character(Year))
    
  #====
  
  # Sum the rows which should be aggregated ====
      data <- data %>%  
        mutate_if(is.numeric, ~.*100) %>%
          mutate(Global = rowSums(select(., contains("median"))),
                  EUFootprint = case_when((energy_exports == "ex" | energy_exports == "EPnoex") 
                                          ~ rowSums(select(., contains("median") & ((contains("EP") & contains("EU"))|(starts_with("For") & contains("EU"))|(contains("EP") & contains("im"))|(starts_with("For_") & contains("im")))), na.rm = TRUE),
                                          # For_ClearCut_EU_median + For_ClearCut_im_median + For_Retention_EU_median + For_Plantation_im_median + For_TimberPlant_EU_median + 
                                          #   + For_SelectionSystem_EU_median + For_Selective_im_median + EP_EU_median + EP_conv_EU_median + EP_conv_im_median + ForOther_Extensive_EU_median + 
                                          #   + ForOther_Intensive_EU_median,
                                          (energy_exports == "noEPnoex")
                                          ~ rowSums(select(., contains("median") & ((starts_with("For") & contains("EU"))|(starts_with("For_") & contains("im")))), na.rm = TRUE)),
                  EUForest = case_when((energy_exports == "ex") 
                                       ~ rowSums(select(., contains("median")& ((starts_with("For_") & (contains("EU")|contains("ex"))) | (contains("ForOther") & contains("EU")))), na.rm = TRUE),
                                       (energy_exports == "EPnoex") 
                                       ~ rowSums(select(., contains("median") & (((starts_with("For_") | starts_with("EP") ) & (contains("EU"))) | (contains("ForOther") & contains("EU")))), na.rm = TRUE),
                                       (energy_exports == "noEPnoex") 
                                       ~ rowSums(select(., contains("median") & ((starts_with("For_") & contains("EU")) | (contains("ForOther") & contains("EU")))), na.rm = TRUE))
            ) %>%
            select(Group, Scenario, Level, Ecoregion, Year, contains(id)) %>%
              dplyr::rename_at(vars(all_of(id)), ~ "Values") %>%
                dplyr::rename(eco_code = Ecoregion) 
      
        if(id == "Global") {                
          data <- data %>% filter(Year == "2100" | (Year == "2020" & Group == "RCP6.5-REF")) %>% # for the year 2020, select only one climate scenario
                            rename(Management = Scenario) %>%
                              unite("Scenario", c(Year, Group), sep = "-", remove = TRUE) %>%
                                mutate(Scenario = str_replace(Scenario, "2020-RCP6.5-REF", "2020-RCP6.5-REF/RCP2.6"))
          data$Scenario <- factor(data$Scenario, levels = c("2020-RCP6.5-REF/RCP2.6", "2100-RCP6.5-REF", "2100-RCP2.6"), labels = c("2020 - RCP6.5(REF) / RCP2.6", "2100 - RCP6.5 (REF)", "2100 - RCP2.6"))
        }
      
          if(id == "EUFootprint") {
          data <- data %>% filter(Level == "100%" | (Level == "Free" & Scenario == "Multifunctional")) %>%
                            select(-Group) %>%
                              unite("Scenario", c(Scenario, Level), sep = "-", remove = TRUE) %>%
                                mutate(Scenario = str_replace(Scenario, "Multifunctional-Free", "AFM-Free"))
          data$Scenario = factor(data$Scenario, levels = c("AFM-Free", "Multifunctional-100%", "Set-aside-100%"))
        }
      
      
        if(id == "EUForest") {
          data <- data %>% filter(Scenario == "Set-aside" | (Scenario == "Multifunctional" & Level == "100%")) %>%
                              unite("Scenario", c(Scenario, Level), sep = "-", remove = TRUE) %>%
                                mutate(Scenario = str_replace(Scenario, "Set-aside-Baseline", "Baseline"))
          data$Scenario = factor(data$Scenario, levels = c("Baseline", "Multifunctional-100%", "Set-aside-25%", "Set-aside-100%"))
          data$Group = factor(data$Group, levels = c("RCP6.5-REF", "RCP2.6"), labels = c("RCP6.5 (REF)", "RCP2.6"))
        }
      
  #==== 
  #write.csv(data, paste0("./plotting/no_cutoff/Global_PDF_REF-RCP_", id ,"_ecoregion.csv"), row.names = FALSE)
  
  df <- left_join(shp, data)
      
  # if (region == "global") {
  #   
  #   df <- df %>% filter(Values >= 0)
  #   
  # }    
      
  # Zoom ====
    if (Zoom == TRUE) {
      
    Globiom_eco_org <- read.csv("./grouped_land_use_files/GLOBIOM_Ecoregion.csv", header = TRUE)
      # rearrange data in Globiom_eco
      Globiom_eco_org <- filter(Globiom_eco_org, Ecoregion!="Lake", Ecoregion != "Rock and Ice") %>%
                          dplyr::rename(eco_code = Ecoregion) 
      Globiom_eco_org = droplevels(Globiom_eco_org, "Lake", "Rock and Ice")  #erase these two names from the list of factors
    
    df <- join_regions(df, Globiom_eco_org) %>%
            filter(Globiom_Reg == region)
    }
  #====
  
  df <- df %>% filter(Scenario != "NA")
  
  if(map == "PDFha") {  
    df <- df %>% mutate(Values = log10(Values))
    df$Values[is.infinite(df$Values)] <- NA
  }
  
  #test = "RdYlBu"
      
  #pal <- c("#fafafa", "#EAEDF6", "#D7DFEF", "#C5D1E9", "#B2C3E2", "#A0B5DC", "#8CA7D5", "#789ACF", "#638CC8", "#4B7FC3", "#3872B6", "#3165A2", "#2B598E", "#244D7B", "#1E4069")
  #pal <- c("#F1F1F1", "#E1EDC9", "#C0E7BB", "#98DEB6", "#6BD1B9", "#3CC2BE", "#1CAEC3", "#3797C3", "#5B7BBD", "#775BAF", "#833993", "#80146E")
  # this is the palette to use:    
  pal <- c("#fafafa", "#f0f9e8", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081", "#1E4069", "#011959")
      # #f3f7f0
  ptm <- proc.time()
  
  if (length(year) > 1) {
    
    #pdf(file = paste0("./plotting/no_cutoff/", map, "_", climate, "_", region, "_", test,"lr.pdf"), width = 8, height = 15)
    png(file = paste0(plots_path, map, "_", climate, "_", region, case_subcase, "_", energy_exports , ".png"), width = 14, height = 24, res = 600, units = "in")
    } else{
      
    #pdf(file = paste0("./plotting/no_cutoff/", map, "-", id,"_", climate, "_", year[1], "_", region, "_", test,"lr.pdf"), width = 8, height = 4)
    png(file = paste0(plots_path, map, "-", id,"_", climate, "_", year[1], "_", region, case_subcase, "_",energy_exports, ".png"),  width = 14, height = 24, res = 600, units = "in")
  
    }
  
  print(id)
  
  # max for EUForest: 0.006180501
  # max for EUFootprint: 0.007033453
  
    if(id == "EUFootprint")  {# & case_subcase != "_cutoff_mammals") {
      li <- c(0, 0.007033453)
      br <- c(0, 0.002, 0.004, 0.006)
    }

    if(id == "EUForest") { # & case_subcase != "_cutoff_mammals") {
      li <- c(0, 0.006180501)
      br <- c(0, 0.002, 0.004, 0.006)
    }

    # if(case_subcase == "_cutoff_mammals") {
    #   li <- c(0, max(df$Values))
    #   br <- round(seq(0, max(df$Values), length = 6), digits = 4)
    # }
    # 
    # if(id = "EUForest" & (case_subcase == "_cutoff" | case_subcase == "_Chaudhary2015" | 
    #    case_subcase == "_Chaudhary2018" | case_subcase == "_LCImpact")) {
    #   li <- c(0, 0.014)
    #   br <- c(0, 0.002, 0.004, 0.006, 0.008, 0.010, 0.012, 0.014)
    # }
    # 
    # if(id = "EUFootprint" & (case_subcase == "_cutoff" | case_subcase == "_Chaudhary2015" | 
    #      case_subcase == "_Chaudhary2018" | case_subcase == "_LCImpact")) {
    #     li <- c(0, 0.015)
    #     br <- c(0, 0.005, 0.010, 0.015)
    # }
    # 
  figure <- ggplot() +
              geom_sf(data = shp, fill = "transparent", colour = NA) +
                geom_sf(data = df, aes(fill = Values), colour = NA) + 
                  #scale_fill_viridis(option = test, na.value = "grey50", direction = -1) + # for PDF
                  #scale_fill_continuous_diverging(test, c1 = 70, na.value = "white", rev = TRUE) +
                  #scale_fill_scico(palette = test, direction = 1) + #, begin = 0.5, end = 1) +
                  scale_fill_gradientn(colors = pal, na.value = "white", limits = li, breaks = br) + ################################ this is the line to keep
                  #labs(fill = "", x = "", y = "", title = title) +
                  #theme_minimal() +
                  #scale_fill_distiller(palette = test, direction = 1, na.value = "grey90") +
                  #scale_fill_continuous_sequential(test, na.value = "white") +
                  #theme(plot.title = element_text(size = 10, face = "bold.italic")) +
                  #facet_wrap(~Scenario, nrow = 4, ncol = 2) +
                  labs(fill = legend) +
                  theme(text = element_text(size = 30), 
                        axis.text = element_blank(),
                        legend.title = element_text(size = 30),
                        legend.text = element_text(size = 30),
                        legend.key.size = unit(2, "cm"), legend.key.width = unit(1,"cm"),
                        legend.position = "right") +
                  theme(strip.background = element_rect(color = NULL, fill = "white", size = 1.5, linetype = "solid"),
                        strip.text = element_text(size = 35)) +
                  theme(panel.background = element_blank(),
                        panel.border = element_rect(colour = "grey", fill = "transparent", size = 0.5),
                        plot.title = element_text(hjust = 0.5))
  
  if( region == "global") {figure <- figure + facet_wrap( ~ Scenario, ncol = 1) } 
  
  #if(length(year) > 1 && region == "global") {figure <- figure + facet_wrap(vars(Scenario))}
  
  if (region != "global") {
    figure <- figure + facet_grid(Scenario ~ Group) 
    figure <- figure + coord_sf(xlim=c(-15, 45), ylim=c(30, 75)) 
    }
  
  figure
  
  
  
  dev.off()
  
}
  # 
  # 
  # figure <- ggarrange(figure1, figure2, figure3, common.legend = TRUE, legend = "bottom")
  #   png(file = paste0(save_files_path, map, "-", id,"_", climate, "_", year[1], "_", region, case_subcase, ".png"),  width = 6, height = 6, res = 600, units = "in")
  # 
  # figure
  # dev.off()
  # 
  # ggsave(paste0(save_files_path, map, "-", id,"_", climate, "_", year[1], "_", region, case_subcase, ".png"), width = 21, height = 20, units = "cm")
  # 
