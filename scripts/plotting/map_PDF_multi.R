

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

plot.map <- function(results_path, result_files, file_label, plots_path, id, map, graph, ratio, difference) {

  if (id == "Footprint" & missing(graph)) {print("Choose which scenarios will be plotted and assign the value to graph")}
  
  shp_original <- st_read(dsn = "./scripts/plotting/maps_shapefiles/WWF_Ecoregions", layer = "wwf_terr_ecos")
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
      climate = "RCP6.5-RCP2.6"
      Zoom = F
      group = c("RCP6.5", "RCP2.6")
      scenario = c("CFM")
      level = "noAFM"
      title = "Global loss of species - Impacts of global land use in 2020 and 2100"
  
  } else if (id == "EUFootprint") {
      print(id)
      region = "global"
      year = c(2100)
      focus = "European forest biomass"
      climate = "RCP6.5-RCP2.6"
      Zoom = F
      group = c("RCP6.5", "RCP2.6")
      scenario = c("CFM", "SFM")
      level = c("noAFM", "25%", "50%")
      title = "Global loss of species - Impacts of EU forest biomass demand in 2100 (RCP2.6)"
  
  } else if (id == "EUForest"){
      region = "EU"
      year = c(2100)
      focus = "European internal forest"
      climate = "RCP6.5-RCP2.6"
      Zoom = TRUE
      group = c("RCP6.5","RCP2.6")
      scenario = c("CFM", "SFM")
      # level = c("noAFM", "25%", "50%")
      level = c("noAFM", "25%", "50%")
      title = "Global loss of species - Impacts of EU internal forest management in 2100"
  
  } else {stop("Define id")}
  
  # Load the data ====
  
  if(map == "PDF") {
      legend = "Extinction risk [PDF]"
      data <- read.csv(paste0(results_path, "/", result_files, year[1], file_label, ".csv"))
    
      if (length(year) > 1) {
        data_temp <- read.csv(paste0(results_path, "/", result_files, year[2], file_label, ".csv"))
        data <- data %>% full_join(data_temp)
      }
  }
  
  if(map == "PDFha") {
      legend = "Extinction risk per hectare [PDF/ha] (logarithmic scale)"
      data <- read.csv(paste0(csv_path, "PDF-ha.csv"), header = TRUE)
      
        if(id == "Global") {data <- data %>% filter(Year == "2100" | Year == "2020")
        }else if (id != "Global")  {data <- data %>% filter(Year == "2100")}
  }
  
  #====
      
  # Rename elements ====
    data <- data %>% separate(Scenario, into = c("Group", "Scenario", "Level"), sep = "_") %>%                                       # separate the column Scenario into three columns 
                mutate(Level = str_replace(Level,"noAF", "noAFM"), Level = str_replace(Level,"AF0", "Free"),  # rename the factors in the column with Level information
                  Level = str_replace(Level,"AF25", "12.5%"), Level = str_replace(Level,"AF50", "25%"),     
                  Level = str_replace(Level,"AF75", "37.5%"), Level = str_replace(Level,"AF100", "50%"),
                  Group = str_replace(Group, "RCP", "RCP2.6"),  Group = str_replace(Group, "REF", "RCP6.5"), Scenario = str_replace(Scenario,"MFM","CFM"), Scenario = str_replace(Scenario, "SFM", "SFM")) #%>%                                                              # rename the rcp scenario 
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
        #mutate_if(is.numeric, ~.*100) %>%
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
          print("Global")
          data <- data %>% filter(Year == "2100" | (Year == "2020" & Group == "RCP6.5")) %>% # for the year 2020, select only one climate scenario
                            rename(Management = Scenario) %>%
                              unite("Scenario", c(Year, Group), sep = "-", remove = TRUE) %>%
                                mutate(Scenario = str_replace(Scenario, "2020-RCP6.5", "2020-RCP6.5/RCP2.6"))
          data$Scenario <- factor(data$Scenario, levels = c("2020-RCP6.5/RCP2.6", "2100-RCP6.5", "2100-RCP2.6"), labels = c("2020 - RCP6.5 / RCP2.6", "2100 - RCP6.5", "2100 - RCP2.6"))
        
      } else if(id == "EUFootprint") {
            print("EUFootprint")
              if(graph == "B-25-50") {
                print("B-25-50")
                data <- data %>% filter(Level == "25%" | Level == "50%" | Level == "noAFM" & Scenario == "CFM") %>%
                                    unite("Scenario", c(Scenario, Level), sep = "-", remove = TRUE) %>%
                                      mutate(Scenario = str_replace(Scenario, "CFM-noAFM", "noAFM"))
                data$Group = factor(data$Group, levels = c("RCP6.5", "RCP2.6"), labels = c("RCP6.5", "RCP2.6"))
                data$Scenario = factor(data$Scenario, levels = c("noAFM", "CFM-25%", "CFM-50%", "SFM-25%", "SFM-50%"), labels = c("noAFM", "CFM 25%", "CFM 50%", "SFM 25%" , "SFM 50%"))

              } else if(graph == "B-50") {
                print("B-50")
                data <- data %>% filter(Level == "50%" | Level == "noAFM" & Scenario == "CFM") %>%
                                    unite("Scenario", c(Scenario, Level), sep = "-", remove = TRUE) %>%
                                      mutate(Scenario = str_replace(Scenario, "CFM-noAFM", "noAFM"))
                data$Group = factor(data$Group, levels = c("RCP6.5", "RCP2.6"), labels = c("RCP6.5", "RCP2.6"))
                data$Scenario = factor(data$Scenario, levels = c("noAFM", "CFM-50%", "SFM-50%"), labels = c("noAFM", "CFM 50%", "SFM 50%"))
                
          
      }} else if(id == "EUForest") {
          print("EUForest")
          data <- data %>% filter(Level == "25%" | Level == "50%" | (Level == "noAFM" & Scenario == "CFM") | (Level == "Free" & Scenario == "CFM")) %>%
                              unite("Scenario", c(Scenario, Level), sep = "-", remove = TRUE) %>%
                                mutate(Scenario = str_replace(Scenario, "CFM-noAFM", "noAFM"))
           data$Group = factor(data$Group, levels = c("RCP6.5", "RCP2.6"), labels = c("RCP6.5", "RCP2.6"))
          data$Scenario = factor(data$Scenario, levels = c("noAFM", "CFM-25%", "SFM-25%", "CFM-50%", "SFM-50%"), labels = c("noAFM", "CFM 25%", "CFM 50%", "SFM 25%" , "SFM 50%"))

          # data <- data %>% filter(Level == "50%" | (Level == "noAFM" & Scenario == "CFM") | (Level == "Free" & Scenario == "CFM")) %>%
          #                     unite("Scenario", c(Scenario, Level), sep = "-", remove = TRUE) %>%
          #                       mutate(Scenario = str_replace(Scenario, "CFM-noAFM", "noAFM"))
          #  data$Group = factor(data$Group, levels = c("RCP6.5", "RCP2.6"), labels = c("RCP6.5", "RCP2.6"))
          # data$Scenario = factor(data$Scenario, levels = c("noAFM", "CFM-50%", "SFM-50%"), labels = c("noAFM (noAFM)", "CFM 50%", "SFM 50%"))
          # 
          }

  
  if (ratio == TRUE) {
    data_wide <- data.frame(data %>% mutate(Scenario = str_replace(Scenario, "CFM 50%", "CFM50"),
                                            Scenario = str_replace(Scenario, "CFM 25%", "CFM25"),
                                            Scenario = str_replace(Scenario, "SFM 25%", "SFM25"),
                                            Scenario = str_replace(Scenario, "SFM 50%", "SFM50")) %>%
                    unite("Scenario", c(Group, Scenario), sep = "_") %>%
                      arrange(Scenario, Year, eco_code) %>%
                        pivot_wider(names_from = Scenario, values_from = Values))
    
    data_wide <- data_wide %>% mutate(reference = RCP6.5_noAFM) %>% mutate_if(is.numeric, ~./reference)
    
  } else if (difference == TRUE) {
  
     data_wide <- data.frame(data %>% mutate(Scenario = str_replace(Scenario, "CFM 50%", "CFM50"),
                                            Scenario = str_replace(Scenario, "CFM 25%", "CFM25"),
                                            Scenario = str_replace(Scenario, "SFM 25%", "SFM25"),
                                            Scenario = str_replace(Scenario, "SFM 50%", "SFM50")) %>%
                    unite("Scenario", c(Group, Scenario), sep = "_") %>%
                      arrange(Scenario, Year, eco_code) %>%
                        pivot_wider(names_from = Scenario, values_from = Values))
    
    data_wide <- data_wide %>% mutate(reference = RCP6.5_noAFM) %>% mutate_if(is.numeric, ~.-reference)
  }
  
  if(ratio == TRUE | difference == TRUE) {
      if(graph == "B-25-50") {
          data_wide <- data_wide %>% transmute(eco_code = eco_code, Year = Year, RCP6.5_noAFM = RCP6.5_noAFM, 
                                               RCP6.5_CFM25 = RCP6.5_CFM25, 
                                               RCP6.5_CFM50 = RCP6.5_CFM50, 
                                               RCP6.5_SFM25 = RCP6.5_SFM25, 
                                               RCP6.5_SFM50 = RCP6.5_SFM50, 
                                               RCP2.6_noAFM = RCP2.6_noAFM, 
                                               RCP2.6_CFM25 = RCP2.6_CFM25,
                                               RCP2.6_CFM50 = RCP2.6_CFM50, 
                                               RCP2.6_SFM25 = RCP2.6_SFM25, 
                                               RCP2.6_SFM50 = RCP2.6_SFM50) 
      }else if(graph == "B-50") {
          data_wide <- data_wide %>% transmute(eco_code = eco_code, Year = Year, RCP6.5_noAFM = RCP6.5_noAFM, 
                                               RCP6.5_CFM50 = RCP6.5_CFM50, 
                                               RCP6.5_SFM50 = RCP6.5_SFM50, 
                                               RCP2.6_noAFM = RCP2.6_noAFM, 
                                               RCP2.6_CFM50 = RCP2.6_CFM50, 
                                               RCP2.6_SFM50 = RCP2.6_SFM50) 
      }
    
    
    data_wide[is.na(data_wide)] <- 0
    
    
    data_long <- data.frame(data_wide %>% pivot_longer(cols = RCP6.5_noAFM:RCP2.6_SFM50, names_to = "Scenario", values_to = "Values") %>%
                    separate(Scenario, into = c("Group", "Scenario"), sep = "_")  %>%
                        arrange(Group, Year, Scenario, eco_code))

    if(graph == "B-25-50") {
    data_long <- data_long %>%  
                 mutate(Scenario = str_replace(Scenario, "CFM25", "CFM-25%")) %>%
                    mutate(Scenario = str_replace(Scenario, "CFM50", "CFM-50%")) %>%
                        mutate(Scenario = str_replace(Scenario, "SFM25", "SFM-25%")) %>%
                          mutate(Scenario = str_replace(Scenario, "SFM50", "SFM-50%")) 
          data_long$Group = factor(data_long$Group, levels = c("RCP6.5", "RCP2.6"))
          #data_long$Scenario = factor(data_long$Scenario, levels = c("noAFM", "CFM-50%", "SFM-50%"))
          data_long$Scenario = factor(data_long$Scenario, levels = c("noAFM", "CFM-25%", "CFM-50%", "SFM-25%", "SFM-50%"))
    }
    
    if(graph == "B-50") {
    data_long <- data_long %>%  
                 mutate(Scenario = str_replace(Scenario, "CFM50", "CFM-50%")) %>%
                        mutate(Scenario = str_replace(Scenario, "SFM50", "SFM-50%")) 
          data_long$Group = factor(data_long$Group, levels = c("RCP6.5", "RCP2.6"))
          #data_long$Scenario = factor(data_long$Scenario, levels = c("noAFM", "CFM-50%", "SFM-50%"))
          data_long$Scenario = factor(data_long$Scenario, levels = c("noAFM", "CFM-50%", "SFM-50%"))
    }
    
    if(ratio == TRUE) {
    data_long[data_long$eco_code == "PA1212", "Values"] = NaN
    legend = "log-scale"
    }

    data <- data_long
    
  }
  
   
  #data <- data %>% filter(Scenario == "noAFM" | Scenario == "SFM-50%")
  
  data_backup <- data
  #==== 
  #write.csv(data, paste0("./plotting/no_cutoff/Global_PDF_BAU-RCP_", id ,"_ecoregion.csv"), row.names = FALSE)

  # if (region == "global") {
  #   
  #   df <- df %>% filter(Values >= 0)
  #   
  # }    

  df <- left_join(shp, data)
    
  
  # Zoom ====
    if (Zoom == TRUE) {
  
    EU_map <- st_read(dsn = "./scripts/plotting/maps_shapefiles/Map_EU_dissolved", layer = "dissolved_EU")
    
    }


  #====
  
  df <- df %>% filter(Scenario != "NA")
  
  if(map == "PDFha" | ratio == TRUE) {  
    df <- df %>% mutate(Values = log10(Values))
    df$Values[is.infinite(df$Values)] <- NA
    # if(ratio == TRUE){
    #      df <- df %>% mutate(Values = ifelse(Values < -4, 0, Values))
    # }
  }
  
  #test = "RdYlBu"
      
  #pal <- c("#fafafa", "#EAEDF6", "#D7DFEF", "#C5D1E9", "#B2C3E2", "#A0B5DC", "#8CA7D5", "#789ACF", "#638CC8", "#4B7FC3", "#3872B6", "#3165A2", "#2B598E", "#244D7B", "#1E4069")
  #pal <- c("#F1F1F1", "#E1EDC9", "#C0E7BB", "#98DEB6", "#6BD1B9", "#3CC2BE", "#1CAEC3", "#3797C3", "#5B7BBD", "#775BAF", "#833993", "#80146E")
  # this is the palette to use:    
  pal <- c("#fafafa", "#f0f9e8", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081", "#1E4069", "#011959")
  pal <- c("#fafafa", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081", "#1E4069", "#011959")

        # #f3f7f0
  ptm <- proc.time()
  
  if (length(year) > 1) {
    
    png(file = paste0(plots_path, map, "_", climate, "_", region, file_label, "_", energy_exports , "_new.png"), width = 7, height = 12, res = 600, units = "in") # 7, 12
    } else{
      
    if(ratio == TRUE & difference == FALSE) {
    #png(file = paste0(plots_path, map, "-", id,"_", climate, "_", year[1], "_", region, file_label, "_",energy_exports, "_diff.png"),  width = 5, height = 9, res = 600, units = "in") # 10, 18
      png(file = paste0(plots_path, map, "-", id,"_", climate, "_", year[1], "_", region, file_label, "_",energy_exports, "_ratio_new.png"),  width = 30, height = 15, res = 600, units = "cm") # 20, 10
        }else if(difference == TRUE & ratio == FALSE) {
          png(file = paste0(plots_path, map, "-", id,"_", climate, "_", year[1], "_", region, file_label, "_",energy_exports, "_diff_new.png"),  width = 30, height = 15, res = 600, units = "cm") # 20, 10
          }else if(ratio == FALSE & difference == FALSE){
            png(file = paste0(plots_path, map, "-", id,"_", climate, "_", year[1], "_", region, file_label, "_",energy_exports, "_new_pal.png"),  width = 30, height = 15, res = 600, units = "cm") # 20, 10
              }
    }
  
  print(id)
  
  # max for EUForest: 0.006180501
  # max for EUFootprint: 0.007033453
  
    # if(id == "EUFootprint")  {# & file_label != "_cutoff_mammals") {
    #   li <- c(0, 0.020)# max(data$Values))
    #   br <- c(0, 0.005, 0.010, 0.015)
    # }
    # 
    # if(id == "EUForest") { # & file_label != "_cutoff_mammals") {
    #   li <- c(0, 0.015)
    #   br <- c(0, 0.005, 0.010, 0.015)
    # }

    # if(id == "EUFootprint")  {# & file_label != "_cutoff_mammals") {
    #   li <- c(0, 0.010)# max(data$Values))
    #   br <- c(0, 0.002, 0.004, 0.006, 0.008, 0.010)
    # }
    # 
  
    # if(id == "EUForest" & ratio == FALSE & difference == FALSE) {
    #   li <- c(0, 0.01)
    #   br <- c(0, 0.002, 0.004, 0.006, 0.008, 0.010)
    # }
    # 
    # if(id == "EUFootprint" & ratio == FALSE & difference == FALSE) {
    #   li <- c(0, 0.022)
    #   br <- c(0, 0.004, 0.008, 0.012, 0.016, 0.020)
    # } 
  
    if(id == "EUForest" && (grepl("mammals", file_label, fixed = TRUE)||grepl("birds", file_label, fixed = TRUE)||grepl("plants", file_label, fixed = TRUE)) && ratio == FALSE & difference == FALSE) {
      li <- c(0, 1.1e-04)
      br <- c(0, 2e-05, 4e-05, 6e-05, 8e-05, 1e-04)
    }
    
    if(id == "EUFootprint" && (grepl("mammals", file_label, fixed = TRUE)||grepl("birds", file_label, fixed = TRUE)||grepl("plants", file_label, fixed = TRUE)) && ratio == FALSE & difference == FALSE) {
      li <- c(0, 1.3e-04)
      br <- c(0, 3e-05, 6e-05, 9e-05, 1.2e-04)
      show(li)
      show(br)
    }
    # 
    # 
    # if(id == "EUForest" & subcase == "" & difference == TRUE) {
    #   li <- c(-0.002, 0.01)
    #   br <- c(-0.002, 0, 0.002, 0.004, 0.006, 0.008, 0.010)
    # }
    
    # if(id == "EUFootprint" & subcase == "" & difference == TRUE) {
    #   li <- c(-0.0025, 0.0175)
    #   br <- c(0, 0.005, 0.010, 0.015)
    # } 

    # if(id == "EUForest" & subcase == "" & ratio == TRUE) {
    #   li <- c(-1, 1.5)
    #   br <- c(-1, -0.5, 0, 0.5, 1)
    # }
    
    # if(id == "EUFootprint" & subcase == "" & ratio == TRUE) {
    #   li <- c(-0.5, 7)
    #   br <- c(0, 2, 4, 6)
    # } 

  
  figure <- ggplot() +
              geom_sf(data = shp, fill = "transparent", colour = "gray45", lwd = 0.2) +
                geom_sf(data = df, aes(fill = Values), colour = NA) + 
                  #scale_fill_viridis(option = test, na.value = "grey50", direction = -1) + # for PDF
                  #scale_fill_continuous_diverging(test, c1 = 70, na.value = "white", rev = TRUE) +
                  #scale_fill_scico(palette = test, direction = 1) + #, begin = 0.5, end = 1) +
                  #scale_fill_distiller(palette = "RdYlGn", na.value = "white")  +   
                  #labs(fill = "", x = "", y = "", title = title) +
                  #theme_minimal() +
                  #scale_fill_distiller(palette = test, direction = 1, na.value = "grey90") +
                  #scale_fill_continuous_sequential(test, na.value = "white") +
                  #theme(plot.title = element_text(size = 10, face = "bold.italic")) +
                  #facet_wrap(~Scenario, nrow = 4, ncol = 2) +
                  labs(fill = legend) +
                  theme(text = element_text(size = 14), 
                        axis.text = element_blank(),
                        legend.title = element_text(size = 11),
                        #legend.text = element_text(size = 17),
                        legend.key.size = unit(0.5, "cm"), legend.key.width = unit(0.5,"cm"), legend.key.height = unit(1, "cm"),
                        legend.position = "right") +
                  theme(strip.background = element_rect(color = NULL, fill = "white", size = 1.5, linetype = "solid")) + #strip.text = element_text(size = 17)) +
                  theme(panel.background = element_blank(),
                        panel.border = element_rect(colour = "grey", fill = "transparent", size = 0.5),
                        plot.title = element_text(hjust = 0.5))
  
  if ((!grepl("mammals", file_label, fixed = TRUE) && !grepl("birds", file_label, fixed = TRUE) && !grepl("plants", file_label, fixed = TRUE)) && ratio == FALSE && difference == FALSE) {
    figure <- figure +
      scale_fill_gradientn(colors = pal, na.value = "white", labels = function(x) format(x, scientific = TRUE)) #, limits = li, breaks = br) ################################ this is the line to keep
      show(pal)
  } else if(ratio == TRUE || difference == TRUE) {
    figure <- figure +                   
    scale_fill_gradient2(low = "darkgreen", mid = "white", high = "red", midpoint = 0, na.value = "grey98", labels = function(x) format(x, scientific = TRUE)) #, limits = li, breaks = br)
  } else if((grepl("mammals", file_label, fixed = TRUE)||grepl("birds", file_label, fixed = TRUE)||grepl("plants", file_label, fixed = TRUE)) && ratio == FALSE && difference == FALSE) {
    figure <- figure + 
          scale_fill_gradientn(colors = pal, na.value = "white", limits = li, breaks = br, labels = function(x) format(x, scientific = TRUE)) ################################ this is the line to keep
          show(li)
          show(br)
  }
  
  # if( region == "global") {figure <- figure + facet_wrap(~ Group + Scenario, nrow = 4) + coord_sf(ylim=c(-60, 90)) } 
  if(id == "EUFootprint") {figure <- figure + facet_grid(vars(Scenario), vars(Group)) + coord_sf(ylim=c(-60, 90)) } 
  
  if(id == "Global") {figure <- figure + facet_wrap( ~ Scenario, ncol = 1) } 
  
  #if(length(year) > 1 && region == "global") {figure <- figure + facet_wrap(vars(Scenario))}
  
  if (id == "EUForest") {
    figure <- figure + geom_sf(data = EU_map, size = 0.3, fill = "transparent", colour = "black")
    figure <- figure + facet_grid(Group ~ Scenario) 
    figure <- figure + coord_sf(xlim = c(-15, 40), ylim = c(30, 75)) 
    }
  
  print(figure)
  
  dev.off()
 } 

  # 
  # 
  # figure <- ggarrange(figure1, figure2, figure3, common.legend = TRUE, legend = "bottom")
  #   png(file = paste0(save_files_path, map, "-", id,"_", climate, "_", year[1], "_", region, file_label, ".png"),  width = 6, height = 6, res = 600, units = "in")
  # 
  # figure
  # dev.off()
  # 
  # ggsave(paste0(save_files_path, map, "-", id,"_", climate, "_", year[1], "_", region, file_label, ".png"), width = 21, height = 20, units = "cm")
  # 
