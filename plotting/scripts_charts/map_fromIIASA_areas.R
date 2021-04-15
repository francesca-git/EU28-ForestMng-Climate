setwd("C:/Users/Rosa/Documents/PhD_project/Forest_management/calculation/") 

#library(globiomvis)
library(ggplot2)
library(sf)
library(sp)
library(dplyr)
library(rgdal)
library(viridis)
library(RColorBrewer)
library(colorspace)
library(stringr)
library(scico)

source("./areas_allocation.R")

#https://iiasa.github.io/globiomvis/articles/example2.html


shp_original <- st_read(dsn = "./ecoregions_data/WWF_Ecoregions", layer = "wwf_terr_ecos")

shp <- shp_original 
shp <- shp %>% filter(Shape_Area > 0.1)
Zoom = F
region = "global"
year = c(2020,2100)
map = "Areas" #options: "Areas" 
climate = "REF-RCP"

# Parameters ====
group = c("RCP6.5 (REF)","RCP2.6")
scenario = c("Multifunctional")
level = c("Baseline")
# Load the data ====
    title = paste0("Projections of future land use shares in each ecoregion")
    legend = "Area %"
    data <- read.csv(paste0("./NotRel-Wet/areas/MG/disaggregated/areas_disaggr_mg_", year[1], "_notRel-Wet.csv"))
    data$Year = year[1]
      if (length(year) > 1) {
        data_temp <- read.csv(paste0("./NotRel-Wet/areas/MG/disaggregated/areas_disaggr_mg_", year[2], "_notRel-Wet.csv"))
        data_temp$Year = year[2]
        data <- data %>% full_join(data_temp)
      }
# Rename elements ====
  data <- data %>% separate( Scenario, into = c("Group", "Scenario", "Level"), sep = "_") %>%                                       # separate the column Scenario into three columns 
              mutate(Level = str_replace(Level,"noAF", "Baseline"), Level = str_replace(Level,"AF0", "Free"),  # rename the factors in the column with Level information
                Level = str_replace(Level,"AF25", "25%"), Level = str_replace(Level,"AF50", "50%"),     
                Level = str_replace(Level,"AF75", "75%"), Level = str_replace(Level,"AF100", "100%"),
                Group = str_replace(Group, "RCP", "RCP2.6"), Group = str_replace(Group, "REF", "RCP6.5 (REF)"), Scenario = str_replace(Scenario,"MFM","Multifunctional"), Scenario = str_replace(Scenario, "SFM", "Set-aside")) #%>%                                                              # rename the rcp scenario 
# Filter the elements according to what we want to plot ====
data <- data %>% dplyr::filter(Group %in% group) %>%
                  dplyr::filter(Scenario %in% scenario) %>%
                    dplyr::filter(Level %in% level) %>%
            mutate(Year = as.character(Year))
# Sum the rows which should be aggregated ====
    data <- data %>%
      # calculate the share of each land use type
      mutate(Annual = rowSums(select(., contains("Annual")))/A_org*100, Pasture = rowSums(select(., contains("Pasture")))/A_org*100, 
             Forest = rowSums(select(., contains("For")))/A_org*100, EP = rowSums(select(., contains("EP")))/A_org*100) %>% #/A_org*100) %>%
        dplyr::select(Group, Scenario, Level, Ecoregion, Year, Annual, Pasture, Forest, EP) %>%
            pivot_longer(cols= Annual:EP, names_to = "Category", values_to = "Values") %>%
              dplyr::rename(eco_code = Ecoregion) %>%
                filter(Year == "2100" | (Year == "2020" & Group == "RCP6.5 (REF)")) # for the year 2020, select only one climate scenario
    data$Group[data$Year == "2020"] = "RCP6.5 (REF)/RCP2.6"
    data$Category <- factor(data$Category, levels = unique(data$Category))
    data$Group <- factor(data$Group, levels = c("RCP6.5 (REF)/RCP2.6", "RCP6.5 (REF)", "RCP2.6"))
#==== 
#write.csv(data, paste0("./plotting/no_cutoff/Global_Areas_REF-RCP_areas.csv"), row.names = FALSE)

df <- left_join(shp, data)

df <- df %>% filter(Scenario != "NA")

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
# ====


df <- df %>% mutate(Category = str_replace(Category, "Annual", "Annual crops"), Category = str_replace(Category, "Pasture", "Pastures"),
                    Category = str_replace(Category, "Forest", "Forests"), Category = str_replace(Category, "EP", "Energy crops and plantations"))

df$Category <- factor(df$Category, levels = c("Annual crops", "Pastures", "Forests", "Energy crops and plantations"))

test = "Oslo_mod1"
pal <- c("#f0f9e8", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081", "#1E4069", "#011959")

ptm <- proc.time()

if (length(year) > 1) {
  
  #pdf(file = paste0("./NotRel-Wet/", map, "_", climate, "_", region, "_", test, "_1.pdf"), width = 9, height = 4.5)
  png(file = paste0("./NotRel-Wet/", map, "_", climate, "_", region, "_", test,".png"), width = 9, height = 4.5, res = 1000, units = "in")

  } else{
    
  #pdf(file = paste0("./NotRel-Wet/", map, "_", climate, "_", year[1], "_", region,"_", test, ".pdf"), width = 14, height = 14)
  png(file = paste0("./NotRel-Wet/", map, "-", id,"_", climate, "_", year[1], "_", region, "_", test,".png"), width = 14, height = 14, res = 1000, units = "in")

  }

figure <- ggplot() +
            geom_sf(data = shp, fill = "transparent", colour = NA) +
              geom_sf(data = df, aes(fill = Values), colour = NA) + 
                #scale_fill_viridis() +
                #scale_fill_continuous_sequential(test, na.value = "white") +
                #scale_fill_distiller(palette = test, guide = "colourbar", direction = 1, na.value = "white") +
                # scale_fill_scico(palette = "batlow") +
                scale_fill_gradientn(colors = pal, na.value = "white") +
                labs(fill = "", x = "", y = "") + #, title = title) +
                labs(fill = legend) +
                #theme(plot.title = element_text(size = 12, face = "bold.italic")) +
                theme(panel.background = element_blank(),
                      panel.border = element_rect(colour = "black", fill = "transparent"),
                      plot.title = element_text(hjust = 0.5))

if(length(year) == 1) {figure <- figure + facet_wrap(vars(Group)) } 

if(length(year) > 1) {
  figure <- figure + facet_grid(Category ~ Year + Group, labeller = label_wrap_gen(width = 20)) + 
    theme(strip.text = element_text(size = 8)) # 
  }

if (region != "global") {figure <- figure + coord_sf(xlim=c(-15, 45), ylim=c(30, 75)) }

figure

dev.off()


 # cat.labs = c("Annual crops", "Pastures", "Managed forests", "Energy crops and plantations")
  # names(cat.labs) = c("Annual", "Pastures", "Forests", "EP")
  # group.labs = c("REF/RCP2.6", "REF", "RCP2.6")
  # names(cat.labs) = c("Base", "REF", "RCP2.6")
  # year.labs = c("2020", "2100")
  # names(year.labs) = c("year1", "year2")
 
