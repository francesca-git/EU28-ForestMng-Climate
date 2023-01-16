
# Task: Calculate the minimum and the maximum PDF/Mm3 per GLOBIOM region 
# Author: Francesca Rosa
# Date: 22 Oct. 2021

# load the .csv file containing the PDF/Mm3 per GLOBIOM region

df <- read.csv(paste0(csv_path, "PDF_Mm3_globiomregions", file_label, ".csv"))

# columns:  "Scenario"       "Year"           "Region"         "forest_EU_noex" "forest_ex"      "forest_EU_ex"   "forest_im"      "EP_EU"          "EP_im"          "tot_noexport"  

# keep only the rows with values for 2100

df <- df %>% filter(Year == 2100)

df <- df %>% separate(Scenario, into = c("Group", "Scenario", "Level"), sep = "_") %>%
        select(-Scenario, -Level, -Year) %>%
          mutate(Group = str_replace(Group, "RCP", "RCP2.6"), Group = str_replace(Group, "REF", "RCP6.5")) %>%
            rename(Scenario = Group)
        
df_group <- data_frame(df %>% dplyr::group_by(Scenario, Region) %>%
              dplyr::summarise(forest_EU_noex_min = min(forest_EU_noex, na.rm = TRUE), forest_EU_noex_max = max(forest_EU_noex, na.rm = TRUE), forest_EU_noex_mean = mean(forest_EU_noex, na.rm = TRUE), 
                               forest_ex_min = min(forest_ex, na.rm = TRUE), forest_ex_max = max(forest_ex, na.rm = TRUE), forest_ex_mean = mean(forest_ex, na.rm = TRUE),
                               forest_EU_ex_min = min(forest_EU_ex, na.rm = TRUE), forest_EU_ex_max = max(forest_EU_ex, na.rm = TRUE), forest_EU_ex_mean = mean(forest_EU_ex, na.rm = TRUE),
                               forest_im_min = min(forest_im, na.rm = TRUE), forest_im_max = max(forest_im, na.rm = TRUE), forest_im_mean = mean(forest_im, na.rm = TRUE),
                               EP_EU_min = min(EP_EU, na.rm = TRUE), EP_EU_max = max(EP_EU, na.rm = TRUE), EP_EU_mean = mean(EP_EU, na.rm = TRUE), 
                               EP_im_min = min(EP_im, na.rm = TRUE), EP_im_max = max(EP_im, na.rm = TRUE), EP_im_mean = mean(EP_im, na.rm = TRUE),
                               tot_noexport_min = min(tot_noexport, na.rm = TRUE), tot_noexport_max = max(tot_noexport, na.rm = TRUE), tot_noexport_mean = mean(tot_noexport, na.rm = TRUE)))
        
df_group <- do.call(data.frame,lapply(df_group, function(x) replace(x, is.infinite(x), NA)))

write.csv(df_group, file = paste0(csv_path, "PDF_Mm3_globiomregions", file_label, "_ranges.csv"))

EU <- df_group %>% select(Scenario, Region, forest_EU_noex_min, forest_EU_noex_max, forest_EU_noex_mean, EP_EU_min, EP_EU_max, EP_EU_mean) %>%
                        rename(forest_min = forest_EU_noex_min, forest_max = forest_EU_noex_max, forest_mean = forest_EU_noex_mean,
                              EP_min = EP_EU_min, EP_max = EP_EU_max, EP_mean = EP_EU_mean) %>%
                          filter(Region == "EU")

df_temp <- df_group %>% select(Scenario, Region, contains("im"))

df_temp <- df_temp %>% rename(forest_min = forest_im_min, forest_max = forest_im_max, forest_mean = forest_im_mean,
                              EP_min = EP_im_min, EP_max = EP_im_max, EP_mean = EP_im_mean) %>% filter(Region != "EU")
EU <- EU %>% mutate(Region = str_replace(Region, "EU", "EU*"))

df_temp <- df_temp %>% bind_rows(EU) 

df_temp$Region  <- as.character(df_temp$Region )
df_temp$Region <- factor(df_temp$Region, levels = unique(df_temp$Region))

figure1 <- ggplot(df_temp %>% filter(!is.na(forest_mean)), aes(x = Region, y = forest_mean, color = Scenario)) +
    geom_point(size = 7, alpha = 0.7) +
      theme(text = element_text(size = 45)) +
        geom_errorbar(aes(ymin = forest_min, ymax = forest_max), width = 0.0, size = 2, alpha = 0.7) +
          scale_y_log10() + 
            labs(x = "GLOBIOM region", y = "PDF*year/Mm3") +
              coord_flip()

ggsave(filename = paste0(plots_path, "PDF-Mm3_2100_Globiom-reg", file_label, "_EP_forest_errbars_EU.png"), plot = figure1, width = 15, height = 30, units = "in")


figure2 <- ggplot(df_temp %>% filter(!is.na(EP_mean)), aes(x = Region, y = EP_mean, color = Scenario)) +
    geom_point(size = 7, alpha = 0.7) +
      theme(text = element_text(size = 45)) +
        geom_errorbar(aes(ymin = EP_min, ymax = EP_max), width = 0.0, size = 2, alpha = 0.7) +
          scale_y_log10() + 
            labs(x = "GLOBIOM region", y = "PDF*year/Mm3") +
              coord_flip()

ggsave(filename = paste0(plots_path, "PDF-Mm3_2100_Globiom-reg", file_label, "_EP_EP_errbars_EU.png"), plot = figure2, width = 15, height = 30, units = "in")


# ggplot(df_temp) +
#   geom_point(aes(x = forest_min, y = forest_max, color = Region, shape = Scenario), size = 8, alpha = 0.7) +
#         #scale_color_viridis_d(direction = 1) +
#           labs(x = "Minimum PDF/Mm3", y = "Maximum PDF/Mm3") +
#             theme(text = element_text(size = 30)) +
#               scale_x_log10() + 
#                 scale_y_log10()
# 
# ggsave(paste0(plots_path, "PDF-Mm3_2100_Globiom-reg", file_label, "_EP_forest_EU.pdf"), width = 18, height = 11, units = "in")
#             
# 
# 
# ggplot(df_temp) +
#   geom_point(aes(x = EP_min, y = EP_max, color = Region, shape = Scenario), size = 8, alpha = 0.7) +
#         #scale_color_viridis_d(direction = 1) +
#           labs(x = "Minimum PDF/Mm3", y = "Maximum PDF/Mm3") +
#             theme(text = element_text(size = 30)) +
#               scale_x_log10() + 
#                 scale_y_log10()
# 
# ggsave(paste0(plots_path, "PDF-Mm3_2100_Globiom-reg", file_label, "_EP_EP_EU.pdf"), width = 18, height = 11, units = "in")
#          
