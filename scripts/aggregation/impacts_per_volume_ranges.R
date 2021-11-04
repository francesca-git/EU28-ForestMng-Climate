
# Task: Calculate the minimum and the maximum PDF/Mm3 per GLOBIOM region 
# Author: Francesca Rosa
# Date: 22 Oct. 2021

# load the .csv file containing the PDF/Mm3 per GLOBIOM region

df <- read.csv(paste0(csv_path, "PDF_Mm3_globiomregions_cutoff_bs_mg.csv"))

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

write.csv(df_group, file = paste0(csv_path, "PDF_Mm3_globiomregions_cutoff_bs_mg_ranges.csv"))


ggplot(df_group %>% filter(!is.na(forest_im_mean)), aes(x = Region, y = forest_im_mean, color = Scenario)) +
    geom_point(size = 7, alpha = 0.7) +
      theme(text = element_text(size = 45)) +
        geom_errorbar(aes(ymin = forest_im_min, ymax = forest_im_max), width = 0.0, size = 2, alpha = 0.7) +
          scale_y_log10() + 
            labs(x = "GLOBIOM region", y = "PDF*year/Mm3") +
              coord_flip()

ggsave(paste0(plots_path, "PDF-Mm3_2100_Globiom-reg", file_label, "_EP_forest_errbars.png"), width = 15, height = 30, units = "in")


ggplot(df_group %>% filter(!is.na(EP_im_mean)), aes(x = Region, y = EP_im_mean, color = Scenario)) +
    geom_point(size = 7, alpha = 0.7) +
      theme(text = element_text(size = 45)) +
        geom_errorbar(aes(ymin = EP_im_min, ymax = EP_im_max), width = 0.0, size = 2, alpha = 0.7) +
          scale_y_log10() + 
            labs(x = "GLOBIOM region", y = "PDF*year/Mm3") +
              coord_flip()

ggsave(paste0(plots_path, "PDF-Mm3_2100_Globiom-reg", file_label, "_EP_EP_errbars.png"), width = 15, height = 30, units = "in")


ggplot(df_group) +
  geom_point(aes(x = forest_im_min, y = forest_im_max, color = Region, shape = Scenario), size = 8, alpha = 0.7) +
        #scale_color_viridis_d(direction = 1) +
          labs(x = "Minimum PDF/Mm3", y = "Maximum PDF/Mm3") +
            theme(text = element_text(size = 30)) +
              scale_x_log10() + 
                scale_y_log10()

ggsave(paste0(plots_path, "PDF-Mm3_2100_Globiom-reg", file_label, "_EP_forest.pdf"), width = 18, height = 11, units = "in")
            


ggplot(df_group) +
  geom_point(aes(x = EP_im_min, y = EP_im_max, color = Region, shape = Scenario), size = 8, alpha = 0.7) +
        #scale_color_viridis_d(direction = 1) +
          labs(x = "Minimum PDF/Mm3", y = "Maximum PDF/Mm3") +
            theme(text = element_text(size = 30)) +
              scale_x_log10() + 
                scale_y_log10()

ggsave(paste0(plots_path, "PDF-Mm3_2100_Globiom-reg", file_label, "_EP_EP.pdf"), width = 18, height = 11, units = "in")
         
