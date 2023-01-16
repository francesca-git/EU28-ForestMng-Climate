areas_in_summary <- areas_in %>% group_by(Scenario) %>%
           summarise_if(is.numeric, sum, na.rm =TRUE) %>% data.frame()

areas_in_globiom <- share_Glreg(areas_in, Globiom_eco_org, "MngFor")[[1]]
areas_in_globiom_lu <- share_Glreg(areas_in, Globiom_eco_org, "MngFor")[[2]]

areas_in_forest <- areas_in %>% select(Scenario, Ecoregion, Clear_cut_EU, Selection_EU, Retention_EU, MngFor, sum_EUfor) %>% mutate(ratio = (MngFor-sum_EUfor)/MngFor)
areas_in_forest_globiom_lu <- areas_in_globiom_lu %>% select(Scenario, Ecoregion, Globiom_Reg, Share, Clear_cut_EU, Selection_EU, Retention_EU, MngFor, sum_EUfor) %>% mutate(ratio = (MngFor-sum_EUfor)/MngFor) %>%
  filter(sum_EUfor > 0)


areas_in_2000_summary <- areas.in[[1]] %>% group_by(Scenario) %>%
           summarise_if(is.numeric, sum, na.rm =TRUE) %>% data.frame()

areas_subset_alloc_summary <- areas_subset_alloc %>% group_by(Scenario) %>%
           summarise_if(is.numeric, sum, na.rm =TRUE) %>% data.frame()

areas_subset_alloc_globiom <- share_Glreg(areas_subset_alloc, Globiom_eco_org, "MngFor")[[1]]

areas_subset_alloc_globiom_lu <- share_Glreg(areas_subset_alloc, Globiom_eco_org, "MngFor")[[2]]

areas_neg_alloc_summary <- areas_neg_alloc %>% group_by(Scenario) %>%
           summarise_if(is.numeric, sum, na.rm =TRUE) %>% data.frame()

areas_neg_alloc_globiom <- share_Glreg(areas_neg_alloc, Globiom_eco_org, "MngFor")[[1]]

areas_neg_alloc_globiom_lu <- share_Glreg(areas_neg_alloc, Globiom_eco_org, "MngFor")[[2]]

areas_neg_alloc_forest <- areas_neg_alloc %>% select(Scenario, Ecoregion, Clear_cut_EU, Selection_EU, Retention_EU, MngFor) %>% 
  mutate(sum_EUfor = Clear_cut_EU + Selection_EU + Retention_EU, ratio = (MngFor-sum_EUfor)/MngFor) %>%
    filter(sum_EUfor > 0)

areas_post_alloc_summary <- areas_post_alloc %>% group_by(Scenario) %>%
           summarise_if(is.numeric, sum, na.rm =TRUE) %>% data.frame()

areas_for_alloc_summary <- areas_for_alloc %>% group_by(Scenario) %>%
           summarise_if(is.numeric, sum, na.rm =TRUE) %>% data.frame()

areas_for_alloc_globiom <- share_Glreg(areas_in, Globiom_eco_org, "MngFor")[[1]]

Df_fin_summary <- Df_fin %>% group_by(Scenario) %>%
           summarise_if(is.numeric, sum, na.rm =TRUE) %>% data.frame()
Df_fin_globiom <- share_Glreg(Df_fin, Globiom_eco_org, "MngFor")[[1]]

Df_summary <- Df %>% group_by(Scenario) %>%
           summarise_if(is.numeric, sum, na.rm =TRUE) %>% data.frame()
Df_globiom <- share_Glreg(Df, Globiom_eco_org, "MngFor")[[1]]

fin_summary <- fin %>% group_by(Scenario) %>%
           summarise_if(is.numeric, sum, na.rm =TRUE) %>% data.frame()
fin_globiom_ex <- share_Glreg(fin, Globiom_eco_org, "For_Extensive_EU")[[1]]
fin_globiom_in <- share_Glreg(fin, Globiom_eco_org, "For_Intensive_EU")[[1]]





Regions_EU <- Regions %>% filter(Globiom_Reg == "EU") %>% select(Ecoregion) %>% pull()

Df <- Df %>% rename(MngFor_eco = MngFor)

Df_EU_reg <- Df_Globiom %>% full_join(Df)

Df_EU_reg <- Df_EU_reg %>% filter(Ecoregion %in% Regions_EU)

MngFor_EU <- Df_EU_reg %>% filter(Globiom_Reg == "EU") %>% mutate(MngFor_EU = MngFor) %>% select(Scenario, Ecoregion, MngFor_EU)

Df_EU_reg <- Df_EU_reg %>% full_join(MngFor_EU, by = c("Ecoregion", "Scenario")) %>% 
    mutate(new_Share = MngFor/(MngFor_eco - MngFor_EU)) %>%
      mutate(MngFor = MngFor + MngFor_EU*new_Share) %>% 
        filter(Globiom_Reg != "EU" | Share == 1) %>%
          select(Scenario, Ecoregion, MngFor, Globiom_Reg, new_Share) %>% rename(Share = new_Share)
            Df_EU_reg[is.na(Df_EU_reg)] <- 0
        





