areas_in_cropped <- areas_in %>% filter(Ecoregion == "PA0412" | Ecoregion == "PA0436" | Ecoregion == "PA0501" | 
                                  Ecoregion == "PA0504" | Ecoregion == "PA0608" | Ecoregion == "PA1211" | Ecoregion == "PA1222",
                                Scenario == "RCP_MFM_AF100" | Scenario == "RCP_SFM_AF100")
View(areas_in_cropped)

areas_neg_alloc_cropped <- areas_neg_alloc %>% filter(Ecoregion == "PA0412" | Ecoregion == "PA0436" | Ecoregion == "PA0501" | 
                                  Ecoregion == "PA0504" | Ecoregion == "PA0608" | Ecoregion == "PA1211" | Ecoregion == "PA1222",
                                Scenario == "RCP_MFM_AF100" | Scenario == "RCP_SFM_AF100")
View(areas_neg_alloc_cropped)

areas_for_alloc_cropped <- areas_for_alloc %>% filter(Ecoregion == "PA0412" | Ecoregion == "PA0436" | Ecoregion == "PA0501" | 
                                  Ecoregion == "PA0504" | Ecoregion == "PA0608" | Ecoregion == "PA1211" | Ecoregion == "PA1222",
                                Scenario == "RCP_MFM_AF100" | Scenario == "RCP_SFM_AF100")
View(areas_for_alloc_cropped)