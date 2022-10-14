


# Load biomass harvested per land use category

biomass <- read.csv(paste0(csv_path, "EUdemand_volumes_", year, file_label, "_disaggr.csv"), header = TRUE)
biomass_new <- biomass %>% pivot_wider(names_from = Category, values_from = Values) %>%
        transmute(Pathway = Pathway, Group = Group, Scenario = Scenario, EU28_internal_tot_Mm3 = rowSums(dplyr::select(., contains("EU28"))),
                  Imports_tot_Mm3 = rowSums(dplyr::select(., contains("Import"))), EU28_forests_Mm3 = rowSums(dplyr::select(., contains("EU28") & !contains("energy"))), 
                  Import_forests_Mm3 = rowSums(dplyr::select(., contains("Import") & !contains("Energy"))), Import_energy_Mm3 = rowSums(dplyr::select(., contains("Import") & contains("Energy")))) %>%
                    data.frame()

                    
impacts <- read.csv(paste0(csv_path, "EU_Footprint_", year, file_label, "_EP_disaggregated.csv"), header = TRUE) %>%
          arrange(Pathway, Group, Scenario)
impacts_new <- impacts %>% pivot_wider(names_from = Category, values_from = PDF) %>%
        transmute(Pathway = Pathway, Group = Group, Scenario = Scenario, EU28_internal_tot_pdf = rowSums(dplyr::select(., contains("EU28"))),
                  Imports_tot_pdf = rowSums(dplyr::select(., contains("Import"))), EU28_forests_pdf = rowSums(dplyr::select(., contains("EU28") & !contains("energy"))), 
                  Import_forests_pdf = rowSums(dplyr::select(., contains("Import") & !contains("Energy"))), Import_energy_pdf = rowSums(dplyr::select(., contains("Import") & contains("Energy")))) %>%
                    data.frame()


# Merge the two dataframes

df <- inner_join(biomass, impacts, by = c("Pathway", "Group", "Scenario"))

df <- df %>% transmute(Pathway = Pathway, Group = Group, Scenario = Scenario,
                        EU28_forests_ratio = EU28_forests_pdf/EU28_forests_Mm3, 
                          Import_forests_ratio = Import_forests_pdf/Import_forests_Mm3, Import_energy_ratio = Import_energy_pdf/Import_energy_Mm3,
                            Imports_tot_ratio = Imports_tot_pdf/Imports_tot_Mm3)


write.csv(df, paste0(csv_path, "PDF_Mm3_biomass-harvested_", year, file_label, ".csv"), row.names = FALSE)
