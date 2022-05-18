
############################# ARRANGE THE COLUMNS OF RATIO_ECO ############################# 

# function used to arrange the columns of ratio_eco and to put them in the same order as the columns of the dataframe with the areas
#ratio_eco - land use names
#[1] "Annual"               "Permanent"            "Pasture"              "Urban"                "PlantationNonTimber"  "ClearCut"             
# "Retention"            "PlantationFuel"       "SlashAndBurn"         "SelectionSystem"      "SelectiveLogging"    
# "Agroforestry"         "Plantation"           "ReducedImpactLogging" "Afforested"           "ForExtensive"         "ForIntensive"

name.landuse <- function() {
  
  # this function creates vectors useful to select and replicate the columns of land use types in the matrix ratio_eco according 
  # to the initial options (approach and inclusion of timber)
  
  # vectors with the maximum number of land use assessed in this analysis (union of Timber = TRUE and Approach = "AV")
  
  lu_to_assess = c("Annual",  "Permanent", "Pasture", "Urban",  "PlantationNonTimber", "ClearCut",  "Retention",  "PlantationFuel", 
                   "SlashAndBurn", "SelectionSystem", "SelectiveLogging" , "Agroforestry", "Plantation", "ReducedImpactLogging",
                   "Afforested",  "ForExtensive", "ForIntensive")
  
  return(lu_to_assess)
  
}      


############################# ALLOCATE THE IMPACTS TO THE DISAGGREGATED LAND USE CATEGORIES ############################# 

allocate.impacts <- function(df, fraction_of_areas) {
 
  df_disaggr <- df %>% 
    full_join(fraction_of_areas, by = c("Scenario", "Eco_code")) %>%
    transmute(Scenario = Scenario, Eco_code = Eco_code, Year = Year,
              Annual_EU_median = fr_Annual_EU*Annual_median, 
              Permanent_EU_median = fr_Permanent_EU*Permanent_median, 
              Pasture_EU_median = fr_Pasture_EU*Pasture_median, 
              Urban_EU_median = fr_Urban_EU*Urban_median, 
              Annual_RoW_median = fr_Annual_RoW*Annual_median, 
              Permanent_RoW_median = fr_Permanent_RoW*Permanent_median, 
              Pasture_RoW_median = fr_Pasture_RoW*Pasture_median, 
              Urban_RoW_median = fr_Urban_RoW*Urban_median,
              For_ClearCut_EU_median = fr_For_ClearCut_EU*ClearCut_median, 
              For_ClearCut_im_median = fr_For_ClearCut_im*ClearCut_median, 
              For_ClearCut_ex_median = fr_For_ClearCut_ex*ClearCut_median,
              For_Retention_EU_median = fr_For_Retention_EU*Retention_median, 
              For_Plantation_im_median = fr_For_PlantationFuel_im*PlantationFuel_median + fr_For_PlantationTimber_im*Plantation_median,
              For_TimberPlant_EU_median = fr_For_TimberPlant_EU*Plantation_median, 
              For_TimberPlant_ex_median = fr_For_TimberPlant_ex*Plantation_median, 
              For_SelectionSystem_EU_median = fr_For_SelectionSystem_EU*SelectionSystem_median,
              For_SelectionSystem_im_median = fr_For_SelectionSystem_im*SelectionSystem_median,
              For_SelectionSystem_ex_median = fr_For_SelectionSystem_ex*SelectionSystem_median,
              For_Selective_im_median = fr_For_Selective_im*SelectiveLogging_median,
              EP_EU_median = fr_EP_EU*Permanent_median, 
              EP_conv_EU_median = fr_EP_conv_EU*Permanent_median, 
              EP_RoW_median = fr_EP_RoW*PlantationFuel_median, 
              EP_conv_im_median = fr_EP_conv_im*PlantationFuel_median, 
              Afforested_EU_median = fr_Afforested_EU*Afforested_median, 
              Afforested_RoW_median = fr_Afforested_RoW*Afforested_median, 
              Regrowth_median = fr_Regrowth*Afforested_median, 
              ForOther_Extensive_EU_median = fr_ForOther_Extensive_EU*ForExtensive_median, 
              ForOther_Extensive_RoW_median = fr_ForOther_Extensive_RoW*ForExtensive_median, 
              ForOther_Intensive_EU_median = fr_ForOther_Intensive_EU*ForIntensive_median, 
              ForOther_Intensive_RoW_median = fr_ForOther_Intensive_RoW*ForIntensive_median,
              
              Annual_EU_lower95 = fr_Annual_EU*Annual_lower95, 
              Permanent_EU_lower95 = fr_Permanent_EU*Permanent_lower95, 
              Pasture_EU_lower95 = fr_Pasture_EU*Pasture_lower95, 
              Urban_EU_lower95 = fr_Urban_EU*Urban_lower95, 
              Annual_RoW_lower95 = fr_Annual_RoW*Annual_lower95, 
              Permanent_RoW_lower95 = fr_Permanent_RoW*Permanent_lower95, 
              Pasture_RoW_lower95 = fr_Pasture_RoW*Pasture_lower95, 
              Urban_RoW_lower95 = fr_Urban_RoW*Urban_lower95,
              For_ClearCut_EU_lower95 = fr_For_ClearCut_EU*ClearCut_lower95, 
              For_ClearCut_im_lower95 = fr_For_ClearCut_im*ClearCut_lower95, 
              For_ClearCut_ex_lower95 = fr_For_ClearCut_ex*ClearCut_lower95,
              For_Retention_EU_lower95 = fr_For_Retention_EU*Retention_lower95, 
              For_Plantation_im_lower95 = fr_For_PlantationFuel_im*PlantationFuel_lower95 + fr_For_PlantationTimber_im*Plantation_lower95,
              For_TimberPlant_EU_lower95 = fr_For_TimberPlant_EU*Plantation_lower95, 
              For_TimberPlant_ex_lower95 = fr_For_TimberPlant_ex*Plantation_lower95, 
              For_SelectionSystem_EU_lower95 = fr_For_SelectionSystem_EU*SelectionSystem_lower95,
              For_SelectionSystem_im_lower95 = fr_For_SelectionSystem_im*SelectionSystem_lower95,
              For_SelectionSystem_ex_lower95 = fr_For_SelectionSystem_ex*SelectionSystem_lower95,
              For_Selective_im_lower95 = fr_For_Selective_im*SelectiveLogging_lower95,
              EP_EU_lower95 = fr_EP_EU*Permanent_lower95, 
              EP_conv_EU_lower95 = fr_EP_conv_EU*Permanent_lower95, 
              EP_RoW_lower95 = fr_EP_RoW*PlantationFuel_lower95, 
              EP_conv_im_lower95 = fr_EP_conv_im*PlantationFuel_lower95, 
              Afforested_EU_lower95 = fr_Afforested_EU*Afforested_lower95, 
              Afforested_RoW_lower95 = fr_Afforested_RoW*Afforested_lower95, 
              Regrowth_lower95 = fr_Regrowth*Afforested_lower95, 
              ForOther_Extensive_EU_lower95 = fr_ForOther_Extensive_EU*ForExtensive_lower95, 
              ForOther_Extensive_RoW_lower95 = fr_ForOther_Extensive_RoW*ForExtensive_lower95, 
              ForOther_Intensive_EU_lower95 = fr_ForOther_Intensive_EU*ForIntensive_lower95, 
              ForOther_Intensive_RoW_lower95 = fr_ForOther_Intensive_RoW*ForIntensive_lower95,
              
              Annual_EU_upper95 = fr_Annual_EU*Annual_upper95, 
              Permanent_EU_upper95 = fr_Permanent_EU*Permanent_upper95, 
              Pasture_EU_upper95 = fr_Pasture_EU*Pasture_upper95, 
              Urban_EU_upper95 = fr_Urban_EU*Urban_upper95, 
              Annual_RoW_upper95 = fr_Annual_RoW*Annual_upper95, 
              Permanent_RoW_upper95 = fr_Permanent_RoW*Permanent_upper95, 
              Pasture_RoW_upper95 = fr_Pasture_RoW*Pasture_upper95, 
              Urban_RoW_upper95 = fr_Urban_RoW*Urban_upper95,
              For_ClearCut_EU_upper95 = fr_For_ClearCut_EU*ClearCut_upper95, 
              For_ClearCut_im_upper95 = fr_For_ClearCut_im*ClearCut_upper95, 
              For_ClearCut_ex_upper95 = fr_For_ClearCut_ex*ClearCut_upper95,
              For_Retention_EU_upper95 = fr_For_Retention_EU*Retention_upper95, 
              For_Plantation_im_upper95 = fr_For_PlantationFuel_im*PlantationFuel_upper95 + fr_For_PlantationTimber_im*Plantation_upper95,
              For_TimberPlant_EU_upper95 = fr_For_TimberPlant_EU*Plantation_upper95, 
              For_TimberPlant_ex_upper95 = fr_For_TimberPlant_ex*Plantation_upper95, 
              For_SelectionSystem_EU_upper95 = fr_For_SelectionSystem_EU*SelectionSystem_upper95,
              For_SelectionSystem_im_upper95 = fr_For_SelectionSystem_im*SelectionSystem_upper95,
              For_SelectionSystem_ex_upper95 = fr_For_SelectionSystem_ex*SelectionSystem_upper95,
              For_Selective_im_upper95 = fr_For_Selective_im*SelectiveLogging_upper95,
              EP_EU_upper95 = fr_EP_EU*Permanent_upper95, 
              EP_conv_EU_upper95 = fr_EP_conv_EU*Permanent_upper95, 
              EP_RoW_upper95 = fr_EP_RoW*PlantationFuel_upper95, 
              EP_conv_im_upper95 = fr_EP_conv_im*PlantationFuel_upper95, 
              Afforested_EU_upper95 = fr_Afforested_EU*Afforested_upper95, 
              Afforested_RoW_upper95 = fr_Afforested_RoW*Afforested_upper95, 
              Regrowth_upper95 = fr_Regrowth*Afforested_upper95, 
              ForOther_Extensive_EU_upper95 = fr_ForOther_Extensive_EU*ForExtensive_upper95, 
              ForOther_Extensive_RoW_upper95 = fr_ForOther_Extensive_RoW*ForExtensive_upper95, 
              ForOther_Intensive_EU_upper95 = fr_ForOther_Intensive_EU*ForIntensive_upper95, 
              ForOther_Intensive_RoW_upper95 = fr_ForOther_Intensive_RoW*ForIntensive_upper95
              
    )
  
    
      test1 <- df_disaggr %>% mutate(sum_test1 = rowSums(select(., contains("median")), na.rm = TRUE)) %>%
        select(Scenario, Eco_code, sum_test1)
      test2 <- df %>% mutate(sum_test2 = rowSums(select(.,contains("median")), na.rm = TRUE)) %>%
        select(Scenario, Eco_code, sum_test2)
      compare_df <- full_join(test1, test2, by = c("Scenario", "Eco_code")) %>%
        mutate(diff = abs(sum_test1 - sum_test2))
      
      if(vulnerability == TRUE && max(compare_df$diff, na.rm = TRUE) > 1e-15) {stop("ERROR in the allocation of disaggregated areas (median)")
        } else if(vulnerability == FALSE && max(compare_df$diff, na.rm = TRUE) > 1e-12) {stop("ERROR in the allocation of disaggregated areas (median)")}
       rm(test1, test2, compare_df)

      test1 <- df_disaggr %>% mutate(sum_test1 = rowSums(select(., contains("lower95")), na.rm = TRUE)) %>%
        select(Scenario, Eco_code, sum_test1)
      test2 <- df %>% mutate(sum_test2 = rowSums(select(.,contains("lower95")), na.rm = TRUE)) %>%
        select(Scenario, Eco_code, sum_test2)
      compare_df <- full_join(test1, test2, by = c("Scenario", "Eco_code")) %>%
        mutate(diff = abs(sum_test1 - sum_test2))
      
      if(vulnerability == TRUE && max(compare_df$diff, na.rm = TRUE) > 1e-15) {stop("ERROR in the allocation of disaggregated areas (lower 95)")
        } else if(vulnerability == FALSE && max(compare_df$diff, na.rm = TRUE) > 1e-12) {stop("ERROR in the allocation of disaggregated areas (lower 95)")}
       rm(test1, test2, compare_df)
      
      test1 <- df_disaggr %>% mutate(sum_test1 = rowSums(select(., contains("upper95")), na.rm = TRUE)) %>%
        select(Scenario, Eco_code, sum_test1)
      test2 <- df %>% mutate(sum_test2 = rowSums(select(.,contains("upper95")), na.rm = TRUE)) %>%
        select(Scenario, Eco_code, sum_test2)
      compare_df <- full_join(test1, test2, by = c("Scenario", "Eco_code")) %>%
        mutate(diff = abs(sum_test1 - sum_test2))
      
       if(vulnerability == TRUE && max(compare_df$diff, na.rm = TRUE) > 1e-15) {stop("ERROR in the allocation of disaggregated areas (upper 95)")
        } else if(vulnerability == FALSE && max(compare_df$diff, na.rm = TRUE) > 1e-12) {stop("ERROR in the allocation of disaggregated areas (uppper 95)")}
       rm(test1, test2, compare_df)
      
      df_disaggr <- df_disaggr %>% rename(Ecoregion = Eco_code)

      return(df_disaggr)
      
}



