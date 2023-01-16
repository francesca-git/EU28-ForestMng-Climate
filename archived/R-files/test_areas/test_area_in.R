
compare_areas = function(Df, original_values) {

  tocheck = select_rows(Df)

  difference = (tocheck[,3:length(tocheck)] - original_values[,3:length(original_values)])
  relative_difference = sqrt((difference/tocheck[,3:length(tocheck)])^2)
  check_equals = all.equal(tocheck, original_values)
  comparison = setdiff(tocheck, original_values)
  sum_diff = sum(sqrt(difference^2))
  
  results = list("difference" = difference, "relative difference" = relative_difference, "check_equality" = check_equals, "comparison" = comparison, "sum of differences" = sum_diff)
  return(results)

}

select_rows = function(Df) {
  
  first_tocheck = Df[(Df$Scenario == "RCP_SFM_AF75") & (Df$Ecoregion == "PA1222"),]
  second_tocheck = Df[(Df$Scenario == "RCP_MFM_AF100") & (Df$Ecoregion == "NT0210"),]
  third_tocheck = Df[(Df$Scenario == "REF_SFM_AF25") & (Df$Ecoregion == "PA0814"),]
  fourth_tocheck = Df[(Df$Scenario == "REF_MFM_AF50") & (Df$Ecoregion == "AA0708"),]
  
  tocheck = first_tocheck %>%
    bind_rows(second_tocheck, third_tocheck, fourth_tocheck)
  
  tocheck$Scenario <- as.factor(tocheck$Scenario) # keep the Ecoregion column as factor
  tocheck$Ecoregion <- as.factor(tocheck$Ecoregion)    
  
  return(tocheck)
  
}
  
write.csv(areas.mg.in[[toString(tstep[i])]], paste0("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/extended_energy_areas_mg", tstep[i], ".csv"), row.names = FALSE)
original_values <- read.csv("C:/Users/Rosa/Documents/PhD_project/Forest_management/Recalculation_CF/to_compare_mg2100.csv", header = TRUE)

control = compare_areas(areas.mg_fin_full, original_values)

to_compare = select_rows(areas.mg_fin_full)

Eco_and_Globiom = function(Df, Regions) {
      
        Df <- Df %>% inner_join(Regions) %>% 
          mutate_if(is.numeric, ~(.*Share)) %>%                                                                     # multiplies all the areas for the relative share at Globiom region
            mutate(Share = sqrt(Share)) 
        return(Df)
        
}

sum_Globiom = function(Df, Regions) {
  Df <- Eco_and_Globiom(Df, Regions)
  Df <- Df %>% group_by(Scenario, Globiom_Reg) %>%  # groups by scenario and ecoregion
                  summarise_if(is.numeric, sum) %>%
                    arrange(Scenario, Globiom_Reg) %>%
                      select(-Share)
  Df <- data.frame(Df)
  
}

fin_Gl = sum_Globiom(fin, Regions)
fin_Gl_sample = fin_Gl[fin_Gl$Scenario == "RCP_SFM_AF100",]
  
Df_in_Gl = sum_Globiom(Df_in, Regions)
Df_in_Gl_sample = Df_in_Gl[Df_in_Gl$Scenario == "RCP_SFM_AF100",]

Df1_Gl =  sum_Globiom(Df1, Regions)
Df1_Gl_sample = Df1_Gl[Df1_Gl$Scenario == "RCP_SFM_AF100",]

Df2_Gl =  sum_Globiom(Df2, Regions)
Df2_Gl_sample = Df2_Gl[Df2_Gl$Scenario == "RCP_SFM_AF100",]
Df2_Gl_sample <- Df2_Gl_sample %>% mutate(MngFor = For_Extensive + For_Intensive + For_Regrowth)

diff_MngFor_Df1_Df2 = Df2_Gl_sample$MngFor-Df1_Gl_sample$MngFor
rel_diff = sqrt((diff_MngFor_Df1_Df2/Df1_Gl_sample$MngFor)^2)
