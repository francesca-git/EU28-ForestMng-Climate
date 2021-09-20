
########## FUNTION USED TO RE-ALLOCATE LAND USE AREAS IN THE MATCHING BETWEEN THE MAIN MODEL AND THE EU-FOREST MODEL ##########


# Title: Functions for area allocation
# Task: List of functions used in match_areas.R to prepare the ares for the biodiversity model
# Author: Francesca Rosa
# Date: started in March 2020


#library(MASS)
library(dplyr) 
library(tidyr)
library(compare)

# 
# Df = areas.av_in
# Regions = Globiom_eco_org
# For = "MngFor"
# names_lu = EPtoconvert
# names_conv = EPconverted
# Forest_intensity = data.list.steps[["Forest_intensity"]][[i]]
# Forest_intensity_bu = data.list.steps[["Forest_intensity"]][[i]]

  share_Glreg = function(Df, Regions, Columns) { #Df = a.av_temp, Regions = Globiom_eco_org, Columns = vector of names with the columns whose shares must be computed
    #Df = dataframe - its first two columns are Scenario and Ecoregion, the others contain the area per land use type
    #Regions = dataframe of two columns
    
    # Outputs:
    # 1) Df with the areas allocated at both ecoregion and Globiom level according to the share that each ecoregion has in each Globiom region. Five columns, content: scenarios, ecoregions, Globiom regions, areas, share at Globiom region of each ecoregion 
    # 2a) If Columns has length > 1 - Df that contains: Scenario, Regions, Columns grouped at Globiom Regions level, share that each column of Columns has in each ecoregion
    # 2b) If Columns has length == 1, Df that contains: Scenario, Regions, Columns grouped at Globiom Regions level, rowwise sum of Columns 
    
    Glreg_in = Df %>%
      inner_join(Regions)                               # adds two columns: one with the Glreg and the other with the relative share 
    Glreg_in$Ecoregion <- as.factor(Glreg_in$Ecoregion) 
    
    # 1) 
    Glreg_in <- Glreg_in %>%
      mutate_if(is.numeric, ~(.*Share)) %>%             # allocation of each area to each combination of Glreg region and Ecoregion, multiplying them by the Glreg Share
        mutate(Share = sqrt(Share)) 
    
    Sums_over_reg <- Glreg_in %>% 
      group_by(Scenario, Globiom_Reg) %>% 
        select(Scenario, Globiom_Reg, all_of(Columns)) %>%
          summarise_all(sum)                            # each colum contains the sums over each scenario and Glreg region for all the Columns
    
    Sums_over_reg <- data.frame(Sums_over_reg) 
    
    if (length(Columns) <= 1) {
      # 2a) 
      Shares_over_reg <- Sums_over_reg
      names(Shares_over_reg) = c("Scenario", "Globiom_Reg", paste(Columns, "Glreg_sum", sep = "_"))
      
    } else {
      # 2b)
      Sums_over_reg$Sum_tot <- rowSums(Sums_over_reg[,c(Columns)])  # sums over the Columns
      names(Sums_over_reg) = c("Scenario", "Globiom_Reg", paste(Columns, "Glreg_sum", sep = "_"), "For_Glreg_sum_tot" )
      
      Shares_over_reg <- Sums_over_reg %>%
        mutate_if(is.numeric, ~(./For_Glreg_sum_tot)) %>% # share of each column in each Scenario/Glreg
          select(-For_Glreg_sum_tot)
      names(Shares_over_reg) = c("Scenario", "Globiom_Reg", paste(Columns, "Glreg_share", sep = "_"))
      
    }
    
    
    results = list("Shares" = Shares_over_reg, "Land_use_classes_Glreg" = Glreg_in)
    return(results)
    
  }
   
  join_regions = function(Df, Regions) {
    
      Df <- Df %>% inner_join(Regions) %>% 
          mutate_if(is.numeric, ~(.*Share)) %>%                                                                     # multiplies all the areas for the relative share at Globiom region
            mutate(Share = sqrt(Share)) 
      return(Df)
    
  }
  
  sum_over_regions= function(Df, Regions) {
    Df <- join_regions(Df, Regions)
    Df <- Df %>% group_by(Scenario, Globiom_Reg) %>%  # groups by scenario and ecoregion -> no ecoregion level anymore
                    summarise_if(is.numeric, sum, na.rm =TRUE) %>%
                      arrange(Scenario, Globiom_Reg) %>%
                        select(-Share)
    Df <- data.frame(Df)
    
  } 
  
  sum_over_ecoregions = function(Df) {
    Df <- Df %>% group_by(Scenario, Ecoregion) %>%  # groups by scenario and ecoregion -> no Globiom region level anymore
                  summarise_if(is.numeric, sum) %>%
                    arrange(Scenario, Ecoregion) %>%
                      select(-Share)
    Df <- data.frame(Df)
    
  } 
  
  
  # Df = areas_EP_alloc_EU
  # Regions = Globiom_eco_org
  # Columns = "Urban"
  # OneRegion = "EU"
  # 

  separate_region = function(Df, Regions, Columns, OneRegion) {
    # Df = starting dataframe, Regions = spatial units alternative to ecoregions accoding which the separation should be done 
    # Columns = name of the column that has to be separate into different regions
    # OneRegion = name of the region for which the separation from the other regions must be performed
    # example: Regions = Globiom_regions, Columns = "Urban", OneRegion = "EU" --> this function replace the original column "Urban" with two columns: one contains values only for those ecoregions which belong to the EU region, the other only values for ecoregions that do not belong to EU or at least not entirely. 

    Dfsubset <- Df %>% 
        select(Scenario, Ecoregion, all_of(Columns))
    
    Dfsubset$new_col <- Dfsubset$Urban 
    
    name_newCol_sep = (paste0(Columns,"_",OneRegion))
    name_newCol_RoW = (paste0(Columns,"_RoW"))
    
    Dfsubset <- Dfsubset %>% 
                rename_at(vars(contains(Columns)), list(~(name_newCol_sep))) %>%
                  rename_at(vars(contains("new")), list(~(name_newCol_RoW)))
    
    Dfsubset <- join_regions(Dfsubset, Regions)
    
    Dfsubset[Dfsubset$Globiom_Reg != toString(OneRegion),3] <- 0
    Dfsubset[Dfsubset$Globiom_Reg == toString(OneRegion),4] <- 0
    
    Dfsubset <- sum_over_ecoregions(Dfsubset)
    
    fin <- Df %>% 
      select(-all_of(Columns))
    
    fin[,name_newCol_sep] = Dfsubset[,3]
    fin[,name_newCol_RoW] = Dfsubset[,4]
    
    return(fin)
    
    }
  
  
    # ALLOCATION OF MANAGED FORESTS
    
      # ALLOCATION OF NEGATIVE AREAS BELONGING TO ONE COLUMN TO THE POSITIVE AREAS OF THE SAME COLUMN. This is done allocating the areas at Globiom region.  
 
      # test = Neg_allocation(areas.av_in, Regions, For)
  
    Neg_allocation = function(Df, Regions, For) { 
      #Df = dataframe with all land use areas, ecoregions and scenarios, Regions = Globiom regions, For = Column with potential negative values
      
      #Df = areas.av_in
    
      Df_org <- Df %>%
        arrange(Scenario, Ecoregion)                                                    # back-up copy that will be merged with the results of the allocation
      
      # Selection of the involved columns (Scenario, Ecoregion and the column with negative values)
       
      Df <- Df %>%
              select(Scenario, Ecoregion, all_of(For))
      
      # This part creates a df containing only rows with positive values in Column 

        pos <- Df %>%                                                 # rows with positive values, needed to quantify the allocation factors
            filter_at(vars(matches(For)), all_vars(. > 0))            # selects positive values
        
      # If there are no negative value, the final df is equal to the initial one
        
        if(nrow(pos) == nrow(Df)) {                                   # if the number of rows of positive values is equal to the total number of rows, 
          fin <- pos                                                  # it means that there are no negative value and no modifications are needed. 
        } else {
        
      #  This part allocates the negative values of Column to the positive, considering the share at Globiom level
          
        neg_at_eco <- Df %>%                                          
          filter_at(vars(matches(For)), all_vars(. <= 0)) %>%         # filters the rows with negative or 0 values in Df (columns: Scenario, Ecoregion, Neg_areas)
            rename(Neg_areas = all_of(For))                           # renames the column containing area values to later distinguish positive and negative values
        
        # This part calculates the share of positive areas at Regions level
        
          pos_Glreg = share_Glreg(pos, Regions, For)                                  # applies the function share_Glreg, defined at the top of this file
          pos_Glreg_sums <- pos_Glreg[[1]] %>%                                        # takes the first element of the results of the function: areas grouped and summed at Globiom region (no ecoregion level anymore)
                              rename_at(vars(contains(For)), list(~ "Pos_areas"))%>%  # renames the columns containing the name of the concerned column to distinguish the positive values from the negative
                                arrange(Scenario, Globiom_Reg)                        # orders the rows according to the column Scenario and Globiom_Reg (Globiom regions) 
  
          pos_Glreg_eco_lu <- pos_Glreg[[2]]     # Df with the areas allocated at both ecoregion and Globiom level according to the share that each ecoregion has in each Globiom region
                              
          pos_Glreg_sums <- data.frame(pos_Glreg_sums)
          
          # computes the positive shares within each ecoregion at Globiom level
          share_eco_Glreg <- pos_Glreg_eco_lu %>%                                     # Df with the areas allocated at both ecoregion and Globiom level according to the share that each ecoregion has in each Globiom region
                              full_join(pos_Glreg_sums) %>%                           # joins the share at ecoregion/Globiom region level and the positive areas. Five columns, content: Scenario, Ecoregion, Globiom_reg,
                                select(-Share)                                        # Column, Pos_areas (sum of all the areas of the relative Globiom region (this column contains the same value in all the rows belonging to the same Globiom region)) 
          
          # downscaling of the share at ecoregion level
          share_eco_Glreg[,"Pos_areas_share"] <- share_eco_Glreg[,For]/share_eco_Glreg[,"Pos_areas"] # share of the positive areas of Column within each ecoregion at Globiom level (ratio of the area per ecoregion and the total area at Globiom level)
          
        # This part calculates the sum of the negative areas at Globiom level
        
          # sum of negative areas over each Globiom region
          
          neg_Glreg_sums <- Df %>% 
                        inner_join(Regions) %>%                                       # joins the Df and the shares of Globiom regions
                          mutate_if(is.numeric, ~(.*Share)) %>%                       # multiplies all the areas for the relative share at Globiom region
                            mutate(Share = sqrt(Share)) %>%
                              filter_at(vars(matches(For)), all_vars(. <= 0)) %>%     # keeps the rows with negative values
                                #select(Scenario, Globiom_Reg, For) %>%               # selects only the concerned columns
                                  group_by(Scenario, Globiom_Reg) %>%                 # groups by Scenario and Globiom region (no ecoregion level anymore) 
                                    summarise_if(is.numeric, sum) %>%                 # sums elements of each group
                                      rename(Neg_areas = all_of(For)) %>%             # renames the column to distinguish the negative values from the positive
                                        arrange(Scenario, Globiom_Reg)                # orders the rows according to the column Scenario and Globiom_Reg (Globiom regions) 
  
          neg_Glreg_sums <- data.frame(neg_Glreg_sums)
          
          # checks that there is enought positive area to allocate the negative values
          test_Glreg <- neg_Glreg_sums %>% 
                          full_join(pos_Glreg_sums) %>%
                            mutate(Neg_areas = replace_na(Neg_areas, 0)) %>%
                              mutate(Diff = Pos_areas - Neg_areas) %>%
                                filter(Diff < 0)
          
          if(nrow(test_Glreg) > 0) {print("Allocation of negative values: not enough positive areas")}

    
        # This part allocates negative values to positive areas in the ecoregions of the same Globiom region, according to the relative share
        
          allocation <- share_eco_Glreg %>%                                                         # Df with shares of positive areas at ecoregion level 
                          select(-Pos_areas) %>%                                                    # removes the column with the sum of positive areas per Globiom region
                            left_join(neg_Glreg_sums, by = c("Scenario", "Globiom_Reg")) %>%        # joins the negative sums (the column  with the negative values contains the same value in all the rows belonging to the same Globiom region)
                              mutate(Neg_areas = replace_na(Neg_areas, 0)) %>%                      # replaces na with 0: Neg_areas are na in those ecoregions where Column was not negative
                                mutate_at(vars(matches(For)), ~(. + Neg_areas*Pos_areas_share)) %>% # allocates negative areas to the positive areas
                                  group_by(Scenario, Ecoregion) %>%                                 # groups by scenario and ecoregion
                                    summarise_if(is.numeric, sum) %>%                               # sums the areas which belong to the same ecoregion but to different Globiom regions, to get rid of Globiom regions
                                      select(-Pos_areas_share, -Neg_areas, -Share)
          
          allocation <- data.frame(allocation)                       
        
        # This part sets to 0 the initially negative values and binds the rows with the new positive areas result of the allocation
        
        fin <- neg_at_eco %>%                                                                       # Df with the negative rows 
                rename_at(vars(matches("Neg_areas")), list(~ For[1])) %>%                           # renames the column with the original name
                  mutate_at(vars(matches(For)), ~(.*0)) %>%                                         # converts the negative values to 0, since they have already been allocated   
                    bind_rows(allocation)%>%                                                        # binds the rows with the new positive values  
                      arrange(Scenario, Ecoregion)
        
        fin$Ecoregion <- as.factor(fin$Ecoregion) 
        
        
        
        }
        
        fin <- Df_org %>% mutate_at(vars(all_of(For)), ~ (.= fin[,For]) )
        
        return(fin)

    }
    
    # ALLOCATION OF MANAGED FOREST TO FOREST MANAGEMENT INTENSITIES (extensive, intensive, regrowth)
    
     # Df1 = Neg_allocation(Df_in, Regions, For)
     # Df = Df1
    # 
    For_allocation = function(Df, Regions, For, Forest_intensity, Forests) {#Df = dataframe with all land use areas, ecoregions and scenarios, 
      #Regions = Globiom regions, For = Column with potential negative values, Forest_intensity = Df with the forest intensity areas per Globiom region
      #Forests = Columns of Forest_intensity to which For has to be allocated
       
      #back-up copy that will be merged with the results of the allocation
      
      Df_org <- Df %>%
        arrange(Scenario, Ecoregion)
      
      Df <- Df %>% select(Scenario, Ecoregion, all_of(For))
      
      Forest_intensity$Sum <- rowSums(Forest_intensity[,c(Forests)]) 
      names_share <- paste(Forests, "Glreg_share", sep = "_")           # names for the columns containing the share at Globiom level
      names_for <- paste("For", Forests, sep = "_")
      
      Forest_intensity[,c(names_share)] = signif(Forest_intensity[,c(Forests)]/Forest_intensity$Sum, digits = 21)   # shares of Forests in each Globiom region 
      
      Forest_intensity$Sum_check = rowSums(Forest_intensity[,names_share])
      
      #if ((length(which(Forest_intensity$Sum_check != 1))) != 0) {"The shares are not computed correctly"}         # checks that the shares are computed correctly 

      Forest_intensity_eco = Forest_intensity %>%
                              inner_join(Regions) %>%                                                       # joins the shares with the ecoregions
                                select(Globiom_Reg, Climate, all_of(names_share), Ecoregion)    
      
      Forest_intensity_eco$Globiom_Reg <- as.factor(Forest_intensity_eco$Globiom_Reg) 

      Df_Globiom = Df %>%
        inner_join(Regions) %>% 
          mutate_if(is.numeric, ~(.*Share)) %>%                                                                     # multiplies all the areas for the relative share at Globiom region
            mutate(Share = sqrt(Share)) 
      
      Df_Globiom$Ecoregion <- as.factor(Df_Globiom$Ecoregion) 
      
      Df_Globiom <- Df_Globiom %>%
                      separate(Scenario, c("Climate", "Forest_use", "Management"), "_")                             # forest intensities are classified only by climate scenario, so the column scenario must be splitted
      
      # Forest_intensity_eco contains the share of forest intensities at Globiom level, for each ecoregion (ecoregions belonging to the same Globiom regions have the same share of forest intensities)
      # Df_Globiom contains the areas at ecoregion level multiplied by the share they have at Globiom rlevel
      
      Df_fin <- Df_Globiom %>%
                  inner_join(Forest_intensity_eco)
      
      Df_fin$Globiom_Reg <- as.factor(Df_fin$Globiom_Reg) 
      Df_fin$Ecoregion <- as.factor(Df_fin$Ecoregion) 
      
      Df_fin[,c(names_share)] = signif(Df_fin[,For]*Df_fin[,c(names_share)], digits = 21)                           # allocates areas in For to new columns corresponding to Forests
      
      Df_fin <- Df_fin %>% 
                  rename_at(vars(contains("Glreg_share")), list( ~ names_for)) %>%                                  # renames the columns that contain the new areas
                    unite("Scenario", Climate:Management, remove = TRUE) 

      # This block splits the columns of For_Extensive, For_Intensive and For_Regrowth in areas belonging to EU and areas outside EU
        names_EU = paste(names_for, "EU", sep = "_")
          Df_fin[,names_EU] = Df_fin[,names_for]
            Df_fin[Df_fin$Globiom_Reg != "EU", names_EU] <- 0             # areas outside EU are set to 0
              Df_fin[Df_fin$Globiom_Reg == "EU", names_for] <- 0          # areas inside EU are set to 0
                Df_fin <- sum_over_ecoregions(Df_fin)                     # no Globiom level anymore

      Df_fin <- Df_fin %>%
              arrange(Scenario, Ecoregion) %>%
                select(-all_of(For))
      
      Df_fin <- data.frame(Df_fin)
      
      Df_fin$Ecoregion <- as.factor(Df_fin$Ecoregion)
      Df_fin$Scenario <- as.factor(Df_fin$Scenario)
      
      fin <- Df_org %>% 
        select(-all_of(For))
      
      fin[,c(names_for)] = Df_fin[,c(names_for)]
      fin[,c(names_EU)] = Df_fin[,c(names_EU)]
      
      return(fin)
      
      
    }
    
    # ALLOCATION OF ENERGY PLANTATIONS FROM CROPLANDS, GRASSLANDS AND NATURAL LANDS
    # Df1 = Neg_allocation(Df_in, Regions, For)
    # Df2 = For_allocation(Df1, Regions, For, Forest_intensity, Forests)
    # Df = Df2
    
    EP_allocation = function(Df, Regions, names_lu, names_conv) {
     # names_lu = land uses that have some negative areas and to which the negative areas have to be allocated 
     # names_conv = areas converted to energy plantation and needed to calculate the share
  
    #back-up copy that will be merged with the results of the allocation
    
    Df_org <- Df %>%
              arrange(Scenario, Ecoregion)
    
    Df <- Df %>%
      select(Scenario, Ecoregion, all_of(names_lu), all_of(names_conv))
    
      
    pos <- Df %>%                                     # rows with positive values for all land use classes, needed to quantify the allocation factors
          filter_at(names_lu, all_vars(. >= 0))
    
    
    if (nrow(pos) == nrow(Df)) {                      # if the number of rows of positive values is equal to the total number of rows, 
        fin <- Df_org                                 # it means that there are no negative value and no modifications are needed. 
    } else {  
      
      # Ecoregion level 
      
      # Allocation of negative value for CrpLnd, GrsLnd and NatLnd
      
      #  CrpLnd < 0 | GrsLnd < 0 | NatLnd < 0 
      
        neg_in <- setdiff(Df, pos)                                  # rows that contain at least a negative value for one of the land use classes
        neg_in$LU_sum = rowSums(neg_in[,c(names_lu)])               # column equal to the sum of land use areas of Crp, Grs and Nat for each ecoregion
   
        neg <- neg_in
        
        # this block creates two separate copies of lu columns, one containing negative values, the other containing positive values, and calculates the rowwise sum for both cases 
        
        names_lu_pos = paste(names_lu, "pos", sep = "_")            # names for the columns containing positive values
          neg[,names_lu_pos] = neg[,names_lu]                       # creates three other columns to store positive values
            neg[,names_lu][neg[,names_lu] > 0] <- 0                 # assigns 0 to all positive values in the original columns
              neg[,names_lu_pos][neg[,names_lu_pos] < 0] <- 0       # assigns 0 to negative values in the new columns
                neg$LU_neg_sum <- rowSums(neg[,c(names_lu)])        # sums the negative values per ecoregion
                  neg$LU_pos_sum <- rowSums(neg[,c(names_lu_pos)])  # sums the positive values per ecoregion
          
        neg <- neg %>%
                filter(LU_sum > 0)                                  # selects the rows with positive areas > negative areas 
        
        # share of land use of converted areas
        
        # this block creates a copy of the columns containing the converted land use areas, and sets to 0 the land use class which has a negative value in the corresponding broad land use class
          names_conv_pos = paste(names_conv, "pos", sep = "_")
            neg[,names_conv_pos] <- neg[,names_conv]*neg[,names_lu_pos]/neg[,names_lu_pos]
              neg[is.na(neg)] <- 0
                neg$Conv_pos_sum <- rowSums(neg[,names_conv_pos])
                
          notalloc_eco_share <- neg %>%                             # selects rows whose sum of converted areas is 0 -> here there can be Inf values when the share is computed, so the allocation cannot be done at ecoregion level
                                  filter(Conv_pos_sum <= 0)
            notalloc_eco_share[,names_lu] <- notalloc_eco_share[,names_lu] + notalloc_eco_share[,names_lu_pos]   # all the areas for the positive lu columns are used to compensate the negative values, but this causes negative values to appearin the positive columns, therefore later there will be an additional allocation at Globiom level
                notalloc_eco_share <- notalloc_eco_share %>% select(-all_of(names_lu_pos), -all_of(names_conv_pos), -Conv_pos_sum, -LU_neg_sum, -LU_pos_sum)
          
        #  CrpLnd < 0 | GrsLnd < 0 | NatLnd < 0 & sum(neg Crp, Grs, Nat) < sum(pos Crp, Grs, Nat) -> allocation within the same ecoregion
          
          names_lu_pos_share = paste(names_lu, "pos_share", sep = "_")      # names for the columns containing the share of positive values (to allocate)
          names_conv_pos_share = paste(names_conv, "share", sep = "_")      # names for the columns containing the share of positive values (converted)
          
          alloc_eco <- neg                                                                            # neg contains the negative values to allocate at ecoregion level
          alloc_eco[,names_conv_pos_share] <- alloc_eco[,c(names_conv_pos)]/alloc_eco$Conv_pos_sum    # shares of converted land use area corresponding to positive areas in the broad land use class 
          
          # this block allocated the value
          
          alloc_eco <- alloc_eco %>%
                          filter(Conv_pos_sum > 0)                                                                                                    # filters positive values of Conv_pos_sum, otherwise NA appear in the names_lu_pos columns
                            alloc_eco[,c(names_lu_pos)] <- alloc_eco[,c(names_lu_pos)] + alloc_eco[,"LU_neg_sum"]*alloc_eco[,c(names_conv_pos_share)] # allocates negative values to postive land use classes according to the share
                              alloc_eco[,names_lu] = alloc_eco[,names_lu_pos]                                                                         # the updated values are moved to the columns with the original names
                                alloc_eco <- alloc_eco %>% 
                                  select(-all_of(names_conv_pos_share), -all_of(names_lu_pos), -all_of(names_conv_pos), -Conv_pos_sum, -LU_neg_sum, -LU_pos_sum)
                                    alloc_eco$LU_sum <- rowSums(alloc_eco[,names_lu])                                                                 # new sum
         
          # this block filters the values which are still negative                                                     
          alloc_eco_add <- alloc_eco %>%                                    # filters the rows which are still negative after the first allocation             
                            filter_at(names_lu, any_vars(. < 0)) %>%
                              bind_rows(notalloc_eco_share)                 # and binds them to the rows which were previously filtered to be allocated at Globiom level

          alloc_eco <- alloc_eco %>% filter_at(names_lu, all_vars(. >= 0))  #filters rows with positive values for the broad land use classes after the first allocation

          # this block repeats the same procedure applied at the beginning of the function to separate positive and negative values in the broad land use classes
          if(nrow(alloc_eco_add) > 0) { 
            
            alloc_eco_add[,names_lu_pos] <- alloc_eco_add[,names_lu]
              alloc_eco_add[,names_lu][alloc_eco_add[,names_lu] > 0] <- 0             # assign 0 to all positive values in the original columns
                alloc_eco_add[,names_lu_pos][alloc_eco_add[,names_lu_pos] < 0] <- 0   # assign 0 to negative values in the new columns
                  alloc_eco_add$LU_neg_sum <- rowSums(alloc_eco_add[,names_lu])
                    alloc_eco_add$LU_pos_sum <- rowSums(alloc_eco_add[,names_lu_pos])
                      alloc_eco_add$LU_sum <- alloc_eco_add[,"LU_neg_sum"] + alloc_eco_add[,"LU_pos_sum"]
                      
            notalloc_eco <- alloc_eco_add %>% 
                              filter(LU_sum < 0) %>%                        # rows which cannot be allocated at ecoregion level because the first allocation still produces negative values and there is not enough positive areas in the broad land use classes
                                select(-all_of(names_lu_pos), -LU_neg_sum, -LU_pos_sum)
            
            alloc_eco_add <- alloc_eco_add %>%                              # rows which can be allocated at ecoregion level using the share of the broad land use classes instead of the share of the converted areas
                              filter(LU_sum >= 0)
            
            alloc_eco_add[,names_lu_pos_share] = alloc_eco_add[,names_lu_pos]/alloc_eco_add[,"LU_pos_sum"]                                            # calculates the share of the broad land use classes per ecoregion
            alloc_eco_add[, c(names_lu_pos)] <- alloc_eco_add[,c(names_lu_pos)] + alloc_eco_add[,"LU_neg_sum"]*alloc_eco_add[,c(names_lu_pos_share)]  # allocate of the negative values to the postive land use classes according to the share
            
            alloc_eco_add[,c(names_lu)] <- alloc_eco_add[,c(names_lu_pos)]                                                            # the updated values are moved to the columns with the original names
            alloc_eco_add <- alloc_eco_add %>% select(-all_of(names_lu_pos), -all_of(names_lu_pos_share), -LU_neg_sum, -LU_pos_sum)  
        
            alloc_eco <- alloc_eco %>%                
                          bind_rows(alloc_eco_add)               # the updated values calculated in this part are binded to those calculated previously
            
            
          } else {notalloc_eco <- alloc_eco %>% filter_at(names_lu, all_vars(. < 0)) } # empty dataframe in case there isn't any remaining negative value
          
            alloc_eco <- alloc_eco %>% 
                          select(-LU_sum)
            
         # Globiom level 
          #  CrpLnd < 0 | GrsLnd < 0 | NatLnd < 0 & sum(neg Crp, Grs, Nat) > sum(pos Crp, Grs, Nat) -> allocation within the same Globiom region according to the share of the broad land use classes and not the converted areas
          
          # Share of EP_CrpLnd/EP_GrsLnd/EP_NatLnd within the same Globiom region
            
              pos_for_Glreg <- alloc_eco %>%
                                bind_rows(pos) %>%
                                  # bind_rows(notalloc_eco_for_Glreg) %>%
                                    arrange(Scenario, Ecoregion)
              
              Glreg = share_Glreg(pos_for_Glreg, Regions, names_lu)
              Glreg_shares <- Glreg[[1]] # share of broad land use classes associated at each Globiom region
              names_Glreg_share = names(Glreg_shares)[3:length(Glreg_shares)]
              Glreg_lu <- Glreg[[2]] # dataframe containing broad land use areas and Globiom regions (with the relative share)
              
              Glreg_pos_sums <- Glreg_lu %>%
                                  select(Scenario, Ecoregion, Globiom_Reg, all_of(names_lu)) %>%
                                    group_by(Scenario, Globiom_Reg) %>%                                       # no ecoregion level anymore
                                      summarise_if(is.numeric, sum) %>%                                       # sums the positive values of broad land use at Globiom level
                                        rename_at(vars(names_lu), ~ paste(names_lu, "Glreg_sum", sep = "_"))
                    
              Glreg_pos_sums <- data.frame(Glreg_pos_sums) 
              # shares at Globiom level - this part is needed later to downscale the share from Globiom level to ecoregion level
              Glreg_shares_eco <- Glreg_lu %>%                         # ecoregions are back                                       
                                    left_join(Glreg_pos_sums)
                Glreg_shares_eco[,names_Glreg_share] <- Glreg_shares_eco[,names_lu]/Glreg_shares_eco[,paste(names_lu, "Glreg_sum", sep = "_")] # share of each land use per ecoregion within the Globiom regions

            # Ecoregion level ~ Globiom level 
            
            # share of CrpLnd/GrsLnd/NatLnd area at ecoregion level within each Globiom region
            # positive values 
              Glreg_lu_shares <- Glreg_lu %>%
                                    full_join(Glreg_shares) # share of each land use at Globiom level
            
            # values which cannot be allocated within the same ecoregion 
              if (nrow(notalloc_eco) > 0 ) {
                notalloc_eco[,names_lu] <- 0
              }
              
              neg_in <- neg_in %>% filter(LU_sum < 0) # rows with positive areas < negative areas
              
              neg_fin <- notalloc_eco %>%                       # this block binds together the rows which could not be allocated at ecoregion level
                          bind_rows(neg_in) %>%
                            mutate_at(vars(names_lu), ~(.= 0))  # the negative values are set to 0
              
              # this part sums the negative values in each Globiom region 
              toalloc_Glreg <- neg_fin %>% 
                                select(Scenario, Ecoregion, LU_sum) %>%
                                  inner_join(Regions) %>%                             # the sum of negative values at ecoregion level is the same for all the rows that belongs to the same ecoregion -> they need to be multiplied for the share at Globiom level
                                    mutate(LU_sum = LU_sum*Share) %>%                 # the sums at ecoregion level are allocated according to the share that each ecoregion has at Globiom region
                                      group_by(Scenario, Globiom_Reg) %>%             # no ecoregion level anymore
                                        select(-Ecoregion, -Share) %>%    
                                          summarise_if(is.numeric, sum) %>%           # sum of the negative values at Globiom level
                                            full_join(Glreg_shares) %>%               # share of each broad land use classes at Globiom level
                                              mutate(LU_sum = replace_na(LU_sum, 0))  # set to 0 the negative sums of the row that were added (which refer to the positive values)
              
            toalloc_Glreg[, c(paste(names_lu, "neg_to_add", sep = "_"))] = toalloc_Glreg$LU_sum*toalloc_Glreg[,names_Glreg_share]  # calculates the areas that has to be allocated per each Globiom region and per each broad land use class                            
            
            toalloc_Glreg <- toalloc_Glreg %>% select(Scenario, Globiom_Reg, c(paste(names_lu, "neg_to_add", sep = "_")))     
            
            toalloc_Glreg <- data.frame(toalloc_Glreg)
            
            neg_toGlreg <- Glreg_shares_eco %>%                                 # share of each land use per ecoregion within the Globiom regions -> downscaling at ecoregion level
              left_join(toalloc_Glreg, by = c("Scenario", "Globiom_Reg"))
            
            # allocation of negative values of Crp/Grs/Nat to the positive areas in the ecoregions of the same Globiom region, according to the relative share
            
            neg_toGlreg[,names_lu] <- neg_toGlreg[,names_lu] + neg_toGlreg[,paste(names_lu, "neg_to_add", sep = "_")]*neg_toGlreg[,names_Glreg_share]                             
            
            # this block gets rid of what is no longer necessary, including Globiom region 
            neg_toGlreg <- neg_toGlreg %>%
                            select(-all_of(paste(names_lu, "neg_to_add", sep = "_")), -all_of(names_Glreg_share), 
                                  -all_of(paste(names_lu, "Glreg_sum", sep = "_")), -Globiom_Reg, -Share) %>%
                              group_by(Scenario, Ecoregion) %>%
                                summarise_if(is.numeric, sum) # gets rid of the Globiom region
            
            neg_toGlreg <- data.frame(neg_toGlreg) 
            
            # This part merges the dataframe where the areas have been allocated and the dataframe with the ecoregion with initial negative values
            
            fin <- neg_fin %>%                  # dataframe which has only 0 in the broad land use class columns (0 replaced the negative values)
                    select(-LU_sum) %>%
                      bind_rows(neg_toGlreg)%>%
                        arrange(Scenario, Ecoregion)
                  
            fin$Ecoregion <- as.factor(fin$Ecoregion)
            
            fin_temp <- Df_org
            
            fin_temp[,c(names_lu)] <- fin[,c(names_lu)]
            
            fin <- fin_temp

      }
    
    return(fin)
  }

    
    areas_allocation = function(Df, Regions, For, Forests, Forest_intensity, names_lu, names_conv) {
      
      Df1 = Neg_allocation(Df, Regions, For)
      Df2 = For_allocation(Df1, Regions, For, Forest_intensity, Forests)
      fin = EP_allocation(Df2, Regions, names_lu, names_conv)
      
      return(fin)
      
    }
    
    
    # Df = areas.av.in[[toString(tstep[i])]]
    # Df_in = areas.av.in[["2000"]]
    # Area = "MngFor"
    # Area_subset = "sum_EUfor"
    # Receiving_area = "PriFor"
    # scenario_exclusion = c("NaN", "MFM", "noAF")
    
    Subset_allocation = function(Df, Df_in, Area, Area_subset, Receiving_area, scenario_exclusion) {
      # This function considers the following elements: a column (Area) in the input dataframe (Df), a subset of that column (Area_subset), a refence dataframe (Df_in -> referring to a previous year),
      # another column in Df (Receiving_area) to which part of Area should be allocated, the indication about which scenarios are not concerned (scenario_sxclusion).
      # The aim is to subtract from Area the corresponding subset area in the reference Df_if, and allocate the difference between the two subset areas 
      # (the one in Df and the one in Df_in) to Receiving_area
      # ! scenario_exclusion is a string vector with three elements, one for each aspect of the scenario (climate, forest use and management). If no restriction is needed for either of them,
      # write "NaN" as elements of the vector.
      
      do_test = TRUE                                         
      Df[,"Area_in_subset"] = Df_in[,Area_subset]                                                        # creates a column with the reference Area (Area_in_subset)
      Df[,"Area_new"] = Df[,Area]-Df[,"Area_in_subset"]                                                  # calculates the new Area as difference between Area and the reference Area
      areas_excluded <- Df %>% filter( (str_detect(Scenario, toString(scenario_exclusion[1]))) 
                                      | (str_detect(Scenario, toString(scenario_exclusion[2]))) 
                                      | (str_detect(Scenario, toString(scenario_exclusion[3]))))  # filters the rows which are not concerned according to given exclusion
      
      areas_excluded <- areas_excluded %>%
                          full_join(Df[Df[,Area_subset] == 0,])                                   # filters the rows which are 0 in the involved column
    
      areas_included <- dplyr::setdiff(Df, areas_excluded)                                        # filters the rows needed for the allocation
      
      areas_excluded[,Area] <- areas_excluded[,Area] - areas_excluded[,Area_subset]               # calculates the new areas as difference between the original Area and the subset
        if (do_test == TRUE) { # test 
          scenario_test = "RCP_SFM_noAF"
          ecoregion_test = "PA0608"
          test1 <- areas_excluded %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
          test2 <-Df %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test)
          test3 <- test2[,Area] - test2[,Area_subset]
          if (test3 != test1[,Area]) {print("error in the calculation for the areas not involved in the allocation")}
        # end of the test
      }
      
      areas_included[,Area] = areas_included[,"Area_new"]                                         # sets Area equal to the one calculated for the allocation process
      areas_included[,Receiving_area] = areas_included[,Receiving_area] +                         # allocates the difference between the reference Area and the subset Area to the receiving Area
                                        (areas_included[,"Area_in_subset"] - areas_included[,Area_subset])   
        if (do_test == TRUE) {
        # test 
          scenario_test = "RCP_SFM_AF0"
          ecoregion_test = "PA0404"
          test1 <- areas_included %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
          test2 <-Df %>% filter(Scenario == scenario_test, Ecoregion == ecoregion_test) 
          test3 <- test1[,Area] + test1[,Receiving_area] + test1[,Area_subset] + test2[,"Area_in_subset"]
          test4 <- test2[,Area] + test2[,Receiving_area] + test2[,"Area_in_subset"]
          if (test3 != test4) {print("error in the allocation for the involved areas")}
      }
      
      Df_fin <- areas_excluded %>% full_join(areas_included) %>%                                  # joins the areas which have undergone the allocation and those which have not
                  arrange(Scenario, Ecoregion)
      
      Df_fin <- Df_fin %>% select(-Area_in_subset, -Area_new, -all_of(Area_subset)) 
      return(Df_fin)
      
    }
    
   # test = areas_allocation(Df, Regions, For, Forests, Forest_intensity, names_lu, names_conv)
  # 
  # test1 = Neg_allocation(Df_in, Regions, For)
  # test3 = EP_allocation(test2, Regions, names_lu, names_conv)

  # 
  # test4 = colSums(fin[,3:length(fin)])
  # test4a = sum(test4)
  # 
  # test5 = colSums(Df[,3:length(Df)])
  # test5a = sum(test5)
  # 
  # test6 = colSums(Df1[,3:length(Df1)])
  # test6a = sum(test6)
  # 
  # test7 = colSums(Df2[,3:length(Df2)])
  # test7a = sum(test7)
  # 
  # test8 = colSums(neg_toGlreg[,9:25])
  # test8a = sum(test8)
  # 
  # test9 = colSums(a.av_EPtoconv_neg_toGlreg[,9:25])
  # test9a = sum(test9)
  # 
  # test10 = colSums(alloc_eco_add[,c(9:25, 27:29)])
  # test10a = sum(test10)
  # # 
  # test11 = colSums(Df[,c(9:26)])
  # test11a = sum(test11)
  # # 
  # test4a
  # test5a
  # test6a
  # test7a
  # 
  # test16 = colSums(a.av_EPtoconv_notalloc_new_eco[a.av_EPtoconv_notalloc_new_eco$Ecoregion != "PA1204",c(9:25,27:29)])
  # test16a = sum(test16)
  # 
  # test17 = colSums(alloc_eco_add[alloc_eco_add$Ecoregion != "PA1204",c(9:25,27:29)])
  # test17a = sum(test17)
  # 
  # 
  # 
  # sum(colSums(Df_in[Df_in$Ecoregion == "NT0179", 9:26]), colSums(Df_in[Df_in$Ecoregion == "OC0114", 9:26]), colSums(Df_in[Df_in$Ecoregion == "PA0425", 9:26]), colSums(Df_in[Df_in$Ecoregion == "PA0807", 9:26]))
  # 
  # test <- Df2 %>% mutate(MngFor = For_Extensive + For_Intensive + For_Regrowth)
  # relative <- test %>% mutate (rel_diff = sqrt((test$MngFor - Df1$MngFor)^2/(Df1$MngFor^2)))
  # which.max(relative$rel_diff)
  