rm(list = ls())
setwd("P:/surfdrive/Documents/projects/ongoing/UN-LCI/EQ/harmonization/land use/data")

# data ====
# land occupation, average approach, taxa aggregated, 2018
CF_2018 <- openxlsx::read.xlsx("Chaudhary_2018_lca_land_intensity_si_001.xlsx",
                               sheet = "Table S3 Occupation CFs",
                               startRow = 3, cols = c(1,228:242), rowNames = TRUE,
                               na.strings = "NaN", sep.names = " ")

# land occupation, average approach, taxa disaggregated, 2018
CF_2018_taxa <- openxlsx::read.xlsx("Chaudhary_2018_lca_land_intensity_si_001.xlsx",
                                    sheet = "Table S3 Occupation CFs",
                                    startRow = 3, cols = c(1,3:77), rowNames = TRUE,
                                    na.strings = "NaN", sep.names = " ")
CF_2018_3dim <- openxlsx::read.xlsx("Chaudhary_2018_lca_land_intensity_si_001.xlsx",
                                    sheet = "Table S3 Occupation CFs",
                                    startRow = 2, rows = 2, cols = 3:77, sep.names = " ")
colnames(CF_2018_3dim) <- sapply(colnames(CF_2018_3dim), function(x) substr(x, 13, gregexpr(" ", x)[[1]][2]-1))
CF_2018_taxa <- abind::abind(CF_2018_taxa[,1:15], CF_2018_taxa[,16:30], CF_2018_taxa[,31:45],
                             CF_2018_taxa[,46:60], CF_2018_taxa[,61:75], along = 3)
dimnames(CF_2018_taxa)[[3]] <- colnames(CF_2018_3dim)
rm(list = "CF_2018_3dim")

# land occupation, average approach, taxa aggregated, 2016
CF_GLAM1 <- openxlsx::read.xlsx("CFs_land_Use_average_20160717.xlsx",
                                sheet = "occupation average ecoregion",
                                startRow = 3, cols = 1:19, rowNames = TRUE, na.strings = "NaN")
CF_GLAM1 <- CF_GLAM1[,colnames(CF_GLAM1) == "Median"]
CF_GLAM1_cols <- openxlsx::read.xlsx("CFs_land_Use_average_20160717.xlsx",
                                     sheet = "occupation average ecoregion",
                                     rows = 2, cols = 2:19, sep.names = " ")
colnames(CF_GLAM1) <- colnames(CF_GLAM1_cols)
CF_GLAM1 <- CF_GLAM1 / 40 # see conversion factor described in LC-Impact documentation

# land occupation, average approach, taxa disaggregated, 2016
CF_GLAM1_taxa <- openxlsx::read.xlsx("PerTaxonAndAggregated global CF_Average_July 17th 2016.xlsx",
                                sheet = "Occupation CF Wgt",
                                startRow = 3, cols = 1:91, rowNames = TRUE, na.strings = "NaN")
CF_GLAM1_taxa <- CF_GLAM1_taxa[,colnames(CF_GLAM1_taxa) == "Median"]
CF_GLAM1_cols <- openxlsx::read.xlsx("PerTaxonAndAggregated global CF_Average_July 17th 2016.xlsx",
                                     sheet = "Occupation CF Wgt",
                                     rows = 2, cols = 2:91, sep.names = " ")
colnames(CF_GLAM1_taxa) <- colnames(CF_GLAM1_cols)
CF_GLAM1_3dim <- openxlsx::read.xlsx("PerTaxonAndAggregated global CF_Average_July 17th 2016.xlsx",
                                     sheet = "Occupation CF Wgt",
                                     rows = 1, cols = 2:91, sep.names = " ")
colnames(CF_GLAM1_3dim) <- gsub("\\s*\\([^\\)]+\\)", "", colnames(CF_GLAM1_3dim)) # remove parentheses with units
CF_GLAM1_taxa <- abind::abind(CF_GLAM1_taxa[,1:6], CF_GLAM1_taxa[,7:12], CF_GLAM1_taxa[,13:18],
                              CF_GLAM1_taxa[,19:24], CF_GLAM1_taxa[,25:30], along = 3)
dimnames(CF_GLAM1_taxa)[[3]] <- colnames(CF_GLAM1_3dim)
rm(list = c("CF_GLAM1_cols", "CF_GLAM1_3dim"))
CF_GLAM1_taxa <- CF_GLAM1_taxa[,,c("Mammals", "Birds", "Amphibians", "Reptiles", "Plants")]

# land occupation, average approach, taxa aggregated, 2015
CF_2015 <- openxlsx::read.xlsx("Chaudhary_2015_land_biodiversity_global_si_002.xlsx",
                                sheet = "Occ_average_global",
                                startRow = 6, cols = c(1,74:91), rowNames = TRUE, na.strings = "NaN")
CF_2015 <- CF_2015[,colnames(CF_2015) == "Median"]
CF_2015_cols <- openxlsx::read.xlsx("Chaudhary_2015_land_biodiversity_global_si_002.xlsx",
                                     sheet = "Occ_average_global",
                                     rows = 5, cols = 74:91, sep.names = " ")
colnames(CF_2015) <- colnames(CF_2015_cols)

# land occupation, average approach, taxa disaggregated, 2015
CF_2015_taxa <- openxlsx::read.xlsx("Chaudhary_2015_land_biodiversity_global_si_002.xlsx",
                                     sheet = "Occ_average_global",
                                     startRow = 6, cols = 1:73, rowNames = TRUE, na.strings = "NaN")
CF_2015_taxa <- CF_2015_taxa[,colnames(CF_2015_taxa) == "Median"]
CF_2015_cols <- openxlsx::read.xlsx("Chaudhary_2015_land_biodiversity_global_si_002.xlsx",
                                     sheet = "Occ_average_global",
                                     rows = 5, cols = 2:73, sep.names = " ")
colnames(CF_2015_taxa) <- colnames(CF_2015_cols)
CF_2015_3dim <- openxlsx::read.xlsx("Chaudhary_2015_land_biodiversity_global_si_002.xlsx",
                                     sheet = "Occ_average_global",
                                     rows = 4, cols = 2:73, sep.names = " ")
colnames(CF_2015_3dim) <- gsub("\\s*\\([^\\)]+\\)", "", colnames(CF_2015_3dim)) # remove parentheses with units
CF_2015_taxa <- abind::abind(CF_2015_taxa[,1:6], CF_2015_taxa[,7:12], CF_2015_taxa[,13:18],
                              CF_2015_taxa[,19:24], along = 3)
dimnames(CF_2015_taxa)[[3]] <- colnames(CF_2015_3dim)
rm(list = c("CF_2015_cols", "CF_2015_3dim"))

# taxa aggregation ====
N <- c(Mammals = 4, Birds = 4, Amphibians = 4, Reptiles = 4, Plants = 1)
S_world_2016 <- c(Mammals = 5490, Birds = 10104, Amphibians = 6433, Reptiles = 9084, Plants = 321212)
S_world_2015 <- c(Mammals = 5386, Birds = 10104, Amphibians = 6251, Reptiles = 3384, Plants = 321212)
# sum(c(Mammals = 5490, Birds = 10104, Amphibians = 6433)) # 22027
# publication in 2018: "22386 species of these three taxa"
VS_world <- c(Mammals = 0.44, Birds = 0.29, Amphibians = 0.59, Reptiles = 0.46, Plants = 1.0)

taxa_agg <- function(CF, S_world = S_world_2016) {
  weight <- 1 / (N * S_world * VS_world)
  for (i in 1:length(S_world)) {
    CF[,,i] <- CF[,,i] * weight[i]
  }
  CF <- abind::abind(CF, Animals = apply(CF[,,dimnames(CF)[[3]] != "Plants"], c(1,2), sum), along = 3)
  CF <- CF[,,c("Plants", "Animals")]
  CF <- abind::abind(CF, All = apply(CF, c(1,2), sum)/2, along = 3)
  CF
}
CF_2018_agg <- taxa_agg(CF_2018_taxa)
CF_GLAM1_agg <- taxa_agg(CF_GLAM1_taxa)
CF_2015_agg <- taxa_agg(CF_GLAM1_taxa, S_world = S_world_2015)

# filter ====
# for comparison with case study

# aggregation reproduced
# signif(CF_2018_agg[row.names(CF_2018_agg) %in% c("IM0118", "IM0120"), colnames(CF_2018_agg) == "CF_Lt crop", ], 3)
CF_2018_agg[row.names(CF_2018_agg) %in% c("IM0118", "IM0120"), colnames(CF_2018_agg) == "CF_Lt crop", ]
#          Plants  Animals      All
# IM0118 4.83e-17 2.56e-15 1.31e-15
# IM0120 1.62e-16 1.23e-15 6.94e-16

# signif(CF_GLAM1_agg[row.names(CF_GLAM1_agg) %in% c("IM0118", "IM0120"), colnames(CF_GLAM1_agg) == "Annual crops", ], 3)
CF_GLAM1_agg[row.names(CF_GLAM1_agg) %in% c("IM0118", "IM0120"), colnames(CF_GLAM1_agg) == "Annual crops", ]
#          Plants  Animals      All
# IM0118 1.74e-15 1.70e-15 1.72e-15
# IM0120 2.83e-15 1.37e-15 2.10e-15

# signif(CF_2015_agg[row.names(CF_2015_agg) %in% c("IM0118", "IM0120"), colnames(CF_2015_agg) == "Annual crops", ], 3)
CF_2015_agg[row.names(CF_2015_agg) %in% c("IM0118", "IM0120"), colnames(CF_2015_agg) == "Annual crops", ]
#          Plants  Animals      All
# IM0118 1.74e-15 2.09e-15 1.91e-15
# IM0120 2.83e-15 1.57e-15 2.20e-15

# original aggregation
CF_2018[row.names(CF_2018) %in% c("IM0118", "IM0120"), "CF_Lt crop"]
# 1.42e-13 9.78e-14

CF_GLAM1[row.names(CF_GLAM1) %in% c("IM0118", "IM0120"), "Annual crops"]
# 1.72e-15 2.10e-15

CF_2015[row.names(CF_2015) %in% c("IM0118", "IM0120"), "Annual crops"]
# 2.28e-15 1.73e-15

# taxa disaggregated
# signif(CF_2018_taxa[row.names(CF_2018_taxa) %in% c("IM0118", "IM0120"), colnames(CF_2018_taxa) == "CF_Lt crop", ], 3)
#         Mammals   Birds Amphibians Reptiles   Plants
# IM0118 4.00e-12 8.5e-12   1.86e-11 3.34e-12 1.55e-11
# IM0120 3.61e-12 7.6e-12   1.96e-12 1.24e-12 5.20e-11

# signif(CF_GLAM1_taxa[row.names(CF_GLAM1_taxa) %in% c("IM0118", "IM0120"), colnames(CF_GLAM1_taxa) == "Annual crops", ], 3)
#         Mammals    Birds Amphibians Reptiles   Plants
# IM0118 2.59e-12 6.04e-12   1.06e-11 3.63e-12 5.58e-10
# IM0120 3.51e-12 8.26e-12   2.87e-12 1.84e-12 9.08e-10

# signif(CF_2015_taxa[row.names(CF_2015_taxa) %in% c("IM0118", "IM0120"), colnames(CF_2015_taxa) == "Annual crops", ], 3)
#         Mammals    Birds Amphibians Reptiles
# IM0118 2.59e-12 6.04e-12   1.06e-11 3.63e-12
# IM0120 3.51e-12 8.26e-12   2.87e-12 1.84e-12

# comparison ====
# correlation
mapply(function(x, y) cor(x, y, use = "pairwise.complete.obs"),
       CF_GLAM1[-1,], CF_2015[-1,])
# Annual crops    Permanent crops            Pasture              Urban 
#    0.8946751          0.9238052          0.9781457          0.9519065 
# Extensive forestry Intensive forestry 
#          0.9206500          0.9023425
mapply(function(x, y) cor(x, y, use = "pairwise.complete.obs"),
       CF_GLAM1[-1, c("Annual crops", "Permanent crops", "Pasture", "Urban", "Extensive forestry", "Intensive forestry", "Extensive forestry", "Intensive forestry")],
       CF_2018[, c("CF_Lt crop", "CF_Lt crop", "CF_Lt pasture", "CF_Lt urb", "CF_RIL", "CF_clear cut", "CF_min plantation", "CF_Int plantation")])
# Annual crops      Permanent crops              Pasture 
#    0.7261296            0.7242620            0.1543001 
#     Urban   Extensive forestry   Intensive forestry 
# 0.7739368            0.6987570            0.8460463 
# Extensive forestry.1 Intensive forestry.1 
#            0.6831448            0.7771649 
mapply(function(x, y) cor(x, y, use = "pairwise.complete.obs"),
       as.data.frame(CF_GLAM1_agg[-1, , "Animals"]),
       CF_2015[-1,])
# Annual crops    Permanent crops            Pasture              Urban 
#    0.9689555          0.9701714          0.9871211          0.9783746 
# Extensive forestry Intensive forestry 
#          0.9702664          0.9792931
mapply(function(x, y) cor(x, y, use = "pairwise.complete.obs"),
       as.data.frame(CF_GLAM1_agg[-1, c("Annual crops", "Permanent crops", "Pasture", "Urban", "Extensive forestry", "Intensive forestry", "Extensive forestry", "Intensive forestry"), "All"]),
       as.data.frame(CF_2018_agg[, c("CF_Lt crop", "CF_Lt crop", "CF_Lt pasture", "CF_Lt urb", "CF_RIL", "CF_clear cut", "CF_min plantation", "CF_Int plantation"), "All"]))
# Annual crops    Permanent crops            Pasture              Urban 
#    0.7129997          0.7049182          0.1188404          0.7563070 
# Extensive forestry Intensive forestry Extensive forestry Intensive forestry 
#          0.6968528          0.8281426          0.6897609          0.7292376
mapply(function(x, y) cor(x, y, use = "pairwise.complete.obs"),
       as.data.frame(CF_GLAM1_agg[-1, c("Pasture", "Pasture", "Pasture"), "All"]),
       as.data.frame(CF_2018_agg[, c("CF_min pasture", "CF_Lt pasture", "CF_Int pasture"), "All"]))
#   Pasture   Pasture   Pasture 
# 0.1346962 0.1188404 0.1081048

# pbias
mapply(function(x, y) hydroGOF::pbias(x, y),
       CF_GLAM1[-1,], CF_2015[-1,])
# Annual crops    Permanent crops            Pasture              Urban 
#        -26.8              -33.8              -40.2              -37.2 
# Extensive forestry Intensive forestry 
#              -32.0              -25.8 
mapply(function(x, y) hydroGOF::pbias(x, y),
       CF_GLAM1[-1, c("Annual crops", "Permanent crops", "Pasture", "Urban", "Extensive forestry", "Intensive forestry", "Extensive forestry", "Intensive forestry")],
       CF_2018[, c("CF_Lt crop", "CF_Lt crop", "CF_Lt pasture", "CF_Lt urb", "CF_RIL", "CF_clear cut", "CF_min plantation", "CF_Int plantation")])
# Annual crops      Permanent crops              Pasture 
#        -99.3                -99.4                -99.5 
# Urban   Extensive forestry   Intensive forestry 
# -99.1                -98.2                -99.5 
# Extensive forestry.1 Intensive forestry.1 
#                -99.9                -99.6
mapply(function(x, y) hydroGOF::pbias(x, y),
       as.data.frame(CF_GLAM1_agg[-1, , "Animals"]),
       CF_2015[-1,])
# Annual crops    Permanent crops            Pasture              Urban 
#        -28.3              -30.0              -24.6              -25.6 
# Extensive forestry Intensive forestry 
#              -27.4              -23.9
mapply(function(x, y) hydroGOF::pbias(x, y),
       as.data.frame(CF_GLAM1_agg[-1, c("Annual crops", "Permanent crops", "Pasture", "Urban", "Extensive forestry", "Intensive forestry", "Extensive forestry", "Intensive forestry"), "All"]),
       as.data.frame(CF_2018_agg[, c("CF_Lt crop", "CF_Lt crop", "CF_Lt pasture", "CF_Lt urb", "CF_RIL", "CF_clear cut", "CF_min plantation", "CF_Int plantation"), "All"]))
# Annual crops    Permanent crops            Pasture              Urban 
#        -18.6              -38.5              -47.4                1.1 
# Extensive forestry Intensive forestry Extensive forestry Intensive forestry 
#               98.1              -42.5              -84.8              -59.5