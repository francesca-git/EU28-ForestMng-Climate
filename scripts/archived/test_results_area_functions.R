

load("./data/land_use_data/areas_processed/areas-to-match.RData")
old <- data.list.steps
names(old)

load("./data/land_use_data/areas_processed/areas-to-match_Baseline.RData")
new <- data.list.steps
names(new)

all.equal(new[["Import_forest"]][["2100"]], old[["Import_forest_mg"]][["2100"]])
all.equal(new[["Import_energy_plant"]][["2100"]], old[["Import_energy_plant"]][["2100"]])
all.equal(new[["EU_forest"]][["2100"]], old[["EU_forest"]][["2100"]])
all.equal(new[["EU_energy_plant"]][["2100"]], old[["EU_energy_plant"]][["2100"]])
all.equal(new[["Export_forest"]][["2100"]], old[["Export_forest_mg"]][["2100"]])
all.equal(new[["Broad_land_use"]][["2100"]], old[["Broad_land_use"]][["2100"]])
all.equal(new[["Forest_intensity"]][["2100"]], old[["Forest_intensity"]][["2100"]])


load("./data/land_use_data/areas_processed/areas-to-match.RData")
old <- data.list.steps
names(old)

load("./data/land_use_data/areas_processed/areas-to-match_LowerIntensity.RData")
new <- data.list.steps
names(new)

all.equal(new[["Import_forest"]][["2100"]], old[["Import_forest_av"]][["2100"]])
all.equal(new[["Import_energy_plant"]][["2100"]], old[["Import_energy_plant"]][["2100"]])
all.equal(new[["EU_forest"]][["2100"]], old[["EU_forest"]][["2100"]])
all.equal(new[["EU_energy_plant"]][["2100"]], old[["EU_energy_plant"]][["2100"]])
all.equal(new[["Export_forest"]][["2100"]], old[["Export_forest_av"]][["2100"]])
all.equal(new[["Broad_land_use"]][["2100"]], old[["Broad_land_use"]][["2100"]])
all.equal(new[["Forest_intensity"]][["2100"]], old[["Forest_intensity"]][["2100"]])


load("./data/land_use_data/areas_processed/areas-to-match.RData")
old <- data.list.steps
names(old)

load("./data/land_use_data/areas_processed/areas-to-match_LowerIntensity.RData")
new <- data.list.steps
names(new)

all.equal(new[["Import_forest"]][["2100"]], old[["Import_forest_av"]][["2100"]])


new_aggr <- read.csv(paste0(areas_processed_path, "aggregated/areas_aggr_Baseline_2100.csv"))
old_aggr <- read.csv("./data/land_use_data/areas_processed/archived/notimber_old/mg/aggregated/areas_aggr_mg_2100.csv")
new_disaggr <- read.csv(paste0(areas_processed_path, "disaggregated/areas_disaggr_Baseline_2100.csv"))
old_disaggr <- read.csv("./data/land_use_data/areas_processed/archived/notimber_old/mg/disaggregated/areas_disaggr_mg_2100.csv")
new_fr <- read.csv(paste0(areas_processed_path, "fraction/fr_areas_disaggr_Baseline_2100.csv"))
old_fr <- read.csv("./data/land_use_data/areas_processed/archived/notimber_old/mg/fraction/fr_areas_disaggr_mg_2100.csv")

all_equal(old_aggr, new_aggr)
all_equal(old_disaggr, new_disaggr)
all_equal(old_fr, new_fr)


new_aggr <- read.csv(paste0(areas_processed_path, "aggregated/areas_aggr_SharedEffort_2100.csv"))
old_aggr <- read.csv("./data/land_use_data/areas_processed/archived/notimber_old/av-LII/aggregated/areas_aggr_av_2100.csv")
new_disaggr <- read.csv(paste0(areas_processed_path, "disaggregated/areas_disaggr_SharedEffort_2100.csv"))
old_disaggr <- read.csv("./data/land_use_data/areas_processed/archived/notimber_old/av-LII/disaggregated/areas_disaggr_av_2100.csv")
new_fr <- read.csv(paste0(areas_processed_path, "fraction/fr_areas_disaggr_SharedEffort_2100.csv"))
old_fr <- read.csv("./data/land_use_data/areas_processed/archived/notimber_old/av-LII/fraction/fr_areas_disaggr_av_2100.csv")

all_equal(old_aggr, new_aggr)
all_equal(old_disaggr, new_disaggr)
all_equal(old_fr, new_fr)


new_aggr <- read.csv(paste0(areas_processed_path, "aggregated/areas_aggr_LowerIntensity_2100.csv"))
old_aggr <- read.csv("./data/land_use_data/areas_processed/archived/notimber_old/av/aggregated/areas_aggr_av_2100.csv")
new_disaggr <- read.csv(paste0(areas_processed_path, "disaggregated/areas_disaggr_LowerIntensity_2100.csv"))
old_disaggr <- read.csv("./data/land_use_data/areas_processed/archived/notimber_old/av/disaggregated/areas_disaggr_av_2100.csv")
new_fr <- read.csv(paste0(areas_processed_path, "fraction/fr_areas_disaggr_LowerIntensity_2100.csv"))
old_fr <- read.csv("./data/land_use_data/areas_processed/archived/notimber_old/av/fraction/fr_areas_disaggr_av_2100.csv")

all_equal(old_aggr, new_aggr)
all_equal(old_disaggr, new_disaggr)
all_equal(old_fr, new_fr)



new_aggr <- read.csv(paste0(areas_processed_path, "aggregated/areas_aggr_Baseline_2090_timber.csv"))
old_aggr <- read.csv("./data/land_use_data/areas_processed/archived/timber_baseline_old/aggregated/areas_aggr_mg_2090_timber.csv")
new_disaggr <- read.csv(paste0(areas_processed_path, "disaggregated/areas_disaggr_Baseline_2090_timber.csv"))
old_disaggr <- read.csv("./data/land_use_data/areas_processed/archived/timber_baseline_old/disaggregated/areas_disaggr_mg_2090_timber.csv")
new_fr <- read.csv(paste0(areas_processed_path, "fraction/fr_areas_disaggr_Baseline_2090_timber.csv"))
old_fr <- read.csv("./data/land_use_data/areas_processed/archived/timber_baseline_old/fraction/fr_areas_disaggr_mg_2090_timber.csv")

all_equal(old_aggr, new_aggr)
all_equal(old_disaggr, new_disaggr)
all_equal(old_fr, new_fr)


new_slost <- read.csv(paste0(results_path, "/Slost_2100", file_label, ".csv"))
old_slost <- read.csv(paste0("./results/20221020/species-lost_cutoff_static_mg/Slost_2100_cutoff_static_mg.csv"))
all_equal(old_slost, new_slost)


new_slost <- read.csv(paste0(results_path, "/Slost_2100", file_label, ".csv"))
old_slost <- read.csv(paste0("./results/20221020/species-lost_cutoff_static_av-LII/Slost_2100_cutoff_static_av-LII.csv"))
all_equal(old_slost, new_slost)

