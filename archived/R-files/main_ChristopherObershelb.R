
########## Main CF calculation model  ########## 

# Author: Christopher Oberschelp
# Contact: cobersch@ethz.ch
# Last iteration: 2020-11-14
# version number: 0.01

# Description: Central file for calculating PM human health impact CFs with plume model

# Changelog:
#   2018-02-19 - Initial version created (as diffusion_2018-02-19.R)
#   2018-05-08 - Improvement of documentation, adaption to sphere, wind rose correction
#   2018-06-07 - Fixed angle calculation for points on top of each other
#   2019-07-27 - Adapted to most recent calculation functions
#   2019-09-17 - Adapted to most recent calculation functions
#   2020-11-14 - Prepared for release

######## Basic setup ########

# Start with clean workspace
rm(list = ls())

# Load packages
if(!require("foreign")){install.packages("foreign")};require("foreign")
if(!require("gdata")){install.packages("gdata")};require("gdata")
if(!require("sp")){install.packages("sp")};require("sp")
if(!require("raster")){install.packages("raster")};require("raster")
if(!require("maptools")){install.packages("maptools")};require("maptools")
if(!require("ncdf4")){install.packages("ncdf4")};require("ncdf4")  # only needed if netcdf map need to be imported
if(!require("triangle")){install.packages("triangle")};require("triangle")
if(!require("rgeos")){install.packages("rgeos")};require("rgeos")
if(!require("rgdal")){install.packages("rgdal")};require("rgdal")

# Set directory (please adjust based on local setup)
setwd("./Global_particulate_matter_life_cycle_impact_assessment_maps_v1/Code/")

######## Load and process basic data ########

source("./Supplementary_scripts/loading_and_processing_basic_data.R", echo = T)

######## Load basic functions ########

# load basic functions for handling coordinates
source("./Supplementary_scripts/spatial_functions.R", echo = T)

# load air model functions
source("./Supplementary_scripts/plume_model_functions.R")

# load CF function
source("./Supplementary_scripts/CF_function.R")

# load day length function
source("./Supplementary_scripts/day_length_calculation.R")

# load wrapper functions for calulating CFs
source("./Supplementary_scripts/wrapper_functions.R")

######## Executing functions ########

source("./Supplementary_scripts/prepare_input_output_data.R", echo = T)

###### Calculate CFs ######

# example
get_characterization_factors_DALY_per_kg(
  LAT=-6.5, 
  LONG=107, 
  emission_height_m=25, 
  emission_source_diameter_m=NA, 
  emission_exit_velocity_m_per_s=NA, 
  emission_exit_temperature_K=NA, 
  MW_generator=10, 
  emission_reference_flow_PM_2.5_kg_per_s=0.01, 
  emission_reference_flow_SO2_kg_per_s=0.01, 
  emission_reference_flow_NOx_kg_per_s=0.01,
  emission_reference_flow_NH3_kg_per_s=0.01, 
  monte_carlo_runs_amount=500, 
  urban_instead_of_rural=NA, 
  ozone_background_concentration_ppm=NA, 
  NOx_flue_gas_reference_concentration_ppm=NA,
  cutoff_radius_km=250,
  month_number=NA,
  day_instead_of_night = NA,
  simplify_by_neglecting_zero_EF_cells = F,
  speed_up_with_narrow_cone = T,
  small_instead_of_full_results = T,
  DEBUG=F)$report$characterization_factors_DALY_per_kg_emitted

# activate package for parallel computing
if(!require("parallel")){install.packages("parallel")}; require("parallel")

# multicore routine setup
if(detectCores()>24){
  amount_cores <- detectCores()-2
} else {
  amount_cores <- detectCores()-1
}

# set up cluster
cl <- parallel::makeCluster(amount_cores)

# hand over functions to the cluster
#parallel::clusterExport(cl, "spatial.get_pm_concentration_profile")
#parallel::clusterExport(cl, "ground_level_conc")
#parallel::clusterExport(cl, "ground_level_conc_raster")
parallel::clusterExport(cl, "own_unif")
parallel::clusterExport(cl, "spatial.angle_between_lines_degree")
parallel::clusterExport(cl, "spatial.angle_between_vectors_degree")
parallel::clusterExport(cl, "spatial.cartesian_to_polar")
parallel::clusterExport(cl, "spatial.distance")
parallel::clusterExport(cl, "spatial.give_bounding_box_long_lat_degree")
parallel::clusterExport(cl, "spatial.polar_to_cartesian")
parallel::clusterExport(cl, "spatial.vector_product")
parallel::clusterExport(cl, "unif_index")
parallel::clusterExport(cl, "extent")
parallel::clusterExport(cl, "colFromX")
parallel::clusterExport(cl, "rowFromY")
parallel::clusterExport(cl, "crop")
parallel::clusterExport(cl, "values<-")
parallel::clusterExport(cl, "ncell")
parallel::clusterExport(cl, "xFromCol")
parallel::clusterExport(cl, "yFromRow")
parallel::clusterExport(cl, "cellFromXY")
parallel::clusterExport(cl, "extract")
parallel::clusterExport(cl, "SpatialPoints")
parallel::clusterExport(cl, "get_characterization_factors_DALY_per_kg")
parallel::clusterExport(cl, "get_stability_class")
parallel::clusterExport(cl, "get_sy_m")
parallel::clusterExport(cl, "get_sz_m")
parallel::clusterExport(cl, "get_stack_tip_wind_speed_m_per_s")
parallel::clusterExport(cl, "get_plume_rise_from_stack_base_m")
parallel::clusterExport(cl, "get_wet_deposition_scavenging_ratio_per_s")
parallel::clusterExport(cl, "get_dry_deposition_scavenging_ratio_per_s")
parallel::clusterExport(cl, "get_concentration_kg_per_m3")
parallel::clusterExport(cl, "values")
parallel::clusterExport(cl, "extract")
parallel::clusterExport(cl, "over")
parallel::clusterExport(cl, "day_length_hours")
parallel::clusterExport(cl, "rtriangle")
parallel::clusterExport(cl, "get_molar_mass_kg_per_kmol")
parallel::clusterExport(cl, "random_daytime_solar_angle_degrees")
parallel::clusterExport(cl, "convert_ppbv_to_ug_per_m3")
parallel::clusterExport(cl, "get_reaction_rate_ratios_per_s")
parallel::clusterExport(cl, "get_ammonium_nitrate_equilibrium_concentration_kg_per_m3")
parallel::clusterExport(cl, "convert_ug_per_m3_to_ppbv")
parallel::clusterExport(cl, "get_ammonium_nitrate_equilibrium_concentration_ppbv")
parallel::clusterExport(cl, "get_dissociation_constant_ppb2")
parallel::clusterExport(cl, "get_all_characterization_factors_of_a_tile")

# hand over data files to the cluster
#parallel::clusterExport(cl, "pp_eff")
#parallel::clusterExport(cl, "EF_DALY_per_kg")
#parallel::clusterExport(cl, "elevation")
parallel::clusterExport(cl, "pm_concentration_kg_per_m3")
parallel::clusterExport(cl, "pop_2015_aggregated")
parallel::clusterExport(cl, "precipitation_frequency_days_with_rain_per_month")
parallel::clusterExport(cl, "precipitation_mm_per_month")
parallel::clusterExport(cl, "solar_radiation_kJ_per_m2_per_day")
parallel::clusterExport(cl, "tmin_c")
parallel::clusterExport(cl, "tavg_c")
parallel::clusterExport(cl, "tmax_c")
parallel::clusterExport(cl, "cloud_cover_percent")
parallel::clusterExport(cl, "average_annual_humidity_percent")
parallel::clusterExport(cl, "precipitation_frequency_days_with_rain_per_month_mean")
parallel::clusterExport(cl, "precipitation_mm_per_month_mean")
parallel::clusterExport(cl, "solar_radiation_kJ_per_m2_per_day_mean")
parallel::clusterExport(cl, "tmin_c_mean")
parallel::clusterExport(cl, "tavg_c_mean")
parallel::clusterExport(cl, "tmax_c_mean")
parallel::clusterExport(cl, "cloud_cover_percent_mean")
parallel::clusterExport(cl, "average_annual_humidity_percent_mean")
parallel::clusterExport(cl, "provinces_map")
parallel::clusterExport(cl, "EF_DALY_m3_per_kg_per_s")
parallel::clusterExport(cl, "wind_data_index")
parallel::clusterExport(cl, "zero_EF_cells")
parallel::clusterExport(cl, "urban_areas_map")
parallel::clusterExport(cl, "NOx_concentration_ground_level_ppbv")
parallel::clusterExport(cl, "O3_concentration_ground_level_ppbv")
parallel::clusterExport(cl, "NH3_background_concentration_ppbv")
parallel::clusterExport(cl, "NOx_concentration_ground_level_ppbv_mean")
parallel::clusterExport(cl, "O3_concentration_ground_level_ppbv_mean")
parallel::clusterExport(cl, "average_annual_NH3_concentration_ppbv")
parallel::clusterExport(cl, "average_dec_jan_feb_NH3_concentration_ppbv")
parallel::clusterExport(cl, "average_mar_apr_may_NH3_concentration_ppbv")
parallel::clusterExport(cl, "average_jun_jul_aug_NH3_concentration_ppbv")
parallel::clusterExport(cl, "average_sep_oct_nov_NH3_concentration_ppbv")
parallel::clusterExport(cl, "average_annual_reference_temperature_for_NH3_celsius")
parallel::clusterExport(cl, "average_dec_jan_feb_reference_temperature_for_NH3_celsius")
parallel::clusterExport(cl, "average_mar_apr_may_reference_temperature_for_NH3_celsius")
parallel::clusterExport(cl, "average_jun_jul_aug_reference_temperature_for_NH3_celsius")
parallel::clusterExport(cl, "average_sep_oct_nov_reference_temperature_for_NH3_celsius")
parallel::clusterExport(cl, "wind_direction_data")
parallel::clusterExport(cl, "wind_speed_data")

# set up parameter table
parameter_table <- get_parameter_table(
  folder_directory ="./Output",
  LAT_min_degrees = 5,
  LAT_max_degrees = 40,
  LONG_min_degrees = 65,
  LONG_max_degrees = 100,
  delta_LAT_degrees = 0.25,
  delta_LONG_degrees = 0.25,
  tile_start_number = 1,
  tile_size = 100,
  emission_height_m = NA,
  emission_source_diameter_m = NA,
  emission_exit_velocity_m_per_s = NA,
  emission_exit_temperature_K = NA, 
  MW_generator = 300, 
  emission_reference_flow_PM_2.5_kg_per_s = 1, 
  emission_reference_flow_SO2_kg_per_s = 1, 
  emission_reference_flow_NOx_kg_per_s = 1,
  emission_reference_flow_NH3_kg_per_s = 1, 
  monte_carlo_runs_amount = 200, 
  urban_instead_of_rural = NA, 
  ozone_background_concentration_ppm = NA, 
  NOx_flue_gas_reference_concentration_ppm = NA,
  cutoff_radius_km = 250,
  month_number = NA,
  day_instead_of_night = NA,
  simplify_by_neglecting_zero_EF_cells = T,
  speed_up_with_narrow_cone = T
)

# export coordinate table
parallel::clusterExport(cl, "parameter_table")

# save coordinate table
save(parameter_table, file = "./Output/Raw_data/_parameter_table.RData")

# run for all tiles in coordinate table
parallel::parSapply(cl = cl, X = sample(unique(parameter_table$tile_number), replace = F), FUN = function(i) get_all_characterization_factors_of_a_tile(parameter_table = parameter_table, tile_number = i))

# stop cluster
parallel::stopCluster(cl)

# remove variables
rm(cl, amount_cores)

###### Retrieve CF maps ######

# get raster data
raster_list <- get_rasters_from_tiles(parameter_table = parameter_table, tile_numbers = unique(parameter_table$tile_number), delta_LAT_degrees = 0.25, delta_LONG_degrees = 0.25)

# plot results to pdf file
export_rasters_to_pdf(raster_list = raster_list, directory = "./Output/pdfs", xlim=c(xmin(raster_list),xmax(raster_list)), ylim=c(ymin(raster_list),ymax(raster_list)))
export_log_rasters_to_pdf(raster_list = raster_list, directory = "./Output/pdfs", xlim=c(xmin(raster_list),xmax(raster_list)), ylim=c(ymin(raster_list),ymax(raster_list)))

dev.off()

