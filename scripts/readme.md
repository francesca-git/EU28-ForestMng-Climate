October 2022

Author: Francesca Rosa, ETH Zürich

### scripts

#### Files:

- *readme.md*: file describing the folder content in further detail.
- *set_directories.R*: script called in main.R that creates variables containing the paths to folders/files (e.g. 
	the path to the results, to the plots, to the aggregated data, etc).
- *create_directories*: script called in main.R that creates the needed directories if they do not exist already 
	(e.g., where the results are stored).
- *scripts_functions_input_output.xlsx*: excel file with the list of scripts, functions, data input and output.
- *scripts_functions_input_output.pdf*: .pdf file created with *scripts_functions_input_output.xlsx*.

#### Folders:

- **data_preparation**: the scripts in this folder are used to prepare the land use areas from GLOBIOM, the 
z values from Drakare et al. (2005) and the vulnerability scores (VS) to be used in the extinction risk model (*scripts/model*). 
The other parameters needed in the model are already available in the .csv files in *data/model_parameters/ecoregions_data* (*ecoregions_data.csv*,
*CF_local.csv*, *CF_local_forest-use.csv*) and do not need and additional manipulation. 
	- *do_tidy_match.R*: it prepares the land use data to be used as input in the extinction risk model (*scripts/model*). 
	This script calls the function *tidy.areas* from *tidy_areas.R* and *match.areas* from *match_areas.R*. 
	Used in *main.R*.
	- *tidy_areas.R*: it tidies up the original areas and convert them in a .Rdata file to be used as input for *match_areas.R*. Used in *tidy_match_areas.R*. 
	- *match_areas.R*: it uses the .Rdata file produced in *tidy.areas* as input and matches the areas of the 
	two GLOBIOM models (global and EU). The output of this script are .csv files containing the areas per land 
	use type per year (each year is a separate file). Used in *tidy_match_areas.R*.
	- *areas_functions.R*: functions used in *match.areas*.
	- *z_values.R*: it explores and selects the z values from Drakare et al. (2005) to make them ready 
	to be used in the extinction risk model (*scripts/model*). The output of this script is *data/model_parameters/ecoregions_data/z_input-values.csv*.
	- *VS_plants.R*: it calculates the VS for plants from the global and regional CFs of LCImpact, 
	as explained in *data/model_parameters/ecoregions_data/VS*. 

- **model**: scripts that take the output of the functions in *data_preparation/* as input and calculate the 
	extinction risk per year, scenario, land use and ecoregion and to store the results in the folder *results/*.
	**Warning**: for the details of which function is defined in which script and which input and output are needed and produced in each script, please see the pdf file *model/scripts_functions_input_output.pdf*
	- *calculate_impact.R*: main script, which calls all the other functions and is the only one to be ran. Used in *main.R*.
	- *calculate_slost.R*: it contains the main function of the model (*calculate.slost*). Used in *calculate_impacts.R*.
	- *allocate_impacts.R*: it contains the function *name.landuse* (it creates a character vector with the names of the columns of the different land use types) 
	and the function *allocate.impacts* (it allocates the extinction risk calculated with the model to the disaggregated land use classification). Used in *calculate_impacts.R*.
	- *paramenters_calculation.R*: functions which prepare or calculate the parameters needed in the model (e.g., the response ratios, the z values, the weighting factors). Used in *bootstrapping.R* and to calculate the input for *calculate_impacts.R*. 
	WARNING: this function does not need to be run, as its output have already been calculated and are stored in data/model_parameters/ecoregions_data/rr_z.
	- *bootstrapping.R*: functions needed to perform the computation of confidence intervals with bootstrapping. Some functions used in this R file are defined in *parameters_calculation.R*. Used to calculate the input for *calculate_impacts.R*. 
	WARNING: this function does not need to be run, as its output have already been calculated and are stored in data/model_parameters/ecoregions_data/rr_z.
	- *model_functions.R*: multiple functions called in the various scripts of the model.
	- *distributions.R*: function used to test the distributions of the response ratios and define the parametrization. *Used in parameters_calculation.R*
 
Structure of the scripts used for the model:
		

- **aggregation**: scripts that take the files in *results/* as input and aggregate them (e.g. summing the 
	impacts over ecoregions). The output of these scripts is saved in *aggregation_plotting/*.

	- *aggregate_results.R* and *aggregate_areas.R*: they sum the extinction risks in *results/* and the areas in *data/land_use_data/areas_processed* over ecoregion, scenario and 
	land uses at different level. The output are .Rdata files saved in the folder *aggregation_plotting/*. Used in *main.R*. 
 	- *EU_Rdata-to-csv.R* and *areas_Rdata-to-csv.R*: they convert the .Rdata files created using *aggregate_areas.R* and *aggregate_results.R* to separate .csv files and save 
	them in *aggregation_plotting/*. Used in *main.R*. 
	- *volume_per_category.R* and *volume_per_category_oneyear.R*: they calculate the biomass harvested (Mm3 of roundwood equivalent)
	in each scenario, year and per grouped land use categories. The input data for these scripts are in *data/land_use_data/areas_base*. 
	*volume_per_category_oneyear.R* was used to produce the values plotted in Figure S13.
	**Warning**: *volume_per_category.R* can be used only for the Baseline scenario and its output were not included in the final manuscript. 
	- *impacts_per_volumne.R* and *impacts_per_volume_oneyear.R*: they convert the results expressed as 
	PDF to PDF per Mm3 of roundwood equivalent. The input data for these scripts are the .csv files in *results* and in *data/land_use_data/areas_base*.
	Used in *main.R* to. *impacts_per_volume_oneyear.R* was used to produced the data in Table S15.1. *impacts_per_volume.R* was used to produced the data in Table S18.1.
	**Warning**: *impacts_per_volume.R* can be used only for the Baseline scenario. 
	- *convert_to_GLOBIOMres.R*: it converts the results at ecoregion resolution to GLOBIOM resolution. The input data for this script are the .csv files in *results/*. Used in *main.R*.
	- *convert_to_PDFha.R*: it converts the results expressed as PDF to PDF per hectare. The input data for this script are the .csv files in *results/*. 
	**Warning**: its output were not included in the final manuscript.


- **plotting**: scripts used to plot the values of the .csv files in *results/* or in *aggregation_plotting/*; the 
	plots are saved in *aggregation_plotting/*.

	- *EU_barplots.R*: it is called in *main.R* and it contains the functions used to plot the barplots of the impacts (PDF) of the EU internal harvest for EU forest biomass demand,
	the EU footprint, the EU forest biomass demand (Mm3), the areas used to produced the forest biomass to meet the EU demand (Mha). The plots distinguish between
	the various forestry practices. This script was used to produced the plots then combined to obtain Figure 1 and to produce Figures in S12 and S13.
	- *plot_EU_time_series.R*: it is called in *main.R* and it contains the functions used to plot the time series of the impacts (PDF) for EU internal harvest for EU forest biomass demand and 
	the EU footprint. This scripts was used to produced the Figures in S14.
	- *plot_all_maps.R*: it plots all the maps published in the manuscript, both main text and SI. It uses the script in *plot_map.R*.
	- *plot_map.R*: it is called in *plot_all_maps.R* and it contains the function that can plot the global or EU map at ecoregion resolution.
	- *impacts_per_volume_ranges.R*: script that plots the ranges per GLOBIOM region of the impacts per volume of biomass harvested (PDF/Mm3), as in the Figures in S18.
	- *maps_shapefiles/*: it contains the shapefiles of the global ecoregion, of the EU map and of the GLOBIOM regions.
	**Important**: the map of the ecoregions is not uploaded to the github repository as it is not owned by the authors. It can be downloaded here.
	https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world. Once the files have been downloaded, they can be stored in 
	the folder *maps_shapefiles/* by creating a subfolder *WWF_Ecoregions/* and then used for plotting.  

- **archived**: folder containing material used for testing and eventually not included in the final version of the manuscript.

