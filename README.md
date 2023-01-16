
## Structure of the folders in the repository 

### Overview

**data**: Data on land use (areas) and on ecoregions (e.g., original species number) used in the model. 
It contains two subfolders: *land_use_data* and *model_parameters*

**scripts**: All scripts used in the project, except the main.R, which is in the head folder.

**results**: .csv files containing the absolute values of extinction risks for the various scenarios.

**aggregation_plotting**: Aggregated results and plots of the results.

**archived**: Folders and files used for testing and eventually not included in the final version of the manuscript.

### Detailed description of the folders

### data

#### data/model_parameters/ecoregions_data

This folder contains the raw and processed data used to obtain the input parameters for the model, namely the response ratios (rr), the 
z values, the original number of species, the weighting factors and the vulnerability scores (VS).

Files:

- .csv files providing i) the description of the ecoregions and the relative data, ii) the ecoregions included in 
GLOBIOM iii) the local Characterization factors (used to obtain the response ratios) iv) the z values to be used 
in the model.
- A *readme.md* file with a further detailed description of the .csv files.

Folders (see the readme.md file in each folder for further details):

- **z_analysis**: it contains the raw files then processed to obtain the z values used as input in the model for the calculation of the impacts and some analyses. 
- **rr_zz** (rr = response ratio): it contains the data on response ratios and z values used as input in the model for the calculation of the impacts. The files result from the application of the functions in *scripts/model/parameters_calculation.R*, *scripts/model/bootstrapping.R* and
*scripts/data_preparation* to the .csv files in *data/model_parameters/ecoregions_data/*.
- **VS** (VS = vulnerability scores): it contains the raw and processed files used to obtain the VS (vulnerability scores) for plants usign the scripts *scripts/VS_plants.R*.

#### data/land_use_data

This folder contains the raw data from GLOBIOM on areas and biomass harvested, as well as the .csv files prepared
 to be used in the processing function that tidies and matches the areas (*scripts/data_preparation/do_tidy_match.R*).

Folders (see the readme.md file in each folder for further details):

- **areas_base**: it contains the raw data from GLOBIOM on areas and biomass harvested, as well as the .csv files prepared
 to be used in the processing function that tidies and matches the areas (*scripts/data_preparation/do_tidy_match.R*).
- **areas_processed**: it contains the results of the process used to match the areas of the two GLOBIOM models: the main 
model which projects global future land use and a model specific for EU internal wood and energy crops 
production and EU forest biomass import and export, as described in the maniscript. The scripts used
to produce the areas are in *scripts/data_preparation* and are called in the section "Preparation of the areas" in the file main.R

### scripts

Files:

- *readme.md*: file describing the folder content in further detail.
- *set_directories.R*: script called in main.R that creates variables containing the paths to folders/files (e.g. 
	the path to the results, to the plots, to the aggregated data, etc).
- *create_directories*: script called in main.R that creates the needed directories if they do not exist already (e.g., where the results are stored).
- *scripts_functions_input_output.xlsx*: excel file with the list of scripts, functions, data input and output.
- *scripts_functions_input_output.pdf*: .pdf file created with *scripts_functions_input_output.xlsx*.

Folders (see the readme.md file in scripts/ for further details):

- **data_preparation**: scripts used to tidy up and match the data on the area and to calculate the model 
	parameters used as input in the model. The input and output of these scripts are in the folder *data/*
- **model**: scripts that take the output of the functions in *data_preparation/* as input and calculate the 
	extinction risk per year, scenario, land use and ecoregion and to store the results in the folder *results/*.
- **aggregation**: scripts that take the files in *results/* as input and aggregate them (e.g. summing the 
	impacts over ecoregions). The output of these scripts is saved in *aggregation_plotting/*.
- **plotting**: scripts used to plot the values of the .csv files in *results/* or in *aggregation_plotting/*; the 
	plots are saved in *aggregation_plotting/*.

- **archived**: folder containing material used for testing and eventually not included in the final version of the manuscript.


### results

Files: 

- *readme.md*: file describing the folder content in further detail.

Folders:

The folders in this path contain the extinction risk calculated at ecoregion resolution, per each scenario and land use types usign the scrips in the folder */scripts/model* that are called in the main.R file.  
Labeling of the folders:
	- Baseline, SharedEffort or LowerIntensity: scenario to which the results belong.
	- timber = EU timber plantations have been included.
	- cutoff = response ratios larger than 1 converted to 1.
	- bs or static = confidence interval calculated with bootstrapping or are not calculated. 

### aggregation_plotting

Files:

- *readme.md*: file describing the folder content in further detail.

Folders:

- **areas/**: it contains the files created by the scripts *aggregate_areas.R* and *areas_Rdata-to-csv.R* and the relative plots. 
- **archived/**: folder containing material used for testing and eventually not included in the final version of the manuscript.
- The other folders containing the aggregated values and plotting of the extinction risk. Each of these folders contains multiple sub-folders corresponding to the multiple scenarios, 
where the aggregated results and the plots are stores. Each sub-folder contains:
	- an .Rdata file with the aggregated results output of the script *aggregate_results.R*.
	- A folder called *csv/* which contains the .csv files of the aggregated results created by *EU_Rdata-to-csv.R*.
	- A folder called *plots/* with the plots and maps created by the scripts in *scripts/plotting*.











