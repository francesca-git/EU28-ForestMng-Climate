
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

Files:

- .csv files providing i) the description of the ecoregions and the relative data, ii) the ecoregions included in 
GLOBIOM iii) the local Characterization factors (used to obtain the response ratios) iv) the z values to be used 
in the model.
- A *readme.md* file with a further detailed description of the .csv files.

Folders:

- **z_analysis**
	- *z_SAR_data_full-table.csv*: table with the z values sent by the corresponding author of Drakare et al. (2005).
	- *z_originals_Drakare _2005_SITable1.csv*: table available in the SI of Drakare et al. (2005).
	- *z_analysis.xlxs*: .xlxs file with some tests on z.
	- Some plots.
	- *readme.md*: file describing the folder content in further detail.
- **rr_zz** (rr = response ratio)
	- .Rdata files containing the results of the calculation of rr and z values with both approaches: static (no bootstrapping) 
	and bootstrapping (bs). For rr, there are different files for rr for forest management and for the other land uses. 
	Additionally, there is a .csv file with the response ratios for forest management (as for there only global values are available).
	- *readme.md*: file describing the folder content in further detail.
- **VS** (VS = vulnerability scores) 
	- *Global_Regional_species-lost_LCImpact.csv*: .csv with the global and regional CFs of LC-Impact used to calculate the vulnerability scores for plants. 
	- *VS_plants_LU.csv*: .csv file with the VS as ratio between global CFs and regional CFs
	- *VS_plants.csv*: .csv file with the average of VS per land use.
	- *original-files/*: folder containing the excel worksheets of the LC-Impact methodology.
	- *readme.md*: file describing the folder content in further detail.
	- *archived/*: folder containing material used for testing and eventually not included in the final version of the manuscript.

#### data/land_use_data
- **areas_base** 
	- .csv files containing the land use areas sent by Fulvio di Fulvio (IIASA) which were obtained with the GLOBIOM model.
	- *analysis*: folder containing files where some data have been looked at in more detail. 
	- *OriginalFiles/*: folder containing the original files sent by IIASA with data on areas and on volume harvested.
	- *Baseline/*: folder containing raw files with data on the Baseline scenario prepared to be used in the scripts main.R through the do_tidy_match.R function.
	- *SharedEffort/*: folder containing raw files with data on the SharedEffort scenario prepared to be used in the scripts main.R through the do_tidy_match.R function.
	- *LowerIntensity/*: folder containing raw files with data on the LowerIntensity scenario prepared to be used in the scripts main.R through the do_tidy_match.R function
		(this scenario was used only for testing and is not included in the final manuscript).
	- *readme.md*: file providing an overview of the scenarios and the acronyms used in GLOBIOM.
	- *archived/*: folder containing material used for testing and eventually not included in the final version of the manuscript.
	
- **areas_processed**
	- *readme.md*: file describing the folder content in further detail.
	- .RData files: results of the tidying up process made through the function do_tidy_match.R called in main.R.
	- *notimber/Baseline/*, *notimber/SharedEffort/* and *notimber/LowerIntensity/*: folders containing the.csv files 
	resulting from the matching between the areas of the different GLOBIOM model, through the application of the function 
	do_tidy_match.R called in main.R. 
	- *timber/Baseline/*: folder containing the.csv files resulting from the matching between the areas of the different GLOBIOM model, through the application of the function 
	do_tidy_match.R called in main.R. under the “timber” setting.- *readme.md*: document that describes the content of the sub-folders.
	- *archived/*: folder containing material used for testing and eventually not included in the final version of the manuscript.
- AREAS USER


### scripts
Files:
- *readme.md*: file describing the folder content in further detail.
- *set_directories.R*: script called in main.R that creates variables containing the paths to folders/files (e.g. 
	the path to the results, to the plots, to the aggregated data, etc).
- INPUT OUTPUT FILE

Folders:
- **create_directories**: script called in main.R that creates the needed directories if they do not exist already 
	(e.g., where the results are stored).
- **data_preparation**: scripts used to tidy up and match the data on the area and to calculate the model 
	parameters used as input in the model. The input and output of these scripts are in the folder */data*
- **model**: scripts that take the output of the functions in */data_preparation* as input and calculate the 
	extinction risk per year, scenario, land use and ecoregion and to store the results in the folder */results*.
- **aggregation**: scripts that take the files in */results* as input and aggregate them (e.g. summing the 
	impacts over ecoregions). The output of these scripts is saved in */aggregation_plotting*.
- **plotting**: scripts used to plot the values of the .csv files in */results* or in */aggregation_plotting*; the 
	plots are saved in */aggregation_plotting*.

- **archived**: folder containing material used for testing and eventually not included in the final version of the manuscript.


### results
Files: 
- *readme.md*: file describing the folder content in further detail.

Folders:

The folders in this path contain the extinction risk calculated at ecoregion resolution, per each scenario and land use types usign the scrips in the folder */scripts/model* that are called in the main.R file.  
Labeling of the folders:
	- Baseline, SharedEffort or LowerIntensity: scenario to which the results belong.
	- timber = EU timber plantations have been included
	- cutoff = response ratios larger than 1 converted to 1
	- bs or static = confidence interval calculated with bootstrapping or are not calculated. 

### aggregation_plotting

Files:
- *readme.md*: file describing the folder content in further detail.

Folders:

- **areas/**: it contains the files created by the scripts aggregate_areas.R and areas_Rdata-to-csv.R and the relative plots. 
- **archived/**: folder containing material used for testing and eventually not included in the final version of the manuscript.
- The other folders containing the aggregated values and plotting of the extinction risk. Each of these folders contains multiple sub-folders corresponding to the multiple scenarios, 
where the aggregated results and the plots are stores. Each sub-folder contains:
	- an .Rdata file with the aggregated results output of the script aggregate_results.R.
	- A folder called */csv* which contains the .csv files of the aggregated results created by EU_Rdata-to-csv.R and global_Rdata-to-csv.R.
	- A folder called */plots* with the plots and maps created by the scripts in scripts/plotting.











