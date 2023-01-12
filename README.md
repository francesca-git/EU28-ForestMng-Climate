# forest-management




# Structure of the folders in the repository 

## Overview of the main folders in this repository

### /data 

Data on land use (areas) and on ecoregions (e.g., original species number) used in the model. 
It contains two subfolders: *land_use_data* and *model_parameters*

### /scripts

All scripts used in the project, except the main.R, which is in the head folder.

### /results

.csv files containing the absolute values of extinction risks for the various scenarios.

### /aggregation_plotting

Aggregated results and plots of the results.

### /archived

Folders and files used for testing and eventually not included in the final version of the manuscript.

## Detailed description of the folders

### /data

#### /data/model_parameters

###### /ecoregions_data

- .csv files providing i) the description of the ecoregions and the relative data, ii) the ecoregions included in 
GLOBIOM iii) the local Characterization factors (used to obtain the response ratios) iv) the z values to be used 
in the model.
- A *readme.md* file with a further detailed description of the .csv files.

- */z_analysis*
	- Table with the z values sent by the corresponding author of Drakare et al. (2005), *z_SAR_data_full-table.csv*
	- Table available in the SI of Drakare et al. (2005), *z_originals_Drakare _2005_SITable1.csv*
	- An .xlxs file with some tests on z, *z_analysis.xlxs*
	- Some plots.
	- A *readme.md* file describing the folder content in further detail.
- */rr_zz* (rr = response ratio)
	- .Rdata files containing the results of the calculation of rr and z values with both approaches: static (no bootstrapping) 
	and bootstrapping (bs). For rr, there are different files for rr for forest management and for the other land uses. 
	Additionally, there is a .csv file with the response ratios for forest management (as for there only global values are available).
	- A *readme.md* file describing the files in more details.
- */VS* (VS = vulnerability scores) 
	- .csv with the global and regional CFs of LC-Impact used to calculate the vulnerability scores for plants (*Global_Regional_species-lost_LCImpact.csv*). 
	- .csv files with the VS as ratio between global CFs and regional CFs (*VS_plants_LU.csv*) and the average 
	of VS per land use (*VS_plants.csv*).
A folder original-files containing the excel worksheets of the LC-Impact methodology.
A readme.txt file describing the folder content in further detail.
A folder archived


















