February 2022

Author: Francesca Rosa, ETH Zürich

# demo/

This folder is meant to be used to calculate the global extinction risk (expressed as PDF) using your areas. 

**Important**: this script is meant to calculate the global extinction risk and not the regional.

**Warning**: this demo calculation is available only for the static case, without the calculation of the confidence intervals.

The main script is *try_with_your_data.R*. 

This script takes the .csv file *areas.csv* as input and generate the .csv file *results/Global_extinction_risk_PDF.csv* as output.

To calculate the impacts with your areas, these are the steps to follow:

1. Please fill out with your data the .csv file *areas.csv*. The columns of the file are the land use categories, the rows the ecoregions. 
In each cell, enter the area of the corresponding land use per that specific ecoregion (either in Mha, km2 or m2).

**Important**: do not change the location of the file or the format, the column's names, the columns' order, the row's names, the rows' order. Just enter the numeric values into the corresponding cells.

2. Open the script *try_with_your_data.R*.

	**Important**: do not change the initial settings.  

3. Before running it, do the following things 
	- Set your working directory.
	- Make sure that the functions and parameters needed for the calculationare in the right place: 
		- data: paste0(your working directory, "/data/model_parameters/ecoregions_data/rr_z/rr_ecoregion_static.Rdata")
		- data: paste0(your working directory, "/data/model_parameters/ecoregions_data/rr_z/zvalues_static.Rdata)
		- functions: paste0(your working directory, "scripts/model/model_functions.R)
		- functions: paste0(your working directory, "scripts/model/parameters_calculation.R)
		- functions: paste0(your working directory, "demo/load_parameters.R)	

	**Important**: store the data and the functions in your working directory by creating the same paths as here above. Otherwise the functions and the data will not be called. 

4. Clean the R environment.

5. Run the whole code. 

More information are provided in the comments of *try_with_your_data.R*