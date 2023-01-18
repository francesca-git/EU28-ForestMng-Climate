August 2021

Author: Francesca Rosa, ETH Zürich

# data/model_parameters/ecoregions_data/rr_z

This folder contains the data on response ratios and z values that result from the application of the functions in *scripts/model/parameters_calculation* and
*scripts/data_preparation* to the .csv files in *data/model_parameters/ecoregions_data/*.

Folders:

- **archived**: folder containing material used for testing and eventually not included in the final version of the manuscript.

Files:

- The .Rdata files with the calculated response ratios (rr) and z values for the different 
	options considered in the study: without bootstrapping (static, no confidence intervals) and with bootstrapping (with confidence intervals). 
	These files are the output of the function calculate.RR in scripts/model/parameters_calculation.R (those labelled as “static”) and of 
	calculate.RR.bs in scripts/model/bootstrapping.R. For rr, there are different files for rr for forest management and for the other land uses.
	*Important*: two of the files, obtained with the bootstrapping procedure (*rr_ecoregion_bs.Rdata* and *zvalues_ecoregion_bs.Rdata*) and used 
	in the model (in *scripts/model/calculate_impacts.R*) when the option to calculate the confidence intervals is on, were too big to be uploaded 
	on github. Therefore, these files have been uploaded to zenodo. When the confidence intervals are not calculated, these files are not 
	needed. 

- *response_ratios_forest.csv*: a file with the response ratios for forest management for the various species groups.
