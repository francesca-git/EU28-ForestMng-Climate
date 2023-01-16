August 2021

Author: Francesca Rosa, ETH Zürich

# data/model_parameters/ecoregions_data/rr_z

This folder contains the data on response ratios and z values that result from the application of the functions in *scripts/model/parameters_calculation* and
*scripts/data_preparation* to the .csv files in *data/model_parameters/ecoregions_data/*.

Folders:

- **archived**: folder containing material used for testing and eventually not included in the final version of the manuscript.

Files:

- The .Rdata files with the calculated response ratios and z values for the different 
	options considered in the study. These files are the output of the function *calculate.RR* 
	in *scripts/model/parameters_calculation.R* (those labelled as “static”) and of *calculate.RR.bs* 
	in *scripts/model/bootstrapping.R* (those labelled as “bs”).
- *response_ratios_forest.csv*: a file with the response ratios for forest management for the various species groups.
