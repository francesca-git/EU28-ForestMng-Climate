August 2021
Author: Francesca Rosa, ETH Zürich

# data/model_parameters/ecoregions_data/z_analysis

This folder contains contains the raw files then processed to obtain the z values used as input in the model for the calculation of the impacts and some analyses. 

Folders: 

- **plots**: plots meant to explore the dataset [1] used to obtain the z values used in the study.

Files

- *z_originals_Drakare_2005_SITable1.csv*: table with the aggregated data from the SI of the data source [1].
- *z_SAR_data_full-table.csv* and *z_SAR_data_full-table.xlsx*: original files sent by Prof. Hillebrand (corresponding 
	author of [1]) with the detailed dataset. The .csv file is used as input for the R code *scripts/data_preparation/z_values.R*.
- *z_analysis.xlsx*: file with some manual analysis to see how the extinction risk respond to different z values.

[1] Drakare, S., Lennon, J. J. & Hillebrand, H. The imprint of the geographical, evolutionary and ecological context on species-area relationships. Ecol. Lett. 9, 215–227 (2006).

