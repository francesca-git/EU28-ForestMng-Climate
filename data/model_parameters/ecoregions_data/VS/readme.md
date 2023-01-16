August 2021
Author: Francesca Rosa, ETH Zürich

data/model_parameters/ecoregions_data/VS

This folder contains the raw and processed files used to obtain the VS (vulnerability scores) for plants usign the scripts *scripts/VS_plants.R*.

Files:

The LC-Impact methodology documentation [1] does not provide the VS for plants (and neither does the publication on which the LC-Impact methodology is based [2]), 
but it provides the CFs for plants. Therefore, it was possible to obtain the VS as ratio between the global CFs for plants 
(file: *original-files/PerTaxonAndAggregated global CF_Average_July 17th 2016.csv*) and the regional CFs for plants (file: *original-files/Ch6 PSLregional v01.csv*).
The global and the regional CFs have been organized in a single table (*Global_Regional_species-lost_LCImpact.csv*) and the ratio is available in *VS_plants_LU.csv*.
The CFs are land-use- and ecoregion-specific, so the ratios have been obtained per ecoregion and per land use class. The VS should be ecoregion-specific but not
land-use specific. Hence, to obtain one VS per ecoregion we took the average of the values for the different land uses, which slightly differed (probably due to
numerical rounding when the CFs were calculated for the LC-Impact methodology). The script scripts/data_preparation/VS_plants.R was used for the averaging and the
results were saved in *VS_plants.csv*. The VS calculated were copied in the *input_data/ecoregions_data/Ecoregions_description.csv* in the column VS_plants (the rest 
of the file Ecoregions_descriptoin.csv comes from Chaudhary et al, 2015 [2]).

Folders: 

- **original-files**: folder containing the files used to calculate the VS (*Ch6 PSLregional v01.csv* and *PerTaxonAndAggregated global CF_Average_July 17th 2016.csv*). 

- **archived**: folder containing files used to perform some tests but not inlcuded in the final analysis.

[1] Chaudhary, A.; Verones, F.; De Baan, L.; Pfister, S.; Hellweg, S. Chapter 11 - Land Stress: Potential Species Loss Form Land Use. LC-IMPACT Version 1.0, www.lc-impact.eu. 2016.
[2] Chaudhary, A.; Verones, F.; De Baan, L.; Hellweg, S. Quantifying Land Use Impacts on Biodiversity: Combining Species-Area Models and Vulnerability Indicators. Environ. Sci. Technol. 2015, 49 (16), 9987–9995. https://doi.org/10.1021/acs.est.5b02507.