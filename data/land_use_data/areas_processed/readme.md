August 2021
Author: Francesca Rosa

# data/land_use_data/areas_processed

The folder areas_processed contains the results of the process used to match the results of two GLOBIOM models: the main 
model which projects global future land use and a model specific for EU internal wood and energy crops 
production and EU forest biomass import and export, as described in the maniscript. The scripts used
to produce the areas are in *scripts/data_preparation* and are called in the section "Preparation of the areas" in the file main.R

Files *areas-to-match_Baseline.RData*, *areas-to-match_SharedEffort.RData*, *areas-to-match_LowerIntensity.RData* and 
*areas-to-match_Baseline_timber.RData*: .RData files output of the script *data_preparation/do_tidy_match.R*, more specifically of
its function *tidy_areas.R*.

The folders in areas_processed contain the output of the function *match_areas.R*, also in the script *data_preparation/do_tidy_match.R*.
 
Folders: 

The folders timber and notimber contain the land use areas (in Mha) that are used in the biodiversity model
(scripts in scripts_model). The default option is **notimber**, which means that in the EU land use model no timber
plantations are projected to be adopted in the areas considered by the model. **timber** means that part of areas
projected by the model to be occupied by clear-cut is replaced by timber plantations. This second option is explored
in the sensitivity analysis described in the manuscript. According to the options selected in the scripts of the
biodiversity model, either the values in timber or in notimber will be loaded to calculate the species loss. 

In the folder notimber, three sub-folders are present, which correspond to additional scenarios independed from timber:
- **Baseline**. The folder *Baseline* contains the tidied and matched areas originally obtained when the GLOBIOM model is run under the Baseline scenario. Under the
	Baseline scenario, the EU forest area suitable for management is kept 138 Mha, same as the current situation, and only 
	biomass from the most productive forest management is assumed to be imported to the EU28 (Clear-cut in Boreal/Temperate 
	and Plantations in the Tropics) or exported from EU28 (Clear-cut), following the historical trend and given economical 
	advantage of intensive management. This option was selected as default because according to IIASA’s experts it appeared the most likely. 
- **SharedEffort**. The folder *SharedEffort* contains the tidied and matched areas obtained when the GLOBIOM model is run under the Shared-effort scenario. 
	Under the Shared-effort scenario, the EU forest area suitable for management can be extended to 160 Mha (equal to the total 
	forest area in the EU) and that the areas from where the imports came from were partially set to under very low intensity forest management
	(such as Selection and Reduced Impact Logging). 
- **LowerIntensity**. The folder *LowerIntensity* contains the tidied and matched areas obtained when the GLOBIOM model is run under the Lower-intensity scenario. The
	Lower-intensity scenario is similar to the Baseline scenario, except that part of the imports and the exports are harvested from low-intensity forestry, 
	though for the imports much less area is covered by low-intensity management than the SharedEffort approach and the management is Selective logging instead of Reduced impact logging
	WARNING: This option was used for testing, but the results and description were not included in the final manuscript. 

In each subfolder, there are three sub-subfolders with the land use data per ecoregion:
- **aggregated**: the values of areas are aggregated without distinction between EU and RoW and considering the categories used 
	in the biodiversity model (e.g., there is no distinction between Cropland_EU and Cropland_RoW, and the Energy crops (EP) in EU
	are under the category of Permanent crops, as that is the category from which the response ration for the Energy crops in the EU
	is taken).
- **disaggregated**: the values are disaggregated, with a distinction between EU and RoW and between all the categories considered in GLOBIOM.
- **fraction**: fraction of a given land use in that ecoregion.

The folder *archived* contains files which in the end were not used for the final manuscript.

#### Abbreviations use in the files of this folder

**WARNING**: In the original files, in the results and in the aggregated values, the labeling of the scenarios is slightly different
          compared to the abbreviations used in the manuscript (see below)

- Climate mitigation scenarios
	- REF = Reference -> RCP6.5 in the manuscript
	- RCP = RCP 2.6

- EU Forest Use Scenarios
	- MFM = Multifunctional Forest Management > forcing of multifunctional forest management models on suitable area. This scenario is called closer-to-nature (CFM) in the manuscript.
	- SFM = Set Aside Forest Management > forcing of set aside on suitable area

- EU Forest Management scenarios
	- noAF = no alternative forest management
	- AF0 = not forcing payments for alternative extensive managements (favouring alternative intensification managements > clearcuts) -> *laissez-faire* in the manuscript
	- AF25 = forcing with payments to 25% expansion of alternative extensive managements (selective, retention) or set-aside -> CFM12.5% or SFM12.5% in the manuscript
	- AF50 = forcing with payments to 50% expansion of alternative extensive managements (selective, retention) or set-aside -> CFM25% or SFM25% in the manuscript
	- AF75 = forcing with payments to 75% expansion of alternative extensive managements (selective, retention) or set-aside -> CFM37.5% or SFM37.5% in the manuscript
	- AF100 = forcing with payments to 100% expansion of alternative extensive managements (selective, retention) or set-aside -> CFM50% or SFM50% in the manuscript
In this case, the difference in the labelling is due to the fact that the % in the scenarios listed in the original files were referred to the EU suitable area, which is 1/2 than the area
currently under forest management, to which the figures and results in the manuscript are referred. 