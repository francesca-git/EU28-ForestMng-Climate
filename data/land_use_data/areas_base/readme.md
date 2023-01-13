August 2021
Author: Francesca Rosa, ETH Zürich

# data/land_use_data/areas_base
    
This folder contains the unprocessed raw data from GLOBIOM on areas and biomass harvested, as well as the converted
.csv files made suitable to be used in the processing function that tidies and matches the areas (*scripts/data_preparation/do_tidy_match.R*).

Folders: 

- OriginalFiles: list of original files sent by GLOBIOM, containing data on the areas and the volume of wood harvested under the multiple scenarios considered
- Baseline: raw files with data on the Baseline scenario prepared to be used in the scripts main.R through the do_tidy_match.R function.
- SharedEffort: raw files with data on the SharedEffort scenario prepared to be used in the scripts main.R through the do_tidy_match.R function.
- LowerIntensity: raw files with data on the LowerIntensity scenario prepared to be used in the scripts main.R through the do_tidy_match.R function.
		This falls under the Baseline scenario, but part of the imports and the exports are harvested from 
		low-intensity forestry, though for the imports much less area is covered by low-intensity management than the SharedEffort approach and the management is selective logging instead of Reduced impact logging.
		WARNING: the LowerIntensity approach was tested but the results were not included in the published article.
    

In each of these folders, there are the files with data on the area and the biomass harvested used as input in some of the scripts and functions called in the main.R are 

#### Abbreviations use in the files of this folder

*WARNING*: In the original files, in the results and in the aggregated values, the labeling of the scenarios is slightly different
          compared to the abbreviations used in the manuscript (see below)

-Climate mitigation scenarios
	- REF = Reference -> RCP6.5 in the manuscript
	- RCP = RCP 2.6

-EU Forest Use Scenarios
	- MFM = Multifunctional Forest Management > forcing of multifunctional forest management models on suitable area. This scenario is called closer-to-nature (CFM) in the manuscript.
	- SFM = Set Aside Forest Management > forcing of set aside on suitable area

-EU Forest Management scenarios
	- noAF = no alternative forest management
	- AF0 = not forcing payments for alternative extensive managements (favouring alternative intensification managements > clearcuts) -> *laissez-faire* in the manuscript
	- AF25 = forcing with payments to 25% expansion of alternative extensive managements (selective, retention) or set-aside -> CFM12.5% or SFM12.5% in the manuscript
	- AF50 = forcing with payments to 50% expansion of alternative extensive managements (selective, retention) or set-aside -> CFM25% or SFM25% in the manuscript
	- AF75 = forcing with payments to 75% expansion of alternative extensive managements (selective, retention) or set-aside -> CFM37.5% or SFM37.5% in the manuscript
	- AF100 = forcing with payments to 100% expansion of alternative extensive managements (selective, retention) or set-aside -> CFM50% or SFM50% in the manuscript
In this case, the difference in the labelling is due to the fact that the % in the scenarios listed in the original files were referred to the EU suitable area, which is 1/2 than the area
currently under forest management, to which the figures and results in the manuscript are referred. 






