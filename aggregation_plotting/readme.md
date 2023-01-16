February 2022

Author: Francesca Rosa, ETH Zürich

# aggregation_plotting

The purpose of the folder is to store aggregated results and plots. The results are aggregated fist with a general
function that sums the impacts over the ecoregions and store the resulting values in an .RData file and 
then with more specific functions that calculate additional results and produce .csv files. This process is performed
with the functions and scripts in the section "Aggregatie the results for analysis and for plotting" of the main.R.

This folder contains multiple subfolders corresponding to the scenarios/settings considered in the analysis and the folder *areas/*.

The folder *areas/* contains the .RData file and the .csv files output of the scripts *aggregate_areas.R* and *areas_Rdata-to-csv.R*.

The other subfolders contains:
- an .RData file with a first general aggregation over the ecoregions resulting from the script *aggregate_results.R*
   	through the function aggregate.results. This .RData file is used as input in the function *create.csv.EU.EPnoex* from 
   	*EU_Rdata-to-csv.R*, which takes the values in the .RData files and further aggregate or group them into multiple .csv files. 
   
- A folder */csv*, containing the results of the more specific aggregation process. 

- A folder */plots*.


#### Abbreviations used in the files of this folder

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





