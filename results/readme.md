February 2022
Author: Francesca Rosa

Folder results/

This folder contains multiple sub-folders with the results of the application of the biodiversity model through 
the scripts in the folder scripts/model, more specifically of calculate_impacts.R, which are called 
in the section "Calculation of the extinction risk" in the file main.R

The code words used to identify the scenario/settings to which each sub-folder belongs are listed here.
Scenario - "Baseline", "SharedEffort" or "LowerIntensity". The description of the first two options is available 
in the main manuscript. The third option instead was used for 
testing but was not included in the manuscript, for more details see the initial settings of main.R)

Confidence intervals - “bs” or “static”:
	bs: bootstrapping – in the biodiversity model, the bootstrapping procedure has been applied to both the response 
	    ratios and the z values to quantify the response ratio to quantify the confidence intervals.
	static: the biodiversity model has calculated the static results (no bootstrapping applied). They correspond to 
	        S_hat_lost_global_aggr_i,j in S10 and are the values to which the confidence intervals are applied.

Cutoff on the response ratio – “cutoff” 
	cutoff: in the biodiversity model, all response ratios above 1 have been set to 1.
	The option without the cutoff (nocutoff) was explored for testing but not included in the final manuscript.  

Inclusion of timber plantations – “timber” or no labelling at all.
	no labelling: in the GLOBIOM model, the EU forest management which is slightly more intense than clear cut has been 
		      considered as clear cut. 
	timber: in the GLOBIOM model, part of EU clear-cut management has been replaced with timber plantations. 

The folder 'archived' contains material used for testing and eventually not included in the manuscript.
