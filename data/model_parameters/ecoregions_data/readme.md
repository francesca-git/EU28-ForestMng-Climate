August 2021

Author: Francesca Rosa

# ecoregions_data

This folder contains the raw and processed data used to obtain the input parameters for the model, namely the response ratios (rr), the 
z values, the original number of species, the weighting factors and the vulnerability scores (VS).

Each sub-folder has a dedicated readme.md file.

List of .csv files available in this:

**Important**: the following files are not owned by the authors for the manuscript but by the authors of the 
references listed below. Please cite them if you use these files. 

- *Ecoregions_description.csv*
	Data on ecoregions from supplementary information of Chaudhary et al. (2015) [1], sheet Ecoregion description and VS.
	The column VS_plants was obtained as described in the folder on vulnerability scores (*/input_data/VS*)
- *Ecoregions_in-Globiom.csv*
	List of ecoregions which resulted from the intersection of the ecoregions used in Chaudhary et al. (2015) [1] and
	the ecoregions modelled in GLOBIOM. 
- *CF_local.csv*
	List of local characterization factors from supplementary information of Chaudhary (2015) [1], sheet CF_local. 
	Unlike the original paper, for this study no biome has been assigned to the land use class Urban. Already in Chaudhary
	et al. (2015) [1], information on biome for artificial lands is not available for 164 out of 190 data points, and the
	remaining data points cover only Plants and few biomes. Therefore, to avoid inconsistency, all the data have been
	grouped under the same category at biome level. Only data on Plants, Mammals and Birds have been selected, because
	for these taxa there are enough data points to model CF for forest use intensities (see next point on the list).
	Also, only the following land use types has been considered, as for forest management data comes from Chaudhary
	et al. (2016) [1], as explained in the next point: Annual crops, Permanent crops, Pastures, Urban.
- *CF_local_forest-use.csv*
	List of local characterization factors from supplementary information of Chaudhary et al. (2016) [2], sheet Raw data.
	CF have been calculated as 1 minus the ratio between mean species richness in disturbed (managed) forest sites
	and mean species richness in reference (unmanaged) forest sites. 
	Only data on Plants, Mammals and Birds have been selected, the reason in described in the previous paragraph.
- *CF_local_complete.csv*
	Same as CF_local.csv but including data for all the species group and all the land use types, as in Chaudhary et al. (2015) [1].
- *z_input_values.csv*
	Z values resulting from the aggregation of the original data of Drakare et al. (2006) [3].

[1] Chaudhary, A., Verones, F., de Baan, L., & Hellweg, S. (2015) Quantifying land use impacts on biodiversity: combining
species−area models and vulnerability indicators. Environmental Science & Technology, 16, 9987−9995. https://doi.org/10.1021/acs.est.5b02507.

[2] Chaudhary, A., Burivalova, Z., Koh, L. P., & Hellweg, S. (2016). Impact of Forest Management on Species Richness:
Global Meta-Analysis and Economic Trade-Offs. Nature Scientific Reports, 6, 23954. https://doi.org/10.1038/srep23954.

[3] Drakare, S.; Lennon, J. J.; Hillebrand, H. (2006). The Imprint of the Geographical, Evolutionary and Ecological Context
on Species-Area Relationships. Ecol. Lett. 9 (2), 215–227. https://doi.org/10.1111/j.1461-0248.2005.00848.x



