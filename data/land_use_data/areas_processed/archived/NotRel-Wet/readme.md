
data/land_use_data/areas_processed/archived/notRel-Wet

The folder NotRel-Wet contains files with land use areas as well. Differently from timber and notimber, these files
include the land use areas of the categories Not relevant land and Wetland as defined by GLOBIOM. In the matching
under the default settings (timber or notimber), the areas of these land use categories are removed, as they have
no corresponding category in the biodiversity model and are not productive lands.
do_tidy_match.R allows the user to select an option which does not remove these land use classes and their areas
during the matching. The results, saved as .csv files, are stored in NotRel-Wet/Baseline/. The sub-folder check_area_share/
contains files, scripts and plots used to check that the fraction of Not relevant land and Wetland areas in the world
ecoregions were not too high where the results for species loss showed a large PDF. Not including some unproductive
areas of an ecoregion when applying the biodiversity model might cause an overestimation of the impacts, therefore
it was important to verify that the highest share of Not relevant land and Wetland did not occur where the model
projects large impacts. 