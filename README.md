# firesize_publi
code for the publication in GCB entitled: Increasing aridity causes larger and more severe forest fires across Europe

# scripts
all scripts are in the folder "lib". In order to perform the full analysis, please follow through all scripts.
we provide the results of some steps in order to reduce runtime for the user. we indicate this in the headings of the scripts.

1. in the first script we prepare the ERA5-Land data. The code for the download is provided.
The user needs a CDS API for downloading.

2. the data from the ERA5-Land summer VPD is extracted for the fire complexes

3. calculation of the maximum fire size to total burned area relationship

4. the models are calibrated and compared. In this script the figure 3 and 4 are created 

5. preparation of the future climate dataset. Again, the script for the download is provided but the user needs an API.

6. Extraction of the CMIP6 VPD.

7. Future climate analysis

8. Plotting of all figures that were not done in the previous scripts


# data

climate: we provide the climate grid. all other climate data can be downloaded following the instructions within the scripts.

complexes: we provide the fire complexes of each country. This data contains all information needed for the analysis including year, size, severity and polygon information.

countries: we provide the shapefiles of each country and Europe that are needed for the analysis in this folder.

ecoregions: Olson et al. terrestrial ecosystems should be downloaded from: https://www.arcgis.com/home/item.html?id=be0f9e21de7a4a61856dad78d1c79eae

models: we provide all final models used for the analysis.

results: we provide the results of the individual steps. This should help to reduce the runtime for the user.