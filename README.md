# Project Title: Site Suitability Analysis for Sustainable Housing Developments in Exeter, Devon

For any housing development to be termed "sustainable", it must be environmentally fit (i.e., safe from flooding and terrain risks) and socially accessible (i.e., within proximity to existing infrastructural developments).

This study focused on Exeter, a city in Devon, South-West England with a population of 130,700 in 2021 as at the 2021 Census. In 2021, Exeter was home to around 19.8 people per football pitch-sized (approximately 7,140sqm) piece of land, compared with 17.9 in 2011. This area was among the top 25% most densely populated English local authority areas at the last census (How life has changed in Exeter: Census 2021). 

Exeter is a perfect case study for the need to create balance between housing needs and environmental protection. It is well-known for complex topography that ranges from the low-lying and flood-prone valley of the River Exeter to the steep gradients of the northern hills such as Pennsylvania and Exwick

This repository contains the R code, spatial data, and outputs for a site suitability analysis assessing potential locations for sustainable housing developments in Exeter, Devon, United Kingdom.


## Files

# Data
- Exeter_DTM_1m.tif – LiDAR Composite Digital Terrain Model (DTM), 1 m resolution (2022)
- rofrs_4band_Exeter.shp – Risk of Flooding from Rivers and Sea shapefile  
	The two datasets were sourced from https://environment.data.gov.uk
- Data of road network, supermarkets, hospitals, and schools were retrieved from the OpenStreetMap through the API using the osmdata R package 

# Script
- The "Site_Suitability_Analysis" is the R script used for data processing, analysis, and map generation.

## How to Run the Script
- Start RStudio and set the project folder as your working directory.
- Install the required packages by running this in the console:
    ```r
  install.packages(c("sf", "terra", "osmdata", "tmap", "tidyverse"))
- Run the script without changing file paths.

## Outputs
The "Outputs" folder contains the maps produced from the analysis in RStudio

## References
Department for Environment, Food & Rural Affairs (2024) Survey Data Download (LiDAR) and Risk of Flooding from Rivers and Sea (RoFRS). Available at: https://environment.data.gov.uk/ (Accessed: 28 November 2025).

Hijmans, R. J. (2024) terra: Spatial Data Analysis. R package version 1.7-71. Available at: https://cran.r-project.org/package=terra (Accessed: 28 November 2025).

Nash, A. (2019) National population projections: 2018-based. Office for National Statistics. Available at: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/nationalpopulationprojections/2018based/previous/v2 (Accessed: 30 November 2025).

Office for National Statistics (2021) How life has changed in Exeter: Census 2021. Available at: https://www.ons.gov.uk/visualisations/censusareachanges/E07000041/ (Accessed: 30 November 2025).

OpenStreetMap Contributors (2025) Planet dump. Available at: https://planet.osm.org (Accessed: 25 November 2025).

Padgham, M., Rudis, B., Lovelace, R. and Salmon, M. (2024) osmdata: Import 'OpenStreetMap' Data as Simple Features or Spatial Objects. R package version 0.2.5. Available at: https://cran.r-project.org/package=osmdata (Accessed: 25 November 2025).

Pebesma, E. (2018) 'Simple Features for R: Standardized Support for Spatial Vector Data', The R Journal, 10(1), pp. 439-446.

Prochorskaite, A., Couch, C., Malys, N. and Maliene, V. (2016) 'Housing stakeholder preferences for the “Soft” features of sustainable and healthy housing design in the UK', International Journal of Environmental Research and Public Health, 13(1), p. 111.

R Core Team (2024) R: A Language and Environment for Statistical Computing. Vienna, Austria: R Foundation for Statistical Computing. Available at: https://www.R-project.org/ (Accessed: 28 November 2025).
Savills (2025) English Housing Supply Update Q2 2025. Available at: https://www.savills.co.uk/research_articles/229130/380149-0 (Accessed: 30 November 2025).

Tennekes, M. (2018) 'tmap: Thematic Maps in R', Journal of Statistical Software, 84(6), pp. 1-39.

Wickham, H. et al. (2019) 'Welcome to the Tidyverse', Journal of Open Source Software, 4(43), p. 1686.
