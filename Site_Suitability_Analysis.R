# Topic: Site Suitability Analysis for Sustainable Housing Developments in Exeter, Devon
# Date: March 2026

## Automate Data Retrieval and Spatial Processing
# Fetch and transform OSM data
getOSMFeature <- function(bbox, key, value, type = "lines"){
  # Add timeout = 300 to wait 5 minutes for server response. This fixes timing out after 25seconds
  # whenever the Open Street Map API is busy 
  query <- osmdata::opq(bbox = bbox, timeout = 300) %>% 
    osmdata::add_osm_feature(key = key, value = value) %>% 
    osmdata::osmdata_sf()
  if(type == "lines") {
    data <- query$osm_lines
  } else {
    # Combine points and polygons (centroids) for buildings like schools/markets
    poly <- query$osm_polygons
    pts  <- query$osm_points
    if(!is.null(poly) & !is.null(pts)) data <- dplyr::bind_rows(pts, sf::st_centroid(poly))
    else data <- if(!is.null(pts)) pts else sf::st_centroid(poly)
  }
  # Project the OSM data back to British National Grid
  if(!is.null(data)) return(sf::st_transform(data, sf::st_crs(elevationRaster)))
  return(NULL)
}

## Vectors Export
# Clean and export as GeoPackage
export_clean_geopackage <- function(data, filename) {
  
  # Keep only the 'name' column
  if("name" %in% names(data)) {
    data_clean <- data[, "name"]
  } else {
    data$name <- "Unknown"
    data_clean <- data[, "name"]
  }
  
  # Write file
  sf::st_write(data_clean, filename, quiet = TRUE, delete_layer = TRUE)
  print(paste("Success: Exported", filename))
}

# Load libraries
library("sf")
library("terra")
library("osmdata")
library("tmap")
library("tidyverse")
library("sfnetworks")
library("tidygraph")
library("dodgr")
library("maptiles")

osmdata::set_overpass_url("https://lz4.overpass-api.de/api/interpreter")

# Load the LIDAR Composite DTM for 2022 with 1m resolution 
dtmFilename <- "Exeter_DTM_1m.tif"
if(!file.exists(dtmFilename)) stop("DTM file missing!")
elevationRaster <- rast(dtmFilename)

# Load the "Risk of Flooding from Rivers and Sea" shapefile
floodFilename <- "rofrs_4band_Exeter.shp"
if(!file.exists(floodFilename)) stop("Flood file missing!")
floodVector <- st_read(floodFilename)

## Analyse the slope data
# Calculate slope from elevation. 
slopeMap <- terrain(elevationRaster, v = "slope", unit = "degrees")
# Set Rule: Slopes < 10 degrees are suitable (1), > 10 are unsuitable (0)
slopeSuitability <- ifel(slopeMap < 10, 1, 0)

## Analyse the flood data
# Ensure CRS matches (Project Flood Vector to Raster's British National Grid)
floodVector <- st_transform(floodVector, st_crs(elevationRaster))
# Convert the flood polygons into the DTM binary grid
floodRaster <- rasterize(floodVector, elevationRaster, field = 1, background = 0)
# Apply suitability logic: Safe Areas (1) and Flooded Areas (0)
floodSuitability <- ifel(floodRaster == 0, 1, 0)


## Add the Amenities
# Reproject study area to WGS84 (Lat/Lon) for OpenStreetMap API
studyAreaBox <- st_as_sfc(st_bbox(elevationRaster))
studyAreaLatLon <- st_transform(studyAreaBox, 4326)
bboxObj <- st_bbox(studyAreaLatLon)

# Fetch the Amenities Layers
roadsVector <- getOSMFeature(bboxObj, "highway", c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", "residential", "unclassified", "living_street", "service", "pedestrian", "footway", "path", "cycleway", "steps", "track"), "lines")

schoolsVector <- getOSMFeature(bboxObj, "amenity", "school", "polygons")
if(!is.null(schoolsVector)) schoolsVector <- schoolsVector %>% filter(!is.na(name))
# Fetch both large supermarkets and local neighborhood convenience stores
marketsVector <- getOSMFeature(bboxObj, "shop", c("supermarket", "convenience"), "polygons")
if(!is.null(marketsVector)) marketsVector <- marketsVector %>% filter(!is.na(name))

healthcareVector <- getOSMFeature(bboxObj, "amenity", c("doctors", "clinic", "hospital", "pharmacy"), "polygons")
if(!is.null(healthcareVector)) healthcareVector <- healthcareVector %>% filter(!is.na(name))

# Aggregate the DTM from 1m to 10m resolution to speed up processing.
elevationRasterLowRes <- aggregate(elevationRaster, fact = 10, fun = mean)


## Calculate Distances to each amenity

# 1. Road Access remains Euclidean
roadDist <- distance(elevationRasterLowRes, vect(roadsVector))

# 2. Build the Routing Network Graph in WGS84
roads_lines <- st_cast(roadsVector, "LINESTRING", warn = FALSE) 
roads_wgs <- st_transform(roads_lines, 4326) # Project to WGS84 for dodgr
street_net <- weight_streetnet(roads_wgs, wt_profile = "foot")

# 3. Extract Origin Coordinates and Project to WGS84
grid_coords <- crds(elevationRasterLowRes, df = TRUE, na.rm = FALSE)
origins_sf <- st_as_sf(as.data.frame(grid_coords), coords = c("x", "y"), crs = st_crs(elevationRasterLowRes))
origins_wgs <- st_transform(origins_sf, 4326)
origins <- st_coordinates(origins_wgs)

# 4. Temporarily reproject to WGS84
dest_schools <- st_coordinates(st_transform(schoolsVector, 4326))
dest_markets <- st_coordinates(st_transform(marketsVector, 4326))
dest_hosp    <- st_coordinates(st_transform(healthcareVector, 4326))

# 5. Calculate Network Distances 
dist_mat_schools <- dodgr_dists(graph = street_net, from = origins, to = dest_schools)
min_dist_schools <- apply(dist_mat_schools, 1, function(x) {
  if(all(is.na(x))) return(Inf) else return(min(x, na.rm = TRUE))
})

# Calculate network distance to Markets
dist_mat_markets <- dodgr_dists(graph = street_net, from = origins, to = dest_markets)
min_dist_markets <- apply(dist_mat_markets, 1, function(x) {
  if(all(is.na(x))) return(Inf) else return(min(x, na.rm = TRUE))
})

# Calculate network distance to hospital (Healthcare)
dist_mat_hosp <- dodgr_dists(graph = street_net, from = origins, to = dest_hosp)
min_dist_hosp <- apply(dist_mat_hosp, 1, function(x) {
  if(all(is.na(x))) return(Inf) else return(min(x, na.rm = TRUE))
})

# Clone the base raster and inject the new network distances
schoolDist <- rast(elevationRasterLowRes)
values(schoolDist) <- min_dist_schools

marketDist <- rast(elevationRasterLowRes)
values(marketDist) <- min_dist_markets

healthcareDist <- rast(elevationRasterLowRes)
values(healthcareDist) <- min_dist_hosp

# Classify Distances (1 = Suitable, 0 = Too Far). Set Thresholds: Roads 500m, Schools/Markets 1000m, Hospitals 2000m
scoreRoad       <- ifel(roadDist < 1000, 1, 0)       
scoreSchool     <- ifel(schoolDist < 2000, 1, 0)   
scoreMarket     <- ifel(marketDist < 1200, 1, 0)     
scoreHealthcare <- ifel(healthcareDist < 1200, 1, 0)

# Resample the flood and slope risk to the new 10m DTM
floodSuitability <- resample(floodSuitability, elevationRasterLowRes, method = "near")
slopeSuitability <- resample(slopeSuitability, elevationRasterLowRes, method = "near")

# Calculate Final Score
totalScore <- (scoreRoad + scoreSchool + scoreMarket + scoreHealthcare) * (floodSuitability * slopeSuitability)

# Mask results to the exact study area shape (remove ocean/edges)
finalPlot <- mask(totalScore, elevationRasterLowRes)

# Set the tmap library to static plotting mode
tmap_mode("plot")

# Fetch the web tiles and crop them perfectly to the 10m raster bounds
exeter_basemap <- get_tiles(elevationRasterLowRes, provider = "CartoDB.Positron", zoom = 13, crop = TRUE)

## Generate all output maps 
## Mask the output data to the study area (Exeter DTM 10m) so it looks clean
## Set the layout, grid and legend styles
## Plot and save output as png

# Set the grid style
custom_grid <- tm_grid(
  n.x = 4, n.y = 4,
  labels.inside = FALSE,
  labels.size = 0.7,
  col = "grey40",
  alpha = 0.4,
  labels.format = list(digits = 3, big.mark = ",", scientific = FALSE)
)

# Set the Legend Layout Style
custom_layout <- tm_layout(
  legend.outside = TRUE,
  legend.outside.position = "right",
  legend.outside.size = 0.25,
  inner.margins = c(0.1, 0.1, 0.05, 0.05), 
  main.title.size = 1.1,
  frame = TRUE
)

#  Plot and save roads
mapRoads <- tm_shape(exeter_basemap) + 
  tm_rgb() + # Renders the basemap image
  tm_shape(mask(scoreRoad, elevationRasterLowRes)) +
  # Changed "transparent" to "#FFFFFF00" to fix the cols4all parsing error
  tm_raster(col.scale = tm_scale_categorical(values = c("#FFFFFF00", "orange"), 
                                             labels = c("> 1km", "< 1km")),
            col.legend = tm_legend(title = "Road Access")) +
  tm_shape(roadsVector) + 
  tm_lines(col = "black", col_alpha = 0.3) +
  
  custom_grid +
  tm_compass(position = c("left", "top"), size = 2.0) +
  tm_scalebar(position = c("left", "bottom")) + 
  
  tm_title("Public Transport System") +
  custom_layout
mapRoads
tmap_save(mapRoads, "Exeter_Public_Transport.png", width=10, height=8)

# Plot and save schools
mapSchools <- tm_shape(exeter_basemap) + tm_rgb() +
  tm_shape(mask(scoreSchool, elevationRasterLowRes)) +
  tm_raster(col.scale = tm_scale_categorical(values = c("#FFFFFF00", "blue"), 
                                             labels = c("> 2km", "< 2km")),
            col.legend = tm_legend(title = "School Access")) +
  tm_shape(schoolsVector) + 
  tm_dots(size = 0.5) +
  
  custom_grid +
  tm_compass(position = c("left", "top"), size = 2.0) +
  tm_scalebar(position = c("left", "bottom")) +
  
  tm_title("Schools") +
  custom_layout
mapSchools
tmap_save(mapSchools, "Exeter_Sustainability_Schools.png", width=10, height=8)

# Plot and save markets
mapMarkets <- tm_shape(exeter_basemap) + tm_rgb() +
  tm_shape(mask(scoreMarket, elevationRasterLowRes)) +
  tm_raster(col.scale = tm_scale_categorical(values = c("#FFFFFF00", "purple"), 
                                             labels = c("> 1.2km", "< 1.2km")),
            col.legend = tm_legend(title = "Market Access")) +
  tm_shape(marketsVector) + 
  tm_dots(size = 0.5) +
  
  custom_grid +
  tm_compass(position = c("left", "top"), size = 2.0) +
  tm_scalebar(position = c("left", "bottom")) +
  
  tm_title("Supermarkets") +
  custom_layout
mapMarkets
tmap_save(mapMarkets, "Exeter_Sustainability_Supermarkets.png", width=10, height=8)

# Plot and save healthcare access
mapHealthcare <- tm_shape(exeter_basemap) + tm_rgb() +
  tm_shape(mask(scoreHealthcare, elevationRasterLowRes)) +
  tm_raster(col.scale = tm_scale_categorical(values = c("#FFFFFF00", "red"), 
                                             labels = c("> 1.2km", "< 1.2km")),
            col.legend = tm_legend(title = "Healthcare Access")) +
  tm_shape(healthcareVector) + 
  tm_dots(size = 0.8, shape = 3) +
  
  custom_grid +
  tm_compass(position = c("left", "top"), size = 2.0) +
  tm_scalebar(position = c("left", "bottom")) +
  
  tm_title("Primary Healthcare") +
  custom_layout
mapHealthcare
tmap_save(mapHealthcare, "Exeter_Sustainability_Healthcare.png", width=10, height=8)

# Plot and save Flood Risk Analysis
constraintsMap <- (floodSuitability * slopeSuitability)

# Force to factor and rename to fix the legend
constraintsMap_cat <- as.factor(constraintsMap)
names(constraintsMap_cat) <- "Risk"

mapConstraints <- tm_shape(exeter_basemap) + tm_rgb() +
  tm_shape(mask(constraintsMap_cat, elevationRasterLowRes)) +
  tm_raster(col.scale = tm_scale_categorical(
    values = c("#D95F02", "#FFFFFF00"), 
    labels = c("Flood-Prone", "Safe")
  ),
  col.legend = tm_legend(title = "Flood Risk"), alpha = 0.6) + 
  
  custom_grid +
  tm_compass(position = c("left", "top"), size = 2.0) +
  tm_scalebar(position = c("left", "bottom")) +
  
  tm_title("Flood Risk Analysis") +
  custom_layout

mapConstraints
tmap_save(mapConstraints, "Exeter_FloodRisk_Analysis.png", width=10, height=8)


#Plot and save the slope analysis
slopePlotData <- mask(slopeSuitability, elevationRasterLowRes)

# Force to factor and rename to fix the legend
slopePlotData_cat <- as.factor(slopePlotData)
names(slopePlotData_cat) <- "Terrain"

mapSlope <- tm_shape(exeter_basemap) + tm_rgb() +
  tm_shape(slopePlotData_cat) +
  tm_raster(col.scale = tm_scale_categorical(
    values = c("#D95F02", "#FFFFFF00"), 
    labels = c("> 10° (Steep)", "< 10° (Flat)")
  ),
  col.legend = tm_legend(title = "Terrain Suitability"), alpha = 0.6) +
  
  custom_grid +
  tm_compass(position = c("left", "top"), size = 2.0) +
  tm_scalebar(position = c("left", "bottom")) +
  
  tm_title("Slope Analysis (10° Threshold)") +
  custom_layout

mapSlope
tmap_save(mapSlope, "Exeter_Slope_Analysis.png", width=10, height=8)


# Plot and save the Amenity density
amenitySum <- (scoreRoad + scoreSchool + scoreMarket + scoreHealthcare)

# Force to factor and rename to fix the legend
amenitySum_cat <- as.factor(amenitySum)
names(amenitySum_cat) <- "Count"

mapAmenitySum <- tm_shape(exeter_basemap) + tm_rgb() +
  tm_shape(mask(amenitySum_cat, elevationRasterLowRes)) +
  tm_raster(col.scale = tm_scale_categorical(
    values = "Blues", 
    labels = c("0 Amenities", "1 Amenity", "2 Amenities", "3 Amenities", "4 Amenities")
  ),
  col.legend = tm_legend(title = "Amenity Density Count"), alpha = 0.85) +
  
  custom_grid +
  tm_compass(position = c("left", "top"), size = 2.0) +
  tm_scalebar(position = c("left", "bottom")) +
  
  tm_title("Amenity Density") +
  custom_layout

mapAmenitySum
tmap_save(mapAmenitySum, "Exeter_Amenity_Density.png", width=10, height=8)


# Rename the raster layer natively
names(finalPlot) <- "Score" 

# Plot and save final sustainability score
mapFinal <- tm_shape(exeter_basemap) + tm_rgb() +
  tm_shape(finalPlot) +
  # Force strict interval breaks so the legend never breaks
  tm_raster(
    col = "Score", # <--- Now accurately matches the name assigned above
    col.scale = tm_scale_intervals(
      breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5), 
      values = c("#FFFFFF00", "#FDBF6F", "#FFF7BC", "#A6D96A", "#1A9850"),
      labels = c("Unsuitable", "1 Amenity", "2 Amenities", "3 Amenities", "Fully Sustainable (4)")
    ),
    col.legend = tm_legend(title = "Suitability Score"), 
    alpha = 0.85
  ) +
  
  custom_grid +
  tm_compass(position = c("left", "top"), size = 2.0) +
  tm_scalebar(position = c("left", "bottom")) +
  
  tm_title("Suitability Score for Sustainable Housing Development in Exeter") +
  custom_layout

mapFinal
tmap_save(mapFinal, "Exeter_Suitability_Score.png", width=10, height=8)


## Export vector data
# Export the GeoPackages for Roads, Schools, Markets, Healthcare and Flood Zones
export_clean_geopackage(roadsVector,      "Roads.gpkg")
export_clean_geopackage(schoolsVector,    "Schools.gpkg")
export_clean_geopackage(marketsVector,    "Markets.gpkg")
export_clean_geopackage(healthcareVector, "Healthcare.gpkg")
export_clean_geopackage(floodVector,      "FloodZones.gpkg")


## Export Raster Files
## Mask them to 'elevationRasterLowRes' (The 10m DTM) so they have clean edges like the images.
# Export the calculated Distances (in metres) rasters
writeRaster(mask(roadDist, elevationRasterLowRes),   "Distance_to_Roads.tif",   overwrite = TRUE)
writeRaster(mask(schoolDist, elevationRasterLowRes), "Distance_to_Schools.tif", overwrite = TRUE)
writeRaster(mask(marketDist, elevationRasterLowRes), "Distance_to_Markets.tif", overwrite = TRUE)
writeRaster(mask(healthcareDist, elevationRasterLowRes),   "Distance_to_Healthcare.tif", overwrite = TRUE)

# Export Constraint (Flood and Slope) raster
writeRaster(mask(floodSuitability, elevationRasterLowRes), "Flood_Analysis.tif", overwrite = TRUE)
writeRaster(mask(slopeSuitability, elevationRasterLowRes), "Slope_Analysis.tif", overwrite = TRUE)

# Export Exeter Sustainability score raster
writeRaster(mask(amenitySum, elevationRasterLowRes), "Amenity_Density.tif", overwrite = TRUE)
writeRaster(finalPlot,                               "Final_Sustainability_Score.tif", overwrite = TRUE)

# Export 10m DTM
writeRaster(elevationRasterLowRes, "Exeter_DTM_10m.tif", overwrite = TRUE)


## Project to WGS84 / EPSG:4326) primarily for shiny app
# Reproject and export
export_clean_geopackage(st_transform(roadsVector, 4326),      "Shiny_Roads.gpkg")
export_clean_geopackage(st_transform(schoolsVector, 4326),    "Shiny_Schools.gpkg")
export_clean_geopackage(st_transform(marketsVector, 4326),    "Shiny_Markets.gpkg")
export_clean_geopackage(st_transform(healthcareVector, 4326), "Shiny_Healthcare.gpkg")
export_clean_geopackage(st_transform(floodVector, 4326),      "Shiny_FloodZones.gpkg")

## Project rasters to WGS84 for fast Leaflet rendering
# Using method="near" so integer scores/binary classifications aren't interpolated into decimals
writeRaster(project(finalPlot, "EPSG:4326", method="near"),        "Shiny_Final_Score.tif", overwrite = TRUE)
writeRaster(project(amenitySum, "EPSG:4326", method="near"),       "Shiny_Amenity_Density.tif", overwrite = TRUE)
writeRaster(project(slopeSuitability, "EPSG:4326", method="near"), "Shiny_Slope_Analysis.tif", overwrite = TRUE)

writeRaster(project(schoolDist, "EPSG:4326"),     "Shiny_Distance_Schools.tif", overwrite = TRUE)
writeRaster(project(marketDist, "EPSG:4326"),     "Shiny_Distance_Markets.tif", overwrite = TRUE)
writeRaster(project(healthcareDist, "EPSG:4326"), "Shiny_Distance_Healthcare.tif", overwrite = TRUE)
writeRaster(project(roadDist, "EPSG:4326"),       "Shiny_Distance_Roads.tif", overwrite = TRUE)
