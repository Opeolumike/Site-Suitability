# Load libraries
library(shiny)
library(leaflet)
library(sf)
library(raster)
library(RColorBrewer)
library(dplyr)
library(munsell)

# Using quiet = TRUE to keep the console clean when launching
# Load the Vectors (Already in EPSG:4326)
schools    <- st_read("Shiny_Schools.gpkg", quiet = TRUE)
markets    <- st_read("Shiny_Markets.gpkg", quiet = TRUE)
healthcare <- st_read("Shiny_Healthcare.gpkg", quiet = TRUE)
flood_zone <- st_read("Shiny_FloodZones.gpkg", quiet = TRUE)

# Load the Vectors (Already in EPSG:4326)
final_score     <- raster("Shiny_Final_Score.tif")
amenity_density <- raster("Shiny_Amenity_Density.tif")
slope_analysis  <- raster("Shiny_Slope_Analysis.tif")