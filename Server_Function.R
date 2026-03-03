output$map <- renderLeaflet({
  
# Define colour Palette for Rasters
  pal_score <- colorNumeric(
    palette = c("#FFFFFF00", "#FDBF6F", "#FFF7BC", "#A6D96A", "#1A9850"), 
    domain = c(0, 4), 
    na.color = "transparent"
  )
  
  pal_density <- colorNumeric("Blues", domain = c(0, 4), na.color = "transparent")
  
  pal_slope <- colorNumeric(
    palette = c("#D95F02", "#FFFFFF00"), 
    domain = c(0, 1), 
    na.color = "transparent"
  )
  
# Build Leaftlet Map
  leaflet() %>% 
    addMapPane("topPoints", zIndex = 450) %>% 
    setView(lng = -3.5300, lat = 50.7200, zoom = 12) %>%
    
    # Base maps (User can toggle between these)
    addProviderTiles(providers$CartoDB.Positron, group = "Street Map") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Imagery") %>%
    
    # Display the processed suitability, density, and slope grids on the map.
    addRasterImage(slope_analysis, colors = pal_slope, opacity = 0.6, group = "Slope Constraint", project = FALSE) %>%
    addRasterImage(amenity_density, colors = pal_density, opacity = 0.8, group = "Amenity Density", project = FALSE) %>%
    addRasterImage(final_score, colors = pal_score, opacity = 0.85, group = "Suitability Score", project = FALSE) %>%
    
    # Display the environmental flood constraints as a transparent blue vector layer.
    addPolygons(data = flood_zone, color = "transparent", fillColor = "#3182bd", fillOpacity = 0.4, group = "Flood Zones") %>%
    
    # Draw Schools, Markets, and Healthcare as high-priority, clickable dots for popups.
    addCircleMarkers(data = schools, color = "#08519c", radius = 9, stroke = TRUE, weight = 3, 
                     fillOpacity = 0.9, popup = ~name, group = "Schools",
                     options = pathOptions(pane = "topPoints")) %>%
    
    addCircleMarkers(data = markets, color = "#54278f", radius = 9, stroke = TRUE, weight = 3, 
                     fillOpacity = 0.9, popup = ~name, group = "Markets",
                     options = pathOptions(pane = "topPoints")) %>%
    
    addCircleMarkers(data = healthcare, color = "#a50f15", radius = 9, stroke = TRUE, weight = 3, 
                     fillOpacity = 0.9, popup = ~name, group = "Healthcare",
                     options = pathOptions(pane = "topPoints")) %>%
    
    ## Legend and Control
    # Dynamic Legend
    # 1. Add Final Score Legend
    addLegend(colors = c("transparent", "#FDBF6F", "#FFF7BC", "#A6D96A", "#1A9850"), 
              labels = c("Unsuitable", "1 Amenity", "2 Amenities", "3 Amenities", "Fully Sustainable (4)"), 
              title = "Suitability Score", position = "bottomright", group = "Suitability Score") %>%
    
    # 2. Add Amenity Density Legend 
    addLegend(colors = RColorBrewer::brewer.pal(5, "Blues"), 
              labels = c("0 Amenities", "1 Amenity", "2 Amenities", "3 Amenities", "4 Amenities"), 
              title = "Amenity Density", position = "bottomleft", group = "Amenity Density") %>%
    
    # 3. Add Slope Legend 
    addLegend(colors = c("#D95F02", "transparent"), 
              labels = c("> 10° (Steep)", "< 10° (Flat)"), 
              title = "Slope Constraint", position = "bottomleft", group = "Slope Constraint") %>%
    
    # 4. Add Flood Zone Legend 
    addLegend(colors = "#3182bd", labels = "Flood-Prone Area", 
              title = "Flood Risk", position = "bottomleft", group = "Flood Zones") %>%
    
    ## Add Layer Control
    addLayersControl(
      # Users can only pick ONE base group at a time
      baseGroups = c("Street Map", "Satellite Imagery"), 
      
      # Enable User to check/uncheck as many of these as they want
      overlayGroups = c(
        "Suitability Score", 
        "Amenity Density", 
        "Slope Constraint", 
        "Flood Zones", 
        "Schools", 
        "Markets", 
        "Healthcare"
      ),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup(c("Amenity Density", "Slope Constraint", "Flood Zones", "Schools", "Markets", "Healthcare"))
})