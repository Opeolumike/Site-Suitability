ui_content <- div(
  # Forcing the map to take the full viewport height minus the navigation bar
  leafletOutput("map", width = "100%", height = "calc(100vh - 70px)")
)