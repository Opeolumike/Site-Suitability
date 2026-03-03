# Project Title: Site Suitability Analysis for Sustainable Housing in Exeter
# Date: March 2026

# Run global script containing all your relevant data 
source("Global.R")

# Define UI for visualisation
source("UI.R")

ui <- navbarPage("Exeter Sustainable Housing Analytics", id = 'nav',
                 tabPanel("Suitability Dashboard", 
                          ui_content
                 )
)

# Define the server that performs all necessary operations
server <- function(input, output, session){
  source("Server_Function.R", local = TRUE)
}

# Force the app to launch in the system's default browser
options(shiny.launch.browser = TRUE)

# Run the application
shinyApp(ui, server)