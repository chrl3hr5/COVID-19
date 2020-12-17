# Loading libraries
library(tidyverse) # For data analysis
library(data.table) # For performance
library(COVID19) # For COVID-19 data
library(shiny) # For R Shiny
library(shinydashboard) # For Shiny dashboard
library(semantic.dashboard) # For Semantic dashboard
library(leaflet) # For maps
library(styler) # For formatting R code

# Loading data
Data <- as.data.table(covid19()) # Worldwide data by country

# User Interface
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Tracker"),
  dashboardSidebar(),
  dashboardBody(leafletOutput(outputId = "COVID Map"))
)

# Server
server <- function(input, output) {
  output$`COVID Map` <- renderLeaflet({
    leaflet(filter(Data, !is.na(latitude) & !is.na(longitude))) %>%
      addTiles() %>%
      fitBounds(~ min(latitude), ~ min(longitude), ~ max(latitude), ~ max(longitude)) %>%
      addCircleMarkers(stroke = F, opacity = 0.5, radius = 2, popup = "Map")
  })
}

# Application
shinyApp(ui, server)