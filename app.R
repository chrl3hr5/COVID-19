# Loading libraries
library(tidyverse) # For data analysis
library(data.table) # For performance
library(skimr) # For data summary
library(COVID19) # For COVID-19 data
library(shiny) # For R Shiny
library(shinydashboard) # For Shiny dashboard
library(leaflet) # For maps
library(styler) # For formatting R code

# Loading data
Data <- as.data.table(covid19()) # Worldwide data by country

# User Interface
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Data Visualization", tabName = "Visual", icon = icon("window-maximize"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Dashboard", leafletOutput(outputId = "COVID Map"),
              selectInput(inputId = "Map", label = "List of Maps", choices = names(providers), selected = "Stamen.Watercolor"),
      tabItem(tabName = "Visual")
  )))
)

# Server
server <- function(input, output) {
  output$`COVID Map` <- renderLeaflet({
    leaflet(filter(Data, !is.na(latitude) & !is.na(longitude))) %>%
      setView(lng = 78, lat = 20, zoom = 3) %>%
      addTiles() %>%
      addProviderTiles(input$Map)
  })
}

# Application
shinyApp(ui, server)