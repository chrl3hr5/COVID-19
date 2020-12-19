# Loading libraries
library(tidyverse) # For data analysis
library(data.table) # For performance
library(skimr) # For data summary
library(COVID19) # For COVID-19 data
library(shiny) # For R Shiny
library(semantic.dashboard) # For Semantic UI dashboard
library(leaflet) # For maps
library(styler) # For formatting R code
library(waiter) # Loading screens

# Loading data
Data <- as.data.table(covid19()) # Worldwide data by country

# User Interface
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Visualization", tabName = "Visual", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "Dashboard", box(leafletOutput(outputId = "COVID Map"), selectInput(inputId = "Map", label = "List of Maps", choices = names(providers), selected = "Stamen.Watercolor"), width = 16, solidHeader = TRUE, title = "COVID-19 MAP")
      ),
      tabItem(
        "Visual",
        box(plotOutput(outputId = "Plots"), width = 16)
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$`COVID Map` <- renderLeaflet({
    leaflet(filter(Data, !is.na(latitude) & !is.na(longitude))) %>%
      setView(lng = 78, lat = 20, zoom = 4) %>%
      addTiles() %>%
      addProviderTiles(input$Map)
  })
  output$Plots <- renderPlot({
    ggplot(Data) +
      geom_point(aes(x = population, y = recovered, color = tests)) +
      labs(x = "Population", y = "Recovered") +
      theme_bw() +
      theme(legend.position = NULL)
  })
}

# Application
shinyApp(ui, server)