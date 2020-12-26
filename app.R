# Loading libraries
library(tidyverse) # For data analysis
library(plotly) # For data visualization
library(data.table) # For performance
library(skimr) # For data summary
library(COVID19) # For COVID-19 data
library(shiny) # For R Shiny
library(semantic.dashboard) # For Semantic UI dashboard
library(leaflet) # For maps
library(styler) # For formatting R code
library(waiter) # For loading screens

# Loading data
Data <- covid19() # Worldwide data by country
colnames(Data) <- gsub("_", " ", str_to_title(colnames(Data))) # Reformatting column names

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
        "Dashboard", box(leafletOutput(outputId = "COVID Map"), selectInput(inputId = "Information", label = "Select variable", choices = colnames(Data)[!colnames(Data) %in% c("Id", "Iso alpha 3", "Iso alpha 2", "Currency", "Administrative area level", "Administrative area level 1", "Administrative area level 2", "Administrative area level 3", "Latitude", "Longitude", "Key", "Key apple mobility", "Key google mobility")], selected = "Tests"), sliderInput(inputId = "Time", label = "Choose date", min = Data$Date[1], max = Data$Date[length(Data$Date)], timeFormat = "%F", value = Data$Date[1]), width = 16, solidHeader = TRUE, title = "COVID-19 MAP", title_side = "top left", collapsible = F)
      ),
      tabItem(
        "Visual",
        fluidRow(box(plotlyOutput(outputId = "Plots"), width = 16, color = "blue", ribbon = T, title = "Recovery", title_side = "top left", collapsible = F, fluidRow(selectInput(inputId = "Plot x-axis", label = "Select x-axis variable", choices = colnames(Data)[!colnames(Data) %in% c("Id", "Iso alpha 3", "Iso alpha 2", "Currency", "Administrative area level", "Administrative area level 1", "Administrative area level 2", "Administrative area level 3", "Latitude", "Longitude", "Key", "Key apple mobility", "Key google mobility")]), selectInput(inputId = "Plot y-axis", label = "Select y-axis variable", choices = colnames(Data)[!colnames(Data) %in% c("Id", "Date", "Iso alpha 3", "Iso alpha 2", "Currency", "Administrative area level", "Administrative area level 1", "Administrative area level 2", "Administrative area level 3", "Latitude", "Longitude", "Key", "Key apple mobility", "Key google mobility")]), selectInput(inputId = "Plot color", label = "Select color variable", choices = colnames(Data)[colnames(Data) %in% c("Id", "Iso alpha 3", "Iso alpha 2", "Currency", "Administrative area level 1", "Key apple mobility", "Key google mobility")]))))
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$`COVID Map` <- renderLeaflet({
    leaflet(filter(Data, !is.na(Latitude) & !is.na(Longitude)), options = leafletOptions(minZoom = 2)) %>%
      setView(lng = 78, lat = 20, zoom = 2) %>%
      addTiles() %>%
      addProviderTiles("Stamen.Watercolor")
  })
  output$Plots <- renderPlotly({
    ggplotly(ggplot(Data) +
      geom_point(aes(x = eval(parse(text = paste0("`", input$`Plot x-axis`, "`"))), y = eval(parse(text = paste0("`", input$`Plot y-axis`, "`"))), color = eval(parse(text = paste0("`", input$`Plot color`, "`"))))) +
      labs(x = input$`Plot x-axis`, y = input$`Plot y-axis`) +
      theme_bw() +
      theme(legend.position = "none"))
  })
}

# Application
shinyApp(ui, server)