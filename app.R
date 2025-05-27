# Loading libraries
source("external/libraries.R")

# Loading and manipulating data
source("data/data.R")

# User Interface
source("external/theme.R")

# Setting loading spinner parameters
options(spinner.type = 5, spinner.color = "#0dc5c1")

# UI
ui <- dashboardPage(
  dashboardHeader(title = span(icon("head-side-mask", "fa-1x"), span("COVID-19", style = "font-size: 17px; font-weight: bold;", span("Tracker", style = "font-size: 17px; font-weight: normal;")))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Visualization", tabName = "Visual", icon = icon("th"))
    )
  ),
  dashboardBody(
    Theme,
    tags$head(tags$style(".selectize-dropdown {position: static}")),
    tags$head(tags$style(HTML(".selectize-input {white-space: nowrap} 
                              #Information+ div>.selectize-input{color: black;}
                              #Select_country+ div>.selectize-input{color: black;}
                              #Plot_x-axis+ div>.selectize-input{color: black;}
                              #Plot_y-axis+ div>.selectize-input{color: black;}"))),
    tags$style(type = "text/css", ".irs-grid-text {color: black; bottom: 5px; z-index: 1;}"),
    tabItems(
      tabItem(
        "Dashboard", box(withSpinner(leafletOutput(outputId = "map")), br(),
          fluidRow(box(width = 12, solidHeader = TRUE, title = span(icon("laptop-code"), "SELECT VARIABLE"), status = "primary", selectInput(inputId = "information", width = "100%", label = NULL, choices = colnames(manipulated_data)[!colnames(manipulated_data) %in% c("id", "date", "iso_alpha_3", "iso_alpha_2", "iso_numeric", "administrative_area_level", "administrative_area_level_1", "administrative_area_level_2", "administrative_area_level_3", "latitude", "longitude", "key_apple_mobility", "key_google_mobility")], selected = "tests"))),
          fluidRow(box(width = 12, solidHeader = TRUE, title = span(icon("calendar-alt"), "CHOOSE TIME PERIOD"), status = "primary", div(style = "margin: auto; width: 95%", sliderInput(inputId = "time", width = "100%", label = NULL, min = manipulated_data$date[1], max = manipulated_data$date[length(manipulated_data$date)], timeFormat = "%F", value = manipulated_data$date[1])))),
          width = 16, solidHeader = TRUE, title = span(icon("globe"), "COVID-19 MAP"), title_side = "top left", collapsible = F, status = "primary"
        )
      ),
      tabItem(
        "Visual",
        box(plotlyOutput(outputId = "Plots", height = "325px"), br(),
          width = 16, color = "blue", title = span(icon("chart-line"), "COVID-19 GRAPH"), title_side = "top left", collapsible = F, status = "primary", solidHeader = T,
          fluidRow(
            column(4, box(width = 12, solidHeader = TRUE, title = span(icon("flag"), "SELECT COUNTRY"), status = "primary", selectInput(inputId = "select_country", width = "100%", label = NULL, choices = unique(manipulated_data$administrative_area_level_1), selected = "India"))),
            column(4, box(width = 12, solidHeader = TRUE, title = "SELECT X-AXIS VARIABLE", status = "primary", selectInput(inputId = "Plot_x-axis", width = "100%", label = NULL, choices = colnames(manipulated_data)[!colnames(manipulated_data) %in% c("id", "iso_alpha_3", "iso_alpha_2", "administrative_area_level", "administrative_area_level_1", "administrative_area_level_2", "administrative_area_level_3", "latitude", "longitude", "key_apple_mobility", "key_google_mobility")], selected = "date"))),
            column(4, box(width = 12, solidHeader = TRUE, title = "SELECT Y-AXIS VARIABLE", status = "primary", selectInput(inputId = "Plot_y-axis", width = "100%", label = NULL, choices = colnames(manipulated_data)[!colnames(manipulated_data) %in% c("id", "date", "iso_alpha_3", "iso_alpha_2", "Currency", "administrative_area_level", "administrative_area_level_1", "administrative_area_level_2", "administrative_area_level_3", "latitude", "longitude", "key_apple_mobility", "key_google_mobility")], selected = "tests")))
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Obtaining data to display on map based on the selected variable and time period
  map_data <- reactive({req(input$time, input$information)
    manipulated_data %>% filter(date == input$time) %>% mutate(
      map_value = as.numeric(.data[[input$information]]),
      shapeName = administrative_area_level_1, .keep = "none")
  })
  # Merging the obtained data with polygon data
  map_data_merged <- reactive({
    req(map_data())
    left_join(polygons, map_data(), by = "shapeName")
  })
  # Creating palette based on country values for the selected variable
  palette <- reactive({req(map_data_merged())
    colorNumeric(palette = "viridis",domain = map_data_merged()$map_value,na.color = "#555555")
  })
  # Rendering map
  output$map <- renderLeaflet({
    leaflet(map_data_merged(), options = leafletOptions(minZoom = 2)) %>%
      setView(lng = 78, lat = 20, zoom = 2) %>%
      addTiles(urlTemplate = "") %>%
      setMaxBounds(lng1 = 180, lat1 = 84, lng2 = -140, lat2 = -84) %>%
      #addProviderTiles("Stamen.TonerLite") %>% 
      addPolygons(fillColor = ~ palette()(map_value), stroke = F, popup = ~shapeName, label = ~paste0(shapeName, ': ', ifelse(is.na(map_value), "No data", map_value)), highlight = highlightOptions(weight = 2, fillOpacity = 0.5, color = "black", opacity = 0.5, bringToFront = TRUE, sendToBack = TRUE))
  })
  output$Plots <- renderPlotly({
    canvas <- ggplot(droplevels(manipulated_data[which(manipulated_data$administrative_area_level_1 == input$select_country), ]), aes(x = eval(parse(text = paste0("`", input$`Plot_x-axis`, "`"))), y = eval(parse(text = paste0("`", input$`Plot_y-axis`, "`"))))) +
      coord_cartesian()
    `select colors` <- function(n) {
      hues <- seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    colors <- `select colors`(length(unique(manipulated_data$Id)))
    plot <- canvas + geom_line(lwd = 0.25) +
      suppressWarnings(geom_line(aes(
        group = 1,
        text = paste(
          input$`Plot_x-axis`, ":", eval(parse(text = paste0("`", input$`Plot_x-axis`, "`"))), "\n",
          input$`Plot_y-axis`, ":", eval(parse(text = paste0("`", input$`Plot_y-axis`, "`")))
        )
      ),
      alpha = 0.75,
      colour = colors[which(unique(manipulated_data$administrative_area_level_1) == input$select_country)]
      )) +
      labs(x = input$`Plot_x-axis`, y = input$`Plot_y-axis`) +
      theme_bw() +
      theme(legend.position = "none", panel.border = element_blank(), rect = element_rect(fill = "transparent"))
    ggplotly(plot, tooltip = "text") %>% layout(plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
}

# Application
shinyApp(ui, server)