# Loading libraries
source("external/libraries.R")

# Loading and manipulating data
source("data/data.R")

# User Interface
source("external/theme.R")

# Setting loading spinner parameters
options(spinner.type = 6, spinner.color = "#0dc5c1", spinner.size = 0.5)

# UI
ui <- dashboardPage(title = "COVID-19 Tracker",
  dashboardHeader(title = tagList(span(icon("head-side-mask", "fa-1x"), span("COVID-19", style = "font-size: 17px; font-weight: bold;", span("Tracker", style = "font-size: 17px; font-weight: normal;"))))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Visualization", tabName = "Visual", icon = icon("th"))
    ),
    tags$div(tags$p("Created by Digvijay Singh", style = "margin: 0;"),
             tags$p("GitHub:", tags$a("github.com/chrl3hr5", href = "https://github.com/chrl3hr5", target = "_blank", style = "color: #007BFF;"), style = "margin: 0;"),
             tags$p("LinkedIn:", tags$a("linkedin.com/in/chrl3hr5", href = "https://linkedin.com/in/chrl3hr5", target = "_blank", style = "color: #007BFF;"), style = "margin: 0;"),
    style = "position: absolute; bottom: 10px; left: 15px; font-size: 12px; color: #333333;")
  ),
  dashboardBody(
    dashboardTheme,
    tags$head(tags$style(".selectize-dropdown {position: static}")),
    tags$head(tags$style(HTML(".selectize-input {white-space: nowrap}
                              .selectize-dropdown .selectize-dropdown-content .option {color: black}
                              .selectize-dropdown-content {max-height: 90px;overflow-y: auto;}
                              body {font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;}
                              #information+ div>.selectize-input{color: black;}
                              #select_country+ div>.selectize-input{color: black;}
                              #plot_x_axis+ div>.selectize-input{color: black;}
                              #plot_y_axis+ div>.selectize-input{color: black;}
                              .white-background-box {background: white;}
                              "))),
    tags$style(type = "text/css", ".irs-grid-text {color: black; bottom: 5px; z-index: 1;}"),
    tabItems(
      tabItem(
        "Dashboard", box(withSpinner(leafletOutput(outputId = "map")), class = 'white-background-box', br(),
          fluidRow(box(width = 12, solidHeader = TRUE, title = span(icon("laptop-code"), "SELECT VARIABLE", style = "font-weight: bold; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;"), status = "primary", selectInput(inputId = "information", width = "100%", label = NULL, choices = colnames(manipulated_data)[!colnames(manipulated_data) %in% c("Id", "Date", "Administrative Area Level 1")], selected = "Confirmed"))),
          fluidRow(box(width = 12, solidHeader = TRUE, title = span(icon("calendar-alt"), "CHOOSE TIME PERIOD", style = "font-weight: bold; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;"), status = "primary", div(style = "margin: auto; width: 95%", sliderInput(inputId = "time", width = "100%", label = NULL, min = manipulated_data$Date[1], max = manipulated_data$Date[length(manipulated_data$Date)], timeFormat = "%F", value = manipulated_data$Date[1])))),
          width = 16, solidHeader = TRUE, title = span(icon("globe"), "COVID-19 MAP", style = "font-weight: bold; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;"), title_side = "top left", collapsible = F, status = "primary"
        )
      ),
      tabItem(
        "Visual",
        box(withSpinner(plotlyOutput(outputId = "Plots", height = "325px")), class = 'white-background-box', br(),
          width = 16, color = "blue", title = span(icon("chart-line"), "COVID-19 GRAPH", style = "font-weight: bold; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;"), title_side = "top left", collapsible = F, status = "primary", solidHeader = T,
          fluidRow(
            column(4, box(width = 12, solidHeader = TRUE, title = span(icon("flag"), "SELECT COUNTRY", style = "font-weight: bold; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;"), status = "primary", selectInput(inputId = "select_country", width = "100%", label = NULL, choices = unique(manipulated_data$`Administrative Area Level 1`), selected = "India"))),
            column(4, box(width = 12, solidHeader = TRUE, title = span("SELECT X-AXIS VARIABLE", style = "font-weight: bold; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;"), status = "primary", selectInput(inputId = "plot_x_axis", width = "100%", label = NULL, choices = colnames(manipulated_data)[!colnames(manipulated_data) %in% c("Id", "Administrative Area Level 1")], selected = "Date"))),
            column(4, box(width = 12, solidHeader = TRUE, title = span("SELECT Y-AXIS VARIABLE", style = "font-weight: bold; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;"), status = "primary", selectInput(inputId = "plot_y_axis", width = "100%", label = NULL, choices = colnames(manipulated_data)[!colnames(manipulated_data) %in% c("Id", "Date", "Administrative Area Level 1")], selected = "Confirmed")))
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Obtaining data to display on map based on the selected variable and time period
  map_data_merged <- reactive({req(input$time, input$information)
    filtered_data <- manipulated_data %>% filter(Date == input$time) %>% mutate(
      map_value = as.numeric(.data[[input$information]]),
      shapeName = `Administrative Area Level 1`, .keep = "none")
    values <- rep(NA, nrow(polygons))
    indexes <- match(polygons$name, filtered_data$shapeName)
    values[!is.na(indexes)] <- filtered_data$map_value[indexes[!is.na(indexes)]]
    polygons$map_value <- values
    polygons
  })
  # Adding text to show as label and popup message
  map_data_with_text <- reactive({
    req(map_data_merged())
    map_data_with_text <- map_data_merged()
    map_data_with_text$label <- paste0('<center> <p> <b>', map_data_with_text$name, '</b> </br> (Click for info) </p> </center>')
    map_data_with_text$popup <- paste0('<p> <b>', map_data_with_text$name, '</b>', '</br> <b> Date: </b>', input$time, '</br> <b>', input$information, ': </b>', ifelse(is.na(map_data_with_text$map_value), "No data", map_data_with_text$map_value),'</p>')
    map_data_with_text
  })
  # Creating palette based on country values for the selected variable
  palette <- reactive({req(map_data_with_text())
    colorNumeric(palette = "viridis", domain = map_data_with_text()$map_value, na.color = "#555555")
  })
  # Rendering map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      setView(lng = 78, lat = 20, zoom = 2) %>%
      addTiles(urlTemplate = "") %>%
      setMaxBounds(lng1 = 180, lat1 = 84, lng2 = -140, lat2 = -84)
  })
  observe({
    leafletProxy("map", data = map_data_with_text()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = ~ palette()(map_value), stroke = F, popup = lapply(map_data_with_text()$popup, HTML), label = lapply(map_data_with_text()$label, HTML), highlight = highlightOptions(weight = 2, fillOpacity = 0.5, color = "black", opacity = 0.5, bringToFront = TRUE, sendToBack = TRUE)) %>%
      addLegend("topright", pal = palette(), values = ~map_value, title = "Value", opacity = 1)
  })
  output$Plots <- renderPlotly({
    canvas <- ggplot(droplevels(manipulated_data[which(manipulated_data$`Administrative Area Level 1` == input$select_country), ]), aes(x = eval(parse(text = paste0("`", input$plot_x_axis, "`"))), y = eval(parse(text = paste0("`", input$plot_y_axis, "`"))))) +
      coord_cartesian()
    plot <- canvas + geom_line(lwd = 0.25) +
      suppressWarnings(geom_line(aes(
        group = 1,
        text = paste0(
          '<b>', input$plot_x_axis, ": </b>", eval(parse(text = paste0("`", input$plot_x_axis, "`"))), "\n",
          '<b>', input$plot_y_axis, ": </b>", eval(parse(text = paste0("`", input$plot_y_axis, "`")))
        )
      ),
      alpha = 0.75,
      color = "#0096FF"
      )) +
      labs(x = input$plot_x_axis, y = input$plot_y_axis) +
      theme_bw() +
      theme(legend.position = "none", panel.border = element_blank(), rect = element_rect(fill = "transparent"))
    ggplotly(plot, tooltip = "text") %>% layout(plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
}

# Application
shinyApp(ui, server)