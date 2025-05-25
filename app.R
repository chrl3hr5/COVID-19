# Loading libraries
source("external/libraries.R")

# Loading and manipulating data
source("external/data.R")

# User Interface
source("external/ui_theme.R")

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
        "Dashboard", box(leafletOutput(outputId = "COVID Map"), br(),
          fluidRow(box(width = 12, solidHeader = TRUE, title = span(icon("laptop-code"), "SELECT VARIABLE"), status = "primary", selectInput(inputId = "Information", width = "100%", label = NULL, choices = colnames(`Manipulated Data`)[!colnames(`Manipulated Data`) %in% c("Id", "Date", "Iso alpha 3", "Iso alpha 2", "Iso numeric", "Currency", "Administrative area level", "Administrative area level 1", "Administrative area level 2", "Administrative area level 3", "Latitude", "Longitude", "Key", "Key apple mobility", "Key google mobility")], selected = "Tests"))),
          fluidRow(box(width = 12, solidHeader = TRUE, title = span(icon("calendar-alt"), "CHOOSE TIME PERIOD"), status = "primary", div(style = "margin: auto; width: 95%", sliderInput(inputId = "Time", width = "100%", label = NULL, min = `Manipulated Data`$Date[1], max = `Manipulated Data`$Date[length(`Manipulated Data`$Date)], timeFormat = "%F", value = `Manipulated Data`$Date[1])))),
          width = 16, solidHeader = TRUE, title = span(icon("globe"), "COVID-19 MAP"), title_side = "top left", collapsible = F, status = "primary"
        )
      ),
      tabItem(
        "Visual",
        box(plotlyOutput(outputId = "Plots", height = "325px"), br(),
          width = 16, color = "blue", title = span(icon("chart-line"), "COVID-19 GRAPH"), title_side = "top left", collapsible = F, status = "primary", solidHeader = T,
          fluidRow(
            column(4, box(width = 12, solidHeader = TRUE, title = span(icon("flag"), "SELECT COUNTRY"), status = "primary", selectInput(inputId = "Select_country", width = "100%", label = NULL, choices = unique(`Manipulated Data`$`Administrative area level 1`), selected = "India"))),
            column(4, box(width = 12, solidHeader = TRUE, title = "SELECT X-AXIS VARIABLE", status = "primary", selectInput(inputId = "Plot_x-axis", width = "100%", label = NULL, choices = colnames(`Manipulated Data`)[!colnames(`Manipulated Data`) %in% c("Id", "Iso alpha 3", "Iso alpha 2", "Currency", "Administrative area level", "Administrative area level 1", "Administrative area level 2", "Administrative area level 3", "Latitude", "Longitude", "Key", "Key apple mobility", "Key google mobility")], selected = "Date"))),
            column(4, box(width = 12, solidHeader = TRUE, title = "SELECT Y-AXIS VARIABLE", status = "primary", selectInput(inputId = "Plot_y-axis", width = "100%", label = NULL, choices = colnames(`Manipulated Data`)[!colnames(`Manipulated Data`) %in% c("Id", "Date", "Iso alpha 3", "Iso alpha 2", "Currency", "Administrative area level", "Administrative area level 1", "Administrative area level 2", "Administrative area level 3", "Latitude", "Longitude", "Key", "Key apple mobility", "Key google mobility")], selected = "Tests")))
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$`COVID Map` <- renderLeaflet({
    `Map Data` <- filter(`Manipulated Data`, Date == input$Time)[, c(input$Information)]
    wrld_simpl@data <- cbind(wrld_simpl@data, as.vector(as.matrix(`Map Data`))[Locations])
    colnames(wrld_simpl@data)[ncol(wrld_simpl@data)] <- "map_value"
    Palette <- colorNumeric(palette = "viridis", domain = wrld_simpl@data$map_value, na.color = "transparent")
    wrld_simpl@data$label <- with(wrld_simpl@data, paste("<p> <b>", NAME, "</b> </br>", input$Time, "</br>", input$Information, ":", map_value, "</p>"))
    wrld_simpl@data$string <- paste(as.character.factor(wrld_simpl@data$NAME), rep("(Click for more)", times = nrow(wrld_simpl@data)))
    leaflet(wrld_simpl, options = leafletOptions(minZoom = 2)) %>%
      setView(lng = 78, lat = 20, zoom = 2) %>%
      addTiles() %>%
      setMaxBounds(lng1 = 180, lat1 = 84, lng2 = -140, lat2 = -84) %>%
      addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
      addPolygons(fillColor = ~ Palette(map_value), stroke = F, popup = ~label, label = ~string, highlight = highlightOptions(weight = 2, fillOpacity = 0.5, color = "black", opacity = 0.5, bringToFront = TRUE, sendToBack = TRUE))
  })
  output$Plots <- renderPlotly({
    canvas <- ggplot(droplevels(`Manipulated Data`[which(`Manipulated Data`$`Administrative area level 1` == input$`Select_country`), ]), aes(x = eval(parse(text = paste0("`", input$`Plot_x-axis`, "`"))), y = eval(parse(text = paste0("`", input$`Plot_y-axis`, "`"))))) +
      coord_cartesian()
    `select colors` <- function(n) {
      hues <- seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    colors <- `select colors`(length(unique(`Manipulated Data`$Id)))
    plot <- canvas + geom_line(lwd = 0.25) +
      suppressWarnings(geom_line(aes(
        group = 1,
        text = paste(
          input$`Plot_x-axis`, ":", eval(parse(text = paste0("`", input$`Plot_x-axis`, "`"))), "\n",
          input$`Plot_y-axis`, ":", eval(parse(text = paste0("`", input$`Plot_y-axis`, "`")))
        )
      ),
      alpha = 0.75,
      colour = colors[which(unique(`Manipulated Data`$`Administrative area level 1`) == input$`Select_country`)]
      )) +
      labs(x = input$`Plot_x-axis`, y = input$`Plot_y-axis`) +
      theme_bw() +
      theme(legend.position = "none", panel.border = element_blank(), rect = element_rect(fill = "transparent"))
    ggplotly(plot, tooltip = "text") %>% layout(plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
}

# Application
shinyApp(ui, server)