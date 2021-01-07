# Loading libraries
library(tidyverse) # For data analysis
library(plotly) # For data visualization
library(skimr) # For data summary
library(COVID19) # For COVID-19 data
library(shiny) # For R Shiny
library(shinydashboard) # For Shiny dashboard
library(leaflet) # For maps
library(styler) # For formatting R code
library(waiter) # For loading screens
library(dashboardthemes) # For adding themes
library(maptools) # For shape files

# Loading data
data(wrld_simpl) # World polygons
Data <- covid19() # Worldwide COVID-19 data by country
colnames(Data) <- gsub("_", " ", str_to_title(colnames(Data))) # Reformatting column names

# Manipulating data
# COVID-19 data
`Cumulative to Individual` <- function(x) {
  if (length(x) > 1) {
    abs(x[2:length(x)] - x[1:(length(x) - 1)])
  } else {
    cat("Vector length not sufficient!\n")
  }
}
`Manipulated Data` <- rbind(Data[1, c(3:9, 11:21)], apply(Data[, c(3:9, 11:21)], 2, `Cumulative to Individual`))
`Manipulated Data` <- cbind(Data[, c(1:2)], `Manipulated Data`, Data[, c(10, 22:36)])
for (i in 1:length(unique(`Manipulated Data`$Id)))
{
  `Manipulated Data`[head(which(`Manipulated Data`$Id == unique(`Manipulated Data`$Id)[i]), 1), ] <- Data[head(which(`Manipulated Data`$Id == unique(`Manipulated Data`$Id)[i]), 1), ]
}

# Polygon data
Positions <- as.data.frame(cbind(rep(unique(`Manipulated Data`$Id), times = length(wrld_simpl@data$ISO3)), rep(as.character.factor(wrld_simpl@data$ISO3), each = length(unique(`Manipulated Data`$Id)))))
Unique <- length(unique(Positions$V1))
Locations <- NULL
for (i in 1:(nrow(Positions) / Unique))
{
  Locations[i] <- if (length(which(unname(apply(Positions[1:Unique, ], 1, function(t) {
    length(unique(t)) == 1
  })))) == 1) {
    which(unname(apply(Positions[1:Unique, ], 1, function(t) {
      length(unique(t)) == 1
    })))
  } else {
    NA
  }
  Positions <- tail(Positions, -Unique)
}

# User Interface
Theme <- shinyDashboardThemeDIY(
  # General
  appFontFamily = "Maven Pro",
  appFontColor = "rgb(255,255,255)",
  bodyBackColor = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgba(65,179,163,1)",
    colorMiddle = "rgba(65,179,230,1)",
    colorEnd = "rgba(65,179,163,1)",
    colorStartPos = 0,
    colorMiddlePos = 50,
    colorEndPos = 100
  ),

  # Header
  logoBackColor = "rgb(23,103,124)",
  headerButtonBackColor = "rgb(23,103,124)",
  headerButtonIconColor = "rgb(238,238,238)",
  headerButtonBackColorHover = "rgb(23,103,124)",
  headerButtonIconColorHover = "rgb(0,0,0)",
  headerBackColor = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgb(23,103,124)",
    colorMiddle = "rgb(65,179,230)",
    colorEnd = "rgb(65,179,230)",
    colorStartPos = 0,
    colorMiddlePos = 75,
    colorEndPos = 100
  ),
  headerBoxShadowSize = "0px 0px 0px",
  headerBoxShadowColor = "#aaaaaa",

  # Sidebar
  sidebarBackColor = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgb(20,97,117)",
    colorMiddle = "rgb(65,179,230)",
    colorEnd = "rgb(56,161,187)",
    colorStartPos = 0,
    colorMiddlePos = 50,
    colorEndPos = 100
  ),
  sidebarShadowRadius = "1px 5px 5px",
  sidebarShadowColor = "rgb(255,255,255)",
  sidebarPadding = 0,
  sidebarMenuBackColor = "transparent",
  sidebarMenuPadding = 0,
  sidebarMenuBorderRadius = 0,
  sidebarUserTextColor = "rgb(255,255,255)",
  sidebarTabTextColor = "rgb(255,255,255)",
  sidebarTabTextSize = 13,
  sidebarTabBorderStyle = "none none solid none",
  sidebarTabBorderColor = "rgb(35,106,135)",
  sidebarTabBorderWidth = 1,
  sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgb(65,179,220)",
    colorMiddle = "rgb(65,179,230)",
    colorEnd = "rgb(65,179,220)",
    colorStartPos = 0,
    colorMiddlePos = 50,
    colorEndPos = 100
  ),
  sidebarTabTextColorSelected = "rgb(0,0,0)",
  sidebarTabRadiusSelected = "0px 20px 20px 0px",
  sidebarTabBorderStyleHover = "none none solid none",
  sidebarTabBorderColorHover = "rgb(75,126,151)",
  sidebarTabBorderWidthHover = 1,
  sidebarTabRadiusHover = "0px 20px 20px 0px",
  sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgba(255,255,255,0.8)",
    colorMiddle = "rgba(255,255,255,0.8)",
    colorEnd = "rgba(255,255,255,0.8)",
    colorStartPos = 90,
    colorMiddlePos = 95,
    colorEndPos = 100
  ),
  sidebarTabTextColorHover = "rgb(50,50,50)",
  sidebarSearchBackColor = "rgb(55,72,80)",
  sidebarSearchBorderColor = "rgb(55,72,80)",
  sidebarSearchIconColor = "rgb(153,153,153)",

  # Box
  boxBackColor = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgba(65,179,163,1)",
    colorMiddle = "rgba(65,179,230,0.5)",
    colorEnd = "rgba(255,255,255,1)",
    colorStartPos = 0,
    colorMiddlePos = 10,
    colorEndPos = 100
  ),
  boxBorderRadius = 10,
  boxShadowSize = "0px 5px 5px",
  boxShadowColor = "rgba(23,103,124,0.25)",
  boxTitleSize = 16,
  tabBoxTabColor = "rgb(255,255,255)",
  tabBoxBorderRadius = 5,
  tabBoxHighlightColor = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgba(65,179,163,1)",
    colorMiddle = "rgba(65,179,230,0.5)",
    colorEnd = "rgba(255,255,255,1)",
    colorStartPos = 0,
    colorMiddlePos = 10,
    colorEndPos = 100
  ),
  tabBoxTabTextColorSelected = "rgb(0,0,0)",
  tabBoxTabTextSize = 14,
  tabBoxTabTextColor = "rgb(255,255,255)",
  tabBoxBackColor = "rgb(255,255,255)",
  boxDefaultColor = "rgb(210,214,220)",
  boxPrimaryColor = "rgba(65,179,230,1)",
  boxSuccessColor = "rgba(0,255,213,1)",
  boxWarningColor = "rgb(244,156,104)",
  boxDangerColor = "rgb(255,88,55)",

  # Input
  textboxBorderRadius = 5,
  buttonBackColor = "rgb(245,245,245)",
  buttonTextColor = "rgb(255,255,255)",
  buttonBorderColor = "rgb(200,200,200)",
  buttonBorderRadius = 5,
  buttonBackColorHover = "rgb(235,235,235)",
  buttonTextColorHover = "rgb(100,100,100)",
  buttonBorderColorHover = "rgb(200,200,200)",
  textboxBackColor = "rgb(255,255,255)",
  textboxBorderColor = "rgb(200,200,200)",
  textboxBackColorSelect = "rgb(245,245,245)",
  textboxBorderColorSelect = "rgb(200,200,200)",

  # Table
  tableBackColor = "rgb(255,255,255)",
  tableBorderRowSize = 1,
  tableBorderColor = "rgb(240,240,240)",
  tableBorderTopSize = 1
)

Logo <- shinyDashboardLogoDIY(
  boldText = "COVID-19",
  mainText = "Tracker",
  textSize = 16,
  badgeText = "",
  badgeTextColor = "rgb(23,103,124)",
  badgeTextSize = 2,
  badgeBackColor = "rgb(23,103,124)",
  badgeBorderRadius = 3
)

ui <- dashboardPage(
  dashboardHeader(title = Logo),
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
          fluidRow(box(width = 12, solidHeader = TRUE, title = span(icon("laptop-code"), "SELECT VARIABLE"), status = "primary", selectInput(inputId = "Information", width = "100%", label = NULL, choices = colnames(`Manipulated Data`)[!colnames(`Manipulated Data`) %in% c("Id", "Iso alpha 3", "Iso alpha 2", "Currency", "Administrative area level", "Administrative area level 1", "Administrative area level 2", "Administrative area level 3", "Latitude", "Longitude", "Key", "Key apple mobility", "Key google mobility")], selected = "Tests"))),
          fluidRow(box(width = 12, solidHeader = TRUE, title = span(icon("calendar-alt"), "CHOOSE TIME PERIOD"), status = "primary", div(style = "margin: auto; width: 95%", sliderInput(inputId = "Time", width = "100%", label = NULL, min = `Manipulated Data`$Date[1], max = `Manipulated Data`$Date[length(`Manipulated Data`$Date)], timeFormat = "%F", value = `Manipulated Data`$Date[1])))),
          width = 16, solidHeader = TRUE, title = span(icon("globe"), "COVID-19 MAP"), title_side = "top left", collapsible = F, status = "primary"
        )
      ),
      tabItem(
        "Visual",
        box(plotlyOutput(outputId = "Plots"), br(),
          width = 16, color = "blue", title = span(icon("chart-line"), "COVID-19 GRAPH"), title_side = "top left", collapsible = F, status = "primary", solidHeader = T,
          fluidRow(
            column(4, box(width = 12, solidHeader = TRUE, title = span(icon("search-location"), "SELECT COUNTRY"), status = "primary", selectInput(inputId = "Select_country", width = "100%", label = NULL, choices = unique(`Manipulated Data`$`Administrative area level 1`), selected = "India"))),
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
    plot <- canvas + suppressWarnings(geom_line(aes(
      group = 1,
      text = paste(
        input$`Plot_x-axis`, ":", eval(parse(text = paste0("`", input$`Plot_x-axis`, "`"))), "\n",
        input$`Plot_y-axis`, ":", eval(parse(text = paste0("`", input$`Plot_y-axis`, "`")))
      )
    ),
    colour = colors[which(unique(`Manipulated Data`$`Administrative area level 1`) == input$`Select_country`)]
    )) +
      labs(x = input$`Plot_x-axis`, y = input$`Plot_y-axis`) +
      theme_bw() +
      theme(legend.position = "none", panel.border = element_blank())
    ggplotly(plot, tooltip = "text")
  })
}

# Application
shinyApp(ui, server)