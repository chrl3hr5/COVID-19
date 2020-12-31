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
`Manipulated Data` <- cbind(Data[, c(1:2)], `Manipulated Data`, Data[, c(10, 22:35)])
for (i in 1:length(unique(`Manipulated Data`$Id)))
{
  `Manipulated Data`[head(which(`Manipulated Data`$Id == unique(`Manipulated Data`$Id)[i]), 1), ] <- Data[head(which(`Manipulated Data`$Id == unique(`Manipulated Data`$Id)[i]), 1), ]
}

# Polygon data
`Manipulated Data` <- `Manipulated Data`[-which(`Manipulated Data`$`Iso alpha 3` %in% unique(`Manipulated Data`$`Iso alpha 3`)[which(!unique(`Manipulated Data`$`Iso alpha 3`) %in% wrld_simpl@data$ISO3)]), ]
wrld_simpl@data <- wrld_simpl@data[which(wrld_simpl@data$ISO3 %in% unique(`Manipulated Data`$`Iso alpha 3`)), ]
wrld_simpl@polygons <- wrld_simpl@polygons[which(wrld_simpl@data$ISO3 %in% unique(`Manipulated Data`$Id))]
wrld_simpl@plotOrder <- wrld_simpl@plotOrder[which(wrld_simpl@plotOrder %in% which(wrld_simpl@data$ISO3 %in% unique(`Manipulated Data`$Id)))]

# User Interface
Theme <- shinyDashboardThemeDIY(
  # General
  appFontFamily = "Arial",
  appFontColor = "rgb(0,0,0)",
  bodyBackColor = "rgb(248,248,248)",

  # Header
  logoBackColor = "rgb(23,103,124)",
  headerButtonBackColor = "rgb(56,161,187)",
  headerButtonIconColor = "rgb(238,238,238)",
  headerButtonBackColorHover = "rgb(56,161,187)",
  headerButtonIconColorHover = "rgb(0,0,0)",
  headerBackColor = "rgb(56,161,187)",
  headerBoxShadowSize = "0px 0px 0px",
  headerBoxShadowColor = "#aaaaaa",

  # Sidebar
  sidebarBackColor = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgb(20,97,117)",
    colorMiddle = "rgb(56,161,187)",
    colorEnd = "rgb(3,22,56)",
    colorStartPos = 0,
    colorMiddlePos = 50,
    colorEndPos = 100
  ),
  sidebarShadowRadius = "3px 5px 5px",
  sidebarShadowColor = "#aaaaaa",
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
    colorStart = "rgba(44,222,235,1)",
    colorMiddle = "rgba(44,222,235,1)",
    colorEnd = "rgba(0,255,213,1)",
    colorStartPos = 0,
    colorMiddlePos = 30,
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
    colorStart = "rgba(44,222,235,1)",
    colorMiddle = "rgba(44,222,235,1)",
    colorEnd = "rgba(0,255,213,1)",
    colorStartPos = 0,
    colorMiddlePos = 30,
    colorEndPos = 100
  ),
  sidebarTabTextColorHover = "rgb(50,50,50)",
  sidebarSearchBackColor = "rgb(55,72,80)",
  sidebarSearchBorderColor = "rgb(55,72,80)",
  sidebarSearchIconColor = "rgb(153,153,153)",

  # Box
  boxBackColor = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgba(44,222,235,1)",
    colorMiddle = "rgba(44,222,235,1)",
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
  tabBoxHighlightColor = "rgba(44,222,235,1)",
  tabBoxTabTextColorSelected = "rgb(0,0,0)",
  tabBoxTabTextSize = 14,
  tabBoxTabTextColor = "rgb(0,0,0)",
  tabBoxBackColor = "rgb(255,255,255)",
  boxDefaultColor = "rgb(210,214,220)",
  boxPrimaryColor = "rgba(44,222,235,1)",
  boxSuccessColor = "rgba(0,255,213,1)",
  boxWarningColor = "rgb(244,156,104)",
  boxDangerColor = "rgb(255,88,55)",

  # Input
  textboxBorderRadius = 5,
  buttonBackColor = "rgb(245,245,245)",
  buttonTextColor = "rgb(0,0,0)",
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
  boldText = "COVID19",
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
    tabItems(
      tabItem(
        "Dashboard", box(leafletOutput(outputId = "COVID Map"), br(),
          fluidRow(box(width = 12, solidHeader = TRUE, title = "Select variable", status = "primary", selectInput(inputId = "Information", width = "100%", label = NULL, choices = colnames(`Manipulated Data`)[!colnames(`Manipulated Data`) %in% c("Id", "Iso alpha 3", "Iso alpha 2", "Currency", "Administrative area level", "Administrative area level 1", "Administrative area level 2", "Administrative area level 3", "Latitude", "Longitude", "Key", "Key apple mobility", "Key google mobility")], selected = "Tests"))),
          fluidRow(box(width = 12, solidHeader = TRUE, title = "Choose date", status = "primary", sliderInput(inputId = "Time", width = "100%", label = NULL, min = `Manipulated Data`$Date[1], max = `Manipulated Data`$Date[length(`Manipulated Data`$Date)], timeFormat = "%F", value = `Manipulated Data`$Date[1]))),
          width = 16, solidHeader = TRUE, title = "COVID-19 MAP", title_side = "top left", collapsible = F, status = "primary"
        )
      ),
      tabItem(
        "Visual",
        box(plotlyOutput(outputId = "Plots"),
          width = 16, color = "blue", title = "COVID-19 Graph", title_side = "top left", collapsible = F, status = "primary", solidHeader = T,
          selectInput(inputId = "Select country", width = "100%", label = "Select country", choices = unique(`Manipulated Data`$`Administrative area level 1`)),
          selectInput(inputId = "Plot x-axis", width = "100%", label = "Select x-axis variable", choices = colnames(`Manipulated Data`)[!colnames(`Manipulated Data`) %in% c("Id", "Iso alpha 3", "Iso alpha 2", "Currency", "Administrative area level", "Administrative area level 1", "Administrative area level 2", "Administrative area level 3", "Latitude", "Longitude", "Key", "Key apple mobility", "Key google mobility")]),
          selectInput(inputId = "Plot y-axis", width = "100%", label = "Select y-axis variable", choices = colnames(`Manipulated Data`)[!colnames(`Manipulated Data`) %in% c("Id", "Date", "Iso alpha 3", "Iso alpha 2", "Currency", "Administrative area level", "Administrative area level 1", "Administrative area level 2", "Administrative area level 3", "Latitude", "Longitude", "Key", "Key apple mobility", "Key google mobility")])
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$`COVID Map` <- renderLeaflet({
    `Map Data` <- filter(`Manipulated Data`, Date == input$Time)[, c(input$Information)]
    wrld_simpl@data <- cbind(wrld_simpl@data, `Map Data`)
    colnames(wrld_simpl@data)[ncol(wrld_simpl@data)] <- "map_value"
    Palette <- colorNumeric(palette = "viridis", domain = wrld_simpl@data$map_value, na.color = "transparent")
    leaflet(wrld_simpl, options = leafletOptions(minZoom = 2)) %>%
      setView(lng = 78, lat = 20, zoom = 2) %>%
      addTiles() %>%
      setMaxBounds(lng1 = 180, lat1 = 84, lng2 = -140, lat2 = -84) %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      addPolygons(fillColor = ~ Palette(map_value), stroke = F)
  })
  output$Plots <- renderPlotly({
    canvas <- ggplot(`Manipulated Data`[which(`Manipulated Data`$`Administrative area level 1` == input$`Select country`),], aes(x = eval(parse(text = paste0("`", input$`Plot x-axis`, "`"))), y = eval(parse(text = paste0("`", input$`Plot y-axis`, "`"))))) +
      coord_cartesian()
    plot <- canvas + geom_point(aes(
      text = paste(
        input$`Plot x-axis`, ":", eval(parse(text = paste0("`", input$`Plot x-axis`, "`"))), "\n",
        input$`Plot y-axis`, ":", eval(parse(text = paste0("`", input$`Plot y-axis`, "`")))
      ), color = input$`Select country`
    )) +
      labs(x = input$`Plot x-axis`, y = input$`Plot y-axis`) +
      theme_bw() +
      theme(legend.position = "none", panel.border = element_blank())
    ggplotly(plot, tooltip = "text")
  })
}

# Application
shinyApp(ui, server)