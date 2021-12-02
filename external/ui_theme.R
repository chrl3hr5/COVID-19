Theme <- shinyDashboardThemeDIY(
  # General
  appFontFamily = "Maven Pro",
  appFontColor = "rgb(255,255,255)",
  bodyBackColor = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgba(255,255,255,1)",
    colorMiddle = "rgba(65,179,230,0.8)",
    colorEnd = "rgba(255,255,255,1)",
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
    colorMiddle = "rgba(65,179,230,0.8)",
    colorEnd = "rgb(255,255,255)",
    colorStartPos = 0,
    colorMiddlePos = 70,
    colorEndPos = 100
  ),
  headerBoxShadowSize = "0px 0px 0px",
  headerBoxShadowColor = "#aaaaaa",
  
  # Sidebar
  sidebarBackColor = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgb(23,103,124)",
    colorMiddle = "rgba(65,179,230,0.8)",
    colorEnd = "rgb(255,255,255)",
    colorStartPos = 0,
    colorMiddlePos = 60,
    colorEndPos = 100
  ),
  sidebarShadowRadius = "1px 5px 5px",
  sidebarShadowColor = "rgb(255,255,255)",
  sidebarPadding = 0,
  sidebarMenuBackColor = "transparent",
  sidebarMenuPadding = 3,
  sidebarMenuBorderRadius = 3,
  sidebarUserTextColor = "rgb(255,255,255)",
  sidebarTabTextColor = "rgb(220,220,220)",
  sidebarTabTextSize = 16,
  sidebarTabBorderStyle = "none none none none",
  sidebarTabBorderColor = "rgb(35,106,135)",
  sidebarTabBorderWidth = 1,
  sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgb(65,179,220)",
    colorMiddle = "rgba(65,179,255,0.7)",
    colorEnd = "rgb(65,179,235)",
    colorStartPos = 0,
    colorMiddlePos = 50,
    colorEndPos = 100
  ),
  sidebarTabTextColorSelected = "rgb(255,255,255)",
  sidebarTabRadiusSelected = "0px 20px 20px 0px",
  sidebarTabBorderStyleHover = "none none solid none",
  sidebarTabBorderColorHover = "rgb(75,126,151)",
  sidebarTabBorderWidthHover = 1,
  sidebarTabRadiusHover = "0px 20px 20px 0px",
  sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgba(255,255,255,1)",
    colorMiddle = "rgba(255,255,255,0.9)",
    colorEnd = "rgba(255,255,255,1)",
    colorStartPos = 0,
    colorMiddlePos = 50,
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
  boxPrimaryColor = "rgba(65,170,163,1)",
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