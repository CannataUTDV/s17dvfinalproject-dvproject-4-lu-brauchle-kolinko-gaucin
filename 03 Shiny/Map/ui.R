
#ui.R

require(shiny)
require(shinydashboard)
require(DT)
require(leaflet)
require(plotly)


dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Happiness Score Map", tabName = "Happiness Score Map", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItem(tabName = "Happiness Score Map",
            tabsetPanel(
              tabPanel("Happiness Score Map", leafletOutput("map", height=500) )
            )
    )
  )
)

