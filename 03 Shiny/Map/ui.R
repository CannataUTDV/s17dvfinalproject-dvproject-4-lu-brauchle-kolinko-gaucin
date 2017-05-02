
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
      menuItem("Action Example", tabName = "action", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItem(tabName = "map",
            tabsetPanel(
              tabPanel("map", leafletOutput("map", height=500) )
            )
    )
  )
)

