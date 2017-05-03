#ui.R

require(shiny)
require(shinydashboard)
require(DT)
require(leaflet)
require(plotly)

dashboardPage(skin = "black",
  dashboardHeader(title="Final"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "getData", icon = icon("dashboard")),
      menuItem("Scatterplot", tabName = "Scatterplot", icon = icon("dashboard")),
      menuItem("World Map", tabName = "Map", icon = icon("dashboard"))
      )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName= "getData", 
              tabsetPanel(
                tabPanel("Data",
                         DT::dataTableOutput("table1")
                         )
                )
              ),
      
      tabItem(tabName = "Scatterplot",
              tabsetPanel(
                tabPanel("Scatterplot",
              plotOutput("plot1",
                         click = "plot_click",
                         dblclick = "plot_dblclick",
                         hover = "plot_hover",
                         brush = "plot_brush"
              ),hr(),
              plotOutput("plot2")
              )
              )
      ),  
      tabItem(tabName = "Map",
                  tabsetPanel(
                    tabPanel("World Map", leafletOutput("plot3", height=500) )
                  )
      )
    )
  )
)

