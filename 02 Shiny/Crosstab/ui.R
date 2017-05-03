#ui.R
require(shiny)
require(shinydashboard)
require(DT)
#require(leaflet)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crosstab", tabName = "crosstab", icon = icon("dashboard")))
  ),
  dashboardBody(    
    tabItems(
      # Begin Crosstab tab content.
      tabItem(tabName = "crosstab",
        tabsetPanel(
            tabPanel("Data",  
              sliderInput("KPI1", "KPI_Low:", 
                          min = 0, max = 50,  value = 33),
              sliderInput("KPI2", "KPI_Medium:", 
                          min = 50.1, max = 100,  value = 67),
              actionButton(inputId = "click1",  label = "To get data, click here"),
              hr(), # Add space after button.
              DT::dataTableOutput("data1")
            ),
            tabPanel("Crosstab", plotOutput("plot1", height=1000))
          )
        )
      # End Crosstab tab content.
    )
  )
)

