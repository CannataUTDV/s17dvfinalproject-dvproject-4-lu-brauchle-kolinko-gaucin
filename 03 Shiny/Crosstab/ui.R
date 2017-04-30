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
              radioButtons("rb1", "Get data from:",
                c("SQL" = "SQL",
                  "CSV" = "CSV"), inline=T),
              sliderInput("KPI1", "KPI_Low:", 
                          min = 0.66, max = 1.5,  value = 1),
              sliderInput("KPI2", "KPI_Medium:", 
                          min = 1.5, max = 2.33,  value = 2),
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

