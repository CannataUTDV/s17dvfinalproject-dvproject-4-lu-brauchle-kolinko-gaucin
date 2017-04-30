#ui.R
require(shiny)
require(shinydashboard)
require(DT)
#require(leaflet)
#require(plotly)

dashboardPage(
    dashboardHeader(
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),    
        tabItems(
            # Begin Barchart tab content.
            tabItem(tabName = "barchart",
                    tabsetPanel(
                        tabPanel("Data",  
                                 
                                 uiOutput("claim_types2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                                 
                                 actionButton(inputId = "click2",  label = "To get data, click here"), hr(), # Add space after button.
                                 
                                 'Here is data for the "Barchart with Table Calculation" tab', hr(),
                                 
                                 DT::dataTableOutput("barchartData1"), hr()),
                        
                        tabPanel("Barchart with Table Calculation", "Black = Avg spending of the state of the claim type, Red = Avg spending of the state, and  Blue = (Avg spending of the state of the claim type/Healthcare Spending Per Capita)", plotOutput("barchartPlot1", height=1500))
                        )
                    )
            # End Barchart tab content.
        )
    )
)

