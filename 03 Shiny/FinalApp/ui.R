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
      menuItem("Boxplot", tabName = "Bplot", icon = icon("dashboard")),
      menuItem("World Map", tabName = "Map", icon = icon("dashboard")),
     menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard"))
      )
  ),
  
  dashboardBody(
    tabItems(
      # Data tab
      tabItem(tabName= "getData", 
              tabsetPanel(
                tabPanel("Data",
                         DT::dataTableOutput("table1")
                         )
                )
              ),
      
      #Scatterplot tab
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
      
      #Boxplot tab
      tabItem(tabName = "Bplot",
              tabsetPanel(
                tabPanel("Boxplot",  
                         radioButtons("rb5", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         uiOutput("boxplotRegions"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click5",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("boxplotData1")
                ),
                tabPanel("Simple Box Plot", 
                         sliderInput("boxSalesRange1", "Happiness Range:", # See https://shiny.rstudio.com/articles/sliders.html
                                     min = min(df2$Happiness.Score), max = max(df2$Happiness.Score), 
                                     value = c(min(df2$Happiness.Score), max(df2$Happiness.Score))),
                         
                         plotlyOutput("boxplotPlot1", height=500))
              )
      ),
      
      #Map tab
      tabItem(tabName = "Map",
                  tabsetPanel(
                    tabPanel("World Map", leafletOutput("plot3", height=500) )
                  )
      ),
      
        #Begin Barchart tab content.
        tabItem(tabName = "barchart",
                tabsetPanel(
                  tabPanel("Data",  
                           
                           uiOutput("regions2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                           
                           actionButton(inputId = "click2",  label = "To get data, click here"), hr(), # Add space after button.
                           
                           'Here is data for the "Barchart with Table Calculation" tab', hr(),
                           DT::dataTableOutput("barchartData1")
                  ),
                  tabPanel("Barchart with Table Calculation", "Black = Sum of Sales per Region, Red = Average Sum of Sales per Category, and  Blue = (Sum of Sales per Region - Average Sum of Sales per Category)", plotOutput("barchartPlot1", height=1500))
                )
        )
        # End Barchart tab content.
      
      #Crosstab tab
      
      
    )
  )
)

