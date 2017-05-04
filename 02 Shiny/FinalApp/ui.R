#ui.R

require(shiny)
require(shinydashboard)
require(DT)
require(leaflet)
require(plotly)
require(plyr)

dashboardPage(skin = "black",
  dashboardHeader(title="Final"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "getData", icon = icon("dashboard")),
      menuItem("Scatterplot", tabName = "Scatterplot", icon = icon("dashboard")),
      menuItem("Histogram", tabName = "histogram", icon = icon("dashboard")),
      menuItem("Boxplot", tabName = "Bplot", icon = icon("dashboard")),
      menuItem("World Map", tabName = "Map", icon = icon("dashboard")),
     menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard")),
     menuItem("Crosstab", tabName = "crosstab", icon = icon("dashboard"))
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
                         # radioButtons("rb5", "Get data from:",
                         #              c("SQL" = "SQL",
                         #                "CSV" = "CSV"), inline=T),
                         uiOutput("boxplotRegions"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click5",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("boxplotData1")
                ),
                tabPanel("Simple Box Plot", 
                         sliderInput("boxSalesRange1", "Happiness Range:", # See https://shiny.rstudio.com/articles/sliders.html
                                     min = min(globals$Happiness.Score), max = max(globals$Happiness.Score), 
                                     value = c(min(globals$Happiness.Score), max(globals$Happiness.Score))),
                         
                         plotlyOutput("boxplotPlot1", height=500))
              )
      ),
      
      #Map tab
      tabItem(tabName = "Map",
                  tabsetPanel(
                    tabPanel("World Map", leafletOutput("plot3", height=500) )
                  )
      ),
      tabItem(tabName = "histogram",
              tabsetPanel(
                tabPanel("Data",
                         actionButton(inputId = "click10",  label = "To get data, click here"), hr(), # Add space after button.
                         
                         'Here is data for the "Histogram" tab', hr(),
                         
                         DT::dataTableOutput("histogramData1")
                ),
                tabPanel("Histogram", plotOutput("histogramPlot1", height = 800))
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
                  tabPanel("Barchart with Table Calculation", "Black = Average Happiness Score, Red = Average Happiness Score per Region, and  Blue = (Average Happiness Score - Average Happiness Score per Region)", plotOutput("barchartPlot1", height=1500))
                )
        ),
      
      # Begin Crosstab tab content.
      tabItem(tabName = "crosstab",
              tabsetPanel(
                tabPanel("Data",  
                         sliderInput("KPI1", "Rank_High:", 
                                     min = 9, max = 70,  value = 50),
                         sliderInput("KPI2", "Rank_Medium:", 
                                     min = 70.1, max = 136,  value = 71),
                         actionButton(inputId = "click1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data10")
                ),
                tabPanel("Crosstab", plotOutput("plot10", height=1000))
              )
      )
      
      
    )
  )
)

