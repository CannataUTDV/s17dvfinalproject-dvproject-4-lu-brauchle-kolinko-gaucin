dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Box Plots", tabName = "boxplot", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),    
  
  tabItem(tabName = "boxplot",
        tabsetPanel(
          tabPanel("Data",  
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
)
)
)