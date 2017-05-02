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
                               min = min(df$Happiness.Score), max = max(df$Happiness.Score), 
                               value = c(min(globals$Sales), max(globals$Sales))),
                   plotlyOutput("boxplotPlot1", height=500))
        )