require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(leaflet)
require(plotly)
require(lubridate)

# fix up dataset for boxplots
df <- read.csv("https://query.data.world/s/6v1xt64l1z4r2k2qf1i80ql0p",header=T); 
df2 <- df %>% dplyr::mutate(ratio_agr = (Total.employment.in.agriculture..thousands./Population),ratio_ind =  (Total.employment.in.industry..thousands./Population), ratio_serv = (Total.employment.in.services..thousands./Population))

tdf1 = df2 %>% dplyr::distinct(Region) %>% arrange(Region) %>% dplyr::rename(D = Region)
tdf2 = df2 %>% dplyr::distinct(Region) %>% arrange(Region) %>% dplyr::rename(R = Region)
regions = bind_cols(tdf1, tdf2)

region_list <- as.list(regions$D, regions$R)
region_list <- append(list("All" = "All"), region_list)
region_list5 <- region_list

#begin shiny server

shinyServer(function(input, output) {   
  # These widgets are for the Box Plots tab.
  online5 = reactive({input$rb5})
  output$boxplotRegions <- renderUI({selectInput("selectedBoxplotRegions", "Choose Regions:",
                                                 region_list5, multiple = TRUE, selected='All') })
  
  dfbp1 <- eventReactive(input$click5, {
    if(input$selectedBoxplotRegions == 'All') region_list5 <- input$selectedBoxplotRegions
    else region_list5 <- append(list("Skip" = "Skip"), input$selectedBoxplotRegions)

      df2 %>% dplyr::select(Happiness.Score, Income.Class, Region, ratio_agr) %>% dplyr::filter(Region %in% input$selectedBoxplotRegions | input$selectedBoxplotRegions == "All") # %>% View()
    
  })
  
  output$boxplotData1 <- renderDataTable({DT::datatable(dfbp1(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  
  dfbp2 <- eventReactive(c(input$click5, input$boxSalesRange1), {
    dfbp1() %>% dplyr::filter(Happiness.Score >= input$boxSalesRange1[1] & Happiness.Score <= input$boxSalesRange1[2]) # %>% View()
  })
  
  
  output$boxplotPlot1 <- renderPlotly({
    #View(dfbp3())
    p <- ggplot(dfbp2()) + 
      geom_boxplot(aes(x=Region, y=Happiness.Score, colour=Income.Class)) + 
      ylim(0, input$boxSalesRange1[2]) +
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
    ggplotly(p)
  })
}
)
#end shiny server  

