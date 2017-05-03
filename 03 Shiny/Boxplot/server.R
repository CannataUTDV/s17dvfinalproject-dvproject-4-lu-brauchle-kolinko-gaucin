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
require(gridExtra)
require(cowplot)
# fix up dataset for boxplots
df <- read.csv("https://query.data.world/s/6v1xt64l1z4r2k2qf1i80ql0p",header=T); 
df2 <- df %>% dplyr::mutate(ratio_agr = (Total.employment.in.agriculture..thousands.*1000/Population),ratio_ind =  (Total.employment.in.industry..thousands.*1000/Population), ratio_serv = (Total.employment.in.services..thousands.*1000/Population)) %>% dplyr::filter(Income.Class != '')

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

      df2 %>% dplyr::select(Happiness.Score, Income.Class, Region, ratio_agr, ratio_ind,ratio_serv) %>% dplyr::filter(Region %in% input$selectedBoxplotRegions | input$selectedBoxplotRegions == "All") # %>% View()
    
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
      geom_boxplot(aes(x=Region, y=ratio_agr, fill=Income.Class)) + 
      ylim(0, input$boxSalesRange1[2]) +
      coord_cartesian(ylim = c(0, .5)) +
      theme(legend.title = element_blank(),legend.position = "none",axis.text.x=element_text(angle=20, size=10, vjust=0.5), axis.text.y=element_text(size = 10)) +
      ylab("Percent in Agriculture")

      
    p2 <- ggplot(dfbp2()) + 
      geom_boxplot(aes(x=Region, y=ratio_ind, fill=Income.Class)) + 

      ylim(0, input$boxSalesRange1[2]) +
      coord_cartesian(ylim = c(0, .5)) +
      theme(legend.title = element_blank(), legend.position = "none",axis.text.x=element_text(angle=20, size=10, vjust=0.5), axis.text.y=element_text(size = 10))
    
    
    p3 <- ggplot(dfbp2()) + 
      geom_boxplot(aes(x=Region, y=ratio_serv, fill=Income.Class)) + 
      ylim(0, input$boxSalesRange1[2]) +
      coord_cartesian(ylim = c(0, .5)) +
      theme(legend.title = element_blank(),legend.text = element_text( size = 8),axis.text.x=element_text(angle=20, size=10, vjust=0.5), axis.text.y=element_text(size = 10))
    
    
    finalp <- subplot(p, p2, p3, nrows = 3, shareX  = TRUE, margin = 0.05, heights = c(.3, .4, .3))
    
    ggplotly(finalp, height = 10000)
    

    
    
  })


}
)
#end shiny server  

