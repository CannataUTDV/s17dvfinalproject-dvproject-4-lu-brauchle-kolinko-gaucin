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
#require(plyr)

server <- function(input, output) {
    
    #globals <- df %>% dplyr::select(Happiness.Score) %>% dplyr::distinct()
    
    tdf1 = df %>% dplyr::distinct(Region) %>% arrange(Region) %>% dplyr::rename(D = Region)
    tdf2 = df %>% dplyr::distinct(Region) %>% arrange(Region) %>% dplyr::rename(R = Region)
    regions = bind_cols(tdf1, tdf2)
    
    region_list <- as.list(regions$D, regions$R)
    region_list <- append(list("All" = "All"), region_list)
    region_list5 <- region_list
  
  #------------Begin Data------------
  output$table1 <- renderDataTable(df, options = list(scrollX = TRUE, 
    pageLength = 5)
  )

  #------------End Data------------
  
  #------------Begin Scatterplot------------
  output$plot1 <- renderPlot({
    ggplot(df,
           aes(
             x = Economy..GDP.per.Capita.,
             y = Happiness.Score,
             colour = Region,
             size = 4
           )) + xlab("GDP Per Capita") + ylab("Happiness Score") + ggtitle("Happiness by GDP") + geom_smooth(method = "lm", size = 1) + geom_point() + guides(size =FALSE)
  })
  
  output$plot2 <- renderPlot({
    brush = brushOpts(id = "plot_brush",
                      delayType = "throttle",
                      delay = 30)
    bdf = brushedPoints(df, input$plot_brush)
    #View(bdf)
    if (!is.null(input$plot_brush)) {
      df %>% dplyr::filter(Economy..GDP.per.Capita. %in% bdf[, "Economy..GDP.per.Capita."]) %>%
        ggplot(aes(
        x = Economy..GDP.per.Capita.,
        y = Happiness.Score,
        colour = Region,
        size = 4
      )) + geom_point() + xlab("GDP Per Capita") + ylab("Happiness Score") + ggtitle("Happiness by GDP") +  geom_smooth(method = "lm", size = 1) + guides(size = FALSE)
    }
  })
    #------------End Scatterplot------------
  
  #------------Begin BoxPlot------------
    
  #online5 = reactive({input$rb5})
  output$boxplotRegions <- renderUI({selectInput("selectedBoxplotRegions", "Choose Regions:",
                                                 region_list5, multiple = TRUE, selected='All') })
  
  dfbp1 <- eventReactive(input$click5, {
    if(input$selectedBoxplotRegions == 'All') region_list5 <- input$selectedBoxplotRegions
    else region_list5 <- append(list("Skip" = "Skip"), input$selectedBoxplotRegions)
    
    df %>% dplyr::select(Happiness.Score, Income.Class, Region, ratio_agr, ratio_ind,ratio_serv) %>% dplyr::filter(Region %in% input$selectedBoxplotRegions | input$selectedBoxplotRegions == "All", Income.Class != '') # %>% View()
    
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
  #------------End Boxplot------------
  
    #------------Begin Map------------
    output$plot3 <- renderLeaflet({
  
      pal <- colorNumeric(
        palette = c('red', 'green'),
        domain = df$Happiness.Score
      )
      
      leaflet(df) %>%
        setView(lng = 0,
                lat = 0,
                zoom = 2) %>%
        addTiles() %>%
        addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
        addCircleMarkers(
          ~ longitude,
          ~ latitude,
          label = as.character(df$name),
          labelOptions = labelOptions(
            noHide = F,
            direction = 'auto',
            textOnly = FALSE,
            style = list('border-color' = 'rgba(0, 0, 0, 0)')
          ),
          radius =  ~ Happiness.Score * 1.5,
          stroke = FALSE,
          fillOpacity = 0.5,
          color =  ~ pal(df$Happiness.Score),
          popup = ~ paste(
            sep = "<br/>",
            "<b>Happiness Score:</b> ",
            as.character(df$Happiness.Score),
            " <b>Happiness Rank:</b> ",
            as.character(df$Happiness.Rank)
          ) 
        ) %>%
        addLegend("bottomright", pal = pal, values = ~Happiness.Score, 
                  title = "Happiness Score",
                  opacity = 1
        )
      
    })
    #------------End Map------------
  
# -----------Begin Barchart----------
  
  
  output$regions2 <- renderUI({selectInput("selectedRegions", "Choose regions:", region_list, multiple = TRUE, selected='All') })
    
  ddf <- read.csv("https://query.data.world/s/1bf1rqx0f351otahiiji5vy6q", header = T)
    
  dfbc1 <- eventReactive(input$click2, {
      if(input$selectedRegions == 'All') region_list <- input$selectedRegions
      else region_list <- append(list("Skip" = "Skip"), input$selectedRegions)
      
      tdf <- df %>% dplyr::select(Region, Income.Class, Happiness.Score, Happiness.Rank) %>% 
        dplyr::filter(Income.Class %in% c('High Income','Upper Middle Income','Lower Middle Income','Low Income'), Region %in% input$selectedRegions | input$selectedRegions == "All") %>% 
        dplyr::group_by(Income.Class, Region) %>% 
        dplyr::summarise(avg_scores = mean(Happiness.Score), avg_ranks = mean(Happiness.Rank),
                         kpi = if_else(avg_ranks <= 33, '03 Low', if_else(avg_ranks <= 67, '02 Medium', '01 High')))
      
      # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
      tdf2 <- tdf %>% dplyr::select(Income.Class, avg_scores) %>% 
                     dplyr::group_by(Income.Class) %>% summarise(window_avg_scores = mean(avg_scores))
      tdf2 %>% dplyr::inner_join(tdf, by = Income.Class)
    })
    output$barchartData1 <- renderDataTable({DT::datatable(dfbc1(),
                                                           rownames = FALSE,
                                                           extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
    })
    
    output$barchartPlot1 <- renderPlot({ggplot(dfbc1(), aes(x=Region, y=avg_scores, fill=kpi)) +
        scale_y_continuous(labels = scales::comma) + # no scientific notation
        theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
        theme(axis.text.y=element_text(size=12, hjust=0.5)) +
        geom_bar(stat = "identity") + 
        facet_wrap(~Income.Class, ncol=1) + 
        coord_flip() + 
        # Add sum_sales, and (sum_sales - window_avg_sales) label.
        geom_text(mapping=aes(x=Region, y=avg_scores, label=round(avg_scores)),colour="black", hjust=-.5) +
        geom_text(mapping=aes(x=Region, y=avg_scores, label=round(avg_scores - window_avg_scores)),colour="blue", hjust=-2) +
        # Add reference line with a label.
        geom_hline(aes(yintercept = round(window_avg_scores)), color="red") +
        geom_text(aes( -1, window_avg_scores, label = window_avg_scores, vjust = -.5, hjust = -.25), color="red")
    })
    
    
#----------Crostabs-------------
    KPI_Low = reactive({input$KPI1})     
    KPI_Medium = reactive({input$KPI2})
    
    dfabc1 <- eventReactive(input$click1, {
      
      
      dff1 <- df %>% dplyr::select(Total.employment.in.agriculture..thousands.,
                                      Total.employment.in.industry..thousands.,
                                      Total.employment.in.services..thousands.,
                                      Region,Happiness.Score,Happiness.Rank)
      
      dff2 <- dff1 %>% dplyr::mutate(MainIndustry = if_else(Total.employment.in.agriculture..thousands.>=Total.employment.in.industry..thousands. & Total.employment.in.agriculture..thousands.>=Total.employment.in.services..thousands., 'Agriculture', if_else(Total.employment.in.industry..thousands.>=Total.employment.in.agriculture..thousands. & Total.employment.in.industry..thousands.>=Total.employment.in.services..thousands., 'Industry', 'Services')))
      
      dff2 %>% dplyr::select(Region, Happiness.Score, Happiness.Rank, MainIndustry) %>% 
        dplyr::group_by(MainIndustry, Region) %>% 
        dplyr::summarise(avg_scores = round(mean(Happiness.Score),2), avg_ranks = round(mean(Happiness.Rank),2), 
                         kpi = if_else(avg_ranks <= KPI_Low(), '03 Low', if_else(avg_ranks <= KPI_Medium(), '02 Medium', '01 High')))
    })
    
    output$data10 <- renderDataTable({DT::datatable(dfabc1(), rownames = FALSE,
                                                   extensions = list(Responsive = TRUE, FixedHeader = TRUE))
    })
    
    output$plot10 <- renderPlot({ggplot(dfabc1()) + 
        geom_text(aes(x=Region, y=MainIndustry, label=avg_scores), size=6) +
        geom_tile(aes(x=Region, y=MainIndustry, fill=kpi), alpha=0.50) +
        theme(axis.text.x=element_text(angle=90, size=12, vjust=0.5)) + 
        theme(axis.text.y=element_text(size=12, hjust=0.5))
    })
    
    
# begin histogram tab 
    dfh1 <- eventReactive(input$click10, {
      df %>% select(Happiness.Score, Region)
    })
    
    output$histogramData1 <- renderDataTable({DT::datatable(dfh1(), rownames = FALSE,
                                                            extensions = list(Responsive = TRUE, 
                                                                              FixedHeader = TRUE)
    )
    })
    
    output$histogramPlot1 <- renderPlot({ggplot(dfh1()) +
        geom_histogram(aes(x=Happiness.Score, fill = Region)) +
        theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
    })
  }

