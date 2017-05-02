library(plotly)

shinyServer(function(input, output) {   
  # These widgets are for the Box Plots tab.
  online5 = reactive({input$rb5})
  output$boxplotRegions <- renderUI({selectInput("selectedBoxplotRegions", "Choose Regions:",
                                                 region_list5, multiple = TRUE, selected='All') })
dfbp1 <- eventReactive(input$click5, {
  if(input$selectedBoxplotRegions == 'All') region_list5 <- input$selectedBoxplotRegions
  else region_list5 <- append(list("Skip" = "Skip"), input$selectedBoxplotRegions)
  if(online5() == "SQL") {
    print("Getting from data.world")
    df <- query(
      data.world(propsfile = "www/.data.world"),
      dataset="kolinkodm/s-17-dv-final-project", type="sql",
      query="select Category, Sales, Region, Order_Date
      from SuperStoreOrders
      where (? = 'All' or Region in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?))",
      queryParameters = region_list5 ) # %>% View()
  }
  else {
    print("Getting from csv")
    file_path = "Final.csv"
    df <- readr::read_csv(file_path)
    names(df) <- gsub(" ", "_", names(df))
    names(df)
    View(df)
    names(df)
    df %>% dplyr::select(Region, Total.employment.in.services..thousands.,Total.employment.in.industry..thousands., Total.employment.in.agriculture..thousands., Population, Happiness.Score) %>% dplyr::filter(Region %in% input$selectedBoxplotRegions | input$selectedBoxplotRegions == "All")  %>%
      dplyr::summarize(ratio_agriculture =Total.employment.in.agriculture..thousands./Population, ratio_industry = Total.employment.in.industry..thousands./Population,
                       ratio_services = Total.employment.in.services..thousands. / Population)
  }
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
  p <- ggplot(dfbp3()) + 
    geom_boxplot(aes(x=Region, y=ratio_industry, colour=Happiness.Score)) + 
    ylim(0, input$boxSalesRange1[2]) +
    theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
  ggplotly(p)
})
}
