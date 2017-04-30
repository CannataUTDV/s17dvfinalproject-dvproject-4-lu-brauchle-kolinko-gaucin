# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
#require(leaflet)

shinyServer(function(input, output) { 
# These widgets are for the Crosstabs tab.
  online1 = reactive({input$rb1})
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  
# Begin Crosstab Tab ------------------------------------------------------------------
  df1 <- eventReactive(input$click1, {
      if(online1() == "SQL") {
        print("Getting from data.world")
        query(
            data.world(propsfile = "www/.data.world"),
            dataset="brauchlen/s-17-edv-project-5", type="sql",
            query="SELECT ProviderState, MeasureDescription,
                   avg(AdjustedScore) as avg_adjusted, avg(ExpectedScore) as avg_expected, 
                   avg(AdjustedScore) / avg(ExpectedScore) as ratio,
                    
                   case
                   when avg(AdjustedScore) / avg(ExpectedScore) < ? then '03 Low'
                   when avg(AdjustedScore) / avg(ExpectedScore) < ? then '02 Medium'
                   else '01 High'
                   end AS kpi
                    
                   FROM rehospitalization  
                   group by ProviderState, MeasureDescription
                   order by ProviderState", 
            queryParameters = list(KPI_Low(), KPI_Medium())
          ) }
      
      else {
        print("Getting from csv")
        file_path = "www/rehospitalization.csv"
        df2 <- readr::read_csv(file_path)
        
        df2_1 <- df2 %>% 
            dplyr::select(ProviderState, MeasureDescription, AdjustedScore, ExpectedScore)
        
        na2zero <- function (x) {
            x[is.na(x)] <- 0
            return(x)}
        
        measures <- c("ExpectedScore", "AdjustedScore")
        
        if( length(measures) > 1) {
            for(m in measures) {
                df2_1[m] <- data.frame(lapply(df2_1[m], na2zero))
                df2_1[m] <- data.frame(lapply(df2_1[m], function(x) as.numeric(as.character(x))))}}
        
        df2_1 %>%
            dplyr::group_by(ProviderState, MeasureDescription) %>% 
            dplyr::summarise(avg_adjusted = mean(AdjustedScore), 
                             avg_expected = mean(ExpectedScore), 
                             ratio = mean(AdjustedScore)/mean(ExpectedScore),
                             kpi = if_else(ratio <= KPI_Low(), '03 Low',
                                   if_else(ratio <= KPI_Medium(), '02 Medium', '01 High'))) %>%
            dplyr::arrange(ProviderState)
        }
      })
  
  output$data1 <- renderDataTable({DT::datatable(df1(), rownames = FALSE,
                                extensions = list(Responsive = TRUE, FixedHeader = TRUE))})
  
  output$plot1 <- renderPlot({ggplot(df1()) + 
    geom_text(aes(x=MeasureDescription, y=ProviderState, label=avg_adjusted), size=6) +
    geom_tile(aes(x=MeasureDescription, y=ProviderState, fill=kpi), alpha=0.50)})
  
})
