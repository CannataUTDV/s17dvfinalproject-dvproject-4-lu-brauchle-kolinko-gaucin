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
  
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  
# Begin Crosstab Tab ------------------------------------------------------------------
  df1 <- eventReactive(input$click1, {
      file_path = "../../CSVs/status_sector_pop_income.csv"
      df <- readr::read_csv(file_path) 
      file_path = "../../CSVs/WorldHappiness.csv"
      happiness <- readr::read_csv(file_path) 
      #names(happiness) <- gsub(" ","_",names)
      names
      joined <- dplyr::inner_join(df, happiness, by = "Country")
        
      df1 <- joined %>% dplyr::select(Total.employment.in.agriculture..thousands.,
                                      Total.employment.in.industry..thousands.,
                                      Total.employment.in.services..thousands.,
                                      Region,Happiness.Score,Happiness.Rank)
      
      df2 <- df1 %>% dplyr::mutate(MainIndustry = if_else(Total.employment.in.agriculture..thousands.>=Total.employment.in.industry..thousands. & Total.employment.in.agriculture..thousands.>=Total.employment.in.services..thousands., 'Agriculture', if_else(Total.employment.in.industry..thousands.>=Total.employment.in.agriculture..thousands. & Total.employment.in.industry..thousands.>=Total.employment.in.services..thousands., 'Industry', 'Services')))
      
      df2 %>% dplyr::select(Region, Happiness.Score, Happiness.Rank, MainIndustry) %>% 
                     dplyr::group_by(MainIndustry, Region) %>% 
                     dplyr::summarise(avg_scores = round(mean(Happiness.Score),2), avg_ranks = round(mean(Happiness.Rank),2), 
                        kpi = if_else(avg_ranks <= KPI_Low(), '03 Low', if_else(avg_ranks <= KPI_Medium(), '02 Medium', '01 High')))
      })
  
  output$data1 <- renderDataTable({DT::datatable(df1(), rownames = FALSE,
                                extensions = list(Responsive = TRUE, FixedHeader = TRUE))
      })
  
  output$plot1 <- renderPlot({ggplot(df1()) + 
    geom_text(aes(x=Region, y=MainIndustry, label=avg_scores), size=6) +
    geom_tile(aes(x=Region, y=MainIndustry, fill=kpi), alpha=0.50) +
    theme(axis.text.x=element_text(angle=90, size=12, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=12, hjust=0.5))
      })
  
})
