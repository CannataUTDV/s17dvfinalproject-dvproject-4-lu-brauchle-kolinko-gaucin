# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)

file_path = "../../CSVs/WorldHappiness.csv"
df <- readr::read_csv(file_path) 
tdf1 = df %>% dplyr::distinct(Region) %>% arrange(Region) %>% dplyr::rename(D = Region)
tdf2 = df %>% dplyr::distinct(Region) %>% arrange(Region) %>% dplyr::rename(R = Region)
regions = bind_cols(tdf1, tdf2)

region_list <- as.list(regions$D, regions$R)
region_list <- append(list("All" = "All"), region_list)

############################### Start shinyServer Function ####################

shinyServer(function(input, output) {   
  
  #cons_states = c("AL","AK","ID","KS","MS", "CA","MN","OR","RI","WA")
  
  # These widgets are for the Barcharts tab.
  output$regions2 <- renderUI({selectInput("selectedRegions", "Choose regions:", region_list, multiple = TRUE, selected='All') })
  
  # Begin Barchart Tab ------------------------------------------------------------------
  
  dfbc1 <- eventReactive(input$click2, {
      if(input$selectedRegions == 'All') region_list <- input$selectedRegions
      else region_list <- append(list("Skip" = "Skip"), input$selectedRegions)
      
      file_path = "../../CSVs/status_sector_pop_income.csv"
      df <- readr::read_csv(file_path) 
      file_path = "../../CSVs/WorldHappiness.csv"
      happiness <- readr::read_csv(file_path) 
      #names(happiness) <- gsub(" ","",names)
      joined <- dplyr::inner_join(df, happiness, by = "Country")
      tdf <- joined %>% dplyr::select(Region, Income.Class, Happiness.Score, Happiness.Rank) %>% 
              dplyr::filter(Income.Class %in% c('High Income','Upper Middle Income','Lower Middle Income','Low Income')) %>% 
              dplyr::group_by(Income.Class, Region) %>% 
              dplyr::summarise(avg_scores = mean(Happiness.Score), avg_ranks = mean(Happiness.Rank),
                               kpi = if_else(avg_ranks <= 33, '03 Low', if_else(avg_ranks <= 67, '02 Medium', '01 High')))
      
      # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
      tdf2 <- tdf %>% group_by(Income.Class) %>% summarize(window_avg_scores = mean(avg_scores))
      dplyr::inner_join(tdf, tdf2, by = "Income.Class")
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
})