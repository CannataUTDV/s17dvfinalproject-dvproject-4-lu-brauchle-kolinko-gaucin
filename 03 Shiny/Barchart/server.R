# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)

claim_types = query(
  data.world(propsfile = "www/.data.world"),
  dataset="brauchlen/s-17-edv-project-6", type="sql",
  query="select distinct Claim_Type as D, Claim_Type as R
  from medicare_spending")

claim_type_list <- as.list(claim_types$D, claim_types$R)
claim_type_list <- append(list("All" = "All"), claim_type_list)
#claim_type_list5 <- claim_type_list

############################### Start shinyServer Function ####################

shinyServer(function(input, output) {   
  
  cons_states = c("AL","AK","ID","KS","MS", "CA","MN","OR","RI","WA")
  
  # These widgets are for the Barcharts tab.
  output$claim_types2 <- renderUI({selectInput("selectedClaim_Types", "Choose claim types:", claim_type_list, multiple = TRUE, selected='All') })
  
  # Begin Barchart Tab ------------------------------------------------------------------
  
  df1 <- eventReactive(input$click2, {
    if(input$selectedClaim_Types == 'All') claim_type_list <- input$selectedClaim_Types
    else claim_type_list <- append(list("Skip" = "Skip"), input$selectedClaim_Types)
    
    tdf = query(
      data.world(propsfile = "www/.data.world"),
      dataset="brauchlen/s-17-edv-project-6", type="sql",
      query="select State, Claim_Type, avg(Avg_Spending_Per_Episode_State) as avg_spending
      from medicare_spending_per_capita
      where (? = 'All' or Claim_Type in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?))
      and (State in ('AL','AK','ID','KS','MS', 'CA','MN','OR','RI','WA'))
      and (Claim_Type not in ('Total'))
      group by State, Claim_Type",
      queryParameters = claim_type_list
    ) # %>% View()
    
    # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
    tdf2 = tdf %>% group_by(State) %>% summarize(state_avg_spending = mean(avg_spending))
    dplyr::inner_join(tdf, tdf2, by = "State")
  })
  output$barchartData1 <- renderDataTable({DT::datatable(df1(),
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$barchartPlot1 <- renderPlot({ggplot(df1(), aes(x=Claim_Type, y=avg_spending)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~State, ncol=1) + 
      coord_flip() + 
      # Add sum_sales, and (sum_sales - window_avg_sales) label.
      geom_text(mapping=aes(x=Claim_Type, y=avg_spending, label=round(avg_spending)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=Claim_Type, y=avg_spending, label=round(avg_spending - state_avg_spending)),colour="blue", hjust=-2) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(state_avg_spending)), color="red") +
      geom_text(aes( -1, state_avg_spending, label = state_avg_spending, vjust = -.5, hjust = -.25), color="red")
  })
})