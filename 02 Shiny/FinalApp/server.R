require(dplyr)
require(ggplot2)
require(RCurl)

#df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ",'oraclerest.cs.utexas.edu:5000/rest/native/?query="select * from Chemical_Dependence"')),httpheader=c(DB='jdbc:data:world:sql:brauchlen:s-17-edv-project-4', USER='kolinkodm', PASS='eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmtvbGlua29kbSIsImlzcyI6ImFnZW50OmtvbGlua29kbTo6Mzg4NTJlZWUtZDZhOS00NjFiLWI4YTgtYWZjNGRjYjA4YjhmIiwiaWF0IjoxNDg0NzE0NjE5LCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.VElhv7u_Pue0KH2VWN-6vbd4XvWqo0u9Ym1S7z-pSzlq_ClWOZiHpKDoMBO-U9cyVs9SZmFN2hdMJ2_yBYVwAg', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))

df <-
  read.csv("https://query.data.world/s/1bf1rqx0f351otahiiji5vy6q",
           header = T)

names(df) <- gsub(" ", "_", names(df))
names(df)

#print(df)

server <- function(input, output) {
  
  #------------Begin Data------------
  output$table1 <- renderDataTable(df, options = list(scrollX = TRUE, 
    pageLength = 5)
  )

  #------------End Scatterplot------------
  
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
    #------------End Scatterplot------------
}
