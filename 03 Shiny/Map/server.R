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

#df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ",'oraclerest.cs.utexas.edu:5000/rest/native/?query="select * from Chemical_Dependence"')),httpheader=c(DB='jdbc:data:world:sql:brauchlen:s-17-edv-project-4', USER='kolinkodm', PASS='eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OmtvbGlua29kbSIsImlzcyI6ImFnZW50OmtvbGlua29kbTo6Mzg4NTJlZWUtZDZhOS00NjFiLWI4YTgtYWZjNGRjYjA4YjhmIiwiaWF0IjoxNDg0NzE0NjE5LCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.VElhv7u_Pue0KH2VWN-6vbd4XvWqo0u9Ym1S7z-pSzlq_ClWOZiHpKDoMBO-U9cyVs9SZmFN2hdMJ2_yBYVwAg', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))

df <- read.csv("https://query.data.world/s/3cc3a7uu6cggb1e1kx6o6qmmc",header=T);
names(df) <- gsub(" ", "_", names(df))
names(df)

server <- function(input, output) {
  
  color <- colorFactor(rainbow(2, s = 1, v = 1, start = 0, end = .3), df$Happiness.Score)
  
  output$map <- renderLeaflet({
    leaflet(df) %>%
      setView(lng = -98.35, lat = 39.5, zoom = 4)%>% 
      addTiles() %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = FALSE)) %>%
      addCircleMarkers(~longitude,
                 ~latitude, radius=~Happiness.Score*1.5, # Total count
                 stroke=FALSE, # Circle stroke
                 fillOpacity=0.5, color=~color(df$Happiness.Score), popup = ~paste(as.character(df$name), "| HS: ", as.character(df$Happiness.Score)))
    
  })
}
