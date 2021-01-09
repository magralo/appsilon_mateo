library(shiny)
library(shiny.semantic)
library(tidyverse)
library(lubridate)
library(leaflet)


ui <- semanticPage(
  titlePanel("Hello Appsilon"),
  sidebar_layout(
    sidebar_panel(
      shiny::selectInput('v_type','Select Vessel type',
                  choices = c("Tanker","Cargo","Tug","Unspecified","Passenger",
                              "Fishing","Pleasure","High Special","Navigation")),
      #names_UI("example"),
      
      uiOutput('selection'),
      width = 1
    ),
    main_panel(
      leafletOutput('simple_map'),
      width = 4
    )
  )
)


server <- function(input, output, session) {
  data = read.csv('toshiny.csv')
  
  names_option <-reactive({
    data %>%
      filter(ship_type==input$v_type)%>%
      select(SHIPNAME)%>%
      pull()
  })
  
  output$selection <- renderUI({
    
    
    shiny::selectInput('v_name',"Select Vessel Name",
                       choices = names_option())
  })
  
  to_show = reactive({
    print(input$v_name)
    max_dist = data%>%
      filter(SHIPNAME==input$v_name)
    
    n=1
    t1=max_dist$LAT[n]
    n1=max_dist$LON[n]
    
    t2=max_dist$LAT_fut[n]
    n2=max_dist$LON_fut[n]
    d=max_dist$distance[n]
    v= max_dist$SPEED[n]
    
    
    return(list(lon=n1,lat=t1,lon_fut=n2,lat_fut=t2,dist=d,vel=v))
    
  })
  
  
  
  output$simple_map <- renderLeaflet({
    
    
    geo_points=to_show()
    
    map_vessels <- leaflet() %>%
      addTiles() %>%  # use the default base map which is OpenStreetMap tiles
      addMarkers(lng=geo_points$lon, lat=geo_points$lat,
                 popup="Origin") %>%  # use the default base map which is OpenStreetMap tiles
      addMarkers(lng=geo_points$lon_fut, lat=geo_points$lat_fut,
                 popup="Destination")%>%
      addPolylines(lat = c(geo_points$lon,geo_points$lon_fut), 
                   lng = c(geo_points$lat,geo_points$lat_fut))
    
    map_vessels
    
    
  })
  
  
}



shinyApp(ui, server)
