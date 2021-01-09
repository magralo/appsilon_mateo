library(shiny)
library(shiny.semantic)
library(tidyverse)
library(lubridate)
library(leaflet)


selectNamesServer <- function(id, vtype , data ) {
  stopifnot(is.reactive(vtype))
  moduleServer(id, function(input, output, session) {
    reactive({
      data %>%
      filter(ship_type==vtype())%>%
      select(SHIPNAME)%>%
      pull()
    })
  })
}




CardsServer <- function(id, value , title="Sample title" ,units) {
  #stopifnot(is.reactive(value))
  moduleServer(id, function(input, output, session) {
    reactive({
      card(
        div(class="content",
            div(class="header", title),
            div(class="meta", units),
            div(class="description", value())
        )
      )
    })
  })
}






ui <- semanticPage(
    titlePanel("Hello Appsilon"),
    sidebar_layout(
      sidebar_panel(
        selectInput('v_type','Select Vessel type',
                           choices = c("Tanker","Cargo","Tug","Unspecified","Passenger",
                                       "Fishing","Pleasure","High Special","Navigation")),
        uiOutput('selection'),
        uiOutput('cards'),
        width = 1
      ),
      main_panel(
        leafletOutput('simple_map'),
        width = 4#,height=5
      )
  )
)


server <- function(input, output, session) {
  data = read.csv('toshiny.csv')
  
  module_aux <- reactive({input$v_type})
  names_options = selectNamesServer ('module' ,vtype = module_aux,data=data)
  
  module_dist <- reactive({to_show()$dist[1]})
  
  distance_vessel = CardsServer('dist',value = module_dist,title = "Distance",units = "Meters")
  
  module_speed <- reactive({to_show()$vel[1]})
  
  speed_vessel = CardsServer('dist',value = module_speed,title = "Speed",units = "knots")
  
  output$selection <- renderUI({
    
    
    shiny::selectInput('v_name','Available vessels',
                       choices = names_options())
    
  })
  
  
  output$cards <- renderUI({
    
    fluidPage(
      cards(
        class = "two",
        distance_vessel(),
        speed_vessel()
      )
    )
  })
  
  
  to_show = reactive({
    
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
    

    z = 14 - findInterval(geo_points$d,rev(c(100000,50000,25000,12000,6000,3000,1000,500,250)))
    
    map_vessels <- leaflet() %>%
      addTiles() %>%  # use the default base map which is OpenStreetMap tiles
      addCircleMarkers(lng=geo_points$lon, lat=geo_points$lat,
                 popup="Origin",color = "red") %>%
      addCircleMarkers(lng=geo_points$lon_fut, lat=geo_points$lat_fut,
                 popup="Destination",color = "green")%>%
      addPolylines( lng= c(geo_points$lon,geo_points$lon_fut), 
                    lat = c(geo_points$lat,geo_points$lat_fut))%>%
      #fitBounds(geo_points$lon,geo_points$lat,geo_points$lon_fut,geo_points$lat_fut)
      setView( geo_points$lon,geo_points$lat,zoom = z)
    
    map_vessels
    
    
  })
  
  
}



shinyApp(ui, server)
