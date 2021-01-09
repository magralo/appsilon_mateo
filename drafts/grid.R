library(shiny)
library(shiny.semantic)
library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)


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




myGrid <- grid_template(default = list(
  areas = rbind(
    c("menu", "none","main")

  ),
  rows_height = c( "90%"),
  cols_width = c("20%","5%", "70%")
))

ui <- semanticPage(
  titlePanel("Hello Appsilon 2"),
  grid(myGrid,
       menu = semanticPage(
         selectInput('v_type','Select Vessel type',
                     choices = c("Tanker","Cargo","Tug","Unspecified","Passenger",
                                 "Fishing","Pleasure","High Special","Navigation")),
         br(),
         uiOutput('selection'),
         uiOutput('cards')
        ),
       main = semanticPage(leafletOutput('simple_map',height = "60%"),plotlyOutput('simple_stats',height = "40%")),
       none = semanticPage()
  )
)


server <- function(input, output, session) {
  data = read.csv('toshiny.csv')
  
  module_aux <- reactive({input$v_type})
  names_options = selectNamesServer ('module' ,vtype = module_aux,data=data)
  
  module_dist <- reactive({to_show()$dist})
  
  distance_vessel = CardsServer('dist',value = module_dist,title = "Distance",units = "Meters")
  
  module_speed <- reactive({to_show()$vel})
  
  speed_vessel = CardsServer('dist',value = module_speed,title = "Speed",units = "knots")
  
  module_mean <- reactive({to_show()$m_dist})
  
  mean_vessel = CardsServer('mean',value = module_mean,title = "Avg distance",units = "Meters")
  
  module_number <- reactive({to_show()$n_sails})
  
  number_vessel = CardsServer('mean',value = module_number,title = "Recorded sails",units = "#")
    
    
  
  output$selection <- renderUI({
    
    
    shiny::selectInput('v_name','Available vessels',
                       choices = names_options())
    
  })
  
  
  output$cards <- renderUI({
    
    fluidPage(
      cards(
        class = "two",
        distance_vessel(),
        speed_vessel(),
        mean_vessel(),
        number_vessel()
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
    m_dist= max_dist$avg_distance[n]
    n_sails= max_dist$number[n]
    
    
    return(list(lon=n1,lat=t1,lon_fut=n2,lat_fut=t2,dist=d,vel=v,m_dist=m_dist,n_sails=n_sails))
    
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
  
  output$simple_stats <- renderPlotly({
    
    
    geo_points=to_show()
    vtype  = input$v_type
    
    
    mini =  data.frame(name = c("number","ln(max distance)","ln(avg distance)"),
                       value = c(geo_points$n_sails,log(1+geo_points$dist),log(1+geo_points$m_dist)),
                       ship = input$v_name)
    gg <- data %>%
      filter(ship_type== vtype)%>%
      select(distance,avg_distance,number)%>%
      pivot_longer(cols = c(distance,avg_distance,number))%>%
      mutate(value=ifelse(name=="number",value,log(value+1)))%>%
      mutate(name=ifelse(name=="distance","ln(max distance)",name))%>%
      mutate(name=ifelse(name=="avg_distance","ln(avg distance)",name))%>%
      ggplot(aes(name,value))+
      geom_boxplot(fill='lightblue')+facet_wrap(~name,scales = "free",ncol=3)+
      geom_point(data=mini,aes(fill=ship))+
      ggthemes::theme_hc()+
      labs(title = "Overview of other vessels of the same type",
           subtitle = vtype,x="",y="Value")
    
    ggplotly(gg)
    
    
    
  })
  
  
}



app=shinyApp(ui, server)




