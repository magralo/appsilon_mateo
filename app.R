library(shiny)
library(shiny.semantic)
library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)
library(bigrquery)

myGrid <- grid_template(default = list(
  areas = rbind(
    c("menu", "none","main")
    
  ),
  rows_height = c( "90%"),
  cols_width = c("20%","5%", "70%")
))

ui <- semanticPage(
  titlePanel("Hello Appsilon"),
  grid(myGrid,
       menu = semanticPage(
         selectInput('v_type','Select Vessel type',
                     choices = c("Tanker","Cargo","Tug","Unspecified","Passenger",
                                 "Fishing","Pleasure","High Special","Navigation")),
         br(),
         uiOutput('selection'),
         uiOutput('selection_id'),
         uiOutput('cards')
       ),
       

       main = semanticPage(
         tabset(tabs =
                  list(
                    list(menu = "Basic info (Max)", 
                         content = semanticPage(leafletOutput('simple_map'),
                                                br(),
                                                plotlyOutput('simple_stats')), 
                         id = "main_tab"),
                    list(menu = "In depht", 
                         content = inDUI("indepht"), 
                         id = "second_tab")
                  ),
                active = "main_tab",
                id = "exampletabset")
       ),
       none = semanticPage()
  )
)

server <- function(input, output, session) {
  
  data <- read.csv('data/toshiny.csv')
  bigrquery::bq_auth(path = 'secret.json')
  
  
  ### Get available vessels names
  module_aux <- reactive({input$v_type})
  names_options = selectNamesServer ('module' ,vtype = module_aux,data=data) ### Explicit restriction
  
  ### Get ids for the selected vessels... usually just 1 id, just to be sure that we are not seeing 2 different vessels with the same name
  id_aux <- reactive({input$v_name})
  id_options = selectIdServer ('module' ,sname = id_aux,data=data)
  
  ### Cards!! Here we put all the information that we need
  module_dist <- reactive({to_show()$dist})
  distance_vessel = CardsServer('dist',value = module_dist,title = "Max Distance",units = "Meters")
  
  module_speed <- reactive({to_show()$vel})
  speed_vessel = CardsServer('dist',value = module_speed,title = "Speed",units = "knots")
  
  module_mean <- reactive({to_show()$m_dist})
  mean_vessel = CardsServer('mean',value = module_mean,title = "Avg distance",units = "Meters")
  
  module_number <- reactive({to_show()$n_sails})
  number_vessel = CardsServer('number',value = module_number,title = "Recorded distances",units = "#")
  
  module_date <- reactive({to_show()$datetime})
  date_vessel = CardsServer('date',value = module_date,title = "Date Recorded",units = "Datetime")
  
  module_ties <- reactive({to_show()$n_max})
  ties_vessel = CardsServer('ties',value = module_ties,title = "Ties for max",units = "#")
  
  ### Call indepht module
  get_id <- reactive({input$v_id})
  
  inDServer("indepht",name=id_aux,sid=get_id)
  
  output$selection <- renderUI({
    
    
    shiny::selectInput('v_name','Available vessels',
                       choices = names_options())
    
  })
  
  output$selection_id <- renderUI({
    if (!is.null(id_options())){
      shiny::selectInput('v_id','Vessel ID',
                         choices = id_options()) 
    }
    
    
    
  })
  
  
  
  
  
  to_show = reactive({
    
    max_dist = data%>%
      filter(SHIPNAME==input$v_name,SHIP_ID==input$v_id)
    
    output$cards <- renderUI({
      fluidPage(
        cards(
          class = "two",
          distance_vessel(),
          speed_vessel(),
          mean_vessel(),
          number_vessel(),
          date_vessel(),
          ties_vessel()
        )
      )
    })
    
    return(list(lon=max_dist$LON,lat=max_dist$LAT, # initial pos
                lon_fut=max_dist$LON_fut,lat_fut=max_dist$LAT_fut, #final pos
                dist=max_dist$distance,# Distance of that obs
                vel=max_dist$SPEED,# Speed of that ob
                m_dist=max_dist$avg_distance, # Average distance of all obs
                n_sails=max_dist$number, #number of obs
                datetime=max_dist$datefull,# Datetime info of the max obs
                n_max=max_dist$n_max # Number of ties for the max
                ))
    
  })
  
  
  
  output$simple_map <- renderLeaflet({
    
    
    geo_points=to_show()
    

      z = 17 - findInterval(geo_points$dist,rev(c(100000,50000,25000,12000,6000,3000,1000,500,250,100,50)))
      ### This a naive implementation for a good zoom, i really did not like the fit bound options
      
      map_vessels <- leaflet() %>%
        addTiles() %>%  
        addCircleMarkers(lng=geo_points$lon, lat=geo_points$lat,
                         popup="Origin",color = "red") %>%
        addCircleMarkers(lng=geo_points$lon_fut, lat=geo_points$lat_fut,
                         popup="Destination",color = "green")%>%
        addPolylines( lng= c(geo_points$lon,geo_points$lon_fut), 
                      lat = c(geo_points$lat,geo_points$lat_fut))%>%
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
      labs(title = paste0("General statistics (",vtype,")"),
           x="",y="Value")
    
    ggplotly(gg)
    
    
    
  })
  
  
}


if(!file.exists('data/toshiny.csv')){
  source('prepro_run.R')
}

shinyApp(ui,server)

### To deploy rsconnect::deployApp()

