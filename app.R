library(shiny)
library(shiny.semantic)
library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)
source('prepro.R')

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


selectIdServer <- function(id, sname , data ) {
  stopifnot(is.reactive(sname))
  moduleServer(id, function(input, output, session) {
    reactive({
      data %>%
        filter(SHIPNAME==sname())%>%
        select(SHIP_ID)%>%
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


inDUI <- function(id) {
  semanticPage(
    leafletOutput(NS(id,'map_all')),
    br(),
    plotlyOutput(NS(id,'dist_all')),
    br(),
    numeric_input(NS(id,'get_th'),label = "Distance threshold for distribution",min = 0,value = 1),
    br(),
    actionButton(NS(id,'get_iddata'),label = "get data from bigquery"),
    h4('If there is no information please click to load data')
  )
}




inDServer <- function(id,name,sid) {
  moduleServer(id, function(input, output, session) {
    data <- eventReactive(input$get_iddata,{
      
      
      
      sql = 'SELECT lat,lon,date,datetime, shipname,ship_id 
              FROM `original-brace-297916.ships.ships` 
              WHERE shipname = "xxsnxx" AND ship_id= xxsidxx
              ORDER BY datetime' 
      
      sql <- str_replace(sql,'xxsnxx',name())
      
      sql <- str_replace(sql,'xxsidxx',sid())
      
      
      with_progress(message = 'Loading data', value = 0, {
                      
      df <- bigrquery::bq_project_query('original-brace-297916',sql)%>%
        bigrquery::bq_table_download()
                    
      
       inc_progress(1)
      })
      df
      
    })
    

    output$map_all <- renderLeaflet({
      df=data()
      if(name()==df$shipname[1]&&sid()==df$ship_id [1]){
        leaflet() %>%
          addTiles() %>%  # use the default base map which is OpenStreetMap tiles
          addPolylines( lng= df$lon, 
                        lat = df$lat)
      }else{
        leaflet() %>%
          addTiles() 
      }
    })
    
    
    output$dist_all <- renderPlotly({
      df=data()
      if(name()==df$shipname[1]&&sid()==df$ship_id [1]){
        ggplotly(df%>%
                   mutate(dist=get_distance(lon,lat))%>%
                   filter(dist>input$get_th)%>%
                   mutate(dist=log(1+dist))%>%
                   ggplot(aes(dist))+
                   geom_histogram(fill='lightblue')+
                   ggthemes::theme_hc()+
                   labs(title = 'Distance (Log) distribution',x='',y='log Distance (M)')
        )
      }else{
        
      }
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
         uiOutput('selection_id'),
         uiOutput('cards')
       ),
       
       
       #main = semanticPage(leafletOutput('simple_map',height = "60%"),br(),plotlyOutput('simple_stats',height = "40%")),
       
       main = semanticPage(
         suppressDependencies("bootstrap"),
         tabset(tabs =
                  list(
                    list(menu = "Basic info (Max)", 
                         content = semanticPage(leafletOutput('simple_map'),br(),plotlyOutput('simple_stats')), 
                         id = "main_tab"),
                    list(menu = "In depht", 
                         content = inDUI("indepht"), 
                         id = "second_tab",height='100%')
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
  
  module_aux <- reactive({input$v_type})
  names_options = selectNamesServer ('module' ,vtype = module_aux,data=data)
  
  id_aux <- reactive({input$v_name})
  id_options = selectIdServer ('module' ,sname = id_aux,data=data)
  
  
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
  
  
  get_id <- reactive({input$v_id})
  inDServer("indepht",name=id_aux,sid=get_id)
  
  output$selection <- renderUI({
    
    
    shiny::selectInput('v_name','Available vessels',
                       choices = names_options())
    
  })
  
  output$selection_id <- renderUI({
    
    
    shiny::selectInput('v_id','Vessel ID',
                       choices = id_options())
    
  })
  
  
  output$cards <- renderUI({
    
    fluidPage(
      cards(
        class = "two",
        distance_vessel(),
        speed_vessel(),
        mean_vessel(),
        number_vessel(),
        date_vessel(),ties_vessel()
      )
    )
  })
  
  
  to_show = reactive({
    ### this could be a module
    
    max_dist = data%>%
      filter(SHIPNAME==input$v_name,SHIP_ID==input$v_id)
    
    n=1
    t1=max_dist$LAT[n]
    n1=max_dist$LON[n]
    
    t2=max_dist$LAT_fut[n]
    n2=max_dist$LON_fut[n]
    d=max_dist$distance[n]
    v= max_dist$SPEED[n]
    m_dist= max_dist$avg_distance[n]
    n_sails= max_dist$number[n]
    
    
    return(list(lon=n1,lat=t1,lon_fut=n2,lat_fut=t2,dist=d,vel=v,m_dist=m_dist,
                n_sails=n_sails,datetime=max_dist$datefull,n_max=max_dist$n_max))
    
  })
  
  
  
  output$simple_map <- renderLeaflet({
    
    
    geo_points=to_show()
    
    z = 15 - findInterval(geo_points$dist,rev(c(100000,50000,25000,12000,6000,3000,1000,500,250)))
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
      labs(title = "General statistics",
           x="",y="Value")
    
    ggplotly(gg)
    
    
    
  })
  
  
}


if(!file.exists('data/toshiny.csv')){
  source('prepro_run.R')
}

shinyApp(ui,server)

### To deploy rsconnect::deployApp()

