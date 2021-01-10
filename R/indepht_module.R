#source('prepro.R') ### For distance functions
library(bigrquery)
inDUI <- function(id) {
  semanticPage(
    h4('If there is no information please click to load data'),
    actionButton(NS(id,'get_iddata'),label = "get data from bigquery"),
    br(),
    leafletOutput(NS(id,'map_all')),
    br(),
    numeric_input(NS(id,'get_th'),label = "Distance threshold for distribution",min = 0,value = 1),
    br(),
    plotlyOutput(NS(id,'dist_all')),
    br()
    
  )
}




inDServer <- function(id,name,sid) {
  stopifnot(is.reactive(name))
  stopifnot(is.reactive(sid))
  
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
        D=df%>%
          mutate(dist=get_distance(lon,lat))%>%
          filter(!is.na(dist))
        
        aux=which.max(D$dist)
        
        
        
        leaflet() %>%
          addTiles() %>%  # use the default base map which is OpenStreetMap tiles
          addPolylines( lng= df$lon, 
                        lat = df$lat)%>%
          addCircleMarkers(lng=df$lon[aux], lat=df$lat[aux],
                           popup="Origin max",color = "red") %>%
          addCircleMarkers(lng=df$lon[aux+1], lat=df$lat[aux+1],
                           popup="Destination max",color = "green")%>%
          addCircleMarkers(lng=D$lon[1], lat=D$lat[1],
                           popup="First Obs",color = "orange")%>%
          addCircleMarkers(lng=last(df$lon), lat=last(df$lat),
                           popup="last Obs",color = "blue")
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
        plot_ly()
      }
    })
    
    reactive({
      sql = 'SELECT lat,lon,date,datetime, shipname,ship_id 
              FROM `original-brace-297916.ships.ships` 
              WHERE shipname = "xxsnxx" AND ship_id= xxsidxx
              ORDER BY datetime' 
      
      sql <- str_replace(sql,'xxsnxx',name())
      
      sql <- str_replace(sql,'xxsidxx',sid())
      
      
      return(sql)
      
    })
    
    
    
    
    
  })
}

