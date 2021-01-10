#install.packages('bigrquery')

bigrquery::bq_auth(path = 'secret.json')

sql = 'SELECT lat,lon,date,datetime FROM `original-brace-297916.ships.ships` 
WHERE shipname = "xxsnxx" AND ship_id= xxsidxx
ORDER BY datetime' 

sql <- str_replace(sql,'xxsnxx','DAR-10')

sql <- str_replace(sql,'xxsidxx','3061267')


df <- bigrquery::bq_project_query('original-brace-297916',sql)%>%
  bigrquery::bq_table_download()


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
