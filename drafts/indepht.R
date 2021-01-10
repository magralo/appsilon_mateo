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
aux= D[c((aux-1):(aux+1)),]

  
leaflet() %>%
  addTiles() %>%  # use the default base map which is OpenStreetMap tiles
  addPolylines( lng= df$lon, 
                lat = df$lat)%>%
  addCircleMarkers(lng=aux$lon[2], lat=aux$lat[2],
                   popup="Origin max",color = "red") %>%
  addCircleMarkers(lng=aux$lon[3], lat=aux$lat[3],
                   popup="Destination max",color = "green")%>%
  addCircleMarkers(lng=D$lon[1], lat=D$lat[1],
                   popup="First Obs",color = "orange")%>%
  addCircleMarkers(lng=D$lon[nrow(D)], lat=D$lat[nrow(D)],
                   popup="last Obs",color = "blue")
