
library(tidyverse)
library(geosphere)

get_distance <- function(lats,long){
  X=data.frame(lats,long)
  d=geosphere::distGeo(X)
  return (d)
}

get_distances <- function(name){
  data = read.csv(name)%>%
    mutate(SHIPNAME=paste0(SHIPNAME,'_id',SHIP_ID))
  data_dist <- data%>%
    select(LAT,LON,SPEED,ship_type,SHIPNAME,DATETIME)%>%
    mutate(datefull=lubridate::ymd_hms(DATETIME))%>%
    group_by(SHIPNAME)%>%
    arrange(datefull)%>%
    mutate(distance= get_distance(LON,LAT),LON_fut=lead(LON),LAT_fut=lead(LAT))%>%
    ungroup()%>%
    filter(!is.na(distance))
  
  return (data_dist)
}

apply_summary <- function(df){
  max_dist = df%>%
    group_by(SHIPNAME)%>%
    mutate(avg_distance=mean(distance), number = length(distance))%>%
    filter(distance==max(distance))%>%
    arrange(datefull)%>%
    do(tail(.,1))%>%
    ungroup()
  
  return (max_dist)
}



