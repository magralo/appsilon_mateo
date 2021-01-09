
library(tidyverse)
library(geosphere)

get_distance <- function(long,lati){
  #' Get distance
  #'
  #' \code{get_distance} returns a dataframe with the distance (geosphere::distGeo) between every 2 consecutive positions of the
  #' @param name is string with the raw observations
  #' 
  #' 
  X=data.frame(long,lati)
  d=geosphere::distGeo(X)
  return (d)
}

get_distances <- function(name){
  
  #' Get distances
  #'
  #' \code{get_distances} returns a dataframe with the distance (geosphere::distGeo) between every 2 consecutive positions of each vessel
  #' @param name is string with the raw observations
  #' 
  #' 
  data = read.csv(name)
  
  data_dist <- data%>%
    select(LAT,LON,SPEED,ship_type,SHIPNAME,DATETIME,SHIPNAME,SHIP_ID)%>% ### HERE WE SELECT JUST THE COLUMNS THAT WE ARE GOING TO NEED
    mutate(datefull=lubridate::ymd_hms(DATETIME))%>% ### TYPE FOR SORT
    group_by(SHIPNAME,SHIP_ID)%>% #### Get info for unique vessels
    arrange(datefull)%>% ##sort for consecutive obs
    mutate(distance= get_distance(LON,LAT),LON_fut=lead(LON),LAT_fut=lead(LAT))%>% ## get distance
    ungroup()%>%
    filter(!is.na(distance))### The last obs produces a NA which can be discarted
  
  return (data_dist)
}

apply_summary <- function(df){
  
  #' Get summary for each vessel
  #'
  #' \code{apply_summary} returns a dataframe with the necessary information for the shinyapp
  #' @param df is a dataframe which contains the distance of every single measure of all vessels
  max_dist = df%>%
    group_by(SHIPNAME,SHIP_ID)%>% ### for each unique vessel
    mutate(avg_distance=mean(distance), number = length(distance))%>% ### Get the summary
    filter(distance==max(distance))%>% ### Filter for the max distance
    arrange(datefull)%>% ### sort
    do(tail(.,1))%>% ### Select the last obs... this is the special case
    ungroup()
  
  return (max_dist)
}



