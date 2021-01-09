
library(tidyverse)
library(geosphere)


source("prepro.R")

max_dist <- get_distances('data/ships.csv') %>% ### Calculating distance between consecutive obs
  apply_summary() ### Get summary for each idship,shipname


max_dist%>%
  write.csv('data/toshiny.csv')


