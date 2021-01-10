library(testthat)
library(shiny)

source('app.R')
source('prepro.R')
source('indepht_module.R')
bigrquery::bq_auth(path = 'secret.json')

### Module test

test_that("test module for ship names", {
  vtype <- reactiveVal()
  data <- data.frame(ship_type=c("t1","t1","t2","t2","t3"),
                     SHIPNAME=c("n1","n2","n3","n4","n5"))
  
  testServer(selectNamesServer, args = list(vtype , data ), {
    vtype("t1")
    
    names_options = selectNamesServer ('module' ,vtype = vtype,data=data)
    
    expect_equal(names_options(), c("n1", "n2"))
    vtype("t2")
    
    names_options = selectNamesServer ('module' ,vtype = vtype,data=data)
    
    expect_equal(names_options(), c("n3", "n4"))
    
    vtype("t3")
    
    names_options = selectNamesServer ('module' ,vtype = vtype,data=data)
    
    expect_equal(names_options(), c("n5"))
    
  }) 
})


### Pre pro test


test_that("basic test for usage of distance", {
  
  origen1 = c(1,1)
  final1 = c(1,2)
  origen2 = final1
  final2 = c(4,4)
  X= rbind(origen1,origen2,final2)
  expect_equal(get_distance(X[,1],X[,2]), geosphere::distGeo(X))
  
})


test_that("check that we modify the name as id_name", {
  #### Here i made a sample dataset with 2 vessels
  results = get_distances('data/example_test3.csv')
  ## LAT,LON but distGeo uses LON,LAT order
  distancias = c(geosphere::distGeo(c(1,1),c(2,1)), 
                 geosphere::distGeo(c(2,1),c(2,2)),
                 geosphere::distGeo(c(1,1),c(2,1)))
  
  exp = data.frame(SHIPNAME = c('v1_id1','v1_id1','v2_id2'),
                   distance = distancias)
  
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','distance'))), nrow(exp))
  
})


test_that("check distances for each ship", {
  #### Here i made a sample dataset with 2 vessels
  results = get_distances('data/example_test1.csv')
  ## LAT,LON but distGeo uses LON,LAT order
  distancias = c(geosphere::distGeo(c(1,1),c(2,1)), 
                 geosphere::distGeo(c(2,1),c(2,2)),
                 geosphere::distGeo(c(1,1),c(2,1)))
  
  exp = data.frame(SHIPNAME = c('v1_id1','v1_id1','v2_id2'),
                   distance = distancias)
  
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','distance'))), nrow(exp))
  
})


test_that("check max_distance for each ship", {
  #### Here i made a sample dataset with 2 vessels
  results = get_distances('data/example_test1.csv')%>%
    apply_summary()
  ## LAT,LON but distGeo uses LON,LAT order
  
  d11=geosphere::distGeo(c(1,1),c(2,1))
  d12=geosphere::distGeo(c(2,1),c(2,2))
  d21=geosphere::distGeo(c(1,1),c(2,1))
  
  distancias = c(max(c(d11,d12)),d21)
  
  means = c(mean(c(d11,d12)),d21)
  
  
  exp = data.frame(SHIPNAME = c('v1_id1','v2_id2'),
                   distance = distancias,
                   avg_distance = means,
                   number = c(2,1))
  
  
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','distance','avg_distance','number'))), nrow(exp))
  
})


test_that("check max_distance for each ship special case", {
  #### Here i made a sample dataset with 3 vessels
  results = get_distances('data/example_test2.csv')%>%
    apply_summary()
  ## LAT,LON but distGeo uses LON,LAT order
  
  d11 = geosphere::distGeo(c(1,1),c(2,1))
  d12 = geosphere::distGeo(c(2,1),c(2,2))
  d21 = geosphere::distGeo(c(1,1),c(2,1))
  d31 = geosphere::distGeo(c(1,1),c(7,1))
  d32 = geosphere::distGeo(c(7,1),c(8,1))
  d33 = geosphere::distGeo(c(8,1),c(7,1))
  d34 = geosphere::distGeo(c(7,1),c(1,1))
  
  
  distancias = c(max(c(d11,d12)),d21,max(c(d31,d32,d33,d34)))
  
  means = c(mean(c(d11,d12)),d21,mean(c(d31,d32,d33,d34)))
  
  
  
  exp = data.frame(SHIPNAME = c('v1_id1','v2_id2','v3_id3'),
                   distance = distancias,
                   avg_distance = means,
                   number = c(2,1,4),
                   datefull = lubridate::ymd_hms(c("2016-12-19T11:29:01Z","2016-12-19T11:29:01Z","2016-12-19T11:35:01Z")))
  
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','distance','avg_distance','number','datefull'))), nrow(exp))
  
})










