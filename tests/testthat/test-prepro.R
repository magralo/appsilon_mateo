
### Pre pro tests


test_that("basic test for usage of distance", {
  
  origen1 = c(1,1)
  final1 = c(1,2)
  origen2 = final1
  final2 = c(4,4)
  X= rbind(origen1,origen2,final2)
  expect_equal(get_distance(X[,1],X[,2]), geosphere::distGeo(X))
  
})


test_that("check that we identify vessels as name+id", {
  #### Here i made a sample dataset with 2 vessels
  results = get_distances('data/example_test3.csv')
  ## LAT,LON but distGeo uses LON,LAT order
  distancias = c(geosphere::distGeo(c(1,1),c(2,1)), 
                 geosphere::distGeo(c(2,1),c(2,2)),
                 geosphere::distGeo(c(1,1),c(2,1)),
                 geosphere::distGeo(c(1,1),c(2,1)), 
                 geosphere::distGeo(c(2,1),c(2,2)))
  
  exp = data.frame(SHIPNAME = c('v1','v1','v2','v1','v1'),
                   SHIP_ID = c(1,1,2,3,3),
                   distance = distancias)
  
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','SHIP_ID','distance'))), nrow(exp))
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','SHIP_ID','distance'))), nrow(results))
  
})


test_that("check distances for each ship", {
  #### Here i made a sample dataset with 2 vessels
  results = get_distances('data/example_test1.csv')
  ## LAT,LON but distGeo uses LON,LAT order
  distancias = c(geosphere::distGeo(c(1,1),c(2,1)), 
                 geosphere::distGeo(c(2,1),c(2,2)),
                 geosphere::distGeo(c(1,1),c(2,1)))
  
  exp = data.frame(SHIPNAME = c('v1','v1','v2'),
                   SHIP_ID = c(1,1,2),
                   distance = distancias)
  
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','SHIP_ID','distance'))), nrow(exp))
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','SHIP_ID','distance'))), nrow(results))
  
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
  
  
  exp = data.frame(SHIPNAME = c('v1','v2'),
                   SHIP_ID = c(1,2),
                   distance = distancias,
                   avg_distance = means,
                   number = c(2,1))
  
  
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','SHIP_ID','distance','avg_distance','number'))), nrow(exp))
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','SHIP_ID','distance','avg_distance','number'))), nrow(results))
  
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
  
  
  
  exp = data.frame(SHIPNAME = c('v1','v2','v3'),
                   SHIP_ID = c(1,2,3),
                   distance = distancias,
                   avg_distance = means,
                   number = c(2,1,4),
                   datefull = lubridate::ymd_hms(c("2016-12-19T11:29:01Z","2016-12-19T11:29:01Z","2016-12-19T11:35:01Z")))
  
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','distance','avg_distance','number','datefull'))), nrow(exp))
  expect_equal(nrow(inner_join(results,exp,by=c('SHIPNAME','distance','avg_distance','number','datefull'))), nrow(results))
  
})










