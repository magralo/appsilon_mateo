test_that("test the used sql", {
  sname <- reactiveVal()
  sid <- reactiveVal()
  
  testServer(inDServer, args = list(sname,sid ), {
    
    sname("DAR-10")
    sid('3061267')
    sql_test = inDServer ('test' ,name = sname,sid=sid)
    
    sql_ok = 'SELECT lat,lon,date,datetime, shipname,ship_id 
              FROM `original-brace-297916.ships.ships` 
              WHERE shipname = "DAR-10" AND ship_id= 3061267
              ORDER BY datetime' 
    
    
    expect_equal(sql_test(), sql_ok)
    
    
  }) 
})