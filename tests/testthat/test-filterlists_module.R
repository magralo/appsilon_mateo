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



test_that("test module for ids", {
  sname <- reactiveVal()
  data <- data.frame(ship_type=c("t1","t1","t2","t2","t3"),
                     SHIPNAME=c("n1","n2","n2","n4","n5"),
                     SHIP_ID=c(1,2,3,4,5))
  
  testServer(selectIdServer, args = list(sname , data ), {
    
    sname("n1")
    id_options = selectIdServer ('module' ,sname = sname,data=data)
    expect_equal(id_options(), c(1))
    
    sname("n2")
    id_options = selectIdServer ('module' ,sname = sname,data=data)
    expect_setequal(id_options(), c(2, 3))
    
    
  }) 
})