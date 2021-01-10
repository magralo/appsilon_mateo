selectNamesServer <- function(id, vtype , data ) {
  stopifnot(is.reactive(vtype))
  moduleServer(id, function(input, output, session) {
    reactive({
      data %>%
        filter(ship_type==vtype())%>%
        select(SHIPNAME)%>%
        pull()
    })
  })
}


selectIdServer <- function(id, sname , data ) {
  stopifnot(is.reactive(sname))
  moduleServer(id, function(input, output, session) {
    reactive({
      data %>%
        filter(SHIPNAME==sname())%>%
        select(SHIP_ID)%>%
        pull()
    })
  })
}
