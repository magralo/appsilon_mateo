


CardsServer <- function(id, value , title="Sample title" ,units) {
  stopifnot(is.reactive(value))
  moduleServer(id, function(input, output, session) {
    reactive({
      card(
        div(class="content",
            div(class="header", title),
            div(class="meta", units),
            div(class="description", value())
        )
      )
    })
  })
}
