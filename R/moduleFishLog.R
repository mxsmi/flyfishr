
fishLogUI <- function(id) {
  uiOutput(NS(id, "fishlog"))
}

fishLogServer <- function(id, logged_in) {
  moduleServer(id, function(input, output, session) {

    output$fishlog <- renderUI({
      if (!logged_in()) {
        "You must log in to use the fish log"
      } else {
        DT::dataTableOutput(NS(id, "fish_table"))
      }
    })

    output$fish_table <- DT::renderDataTable({
      req(logged_in)
      mtcars
    })

  })
}
