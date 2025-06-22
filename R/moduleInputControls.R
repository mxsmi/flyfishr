inputControlsUI <- function(id) {

  tagList(
    fluidRow(
      column(1),
      column(3,
             selectInput(NS(id, "state"), "Choose a State", choices = stateCd$STUSAB)
      ),
      column(1),
      column(1),
    ),
    fluidRow(
      column(1),
      column(3,
             selectizeInput(NS(id, "site"),
                            "Choose a USGS monitoring site for selected state",
                            choices = ""),
      )
    ),
    HTML("<strong>Data sources:</strong> USGS data obtained using the 'dataRetrieval' R package"),
    textOutput(NS(id, "noResultsMessage"))
  )
}

inputControlsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    sites <- reactiveVal()
    show_no_results <- reactiveVal(FALSE)

    observeEvent(input$state, {
      req(input$state)
      showNotification(ui = "Fetching USGS sites with discharge data. This may take a few minutes.",
                       duration = NULL,
                       id = "loadingData",
                       type = "message"
      )
      sites_df <- dischargeDataAvailable(state = input$state)
      if (is.null(sites_df)) {
        show_no_results(TRUE)
        sites(NULL)
        updateSelectizeInput(session, inputId = "site", choices = NULL)
      } else {
        show_no_results(FALSE)
        sites(sites_df)
        updateSelectizeInput(session, inputId = "site",
                             choices = sort(sites_df$station_nm),
                             selected = "",
                             server = TRUE)
      }
      removeNotification(id = "loadingData")
    })

    output$noResultsMessage <- renderText({
      if (show_no_results()) {
        "No discharge results found for this state and search term"
      } else {
        ""
      }
    })

    # Return reactive values for parent app
    return(list(
      sites = sites,
      selected_site = reactive(input$site),
      selected_state = reactive(input$state)
    ))
  })
}

