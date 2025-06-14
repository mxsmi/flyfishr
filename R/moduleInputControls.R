inputControlsUI <- function(id) {

  tagList(
    fluidRow(
      column(1),
      column(3,
             selectInput(NS(id, "state"), "Choose a State", choices = stateCd$STUSAB)
      ),
      column(1),
      column(3,
             textInput(NS(id, "riverinput"), "Enter river name to search by",
                       placeholder = "Ex: Yellowstone"),
      ),
      column(1),
      column(3,
             br(),
             actionButton(NS(id, "findSites"), "Search for water data"),
      ),
    ),
    fluidRow(
      column(1),
      column(3,
             selectizeInput(NS(id, "site"),
                            "Choose a USGS monitoring site from search results",
                            choices = NULL),
      )
    ),
    textOutput(NS(id, "noResultsMessage"))
  )
}

inputControlsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    sites <- reactiveVal()
    show_no_results <- reactiveVal(FALSE)

    observeEvent(input$findSites, {
      req(input$state)
      req(input$riverinput)
      waiter <- waiter::Waiter$new(id = "findSites")$show()
      on.exit(waiter$hide())
      sites_df <- dischargeDataAvailable(state = input$state, site = input$riverinput)
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

