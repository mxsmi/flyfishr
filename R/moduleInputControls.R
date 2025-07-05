### Module that defines the UI input controls and the server functions associated
### with them.

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

    ## Reactive value which stores the sites with discharge data for selected state
    sites <- reactiveVal()
    ## Reactive value which is TRUE if 'sites' is mpty or FALSE if it is non-empty
    show_no_results <- reactiveVal(FALSE)

    ## Whenever state input changes, load sites with discharge data and update
    ## the 'site' drop-down menu
    observeEvent(input$state, {
      req(input$state)
      ## Show a notifcation while loading sites with discharge data
      waiter <- waiter::Waiter$new(color = "#32CD324D")$show()
      on.exit(waiter$hide())
      showNotification(ui = "Fetching USGS sites with discharge data. This may take a few minutes.",
                       duration = NULL,
                       id = "loadingData",
                       type = "message"
      )
      ## Sites with discharge data available
      sites_df <- dischargeDataAvailable(state = input$state)
      if (is.null(sites_df)) {
        show_no_results(TRUE)
        sites(NULL)
        updateSelectizeInput(session, inputId = "site", choices = NULL)
      } else {
        show_no_results(FALSE)
        sites(sites_df)
        ## Update the 'site' drop-down menu
        updateSelectizeInput(session, inputId = "site",
                             choices = sort(sites_df$station_nm),
                             selected = "",
                             server = TRUE)
      }
      ## Remove notification after done loading sites with discharge data for
      ## the selected site
      removeNotification(id = "loadingData")
    })

    ## If no sites with discharge data were found for the selected state, show
    ## text saying that no discharge data was found
    output$noResultsMessage <- renderText({
      if (show_no_results()) {
        "No discharge results found for this state and search term"
      } else {
        ""
      }
    })

    # Return reactive values to flyfishrApp.R
    return(list(
      sites = sites,
      selected_site = reactive(input$site),
      selected_state = reactive(input$state)
    ))
  })
}

