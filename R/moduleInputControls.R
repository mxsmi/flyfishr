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
      column(11,
             selectizeInput(NS(id, "site"),
                            "Choose a USGS monitoring site from search results",
                            choices = NULL),
      )
    )
  )
}

inputControlsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    sites <- reactiveVal()

    observeEvent(input$findSites, {
      req(input$state)
      req(input$riverinput)
      waiter <- waiter::Waiter$new(id = "findSites")$show()
      on.exit(waiter$hide())
      sites_df <- dischargeDataAvailable(state = input$state, site = input$riverinput)
      sites(sites_df)
      updateSelectizeInput(session, inputId = "site",
                           choices = sort(sites_df$station_nm),
                           selected = "",
                           server = TRUE)
    })

    # Return reactive values for parent app
    return(list(
      sites = sites,
      selected_site = reactive(input$site),
      selected_state = reactive(input$state)
    ))
  })
}
