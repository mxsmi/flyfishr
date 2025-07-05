### This file contains a function called flyfishrApp which runs the app locally
### from the flyfishr package. The app is a data dashboard for fly fishermen to
### view a map, current water levels, and get an AI generated fishing report.

## Define 'flyfishrApp' function
flyfishrApp <- function(...) {

  ## Load required libraries
  library(shiny)
  library(dataRetrieval)
  library(ggplot2)
  library(waiter)
  library(lubridate)
  library(dplyr)
  library(stringr)
  library(markdown)
  library(chatLLM)
  library(leaflet)
  library(jsonlite)

  base_theme <- bslib::bs_theme(bootswatch = "lux")

  ui <- function(request) {
    fluidPage(
      theme = bslib::bs_theme_update(theme = base_theme,
                                     fg = "#000000",
                                     bg = "#F2E0CF",
                                     primary = "#000000",
                                     secondary = "#00CED14D"
      ),
      waiter::use_waiter(),
      ## Title panel
      titlePanel("flyfishr App"),

      ## Module containing the UI input controls
      inputControlsUI("controls"),

      ## Modules for the Map, Flow/Temp graphs, and fly-fishing report
      tabsetPanel(
        tabPanel("Map", mapUI("map")),
        tabPanel("Flows & Temperature", chartsUI("charts")),
        tabPanel("Fly Fishing Report", fishingReportUI("report"))
      )
    )
  }

  server <- function(input, output, session) {

    ## Get water USGS data once a state is selected
    search_data <- inputControlsServer("controls")

    ## Get discharge and water temperature data once a site is selected
    water_data <- waterDataServer(id = "data",
                                  sites = search_data$sites,
                                  selected_site = search_data$selected_site)

    ## Create discharge and water temperature plots
    chartsServer("charts", water_data)

    ## Create Map
    mapServer("map", water_data)

    ## Create fly-fishing report
    fishingReportServer("report", search_data$selected_site, water_data)

    # Automatically bookmark every time an input changes
    observe({
      reactiveValuesToList(input)
      session$doBookmark()
    })
    # Update the query string
    onBookmarked(updateQueryString)
  }

  shinyApp(ui, server, enableBookmarking = "url")
}

