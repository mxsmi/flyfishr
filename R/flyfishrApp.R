### StreamNotes App. A data dashboard for fly fishermen to view current water
### levels for any body of water in the country and get an AI generated fishing
### report.

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

# ## Set API key
# if (file.exists(".Renviron")) {
#   readRenviron(".Renviron")
# }

flyfishrApp <- function(...) {
  ui <- function(request) {
    fluidPage(

      waiter::use_waiter(),
      titlePanel("flyfishr App"),

      inputControlsUI("controls"),

      tabsetPanel(
        tabPanel("Map", mapUI("map")),
        tabPanel("Fly Fishing Report", fishingReportUI("report")),
        tabPanel("Charts", chartsUI("charts"))
      )
    )
  }

  server <- function(input, output, session) {

    search_data <- inputControlsServer("controls")
    water_data <- waterDataServer(id = "data",
                                  sites = search_data$sites,
                                  selected_site = search_data$selected_site)
    chartsServer("charts", water_data)
    mapServer("map", water_data)
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

