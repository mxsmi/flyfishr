## Shiny app primary document

library(devtools)

# devtools::load_all()
# apikey = Sys.getenv("GH_MODELS_TOKEN")
#
# cat("API key found:", apikey != "", "\n")
# cat("API key length:", nchar(apikey), "\n")
# cat("All environment variables containing 'GH':\n")

# More concise one-liner
# sapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

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
  fishingReportServer("report", apikey, search_data$selected_site, water_data)

  # Automatically bookmark every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  # Update the query string
  onBookmarked(updateQueryString)
}

shinyApp(ui, server, enableBookmarking = "url")

