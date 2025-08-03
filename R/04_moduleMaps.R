### Module that defines the UI for displaying the map of the currently selected
### site, and the associated server function

mapUI <- function(id) {
  leafletOutput(NS(id, "siteMap"))
}

mapServer <- function(id, water_data) {
  moduleServer(id, function(input, output, session) {

    output$siteMap <- renderLeaflet({
      req(water_data$siteNo())
      createSiteMap(water_data$siteNo())
    })
  })
}
