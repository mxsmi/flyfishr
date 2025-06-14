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
