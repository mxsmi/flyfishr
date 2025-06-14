chartsUI <- function(id) {
  tagList(
    plotOutput(NS(id, "discharge")),
    plotOutput(NS(id, "waterTemp"))
  )
}

chartsServer <- function(id, water_data) {
  moduleServer(id, function(input, output, session) {

    output$discharge <- renderPlot({
      req(water_data$siteNo())
      plotDischarge(water_data$siteNo())
    })

    output$waterTemp <- renderPlot({
      req(water_data$siteNo())
      plotWaterTemp(water_data$siteNo())
    })
  })
}
