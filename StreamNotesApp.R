### Psuedocode for StreamNotes app

# UI:

  # Have user pick state first (use stateCD dataset from dataRetrieval), then 
  # pick site. 

  # Display Discharge cubic feet per second plot

library(shiny)
library(dataRetrieval)

ui <- fluidPage(
  selectInput("state", "Choose a State", choices = stateCd$STUSAB),
  actionButton("findRivers", "Find rivers"),
  selectizeInput("river", "Choose a river", choices = NULL, 
                 options = list(placeholder = "Search for a river"))
)

server <- function(input, output, session) {
  
  observeEvent(input$findRivers, {
    req(input$state)
    state_data <- whatNWISsites(stateCd = input$state)
    updateSelectizeInput(session, inputId = "river", 
                         choices = state_data$station_nm,
                         selected = "",
                         server = TRUE)
  })
  
}

shinyApp(ui, server)