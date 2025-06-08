### StreamNotes App. A data dashboard for fly fishermen to view current water 
### levels for any body of water in the country and get an AI generated fishing
### report.

## Load required libraries
library(shiny)
library(dataRetrieval)
library(waiter)
library(lubridate)
library(dplyr)
library(stringr)
library(markdown)


ui <- fluidPage(
  waiter::use_waiter(),
  titlePanel("StreamNotes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Choose a State", choices = stateCd$STUSAB),
      ## Action button to load data for the selected state
      actionButton("findRivers", "Find rivers"),
      selectizeInput("river", "Choose a river", choices = NULL, 
                     options = list(placeholder = "Search for a river")),
      actionButton("generateReport", "Generate fishing report")
    ),
    mainPanel(
      plotOutput("discharge"),
      uiOutput("fishingReport")
    )
  )
)

server <- function(input, output, session) {
  
  state_data <- reactiveVal()
  fishing_report <- reactiveVal()
  
  observeEvent(input$findRivers, {
    req(input$state)
    waiter <- waiter::Waiter$new(id = "findRivers")$show() ## use loading spinner
    on.exit(waiter$hide()) ## close loading spinner when done
    data <- whatNWISsites(stateCd = input$state) ## load data for selected state
    data <- data %>% 
      filter(str_detect(site_tp_cd, "^(ST|SP)")) ## keep only streams and springs
    state_data(data) ## update reactive value
    updateSelectizeInput(session, inputId = "river", 
                         choices = state_data()$station_nm,
                         selected = "",
                         server = TRUE)
  })
  
  output$discharge <- renderPlot({
    req(input$river)
    site_no <- state_data()$site_no[which(state_data()$station_nm == input$river)]
    dischargePlot(site_no)
  })
  
  observeEvent(input$generateReport, {
    req(input$river)
    prompt <- fishingReportPrompt(input$river)
    fishing_report(call_llm(prompt = prompt, 
                               provider = "github", 
                               model = "openai/gpt-4.1"))
  })
  
  output$fishingReport <- renderUI({
    req(!is.null(fishing_report()))
    response <- as.character(fishing_report())
    html_content <- markdownToHTML(text = response, fragment.only = TRUE)
    HTML(html_content)
  })
  
}

shinyApp(ui, server)