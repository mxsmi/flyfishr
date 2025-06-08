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

## Set API key
Sys.setenv(GH_MODELS_TOKEN = "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")

ui <- fluidPage(
  waiter::use_waiter(),
  titlePanel("StreamNotes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Choose a State", choices = stateCd$STUSAB),
      textInput("riverinput", "Enter name of a river", placeholder = "Ex: Yellowstone"),
      ## Action button to load data for the selected state and river
      actionButton("findSites", "Search for water data"),
      selectizeInput("site", "Choose a USGS monitoring site", choices = NULL, 
                     options = list(placeholder = "Or search by name")),
      actionButton("generateReport", "Generate fishing report")
    ),
    mainPanel(
      plotOutput("discharge"),
      uiOutput("fishingReport")
    )
  )
)

server <- function(input, output, session) {
  
  ## Set reactive values
  state_data <- reactiveVal()
  fishing_report <- reactiveVal()
  
  ## Clear the fishing report when the state is changed
  observeEvent(input$state, {
    fishing_report("")
  })
  
  observeEvent(input$site, {
    fishing_report("")
  })
  
  ## Load data for the currently selected state
  observeEvent(input$findSites, {
    req(input$state)
    req(input$riverinput)
    waiter <- waiter::Waiter$new(id = "findSites")$show() ## use loading spinner
    on.exit(waiter$hide()) ## close loading spinner when done
    ## (Add) process state data to only include ST/SP that have Flow_Inst data available
    data <- whatNWISsites(stateCd = input$state) ## load data for selected state
    data <- data %>% ## keep only streams and springs that match riverinput
      filter(nchar(site_no) == 8,
             site_tp_cd == "ST" | site_tp_cd == "SP",
             str_detect(tolower(station_nm), tolower(input$riverinput)),
      )
    state_data(data) ## update reactive value
    sites <- dischargeDataAvailable(data) ## get sites that have discharge data
    updateSelectizeInput(session, inputId = "site", 
                         choices = sort(sites),
                         selected = "",
                         server = TRUE)
  })
  
  ## Output discharge plot
  output$discharge <- renderPlot({
    req(input$site)
    site_no <- state_data()$site_no[which(state_data()$station_nm == input$site)]
    dischargePlot(site_no)
  })
  
  ## Generate fishing report when button is pressed
  observeEvent(input$generateReport, {
    req(input$site)
    waiter <- waiter::Waiter$new(id = "generateReport")$show() ## use loading spinner
    on.exit(waiter$hide()) ## close loading spinner when done
    prompt <- fishingReportPrompt(input$site)
    report <- 
    fishing_report(call_llm(
                    prompt = prompt, 
                    provider = "github", 
                    model = "openai/gpt-4.1")
                   )
  })
  
  ## Output fishing report
  output$fishingReport <- renderUI({
    req(!is.null(fishing_report()))
    response <- as.character(fishing_report())
    html_content <- markdownToHTML(text = response, fragment.only = TRUE)
    HTML(html_content)
  })
  
}

shinyApp(ui, server)