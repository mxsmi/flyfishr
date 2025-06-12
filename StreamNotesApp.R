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

## Set API key
Sys.setenv(GH_MODELS_TOKEN = "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")

## Source required scripts
path <- paste0(getwd(), "/R")
sapply(list.files(path, pattern = "\\.R$", full.names = TRUE), source)

ui <- fluidPage(
  
  waiter::use_waiter(),
  titlePanel("StreamNotes"),
  
  fluidRow(
    column(1),
    column(3,
      selectInput("state", "Choose a State", choices = stateCd$STUSAB)     
    ),
    column(1),
    column(3, 
      textInput("riverinput", "Enter river name to search by", 
                placeholder = "Ex: Yellowstone"),
    ),
    column(1),
    column(3,
      br(), ## Create a line break
      ## Action button to load data for the selected state and river
      actionButton("findSites", "Search for water data"), 
    ),
  ),
  
  fluidRow(
    column(1),
    column(11, 
      selectizeInput("site", 
                     "Choose a USGS monitoring site from search results", 
                     choices = NULL),
    )
  ),
  
  fluidRow(
    tabsetPanel(
      tabPanel("Map",
        leafletOutput("siteMap")
      ),
      tabPanel("Fly Fishing Report",
        column(1),
        column(10,
          br(),
          actionButton("generateReport", "Generate fishing report"),
          br(),
          br(),
          uiOutput("fishingReport")
        ),
        column(1)
      ),
      tabPanel("Charts",
        plotOutput("discharge"),
        plotOutput("waterTemp")
      )
    )
  )  
)

server <- function(input, output, session) {
  
  ## Set reactive values
  state_data <- reactiveVal()
  fishing_report <- reactiveVal()
  siteNo <- reactive({
    req(state_data())
    req(input$site)
    site_no <- state_data()$site_no[which(state_data()$station_nm == input$site)]
  })
  water_temp <- reactive({
    req(input$site)
    req(siteNo())
    temp_data <- readNWISuv(
      siteNumbers = siteNo(),
      parameterCd = "00010",  # Water temp in °F (use 00010 for °C)
      startDate = Sys.Date() - days(5),
      endDate = Sys.Date()
    )
  })
  
  ## Clear the fishing report when the state is changed
  observeEvent(input$state, {
    fishing_report("")
  })
  
  ## Clear the fishing report when the site is changed
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
    site_no <- siteNo()
    dischargePlot(site_no)
  })
  
  ## Output water temp plot
  output$waterTemp <- renderPlot({
    site_no <- siteNo()
    waterTempPlot(site_no)
  })
  
  ## Generate fishing report when button is pressed
  observeEvent(input$generateReport, {
    req(input$site)
    waiter <- waiter::Waiter$new(id = "generateReport")$show() ## use loading spinner
    on.exit(waiter$hide()) ## close loading spinner when done
    currentWaterTemp <- readNWISdv(
      siteNumbers = siteNo(),
      parameterCd = "00010",
      startDate = Sys.Date(),
      endDate = Sys.Date()
    )
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
  
  ## Output map of chosen site
  output$siteMap <- renderLeaflet({
    req(input$site)
    site_no = siteNo()
    createSiteMap(site_no)
  })
  
}

shinyApp(ui, server)