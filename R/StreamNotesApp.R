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

streamNotesApp <- function(...) {
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

    ## Data for all the sites in a state
    sites <- reactiveVal()

    ## Contains AI generated fishing report text
    fishing_report_text <- reactiveVal()

    ## Contains currently selected site number
    siteNo <- reactive({
      req(sites())
      req(input$site)
      site_no <- sites() %>%
        filter(station_nm == input$site) %>%
        pull(site_no)
      return(site_no)
    })

    ## Contains discharge (cfs) data for currently selected site
    discharge <- reactive({
      req(input$site)
      req(siteNo())
      temp_data <- readNWISuv(
        siteNumbers = siteNo(),
        parameterCd = "00060",  # Water temp in °F (use 00010 for °C)
        startDate = Sys.Date() - days(5),
        endDate = Sys.Date()
      ) %>%
        renameNWISColumns() %>%
        arrange(desc(dateTime))
      return(temp_data)
    })

    ## Contains water temperature data (F) for currently selected site
    water_temp <- reactive({
      req(input$site)
      req(siteNo())
      temp_data <- readNWISuv(
        siteNumbers = siteNo(),
        parameterCd = "00010",  # Water temp in °F (use 00010 for °C)
        startDate = Sys.Date() - days(5),
        endDate = Sys.Date()
      ) %>%
        renameNWISColumns()
      if ("Wtemp_Inst" %in% names(temp_data)) {
        temp_data <- temp_data %>% mutate(
          Wtemp_Inst = (Wtemp_Inst * 9/5) + 32
        ) %>%
          arrange(desc(dateTime))
      }
      return(temp_data)
    })

    ## Contains most current water temp (F) reading for currently selected site
    current_temp <- reactive({
      req(water_temp())
      if ("Wtemp_Inst" %in% names(water_temp())) {
        return(water_temp()$Wtemp_Inst[1])
      } else {
        return("Not available")
      }
    })

    ## Contains most current discharge (cfs) reading for currently selected site
    current_discharge <- reactive({
      req(discharge())
      return(discharge()$Flow_Inst[1])
    })

    ## Clear the fishing report when the state is changed
    observeEvent(input$state, {
      fishing_report_text("")
    })

    ## Clear the fishing report when the site is changed
    observeEvent(input$site, {
      fishing_report_text("")
    })

    ## Load data for the currently selected state and search term
    observeEvent(input$findSites, {
      req(input$state)
      req(input$riverinput)
      waiter <- waiter::Waiter$new(id = "findSites")$show() ## use loading spinner
      on.exit(waiter$hide()) ## close loading spinner when done
      sites_df <- dischargeDataAvailable(state = input$state, site = input$riverinput) ## get sites that have discharge data
      sites(sites_df)
      updateSelectizeInput(session, inputId = "site",
                           choices = sort(sites_df$station_nm),
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
      prompt <- fishingReportPrompt(site = input$site,
                                    temp = current_temp(),
                                    flow = current_discharge())
      llm_response <- call_llm(
          prompt = prompt,
          provider = "github",
          model = "openai/gpt-4.1")

      fishing_report_text(llm_response)
    })

    ## Output fishing report
    output$fishingReport <- renderUI({
      req(!is.null(fishing_report_text()))
      response <- as.character(fishing_report_text())
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
}

