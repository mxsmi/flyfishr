### Module that defines UI and associated server function for generating and
### displaying the AI fly-fishing report.

## Retrieve GH_MODELS_TOKEN API key from environment variables
GH_MODELS_TOKEN = Sys.getenv("GH_MODELS_TOKEN")

fishingReportUI <- function(id) {
  tagList(
    column(1),
    column(10,
           br(),
           actionButton(NS(id, "generateReport"), "Generate AI fishing report",
                        class = "btn-secondary",
                        style = "background-color: #0EA5E9BF; border-color: #000000;"
            ),
           br(),
           "(Uses github openai model: openai/gpt-4o)",
           br(),
           br(),
           uiOutput(NS(id, "fishingReport"))
    ),
    column(1),
  )
}

fishingReportServer <- function(id, selected_site, water_data) {
  moduleServer(id, function(input, output, session) {

    ## Reactive value for storing the text of the fly-fishing report
    fishing_report_text <- reactiveVal()

    ## Clear report when site changes
    observeEvent(selected_site(), {
      fishing_report_text("")
    })

    ## Generate report
    observeEvent(input$generateReport, {
      req(selected_site())

      ## Show waiting spinner while the AI model is generating the report
      waiter <- waiter::Waiter$new(NS(id, "generateReport"))$show()
      on.exit(waiter$hide())

      ## Get the prompt for currently selected site, water temperature, and
      ## discharge levels
      prompt <- fishingReportPrompt(
        site = selected_site(),
        temp = water_data$current_temp(),
        flow = water_data$current_discharge()
      )

      showNotification(ui = "Generating fly-fishing report",
                       duration = NULL,
                       id = "loadingData",
                       type = "message"
      )
      ## Get the response from the AI model
      llm_response <- call_llm(
        prompt = prompt,
        provider = "github",
        model = "openai/gpt-4o"
      )

      ## Update reactive value with response text
      fishing_report_text(llm_response)
      removeNotification(id = "loadingData")
    })

    ## Render report
    output$fishingReport <- renderUI({
      req(!is.null(fishing_report_text()))
      response <- as.character(fishing_report_text())
      html_content <- markdownToHTML(text = response, fragment.only = TRUE)
      HTML(html_content)
    })
  })
}
