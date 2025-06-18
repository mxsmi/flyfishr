fishingReportUI <- function(id) {
  tagList(
    column(1),
    column(10,
           br(),
           actionButton(NS(id, "generateReport"), "Generate AI fishing report"),
           br(),
           "(Uses github openai model: openai/gpt-4o)",
           br(),
           br(),
           uiOutput(NS(id, "fishingReport"))
    ),
    column(1)
  )
}

fishingReportServer <- function(id, selected_site, water_data) {
  moduleServer(id, function(input, output, session) {

    fishing_report_text <- reactiveVal()

    # Clear report when site changes
    observeEvent(selected_site(), {
      fishing_report_text("")
    })

    # Generate report
    observeEvent(input$generateReport, {
      req(selected_site())
      waiter <- waiter::Waiter$new(NS(id, "generateReport"))$show()
      on.exit(waiter$hide())

      prompt <- fishingReportPrompt(
        site = selected_site(),
        temp = water_data$current_temp(),
        flow = water_data$current_discharge()
      )

      llm_response <- call_llm(
        prompt = prompt,
        provider = "github",
        model = "openai/gpt-4o"
      )

      fishing_report_text(llm_response)
    })

    # Render report
    output$fishingReport <- renderUI({
      req(!is.null(fishing_report_text()))
      response <- as.character(fishing_report_text())
      html_content <- markdownToHTML(text = response, fragment.only = TRUE)
      HTML(html_content)
    })
  })
}
