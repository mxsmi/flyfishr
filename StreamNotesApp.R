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


ui <- fluidPage(
  waiter::use_waiter(),
  selectInput("state", "Choose a State", choices = stateCd$STUSAB),
  ## Action button to load data for the selected state
  actionButton("findRivers", "Find rivers"),
  selectizeInput("river", "Choose a river", choices = NULL, 
                 options = list(placeholder = "Search for a river")),
  plotOutput("discharge")
)

server <- function(input, output, session) {
  
  state_data <- reactiveVal()
  
  observeEvent(input$findRivers, {
    req(input$state)
    waiter <- waiter::Waiter$new(id = "findRivers")$show() ## use loading spinner
    on.exit(waiter$hide()) ## close loading spinner when done
    data <- whatNWISsites(stateCd = input$state) ## load data for selected state
    data <- data %>% 
      filter(str_detect(site_tp_cd, "^(ST|SP)"))
    state_data(data) ## update reactive value
    updateSelectizeInput(session, inputId = "river", 
                         choices = state_data()$station_nm,
                         selected = "",
                         server = TRUE)
  })
  
  output$discharge <- renderPlot({
    req(input$river)
    
    ## Parameters for readNWISuc (the function that gets the data for a specific site)
    siteNo <- state_data()$site_no[which(state_data()$station_nm == input$river)]
    pCode <- "00060"
    start.date <- Sys.Date() - days(5)
    end.date <- Sys.Date()
    sCode <- "00003"

    site <- readNWISuv(
      siteNumbers = siteNo,
      parameterCd = pCode,
      startDate = start.date,
      endDate = end.date
    )
    
    site_stat <- readNWISdv(
      siteNumbers = siteNo,
      parameterCd = pCode,
      startDate = start.date,
      endDate = end.date,
      statCd = sCode
    )
    
    ## dataRetreival's built in clean names function
    site <- renameNWISColumns(site)
    site_stat <- renameNWISColumns(site_stat)
    site_stat$Date <- as_datetime(site_stat$Date)
    
    variableInfo <- attr(site, "variableInfo")
    siteInfo <- attr(site, "siteInfo")
    
    ## Build the plot of water discharge
    # Validate data first
    validate(
      need(nrow(site) > 0, "No discharge data available for this river"),
      need("Flow_Inst" %in% names(site), "Flow data not found"),
    )
    
    ## Build plot
    discharge_plot <- ggplot(
      data = site,
      aes(dateTime, Flow_Inst)
    ) + 
      geom_line(color = "blue") + 
      geom_point(data = site_stat,
                 aes(x = Date, y = Flow),
                 color = "red",
                 shape = 17,
                 size = 3
                 ) +
      labs(
        x = "Date",
        y = variableInfo$variableDescription,
        title = siteInfo$station_nm
      ) + 
      theme_minimal()
      
      ## Return plot
      discharge_plot
  })
  
}

shinyApp(ui, server)