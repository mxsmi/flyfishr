
fishLogUI <- function(id) {
  uiOutput(NS(id, "fishlog"))
}

fishLogServer <- function(id, logged_in) {
  moduleServer(id, function(input, output, session) {

    output$fishlog <- renderUI({
      if (!logged_in()) {
        "You must log in to use the fish log"
      } else {
        tagList(
          actionButton(NS(id, "addFishLogLine"), "Add a fish to the log"),
          br(), br(),
          DT::dataTableOutput(NS(id, "fish_table"))
        )
      }
    })

    output$fish_table <- DT::renderDataTable({
      req(logged_in)
      mtcars
    })


    observeEvent(input$addFishLogLine, {
      species <- read.csv("R/fly_fishing_species.csv")
      flies <- read.csv("R/fly_patterns.csv")
      locations <- read.csv("R/fly_fishing_rivers.csv")
      weather <- readLines("R/weather_conditions.txt")
      showModal(modalDialog(
        title = "Enter data about your catch",
        size = "l",
        dateInput("date_caught", "Date"),
        shinyWidgets::timeInput("time_caught", "Time"),
        selectInput(NS(id, "location_caught"), "Location",
                   choices = c("Choose a location" = "", paste(paste0(locations$river_name, ","), locations$state_abb)),
                   selectize = TRUE),
        selectInput("weather_conditions", "Weather",
                    choices = c("Choose an option" = "", weather),
                    selectize = TRUE),
        selectInput(NS(id, "species"), "Species",
                    choices = c("Choose a species" = "", species$fish_name),
                    selectize = TRUE),
        numericInput("approx_length", "Approximate length (inches):", value = NA),
        numericInput("approx_weight", "Approximate weight (lbs):", value = NA),
        selectInput(NS(id, "fly_type"), "Fly",
                    choices = c("Choose a fly" = "", flies$fly_name, "Other specify"),
                    selectize = TRUE),
        fileInput("fish_photo", "Upload Photo (optional)",
                  accept = c('.jpg', '.jpeg', '.png', '.heic', '.heif', '.webp'),
                  width = "100%")
      ))
    })

  })
}
