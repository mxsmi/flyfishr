
fishLogUI <- function(id) {
  uiOutput(NS(id, "fishlog"))
}

fishLogServer <- function(id, logged_in) {
  moduleServer(id, function(input, output, session) {

    values <- reactiveValues(refresh_trigger = 0)

    fish_data <- reactive({

      values$refresh_trigger

      req(logged_in()[[1]])
      conn <- dbConnect(MariaDB(),
                        host = Sys.getenv("DB_HOST"),
                        port = Sys.getenv("DB_PORT"),
                        user = Sys.getenv("DB_USER"),
                        password = Sys.getenv("DB_PASSWORD"),
                        dbname = Sys.getenv("DB_NAME")
      )

      fish_log <- dbGetQuery(conn,
                             "SELECT FISH_ID, USER_ID, CATCH_DATE, CATCH_TIME, CATCH_WATER,
                             CATCH_STATE, WEATHER, SPECIES, APPROX_LENGTH,
                             APPROX_WEIGHT, FLY, PHOTO
                             FROM FISH_LOG")

      dbDisconnect(conn)
      fish_log
    })

    output$fishlog <- renderUI({
      if (!logged_in()[[1]]) {
        "You must log in to use the fish log"
      } else {
        tagList(
          actionButton(NS(id, "addFishLogLine"), "Add a fish to the log"),
          br(), br(),
          DT::dataTableOutput(NS(id, "fish_table")),
          br(),
          actionButton(NS(id, "delete_log_line"), "Delete log line"),
          br()
        )
      }
    })

    output$fish_table <- DT::renderDataTable({
      datatable(fish_data()[,3:length(fish_data())],
                escape = FALSE,  # Allows HTML links to work
                options = list(
                  pageLength = 10,
                  scrollX = TRUE
                )
      )
    })


    observeEvent(input$addFishLogLine, {
      species <- read.csv("R/fly_fishing_species.csv")
      flies <- read.csv("R/fly_patterns.csv")
      locations <- read.csv("R/fly_fishing_rivers.csv")
      weather <- readLines("R/weather_conditions.txt")
      showModal(modalDialog(
        title = "Enter data about your catch",
        size = "l",
        dateInput(NS(id, "catch_date"), "Date *"),
        shinyWidgets::timeInput(NS(id, "catch_time"), "Time"),
        selectInput(NS(id, "catch_water"), "Body of water *",
                   choices = c("Choose a body of water" = "", paste(paste0(locations$river_name, ","), locations$state_abb)),
                   selectize = TRUE),
        selectInput(NS(id, "weather_conditions"), "Weather",
                    choices = c("Choose an option" = "", weather),
                    selectize = TRUE),
        selectInput(NS(id, "species"), "Species *",
                    choices = c("Choose a species" = "", species$fish_name),
                    selectize = TRUE),
        numericInput(NS(id, "approx_length"), "Approximate length (inches):", value = NA),
        numericInput(NS(id, "approx_weight"), "Approximate weight (lbs):", value = NA),
        selectInput(NS(id, "fly_type"), "Fly",
                    choices = c("Choose a fly" = "", flies$fly_name, "Other specify"),
                    selectize = TRUE),
        fileInput(NS(id, "fish_photo"), "Upload Photo (optional)",
                  accept = c('.jpg', '.jpeg', '.png', '.heic', '.heif', '.webp'),
                  width = "100%"),
        "* Indicates a required field",
        footer = tagList(
          actionButton(NS(id, "submit_log_line"), "Submit"),
          modalButton("Cancel")
        )
      ))
    })

    observeEvent(input$submit_log_line, {
      if (length(input$catch_date) == 0) {
        showNotification("Date is required", type = "error")
      } else if (input$catch_water == "") {
        showNotification("Body of water is required", type = "error")
      } else if (input$species == "") {
        showNotification("Species is required", type = "error")
      }

      conn <- dbConnect(MariaDB(),
                        host = Sys.getenv("DB_HOST"),
                        port = Sys.getenv("DB_PORT"),
                        user = Sys.getenv("DB_USER"),
                        password = Sys.getenv("DB_PASSWORD"),
                        dbname = Sys.getenv("DB_NAME")
      )

      user_id <- logged_in()[[2]]
      print(user_id)
      split_catch_water <- str_split_1(input$catch_water, pattern = ", ")
      catch_water <- split_catch_water[1]
      catch_state <- split_catch_water[2]

      # Fix the photo handling
      photo_path <- if(is.null(input$fish_photo)) {
        NA
      } else {
        input$fish_photo$datapath[1]
      }

      dbExecute(conn,
                "INSERT INTO FISH_LOG
           (USER_ID, CATCH_DATE, CATCH_TIME, CATCH_WATER, CATCH_STATE, WEATHER, SPECIES, APPROX_LENGTH, APPROX_WEIGHT, FLY, PHOTO) VALUES
           (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);",
            params = list(
              user_id,
              input$catch_date,
              input$catch_time,
              catch_water,
              catch_state,
              input$weather_conditions,
              input$species,
              input$approx_length,
              input$approx_weight,
              input$fly_type,
              photo_path
            )
      )

      dbDisconnect(conn)
      values$refresh_trigger <- values$refresh_trigger + 1 %% 2
      showNotification("Catch added to your Fish Log!")
      removeModal()
    })

    observeEvent(input$delete_log_line, {
      # print(input$fish_table_rows_selected)
      delete <- fish_data()[input$fish_table_rows_selected, ]$FISH_ID

      conn <- dbConnect(MariaDB(),
                        host = Sys.getenv("DB_HOST"),
                        port = Sys.getenv("DB_PORT"),
                        user = Sys.getenv("DB_USER"),
                        password = Sys.getenv("DB_PASSWORD"),
                        dbname = Sys.getenv("DB_NAME")
      )

      # Instead of multiple DELETE statements, use one with IN clause
      if (length(delete) > 0) {
        placeholders <- paste(rep("?", length(delete)), collapse = ",")
        dbExecute(conn,
                  paste("DELETE FROM FISH_LOG WHERE FISH_ID IN (", placeholders, ")"),
                  params = as.list(delete))
      }

      dbDisconnect(conn)
      values$refresh_trigger <- values$refresh_trigger + 1 %% 2
    })

  })
}
