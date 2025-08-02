
fishLogUI <- function(id) {
  uiOutput(NS(id, "fishlog"))
}

fishLogServer <- function(id, logged_in) {
  moduleServer(id, function(input, output, session) {

    species <- read.csv("R/fly_fishing_species.csv")
    flies <- read.csv("R/fly_patterns.csv")
    locations <- read.csv("R/fly_fishing_rivers.csv")
    weather <- readLines("R/weather_conditions.txt")

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
          br(),
          fluidRow(
            column(3, actionButton(NS(id, "addFishLogLine"), "Add log line", width = "100%",
                                   style = "background-color: #0EA5E9BF; border-color: #000000;")),
            column(3, actionButton(NS(id, "delete_log_line"), "Delete log line", width = "100%",
                                   style = "background-color: #F87171BF; border-color: #000000;")),
            column(3, actionButton(NS(id, "edit_log_line"), "Edit log line", width = "100%",
                                   style = "background-color: #0EA5E9BF; border-color: #000000;"))
          ),
          br(),
          DT::dataTableOutput(NS(id, "fish_table"))
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
                    choices = c("Choose a fly" = "", flies$fly_name),
                    selectize = TRUE),
        fileInput(NS(id, "fish_photo"), "Upload Photo (optional)",
                  accept = c('.jpg', '.jpeg', '.png', '.heic', '.heif', '.webp'),
                  width = "100%"),
        "* Indicates a required field",
        footer = tagList(
          actionButton(NS(id, "submit_log_line"), "Submit",
                       style = "background-color: #0EA5E9BF; border-color: #000000;"),
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
      } else {
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
        showNotification("Catch added to your Fish Log!", type = "message")
        removeModal()
      }
    })

    observeEvent(input$delete_log_line, {
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
      showNotification("Log line deleted", type = "message")
    })

    observeEvent(input$edit_log_line, {

      if (length(input$fish_table_rows_selected) == 0) {
        showNotification("No log lines are selected. Click on a row to select it.", type = "error")
      } else if (length(input$fish_table_rows_selected) > 1) {
        showNotification("You can only edit one log line at a time", type = "error")
      } else {
        fish_id <- fish_data()[input$fish_table_rows_selected,]$FISH_ID

        conn <- dbConnect(MariaDB(),
                          host = Sys.getenv("DB_HOST"),
                          port = Sys.getenv("DB_PORT"),
                          user = Sys.getenv("DB_USER"),
                          password = Sys.getenv("DB_PASSWORD"),
                          dbname = Sys.getenv("DB_NAME")
        )

        log_line <- dbGetQuery(conn,
                               "SELECT DATE_FORMAT(CATCH_DATE, '%Y-%m-%d') as CATCH_DATE,
                               TIME_FORMAT(CATCH_TIME, '%H:%i:%s') as CATCH_TIME,
                               CATCH_WATER, CATCH_STATE, WEATHER, SPECIES,
                               APPROX_LENGTH, APPROX_WEIGHT, FLY, PHOTO
                               FROM FISH_LOG
                               WHERE FISH_ID = ?;",
                               params = list(fish_id)
                               )[1,]

        fly <- as.character(log_line$FLY)
        print(fly)
        log_line <- as.character(log_line)
        log_line[3] = paste(log_line[3], log_line[4], sep = ", ")
        print(log_line)
        log_line <- log_line[-4]

        dbDisconnect(conn)

        # print(log_line)
        # print(log_line[8])
        # print(class(log_line[8]))
        # print(class(flies$fly_name))
        # print(log_line[8] %in% flies$fly_name)
        showModal(modalDialog(
          title = "Update log line",
          size = "l",
          dateInput(NS(id, "catch_date"), "Date *", value = log_line[1]),
          shinyWidgets::timeInput(NS(id, "catch_time"), "Time", value = log_line[2]),
          selectInput(NS(id, "catch_water"), "Body of water *",
                      choices = c("Choose a body of water" = "", paste(paste0(locations$river_name, ","), locations$state_abb)),
                      selectize = TRUE,
                      selected = log_line[3]),
          selectInput(NS(id, "weather_conditions"), "Weather",
                      choices = c("Choose an option" = "", weather),
                      selectize = TRUE,
                      selected = log_line[4]),
          selectInput(NS(id, "species"), "Species *",
                      choices = c("Choose a species" = "", species$fish_name),
                      selectize = TRUE,
                      selected = log_line[5]),
          numericInput(NS(id, "approx_length"), "Approximate length (inches):", value = log_line[6]),
          numericInput(NS(id, "approx_weight"), "Approximate weight (lbs):", value = log_line[7]),
          selectInput(NS(id, "fly_type"), label =  "Fly",
                      choices = c("Choose a fly" = "", flies$fly_name),
                      selectize = TRUE),
          fileInput(NS(id, "fish_photo"), "Upload Photo (optional)",
                    accept = c('.jpg', '.jpeg', '.png', '.heic', '.heif', '.webp'),
                    width = "100%"),
          "* Indicates a required field",
          footer = tagList(
            actionButton(NS(id, "update_log_line"), "Update",
                         style = "background-color: #0EA5E9BF; border-color: #000000;"),
            modalButton("Cancel")
          )
        ))

        updateSelectInput(session, inputId = "fly_type", selected = log_line[8])
      }

    })

    observeEvent(input$update_log_line, {

      if (length(input$catch_date) == 0) {
        showNotification("Date is required", type = "error")
      } else if (input$catch_water == "") {
        showNotification("Body of water is required", type = "error")
      } else if (input$species == "") {
        showNotification("Species is required", type = "error")
      } else {
        conn <- dbConnect(MariaDB(),
                          host = Sys.getenv("DB_HOST"),
                          port = Sys.getenv("DB_PORT"),
                          user = Sys.getenv("DB_USER"),
                          password = Sys.getenv("DB_PASSWORD"),
                          dbname = Sys.getenv("DB_NAME")
        )

        fish_id <- fish_data()[input$fish_table_rows_selected,]$FISH_ID
        old_photo_path <- dbGetQuery(conn,
                                     "SELECT PHOTO FROM FISH_LOG
                                   WHERE FISH_ID = ?;",
                                     params = list(fish_id)
        )[1,1]

        user_id <- logged_in()[[2]]
        print(user_id)
        split_catch_water <- str_split_1(input$catch_water, pattern = ", ")
        catch_water <- split_catch_water[1]
        catch_state <- split_catch_water[2]

        if (!is.na(old_photo_path) & is.null(input$fish_photo)) {
          photo_path <- old_photo_path
        } else if (is.null(input$fish_photo)) {
          photo_path <- NA
        } else {
          photo_path <- input$fish_photo
        }

        dbExecute(conn,
                "UPDATE FISH_LOG
                SET CATCH_DATE = ?, CATCH_TIME = ?, CATCH_WATER = ?, CATCH_STATE = ?,
                WEATHER = ?, SPECIES = ?, APPROX_LENGTH = ?, APPROX_WEIGHT = ?, FLY = ?,
                PHOTO = ?
                WHERE FISH_ID = ?;",
                  params = list(
                    input$catch_date,
                    input$catch_time,
                    catch_water,
                    catch_state,
                    input$weather_conditions,
                    input$species,
                    input$approx_length,
                    input$approx_weight,
                    input$fly_type,
                    photo_path,
                    fish_id
                  )
        )

        dbDisconnect(conn)
        values$refresh_trigger <- values$refresh_trigger + 1 %% 2
        showNotification("Updated the log line!", type = "message")
        removeModal()
      }
    })
  })
}
