## Module for the Fish Log UI and server code

fishLogUI <- function(id) {
  ### UI output placeholder for displaying the Fish Log
  uiOutput(NS(id, "fishlog"))
}

fishLogServer <- function(id, pool, logged_in) {
  moduleServer(id, function(input, output, session) {

    ### Load the files containing information about choices for species, files,
    ### locations, and weather conditions
    species <- read.csv("R/fly_fishing_species.csv")
    flies <- read.csv("R/fly_patterns.csv")
    locations <- read.csv("R/fly_fishing_rivers.csv")
    weather <- readLines("R/weather_conditions.txt")
    ### Reactive value used for refreshing the Fish Log data displayed in the app
    values <- reactiveValues(refresh_trigger = 0)

    ### Reactive containing the rows to display in the Fish Log for the currently
    ### logged in user
    fish_data <- reactive({
      ### Refresh_trigger to re-calculate the rows
      values$refresh_trigger
      ### Requires a user to be logged in
      req(logged_in()[[1]])
      ### Capture the current user id
      user_id <- logged_in()[[2]]
      ### Pull the fish data for the currently logged in user
      fish_log <- dbGetQuery(pool,
                             "SELECT FISH_ID, USER_ID, CATCH_DATE, CATCH_TIME, CATCH_WATER,
                             CATCH_STATE, WEATHER, SPECIES, APPROX_LENGTH,
                             APPROX_WEIGHT, FLY, PHOTO
                             FROM FISH_LOG
                             WHERE USER_ID = ?;",
                             params = list(user_id))
      ### Return the fish data for the currently logged in user
      fish_log
    })

    ### Render the Fish Log UI
    output$fishlog <- renderUI({
      ### If no user is logged in, display message
      if (!logged_in()[[1]]) {
        "You must log in to use the fish log"
      ### If a user is logged in, display their Fish Log
      } else {
        tagList(
          br(),
          ### UI code for buttons for adding, deleting, and updating a log line
          fluidRow(
            column(3, actionButton(NS(id, "addFishLogLine"), "Add log line", width = "100%",
                                   style = "background-color: #0EA5E9BF; border-color: #000000;")),
            column(3, actionButton(NS(id, "delete_log_line"), "Delete log line", width = "100%",
                                   style = "background-color: #F87171BF; border-color: #000000;")),
            column(3, actionButton(NS(id, "edit_log_line"), "Edit log line", width = "100%",
                                   style = "background-color: #0EA5E9BF; border-color: #000000;"))
          ),
          br(),
          ### Display the Fish Log as a DT::datatable
          DT::dataTableOutput(NS(id, "fish_table"))
        )
      }
    })

    ### Server code for rendering Fish Log datatable
    output$fish_table <- DT::renderDataTable({
      ### Cast fish_data() reactive as a datatable. Select only the columns other
      ### than the first two (FISH_ID and USER_ID).
      datatable(fish_data()[,3:length(fish_data())],
                escape = FALSE,  # Allows HTML links to work
                options = list(
                  pageLength = 10,
                  scrollX = TRUE
                )
      )
    })

    ### Show modal for user to enter data for a new log line
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

    ### Add new log line data to the database when user clicks "Submit"
    observeEvent(input$submit_log_line, {
      ### If no catch date, body of water, or species are entered, show notification
      ### saying they are required.
      if (length(input$catch_date) == 0) {
        showNotification("Date is required", type = "error")
      } else if (input$catch_water == "") {
        showNotification("Body of water is required", type = "error")
      } else if (input$species == "") {
        showNotification("Species is required", type = "error")
      } else {
        ### Capture user_id
        user_id <- logged_in()[[2]]
        ### Body of water is a drop-down menu of options in the form 'water name, state'.
        ### Water name and state should be stored as separate data points in the database
        ### so we have to split them up here.
        split_catch_water <- str_split_1(input$catch_water, pattern = ", ")
        catch_water <- split_catch_water[1]
        catch_state <- split_catch_water[2]

        ### If photo_path is null, change it to NA to be compatible with inserting
        ### into the database. Otherwise, extract out just the datapath.
        photo_path <- if(is.null(input$fish_photo)) {
          NA
        } else {
          input$fish_photo$datapath[1]
        }
        ### Insert the data for the new log line into the database
        dbExecute(pool,
           "INSERT INTO FISH_LOG
           (USER_ID, CATCH_DATE, CATCH_TIME, CATCH_WATER, CATCH_STATE, WEATHER,
           SPECIES, APPROX_LENGTH, APPROX_WEIGHT, FLY, PHOTO) VALUES
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
        ### Activate the refresh trigger to update the reactive fish_data() containing
        ### the rows to be displayed in the Fish_Log UI
        values$refresh_trigger <- values$refresh_trigger + 1 %% 2
        ### Show notification that log line was successfully added
        showNotification("Catch added to your Fish Log!", type = "message")
        ### Remove modal
        removeModal()
      }
    })

    ### Delete currently selected log line(s)
    observeEvent(input$delete_log_line, {
      ### Pull out the FISH_ID's of the currently selected log lines(s) to delete
      delete <- fish_data()[input$fish_table_rows_selected, ]$FISH_ID
      ### If there is at least one currently selected log line, delete it
      if (length(delete) > 0) {
        ### Placeholder string (a vector of '?') the length of the FISH_ID's to delete
        placeholders <- paste(rep("?", length(delete)), collapse = ",")
        ### Execture query to delete those rows from the database
        dbExecute(pool,
                  paste("DELETE FROM FISH_LOG WHERE FISH_ID IN (", placeholders, ")"),
                  params = as.list(delete))
        ### Activate refresh trigger to update the reactive containing the data to
        ### display in the Fish Log UI
        values$refresh_trigger <- values$refresh_trigger + 1 %% 2
        ### Show notification stating the the log line was deleted
        showNotification("Log line deleted", type = "message")
      } else {
        ### If no log lines are selected, display message
        showNotification("You must select log line(s) to delete", type = "error")
      }
    })

    ### Edit currently selected log line
    observeEvent(input$edit_log_line, {
      ### If there are no log lines selected, or more than one selected, show error message
      if (length(input$fish_table_rows_selected) == 0) {
        showNotification("No log lines are selected. Click on a row to select it.", type = "error")
      } else if (length(input$fish_table_rows_selected) > 1) {
        showNotification("You can only edit one log line at a time", type = "error")
      } else {
        ### Capture FISH_ID of the log line to edit
        fish_id <- fish_data()[input$fish_table_rows_selected,]$FISH_ID
        ### Capture the row with the FISH_ID to be updated
        log_line <- dbGetQuery(pool,
                               "SELECT DATE_FORMAT(CATCH_DATE, '%Y-%m-%d') as CATCH_DATE,
                               TIME_FORMAT(CATCH_TIME, '%H:%i:%s') as CATCH_TIME,
                               CATCH_WATER, CATCH_STATE, WEATHER, SPECIES,
                               APPROX_LENGTH, APPROX_WEIGHT, FLY, PHOTO
                               FROM FISH_LOG
                               WHERE FISH_ID = ?;",
                               params = list(fish_id)
                               )[1,]
        ### Capture fly as a character to use as an initally selected value in the
        ### form later
        fly <- as.character(log_line$FLY)
        ### Convert log_line to a character vector
        log_line <- as.character(log_line)
        ### Combine water name and state together so that it has the form 'water name, state'
        ### and use that to replace the third element (water name)
        log_line[3] = paste(log_line[3], log_line[4], sep = ", ")
        ### Remove the fourth element, which is state
        log_line <- log_line[-4]
        ### UI form for editing the log line. Set initially selected values to the
        ### values pulled from the database for that log line
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
        ### Update the initally selected value for fly, because for some reason the
        ### selected argument is not working
        updateSelectInput(session, inputId = "fly_type", selected = log_line[8])
      }
    })

    ### Update the log line after the user clicks "Update"
    observeEvent(input$update_log_line, {
      ### If catch date, body of water, or species are not entered, show an error
      ### message.
      if (length(input$catch_date) == 0) {
        showNotification("Date is required", type = "error")
      } else if (input$catch_water == "") {
        showNotification("Body of water is required", type = "error")
      } else if (input$species == "") {
        showNotification("Species is required", type = "error")
      } else {
        ### Capture FISH_ID of the log line to be updated
        fish_id <- fish_data()[input$fish_table_rows_selected,]$FISH_ID
        ### Capture the photo path that was previously in the database for this
        ### log line (if any)
        old_photo_path <- dbGetQuery(pool,
                                     "SELECT PHOTO FROM FISH_LOG
                                   WHERE FISH_ID = ?;",
                                     params = list(fish_id)
        )[1,1]
        ### Capture the USER_ID of the currenly logged in user
        user_id <- logged_in()[[2]]
        ### Body of water is a drop-down menu of options in the form 'water name, state'.
        ### Water name and state should be stored as separate data points in the database
        ### so we have to split them up here.
        split_catch_water <- str_split_1(input$catch_water, pattern = ", ")
        catch_water <- split_catch_water[1]
        catch_state <- split_catch_water[2]

        ### If there was no photo path in the database already, and the user did
        ### not update the photo path, keep new photo path and the old one (nothing)
        if (!is.na(old_photo_path) & is.null(input$fish_photo)) {
          photo_path <- old_photo_path
        ### If old photo path is not NA but the new photopath is null, change
        ### photo path to NA to be compatible with inserting into the database
        } else if (is.null(input$fish_photo)) {
          photo_path <- NA
        ### If old photo path is not NA and the new photo path is not null, use
        ### the new photo path
        } else {
          photo_path <- input$fish_photo
        }
        ### Update the row corresponding to this log line in the database
        dbExecute(pool,
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
        ### Activate the refresh trigger to refresh the data displayed in the
        ### Fish Log UI
        values$refresh_trigger <- values$refresh_trigger + 1 %% 2
        ### Show notification saying that the log line was updated
        showNotification("Updated the log line!", type = "message")
        ### Remove modal
        removeModal()
      }
    })
  })
}
