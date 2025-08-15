## Module for the Fish Log UI and server code

fishLogUI <- function(id) {
  ### UI output placeholder for displaying the Fish Log
  uiOutput(NS(id, "fishlog"))
}

fishLogServer <- function(id, pool, logged_in) {
  moduleServer(id, function(input, output, session) {

    ### Load the files containing information about choices for species, files,
    ### locations, and weather conditions
    species <- read.csv("data/fly_fishing_species.csv")
    locations <- read.csv("data/fly_fishing_rivers.csv")
    weather <- readLines("data/weather_conditions.txt")
    presentations <- readLines("data/presentation.txt")
    patterns <- readLines("data/pattern.txt")
    length_units <- readLines("data/length_units.txt")
    weight_units <- readLines("data/weight_units.txt")
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
                             "SELECT FISH_ID, USER_ID,
                             DATE_FORMAT(CATCH_DATE, '%Y-%m-%d') as CATCH_DATE,
                             TIME_FORMAT(CATCH_TIME, '%h:%i %p') as CATCH_TIME,
                             CATCH_WATER, CATCH_STATE, WEATHER, SPECIES, APPROX_LENGTH,
                             LENGTH_UNITS, APPROX_WEIGHT, WEIGHT_UNITS, PRESENTATION,
                             FLY_PATTERN, FLY_NAME, PHOTO_DATA, NOTES FROM FISH_LOG
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

    # Function to get MIME type from filename
    get_mime_type <- function(filename) {
      if (is.na(filename) || is.null(filename)) return("image/jpeg")

      ext <- tolower(tools::file_ext(filename))
      switch(ext,
             "jpg" = "image/jpeg",
             "jpeg" = "image/jpeg",
             "png" = "image/png",
             "gif" = "image/gif",
             "webp" = "image/webp",
             "bmp" = "image/bmp",
             "image/jpeg"  # default fallback
      )
    }

    output$fish_table <- DT::renderDataTable({
      data <- fish_data()[,3:ncol(fish_data())]

      # Create photo column with proper MIME type detection
      data$PHOTO <- ifelse(
        is.na(data$PHOTO_DATA) | data$PHOTO_DATA == "",
        "No photo",
        paste0('<img src="data:',
               get_mime_type(data$PHOTO_FILENAME),  # Use filename to detect type
               ';base64,', data$PHOTO_DATA,
               '" height="80" width="80" style="object-fit: cover; border-radius: 5px; cursor: pointer;" ',
               'onclick="Shiny.setInputValue(\'photo_click\', \'',
               # You'll need to access the FISH_ID from the original data
               fish_data()$FISH_ID[3:nrow(fish_data())],  # Adjust this based on your data structure
               '\', {priority: \'event\'})">')
      )

      # Remove PHOTO_DATA and PHOTO_FILENAME from display
      display_data <- data[, !names(data) %in% c("PHOTO_DATA", "PHOTO_FILENAME")]

      datatable(display_data,
                escape = FALSE,  # Critical for HTML rendering
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  columnDefs = list(
                    list(targets = which(names(display_data) == "PHOTO") - 1,
                         orderable = FALSE, width = "100px")
                  )
                ),
                colnames = c("Date" = "CATCH_DATE", "Time" = "CATCH_TIME",
                             "Water" = "CATCH_WATER", "State" = "CATCH_STATE", "Weather" = "WEATHER",
                             "Species" = "SPECIES", "Length (Approx)" = "APPROX_LENGTH",
                             "Length units" = "LENGTH_UNITS", "Weight (Approx)" = "APPROX_WEIGHT",
                             "Weight units" = "WEIGHT_UNITS", "Presentation" = "PRESENTATION",
                             "Pattern" = "FLY_PATTERN", "Fly" = "FLY_NAME", "Photo" = "PHOTO", "Notes" = "NOTES")
      )
    })

    # ### Server code for rendering Fish Log datatable
    # output$fish_table <- DT::renderDT({
    #   ### Cast fish_data() reactive as a datatable. Select only the columns other
    #   ### than the first two (FISH_ID and USER_ID).
    #   data <- fish_data()[,3:length(fish_data())]
    #   data$PHOTO <- ifelse(
    #     is.na(data$PHOTO_DATA) | data$PHOTO_DATA == "",
    #     "No photo",
    #     paste0('<img src="data:image/png;base64,', data$PHOTO_DATA,
    #            '" height="80" width="80" style="object-fit: cover; border-radius: 5px; cursor: pointer;" ',
    #            'onclick="Shiny.setInputValue(\'photo_click\', \'', data$LOG_ID, '\', {priority: \'event\'})">')
    #   )
    #   datatable(data,
    #             escape = FALSE,  # Allows HTML links to work
    #             options = list(
    #               pageLength = 10,
    #               scrollX = TRUE
    #             ),
    #             colnames = c("Date" = "CATCH_DATE", "Time" = "CATCH_TIME",
    #                          "Water" = "CATCH_WATER", "State" = "CATCH_STATE", "Weather" = "WEATHER",
    #                          "Species" = "SPECIES", "Length (Approx)" = "APPROX_LENGTH",
    #                          "Length units" = "LENGTH_UNITS", "Weight (Approx)" = "APPROX_WEIGHT",
    #                          "Weight units" = "WEIGHT_UNITS", "Presentation" = "PRESENTATION",
    #                          "Pattern" = "FLY_PATTERN", "Fly" = "FLY_NAME", "Photo" = "PHOTO")
    #   )
    # })

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
        fluidRow(
          column(6,
            numericInput(NS(id, "approx_length"), "Approximate length:", value = NA)
          ),
          column(6,
            selectInput(NS(id, "length_units"), "Units",
                        choices = c("choose length units" = "", length_units),
                        selectize = TRUE
                        )
          )
        ),
        fluidRow(
          column(6,
                 numericInput(NS(id, "approx_weight"), "Approximate weight:", value = NA)
          ),
          column(6,
                 selectInput(NS(id, "weight_units"), "Units",
                             choices = c("choose weight units" = "", weight_units),
                             selectize = TRUE
                 )
          )
        ),
        # numericInput(NS(id, "approx_length"), "Approximate length:", value = NA),
        # numericInput(NS(id, "approx_weight"), "Approximate weight:", value = NA),
        selectInput(NS(id, "presentation"), "Presentation",
                    choices = c("Choose a presentation" = "", presentations),
                    selectize = TRUE),
        selectInput(NS(id, "pattern"), "Fly pattern",
                    choices = c("Choose a fly pattern" = "", patterns),
                    selectize = TRUE),
        textInput(NS(id, "fly_name"), "Fly name"),
        fileInput(NS(id, "fish_photo"), "Upload Photo (optional)",
                  accept = c('.jpg', '.jpeg', '.png', '.heic', '.heif', '.webp'),
                  width = "100%"),
        textInput(NS(id, "fish_notes"), "Notes"),
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

        ### Extract photo file name and photo data to insert into database if available
        photo_filename <- NA
        photo_data <- NA
        if (!is.null(input$fish_photo)) {
          # Read and encode photo
          file_content <- readBin(input$fish_photo$datapath, "raw",
                                  file.info(input$fish_photo$datapath)$size)
          photo_data <- base64enc::base64encode(file_content)
          photo_filename <- input$fish_photo$name
        }

        ### Insert the data for the new log line into the database
        dbExecute(pool,
           "INSERT INTO FISH_LOG
           (USER_ID, CATCH_DATE, CATCH_TIME, CATCH_WATER,
           CATCH_STATE, WEATHER, SPECIES, APPROX_LENGTH, APPROX_WEIGHT,
           PRESENTATION, FLY_PATTERN, FLY_NAME, PHOTO_FILENAME, PHOTO_DATA, NOTES) VALUES
           (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);",
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
                    input$presentation,
                    input$pattern,
                    input$fly_name,
                    photo_filename,
                    photo_data,
                    input$fish_notes
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
                               APPROX_LENGTH, APPROX_WEIGHT, PRESENTATION,
                               FLY_PATTERN, FLY_NAME, NOTES
                               FROM FISH_LOG
                               WHERE FISH_ID = ?;",
                               params = list(fish_id)
                               )[1,]
        ### Convert log_line to a character vector
        log_line <- as.character(log_line)
        ### Combine water name and state together so that it has the form 'water name, state'
        ### and use that to replace the third element (water name)
        log_line[3] = paste(log_line[3], log_line[4], sep = ", ")
        ### Remove the fourth element, which is state
        log_line <- log_line[-4]
        ### UI form for editing the log line. Set initially selected values to the
        ### values pulled from the database for that log line
        print(log_line[1])
        print(log_line[2])
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
          selectInput(NS(id, "presentation"), label =  "Presentation",
                      choices = c("Choose a fly" = "", presentations),
                      selectize = TRUE,
                      selected = log_line[8]),
          selectInput(NS(id, "pattern"), label = "Fly pattern",
                      choices = c("Choose a pattern" = "", patterns),
                      selectize = TRUE,
                      selected = log_line[9]),
          textInput(NS(id, "fly_name"), "Fly name", value = log_line[10]),
          fileInput(NS(id, "fish_photo"), "Upload Photo (optional)",
                    accept = c('.jpg', '.jpeg', '.png', '.heic', '.heif', '.webp'),
                    width = "100%"),
          textInput(NS(id, "fish_notes"), "Notes"),
          "* Indicates a required field",
          footer = tagList(
            actionButton(NS(id, "update_log_line"), "Update",
                         style = "background-color: #0EA5E9BF; border-color: #000000;"),
            modalButton("Cancel")
          )
        ))
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
        old_photo_data <- dbGetQuery(pool,
                                     "SELECT PHOTO_FILENAME, PHOTO_DATA FROM FISH_LOG
                                   WHERE FISH_ID = ?;",
                                     params = list(fish_id)
        )
        ### Capture the USER_ID of the currenly logged in user
        user_id <- logged_in()[[2]]
        ### Body of water is a drop-down menu of options in the form 'water name, state'.
        ### Water name and state should be stored as separate data points in the database
        ### so we have to split them up here.
        split_catch_water <- str_split_1(input$catch_water, pattern = ", ")
        catch_water <- split_catch_water[1]
        catch_state <- split_catch_water[2]

        # ### If there was no photo path in the database already, and the user did
        # ### not update the photo path, keep new photo path and the old one (nothing)
        # if (!is.na(old_photo_path) & is.null(input$fish_photo)) {
        #   photo_path <- old_photo_path
        # ### If old photo path is not NA but the new photopath is null, change
        # ### photo path to NA to be compatible with inserting into the database
        # } else if (is.null(input$fish_photo)) {
        #   photo_path <- NA
        # ### If old photo path is not NA and the new photo path is not null, use
        # ### the new photo path
        # } else {
        #   photo_path <- input$fish_photo$datapath[1]
        # }

        ### Extract photo file name and photo data to insert into database if available
        photo_filename <- old_photo_data$PHOTO_FILENAME
        photo_data <- old_photo_data$PHOTO_DATA
        if (!is.null(input$fish_photo)) {
          # Read and encode photo
          file_content <- readBin(input$fish_photo$datapath, "raw",
                                  file.info(input$fish_photo$datapath)$size)
          photo_data <- base64enc::base64encode(file_content)
          photo_filename <- input$fish_photo$name
        }

        ### Update the row corresponding to this log line in the database
        dbExecute(pool,
                "UPDATE FISH_LOG
                SET CATCH_DATE = ?, CATCH_TIME = ?, CATCH_WATER = ?, CATCH_STATE = ?,
                WEATHER = ?, SPECIES = ?, APPROX_LENGTH = ?, APPROX_WEIGHT = ?,
                PRESENTATION = ?, FLY_PATTERN = ?, FLY_NAME = ?, PHOTO_FILENAME = ?,
                PHOTO_DATA = ?, NOTES = ?
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
                    input$presentation,
                    input$pattern,
                    input$fly_name,
                    photo_filename,
                    photo_data,
                    input$fish_notes,
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
